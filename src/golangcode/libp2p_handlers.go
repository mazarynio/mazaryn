package main

import (
	"context"
	"fmt"
	"log"
	"sync"
	"time"

	"github.com/libp2p/go-libp2p"
	pubsub "github.com/libp2p/go-libp2p-pubsub"
	"github.com/libp2p/go-libp2p/core/host"
	"github.com/libp2p/go-libp2p/core/network"
	"github.com/libp2p/go-libp2p/core/peer"
	"github.com/libp2p/go-libp2p/p2p/protocol/ping"
	"github.com/multiformats/go-multiaddr"
)

type Node struct {
	ID              string
	Host            host.Host
	PubSub          *pubsub.PubSub
	PingService     *ping.PingService
	DiscoveredPeers map[peer.ID]peer.AddrInfo
	Subscriptions   map[string]*pubsub.Topic
	mu              sync.RWMutex
}

var nodes = make(map[string]*Node)

// createAndStartNode creates and starts a new libp2p node.
func createAndStartNode(nodeID string) (*Node, error) {
	if _, exists := nodes[nodeID]; exists {
		return nil, fmt.Errorf("node with ID %s already exists", nodeID)
	}

	host, err := libp2p.New()
	if err != nil {
		return nil, fmt.Errorf("failed to create libp2p host: %w", err)
	}

	ps, err := pubsub.NewGossipSub(context.Background(), host)
	if err != nil {
		return nil, fmt.Errorf("failed to create pubsub service: %w", err)
	}

	pingService := ping.NewPingService(host)

	node := &Node{
		ID:              nodeID,
		Host:            host,
		PubSub:          ps,
		PingService:     pingService,
		DiscoveredPeers: make(map[peer.ID]peer.AddrInfo),
		Subscriptions:   make(map[string]*pubsub.Topic),
	}

	nodes[nodeID] = node

	node.setupEventHandlers()

	log.Printf("Node %s created and started\n", nodeID)
	return node, nil
}

func (n *Node) setupEventHandlers() {
	n.Host.Network().Notify(&network.NotifyBundle{
		ConnectedF: func(network network.Network, conn network.Conn) {
			peerID := conn.RemotePeer()
			n.mu.Lock()
			n.DiscoveredPeers[peerID] = peer.AddrInfo{
				ID:    peerID,
				Addrs: []multiaddr.Multiaddr{conn.RemoteMultiaddr()},
			}
			n.mu.Unlock()
			log.Printf("Node %s connected to peer %s\n", n.ID, peerID)
		},
		DisconnectedF: func(network network.Network, conn network.Conn) {
			peerID := conn.RemotePeer()
			n.mu.Lock()
			delete(n.DiscoveredPeers, peerID)
			n.mu.Unlock()
			log.Printf("Node %s disconnected from peer %s\n", n.ID, peerID)
		},
	})
}

func getNode(nodeID string) (*Node, error) {
	node, exists := nodes[nodeID]
	if !exists {
		return nil, fmt.Errorf("node with ID %s does not exist", nodeID)
	}
	return node, nil
}

func getNodeAddresses(nodeID string) ([]string, error) {
	node, err := getNode(nodeID)
	if err != nil {
		return nil, err
	}

	var addresses []string
	for _, addr := range node.Host.Addrs() {
		addresses = append(addresses, addr.String())
	}
	return addresses, nil
}

func getNodePeerID(nodeID string) (string, error) {
	node, err := getNode(nodeID)
	if err != nil {
		return "", err
	}
	return node.Host.ID().String(), nil
}

// pingPeer pings a remote peer and returns the round-trip time.
func pingPeer(nodeID string, remoteAddr string) (time.Duration, error) {
	node, err := getNode(nodeID)
	if err != nil {
		return 0, err
	}

	ma, err := multiaddr.NewMultiaddr(remoteAddr)
	if err != nil {
		return 0, fmt.Errorf("invalid multiaddr: %w", err)
	}

	peerInfo, err := peer.AddrInfoFromP2pAddr(ma)
	if err != nil {
		return 0, fmt.Errorf("invalid peer address: %w", err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	ch := node.PingService.Ping(ctx, peerInfo.ID)
	select {
	case res := <-ch:
		if res.Error != nil {
			return 0, res.Error
		}
		return res.RTT, nil
	case <-ctx.Done():
		return 0, ctx.Err()
	}
}

func connectToPeer(nodeID string, remoteAddr string) error {
	node, err := getNode(nodeID)
	if err != nil {
		return err
	}

	ma, err := multiaddr.NewMultiaddr(remoteAddr)
	if err != nil {
		return fmt.Errorf("invalid multiaddr: %w", err)
	}

	peerInfo, err := peer.AddrInfoFromP2pAddr(ma)
	if err != nil {
		return fmt.Errorf("invalid peer address: %w", err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	if err := node.Host.Connect(ctx, *peerInfo); err != nil {
		return fmt.Errorf("failed to connect to peer: %w", err)
	}

	return nil
}

func getDiscoveredPeers(nodeID string) ([]peer.AddrInfo, error) {
	node, err := getNode(nodeID)
	if err != nil {
		return nil, err
	}

	node.mu.RLock()
	defer node.mu.RUnlock()

	var peers []peer.AddrInfo
	for _, peerInfo := range node.DiscoveredPeers {
		peers = append(peers, peerInfo)
	}
	return peers, nil
}

// stopNode stops and removes a node.
func stopNode(nodeID string) error {
	node, err := getNode(nodeID)
	if err != nil {
		return err
	}

	if err := node.Host.Close(); err != nil {
		return fmt.Errorf("failed to stop node: %w", err)
	}

	delete(nodes, nodeID)
	log.Printf("Node %s stopped and removed\n", nodeID)
	return nil
}

// lists all active nodes
func listAllNodes() []map[string]string {
	var nodeList []map[string]string
	for nodeID, node := range nodes {
		nodeInfo := map[string]string{
			"id":      nodeID,
			"peer_id": node.Host.ID().String(),
		}
		nodeList = append(nodeList, nodeInfo)
	}
	return nodeList
}

// subscribes to a PubSub topic.
func subscribeToTopic(nodeID string, topic string) error {
	node, err := getNode(nodeID)
	if err != nil {
		return err
	}

	t, err := node.PubSub.Join(topic)
	if err != nil {
		return fmt.Errorf("failed to subscribe to topic: %w", err)
	}

	node.mu.Lock()
	node.Subscriptions[topic] = t
	node.mu.Unlock()

	return nil
}

// unsubscribes from a PubSub topic.
func unsubscribeFromTopic(nodeID string, topic string) error {
	node, err := getNode(nodeID)
	if err != nil {
		return err
	}

	node.mu.Lock()
	delete(node.Subscriptions, topic)
	node.mu.Unlock()

	return nil
}

// publishes a message to a PubSub topic
func publishToTopic(nodeID, topicName, message string) error {
	node, err := getNode(nodeID)
	if err != nil {
		return err
	}

	node.mu.Lock()
	topic, exists := node.Subscriptions[topicName]
	node.mu.Unlock()

	if !exists {
		var err error
		topic, err = node.PubSub.Join(topicName)
		if err != nil {
			return fmt.Errorf("failed to join topic: %v", err)
		}

		node.mu.Lock()
		node.Subscriptions[topicName] = topic
		node.mu.Unlock()
	}

	if err := topic.Publish(context.Background(), []byte(message)); err != nil {
		return fmt.Errorf("failed to publish message: %v", err)
	}

	return nil
}

// retrieves the list of subscribed topics.
func getSubscriptions(nodeID string) ([]string, error) {
	node, err := getNode(nodeID)
	if err != nil {
		return nil, err
	}

	node.mu.RLock()
	defer node.mu.RUnlock()

	var subscriptions []string
	for topic := range node.Subscriptions {
		subscriptions = append(subscriptions, topic)
	}
	return subscriptions, nil
}
