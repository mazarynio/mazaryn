package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"mime/multipart"
	"net/http"
	"strings"
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

	// Create a new libp2p host
	host, err := libp2p.New()
	if err != nil {
		return nil, fmt.Errorf("failed to create libp2p host: %w", err)
	}

	// Create a new PubSub service
	ps, err := pubsub.NewGossipSub(context.Background(), host)
	if err != nil {
		return nil, fmt.Errorf("failed to create pubsub service: %w", err)
	}

	// Create a new Ping service
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

// addFileToIPFS adds a file to IPFS and returns the CID
func addFileToIPFS(nodeID string, fileContent string) (string, error) {
	url := "http://localhost:5001/api/v0/add"

	body := &bytes.Buffer{}
	writer := multipart.NewWriter(body)
	part, err := writer.CreateFormFile("file", "file.txt")
	if err != nil {
		return "", fmt.Errorf("failed to create form file: %w", err)
	}

	_, err = io.Copy(part, strings.NewReader(fileContent))
	if err != nil {
		return "", fmt.Errorf("failed to copy file content: %w", err)
	}

	err = writer.Close()
	if err != nil {
		return "", fmt.Errorf("failed to close writer: %w", err)
	}

	req, err := http.NewRequest("POST", url, body)
	if err != nil {
		return "", fmt.Errorf("failed to create HTTP request: %w", err)
	}
	req.Header.Set("Content-Type", writer.FormDataContentType())

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return "", fmt.Errorf("failed to send request to IPFS: %w", err)
	}
	defer resp.Body.Close()

	var result struct {
		Name string `json:"Name"`
		Hash string `json:"Hash"`
		Size string `json:"Size"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&result); err != nil {
		return "", fmt.Errorf("failed to decode IPFS response: %w", err)
	}

	return result.Hash, nil
}

// retrieves a file from IPFS using its CID via the Kubo RPC API.
func getFileFromIPFS(nodeID string, cidStr string) (string, error) {
	log.Printf("Retrieving file from IPFS for node %s with CID %s", nodeID, cidStr)

	url := fmt.Sprintf("http://localhost:5001/api/v0/cat?arg=%s", cidStr)

	resp, err := http.Post(url, "application/json", nil)
	if err != nil {
		return "", fmt.Errorf("failed to send request to IPFS: %w", err)
	}
	defer resp.Body.Close()

	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", fmt.Errorf("failed to read file content: %w", err)
	}

	return string(data), nil
}

// handles requests to the /nodes endpoint
func handleNodes(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case http.MethodPost:
		nodeID := r.URL.Query().Get("id")
		if nodeID == "" {
			http.Error(w, `{"error": "Missing node ID"}`, http.StatusBadRequest)
			return
		}

		node, err := createAndStartNode(nodeID)
		if err != nil {
			http.Error(w, fmt.Sprintf(`{"error": "Failed to create node: %v"}`, err), http.StatusInternalServerError)
			return
		}

		response := map[string]string{
			"message": fmt.Sprintf("Node %s created successfully with peer ID %s", nodeID, node.Host.ID().String()),
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(response)

	case http.MethodGet:
		nodeList := listAllNodes()
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(nodeList)

	default:
		http.Error(w, `{"error": "Method not allowed"}`, http.StatusMethodNotAllowed)
	}
}

func handleNodeOperations(w http.ResponseWriter, r *http.Request) {
	path := strings.TrimPrefix(r.URL.Path, "/nodes/")
	parts := strings.Split(path, "/")
	if len(parts) < 1 {
		http.Error(w, "Invalid URL path", http.StatusBadRequest)
		return
	}

	nodeID := parts[0]

	_, err := getNode(nodeID)
	if err != nil {
		http.Error(w, fmt.Sprintf("Node not found: %v", err), http.StatusNotFound)
		return
	}

	if len(parts) == 1 {
		switch r.Method {
		case http.MethodGet:
			addresses, _ := getNodeAddresses(nodeID)
			peerID, _ := getNodePeerID(nodeID)

			response := map[string]interface{}{
				"id":        nodeID,
				"peer_id":   peerID,
				"addresses": addresses,
			}
			w.Header().Set("Content-Type", "application/json")
			json.NewEncoder(w).Encode(response)

		case http.MethodDelete:
			if err := stopNode(nodeID); err != nil {
				http.Error(w, fmt.Sprintf("Failed to stop node: %v", err), http.StatusInternalServerError)
				return
			}

			fmt.Fprintf(w, "Node %s stopped and removed successfully", nodeID)

		default:
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		}
		return
	}

	operation := parts[1]

	switch operation {
	case "connect":
		if r.Method != http.MethodPost {
			http.Error(w, `{"error": "Method not allowed"}`, http.StatusMethodNotAllowed)
			return
		}

		remoteAddr := r.URL.Query().Get("addr")
		if remoteAddr == "" {
			http.Error(w, `{"error": "Missing remote address"}`, http.StatusBadRequest)
			return
		}

		if err := connectToPeer(nodeID, remoteAddr); err != nil {
			http.Error(w, fmt.Sprintf(`{"error": "Failed to connect to peer: %v"}`, err), http.StatusInternalServerError)
			return
		}

		response := map[string]string{
			"message": fmt.Sprintf("Successfully connected to %s", remoteAddr),
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(response)

	case "ping":
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		remoteAddr := r.URL.Query().Get("addr")
		if remoteAddr == "" {
			http.Error(w, "Missing remote address", http.StatusBadRequest)
			return
		}

		rtt, err := pingPeer(nodeID, remoteAddr)
		if err != nil {
			http.Error(w, fmt.Sprintf("Failed to ping peer: %v", err), http.StatusInternalServerError)
			return
		}

		fmt.Fprintf(w, "Ping RTT: %v", rtt)

	case "peers":
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		peers, err := getDiscoveredPeers(nodeID)
		if err != nil {
			http.Error(w, fmt.Sprintf("Failed to get discovered peers: %v", err), http.StatusInternalServerError)
			return
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{
			"peers": peers,
		})

	case "pubsub":
		topicName := r.URL.Query().Get("topic")
		if topicName == "" && r.Method != http.MethodGet {
			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusBadRequest)
			json.NewEncoder(w).Encode(map[string]string{
				"error": "Missing topic name",
			})
			return
		}

		switch r.Method {
		case http.MethodGet:
			subscriptions, err := getSubscriptions(nodeID)
			if err != nil {
				w.Header().Set("Content-Type", "application/json")
				w.WriteHeader(http.StatusInternalServerError)
				json.NewEncoder(w).Encode(map[string]string{
					"error": fmt.Sprintf("Failed to get subscriptions: %v", err),
				})
				return
			}

			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusOK)
			json.NewEncoder(w).Encode(map[string]interface{}{
				"subscriptions": subscriptions,
			})

		case http.MethodPost:
			if err := subscribeToTopic(nodeID, topicName); err != nil {
				w.Header().Set("Content-Type", "application/json")
				w.WriteHeader(http.StatusInternalServerError)
				json.NewEncoder(w).Encode(map[string]string{
					"error": fmt.Sprintf("Failed to subscribe to topic: %v", err),
				})
				return
			}

			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusOK)
			json.NewEncoder(w).Encode(map[string]string{
				"message": fmt.Sprintf("Successfully subscribed to topic %s", topicName),
			})

		case http.MethodPut:
			message := r.URL.Query().Get("message")
			if message == "" {
				w.Header().Set("Content-Type", "application/json")
				w.WriteHeader(http.StatusBadRequest)
				json.NewEncoder(w).Encode(map[string]string{
					"error": "Missing message",
				})
				return
			}

			if err := publishToTopic(nodeID, topicName, message); err != nil {
				w.Header().Set("Content-Type", "application/json")
				w.WriteHeader(http.StatusInternalServerError)
				json.NewEncoder(w).Encode(map[string]string{
					"error": fmt.Sprintf("Failed to publish to topic: %v", err),
				})
				return
			}

			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusOK)
			json.NewEncoder(w).Encode(map[string]string{
				"message": fmt.Sprintf("Successfully published to topic %s", topicName),
			})

		case http.MethodDelete:
			if err := unsubscribeFromTopic(nodeID, topicName); err != nil {
				w.Header().Set("Content-Type", "application/json")
				w.WriteHeader(http.StatusInternalServerError)
				json.NewEncoder(w).Encode(map[string]string{
					"error": fmt.Sprintf("Failed to unsubscribe from topic: %v", err),
				})
				return
			}

			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusOK)
			json.NewEncoder(w).Encode(map[string]string{
				"message": fmt.Sprintf("Successfully unsubscribed from topic %s", topicName),
			})

		default:
			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusMethodNotAllowed)
			json.NewEncoder(w).Encode(map[string]string{
				"error": "Method not allowed",
			})
		}

	case "peerid":
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		peerID, err := getNodePeerID(nodeID)
		if err != nil {
			http.Error(w, fmt.Sprintf("Failed to get peer ID: %v", err), http.StatusInternalServerError)
			return
		}

		fmt.Fprintf(w, "%s", peerID)

	case "addresses":
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		addresses, err := getNodeAddresses(nodeID)
		if err != nil {
			http.Error(w, fmt.Sprintf("Failed to get addresses: %v", err), http.StatusInternalServerError)
			return
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{
			"addresses": addresses,
		})

	default:
		http.Error(w, "Invalid operation", http.StatusBadRequest)
	}
}

func getSingleAddress(w http.ResponseWriter, r *http.Request) {
	path := strings.TrimPrefix(r.URL.Path, "/nodes/")
	parts := strings.Split(path, "/")
	if len(parts) < 2 {
		http.Error(w, "Invalid URL path", http.StatusBadRequest)
		return
	}

	nodeID := parts[0]
	node, err := getNode(nodeID)
	if err != nil {
		http.Error(w, fmt.Sprintf("Node not found: %v", err), http.StatusNotFound)
		return
	}

	addresses := node.Host.Addrs()
	if len(addresses) == 0 {
		http.Error(w, "No addresses found", http.StatusNotFound)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]string{
		"address": addresses[0].String(),
	})
}

func handleIPFSAdd(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusMethodNotAllowed)
		json.NewEncoder(w).Encode(map[string]string{
			"error": "Method not allowed",
		})
		return
	}

	path := strings.TrimPrefix(r.URL.Path, "/nodes/")
	parts := strings.Split(path, "/")
	if len(parts) < 1 {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		json.NewEncoder(w).Encode(map[string]string{
			"error": "Invalid URL path",
		})
		return
	}

	nodeID := parts[0]

	body, err := io.ReadAll(r.Body)
	if err != nil {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		json.NewEncoder(w).Encode(map[string]string{
			"error": fmt.Sprintf("Failed to read request body: %v", err),
		})
		return
	}

	cid, err := addFileToIPFS(nodeID, string(body))
	if err != nil {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusInternalServerError)
		json.NewEncoder(w).Encode(map[string]string{
			"error": fmt.Sprintf("Failed to add file to IPFS: %v", err),
		})
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(map[string]string{
		"cid": cid,
	})
}

func handleIPFSGet(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusMethodNotAllowed)
		json.NewEncoder(w).Encode(map[string]string{
			"error": "Method not allowed",
		})
		return
	}

	path := strings.TrimPrefix(r.URL.Path, "/nodes/")
	parts := strings.Split(path, "/")
	if len(parts) < 4 {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		json.NewEncoder(w).Encode(map[string]string{
			"error": "Invalid URL path",
		})
		return
	}

	nodeID := parts[0]
	cid := parts[3]

	content, err := getFileFromIPFS(nodeID, cid)
	if err != nil {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusInternalServerError)
		json.NewEncoder(w).Encode(map[string]string{
			"error": fmt.Sprintf("Failed to get file from IPFS: %v", err),
		})
		return
	}

	w.Header().Set("Content-Type", "text/plain")
	w.WriteHeader(http.StatusOK)
	w.Write([]byte(content))
}

func main() {
	http.HandleFunc("/nodes", handleNodes)
	http.HandleFunc("/nodes/", handleNodeOperations)
	http.HandleFunc("/nodes/{nodeId}/ipfs/add", handleIPFSAdd)
	http.HandleFunc("/nodes/{nodeId}/ipfs/get/", handleIPFSGet)

	http.HandleFunc("/nodes/{nodeId}/single-address", getSingleAddress)

	log.Println("HTTP server running on port 3000")
	if err := http.ListenAndServe(":3000", nil); err != nil {
		log.Fatalf("Failed to start HTTP server: %v", err)
	}
}
