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

// Node represents a libp2p node with associated services.
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

	// Set up event handlers
	node.setupEventHandlers()

	log.Printf("Node %s created and started\n", nodeID)
	return node, nil
}

// setupEventHandlers sets up network event handlers for the node.
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

// getNode retrieves a node by its ID.
func getNode(nodeID string) (*Node, error) {
	node, exists := nodes[nodeID]
	if !exists {
		return nil, fmt.Errorf("node with ID %s does not exist", nodeID)
	}
	return node, nil
}

// getNodeAddresses retrieves the addresses of a node.
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

// getNodePeerID retrieves the peer ID of a node.
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

// connectToPeer connects to a remote peer.
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

// getDiscoveredPeers retrieves the list of discovered peers.
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

// listAllNodes lists all active nodes.
func listAllNodes() []string {
	var nodeIDs []string
	for nodeID := range nodes {
		nodeIDs = append(nodeIDs, nodeID)
	}
	return nodeIDs
}

// subscribeToTopic subscribes to a PubSub topic.
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

// unsubscribeFromTopic unsubscribes from a PubSub topic.
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

// publishToTopic publishes a message to a PubSub topic.
func publishToTopic(nodeID string, topic string, message string) error {
	node, err := getNode(nodeID)
	if err != nil {
		return err
	}

	t, err := node.PubSub.Join(topic)
	if err != nil {
		return fmt.Errorf("failed to join topic: %w", err)
	}

	if err := t.Publish(context.Background(), []byte(message)); err != nil {
		return fmt.Errorf("failed to publish message: %w", err)
	}

	return nil
}

// getSubscriptions retrieves the list of subscribed topics.
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

// addFileToIPFS adds a file to IPFS and returns the CID.
func addFileToIPFS(nodeID string, fileContent string) (string, error) {
	// Kubo RPC API endpoint for adding files
	url := "http://localhost:5001/api/v0/add"

	// Create a multipart form request
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

	// Send the request to the Kubo RPC API
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

	// Parse the response
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

// getFileFromIPFS retrieves a file from IPFS using its CID via the Kubo RPC API.
func getFileFromIPFS(nodeID string, cidStr string) (string, error) {
	// Kubo RPC API endpoint for retrieving files
	url := fmt.Sprintf("http://localhost:5001/api/v0/cat?arg=%s", cidStr)

	// Send the request to the Kubo RPC API
	resp, err := http.Get(url)
	if err != nil {
		return "", fmt.Errorf("failed to send request to IPFS: %w", err)
	}
	defer resp.Body.Close()

	// Read the response body
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", fmt.Errorf("failed to read file content: %w", err)
	}

	return string(data), nil
}

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

		// Return a JSON response
		response := map[string]string{
			"message": fmt.Sprintf("Node %s created successfully with peer ID %s", nodeID, node.Host.ID().String()),
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(response)
	default:
		http.Error(w, `{"error": "Method not allowed"}`, http.StatusMethodNotAllowed)
	}
}

func handleNodeOperations(w http.ResponseWriter, r *http.Request) {
	// Extract node ID from URL path
	path := strings.TrimPrefix(r.URL.Path, "/nodes/")
	parts := strings.Split(path, "/")
	if len(parts) < 1 {
		http.Error(w, "Invalid URL path", http.StatusBadRequest)
		return
	}

	nodeID := parts[0]

	// Check if the node exists
	if _, err := getNode(nodeID); err != nil {
		http.Error(w, fmt.Sprintf("Node not found: %v", err), http.StatusNotFound)
		return
	}

	// Handle different operations based on URL path and method
	if len(parts) == 1 {
		switch r.Method {
		case http.MethodGet:
			// Get node information
			addresses, _ := getNodeAddresses(nodeID)
			peerID, _ := getNodePeerID(nodeID)

			fmt.Fprintf(w, "Node ID: %s\nPeer ID: %s\nAddresses: %v", nodeID, peerID, addresses)
		case http.MethodDelete:
			// Stop and remove the node
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

	// Handle additional operations
	operation := parts[1]

	switch operation {
	case "connect":
		if r.Method != http.MethodPost {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		remoteAddr := r.URL.Query().Get("addr")
		if remoteAddr == "" {
			http.Error(w, "Missing remote address", http.StatusBadRequest)
			return
		}

		if err := connectToPeer(nodeID, remoteAddr); err != nil {
			http.Error(w, fmt.Sprintf("Failed to connect to peer: %v", err), http.StatusInternalServerError)
			return
		}

		fmt.Fprintf(w, "Successfully connected to %s", remoteAddr)

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

		fmt.Fprintf(w, "Discovered peers: %v", peers)

	case "pubsub":
		topicName := r.URL.Query().Get("topic")
		if topicName == "" {
			http.Error(w, "Missing topic name", http.StatusBadRequest)
			return
		}

		switch r.Method {
		case http.MethodGet:
			// Get subscriptions
			subscriptions, err := getSubscriptions(nodeID)
			if err != nil {
				http.Error(w, fmt.Sprintf("Failed to get subscriptions: %v", err), http.StatusInternalServerError)
				return
			}

			fmt.Fprintf(w, "Subscriptions: %v", subscriptions)

		case http.MethodPost:
			// Subscribe to topic
			if err := subscribeToTopic(nodeID, topicName); err != nil {
				http.Error(w, fmt.Sprintf("Failed to subscribe to topic: %v", err), http.StatusInternalServerError)
				return
			}

			fmt.Fprintf(w, "Successfully subscribed to topic %s", topicName)

		case http.MethodPut:
			// Publish to topic
			message := r.URL.Query().Get("message")
			if message == "" {
				http.Error(w, "Missing message", http.StatusBadRequest)
				return
			}

			if err := publishToTopic(nodeID, topicName, message); err != nil {
				http.Error(w, fmt.Sprintf("Failed to publish to topic: %v", err), http.StatusInternalServerError)
				return
			}

			fmt.Fprintf(w, "Successfully published to topic %s", topicName)

		case http.MethodDelete:
			// Unsubscribe from topic
			if err := unsubscribeFromTopic(nodeID, topicName); err != nil {
				http.Error(w, fmt.Sprintf("Failed to unsubscribe from topic: %v", err), http.StatusInternalServerError)
				return
			}

			fmt.Fprintf(w, "Successfully unsubscribed from topic %s", topicName)

		default:
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		}

	default:
		http.Error(w, "Invalid operation", http.StatusBadRequest)
	}
}

// getSingleAddress retrieves a single address of a node.
func getSingleAddress(w http.ResponseWriter, r *http.Request) {
	// Extract node ID from the URL path
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

	// Get the node's addresses
	addresses := node.Host.Addrs()
	if len(addresses) == 0 {
		http.Error(w, "No addresses found", http.StatusNotFound)
		return
	}

	// Return the first address
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]string{
		"address": addresses[0].String(),
	})
}

func handleIPFSAdd(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Extract node ID from URL path
	path := strings.TrimPrefix(r.URL.Path, "/nodes/")
	parts := strings.Split(path, "/")
	if len(parts) < 1 {
		http.Error(w, "Invalid URL path", http.StatusBadRequest)
		return
	}

	nodeID := parts[0]

	// Read request body
	body, err := io.ReadAll(r.Body)
	if err != nil {
		http.Error(w, fmt.Sprintf("Failed to read request body: %v", err), http.StatusBadRequest)
		return
	}

	// Add file to IPFS
	cid, err := addFileToIPFS(nodeID, string(body))
	if err != nil {
		http.Error(w, fmt.Sprintf("Failed to add file to IPFS: %v", err), http.StatusInternalServerError)
		return
	}

	fmt.Fprintf(w, "File added to IPFS with CID: %s", cid)
}

func handleIPFSGet(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Extract node ID and CID from URL path
	path := strings.TrimPrefix(r.URL.Path, "/nodes/")
	parts := strings.Split(path, "/")
	if len(parts) < 4 {
		http.Error(w, "Invalid URL path", http.StatusBadRequest)
		return
	}

	nodeID := parts[0]
	cid := parts[3]

	// Get file from IPFS
	content, err := getFileFromIPFS(nodeID, cid)
	if err != nil {
		http.Error(w, fmt.Sprintf("Failed to get file from IPFS: %v", err), http.StatusInternalServerError)
		return
	}

	fmt.Fprint(w, content)
}

func main() {
	// Existing handlers
	http.HandleFunc("/nodes", handleNodes)
	http.HandleFunc("/nodes/", handleNodeOperations)
	http.HandleFunc("/nodes/{nodeId}/ipfs/add", handleIPFSAdd)
	http.HandleFunc("/nodes/{nodeId}/ipfs/get/", handleIPFSGet)

	// New handler for single address
	http.HandleFunc("/nodes/{nodeId}/single-address", getSingleAddress)

	log.Println("HTTP server running on port 3000")
	if err := http.ListenAndServe(":3000", nil); err != nil {
		log.Fatalf("Failed to start HTTP server: %v", err)
	}
}
