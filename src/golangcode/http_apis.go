package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"
)

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

	// Handle direct node operations (when path is just /nodes/{nodeID})
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

	// Handle operations with sub-paths (like /nodes/{nodeID}/connect)
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
		handlePubsubOperations(w, r, nodeID, parts)

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

	case "ipfs":
		handleIPFSOperations(w, r, nodeID, parts)

	case "ipns":
		handleIPNSOperations(w, r, nodeID, parts)

	case "single-address":
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

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

	case "connect-ipfs":
		if r.Method != http.MethodPost {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		ipfsMultiaddr := r.URL.Query().Get("addr")
		if ipfsMultiaddr == "" {
			http.Error(w, "Missing IPFS multiaddress", http.StatusBadRequest)
			return
		}

		node, err := getNode(nodeID)
		if err != nil {
			http.Error(w, fmt.Sprintf("Node not found: %v", err), http.StatusNotFound)
			return
		}

		if err := connectToIPFSNetwork(node, ipfsMultiaddr); err != nil {
			http.Error(w, fmt.Sprintf("Failed to connect to IPFS network: %v", err), http.StatusInternalServerError)
			return
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{
			"message": fmt.Sprintf("Successfully connected to IPFS network at %s", ipfsMultiaddr),
		})

	case "network-status":
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		networkStatus, err := getNetworkStatus(nodeID)
		if err != nil {
			http.Error(w, fmt.Sprintf("Failed to get network status: %v", err), http.StatusInternalServerError)
			return
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(networkStatus)

	default:
		http.Error(w, "Invalid IPFS operation", http.StatusBadRequest)
	}
}

// Helper function to handle pubsub operations
func handlePubsubOperations(w http.ResponseWriter, r *http.Request, nodeID string, parts []string) {
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
}

// Helper function to handle IPFS operations
func handleIPFSOperations(w http.ResponseWriter, r *http.Request, nodeID string, parts []string) {
	if len(parts) < 3 {
		http.Error(w, "Invalid URL path for IPFS operations", http.StatusBadRequest)
		return
	}

	operation := parts[2]

	switch operation {
	case "add":
		if r.Method != http.MethodPost {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		body, err := io.ReadAll(r.Body)
		if err != nil {
			http.Error(w, fmt.Sprintf("Failed to read request body: %v", err), http.StatusBadRequest)
			return
		}

		cid, err := addFileToIPFS(nodeID, string(body))
		if err != nil {
			http.Error(w, fmt.Sprintf("Failed to add file to IPFS: %v", err), http.StatusInternalServerError)
			return
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{
			"cid": cid,
		})

	case "get":
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		if len(parts) < 4 {
			http.Error(w, "Missing CID in URL path", http.StatusBadRequest)
			return
		}

		cid := parts[3]
		content, err := getFileFromIPFS(nodeID, cid)
		if err != nil {
			http.Error(w, fmt.Sprintf("Failed to get file from IPFS: %v", err), http.StatusInternalServerError)
			return
		}

		w.Header().Set("Content-Type", "text/plain")
		w.Write([]byte(content))

	case "get-metadata":
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		if len(parts) < 4 {
			http.Error(w, "Missing CID in URL path", http.StatusBadRequest)
			return
		}

		cid := parts[3]
		metadata, err := getFileMetadata(nodeID, cid)
		if err != nil {
			http.Error(w, fmt.Sprintf("Failed to get file metadata from IPFS: %v", err), http.StatusInternalServerError)
			return
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(metadata)

	default:
		http.Error(w, "Invalid IPFS operation", http.StatusBadRequest)
	}
}

// Helper function to handle IPNS operations
func handleIPNSOperations(w http.ResponseWriter, r *http.Request, nodeID string, parts []string) {
	if len(parts) < 3 {
		http.Error(w, "Invalid URL path for IPNS operations", http.StatusBadRequest)
		return
	}

	operation := parts[2]

	switch operation {
	case "publish":
		if r.Method != http.MethodPost {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		cid := r.URL.Query().Get("cid")
		if cid == "" {
			http.Error(w, "Missing CID parameter", http.StatusBadRequest)
			return
		}

		ipnsName, err := publishToIPNS(nodeID, cid)
		if err != nil {
			http.Error(w, fmt.Sprintf("Failed to publish to IPNS: %v", err), http.StatusInternalServerError)
			return
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{
			"ipns_name": ipnsName,
		})

	case "resolve":
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		ipnsName := r.URL.Query().Get("ipnsName")
		if ipnsName == "" {
			http.Error(w, "Missing IPNS name parameter", http.StatusBadRequest)
			return
		}

		resolvedPath, err := resolveIPNS(nodeID, ipnsName)
		if err != nil {
			http.Error(w, fmt.Sprintf("Failed to resolve IPNS name: %v", err), http.StatusInternalServerError)
			return
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{
			"resolved_path": resolvedPath,
		})

	default:
		http.Error(w, "Invalid IPNS operation", http.StatusBadRequest)
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

func handleGetPeerInfo(w http.ResponseWriter, r *http.Request) {
	nodeID := r.URL.Query().Get("nodeID")
	peerID := r.URL.Query().Get("peerID")

	peerInfo, err := getPeerInfo(nodeID, peerID)
	if err != nil {
		http.Error(w, fmt.Sprintf("Failed to get peer info: %v", err), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(peerInfo)
}

func handleConnectToIPFSNetwork(w http.ResponseWriter, r *http.Request) {
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
	if len(parts) < 2 {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		json.NewEncoder(w).Encode(map[string]string{
			"error": "Invalid URL path",
		})
		return
	}

	nodeID := parts[0]
	ipfsMultiaddr := r.URL.Query().Get("addr")
	if ipfsMultiaddr == "" {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		json.NewEncoder(w).Encode(map[string]string{
			"error": "Missing IPFS multiaddress",
		})
		return
	}

	node, err := getNode(nodeID)
	if err != nil {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusNotFound)
		json.NewEncoder(w).Encode(map[string]string{
			"error": fmt.Sprintf("Node not found: %v", err),
		})
		return
	}

	if err := connectToIPFSNetwork(node, ipfsMultiaddr); err != nil {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusInternalServerError)
		json.NewEncoder(w).Encode(map[string]string{
			"error": fmt.Sprintf("Failed to connect to IPFS network: %v", err),
		})
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(map[string]string{
		"message": fmt.Sprintf("Successfully connected to IPFS network at %s", ipfsMultiaddr),
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

// Publish a CID to IPNS
func handlePublishToIPNS(w http.ResponseWriter, r *http.Request) {
	nodeID := r.URL.Query().Get("nodeID")
	cid := r.URL.Query().Get("cid")

	ipnsName, err := publishToIPNS(nodeID, cid)
	if err != nil {
		http.Error(w, fmt.Sprintf(`{"error": "Failed to publish to IPNS: %v"}`, err), http.StatusInternalServerError)
		return
	}

	response := map[string]string{
		"ipns_name": ipnsName,
	}
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// Resolve an IPNS name to its current CID
func handleResolveIPNS(w http.ResponseWriter, r *http.Request) {
	nodeID := r.URL.Query().Get("nodeID")
	ipnsName := r.URL.Query().Get("ipnsName")

	resolvedPath, err := resolveIPNS(nodeID, ipnsName)
	if err != nil {
		http.Error(w, fmt.Sprintf(`{"error": "Failed to resolve IPNS name: %v"}`, err), http.StatusInternalServerError)
		return
	}

	response := map[string]string{
		"resolved_path": resolvedPath,
	}
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// handles HTTP requests to retrieve the network status.
func handleGetNetworkStatus(w http.ResponseWriter, r *http.Request) {
	nodeID := r.URL.Query().Get("nodeID")

	networkStatus, err := getNetworkStatus(nodeID)
	if err != nil {
		http.Error(w, fmt.Sprintf(`{"error": "Failed to get network status: %v"}`, err), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(networkStatus)
}

func handleIPFSGetMetadata(w http.ResponseWriter, r *http.Request) {
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
	if len(parts) < 3 {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		json.NewEncoder(w).Encode(map[string]string{
			"error": "Invalid URL path. Expected /nodes/{nodeID}/get-metadata/{CID}",
		})
		return
	}

	nodeID := parts[0]
	cid := parts[2]

	metadata, err := getFileMetadata(nodeID, cid)
	if err != nil {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusInternalServerError)
		json.NewEncoder(w).Encode(map[string]string{
			"error": fmt.Sprintf("Failed to get file metadata from IPFS: %v", err),
		})
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(metadata)
}
