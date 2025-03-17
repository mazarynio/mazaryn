package main

import (
	"log"
	"net/http"
)

func main() {
	http.HandleFunc("/nodes", handleNodes)
	http.HandleFunc("/nodes/", handleNodeOperations)
	http.HandleFunc("/nodes/peer-info", handleGetPeerInfo)
	http.HandleFunc("/nodes/{nodeId}/ipfs/add", handleIPFSAdd)
	http.HandleFunc("/nodes/{nodeId}/ipfs/get/", handleIPFSGet)
	http.HandleFunc("/nodes/{nodeId}/connect-ipfs", handleConnectToIPFSNetwork)
	http.HandleFunc("/nodes/{nodeId}/single-address", getSingleAddress)
	http.HandleFunc("/nodes/{nodeId}/ipns/publish", handlePublishToIPNS)
	http.HandleFunc("/nodes/{nodeId}/ipns/resolve", handleResolveIPNS)

	log.Println("HTTP server running on port 3000")
	if err := http.ListenAndServe(":3000", nil); err != nil {
		log.Fatalf("Failed to start HTTP server: %v", err)
	}
}
