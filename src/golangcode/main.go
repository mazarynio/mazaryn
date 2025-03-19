package main

import (
	"log"
	"net/http"
)

func main() {

	http.HandleFunc("/nodes/peer-info", handleGetPeerInfo)
	http.HandleFunc("/nodes/ipfs/add", handleIPFSAdd)
	http.HandleFunc("/nodes/ipfs/get/", handleIPFSGet)
	http.HandleFunc("/nodes/connect-ipfs", handleConnectToIPFSNetwork)
	http.HandleFunc("/nodes/single-address", getSingleAddress)
	http.HandleFunc("/nodes/ipns/publish", handlePublishToIPNS)
	http.HandleFunc("/nodes/ipns/resolve", handleResolveIPNS)
	http.HandleFunc("/nodes/network-status", handleGetNetworkStatus)
	http.HandleFunc("/nodes/ipfs/get-metadata/", handleIPFSGetMetadata)

	http.HandleFunc("/dht/findpeer", handleDHTFindPeer)
	http.HandleFunc("/dht/findprovs", handleDHTFindProvs)
	http.HandleFunc("/dht/provide", handleDHTProvide)

	http.HandleFunc("/nodes", handleNodes)
	http.HandleFunc("/nodes/", handleNodeOperations)

	log.Println("HTTP server running on port 3000")
	if err := http.ListenAndServe(":3000", nil); err != nil {
		log.Fatalf("Failed to start HTTP server: %v", err)
	}
}
