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
	"os/exec"
	"strings"
	"time"

	"github.com/libp2p/go-libp2p/core/peer"
	"github.com/multiformats/go-multiaddr"
)

// getPeerInfo retrieves information about a peer using the IPFS HTTP API.
func getPeerInfo(nodeID, peerID string) (map[string]interface{}, error) {
	url := fmt.Sprintf("http://localhost:5001/api/v0/id?arg=%s", peerID)
	req, err := http.NewRequest("POST", url, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to create HTTP request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{Timeout: 10 * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		node, err := getNode(nodeID)
		if err != nil {
			return nil, fmt.Errorf("failed to get local node: %w", err)
		}

		return map[string]interface{}{
			"peerID": node.Host.ID().String(),
			"addrs":  node.Host.Addrs(),
			"nodeID": nodeID,
			"local":  true,
		}, nil
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("IPFS API returned status: %s, body: %s", resp.Status, body)
	}

	var ipfsPeerInfo map[string]interface{}
	if err := json.NewDecoder(resp.Body).Decode(&ipfsPeerInfo); err != nil {
		return nil, fmt.Errorf("failed to decode IPFS response: %w", err)
	}

	return ipfsPeerInfo, nil
}

// connectToIPFSNetwork connects the libp2p node to the IPFS network.
func connectToIPFSNetwork(node *Node, ipfsMultiaddr string) error {
	ma, err := multiaddr.NewMultiaddr(ipfsMultiaddr)
	if err != nil {
		return fmt.Errorf("invalid IPFS multiaddress: %w", err)
	}

	peerInfo, err := peer.AddrInfoFromP2pAddr(ma)
	if err != nil {
		return fmt.Errorf("failed to extract peer info from multiaddress: %w", err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	if err := node.Host.Connect(ctx, *peerInfo); err != nil {
		return fmt.Errorf("failed to connect to IPFS daemon: %w", err)
	}

	log.Printf("Node %s connected to IPFS daemon at %s\n", node.ID, ipfsMultiaddr)
	return nil
}

// addFileToIPFS adds a file to IPFS and returns the CID
func addFileToIPFS(nodeID string, fileContent string) (string, error) {
	if !isIPFSDaemonRunning() {
		err := startIPFSDaemon()
		if err != nil {
			return "", fmt.Errorf("failed to start IPFS daemon: %w", err)
		}
		time.Sleep(5 * time.Second)
	}

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

// getFileFromIPFS retrieves a file from IPFS using its CID via the Kubo RPC API.
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

func isIPFSDaemonRunning() bool {
	_, err := http.Get("http://localhost:5001/api/v0/id")
	return err == nil
}

func startIPFSDaemon() error {
	cmd := exec.Command("ipfs", "daemon")
	err := cmd.Start()
	if err != nil {
		return fmt.Errorf("failed to start IPFS daemon: %w", err)
	}

	go func() {
		err := cmd.Wait()
		if err != nil {
			log.Printf("IPFS daemon exited with error: %v", err)
		}
	}()

	return nil
}

func publishToIPNS(nodeID string, cid string) (string, error) {
	if !isIPFSDaemonRunning() {
		err := startIPFSDaemon()
		if err != nil {
			return "", fmt.Errorf("failed to start IPFS daemon: %w", err)
		}
		time.Sleep(5 * time.Second)
	}

	url := fmt.Sprintf("http://localhost:5001/api/v0/name/publish?arg=%s", cid)
	req, err := http.NewRequest("POST", url, nil)
	if err != nil {
		return "", fmt.Errorf("failed to create HTTP request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{Timeout: 10 * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		return "", fmt.Errorf("failed to send request to IPFS: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return "", fmt.Errorf("IPFS API returned status: %s, body: %s", resp.Status, body)
	}

	var result struct {
		Name  string `json:"Name"`
		Value string `json:"Value"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&result); err != nil {
		return "", fmt.Errorf("failed to decode IPFS response: %w", err)
	}

	return result.Name, nil
}

// resolves an IPNS name to its current CID.
func resolveIPNS(nodeID string, ipnsName string) (string, error) {
	if !isIPFSDaemonRunning() {
		err := startIPFSDaemon()
		if err != nil {
			return "", fmt.Errorf("failed to start IPFS daemon: %w", err)
		}
		time.Sleep(5 * time.Second)
	}

	url := fmt.Sprintf("http://localhost:5001/api/v0/name/resolve?arg=%s", ipnsName)
	req, err := http.NewRequest("POST", url, nil)
	if err != nil {
		return "", fmt.Errorf("failed to create HTTP request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{Timeout: 10 * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		return "", fmt.Errorf("failed to send request to IPFS: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return "", fmt.Errorf("IPFS API returned status: %s, body: %s", resp.Status, body)
	}

	var result struct {
		Path string `json:"Path"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&result); err != nil {
		return "", fmt.Errorf("failed to decode IPFS response: %w", err)
	}

	return result.Path, nil
}

// retrieves the network status of the IPFS node.
func getNetworkStatus(nodeID string) (map[string]interface{}, error) {
	if !isIPFSDaemonRunning() {
		err := startIPFSDaemon()
		if err != nil {
			return nil, fmt.Errorf("failed to start IPFS daemon: %w", err)
		}
		time.Sleep(5 * time.Second)
	}

	url := "http://localhost:5001/api/v0/stats/bw"
	req, err := http.NewRequest("POST", url, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to create HTTP request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{Timeout: 10 * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		return nil, fmt.Errorf("failed to send request to IPFS: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("IPFS API returned status: %s, body: %s", resp.Status, body)
	}

	var networkStatus map[string]interface{}
	if err := json.NewDecoder(resp.Body).Decode(&networkStatus); err != nil {
		return nil, fmt.Errorf("failed to decode IPFS response: %w", err)
	}

	return networkStatus, nil
}

// retrieves metadata of a file from IPFS using its CID.
func getFileMetadata(nodeID string, cidStr string) (map[string]interface{}, error) {
	url := fmt.Sprintf("http://localhost:5001/api/v0/dag/stat?arg=%s", cidStr)

	req, err := http.NewRequest("POST", url, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to create request: %w", err)
	}

	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return nil, fmt.Errorf("failed to send request to IPFS: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("IPFS API returned status: %s, body: %s", resp.Status, body)
	}

	var metadata map[string]interface{}
	if err := json.NewDecoder(resp.Body).Decode(&metadata); err != nil {
		return nil, fmt.Errorf("failed to decode IPFS response: %w", err)
	}

	return metadata, nil
}
