import process from 'node:process';
import { createLibp2p } from 'libp2p';
import { tcp } from '@libp2p/tcp';
import { noise } from '@chainsafe/libp2p-noise';
import { yamux } from '@chainsafe/libp2p-yamux';
import { multiaddr } from 'multiaddr';
import { ping } from '@libp2p/ping';
import express from 'express';

const app = express();
app.use(express.json());

const nodes = new Map();


const createAndStartNode = async (nodeId) => {
  if (nodes.has(nodeId)) {
    throw new Error(`Node with ID ${nodeId} already exists`);
  }

  const node = await createLibp2p({
    addresses: {
      listen: ['/ip4/127.0.0.1/tcp/0']
    },
    transports: [tcp()],
    connectionEncrypters: [noise()],
    streamMuxers: [yamux()],
    services: {
      ping: ping({
        protocolPrefix: 'ipfs',
      }),
    },
  });

  const nodeData = {
    node,
    discoveredPeers: new Map(),
    createdAt: new Date().toISOString()
  };

  node.addEventListener('peer:discovery', (evt) => {
    const peerId = evt.detail.id.toString();
    const peerInfo = {
      id: peerId,
      multiaddrs: evt.detail.multiaddrs.map(addr => addr.toString()),
      discovered: new Date().toISOString()
    };
    
    nodeData.discoveredPeers.set(peerId, peerInfo);
    console.log(`[Node ${nodeId}] Discovered peer:`, peerId);
  });

  node.addEventListener('peer:connect', (evt) => {
    const peerId = evt.detail.id.toString();
    console.log(`[Node ${nodeId}] Connected to peer:`, peerId);
    
    if (nodeData.discoveredPeers.has(peerId)) {
      const peerInfo = nodeData.discoveredPeers.get(peerId);
      peerInfo.connected = true;
      peerInfo.lastConnected = new Date().toISOString();
      nodeData.discoveredPeers.set(peerId, peerInfo);
    }
  });

  node.addEventListener('peer:disconnect', (evt) => {
    const peerId = evt.detail.id.toString();
    console.log(`[Node ${nodeId}] Disconnected from peer:`, peerId);
    
    if (nodeData.discoveredPeers.has(peerId)) {
      const peerInfo = nodeData.discoveredPeers.get(peerId);
      peerInfo.connected = false;
      peerInfo.lastDisconnected = new Date().toISOString();
      nodeData.discoveredPeers.set(peerId, peerInfo);
    }
  });

  await node.start();
  nodes.set(nodeId, nodeData);
  console.log(`[Node ${nodeId}] libp2p node has started`);
  return nodeId;
};

const getNode = (nodeId) => {
  if (!nodes.has(nodeId)) {
    throw new Error(`Node with ID ${nodeId} does not exist`);
  }
  return nodes.get(nodeId);
};

const getNodeAddresses = (nodeId) => {
  const { node } = getNode(nodeId);
  const addresses = node.getMultiaddrs().map(addr => addr.toString());
  return addresses;
};

const getNodeSingleAddress = (nodeId) => {
  const { node } = getNode(nodeId);
  const addresses = node.getMultiaddrs();
  if (addresses.length === 0) {
    throw new Error('No addresses found');
  }
  return addresses[0].toString();
};

const getNodePeerId = (nodeId) => {
  const { node } = getNode(nodeId);
  return node.peerId.toString();
};

const pingPeer = async (nodeId, remoteAddr) => {
  const { node } = getNode(nodeId);
  const ma = multiaddr(remoteAddr);
  const latency = await node.services.ping.ping(ma);
  return latency;
};

const connectToPeer = async (nodeId, remoteAddr) => {
  const { node, discoveredPeers } = getNode(nodeId);
  
  try {
    const ma = multiaddr(remoteAddr);
    const peerIdStr = ma.getPeerId();
    
    if (peerIdStr && peerIdStr === node.peerId.toString()) {
      return {
        success: false,
        error: "Cannot connect to self. This is our own node."
      };
    }
    
    const connection = await node.dial(ma);
    const remotePeerId = connection.remotePeer.toString();
    
    discoveredPeers.set(remotePeerId, {
      id: remotePeerId,
      multiaddrs: [remoteAddr],
      connected: true,
      lastConnected: new Date().toISOString(),
      discovered: new Date().toISOString()
    });
    
    return {
      success: true,
      peerId: remotePeerId
    };
  } catch (error) {
    return {
      success: false,
      error: error.message
    };
  }
};


const getDiscoveredPeers = (nodeId) => {
  const { discoveredPeers } = getNode(nodeId);
  return Array.from(discoveredPeers.values());
};

const stopNode = async (nodeId) => {
  const { node } = getNode(nodeId);
  await node.stop();
  nodes.delete(nodeId);
  console.log(`[Node ${nodeId}] libp2p node has stopped`);
};

const listAllNodes = () => {
  const nodesList = [];
  for (const [nodeId, nodeData] of nodes.entries()) {
    nodesList.push({
      id: nodeId,
      peerId: nodeData.node.peerId.toString(),
      addresses: nodeData.node.getMultiaddrs().map(addr => addr.toString()),
      peerCount: nodeData.discoveredPeers.size,
      createdAt: nodeData.createdAt
    });
  }
  return nodesList;
};


app.post('/nodes', async (req, res) => {
  try {
    const { nodeId = `node-${Date.now()}` } = req.body;
    await createAndStartNode(nodeId);
    res.status(201).json({
      success: true,
      nodeId,
      message: `Node ${nodeId} created and started`
    });
  } catch (error) {
    res.status(400).json({
      success: false,
      error: error.message
    });
  }
});

app.get('/nodes', (req, res) => {
  try {
    const nodesList = listAllNodes();
    res.status(200).json({
      success: true,
      nodes: nodesList
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});

app.delete('/nodes/:nodeId', async (req, res) => {
  try {
    const { nodeId } = req.params;
    await stopNode(nodeId);
    res.status(200).json({
      success: true,
      message: `Node ${nodeId} stopped and removed`
    });
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message
    });
  }
});
app.get('/nodes/:nodeId/addresses', (req, res) => {
  try {
    const { nodeId } = req.params;
    const addresses = getNodeAddresses(nodeId);
    res.status(200).json({
      success: true,
      addresses
    });
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message
    });
  }
});

app.get('/nodes/:nodeId/single-address', (req, res) => {
  try {
    const { nodeId } = req.params;
    const address = getNodeSingleAddress(nodeId);
    res.status(200).type('text/plain').send(address);
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message
    });
  }
});

app.get('/nodes/:nodeId/peerid', (req, res) => {
  try {
    const { nodeId } = req.params;
    const peerId = getNodePeerId(nodeId);
    res.status(200).type('text/plain').send(peerId);
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message
    });
  }
});

app.post('/nodes/:nodeId/ping', async (req, res) => {
  try {
    const { nodeId } = req.params;
    const { remoteAddr } = req.body;
    
    if (!remoteAddr) {
      return res.status(400).json({
        success: false,
        error: 'Remote address is required'
      });
    }
    
    const latency = await pingPeer(nodeId, remoteAddr);
    res.status(200).json({
      success: true,
      latency
    });
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message
    });
  }
});

app.post('/nodes/:nodeId/connect', async (req, res) => {
  try {
    const { nodeId } = req.params;
    const { remoteAddr } = req.body;
    
    if (!remoteAddr) {
      return res.status(400).json({
        success: false,
        error: 'Remote address is required'
      });
    }
    
    const result = await connectToPeer(nodeId, remoteAddr);
    if (result.success) {
      res.status(200).json(result);
    } else {
      res.status(400).json(result);
    }
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message
    });
  }
});

app.get('/nodes/:nodeId/peers', (req, res) => {
  try {
    const { nodeId } = req.params;
    const peers = getDiscoveredPeers(nodeId);
    res.status(200).json({
      success: true,
      peers
    });
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message
    });
  }
});

const PORT = 3000;
app.listen(PORT, () => {
  console.log(`Multi-node libp2p HTTP server running on port ${PORT}`);
});

const stop = async () => {
  for (const [nodeId] of nodes.entries()) {
    try {
      await stopNode(nodeId);
    } catch (error) {
      console.error(`Error stopping node ${nodeId}:`, error);
    }
  }
  process.exit(0);
};

process.on('SIGTERM', stop);
process.on('SIGINT', stop);