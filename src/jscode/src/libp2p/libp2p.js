import { createLibp2p } from 'libp2p';
import { tcp } from '@libp2p/tcp';
import { noise } from '@chainsafe/libp2p-noise';
import { yamux } from '@chainsafe/libp2p-yamux';
import { multiaddr } from 'multiaddr';
import { ping } from '@libp2p/ping';
import { gossipsub } from '@chainsafe/libp2p-gossipsub';
import { identify } from '@libp2p/identify';
import { createHelia } from 'helia';
import { unixfs } from '@helia/unixfs';

const nodes = new Map();

const inspectEventStructure = (evt, nodeId, eventType) => {
  console.log(`[Node ${nodeId}] ${eventType} event structure inspection:`);
  console.log(`[Node ${nodeId}] Event type: ${evt.type}`);
  console.log(`[Node ${nodeId}] Event detail available: ${evt.detail !== undefined}`);

  if (evt.detail) {
    console.log(`[Node ${nodeId}] Detail properties: ${Object.keys(evt.detail).join(', ')}`);

    if (evt.detail.connection) {
      console.log(`[Node ${nodeId}] Connection properties: ${Object.keys(evt.detail.connection).join(', ')}`);
    }
  }

  if (evt.target) {
    console.log(`[Node ${nodeId}] Event target type: ${typeof evt.target}`);
    if (typeof evt.target === 'object') {
      console.log(`[Node ${nodeId}] Event target properties: ${Object.keys(evt.target).join(', ')}`);
    }
  }
};

export const createAndStartNode = async (nodeId) => {
  if (nodes.has(nodeId)) {
    throw new Error(`Node with ID ${nodeId} already exists`);
  }

  // Create libp2p node
  const libp2pNode = await createLibp2p({
    addresses: {
      listen: ['/ip4/127.0.0.1/tcp/0'],
    },
    transports: [tcp()],
    connectionEncrypters: [noise()],
    streamMuxers: [yamux()],
    services: {
      ping: ping({
        protocolPrefix: 'ipfs',
      }),
      identify: identify(),
      pubsub: gossipsub(),
    },
  });

  // Create Helia node using the existing libp2p node
  const heliaNode = await createHelia({
    libp2p: libp2pNode,
  });

  const nodeData = {
    libp2pNode,
    heliaNode,
    discoveredPeers: new Map(),
    createdAt: new Date().toISOString(),
    subscriptions: new Set(),
  };

  // Event listeners for libp2p node
  libp2pNode.services.pubsub.addEventListener('subscription-change', (evt) => {
    const { peerId, subscriptions } = evt.detail;
    console.log(`[Node ${nodeId}] Peer ${peerId} changed subscriptions:`, subscriptions);
  });

  libp2pNode.services.pubsub.addEventListener('message', (evt) => {
    const { from, topic, data } = evt.detail;
    console.log(`[Node ${nodeId}] Received message from ${from} on topic ${topic}:`, data.toString());
  });

  libp2pNode.addEventListener('peer:discovery', (evt) => {
    if (!evt.detail || !evt.detail.id) {
      console.error(`[Node ${nodeId}] Invalid peer discovery event:`, evt);
      return;
    }

    const peerId = evt.detail.id.toString();
    const peerInfo = {
      id: peerId,
      multiaddrs: evt.detail.multiaddrs.map((addr) => addr.toString()),
      discovered: new Date().toISOString(),
    };

    nodeData.discoveredPeers.set(peerId, peerInfo);
    console.log(`[Node ${nodeId}] Discovered peer:`, peerId);
  });

  libp2pNode.addEventListener('peer:connect', (evt) => {
    console.log(`[Node ${nodeId}] Peer connect event received`);
    inspectEventStructure(evt, nodeId, 'peer:connect');

    let peerId = null;

    if (evt.detail && evt.detail.remotePeer) {
      peerId = evt.detail.remotePeer.toString();
      console.log(`[Node ${nodeId}] Found peer ID in detail.remotePeer: ${peerId}`);
    } else if (evt.detail && evt.detail.connection && evt.detail.connection.remotePeer) {
      peerId = evt.detail.connection.remotePeer.toString();
      console.log(`[Node ${nodeId}] Found peer ID in detail.connection.remotePeer: ${peerId}`);
    } else if (evt.detail && evt.detail.id) {
      peerId = evt.detail.id.toString();
      console.log(`[Node ${nodeId}] Found peer ID in detail.id: ${peerId}`);
    } else if (evt.target && evt.target.remotePeer) {
      peerId = evt.target.remotePeer.toString();
      console.log(`[Node ${nodeId}] Found peer ID in target.remotePeer: ${peerId}`);
    } else {
      try {
        const peers = libp2pNode.getPeers();
        if (peers && peers.length > 0) {
          peerId = peers[0].toString();
          console.log(`[Node ${nodeId}] Using first connected peer as fallback: ${peerId}`);
        }
      } catch (err) {
        console.error(`[Node ${nodeId}] Error accessing peers: ${err.message}`);
      }
    }

    if (!peerId) {
      console.error(`[Node ${nodeId}] Could not extract peer ID from connect event`);
      return;
    }

    console.log(`[Node ${nodeId}] Connected to peer: ${peerId}`);

    if (nodeData.discoveredPeers.has(peerId)) {
      const peerInfo = nodeData.discoveredPeers.get(peerId);
      peerInfo.connected = true;
      peerInfo.lastConnected = new Date().toISOString();
      nodeData.discoveredPeers.set(peerId, peerInfo);
    } else {
      nodeData.discoveredPeers.set(peerId, {
        id: peerId,
        connected: true,
        lastConnected: new Date().toISOString(),
        discovered: new Date().toISOString(),
      });
    }
  });

  libp2pNode.addEventListener('peer:disconnect', (evt) => {
    console.log(`[Node ${nodeId}] Peer disconnect event received`);
    inspectEventStructure(evt, nodeId, 'peer:disconnect');

    let peerId = null;

    if (evt.detail && evt.detail.remotePeer) {
      peerId = evt.detail.remotePeer.toString();
    } else if (evt.detail && evt.detail.connection && evt.detail.connection.remotePeer) {
      peerId = evt.detail.connection.remotePeer.toString();
    } else if (evt.detail && evt.detail.id) {
      peerId = evt.detail.id.toString();
    } else if (evt.target && evt.target.remotePeer) {
      peerId = evt.target.remotePeer.toString();
    }

    if (!peerId) {
      console.error(`[Node ${nodeId}] Could not extract peer ID from disconnect event`);
      return;
    }

    console.log(`[Node ${nodeId}] Disconnected from peer: ${peerId}`);

    if (nodeData.discoveredPeers.has(peerId)) {
      const peerInfo = nodeData.discoveredPeers.get(peerId);
      peerInfo.connected = false;
      peerInfo.lastDisconnected = new Date().toISOString();
      nodeData.discoveredPeers.set(peerId, peerInfo);
    }
  });

  await libp2pNode.start();
  nodes.set(nodeId, nodeData);
  console.log(`[Node ${nodeId}] libp2p and Helia nodes have started`);
  return nodeId;
};

export const getNode = (nodeId) => {
  if (!nodes.has(nodeId)) {
    throw new Error(`Node with ID ${nodeId} does not exist`);
  }
  return nodes.get(nodeId);
};

export const getNodeAddresses = (nodeId) => {
  const { libp2pNode } = getNode(nodeId);
  const addresses = libp2pNode.getMultiaddrs().map((addr) => addr.toString());
  return addresses;
};

export const getNodeSingleAddress = (nodeId) => {
  const { libp2pNode } = getNode(nodeId);
  const addresses = libp2pNode.getMultiaddrs();
  if (addresses.length === 0) {
    throw new Error('No addresses found');
  }
  return addresses[0].toString();
};

export const getNodePeerId = (nodeId) => {
  const { libp2pNode } = getNode(nodeId);
  return libp2pNode.peerId.toString();
};

export const pingPeer = async (nodeId, remoteAddr) => {
  const { libp2pNode } = getNode(nodeId);
  const ma = multiaddr(remoteAddr);
  const latency = await libp2pNode.services.ping.ping(ma);
  return latency;
};

export const connectToPeer = async (nodeId, remoteAddr) => {
  const { libp2pNode, discoveredPeers } = getNode(nodeId);

  try {
    const ma = multiaddr(remoteAddr);
    const remotePeerIdStr = ma.getPeerId();

    if (!remotePeerIdStr) {
      return {
        success: false,
        error: 'Invalid remote address: No peer ID found.',
      };
    }

    if (remotePeerIdStr === libp2pNode.peerId.toString()) {
      return {
        success: false,
        error: 'Cannot connect to self. This is our own node.',
      };
    }

    console.log(`[Node ${nodeId}] Attempting to dial: ${remoteAddr}`);
    const connection = await libp2pNode.dial(ma);
    console.log(`[Node ${nodeId}] Dial result:`, connection);

    discoveredPeers.set(remotePeerIdStr, {
      id: remotePeerIdStr,
      multiaddrs: [remoteAddr],
      connected: true,
      lastConnected: new Date().toISOString(),
      discovered: new Date().toISOString(),
    });

    return {
      success: true,
      peerId: remotePeerIdStr,
    };
  } catch (error) {
    console.error(`[Node ${nodeId}] Connection error:`, error);
    return {
      success: false,
      error: error.message,
    };
  }
};

export const getDiscoveredPeers = (nodeId) => {
  const { discoveredPeers } = getNode(nodeId);
  return Array.from(discoveredPeers.values());
};

export const stopNode = async (nodeId) => {
  const { libp2pNode, heliaNode } = getNode(nodeId);
  await libp2pNode.stop();
  await heliaNode.stop();
  nodes.delete(nodeId);
  console.log(`[Node ${nodeId}] libp2p and Helia nodes have stopped`);
};

export const listAllNodes = () => {
  const nodesList = [];
  for (const [nodeId, nodeData] of nodes.entries()) {
    nodesList.push({
      id: nodeId,
      peerId: nodeData.libp2pNode.peerId.toString(),
      addresses: nodeData.libp2pNode.getMultiaddrs().map((addr) => addr.toString()),
      peerCount: nodeData.discoveredPeers.size,
      createdAt: nodeData.createdAt,
    });
  }
  return nodesList;
};

export const subscribeToTopic = async (nodeId, topic) => {
  const { libp2pNode, subscriptions } = getNode(nodeId);
  await libp2pNode.services.pubsub.subscribe(topic);
  subscriptions.add(topic);
};

export const unsubscribeFromTopic = async (nodeId, topic) => {
  const { libp2pNode, subscriptions } = getNode(nodeId);
  await libp2pNode.services.pubsub.unsubscribe(topic);
  subscriptions.delete(topic);
};

export const publishToTopic = async (nodeId, topic, message) => {
  const { libp2pNode } = getNode(nodeId);
  await libp2pNode.services.pubsub.publish(topic, new TextEncoder().encode(message));
};

export const getSubscriptions = (nodeId) => {
  const { subscriptions } = getNode(nodeId);
  return Array.from(subscriptions);
};

// Helia-specific functions
export const addFileToIPFS = async (nodeId, fileContent) => {
  const { heliaNode } = getNode(nodeId);
  const fs = unixfs(heliaNode);
  const cid = await fs.addBytes(new TextEncoder().encode(fileContent));
  return cid.toString();
};

export const getFileFromIPFS = async (nodeId, cid) => {
  const { heliaNode } = getNode(nodeId); 
  const fs = unixfs(heliaNode);
  const data = [];
  for await (const chunk of fs.cat(cid)) {
    data.push(chunk);
  }
  return Buffer.concat(data).toString();
};