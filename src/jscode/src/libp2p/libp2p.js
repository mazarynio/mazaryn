import { createLibp2p } from 'libp2p';
import { tcp } from '@libp2p/tcp';
import { noise } from '@chainsafe/libp2p-noise';
import { yamux } from '@chainsafe/libp2p-yamux';
import { multiaddr } from 'multiaddr';
import { ping } from '@libp2p/ping';
import { gossipsub } from '@chainsafe/libp2p-gossipsub';
import { identify } from '@libp2p/identify';

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

  const node = await createLibp2p({
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

  const nodeData = {
    node,
    discoveredPeers: new Map(),
    createdAt: new Date().toISOString(),
    subscriptions: new Set(),
  };

  node.services.pubsub.addEventListener('subscription-change', (evt) => {
    const { peerId, subscriptions } = evt.detail;
    console.log(`[Node ${nodeId}] Peer ${peerId} changed subscriptions:`, subscriptions);
  });

  node.services.pubsub.addEventListener('message', (evt) => {
    const { from, topic, data } = evt.detail;
    console.log(`[Node ${nodeId}] Received message from ${from} on topic ${topic}:`, data.toString());
  });

  node.addEventListener('peer:discovery', (evt) => {
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

  node.addEventListener('peer:connect', (evt) => {
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
        const peers = node.getPeers();
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

  node.addEventListener('peer:disconnect', (evt) => {
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

  await node.start();
  nodes.set(nodeId, nodeData);
  console.log(`[Node ${nodeId}] libp2p node has started`);
  return nodeId;
};

export const getNode = (nodeId) => {
  if (!nodes.has(nodeId)) {
    throw new Error(`Node with ID ${nodeId} does not exist`);
  }
  return nodes.get(nodeId);
};

export const getNodeAddresses = (nodeId) => {
  const { node } = getNode(nodeId);
  const addresses = node.getMultiaddrs().map((addr) => addr.toString());
  return addresses;
};

export const getNodeSingleAddress = (nodeId) => {
  const { node } = getNode(nodeId);
  const addresses = node.getMultiaddrs();
  if (addresses.length === 0) {
    throw new Error('No addresses found');
  }
  return addresses[0].toString();
};

export const getNodePeerId = (nodeId) => {
  const { node } = getNode(nodeId);
  return node.peerId.toString();
};

export const pingPeer = async (nodeId, remoteAddr) => {
  const { node } = getNode(nodeId);
  const ma = multiaddr(remoteAddr);
  const latency = await node.services.ping.ping(ma);
  return latency;
};

export const connectToPeer = async (nodeId, remoteAddr) => {
  const { node, discoveredPeers } = getNode(nodeId);

  try {
    const ma = multiaddr(remoteAddr);
    const remotePeerIdStr = ma.getPeerId();

    if (!remotePeerIdStr) {
      return {
        success: false,
        error: 'Invalid remote address: No peer ID found.',
      };
    }

    if (remotePeerIdStr === node.peerId.toString()) {
      return {
        success: false,
        error: 'Cannot connect to self. This is our own node.',
      };
    }

    console.log(`[Node ${nodeId}] Attempting to dial: ${remoteAddr}`);
    const connection = await node.dial(ma);
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
  const { node } = getNode(nodeId);
  await node.stop();
  nodes.delete(nodeId);
  console.log(`[Node ${nodeId}] libp2p node has stopped`);
};

export const listAllNodes = () => {
  const nodesList = [];
  for (const [nodeId, nodeData] of nodes.entries()) {
    nodesList.push({
      id: nodeId,
      peerId: nodeData.node.peerId.toString(),
      addresses: nodeData.node.getMultiaddrs().map((addr) => addr.toString()),
      peerCount: nodeData.discoveredPeers.size,
      createdAt: nodeData.createdAt,
    });
  }
  return nodesList;
};

export const subscribeToTopic = async (nodeId, topic) => {
  const { node, subscriptions } = getNode(nodeId);
  await node.services.pubsub.subscribe(topic);
  subscriptions.add(topic);
};

export const unsubscribeFromTopic = async (nodeId, topic) => {
  const { node, subscriptions } = getNode(nodeId);
  await node.services.pubsub.unsubscribe(topic);
  subscriptions.delete(topic);
};

export const publishToTopic = async (nodeId, topic, message) => {
  const { node } = getNode(nodeId);
  await node.services.pubsub.publish(topic, new TextEncoder().encode(message));
};

export const getSubscriptions = (nodeId) => {
  const { subscriptions } = getNode(nodeId);
  return Array.from(subscriptions);
};