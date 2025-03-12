import express from 'express';
import {
  createAndStartNode,
  getNodeAddresses,
  getNodeSingleAddress,
  getNodePeerId,
  pingPeer,
  connectToPeer,
  getDiscoveredPeers,
  stopNode,
  listAllNodes,
  subscribeToTopic,
  unsubscribeFromTopic,
  publishToTopic,
  getSubscriptions,
} from './libp2p.js';

const app = express();
app.use(express.json());

const PORT = 3000;

// Create and start a new node
app.post('/nodes', async (req, res) => {
  try {
    const { nodeId = `node-${Date.now()}` } = req.body;
    await createAndStartNode(nodeId);
    res.status(201).json({
      success: true,
      nodeId,
      message: `Node ${nodeId} created and started`,
    });
  } catch (error) {
    res.status(400).json({
      success: false,
      error: error.message,
    });
  }
});

// List all nodes
app.get('/nodes', (req, res) => {
  try {
    const nodesList = listAllNodes();
    res.status(200).json({
      success: true,
      nodes: nodesList,
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: error.message,
    });
  }
});

// Stop and remove a node
app.delete('/nodes/:nodeId', async (req, res) => {
  try {
    const { nodeId } = req.params;
    await stopNode(nodeId);
    res.status(200).json({
      success: true,
      message: `Node ${nodeId} stopped and removed`,
    });
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message,
    });
  }
});

// Get addresses of a node
app.get('/nodes/:nodeId/addresses', (req, res) => {
  try {
    const { nodeId } = req.params;
    const addresses = getNodeAddresses(nodeId);
    res.status(200).json({
      success: true,
      addresses,
    });
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message,
    });
  }
});

// Get a single address of a node
app.get('/nodes/:nodeId/single-address', (req, res) => {
  try {
    const { nodeId } = req.params;
    const address = getNodeSingleAddress(nodeId);
    res.status(200).type('text/plain').send(address);
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message,
    });
  }
});

// Get the peer ID of a node
app.get('/nodes/:nodeId/peerid', (req, res) => {
  try {
    const { nodeId } = req.params;
    const peerId = getNodePeerId(nodeId);
    res.status(200).type('text/plain').send(peerId);
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message,
    });
  }
});

// Ping a remote peer
app.post('/nodes/:nodeId/ping', async (req, res) => {
  try {
    const { nodeId } = req.params;
    const { remoteAddr } = req.body;

    if (!remoteAddr) {
      return res.status(400).json({
        success: false,
        error: 'Remote address is required',
      });
    }

    const latency = await pingPeer(nodeId, remoteAddr);
    res.status(200).json({
      success: true,
      latency,
    });
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message,
    });
  }
});

// Connect to a remote peer
app.post('/nodes/:nodeId/connect', async (req, res) => {
  try {
    const { nodeId } = req.params;
    const { remoteAddr } = req.body;

    if (!remoteAddr) {
      return res.status(400).json({
        success: false,
        error: 'Remote address is required',
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
      error: error.message,
    });
  }
});

// Get discovered peers for a node
app.get('/nodes/:nodeId/peers', (req, res) => {
  try {
    const { nodeId } = req.params;
    const peers = getDiscoveredPeers(nodeId);
    res.status(200).json({
      success: true,
      peers,
    });
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message,
    });
  }
});

// Subscribe to a PubSub topic
app.post('/nodes/:nodeId/pubsub/subscribe', async (req, res) => {
  try {
    const { nodeId } = req.params;
    const { topic } = req.body;

    if (!topic) {
      return res.status(400).json({
        success: false,
        error: 'Topic is required',
      });
    }

    await subscribeToTopic(nodeId, topic);
    res.status(200).json({
      success: true,
      message: `Subscribed to topic ${topic}`,
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: error.message,
    });
  }
});

// Unsubscribe from a PubSub topic
app.post('/nodes/:nodeId/pubsub/unsubscribe', async (req, res) => {
  try {
    const { nodeId } = req.params;
    const { topic } = req.body;

    if (!topic) {
      return res.status(400).json({
        success: false,
        error: 'Topic is required',
      });
    }

    await unsubscribeFromTopic(nodeId, topic);
    res.status(200).json({
      success: true,
      message: `Unsubscribed from topic ${topic}`,
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: error.message,
    });
  }
});

// Publish a message to a PubSub topic
app.post('/nodes/:nodeId/pubsub/publish', async (req, res) => {
  try {
    const { nodeId } = req.params;
    const { topic, message } = req.body;

    if (!topic || !message) {
      return res.status(400).json({
        success: false,
        error: 'Topic and message are required',
      });
    }

    await publishToTopic(nodeId, topic, message);
    res.status(200).json({
      success: true,
      message: `Published message to topic ${topic}`,
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: error.message,
    });
  }
});

// Get subscriptions for a node
app.get('/nodes/:nodeId/pubsub/subscriptions', (req, res) => {
  try {
    const { nodeId } = req.params;
    const subscriptions = getSubscriptions(nodeId);
    res.status(200).json({
      success: true,
      subscriptions,
    });
  } catch (error) {
    res.status(404).json({
      success: false,
      error: error.message,
    });
  }
});

// Start the server
app.listen(PORT, () => {
  console.log(`Multi-node libp2p HTTP server running on port ${PORT}`);
});