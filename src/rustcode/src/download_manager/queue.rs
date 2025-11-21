use crate::download_manager::types::{DownloadPriority, DownloadRequest};
use dashmap::DashMap;
use std::collections::BinaryHeap;
use std::sync::Arc;
use tokio::sync::Semaphore;
use uuid::Uuid;

#[derive(Eq, PartialEq)]
struct QueuedDownload {
    id: Uuid,
    priority: DownloadPriority,
    queued_at: std::time::SystemTime,
}

impl Ord for QueuedDownload {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.priority
            .cmp(&other.priority)
            .then_with(|| other.queued_at.cmp(&self.queued_at))
    }
}

impl PartialOrd for QueuedDownload {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub struct DownloadQueue {
    queue: Arc<parking_lot::Mutex<BinaryHeap<QueuedDownload>>>,
    downloads: Arc<DashMap<Uuid, DownloadRequest>>,
    semaphore: Arc<Semaphore>,
}

impl DownloadQueue {
    pub fn new(max_concurrent: usize) -> Self {
        Self {
            queue: Arc::new(parking_lot::Mutex::new(BinaryHeap::new())),
            downloads: Arc::new(DashMap::new()),
            semaphore: Arc::new(Semaphore::new(max_concurrent)),
        }
    }

    pub fn enqueue(&self, id: Uuid, request: DownloadRequest) {
        let priority = request.priority;
        self.downloads.insert(id, request);

        let mut queue = self.queue.lock();
        queue.push(QueuedDownload {
            id,
            priority,
            queued_at: std::time::SystemTime::now(),
        });
    }

    pub async fn dequeue(&self) -> Option<(Uuid, DownloadRequest)> {
        let _permit = self.semaphore.acquire().await.ok()?;

        let id = {
            let mut queue = self.queue.lock();
            queue.pop()?.id
        };

        self.downloads
            .remove(&id)
            .map(|(id, request)| (id, request))
    }

    pub fn remove(&self, id: &Uuid) -> Option<DownloadRequest> {
        self.downloads.remove(id).map(|(_, request)| request)
    }

    pub fn get(&self, id: &Uuid) -> Option<DownloadRequest> {
        self.downloads.get(id).map(|entry| entry.clone())
    }

    pub fn len(&self) -> usize {
        self.queue.lock().len()
    }

    pub fn is_empty(&self) -> bool {
        self.queue.lock().is_empty()
    }
}
