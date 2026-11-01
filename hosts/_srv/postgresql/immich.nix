{...}: {
  services.postgresql.settings = {
    # Cache settings
    shared_buffers = "16GB";
    effective_cache_size = "48GB";
    work_mem = "256MB";
    maintenance_work_mem = "2GB";
    # WAL / checkpoints
    max_wal_size = "4GB";
    min_wal_size = "1GB";
    checkpoint_completion_target = 0.9;
    wal_buffers = "64MB";
    # Planner
    random_page_cost = 1.1;
    effective_io_concurrency = 200;
    max_connections = 200;  # immich opens a pool
    max_parallel_workers_per_gather = 4;
    max_parallel_workers = 8;
    max_parallel_maintenance_workers = 4;
  };
}
