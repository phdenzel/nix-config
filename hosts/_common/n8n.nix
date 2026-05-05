{...}: {
  services = {
    n8n = {
      enable = true;
      openFirewall = true;
      environment = {
        N8N_PORT = 5678;
        N8N_DIAGNOSTICS_ENABLED = false; # disables "Ask AI" in the code node
        N8N_VERSION_NOTIFICATIONS_ENABLED = false;
      };
    };
  };
}
