{
  network.description = "Herculus";

  machine =
    { config, pkgs, ... }:

    let
      # Landing page
      landingPage = import ./../landing-page/client { inherit pkgs; };

      # App server 
      herculusServer = import ./../app/server { inherit pkgs; };
      herculusClient = import ./../app/client {
        inherit pkgs;
        apiUrl = "https://app.herculus.io/api";
        websocketUrl = "wss://app.herculus.io/websocket";
      };

      # Documentation
      documentation = import ./../doc { inherit pkgs; };

      # Forwarded by AWS load balancer
      sslPort = 1443;
      appServerPort = 3000;

    in {
  
      nixpkgs.config.allowUnfree = true;

      networking.firewall.allowedTCPPorts = [ 80 sslPort ];

      environment.systemPackages = [
        pkgs.mongodb-tools
      ];

      fileSystems."/data" = {
        fsType = "ext4";
        device = "/dev/xvdf";
      };

      systemd.services = {

        appServer = {
          description = "Herculus app server";
          wantedBy = [ "multi-user.target" ];
          after = [ "data.mount" "network.target" "mongodb" ];
          serviceConfig.ExecStart = ''
            ${herculusServer}/bin/server-exe \
              --port ${appServerPort} \
              --database-name herculus \
          '';
              #--asset-directory ${herculusClient}
        };

      };

      services = {

        nginx = {
          enable = true;
          recommendedGzipSettings = true;
          virtualHosts = {
            # Redirect non-https traffic
            "_1" = {
              port = 80;
              default = true;
              extraConfig = ''
                return 301 https://$host$request_uri;
              '';
            };
            # Redirect traffic without subdomain
            "herculus.io" = {
              extraConfig = ''
                return 301 https://www.herculus.io$request_uri;
              '';
            };
            # App
            "app.herculus.io" = {
              port = sslPort;
              locations = {
                "/websocket" = {
                  proxyPass = "http://localhost:${appServerPort}";
                  extraConfig = ''
                    proxy_http_version 1.1;
                    proxy_set_header Upgrade $http_upgrade;
                    proxy_set_header Connection "Upgrade";
                  '';
                };
                "/api" = {
                  proxyPass = "http://localhost:${appServerPort}";
                  extraConfig = ''
                    proxy_set_header X-Real-IP $remote_addr;
                    proxy_set_header X-Forwarded-For $remote_addr;
                    proxy_set_header Host $host;
                  '';
                };
                "/doc".root = "${documentation}";
                "/" = {
                  root = "${herculusClient}";
                  extraConfig = ''
                    gzip_static on;
                  '';
                };
              };
            };
            # Landing page
            "_3" = {
              default = true;
              port = sslPort;
              locations = {
                "/".root = "${landingPage}";
              };
            };
          };
        };

        mongodb = {
          enable = true;
          dbpath = "/data/mongodb";
        };

      };

    };
}
