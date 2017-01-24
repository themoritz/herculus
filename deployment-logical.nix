{
  network.description = "Herculus";

  machine =
    { config, pkgs, ... }:

    let
      # Landing page
      landingPage = import ./landing-page/client { inherit pkgs; };
      subscribers = import ./landing-page/server { inherit pkgs; };

      # App
      herculusServer = import ./hexl/server { inherit pkgs; };
      herculusStatic = import ./hexl {
        inherit pkgs;
        apiUrl = "https://app.herculus.io/api";
        websocketUrl = "wss://app.herculus.io/websocket";
      };

      # Forwarded by AWS load balancer
      sslPort = 1443;

    in {
  
      nixpkgs.config.allowUnfree = true;

      networking.firewall.allowedTCPPorts = [ 80 sslPort ];

      environment.systemPackages = [
        pkgs.mongodb-tools
        subscribers
      ];

      fileSystems."/data" = {
        fsType = "ext4";
        device = "/dev/xvdf";
      };

      systemd.services = {

        subscribers = {
          description = "Herculus subscribers endpoint";
          wantedBy = [ "multi-user.target" ];
          after = [ "data.mount" "network.target" "mongodb" ];
          serviceConfig.ExecStart = ''
            ${subscribers}/bin/herculus-subscribers-exe \
              --port 3000 \
              --database-name herculus-subscribers
          '';
        };

        appServer = {
          description = "Herculus app server";
          wantedBy = [ "multi-user.target" ];
          after = [ "data.mount" "network.target" "mongodb" ];
          serviceConfig.ExecStart = ''
            ${herculusServer}/bin/server-exe \
              --port 3001 \
              --database-name herculus \
          '';
              #--asset-directory ${herculusStatic}
        };

      };

      services = {

        nginx = {
          enable = true;
          recommendedGzipSettings = true;
          virtualHosts = {
            "_1" = {
              port = 80;
              default = true;
              extraConfig = ''
                return 301 https://$host$request_uri;
              '';
            };
            "herculus.io" = {
              extraConfig = ''
                return 301 https://www.herculus.io$request_uri;
              '';
            };
            "app.herculus.io" = {
              port = sslPort;
              locations = {
                "/websocket" = {
                  proxyPass = "http://localhost:3001";
                  extraConfig = ''
                    proxy_http_version 1.1;
                    proxy_set_header Upgrade $http_upgrade;
                    proxy_set_header Connection "Upgrade";
                  '';
                };
                "/api" = {
                  proxyPass = "http://localhost:3001";
                  extraConfig = ''
                    proxy_set_header X-Real-IP $remote_addr;
                    proxy_set_header X-Forwarded-For $remote_addr;
                    proxy_set_header Host $host;
                  '';
                };
                "/" = {
                  root = "${herculusStatic}";
                  extraConfig = ''
                    gzip_static on;
                  '';
                };
              };
            };
            "_3" = {
              default = true;
              port = sslPort;
              locations = {
                "/subscriber/" = {
                  proxyPass = "http://localhost:3000";
                  extraConfig = ''
                    proxy_set_header X-Real-IP $remote_addr;
                    proxy_set_header X-Forwarded-For $remote_addr;
                    proxy_set_header Host $host;
                  '';
                };
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
