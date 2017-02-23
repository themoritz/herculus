{
  network.description = "Herculus";

  machine =
    { config, pkgs, ... }:

    let
      domain = "herculus.io";

      # Landing page
      landingPage = import ./../landing-page/client { inherit pkgs; };

      # App server 
      herculusServer = import ./../app/server { inherit pkgs; };
      herculusClient = import ./../app/client {
        inherit pkgs;
        apiUrl = "https://app.${domain}/api/";
        websocketUrl = "wss://app.${domain}/websocket";
      };

      # Documentation
      documentation = import ./../doc { inherit pkgs; };

      # Forwarded by AWS load balancer
      sslPort = 1443;
      appServerPort = 3000;

    in {
  
      nixpkgs.config.allowUnfree = true;
      nixpkgs.config.allowBroken = true;

      networking.firewall.allowedTCPPorts = [ 80 sslPort ];

      # We want to use sendmail
      networking.defaultMailServer = import ./smtp.nix;

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
              --port ${toString appServerPort} \
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
            "${domain}" = {
              extraConfig = ''
                return 301 https://www.${domain}$request_uri;
              '';
            };
            # App
            "app.${domain}" = {
              port = sslPort;
              locations = {
                "/websocket" = {
                  proxyPass = "http://localhost:${toString appServerPort}";
                  extraConfig = ''
                    proxy_http_version 1.1;
                    proxy_set_header Upgrade $http_upgrade;
                    proxy_set_header Connection "Upgrade";
                  '';
                };
                "/api" = {
                  proxyPass = "http://localhost:${toString appServerPort}";
                  extraConfig = ''
                    proxy_set_header X-Real-IP $remote_addr;
                    proxy_set_header X-Forwarded-For $remote_addr;
                    proxy_set_header Host $host;
                  '';
                };
                "/doc".extraConfig = ''
                  alias ${documentation};
                '';
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
