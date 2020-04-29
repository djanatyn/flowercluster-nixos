job "gitlab" {
  datacenters = ["dc1"]

  type = "service"

  group "gitlab" {
    task "omnibus" {
      driver = "docker"

      resources {
        network {
          mode = "host"
          port "http" {
            static = 80
          }
        }

        memory = 4096
      }

      config {
        image = "gitlab/gitlab-ce:latest"
        volumes = [
          "/var/lib/gitlab/config:/etc/gitlab",
          "/var/lib/gitlab/logs:/var/log/gitlab",
          "/var/lib/gitlab/data:/var/opt/gitlab"
        ]
      }
    }
  }
}
