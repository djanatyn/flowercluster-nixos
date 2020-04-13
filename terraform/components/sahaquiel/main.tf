locals {
  org = data.terraform_remote_state.org
}
module "sahaquiel-nonprod" {
  source = "../../modules/project"

  project_name    = "Sahaquiel Non-Prod"
  project_id      = "sahaquiel-nonprod"
  org_id          = local.org.outputs.org_id
  billing_account = local.org.outputs.billing_account

  shared_vpc = local.org.outputs.org-terraform.project.project_id

  shared_vpc_subnets = [
    local.org.outputs.network.vpc.subnets["us-east1/internal-01"].self_link
  ]

  folder_id = local.org.outputs.domains.sahaquiel.folders.environment.ids.nonprod
}

module "nixos_image" {
  source        = "github.com/tweag/terraform-nixos/google_image_nixos"
  nixos_version = "latest"
}

resource "google_compute_instance" "core" {
  name         = "core"
  machine_type = "n1-standard-2"
  zone         = "us-east1-b"

  boot_disk {
    initialize_params {
      image = "${module.nixos_image.self_link}"
    }
  }

  network_interface {
    subnetwork = local.org.outputs.network.vpc.subnets["us-east1/internal-01"].self_link
  }
}
