module "dns-public-zone" {
  source     = "terraform-google-modules/cloud-dns/google"
  version    = "3.0.0"
  project_id = "flowercluster-org-terraform"
  type       = "public"
  name       = "flowercluster-io-public"
  domain     = "flowercluster.io."
}

module "dns-private-zone" {
  source     = "terraform-google-modules/cloud-dns/google"
  version    = "3.0.0"
  project_id = "flowercluster-org-terraform"
  type       = "private"
  name       = "flowercluster-io-private"
  domain     = "flowercluster.io."

  private_visibility_config_networks = [
    module.org-network.vpc.network.network_self_link
  ]
}
