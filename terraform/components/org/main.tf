module "org-terraform" {
  source = "../../modules/project"

  project_name    = "org-terraform"
  org_id          = "947273970991"
  project_id      = "flowercluster-org-terraform"
  billing_account = "015DC6-23F403-4CEB5F"

  apis = [
    "iam.googleapis.com",
    "cloudbilling.googleapis.com",
    "cloudresourcemanager.googleapis.com",
  ]
}

module "org-network" {
  source = "../../modules/network"

  project_id   = module.org-terraform.project.project_id
  network_name = "garden01"

  subnets = [
    {
      subnet_name           = "dmz-01"
      subnet_ip             = "10.10.10.0/24"
      subnet_region         = "us-east1"
      subnet_private_access = "true"
    },
    {
      subnet_name           = "internal-01"
      subnet_ip             = "10.0.10.0/24"
      subnet_region         = "us-east1"
      subnet_private_access = "true"
    }
  ]
}

module "sahaquiel" {
  /*
  Kiseki no kachi wa
  "The Value of Miracles"
  (奇跡の価値は)

  "She said, 'Don't make others suffer for your personal hatred.'"
  */

  source = "../../modules/domain"

  org_id          = "947273970991"
  billing_account = "015DC6-23F403-4CEB5F"
  domain_name     = "sahaquiel"

  shared_vpc = module.org-terraform.project.project_id
  shared_vpc_subnets = [
    module.org-network.vpc.subnets["us-east1/internal-01"].self_link
  ]
}
