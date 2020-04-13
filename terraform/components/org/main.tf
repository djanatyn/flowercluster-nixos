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

module "org-service-account" {
  source     = "terraform-google-modules/service-accounts/google"
  version    = "~> 2.0"
  project_id = module.org-terraform.project.project_id
  names      = ["gardener"]
  project_roles = [
    "${module.org-terraform.project.project_id}=>roles/owner"
  ]
}

module "org-network" {
  source = "../../modules/network"

  project_id   = module.org-terraform.project.project_id
  network_name = "flowerbed01"

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
