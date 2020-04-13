locals {
  default_apis = [
    "iam.googleapis.com",
    "cloudbilling.googleapis.com",
  ]
}

module "domain-folder" {
  source  = "terraform-google-modules/folders/google"
  version = "~> 2.0"

  parent = "organizations/${var.org_id}"

  names     = [var.domain_name]
  set_roles = false
}

module "terraform-project" {
  source = "github.com/djanatyn/terraform-google-project-factory"

  name                 = "${var.domain_name}-terraform"
  project_id           = "${var.domain_name}-terraform"
  org_id               = var.org_id
  billing_account      = var.billing_account
  shared_vpc           = var.shared_vpc
  skip_gcloud_download = true

  folder_id = module.domain-folder.id

  shared_vpc_subnets = var.shared_vpc_subnets

  activate_apis = toset(concat(local.default_apis, var.apis))
}

module "environment-folders" {
  source  = "terraform-google-modules/folders/google"
  version = "~> 2.0"

  parent = module.domain-folder.id

  names = [
    "nonprod",
    "prod",
  ]

  set_roles = false
}
