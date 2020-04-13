locals {
  default_apis = [
    "iam.googleapis.com",
    "cloudbilling.googleapis.com",
  ]
}
module "project-factory" {
  source = "github.com/djanatyn/terraform-google-project-factory"

  name                 = var.project_name
  random_project_id    = var.project_id == null ? true : false
  project_id           = var.project_id
  org_id               = var.org_id
  folder_id            = var.folder_id
  billing_account      = var.billing_account
  shared_vpc           = var.shared_vpc
  skip_gcloud_download = true

  shared_vpc_subnets = var.shared_vpc_subnets

  activate_apis = toset(concat(local.default_apis, var.apis))
}
