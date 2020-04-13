module "sahaquiel-nonprod" {
  source = "../../modules/project"

  project_name    = "Sahaquiel Non-Prod"
  project_id      = "sahaquiel-nonprod"
  org_id          = data.terraform_remote_state.org.outputs.org_id
  billing_account = data.terraform_remote_state.org.outputs.billing_account

  shared_vpc = data.terraform_remote_state.org.outputs.org-terraform.project.project_id

  shared_vpc_subnets = [
    data.terraform_remote_state.org.outputs.network.vpc.subnets["us-east1/internal-01"].self_link
  ]

  folder_id = data.terraform_remote_state.org.outputs.domains.sahaquiel.folders.environment.ids.nonprod
}
