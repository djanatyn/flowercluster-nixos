// ∀ domain ∃ angel

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
