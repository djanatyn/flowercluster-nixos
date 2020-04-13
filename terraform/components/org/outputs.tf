output "org_id" {
  value = "947273970991"
}

output "org-terraform" {
  value = module.org-terraform
}

output "org-service-account" {
  value = module.org-service-account
}

output "domains" {
  value = {
    sahaquiel = module.sahaquiel
  }
}

output "network" {
  value = module.org-network
}
