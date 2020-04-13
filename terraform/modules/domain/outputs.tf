output "projects" {
  value = module.terraform-project
}

output "folders" {
  value = {
    domain      = module.domain-folder
    environment = module.environment-folders
  }
}
