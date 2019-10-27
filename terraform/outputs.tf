output "vpc_id" {
  value = module.network.vpc_id
}

output "subnets" {
  value = module.network.subnets
}

output "security_groups" {
  value = module.network.security_groups
}
