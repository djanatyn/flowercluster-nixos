module "network" {
  source = "./modules/network"

  vpc_name = var.garden_name
}
