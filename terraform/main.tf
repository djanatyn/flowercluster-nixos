module "network" {
  source = "./modules/network"

  vpc_name   = var.garden_name
  public_key = var.garden_key
}
