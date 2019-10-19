provider "aws" {
  region = "us-east-1"
}

terraform {
  backend "s3" {
    bucket = "flowercluster-terraform-state"
    key    = "flowercluster.tfstate"
    region = "us-east-1"
  }
}
