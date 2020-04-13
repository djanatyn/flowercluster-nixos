provider "google" {
  project = "flowercluster-org-terraform"
  region  = "us-east1"
}

terraform {
  backend "gcs" {
    bucket = "flowercluster-org-tf-state"
    prefix = "sahaquiel/state"
  }
}

data "terraform_remote_state" "org" {
  backend = "gcs"
  config = {
    bucket = "flowercluster-org-tf-state"
    prefix = "org/state"
  }
}
