variable "domain_name" {
  type        = string
  description = "name of domain"
}

variable "apis" {
  type    = list(string)
  default = []
}

variable "org_id" {
  type = string
}

variable "shared_vpc_subnets" {
  type    = list(string)
  default = []
}

variable "shared_vpc" {
  type    = string
  default = ""
}

variable "billing_account" {
  type = string
}
