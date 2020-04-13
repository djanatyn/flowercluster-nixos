variable "apis" {
  type    = list(string)
  default = []
}

variable "project_id" {
  type    = string
  default = null
}

variable "project_name" {
  type = string
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
