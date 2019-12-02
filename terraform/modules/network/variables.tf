variable "vpc_name" {
  type        = "string"
  description = "The name of the VPC to create."
}

variable "public_key" {
  type        = "string"
  description = "Public key for keypair to create."
}
