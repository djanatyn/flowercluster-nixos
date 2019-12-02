output "vpc_id" {
  value = aws_vpc.main.id
}

output "subnets" {
  value = {
    dmz      = aws_subnet.dmz.id
    internal = aws_subnet.internal.id
  }
}

output "security_groups" {
  value = {
    dmz_ssh = aws_security_group.dmz_ssh.id
  }
}

output "keypair" {
  value = aws_key_pair.automate.id
}
