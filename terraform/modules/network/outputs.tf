output "vpc_id" {
  value = aws_vpc.main.id
}

output "subnets" {
  value = {
    dmz      = aws_subnet.dmz.id
    internal = aws_subnet.internal.id
  }
}
