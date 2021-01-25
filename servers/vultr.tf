terraform {
  required_providers {
    vultr = {
      source = "vultr/vultr"
      version = "2.1.2"
    }
  }
}

variable "vultr_api_key" {}

provider "vultr" {
  api_key = var.vultr_api_key
  rate_limit = 700
  retry_limit = 3
}

resource "vultr_ssh_key" "main" {
  name = "Personal"
  ssh_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRNU1yPnVxZtK/qrOkAnp5J+EqXJ6wTeXOScw2lhqWg (none)"
}

resource "vultr_startup_script" "install_nixos" {
  name = "nixos-infect"
  script = base64encode(
    <<EOF
    curl https://raw.githubusercontent.com/elitak/nixos-infect/master/nixos-infect | PROVIDER=vultr bash
    EOF
  )
}

resource "vultr_instance" "main" {
  plan = "vc2-1c-1gb" # $5 server plan
  region = "ams"

  label = "krake.one"
  hostname = "krake.one"

  os_id = 413 # Ubuntu 20.10 x64
  script_id = vultr_startup_script.install_nixos.id

  ssh_key_ids = [ vultr_ssh_key.main.id ]
}

resource "time_sleep" "wait_for_nixos_infect" {
  depends_on = [ vultr_instance.main ]

  create_duration = "5m"
}

output "server-ip" {
  value = vultr_instance.main.main_ip
}
