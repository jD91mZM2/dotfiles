module "deploy_nixos" {
  depends_on = [ time_sleep.wait_for_nixos_infect ]

  source = "github.com/tweag/terraform-nixos//deploy_nixos?ref=f0f623208944c80639ccbc9a56b45e72a6cfd26e"

  target_host = vultr_instance.main.main_ip
  nixos_config = "./main/default.nix"
  ssh_agent	= true
}
