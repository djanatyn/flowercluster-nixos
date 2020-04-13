module "org-iam" {
  source        = "terraform-google-modules/iam/google//modules/organizations_iam"
  organizations = ["947273970991"]
  mode          = "additive"

  bindings = {
    "roles/owner" = [
      "serviceAccount:${module.org-service-account.email}"
    ]

    "roles/billing.admin" = [
      "serviceAccount:${module.org-service-account.email}"
    ]

    "roles/resourcemanager.projectCreator" = [
      "serviceAccount:${module.org-service-account.email}"
    ]

    "roles/resourcemanager.organizationAdmin" = [
      "serviceAccount:${module.org-service-account.email}"
    ]

    "roles/storage.admin" = [
      "serviceAccount:${module.org-service-account.email}"
    ]

    "roles/compute.xpnAdmin" = [
      "serviceAccount:${module.org-service-account.email}"
    ]
  }
}
