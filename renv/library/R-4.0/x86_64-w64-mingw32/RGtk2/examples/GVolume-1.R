mount <- volume$getMount() ## mounted, so never NULL
mount_root <- mount$getRoot()
volume_activation_root <- volume$getActivationRoot() ## assume not NULL
