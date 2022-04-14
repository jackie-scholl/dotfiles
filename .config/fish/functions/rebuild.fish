# Defined in /var/folders/dm/2kd_hgp51qx5v41_4ct5c8tw001d24/T//fish.pNd6bl/rebuild.fish @ line 2
function rebuild --description 'Rebuilds the current NixOS configuration.' 
    pushd /etc/nixos
    sudo sh -c "time nixos-rebuild switch --upgrade $argv" &&
    sudo sh -c "time nix-collect-garbage --delete-older-than 30d"
    popd
end
