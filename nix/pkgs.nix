# our packages overlay
pkgs: _:
with pkgs; {
  ouroborosNetworkHaskellPackages = import ./ouroboros-network.nix {
    inherit config pkgs lib stdenv haskell-nix buildPackages;
  };

  network-docs = callPackage ./network-docs.nix { };
  consensus-docs = callPackage ./consensus-docs.nix { };

  cabal = haskell-nix.tool localConfig.ghcVersion "cabal" {
    version = "latest";
    inherit (ouroborosNetworkHaskellPackages) index-state;
  };

  stylish-haskell = haskell-nix.tool localConfig.ghcVersion "stylish-haskell" {
    version = "0.13.0.0";
    inherit (ouroborosNetworkHaskellPackages) index-state;
  };

  trace = builtins.trace;
}
