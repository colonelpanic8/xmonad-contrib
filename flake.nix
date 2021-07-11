{
  inputs = {
    gitIgnoreNix = {
      url = github:IvanMalison/gitignore.nix;
    };
  };
  outputs = { self, nixpkgs, gitIgnoreNix }: {
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {}))
        (self: super: rec {
          xmonad-contrib =
            self.callCabal2nix "xmonad-contrib" (gitIgnoreNix.gitIgnoreSource ./.) { };
        });
      });
    };
  };
}
