{ compiler ? "ghc924" }:
let
  pkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/854fdc68881791812eddd33b2fed94b954979a8e.tar.gz";
      sha256 = "1szch6bhnvfkd3pzck9fvl1lq2j47vrm8d2cq35qq9pi96iwhf9l";
    })
    {
      config.allowBroken = false;
      config.allowUnfree = true;
    };
  gitignoreSrc = import
    (builtins.fetchGit {
      url = "https://github.com/hercules-ci/gitignore";
      rev = "a20de23b925fd8264fd7fad6454652e142fd7f73";
    })
    { };
  ourGHC = pkgs.haskell.packages.${compiler};
in
ourGHC.developPackage {
  name = builtins.baseNameOf ./.;
  root = gitignoreSrc.gitignoreSource ./.;
  overrides = final: prev: { };
  source-overrides = {
    th-compat = builtins.fetchGit {
      url = "https://github.com/haskell-compat/th-compat";
      rev = "0c480e0606fb1fe7bd02942149f598f71734e25d";
    };
  };
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or [ ]) ++ [
      ourGHC.cabal-install
      ourGHC.cabal2nix
      ourGHC.haskell-language-server
    ];
  });
}
