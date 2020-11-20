{
  extras = hackage:
    {
      packages = {
        "sandi" = (((hackage.sandi)."0.5").revisions).default;
        compiler = ./compiler.nix;
        };
      };
  resolver = "lts-16.22";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }