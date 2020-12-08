{}:

let
  iele-assemble = import ./iele-assemble {};

  default =
    {
      inherit (iele-assemble) iele-assemble;
    };

in default
