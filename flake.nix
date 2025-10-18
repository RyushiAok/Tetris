{
  description = "fsharp flake";

  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];

      perSystem =
        { pkgs, ... }:
        {
          devShells = {
            default = pkgs.mkShell {
              name = "dev-shell";
              buildInputs = with pkgs; [
                dotnet-sdk_9
                fontconfig
                xorg.libICE
                xorg.libSM
                xorg.libX11
              ];
              shellHook = ''
                export LD_LIBRARY_PATH=${
                  pkgs.lib.makeLibraryPath [
                    pkgs.fontconfig
                    pkgs.xorg.libICE
                    pkgs.xorg.libSM
                    pkgs.xorg.libX11
                  ]
                }:$LD_LIBRARY_PATH
              '';
            };
          };
        };
    };
}
