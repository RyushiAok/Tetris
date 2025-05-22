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
          devShells.default = pkgs.buildFHSEnv {
            name = "tetris-shell";
            targetPkgs =
              pkgs:
              with pkgs;
              [
                dotnet-sdk_9
                udev
                alsa-lib
                fontconfig
                glew
              ]
              ++ (with pkgs.xorg; [
                libX11
                libICE
                libSM
                libXi
                libXcursor
                libXext
                libXrandr
              ]);
            runScript = "zsh";
          };
        };
    };
}
