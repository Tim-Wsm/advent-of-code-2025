{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    # use my custom nixvim config to manage the neovim install and configuration
    nixvim-flake = {
      url = "github:Tim-Wsm/nixvim-conf";
      # require nixvim to use the same nixpkgs channel as the system
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # make opam packages updatable via the official github repo
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
  };
  outputs = {
    flake-utils,
    opam-nix,
    nixvim-flake,
    nixpkgs,
    ...
  } @ inputs: let
    package = "AdventOfCode2025";
  in
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};

        # make opam packages updatable via the official github repo
        opamParams = {
          # repositories to source the dependencies
          repos = [
            inputs.opam-repository
          ];
        };

        devPackagesQuery = {
          # You can add "development" packages here. They will get added to the devShell automatically.
          ocaml-lsp-server = "*";
          ocamlformat = "*";
        };
        query =
          devPackagesQuery
          // {
            ## You can force versions of certain packages here, e.g:
            ## - force the ocaml compiler to be taken from opam-repository:
            ocaml-base-compiler = "5.3.0";
          };
        scope = on.buildOpamProject' opamParams ./. query;
        overlay = final: prev: {
          # You can add overrides here
          ${package} = prev.${package}.overrideAttrs (_: {
            # Prevent the ocaml dependencies from leaking into dependent environments
            doNixSupport = false;
          });
          # Add my own neovim config here, to ensure that the tooling matches
          neovim = nixvim-flake.packages.${system}.default;
        };
        scope' = scope.overrideScope overlay;
        # The main package containing the executable
        main = scope'.${package};
        # Packages from devPackagesQuery
        devPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
      in {
        legacyPackages = scope';

        packages.default = main;

        devShells.default = pkgs.mkShell {
          inputsFrom = [main];
          buildInputs =
            devPackages
            ++ [
              # You can add packages from nixpkgs here
              pkgs.fish
            ];
          shellHook = ''
            exec ${pkgs.fish}/bin/fish
          '';
        };
      }
    );
}
