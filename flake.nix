{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        cargoToml = builtins.fromTOML (builtins.readFile ./Cargo.toml);
        pname = "clarinet";
        inherit (cargoToml.workspace.package) version;

        clarinet = pkgs.rustPlatform.buildRustPackage {
          inherit pname version;
          src = self;
          cargoLock = {
            lockFile = ./Cargo.lock;
            outputHashes = {
              "clarity-0.0.1" = "sha256-zVwVvLyh/hObWWKN6bIe4J5mf+6N8SyWZozzpKStcME=";
            };
          };

          preBuild =
            let
              versionsToml = pkgs.writeText "versions.toml" ''
                stacks_node_version = "0.0.0.0.1"
                stacks_signer_version = "0.0.0.0.1.0"
              '';
            in
            # Minimal versions.toml for stacks-common build script
            # The build.rs in stacks-common expects it at ../versions.toml
            # relative to the stacks-common crate in the cargo vendor directory
            ''
              cp ${versionsToml} "$NIX_BUILD_TOP/cargo-vendor-dir/versions.toml"
            '';

          nativeBuildInputs = [
            pkgs.pkg-config
          ];

          buildInputs = [
            pkgs.openssl
          ]
          ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
            pkgs.apple-sdk_15
          ];

          # Skip tests that require network access or additional setup
          doCheck = false;
        };
      in
      {
        packages.default = clarinet;
        packages.clarinet = clarinet;
      }
    );
}
