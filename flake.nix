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

        # Hash for clarity git dependency - update this when Cargo.lock changes
        # Run `nix run .#check-git-dependencies-hash` to verify or get the new hash
        clarityHash = "sha256-IaKdIY4NzlwvCYaVjHsSCRXUPn5EIg7BuecGKjmpx9s=";

        clarinet = pkgs.rustPlatform.buildRustPackage {
          inherit pname version;
          src = self;
          cargoLock = {
            lockFile = ./Cargo.lock;
            outputHashes = {
              "clarity-0.0.1" = clarityHash;
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
        # Script to check git dependency hashes match between Cargo.lock and flake.nix
        check-git-dependencies-hash = pkgs.writeShellApplication {
          name = "check-git-dependencies-hash";
          runtimeInputs = [
            pkgs.gnugrep
            pkgs.gnused
            pkgs.nix-prefetch-git
            pkgs.jq
          ];
          text =
            let
              gitRepo = "https://github.com/stacks-network/stacks-core.git";
            in
            ''
              if [ ! -f "Cargo.lock" ]; then
                echo "Error: Cargo.lock not found in current directory" >&2
                exit 1
              fi

              current_hash="${clarityHash}"
              echo "Current hash in flake.nix: $current_hash"

              source_line=$(grep -A2 'name = "clarity"' Cargo.lock | grep 'source = "git+${gitRepo}' || true)

              # Extract the commit hash (after the #)
              commit=$(echo "$source_line" | sed -n 's/.*#\([a-f0-9]*\)".*/\1/p')

              if [ -z "$commit" ]; then
                echo "Error: Could not extract clarity commit from Cargo.lock" >&2
                exit 1
              fi

              echo "Fetching Nix hash for clarity commit: $commit"

              # Get the hash
              sha256=$(nix-prefetch-git --url ${gitRepo} --rev "$commit" --quiet | jq -r '.sha256')
              expected_hash=$(nix hash convert --hash-algo sha256 --to sri "$sha256")
              echo "Expected hash from Cargo.lock: $expected_hash"

              if [ "$expected_hash" != "$current_hash" ]; then
                echo ""
                echo "Error: clarity hash mismatch!"
                echo "Please update outputHashes in flake.nix to:"
                echo "  \"clarity-0.0.1\" = \"$expected_hash\";"
                exit 1
              fi

              echo ""
              echo "Clarity hash is up to date!"
            '';
        };
      in
      {
        packages.default = clarinet;
        packages.clarinet = clarinet;
        apps.check-git-dependencies-hash = {
          type = "app";
          program = toString (pkgs.lib.getExe check-git-dependencies-hash);
        };
      }
    );
}
