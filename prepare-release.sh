#!/bin/bash
# This script prepares the release branch for a new version
#
# Usage: ./prepare-release.sh <new-version>
#
# You'll need to edit the CHANGELOG and git push after running this.

set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <new-version>"
    echo "Example: $0 1.2.3"
    exit 1
fi

NEW_VERSION=$1
ROOT_DIR=$(pwd)

# update version in package.json
update_package_json() {
    local file=$1
    local version=$2
    local dir=$(dirname "$file")

    # Update package version
    sed -i'' -e "s/\"version\": \".*\"/\"version\": \"$version\"/" "$file"

    # If this is a SDK package, also update the wasm dependency
    if [[ $file == *"clarinet-sdk"* ]]; then
        sed -i'' -e "s/\"@clarinet\/sdk-wasm\": \".*\"/\"@clarinet\/sdk-wasm\": \"$version\"/" "$file"
        # Update @stacks/clarinet-sdk-wasm dependency for node package
        sed -i'' -e "s/\"@stacks\/clarinet-sdk-wasm\": \".*\"/\"@stacks\/clarinet-sdk-wasm\": \"$version\"/" "$file"
        # Update @stacks/clarinet-sdk-wasm-browser dependency for browser package
        sed -i'' -e "s/\"@stacks\/clarinet-sdk-wasm-browser\": \".*\"/\"@stacks\/clarinet-sdk-wasm-browser\": \"$version\"/" "$file"
    fi

    echo "Updated version in $file to $version"
}

# update version in Cargo.toml
update_cargo_toml() {
    local file=$1
    local version=$2

    sed -i'' -e "s/^version = \".*\"/version = \"$version\"/" "$file"
    echo "Updated version in $file to $version"
}

echo "Checking out release branch..."
git checkout -b release/next

echo "Generating changelog..."
npx generate-changelog v$NEW_VERSION...HEAD

echo "Starting version updates to $NEW_VERSION..."

# Update root Cargo.toml and package.json
echo "Updating root files..."
update_cargo_toml "./Cargo.toml" "$NEW_VERSION"

# Build SDK WASM
echo "Building SDK WASM..."
npm run build:sdk-wasm

# Update Clarinet SDK packages
echo "Updating Clarinet SDK packages..."

# Update WASM package versions first
echo "Updating SDK wasm package versions..."
update_package_json "components/clarinet-sdk-wasm/pkg-node/package.json" "$NEW_VERSION"
update_package_json "components/clarinet-sdk-wasm/pkg-browser/package.json" "$NEW_VERSION"

# Update SDK node package
echo "Updating SDK node package..."
update_package_json "components/clarinet-sdk/node/package.json" "$NEW_VERSION"

# Update SDK browser package
echo "Updating SDK browser package..."
update_package_json "components/clarinet-sdk/browser/package.json" "$NEW_VERSION"

# Update clarity-vscode
echo "Updating clarity-vscode..."
update_package_json "components/clarity-vscode/package.json" "$NEW_VERSION"
cd components/clarity-vscode
npm i
cd "$ROOT_DIR"

# Install all deps from root to properly handle workspaces
echo "Installing deps from root..."
npm i

echo "All updates completed successfully!"

echo "Next steps:"
echo "1. Manually edit CHANGELOG."
echo "2. Push"
echo "  - git commit -am 'chore: release $NEW_VERSION'"
echo "  - git push origin release/next"
echo "3. Open PR: https://github.com/stx-labs/clarinet/pulls"

# Clean up any backup files created by sed
find . -name "*-e" -delete
find . -name "*.bak" -delete
