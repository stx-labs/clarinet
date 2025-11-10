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

# Function to update version in package.json
update_package_json() {
    local file=$1
    local version=$2
    local dir=$(dirname "$file")

    # Update package version
    sed -i'' -e "s/\"version\": \".*\"/\"version\": \"$version\"/" "$file"

    # If this is a SDK package, also update the wasm dependency
    if [[ $file == *"clarinet-sdk"* ]]; then
        sed -i'' -e "s/\"@clarinet\/sdk-wasm\": \".*\"/\"@clarinet\/sdk-wasm\": \"$version\"/" "$file"
    fi

    echo "Updated version in $file to $version"
}

# Function to update version in Cargo.toml
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

# # Build SDK WASM
echo "Building SDK WASM..."
npm run build:sdk-wasm

# # Update Clarinet SDK packages
echo "Updating Clarinet SDK packages..."

# # SDK wasm
echo "Updating SDK wasm"
cd components/clarinet-sdk/node
update_package_json "package.json" "$NEW_VERSION"
npm install "@stacks/clarinet-sdk-wasm@$NEW_VERSION" --save
npm i
cd "$ROOT_DIR"

# echo "Updating SDK wasm browser"
cd components/clarinet-sdk/browser
update_package_json "package.json" "$NEW_VERSION"
npm install "@stacks/clarinet-sdk-wasm-browser@$NEW_VERSION" --save
npm i
cd "$ROOT_DIR"

echo "Updating clarity-vscode..."
cd components/clarity-vscode
update_package_json "package.json" "$NEW_VERSION"
npm i
cd "$ROOT_DIR"

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
