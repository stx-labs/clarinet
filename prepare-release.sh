#!/bin/bash
# This script prepares the release branch for a new version
#
# Usage: ./prepare-release.sh <major|minor|patch>
#
# You'll need to edit the CHANGELOG and git push after running this.

set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <major|minor|patch>"
    echo "Example: $0 minor"
    exit 1
fi

BUMP_TYPE=$1

if [[ "$BUMP_TYPE" != "major" && "$BUMP_TYPE" != "minor" && "$BUMP_TYPE" != "patch" ]]; then
    echo "Invalid bump type: $BUMP_TYPE"
    echo "Usage: $0 <major|minor|patch>"
    exit 1
fi

CURRENT_VERSION=$(sed -n 's/^version = "\(.*\)"/\1/p' ./Cargo.toml | head -n 1)

if [[ ! "$CURRENT_VERSION" =~ ^([0-9]+)\.([0-9]+)\.([0-9]+)$ ]]; then
    echo "Could not parse current version from Cargo.toml: $CURRENT_VERSION"
    exit 1
fi

MAJOR=${BASH_REMATCH[1]}
MINOR=${BASH_REMATCH[2]}
PATCH=${BASH_REMATCH[3]}

case "$BUMP_TYPE" in
    major)
        MAJOR=$((MAJOR + 1))
        MINOR=0
        PATCH=0
        ;;
    minor)
        MINOR=$((MINOR + 1))
        PATCH=0
        ;;
    patch)
        PATCH=$((PATCH + 1))
        ;;
esac

NEW_VERSION="$MAJOR.$MINOR.$PATCH"

echo "Current version: $CURRENT_VERSION"
echo "Bump type: $BUMP_TYPE"
echo "New version: $NEW_VERSION"

# update version in package.json
update_package_json() {
    local file=$1
    local version=$2

    # Update package version
    sed -i'' -e "s/\"version\": \".*\"/\"version\": \"$version\"/" "$file"

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
pnpm dlx generate-changelog "v$NEW_VERSION...HEAD"

echo "Starting version updates to $NEW_VERSION..."

# Update root Cargo.toml and package.json
echo "Updating root files..."
update_cargo_toml "./Cargo.toml" "$NEW_VERSION"

# Build SDK WASM
echo "Building SDK WASM..."
pnpm run build:sdk-wasm

# Update clarity-vscode
echo "Updating clarity-vscode..."
update_package_json "components/clarity-vscode/package.json" "$NEW_VERSION"
(cd components/clarity-vscode && pnpm install)

# Update @stacks/clarinet-sdk and @stacks/clarinet-sdk-browser
echo "Updating clarinet-sdk..."
update_package_json "components/clarinet-sdk/node/package.json" "$NEW_VERSION"
update_package_json "components/clarinet-sdk/browser/package.json" "$NEW_VERSION"

# Install all deps from root to properly handle workspaces
echo "Installing deps from root..."
pnpm install

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
