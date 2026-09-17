#!/usr/bin/env bash
#
# Resolve the target diff for a review or simplify pass, work out which of the
# repo's three surfaces it reaches, and print the CI gates that actually apply.
#
# Shared by the clarinet-review and clarinet-simplify skills so target
# resolution and the gate list live in exactly one place. Every gate printed
# here is a command CI already runs (.github/workflows/ci*.yaml) — this script
# only decides which subset is relevant to the diff.
#
# Usage: .agents/scripts/clarinet-gates.sh [TARGET]
#
#   (no arg)       uncommitted work if the tree is dirty; else this branch vs
#                  the default branch; else the last commit
#   --unstaged     unstaged changes only
#   --staged       staged changes only
#   --worktree     staged + unstaged
#   <a>..<b>       that range
#   <branch>       merge-base(default, branch)..branch
#   <commit>       that commit alone
#   123 | #123     that pull request, via `gh pr diff` (no local checkout needed)
#
# Output is four labelled sections: TARGET, FILES, SURFACES, GATES.

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

# The default branch, for branch-vs-base resolution.
default_branch=$(git symbolic-ref --quiet --short refs/remotes/origin/HEAD 2>/dev/null || true)
default_branch=${default_branch#origin/}
[[ -n $default_branch ]] || default_branch=main

# Resolve the default branch to a ref that actually exists: prefer the local
# branch, fall back to the remote-tracking one. A worktree cut from a remote
# branch frequently has origin/main and no local main, and silently reviewing
# only the last commit is worse than reviewing nothing.
base_ref() {
  if git rev-parse --quiet --verify "$default_branch" >/dev/null; then
    printf '%s' "$default_branch"
  elif git rev-parse --quiet --verify "origin/$default_branch" >/dev/null; then
    printf '%s' "origin/$default_branch"
  else
    return 1
  fi
}

target=${1:-}
diff_cmd=(git diff)
description=
# Set for a pull-request target: the gates below need the branch checked out.
# Matched by regex rather than a case glob, so a hex SHA like 1f102919 is not
# mistaken for PR 1.
pr=
if [[ $target =~ ^#?[0-9]+$ ]]; then
  pr=${target#\#}
  if ! command -v gh >/dev/null; then
    echo "clarinet-gates: reviewing PR $pr needs the gh CLI" >&2
    exit 1
  fi
  target=--pr
fi

case $target in
  --pr)
    diff_cmd=(gh pr diff "$pr")
    description="pull request #$pr"
    ;;
  "")
    if [[ -n $(git status --porcelain) ]]; then
      diff_cmd=(git diff HEAD)
      description="uncommitted work (staged + unstaged, vs HEAD)"
    elif [[ $(git rev-parse --abbrev-ref HEAD) != "$default_branch" ]] && ref=$(base_ref); then
      base=$(git merge-base "$ref" HEAD)
      diff_cmd=(git diff "$base" HEAD)
      description="branch $(git rev-parse --abbrev-ref HEAD) vs $ref (base ${base:0:8})"
    else
      diff_cmd=(git diff HEAD~1 HEAD)
      description="last commit $(git rev-parse --short HEAD)"
    fi
    ;;
  --unstaged)
    diff_cmd=(git diff)
    description="unstaged changes"
    ;;
  --staged)
    diff_cmd=(git diff --cached)
    description="staged changes"
    ;;
  --worktree)
    diff_cmd=(git diff HEAD)
    description="uncommitted work (staged + unstaged, vs HEAD)"
    ;;
  *..*)
    a=${target%%..*}
    b=${target##*..}
    b=${b#.}
    diff_cmd=(git diff "$a" "${b:-HEAD}")
    description="range $target"
    ;;
  *)
    if git rev-parse --quiet --verify "refs/heads/$target" >/dev/null ||
      git rev-parse --quiet --verify "refs/remotes/$target" >/dev/null; then
      ref=$(base_ref) || skip_ref=1
      if [[ -n ${skip_ref:-} ]]; then
        echo "clarinet-gates: no local or remote ref for default branch '$default_branch'" >&2
        exit 1
      fi
      base=$(git merge-base "$ref" "$target")
      diff_cmd=(git diff "$base" "$target")
      description="branch $target vs $ref (base ${base:0:8})"
    elif git rev-parse --quiet --verify "$target^{commit}" >/dev/null; then
      diff_cmd=(git diff "$target~1" "$target")
      description="commit $(git rev-parse --short "$target")"
    else
      echo "clarinet-gates: '$target' is not a branch, commit or range" >&2
      exit 1
    fi
    ;;
esac

# Newline-delimited rather than an array: macOS ships bash 3.2, which has no
# `mapfile` and errors on an empty array under `set -u`.
files=$("${diff_cmd[@]}" --name-only)

# `git diff` cannot see files git has never been told about, so a brand-new
# source file would be reviewed by nobody and gated by nothing. Fold untracked
# files into the list for working-tree targets only — a commit or a range has
# no untracked component.
untracked=
if [[ ${diff_cmd[*]} == "git diff HEAD" || ${diff_cmd[*]} == "git diff" ]]; then
  untracked=$(git ls-files --others --exclude-standard)
  [[ -z $untracked ]] || files=$(printf '%s\n%s' "$files" "$untracked" | sed '/^$/d' | sort -u)
fi

echo "TARGET: $description"
echo "DIFF: ${diff_cmd[*]}"
echo
echo "FILES:"
if [[ -z $files ]]; then
  echo "  (none — nothing to review)"
  echo
  echo "SURFACES:"
  echo "GATES:"
  exit 0
fi
echo "$files" | sed 's/^/  /'
if [[ -n $untracked ]]; then
  echo
  echo "NOTE: these files are untracked, so \`${diff_cmd[*]}\` will not show them."
  echo "      Use \`git add -N <path>\` first, or diff them with \`git diff --no-index /dev/null <path>\`:"
  echo "$untracked" | sed 's/^/  /'
fi

# Crates in each wasm32 build graph. The two lists differ: the LSP pulls in
# clarinet-format and clarity-static-cost, the SDK does not. Re-derive with:
#   cargo tree --target wasm32-unknown-unknown -p clarinet-sdk-wasm -e no-dev
#   cargo tree --target wasm32-unknown-unknown -p clarity-lsp --no-default-features -e no-dev
sdk_wasm_crates="clarinet-defaults|clarinet-deployments|clarinet-files|clarinet-sdk-wasm|clarinet-utils|clarity-repl|hiro-system-kit"
lsp_crates="clarinet-defaults|clarinet-deployments|clarinet-files|clarinet-format|clarinet-utils|clarity-lsp|clarity-repl|clarity-static-cost|hiro-system-kit"

rust=false wasm=false lsp=false sdk_ts=false vscode=false manifest=false
rust_deps=false js_deps=false

while IFS= read -r f; do
  [[ -n $f ]] || continue
  case $f in
    # .clar boot sources are include_str!'d into clarity-repl, and a manifest
    # or lockfile change can break the build on its own. Both need the compile
    # and test gates, not just the audit.
    *.rs | *.clar | Cargo.toml | Cargo.lock | components/*/Cargo.toml) rust=true ;;
  esac
  # Only a Rust or manifest change to a crate can break that crate's build, so
  # a TypeScript-only edit under components/ must not pull in a cargo gate.
  case $f in
    components/*/*.rs | components/*/*.clar | components/*/Cargo.toml)
      crate=${f#components/}
      crate=${crate%%/*}
      if [[ $crate =~ ^($sdk_wasm_crates)$ ]]; then
        wasm=true
      fi
      if [[ $crate =~ ^($lsp_crates)$ ]]; then
        lsp=true
      fi
      ;;
  esac
  case $f in
    components/clarinet-sdk/* | components/clarinet-sdk-wasm/*) sdk_ts=true ;;
  esac
  case $f in
    components/clarity-vscode/*) vscode=true ;;
  esac
  case $f in
    Cargo.toml | Cargo.lock | components/*/Cargo.toml) rust_deps=true ;;
  esac
  case $f in
    pnpm-lock.yaml | pnpm-workspace.yaml | package.json | */pnpm-lock.yaml | */package.json)
      js_deps=true
      ;;
  esac
  case $f in
    components/clarinet-files/src/*) manifest=true ;;
  esac
  # A here-string rather than a pipe, so the flags above survive the loop.
done <<<"$files"

echo
echo "SURFACES:"
if $rust; then echo "  native      — clarinet-cli and the native crates"; fi
if $wasm; then echo "  wasm-sdk    — crate is in the clarinet-sdk-wasm graph"; fi
if $lsp; then echo "  wasm-lsp    — crate is in the clarity-lsp graph"; fi
if $sdk_ts; then echo "  sdk-ts      — TypeScript SDK surface"; fi
if $vscode; then echo "  vscode      — VSCode extension surface"; fi
if $rust_deps; then echo "  cargo-deps  — Cargo manifest or lockfile change"; fi
if $js_deps; then echo "  npm-deps    — package.json or pnpm lockfile change"; fi
if $manifest; then echo "  manifest    — clarinet-files types (manifest schema)"; fi

echo
echo "GATES:"
echo "# Run in order, cheapest first. Stop at the first failure."
if [[ -n $pr ]]; then
  echo "# PR #$pr is not checked out here — these gates test the working tree, not the PR."
  echo "#   gh pr checkout $pr   (in a worktree) to make them meaningful."
fi
if $rust; then
  echo "cargo fmt-stacks --check"
  echo "cargo clippy --workspace --tests --exclude clarinet-sdk-wasm"
fi
if $wasm; then
  echo "cargo clippy --package clarinet-sdk-wasm --target wasm32-unknown-unknown"
  echo "# stdout macros are silently discarded on wasm32 — see lints/wasm/clippy.toml"
  echo "CLIPPY_CONF_DIR=\$PWD/lints/wasm cargo clippy --no-default-features --lib --target wasm32-unknown-unknown -p clarinet-sdk-wasm -- -A clippy::all -D clippy::disallowed_macros"
fi
if $lsp; then
  echo "cargo clippy --target=wasm32-unknown-unknown --no-default-features --package=clarity-lsp"
fi
if $rust; then
  echo "cargo tst"
fi
if $sdk_ts; then
  echo "# slow (wasm-pack build); only needed if the change reaches the SDK"
  echo "pnpm run build:sdk-wasm && pnpm --filter ./components/clarinet-sdk/node run test"
fi
if $vscode; then
  echo "# components/clarity-vscode is its own pnpm workspace"
  echo "pnpm --dir components/clarity-vscode run lint"
fi
if $rust_deps; then
  echo "cargo audit"
fi
if $js_deps; then
  echo "# pnpm-workspace.yaml sets minimumReleaseAge: any new npm dep must be >= 5 days old"
fi
if $manifest; then
  echo "# regenerate the IDE manifest schema if ProjectManifestFile changed"
  echo "cargo gen-schema"
fi
