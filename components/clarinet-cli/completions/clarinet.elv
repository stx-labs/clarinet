
use builtin;
use str;

set edit:completion:arg-completer[clarinet] = {|@words|
    fn spaces {|n|
        builtin:repeat $n ' ' | str:join ''
    }
    fn cand {|text desc|
        edit:complex-candidate $text &display=$text' '(spaces (- 14 (wcswidth $text)))$desc
    }
    var command = 'clarinet'
    for word $words[1..-1] {
        if (str:has-prefix $word '-') {
            break
        }
        set command = $command';'$word
    }
    var completions = [
        &'clarinet'= {
            cand --help 'Print help information'
            cand --version 'Print version information'
            cand new 'Create and scaffold a new project'
            cand contract 'Subcommands for working with contracts'
            cand console 'Load contracts in a REPL for an interactive session'
            cand test 'Execute test suite'
            cand check 'Check syntax of your contracts'
            cand deployments 'Deploy contracts on chain'
            cand run 'Execute Clarinet extension'
            cand integrate 'Start devnet environment for integration testing'
            cand lsp 'Start an LSP server (for integration with editors)'
            cand completions 'Generate shell completions scripts'
        }
        &'clarinet;new'= {
            cand --help 'Print help information'
            cand --version 'Print version information'
            cand --disable-telemetry 'Do not provide developer usage telemetry for this project'
        }
        &'clarinet;contract'= {
            cand --help 'Print help information'
            cand --version 'Print version information'
            cand new 'Generate files and settings for a new contract'
            cand requirement 'Add third-party requirements to this project'
            cand fork 'Replicate a third-party contract into this project'
        }
        &'clarinet;contract;new'= {
            cand --manifest-path 'Path to Clarinet.toml'
            cand --help 'Print help information'
            cand --version 'Print version information'
        }
        &'clarinet;contract;requirement'= {
            cand --manifest-path 'Path to Clarinet.toml'
            cand --help 'Print help information'
            cand --version 'Print version information'
        }
        &'clarinet;contract;fork'= {
            cand --manifest-path 'Path to Clarinet.toml'
            cand --help 'Print help information'
            cand --version 'Print version information'
        }
        &'clarinet;console'= {
            cand --manifest-path 'Path to Clarinet.toml'
            cand --help 'Print help information'
            cand --version 'Print version information'
        }
        &'clarinet;test'= {
            cand --manifest-path 'Path to Clarinet.toml'
            cand --help 'Print help information'
            cand --version 'Print version information'
            cand --coverage 'Generate coverage file (coverage.lcov)'
            cand --costs 'Generate costs report'
            cand --watch 'Relaunch tests upon updates to contracts'
        }
        &'clarinet;check'= {
            cand --manifest-path 'Path to Clarinet.toml'
            cand --help 'Print help information'
            cand --version 'Print version information'
        }
        &'clarinet;deployments'= {
            cand --manifest-path 'Path to Clarinet.toml'
            cand --help 'Print help information'
            cand --version 'Print version information'
            cand --devnet 'Deploy contracts on devnet, using settings/Devnet.toml'
            cand --testnet 'Deploy contracts on testnet, using settings/Testnet.toml'
            cand --mainnet 'Deploy contracts on mainnet, using settings/Mainnet.toml'
        }
        &'clarinet;run'= {
            cand --manifest-path 'Path to Clarinet.toml'
            cand --help 'Print help information'
            cand --version 'Print version information'
            cand --allow-wallets 'Allow access to wallets'
            cand --allow-write 'Allow write access to disk'
            cand --allow-read 'Allow read access to disk'
        }
        &'clarinet;integrate'= {
            cand --manifest-path 'Path to Clarinet.toml'
            cand --help 'Print help information'
            cand --version 'Print version information'
            cand --no-dashboard 'Display streams of logs instead of terminal UI dashboard'
        }
        &'clarinet;lsp'= {
            cand --help 'Print help information'
            cand --version 'Print version information'
        }
        &'clarinet;completions'= {
            cand --help 'Print help information'
            cand --version 'Print version information'
        }
    ]
    $completions[$command]
}
