
using namespace System.Management.Automation
using namespace System.Management.Automation.Language

Register-ArgumentCompleter -Native -CommandName 'clarinet' -ScriptBlock {
    param($wordToComplete, $commandAst, $cursorPosition)

    $commandElements = $commandAst.CommandElements
    $command = @(
        'clarinet'
        for ($i = 1; $i -lt $commandElements.Count; $i++) {
            $element = $commandElements[$i]
            if ($element -isnot [StringConstantExpressionAst] -or
                $element.StringConstantType -ne [StringConstantType]::BareWord -or
                $element.Value.StartsWith('-') -or
                $element.Value -eq $wordToComplete) {
                break
        }
        $element.Value
    }) -join ';'

    $completions = @(switch ($command) {
        'clarinet' {
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            [CompletionResult]::new('new', 'new', [CompletionResultType]::ParameterValue, 'Create and scaffold a new project')
            [CompletionResult]::new('contract', 'contract', [CompletionResultType]::ParameterValue, 'Subcommands for working with contracts')
            [CompletionResult]::new('console', 'console', [CompletionResultType]::ParameterValue, 'Load contracts in a REPL for an interactive session')
            [CompletionResult]::new('test', 'test', [CompletionResultType]::ParameterValue, 'Execute test suite')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Check syntax of your contracts')
            [CompletionResult]::new('deployments', 'deployments', [CompletionResultType]::ParameterValue, 'Deploy contracts on chain')
            [CompletionResult]::new('run', 'run', [CompletionResultType]::ParameterValue, 'Execute Clarinet extension')
            [CompletionResult]::new('integrate', 'integrate', [CompletionResultType]::ParameterValue, 'Start devnet environment for integration testing')
            [CompletionResult]::new('lsp', 'lsp', [CompletionResultType]::ParameterValue, 'Start an LSP server (for integration with editors)')
            [CompletionResult]::new('completions', 'completions', [CompletionResultType]::ParameterValue, 'Generate shell completions scripts')
            break
        }
        'clarinet;new' {
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            [CompletionResult]::new('--disable-telemetry', 'disable-telemetry', [CompletionResultType]::ParameterName, 'Do not provide developer usage telemetry for this project')
            break
        }
        'clarinet;contract' {
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            [CompletionResult]::new('new', 'new', [CompletionResultType]::ParameterValue, 'Generate files and settings for a new contract')
            [CompletionResult]::new('requirement', 'requirement', [CompletionResultType]::ParameterValue, 'Add third-party requirements to this project')
            [CompletionResult]::new('fork', 'fork', [CompletionResultType]::ParameterValue, 'Replicate a third-party contract into this project')
            break
        }
        'clarinet;contract;new' {
            [CompletionResult]::new('--manifest-path', 'manifest-path', [CompletionResultType]::ParameterName, 'Path to Clarinet.toml')
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            break
        }
        'clarinet;contract;requirement' {
            [CompletionResult]::new('--manifest-path', 'manifest-path', [CompletionResultType]::ParameterName, 'Path to Clarinet.toml')
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            break
        }
        'clarinet;contract;fork' {
            [CompletionResult]::new('--manifest-path', 'manifest-path', [CompletionResultType]::ParameterName, 'Path to Clarinet.toml')
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            break
        }
        'clarinet;console' {
            [CompletionResult]::new('--manifest-path', 'manifest-path', [CompletionResultType]::ParameterName, 'Path to Clarinet.toml')
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            break
        }
        'clarinet;test' {
            [CompletionResult]::new('--manifest-path', 'manifest-path', [CompletionResultType]::ParameterName, 'Path to Clarinet.toml')
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            [CompletionResult]::new('--coverage', 'coverage', [CompletionResultType]::ParameterName, 'Generate coverage file (coverage.lcov)')
            [CompletionResult]::new('--costs', 'costs', [CompletionResultType]::ParameterName, 'Generate costs report')
            [CompletionResult]::new('--watch', 'watch', [CompletionResultType]::ParameterName, 'Relaunch tests upon updates to contracts')
            break
        }
        'clarinet;check' {
            [CompletionResult]::new('--manifest-path', 'manifest-path', [CompletionResultType]::ParameterName, 'Path to Clarinet.toml')
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            break
        }
        'clarinet;deployments' {
            [CompletionResult]::new('--manifest-path', 'manifest-path', [CompletionResultType]::ParameterName, 'Path to Clarinet.toml')
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            [CompletionResult]::new('--devnet', 'devnet', [CompletionResultType]::ParameterName, 'Deploy contracts on devnet, using settings/Devnet.toml')
            [CompletionResult]::new('--testnet', 'testnet', [CompletionResultType]::ParameterName, 'Deploy contracts on testnet, using settings/Testnet.toml')
            [CompletionResult]::new('--mainnet', 'mainnet', [CompletionResultType]::ParameterName, 'Deploy contracts on mainnet, using settings/Mainnet.toml')
            break
        }
        'clarinet;run' {
            [CompletionResult]::new('--manifest-path', 'manifest-path', [CompletionResultType]::ParameterName, 'Path to Clarinet.toml')
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            [CompletionResult]::new('--allow-wallets', 'allow-wallets', [CompletionResultType]::ParameterName, 'Allow access to wallets')
            [CompletionResult]::new('--allow-write', 'allow-write', [CompletionResultType]::ParameterName, 'Allow write access to disk')
            [CompletionResult]::new('--allow-read', 'allow-read', [CompletionResultType]::ParameterName, 'Allow read access to disk')
            break
        }
        'clarinet;integrate' {
            [CompletionResult]::new('--manifest-path', 'manifest-path', [CompletionResultType]::ParameterName, 'Path to Clarinet.toml')
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            [CompletionResult]::new('--no-dashboard', 'no-dashboard', [CompletionResultType]::ParameterName, 'Display streams of logs instead of terminal UI dashboard')
            break
        }
        'clarinet;lsp' {
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            break
        }
        'clarinet;completions' {
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Print help information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Print version information')
            break
        }
    })

    $completions.Where{ $_.CompletionText -like "$wordToComplete*" } |
        Sort-Object -Property ListItemText
}
