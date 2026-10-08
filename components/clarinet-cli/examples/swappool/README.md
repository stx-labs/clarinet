# Description

The purpose of this example is too make sure that clarinet properly handles the clarity versions of contracts dependencies.

This project only calls one external contract:
`SP3K8BC0PPEVCV7NZ6QSRWPQ2JE9E5B6N3PA0KBR9.amm-swap-pool-v1-1`, which relies on `SP3K8...KBR9.alex-vault-v1-1` which itself relies on the [semi-fongible trait contract](https://explorer.hiro.so/txid/0x74db763fbaa66da3368e642ddac48dc5ac81f3c6e5a9b1aaf358c5745608fdde).

The semi-fongible contract caused issues in the past because it's valid with Clarity 1 contract but **invalid** with Clarity 2. (The function `get-total-supply-fixed` is declared twice, which is now illegal). So when loaded with the wrong clarity version, this dependency would lead to issue.

Reference issues: #997, #1079.
