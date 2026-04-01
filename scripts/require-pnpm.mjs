const ua = process.env.npm_config_user_agent ?? '';

if (!ua.startsWith('pnpm/')) {
  process.stderr.write('Error: Do not use `npm publish`, use `pnpm` with the publish scripts from the root package.json.\n');
  process.exit(1);
}
