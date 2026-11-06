import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { expect, it } from "vitest";

import { getSDK } from "..";

it.each(["[network", "[network]\nname = 42"])(
  "reports malformed network settings and can initialize after correction: %s",
  async (content) => {
    const root = fs.mkdtempSync(path.join(os.tmpdir(), "clarinet-network-manifest-"));
    try {
      const manifestPath = path.join(root, "Clarinet.toml");
      const networkPath = path.join(root, "settings", "Devnet.toml");
      fs.mkdirSync(path.dirname(networkPath));
      fs.writeFileSync(manifestPath, '[project]\nname = "manifest-errors"\ntelemetry = false\n');
      fs.writeFileSync(networkPath, content);

      const simnet = await getSDK();
      await expect(simnet.initSession(root, "Clarinet.toml", null)).rejects.toThrow(
        /unable to parse .*Devnet\.toml/,
      );

      fs.copyFileSync("tests/fixtures/settings/Devnet.toml", networkPath);
      await simnet.initSession(root, "Clarinet.toml", null);
      expect(simnet.getAccounts().get("deployer")).toBe(
        "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM",
      );
    } finally {
      fs.rmSync(root, { recursive: true, force: true });
    }
  },
);
