import { Cl, cvToHex } from "@stacks/transactions";
import { describe, expect, it } from "vitest";

const accounts = simnet.getAccounts();
const address1 = accounts.get("wallet_1")!;
const address2 = accounts.get("wallet_2")!;

/*
  The test below is an example. Learn more in the clarinet-sdk readme:
  https://github.com/hirosystems/clarinet/blob/develop/components/clarinet-sdk/README.md
*/

describe("nft basic features", () => {
  it("Ensure that nft can be minted", () => {
    const { result, events } = simnet.callPublicFn(
      "simple-nft",
      "test-mint",
      [Cl.standardPrincipal(address1)],
      address1,
    );
    expect(result).toBeOk(Cl.bool(true));

    expect(events).toContainEqual({
      event: "nft_mint_event",
      data: {
        asset_identifier: `${simnet.deployer}.simple-nft::nft`,
        raw_value: `0x${Buffer.from(Cl.serialize(Cl.uint(1))).toString("hex")}`,
        recipient: address1,
        // we can use asymmetric for clarity values
        value: expect.toBeUint(1),
      },
    });
  });

  it("Ensure that nft can be transferred form one account to another", () => {
    simnet.callPublicFn(
      "simple-nft",
      "test-mint",
      [Cl.standardPrincipal(address1)],
      address1,
    );

    const { result, events } = simnet.callPublicFn(
      "simple-nft",
      "transfer",
      [
        Cl.uint(1),
        Cl.standardPrincipal(address1),
        Cl.standardPrincipal(address2),
      ],
      address1,
    );

    expect(result).toBeOk(Cl.bool(true));
    expect(events).toHaveLength(1);

    // if we know the index of the event
    // it can be used instead of `expect(events).toContainEqual(...)`
    const transferEvent = events[0];

    expect(transferEvent.event).toBe("nft_transfer_event");
    expect(transferEvent.data).toStrictEqual({
      asset_identifier: `${simnet.deployer}.simple-nft::nft`,
      raw_value: cvToHex(Cl.uint(1)),
      recipient: address2,
      sender: address1,
      value: expect.toBeUint(1),
    });
  });

  it("Ensures that nft can be burned", () => {
    simnet.callPublicFn(
      "simple-nft",
      "test-mint",
      [Cl.standardPrincipal(address1)],
      address1,
    );

    const { result, events } = simnet.callPublicFn(
      "simple-nft",
      "test-burn",
      [Cl.uint(1), Cl.standardPrincipal(address1)],
      address1,
    );
    expect(result).toBeOk(Cl.bool(true));

    expect(events).toContainEqual({
      event: "nft_burn_event",
      data: {
        asset_identifier: `${simnet.deployer}.simple-nft::nft`,
        raw_value: `0x${Buffer.from(Cl.serialize(Cl.uint(1))).toString("hex")}`,
        sender: address1,
        value: expect.toBeUint(1),
      },
    });
  });
});
