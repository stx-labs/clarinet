import { tx } from "@stacks/clarinet-sdk";
import { Cl } from "@stacks/transactions";
import { describe, expect, it } from "vitest";

const accounts = simnet.getAccounts();
const address1 = accounts.get("wallet_1")!;

describe("The billboard works as expected", () => {
  it("can get message", () => {
    const { result } = simnet.callReadOnlyFn(
      "billboard",
      "get-message",
      [],
      address1,
    );
    expect(result).toBeUtf8("Hello World!");
  });

  it("can set message", () => {
    const { result } = simnet.callPublicFn(
      "billboard",
      "set-message",
      [Cl.stringUtf8("New message")],
      address1,
    );
    // check that block height is increased, for demo purpose
    expect(simnet.blockHeight).toBe(2);

    expect(result).toBeOk(Cl.uint(110));

    const newMessage = simnet.getDataVar("billboard", "billboard-message");
    expect(newMessage).toBeUtf8("New message");
  });

  it("send an stx-transfer event on set message", () => {
    const { events } = simnet.callPublicFn(
      "billboard",
      "set-message",
      [Cl.stringUtf8("testing")],
      address1,
    );

    expect(events).toHaveLength(1);
    const transferEvent = events[0];
    expect(transferEvent.event).toBe("stx_transfer_event");
    expect(transferEvent.data).toStrictEqual({
      amount: "100",
      memo: "",
      recipient: `${simnet.deployer}.billboard`,
      sender: address1,
    });
  });

  it("increases the set message cost each time it's called", () => {
    const block = simnet.mineBlock([
      tx.callPublicFn(
        "billboard",
        "set-message",
        [Cl.stringUtf8("Message 1")],
        address1,
      ),
      tx.callPublicFn(
        "billboard",
        "set-message",
        [Cl.stringUtf8("Message 2")],
        address1,
      ),
      tx.callPublicFn(
        "billboard",
        "set-message",
        [Cl.stringUtf8("Message 3")],
        address1,
      ),
    ]);

    expect(block).toHaveLength(3);
    expect(block[0].result).toBeOk(Cl.uint(110));
    expect(block[1].result).toBeOk(Cl.uint(120));
    expect(block[2].result).toBeOk(Cl.uint(130));

    const newPrice = simnet.getDataVar("billboard", "price");
    expect(newPrice).toBeUint(130);
  });

  it("update stx balances", () => {
    const initialSTXBalances = simnet.getAssetsMap().get("STX");

    simnet.callPublicFn(
      "billboard",
      "set-message",
      [Cl.stringUtf8("New message")],
      address1,
    );

    const newSTXBalances = simnet.getAssetsMap().get("STX");
    expect(newSTXBalances?.get(address1)).toBeDefined();
    expect(newSTXBalances?.get(address1)).toBe(
      initialSTXBalances?.get(address1)! - 100n,
    );
    expect(newSTXBalances?.get(`${simnet.deployer}.billboard`)).toBe(100n);
  });
});
