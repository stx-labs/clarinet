// TS is disabled here because the project isn't configured to use it yet (tsconfig and other requirements)
// @ts-nocheck

const count = new DataVar<Uint>(0);

function printCount() {
  return print(count.get());
}

function getCount() {
  printCount();
  return count.get();
}

function increment() {
  return ok(count.set(count.get() + 1));
}

function add(n: Uint) {
  const newCount = count.get() + n;
  print(newCount);
  return ok(newCount);
}

export default {
  readOnly: { getCount },
  public: { increment, add },
} satisfies Contract;
