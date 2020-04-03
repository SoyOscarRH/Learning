const nonEmpty = (x: string) => (x === "" ? "∅" : x);

const parentesis = (x: string) => {
  if (x.length < 2) return x;
  if (x[x.length - 1] === ")") return x;
  if (x.split("").every(e => e === "1" || e === "0")) return x;

  return `(${x})`;
};

const or = (x: Array<string>) => {
  const elements = x.filter(e => e).filter(e => e !== "∅");
  return [...new Set(elements)].join(" + ");
};

const and = (x: Array<string>) => {
  if (x.some(e => e === "∅")) return "∅";
  return x
    .filter(e => e)
    .filter(e => e !== "ε")
    .filter(e => e !== "ε^*")
    .map(e => parentesis(e))
    .join("");
};

const kleeneClosure = (x: string) => {
  const data = x.split(" + ");
  const result = data.filter(e => e !== "ε").join(" + ");

  if (!result) return "";

  return `(${result}^*)`;
};

const memo = [
  ["", "", "", ""],
  ["", "", "", ""],
  ["", "", "", ""],
  ["", "", "", ""]
];

memo[0][0] = "0";
memo[0][1] = "1";
memo[1][2] = "1";
memo[1][3] = "0";
memo[2][0] = "1";
memo[2][1] = "0";
memo[3][2] = "0";
memo[3][3] = "0";

memo[1][1] = "ε";
memo[2][2] = "ε";

memo[0][2] = "∅";
memo[0][3] = "∅";
memo[1][0] = "∅";
memo[2][3] = "∅";
memo[3][0] = "∅";
memo[3][1] = "∅";

memo[0][0] = memo[0][0] + " + ε";
memo[3][3] = memo[3][3] + " + ε";

function R(i: number, j: number, k: number): string {
  if (k === 0) return memo[i][j];

  //console.log(`${a}+${b}(${c})^*${d}`);
  const a = R(i, j, k - 1);

  const b = parentesis(R(i, k, k - 1));
  const c = R(k, k, k - 1);
  let d = parentesis(R(k, j, k - 1));

  if (c === "ε" && d === "ε") d = "";

  const concat = and([b, kleeneClosure(c), d]);
  return nonEmpty(or([a, concat]));
}

for (let k = 0; k < 4; ++k) {
  for (let i = 0; i < 4; ++i) {
    for (let j = 0; j < 4; ++j) {
      console.log(`R(${i}, ${j}, ${k}) = ` + R(i, j, k));
    }
  }
  console.log();
}
