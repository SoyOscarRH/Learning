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

const path = [
  ["", "", "", ""],
  ["", "", "", ""],
  ["", "", "", ""],
  ["", "", "", ""]
];

path[0][0] = "0";
path[0][1] = "1";
path[1][2] = "1";
path[1][3] = "0";
path[2][0] = "1";
path[2][1] = "0";
path[3][2] = "0";
path[3][3] = "0";

path[1][1] = "ε";
path[2][2] = "ε";

path[0][2] = "∅";
path[0][3] = "∅";
path[1][0] = "∅";
path[2][3] = "∅";
path[3][0] = "∅";
path[3][1] = "∅";

path[0][0] = path[0][0] + " + ε";
path[3][3] = path[3][3] + " + ε";

function R(i: number, j: number, k: number): string {
  if (k === 0) return path[i][j];

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
