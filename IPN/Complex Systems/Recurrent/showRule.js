const removeIfMee = x => (x === "ε" || x === "∅" ? "" : x);
const parentesis = x => (x.length < 2 ? x : `(${x})`);

function R(i, j, k) {
  if (k === 0) {
    if (i === 0 && j === 0) return "ε + 0";
    if (i === 0 && j === 1) return "1";
    if (i === 0 && j === 2) return "∅";
    if (i === 0 && j === 3) return "∅";
    if (i === 1 && j === 0) return "∅";
    if (i === 1 && j === 1) return "ε";
    if (i === 1 && j === 2) return "1";
    if (i === 1 && j === 3) return "1";
    if (i === 2 && j === 0) return "0";
    if (i === 2 && j === 1) return "1";
    if (i === 2 && j === 2) return "ε";
    if (i === 2 && j === 3) return "∅";
    if (i === 3 && j === 0) return "∅";
    if (i === 3 && j === 1) return "∅";
    if (i === 3 && j === 2) return "1";
    if (i === 3 && j === 3) return "ε + 0";
  }

  let a = parentesis(R(i, j, k - 1));
  let b = parentesis(R(i, k, k - 1));
  let c = parentesis(R(k, k, k - 1));
  let d = parentesis(R(k, j, k - 1));

  //console.log(`${a}+${b}(${c})^*${d}`);
  b = removeIfMee(b);
  c = removeIfMee(c);
  d = removeIfMee(d);

  const concat = b + c + d;
  a = a === "∅" ? "" : a;

  let result = a + (a === "" || concat === "" ? "" : " + ") + concat;
  if (a === concat) result = a;

  return result.length === 0 ? "∅" : result;
}

for (let k = 0; k < 3; ++k) {
  for (let i = 0; i < 4; ++i) {
    for (let j = 0; j < 4; ++j) {
      console.log(`R(${i}, ${j}, ${k}) = ` + R(i, j, k));
    }
  }
}
