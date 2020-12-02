const text = await Deno.readTextFile("./coordenadas.json")
const data = JSON.parse(text) as Array<{x: number, y: number, z: number}>

const average = (nums: Array<number>) => nums.reduce((a, b) => (a + b)) / nums.length;

console.log(average(data.map(({x}) => x)))
console.log(average(data.map(({y}) => y)))
console.log(average(data.map(({z}) => z)))
