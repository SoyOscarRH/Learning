const params = JSON.stringify({ a: 10, b: 20, c: 30 });

const headers = {'Content-Type': 'application/x-www-form-urlencoded'}
const options = { method: "POST", headers };
const url = "http://sisdis.sytes.net:8080/Servicio/rest/ws/prueba?a=10&b=20&c=30";
const response = await fetch(url, options);
const text = await response.text();

console.log(text);
