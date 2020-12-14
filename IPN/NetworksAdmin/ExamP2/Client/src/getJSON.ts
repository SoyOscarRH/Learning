const getJSON = async <T = unknown>(url: string) => {
  const request = await fetch("http://localhost:8000" + url, {mode: "cors"})
  const json = await request.json()

  return json as T
}

export default getJSON
