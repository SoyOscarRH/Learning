import { useParams } from "react-router-dom"
import { Box, Heading, Spinner, Input, Button, Grid, Link } from "@chakra-ui/react"
import { useQuery } from "react-query"
import { Link as ReachLink } from "react-router-dom"

import getJSON from "./getJSON"

const Router = () => {
  const { router } = useParams<{ router: string }>()
  const { isLoading, error, data } = useQuery("/getRouterInfo" + router, () =>
    getJSON("/router/" + router)
  )

  if (isLoading) return <Spinner />
  if (error) return null

  const { info, interfaces, users } = data as {
    info: Record<string, string>
    interfaces: any[]
    users: any[]
  }
  const { name, ip_id, model, version } = info
  users.push({ username: "admin", password: "admin" })
  users.push({ username: "", password: "" })

  return (
    <>
      <Box borderRadius="1" margin="6">
        <form
          onSubmit={e => {
            const elements = ((e.target as unknown) as { elements: any }).elements
            const data = {
              name: elements.name.value,
              ip_id: elements.ip_id.value,
              model: elements.model.value,
              version: elements.version.value,
            }

            fetch("http://localhost:8000/router/editRouter/" + router, {
              method: "POST",
              headers: { "Content-Type": "application/json" },
              body: JSON.stringify(data),
            })

            e.preventDefault()
            return false
          }}
        >
          <Heading>{router}</Heading>
          Name: <Input name="name" defaultValue={name} />
          IP: <Input name="ip_id" defaultValue={ip_id} />
          Model: <Input name="model" defaultValue={model} />
          Version: <Input name="version" defaultValue={version} />
          <Button mt={4} colorScheme="teal" type="submit">
            Edit
          </Button>
        </form>

        <Grid templateColumns={"1fr 1fr"} gap={2} margin={3}>
          {interfaces.map((int: any) => (
            <Link
              as={ReachLink}
              to={`/${router}/${int.name[0] + int.name[2]}`}
              backgroundColor="#d0e8f2"
              padding="4"
              borderRadius="0.5rem"
            >
              {int.name}: {int.ip_id}
            </Link>
          ))}
        </Grid>

        <Heading>Users</Heading>
        <Grid templateColumns={"1fr 1fr"} gap={2} margin={3}>
          {users.map((user: { username: string; password: string }) => (
            <form
              style={{ paddingBottom: "2rem" }}
              onSubmit={e => {
                const elements = ((e.target as unknown) as { elements: any }).elements
                const data = { name: elements.name.value, password: elements.password.value }

                fetch("http://localhost:8000/user/" + router, {
                  method: "POST",
                  headers: { "Content-Type": "application/json" },
                  body: JSON.stringify({ ...data, old: user.username }),
                })

                e.preventDefault()
                return false
              }}
            >
              {user.username === "admin" ? (
                <>
                  username: <Input name="name" disabled value={user.username} />
                  password: <Input name="password" disabled value={user.password} />
                </>
              ) : (
                <>
                  username: <Input name="name" defaultValue={user.username} />
                  password: <Input name="password" defaultValue={user.password} />
                  <Button mt={4} colorScheme="teal" type="submit">
                    {user.username === "" ? "add it!" : "edit it!"}
                  </Button>
                </>
              )}
            </form>
          ))}
        </Grid>
      </Box>
    </>
  )
}

export default Router
