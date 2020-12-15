import { useParams } from "react-router-dom"
import { Box, Heading, Spinner, Input, Button } from "@chakra-ui/react"

import getJSON from "./getJSON"

const Router = () => {
  const { router } = useParams<{ router: string }>()
  const { isLoading, error, data } = useQuery("/", () => getJSON("/router/" + router))

  if (isLoading) return <Spinner />
  if (error) return null

  const { name, ip_id, model, version } = data as {
    name: string
    ip_id: string
    model: string
    version: string
  }

  return (
    <>
      <Box borderRadius="1" margin="6">
        <form
          onSubmit={e => {
            // @ts-ignore
            window.elpepe = e
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
      </Box>
    </>
  )
}

export default Router
