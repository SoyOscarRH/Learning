import { useParams } from "react-router-dom"
import { Box, Heading, Spinner, Input, Link } from "@chakra-ui/react"
import { Link as ReachLink } from "react-router-dom"

import { useQuery } from "react-query"

import getJSON from "./getJSON"

const Interface = () => {
  const { router, int } = useParams<{ router: string; int: string }>()
  const { isLoading, error, data } = useQuery(`/router/${router}/${int}`, () =>
    getJSON(`/router/${router}/${int}`)
  )

  if (isLoading) return <Spinner />
  if (error) return null

  const { name, ip_id, mask, connected_router_name } = data as {
    name: string
    ip_id: string
    mask: string
    connected_router_name: string
  }

  return (
    <>
      <Box borderRadius="1" margin="6">
        <Heading>
          {router} | Interface FA{int[0]}/{int[1]}
        </Heading>
        Name: <Input name="name" defaultValue={name} />
        IP: <Input name="ip_id" defaultValue={ip_id} />
        Mask: <Input name="mask" defaultValue={mask} />
        Connected thing:{" "}
        {connected_router_name === "terminal" ? (
          connected_router_name
        ) : (
          <Link as={ReachLink} to={"/" + connected_router_name}>
            {connected_router_name}
          </Link>
        )}
      </Box>
    </>
  )
}

export default Interface
