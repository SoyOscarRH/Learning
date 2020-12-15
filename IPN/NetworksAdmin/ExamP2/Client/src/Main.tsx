import { Grid, Link, Spinner, Box } from "@chakra-ui/react"
import { Link as ReachLink } from "react-router-dom"
import { useQuery } from "react-query"

import getJSON from "./getJSON"

const Main = () => {
  const { isLoading, error, data } = useQuery("/routesInfo", () => getJSON("/getRoutes"))

  const showData = () => {
    if (isLoading) return <Spinner />
    if (error) return <Box backgroundColor="red"> {JSON.stringify(error)} </Box>

    const routers = data as Array<string>
    return routers.map(router => (
      <Link
        key={router}
        as={ReachLink}
        to={"/" + router}
        backgroundColor="#d0e8f2"
        borderRadius="0.5rem"
        padding="4"
      >
        {router}
      </Link>
    ))
  }

  return (
    <main style={{ padding: "4rem" }}>
      <Grid templateColumns="repeat(3, 1fr)" gap={6}>
        <Link
          as={ReachLink}
          to="/getTopo"
          backgroundColor="#d0e8f2"
          padding="4"
          borderRadius="0.5rem"
        >
          See the topology
        </Link>
        {showData()}
      </Grid>
    </main>
  )
}

export default Main
