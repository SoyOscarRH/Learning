import { useEffect, useState } from "react"
import { Box, Heading, Spinner } from "@chakra-ui/react"

const GetTopo = () => {
  const [isLoading, setLoading] = useState(true)
  useEffect(() => {
    fetch("http://localhost:8000/getTopo").then(() => setLoading(false))
  }, [])

  return (
    <>
      <Box borderRadius="1" margin="6">
        {isLoading && (
          <>
            <Heading>Creating Topology...</Heading>
            <Spinner />
          </>
        )}
        {!isLoading && (
          <>
            <Heading>Current Topology is:</Heading>
            <img alt="topology" src={`http://localhost:8000/topology?v${Math.random()}`} />
          </>
        )}
      </Box>
    </>
  )
}

export default GetTopo
