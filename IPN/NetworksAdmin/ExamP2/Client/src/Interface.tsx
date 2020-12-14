import { useParams } from "react-router-dom"
import { Box, Heading } from "@chakra-ui/react"

const Interface = () => {
  const { router, int } = useParams<{ router: string; int: string }>()

  return (
    <>
      <Box borderRadius="1" margin="6">
        <Heading>{router} | Interface {int}</Heading>
      </Box>
    </>
  )
}

export default Interface
