import { useParams } from "react-router-dom"
import { Box, Heading } from "@chakra-ui/react"

const Router = () => {
  const { router } = useParams<{ router: string }>()

  return (
    <>
      <Box borderRadius="1" margin="6">
        <Heading>{router}</Heading>
      </Box>
    </>
  )
}

export default Router
