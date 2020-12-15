import ReactDOM from "react-dom"
import { ChakraProvider } from "@chakra-ui/react"
import { HashRouter as Router } from "react-router-dom"
import { QueryClient, QueryClientProvider } from "react-query"

import App from "./Routes"

const queryClient = new QueryClient()

ReactDOM.render(
  <ChakraProvider>
    <QueryClientProvider client={queryClient}>
      <Router>
        <App />
      </Router>
    </QueryClientProvider>
  </ChakraProvider>,
  document.getElementById("root")
)
