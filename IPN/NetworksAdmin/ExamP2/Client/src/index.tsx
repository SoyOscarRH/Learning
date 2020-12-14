import { StrictMode } from "react"
import ReactDOM from "react-dom"
import { ChakraProvider } from "@chakra-ui/react"
import { HashRouter as Router } from "react-router-dom"
import { QueryClient, QueryClientProvider } from "react-query"

import App from "./App"

const queryClient = new QueryClient()

ReactDOM.render(
  <StrictMode>
    <ChakraProvider>
      <QueryClientProvider client={queryClient}>
        <Router>
          <App />
        </Router>
      </QueryClientProvider>
    </ChakraProvider>
  </StrictMode>,
  document.getElementById("root")
)
