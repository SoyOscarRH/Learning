import { Switch, Route } from "react-router-dom"
import Main from "./Main"
import Interface from "./Interface"
import Router from "./Router"

const App = () => (
  <Switch>
    <Route path="getTopo" component={Main} />
    <Route path="/:router/:int" component={Interface} />
    <Route path="/:router" component={Router} />
    <Route path="/" component={Main} />
  </Switch>
)

export default App
