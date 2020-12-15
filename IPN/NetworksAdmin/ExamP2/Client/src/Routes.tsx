import { Switch, Route } from "react-router-dom"
import Main from "./Main"
import GetTopo from "./GetTopo"
import Interface from "./Interface"
import Router from "./Router"

const Routes = () => (
  <Switch>
    <Route path="/getTopo" component={GetTopo} />
    <Route path="/:router/:int" component={Interface} />
    <Route path="/:router" component={Router} />
    <Route path="/" component={Main} />
  </Switch>
)

export default Routes
