import { ArNext } from "arnext"
import { Provider } from "@/components/ui/provider"
import "../lib/github-markdown.css"
export default function App(props) {
  return (
    <Provider>
      <ArNext {...props} />
    </Provider>
  )
}
