import { ArNext } from "arnext"
import { Provider } from "@/components/ui/provider"
export default function App(props) {
  return (
    <Provider>
      <ArNext {...props} />
    </Provider>
  )
}
