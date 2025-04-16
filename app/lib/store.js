import { createStore } from "zustand/vanilla"
import { useStore } from "zustand"

export default function store(initial) {
  const zustandStore = createStore((set, get) => {
    const state = {}
    for (const key in initial) {
      state[key] = initial[key]
      state[`set${capitalize(key)}`] = v =>
        set(s => (Object.is(s[key], v) ? s : { [key]: v }))
    }
    return state
  })

  function Use(key) {
    const value = useStore(zustandStore, s => s[key])
    const setter = zustandStore.getState()[`set${capitalize(key)}`]
    return [value, setter]
  }

  return Use
}

const capitalize = s => s[0].toUpperCase() + s.slice(1)
