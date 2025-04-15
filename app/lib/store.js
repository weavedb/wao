import { createStore } from "zustand/vanilla"
import { useStore as useZustand } from "zustand"

const getPath = (obj, path) =>
  path.split(".").reduce((o, k) => (o ? o[k] : undefined), obj)

const setPath = (obj, path, value) => {
  const keys = path.split(".")
  const lastKey = keys.pop()
  const newObj = structuredClone(obj)
  let ptr = newObj
  for (const key of keys) {
    ptr[key] = { ...(ptr[key] ?? {}) }
    ptr = ptr[key]
  }
  ptr[lastKey] = value
  return newObj
}

export default function store(initialState) {
  const zustandStore = createStore(set => ({
    state: initialState,
    set: (path, value) => set(s => ({ state: setPath(s.state, path, value) })),
  }))

  function use(path) {
    const selector = s => getPath(s.state, path)
    const value = useZustand(zustandStore, selector)
    const setValue = v => zustandStore.getState().set(path, v)
    return [value, setValue]
  }

  // Utilities
  use.get = path => getPath(zustandStore.getState().state, path)
  use.set = (path, val) => zustandStore.getState().set(path, val)
  use.store = zustandStore

  return use
}
