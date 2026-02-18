const store = {}
const listeners = {}

export function set(key, val) {
  store[key] = val
  if (listeners[key]) listeners[key].forEach((fn) => fn(val))
}

export function get(key) {
  return store[key]
}

export function on(key, fn) {
  if (!listeners[key]) listeners[key] = []
  listeners[key].push(fn)
  return () => {
    listeners[key] = listeners[key].filter((f) => f !== fn)
  }
}
