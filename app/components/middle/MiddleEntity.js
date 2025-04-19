import use from "/lib/use"
import Module from "/components/middle/MiddleEntityModule"
import Process from "/components/middle/MiddleEntityProcess"

const components = { Module, Process }

export default function MiddleEntity() {
  const [entity] = use("entity")
  const C = components[entity?.type]
  return C ? <C /> : null
}
