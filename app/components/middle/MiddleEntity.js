import use from "/lib/use"
import Module from "/components/middle/MiddleEntityModule"
import Process from "/components/middle/MiddleEntityProcess"
import Message from "/components/middle/MiddleEntityMessage"

const components = { Module, Process, Message }

export default function MiddleEntity() {
  const [entity] = use("entity")
  const C = components[entity?.type]
  return C ? <C /> : null
}
