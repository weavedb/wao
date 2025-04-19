import use from "/lib/use"
import Module from "/components/middle/MiddleEntityModule"
import Process from "/components/middle/MiddleEntityProcess"
import Message from "/components/middle/MiddleEntityMessage"
import Account from "/components/middle/MiddleEntityAccount"

const components = { Module, Process, Message, Account }

export default function MiddleEntity() {
  const [entity] = use("entity")
  const C = components[entity?.type]
  return C ? <C /> : null
}
