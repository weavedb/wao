import use from "/lib/use"
import Module from "/components/middle/MiddleEntityModule"
import Process from "/components/middle/MiddleEntityProcess"
import Message from "/components/middle/MiddleEntityMessage"
import Account from "/components/middle/MiddleEntityAccount"
import Block from "/components/middle/MiddleEntityBlock"
import Tx from "/components/middle/MiddleEntityTx"
import Assignment from "/components/middle/MiddleEntityAssignment"

const components = { Module, Process, Message, Account, Block, Tx, Assignment }

export default function MiddleEntity() {
  const [entity] = use("entity")
  const C = components[entity?.type]
  return C ? <C /> : null
}
