import _Link from "./link"
import { useParams as _useParams } from "./params"
import { useRouter as _useRouter } from "./router"
import ArNext_ from "./ArNext"

export const isArweave = process.env.NEXT_PUBLIC_DEPLOY_TARGET === "arweave"
export const Link = _Link
export const useParams = _useParams
export const useRouter = _useRouter
export const ArNext = ArNext_
