import * as wasm from "./waosm_bg.wasm";
export * from "./waosm_bg.js";
import { __wbg_set_wasm } from "./waosm_bg.js";
__wbg_set_wasm(wasm);