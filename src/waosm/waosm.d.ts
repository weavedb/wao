/* tslint:disable */
/* eslint-disable */
export class Compressor {
  free(): void;
  constructor();
  compress(data: Uint8Array): Uint8Array;
}
export class Decompressor {
  free(): void;
  constructor();
  decompress(data: Uint8Array): Uint8Array;
}

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly __wbg_compressor_free: (a: number, b: number) => void;
  readonly compressor_new: () => number;
  readonly compressor_compress: (a: number, b: number, c: number) => number;
  readonly decompressor_decompress: (a: number, b: number, c: number) => number;
  readonly __wbg_decompressor_free: (a: number, b: number) => void;
  readonly decompressor_new: () => number;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
*
* @returns {InitOutput}
*/
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
*
* @returns {Promise<InitOutput>}
*/
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;
