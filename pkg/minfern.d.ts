/* tslint:disable */
/* eslint-disable */

export class CheckResult {
  private constructor();
  free(): void;
  [Symbol.dispose](): void;
  readonly success: boolean;
  readonly output: string;
  readonly program_type: string;
  readonly errors: any[];
}

/**
 * Type check JavaScript source code and return the result.
 */
export function check_types(source: string): CheckResult;

/**
 * Initialize the WASM module (call once at startup).
 */
export function init(): void;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly __wbg_checkresult_free: (a: number, b: number) => void;
  readonly checkresult_success: (a: number) => number;
  readonly checkresult_output: (a: number) => [number, number];
  readonly checkresult_program_type: (a: number) => [number, number];
  readonly checkresult_errors: (a: number) => [number, number];
  readonly init: () => void;
  readonly check_types: (a: number, b: number) => number;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
  readonly __wbindgen_free: (a: number, b: number, c: number) => void;
  readonly __wbindgen_exn_store: (a: number) => void;
  readonly __externref_table_alloc: () => number;
  readonly __wbindgen_externrefs: WebAssembly.Table;
  readonly __externref_drop_slice: (a: number, b: number) => void;
  readonly __wbindgen_start: () => void;
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
