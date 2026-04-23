// minfern stdlib: core JavaScript built-ins.
//
// This file is embedded in the minfern binary and auto-loaded before every
// user program. It is parsed and type-checked by minfern but never executed
// by any JavaScript runtime, so it freely uses `const x;` without an
// initializer (a SyntaxError in real JS) to express external bindings.
//
// Only non-polymorphic bindings live here. Truly polymorphic ones (Array,
// String, Number, Boolean constructors, JSON) stay in Rust so that each
// lookup produces fresh type variables — minfern's handling of polymorphic
// types inside annotation-declared schemes doesn't yet re-instantiate on
// every use.

/** const console: {log: (T) => Undefined, error: (T) => Undefined, warn: (T) => Undefined} */
const console;

/** const Math: {PI: Number, E: Number, abs: (Number) => Number, floor: (Number) => Number, ceil: (Number) => Number, round: (Number) => Number, sqrt: (Number) => Number, pow: (Number, Number) => Number, min: (Number, Number) => Number, max: (Number, Number) => Number, random: () => Number} */
const Math;

// parse and stringify share the same type variable T within this annotation,
// so `JSON.stringify(JSON.parse(s))` threads a consistent round-trip type.
// They are not truly polymorphic (each lookup does not re-instantiate), but
// this matches the prior behaviour when JSON was defined in Rust.
/** const JSON: {parse: (String) => T, stringify: (T) => String} */
const JSON;

/** const parseInt: (String) => Number */
const parseInt;

/** const parseFloat: (String) => Number */
const parseFloat;

/** const isNaN: (Number) => Boolean */
const isNaN;

/** const isFinite: (Number) => Boolean */
const isFinite;
