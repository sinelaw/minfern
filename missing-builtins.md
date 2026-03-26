# Missing JavaScript Built-in Methods and Properties

Found while running minfern on the [depflame](https://github.com/example/depflame) JavaScript codebase. These are standard JavaScript built-in methods that real-world code commonly uses but minfern doesn't currently know about.

## String methods

minfern knows `String.length` but no String methods at all. Calling any method on a string literal causes a type mismatch (`expected 'String', found '{methodName: ...}'`) because minfern infers the object has a structural `.methodName` property rather than recognizing it as a built-in String method.

| Method | Signature | Notes |
|--------|-----------|-------|
| `indexOf` | `(String) => Number` | Most common — used for substring search |
| `substring` | `(Number, Number) => String` | |
| `substr` | `(Number, Number) => String` | Legacy but widely used |
| `slice` | `(Number, Number) => String` | |
| `split` | `(String) => String[]` | |
| `trim` | `() => String` | |
| `replace` | `(String, String) => String` | Simplified; full JS also accepts RegExp |
| `toUpperCase` | `() => String` | |
| `toLowerCase` | `() => String` | |
| `charAt` | `(Number) => String` | |
| `charCodeAt` | `(Number) => Number` | |
| `startsWith` | `(String) => Boolean` | ES6 |
| `endsWith` | `(String) => Boolean` | ES6 |
| `includes` | `(String) => Boolean` | ES6 |
| `repeat` | `(Number) => String` | ES6 |
| `padStart` | `(Number, String) => String` | ES2017 |

## Array methods

minfern knows `Array.length` and `[]` indexing but no Array methods. Calling any method gives `Property 'X' not found in type T[]`.

| Method | Signature | Notes |
|--------|-----------|-------|
| `push` | `(T) => Number` | Mutating; returns new length |
| `pop` | `() => T` | Mutating |
| `indexOf` | `(T) => Number` | |
| `slice` | `(Number, Number) => T[]` | Non-mutating copy |
| `join` | `(String) => String` | |
| `concat` | `(T[]) => T[]` | |
| `reverse` | `() => T[]` | Mutating |
| `sort` | `() => T[]` | Mutating (could also take comparator) |
| `map` | `((T) => U) => U[]` | Higher-order |
| `filter` | `((T) => Boolean) => T[]` | Higher-order |
| `forEach` | `((T) => void) => void` | Higher-order |
| `reduce` | `((U, T) => U, U) => U` | Higher-order |
| `some` | `((T) => Boolean) => Boolean` | Higher-order |
| `every` | `((T) => Boolean) => Boolean` | Higher-order |
| `find` | `((T) => Boolean) => T` | ES6; returns T or undefined |
| `includes` | `(T) => Boolean` | ES6 |
| `fill` | `(T) => T[]` | ES6; also `new Array(n).fill(v)` pattern |

## Math methods

minfern knows: `abs`, `ceil`, `floor`, `max`, `min`, `pow`, `random`, `round`, `sqrt`, `E`, `PI`.

Missing:

| Method | Signature | Notes |
|--------|-----------|-------|
| `log` | `(Number) => Number` | Natural logarithm — very common |
| `log2` | `(Number) => Number` | |
| `log10` | `(Number) => Number` | |
| `exp` | `(Number) => Number` | |
| `sin` | `(Number) => Number` | |
| `cos` | `(Number) => Number` | |
| `tan` | `(Number) => Number` | |
| `atan2` | `(Number, Number) => Number` | |
| `trunc` | `(Number) => Number` | ES6 |
| `sign` | `(Number) => Number` | ES6 |
| `hypot` | `(Number, Number) => Number` | ES6 |

## Object static methods

| Method | Signature | Notes |
|--------|-----------|-------|
| `Object.keys` | `(Object) => String[]` | Very common; minfern sees `.keys` as a structural property |
| `Array.isArray` | `(any) => Boolean` | Same issue |

## Already working

These work correctly:

- `JSON.stringify`, `JSON.parse`
- `parseInt`, `parseFloat`
- `isNaN`, `isFinite`
- `Number()`, `String()` (type coercion)
- `Math.abs`, `Math.ceil`, `Math.floor`, `Math.max`, `Math.min`, `Math.pow`, `Math.random`, `Math.round`, `Math.sqrt`
- `Math.E`, `Math.PI`
- `String.length`, `Array.length`, `Array[]` indexing

## Impact on real-world code

The depflame codebase (~1600 lines of JS) uses these patterns heavily:
- `str.indexOf(x)` for substring checks (feature resolution, dep parsing)
- `str.split('/')` for parsing `"child/feature"` entries
- `str.substring(4)` for extracting dep names from `"dep:foo"`
- `Math.log(x)` for logarithmic color scaling in the flamegraph
- `[].push()`, `[].slice()` for array manipulation throughout
- `Object.keys(obj)` for iterating over feature maps

Without String and Array methods, most real-world JavaScript cannot be checked.

## Stack overflow on recursive array types

Additionally, minfern stack-overflows on some patterns involving arrays that reference themselves through functions (e.g., a function that takes an array, copies it, and adds elements). This may be a bug in the recursive type handling rather than a missing feature.
