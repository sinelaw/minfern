# Gaps found while writing a minfern-checked SPA

This started as the list of minfern limitations encountered while writing
the todo-list example in this directory. Several of them have since been
fixed — those entries are marked *(resolved)* and kept as a record of
what changed and why.

## Status summary

| # | Gap                                                  | Status       |
|---|------------------------------------------------------|--------------|
| 1 | No function hoisting                                 | *resolved*   |
| 2 | `const x /*: T */;` runtime-unsafe                   | *resolved*   |
| 3 | No nullability / optional types                      | open         |
| 4 | No built-in DOM / `window` / event types             | *resolved*   |
| 5 | No String methods                                    | *resolved*   |
| 6 | No Array methods                                     | *resolved*   |
| 7 | `String` and `String[]` collide under indexing       | open         |
| 8 | Generic row-polymorphic helpers "forget" fields      | open         |
| 9 | Control-flow-dependent return typing is strict       | open         |
| 10| No ergonomic way to seed an empty typed collection   | open         |
| 11| No arrow functions                                   | *resolved*   |
| 12| No `let`                                             | *resolved*   |
| 13| No destructuring / spread / rest                     | open         |
| 14| No `class`, `async`/`await`                          | open         |

## Resolved

### 1. No function hoisting *(resolved)*

Previously, `function outer() { return inner(); } function inner() {...}`
failed with `Undefined variable: 'inner'` because minfern resolved names
strictly in source order. Mutual recursion between peer functions was
equivalently blocked. The SPA's original workaround was a mutable
`var doRender = function () { ... };` placeholder rewritten at the bottom
of the file.

`infer_stmt_list` now packs adjacent `function` declarations into a
binding group and processes each group in two passes (hoist names with
fresh type variables, infer all bodies, generalise afterwards). Forward
references and mutual recursion between peers now type-check. The SPA
drops the `doRender` placeholder.

### 2. `const foo /*: T */;` without an initializer is a runtime syntax error *(resolved)*

`const x;` is a `SyntaxError` in every browser, so a file using it can
be type-checked but never loaded as a `<script>`. The SPA's workaround
was `var document /*: T */;`, which has its own fragility.

Resolved by the `stdlib/` approach (see gap 4): the `const x;` forms
live in `stdlib/*.d.js`, which the type checker parses but no JavaScript
runtime ever sees.

The related limitation that `/*: T */` inline annotations (as sketched
in `declare.md`) aren't actually captured by the scanner still stands —
only `/** var x: T */` doc-comment annotations are. The stdlib files
use the doc-comment form.

### 4. No built-in DOM / `window` / event types *(resolved)*

minfern now ships `stdlib/core.d.js` and `stdlib/dom.d.js`, baked into
the binary via `include_str!` and auto-loaded before every user program.
Users get `document`, `window`, `console`, `Math`, `JSON`, `setTimeout`,
etc. without writing any declarations themselves. Pass `--lib <path>` to
load additional user libs, or `--no-stdlib` to skip the built-in ones.

Still imperfect: the DOM types are a deliberately simplified subset —
`getElementById` returns one flat "Element" shape with all the fields
the SPA uses, because without union types there's no way to distinguish
`HTMLInputElement` from `HTMLDivElement`.

### 5. No String methods *(resolved)*

`escapeHtml` originally looped character-by-character because
`s.replaceAll`, `s.split`, `s.indexOf` and friends were all undeclared.
After the built-in method dispatch in `builtins/mod.rs::string_method_type`,
the SPA's `escapeHtml` is now a five-line `.replaceAll` chain.

Covers `indexOf`, `lastIndexOf`, `substring`, `substr`, `slice`, `split`,
`trim`, `trimStart`, `trimEnd`, `replace`, `replaceAll`, `toUpperCase`,
`toLowerCase`, `charAt`, `charCodeAt`, `startsWith`, `endsWith`,
`includes`, `repeat`, `padStart`, `padEnd`, `concat`, `toString`.

### 6. No Array methods *(resolved)*

`push`, `pop`, `map`, `filter`, `forEach`, `reduce`, etc. now dispatch
through `builtins/mod.rs::array_method_type`, which hands out a fresh
function type per lookup — polymorphic methods like `map`/`reduce` get a
fresh accumulator or result-element variable so each call site infers
independently.

The SPA's `deleteTodo`, `clearDone`, `countRemaining`, `hasAnyDone`,
`renderList`, and all the per-item handler factories collapsed from
loop-based to one-liners (`state.todos.filter(t => t.id !== id)` etc.).

`find` is typed `(T => Boolean) => T` — minfern has no union types, so
"not found" at runtime returns `undefined` regardless of the declared
type. Same trade-off `getElementById` makes.

### 11. No arrow functions *(resolved)*

`(x) => x + 1`, `x => x + 1`, `() => body`, and `{ ... }` block bodies
all parse now. The parser lowers arrows to `Expr::Function` with
`name: None`, so inference/decoration/printing are unchanged.

Not modelled: `this` binding. Arrow functions in JS inherit the
enclosing `this`; minfern's function handler re-creates a fresh `this`
variable for every function regardless. The SPA happens not to care.

### 12. No `let` *(resolved)*

`let` now parses as an alias for `var`. Per-block scoping and the
temporal-dead-zone aren't modelled (a `let` inside a `for` still shares
one binding across iterations for inference purposes) but the statement
parses and the types fall out correctly.

## Open gaps

### 3. No way to say "might be null"

There are no union types and no optional properties, so there's no way
to type `getElementById: (String) => Element | Null`. The shipped DOM
library pretends `getElementById` always succeeds. At runtime this is
fine for IDs we control, but it means minfern cannot catch typos like
`getElementById("todo-lsit")`.

Related: `arr.find(...)`, `obj[missingKey]`, `JSON.parse` of arbitrary
input, any "not found" sentinel at all. This is the one gap that genuinely
needs a type-system extension (unions, options, or an explicit `?T` sugar).

### 7. `String` and `String[]` collide under structural indexing

A function that only uses `s.length` and `s[i]` satisfies both `String`
and `String[]` identically, so minfern picks one by unification order
and may default wrongly. The workaround is to throw a `"" + s` into the
body to trigger the `Plus` type-class and pin it to `String`. Now that
`String.prototype` methods dispatch directly (gap 5), most real code
touches at least one string-specific method and the ambiguity never
fires — but a pure `length`/`[i]` function still has the problem.

### 8. Generic row-polymorphic helpers "forget" fields

A polymorphic helper `function removeById(arr, id)` is inferred at
`<a, b>({id: b | a}[], b) => {id: b | a}[]`, but when called with a
concrete todo array the returned element's `text` field comes back as
a free type variable rather than `String`. The workaround is to inline
the helper (lose the generic). Needs attention in the inference engine.

### 9. Control-flow-dependent return typing is strict

Any non-trailing branch that uses a bare `return;` forces the whole
function's return type to `Undefined`, and the trailing path must then
also return `undefined` explicitly. Handler bodies still need an explicit
`return undefined;` at the end (see `app.js` event handlers).

### 10. No ergonomic way to seed an empty typed collection

```js
var todos = [];           // a[]
todos[0] = {...};         // locks in the type
```

The SPA avoids this by seeding `state.todos` with two entries at init.
For a production app you'd want either an annotation
(`var todos /*: Todo[] */ = [];`) or a type alias.

### 13. No destructuring / spread / rest

`let {id, text} = todo;` and `const rest = [...tail];` would simplify
about a dozen sites in the SPA. Pure parser work but not done.

### 14. No `class`, `async`/`await`

A network-backed variant would need `fetch(...).then(...)` or
`async`/`await`. Neither parses. `fetch` itself isn't in the stdlib
either.

## What's left in priority order

1. **Nullability** (gap 3) — the one open gap that actually blocks idioms
   like `getElementById` safety, `find` results, and `JSON.parse`.
2. **Destructuring / spread / rest** (gap 13) — pure parser work, big
   readability win.
3. **`async`/`await`** (gap 14) — needs a `Promise<T>` built-in plus a
   parser arm; the rest falls out of existing generics.
4. **Inference polish** (gaps 7, 8, 9, 10) — smaller scope but each one
   removes a footgun.
