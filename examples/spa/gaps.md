# Gaps found while writing a minfern-checked SPA

This is the list of minfern limitations I ran into while writing the todo-list
example in this directory. Each one is either a workaround that had to be
applied to keep the JS both type-checkable and runnable in a browser, or
something I'd have reached for as a React-style JS dev and had to rewrite.

Where applicable I note the fix I used in `app.js`.

## Blockers that forced structural rewrites

### 1. No function hoisting

```js
function outer() { return inner(); }   // Error: Undefined variable: 'inner'
function inner() { return 1; }
```

Standard JavaScript hoists `function` declarations, so this works. minfern
resolves names strictly in source order, which also rules out mutual
recursion between two top-level functions.

**Workaround used in `app.js`**: a mutable `var doRender = function () { ... };`
placeholder near the top of the file, overwritten with the real renderer
after every function that `doRender` transitively references has been
defined. Every event handler calls `doRender()` rather than `render()`.

### 2. `const foo /*: T */;` without an initializer is a runtime syntax error *(resolved)*

Previously, `declare.md` recommended `const document /*: T */;` as the
external-declaration syntax, which is a `SyntaxError` in browsers. The SPA's
original workaround was `var document /*: T */;` (a no-op `var` that doesn't
overwrite the existing global).

This is now resolved by the `stdlib/` approach (see gap 4): the `const x;`
forms live in `stdlib/*.d.js`, which the type checker parses but no
JavaScript runtime ever sees. The SPA no longer declares anything —
`document` comes from the embedded stdlib.

The related limitation that `/*: T */` inline annotations (as sketched in
`declare.md`) aren't actually captured by the scanner still stands — only
`/** var x: T */` doc-comment annotations are. The stdlib files use the
doc-comment form.

### 3. No way to say "might be null"

There are no union types and no optional properties, so there's no way to
type `getElementById: (String) => Element | Null`. The declaration in
`app.js` pretends `getElementById` always succeeds. At runtime this is
fine for IDs we control, but it means minfern cannot protect against
typos like `getElementById("todo-lsit")`.

Related: `arr.find(...)`, `obj[missingKey]`, `JSON.parse` of arbitrary
input, any "not found" sentinel at all.

### 4. No built-in DOM / `window` / event types *(resolved)*

minfern now ships `stdlib/core.d.js` and `stdlib/dom.d.js`, baked into
the binary via `include_str!` and auto-loaded before every user program.
Users get `document`, `window`, `console`, `Math`, `JSON`, `setTimeout`,
etc. without writing any declarations themselves. Pass `--lib <path>` to
load additional user libs, or `--no-stdlib` to skip the built-in ones.

Still imperfect: the DOM types are a deliberately simplified subset —
`getElementById` returns one flat "Element" shape with all the fields
the SPA uses, because without union types there's no way to distinguish
`HTMLInputElement` from `HTMLDivElement`. Many `Math` / `Array` /
`String` methods are still missing (gaps 5 and 6).

## Missing standard-library methods

These are also covered in `missing-builtins.md`; the SPA hit them concretely.

### 5. No String methods

`escapeHtml` in `app.js` has to loop character-by-character because
`s.replace`, `s.split`, `s.replaceAll`, `s.toLowerCase` and friends are
all unavailable. For a UI-oriented SPA this is the most frequently painful
gap — even a non-regex `String.prototype.replace(String, String)` would
cover most of it.

### 6. No Array methods

No `push`, `pop`, `map`, `filter`, `slice`, `concat`, `find`, `includes`,
`forEach`, `splice`, etc. The SPA appends with `arr[arr.length] = x` and
implements `filter` as a hand-rolled `for`-loop in `deleteTodo`/`clearDone`.
Anything resembling functional update ("produce a new array with this item
changed") becomes a multi-line loop.

## Type-system papercuts

### 7. `String` and `String[]` collide under structural indexing

`escapeHtml(s)` only uses `s.length` and `s[i]`, which both a `String`
and a `String[]` satisfy identically (`.length: Number`, `[Number] -> String`),
so minfern defaulted the parameter to `String[]`. Calling the function
with a string literal in isolation then failed with
`expected 'String[]', found 'String'`. Oddly, inside the full app the same
code type-checked successfully even though the displayed signature was
still `(String[]) => String`, which suggests there's also a soundness
hole around when that row is unified.

**Workaround used in `app.js`**: add `var out = "" + s;` at the top of
`escapeHtml` to fire the `Plus` type-class and pin `s` to `String`.

### 8. Generic row-polymorphic helpers "forget" fields

```js
function removeById(arr, id) {
    var result = [];
    for (var i = 0; i < arr.length; i++) {
        if (arr[i].id !== id) { result[result.length] = arr[i]; }
    }
    return result;
}

var filtered = removeById(todos, 1);
var t = filtered[0].text;   // inferred as a free variable, not String
```

The function is inferred at `<a, b>({id: b | a}[], b) => {id: b | a}[]`, but
when called with the concrete todo array the returned element's `text`
field comes back as a fresh type variable rather than `String`. The fix
used in `app.js` was to drop the generic helper and inline a
todo-specific loop so the row type stays concrete.

### 9. Control-flow-dependent return typing is strict

Any non-trailing branch that uses a bare `return;` forces the whole
function's return type to `Undefined`, and the trailing path must then
also return `undefined` explicitly. Writing `return;` / `return undefined;`
by hand at the end of every event handler is a notable amount of
boilerplate; JavaScript programmers normally don't.

### 10. No ergonomic way to seed an empty typed collection

```js
var todos = [];           // a[]
todos[0] = {id: 1, ...};  // now locks in the type, but until then the
                          // whole file risks unifying against a[]
```

The SPA avoids this by seeding `state.todos` with two entries at init.
For a production app you'd want an "empty but already typed" array —
either a type annotation (`var todos /*: Todo[] */ = [];`) or a way to
write a type alias.

## Ergonomic gaps ("wouldn't have to think about this in React")

### 11. No arrow functions

Every closure is `function () { ... return undefined; }`. Per-item handler
factories (`makeToggleHandler`, etc.) exist mostly to close over a loop
variable; `let` or an arrow-plus-`forEach` would have made half of them
unnecessary.

### 12. No `let`

`for (let i ...)` would have avoided the loop-variable-capture pattern
(`makeToggleHandler(id)`) entirely. Every loop in `app.js` has to either
go through a factory function or do the IIFE `(function(x){ return ... })(x)`
dance.

### 13. No destructuring / spread / rest

`deleteTodo` and `clearDone` would have been one line each with
`state.todos = state.todos.filter(...)`. Instead they're hand-rolled
for-loops (also blocked by gap 6, of course).

### 14. No `class`, no `async`/`await`

Not used in this SPA, but a realistic app needs `fetch(...).then(...)`
or the `async/await` sugar. Without either, a network-backed variant
would need a callback-style workaround, and `fetch` itself isn't in the
initial env (gap 4).

## Summary

The todo-list SPA is feasible to write in minfern but the story is
roughly: "plus declarations for every DOM surface, minus a couple of
idioms per function, plus a forward-reference hack for render". A minfern
suitable for writing SPAs day-to-day would most want, in rough priority
order:

1. A real DOM environment shipped with the type checker (gap 4),
   which also closes gap 2.
2. `Array.prototype` + `String.prototype` built-ins (gaps 5, 6).
3. Function hoisting (gap 1) or first-class mutual recursion.
4. Some way to express nullability without full union types (gap 3) —
   even a `?T` sugar desugared to a tagged option would be a huge win.
5. `let` and arrow functions (gaps 11, 12).
