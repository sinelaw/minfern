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

### 2. `const foo /*: T */;` without an initializer is a runtime syntax error

`declare.md` recommends

```js
const document /*: { getElementById: (String) => Element } */;
```

for external type declarations. That is a `SyntaxError: Missing initializer
in const declaration` in every JavaScript runtime, so a file using it can
be type-checked but never loaded as a `<script>`. `var document /*: T */;`
is treated by minfern as an immutable declaration and *is* legal runtime
JS (a `var` without an initializer doesn't replace an existing global), so
that's what `app.js` uses. This is fragile — inside a module or in strict
mode it would shadow the real `document`.

A saner design would be a built-in DOM environment (see gap 4) or a
dedicated declaration syntax that the runtime can safely ignore.

### 3. No way to say "might be null"

There are no union types and no optional properties, so there's no way to
type `getElementById: (String) => Element | Null`. The declaration in
`app.js` pretends `getElementById` always succeeds. At runtime this is
fine for IDs we control, but it means minfern cannot protect against
typos like `getElementById("todo-lsit")`.

Related: `arr.find(...)`, `obj[missingKey]`, `JSON.parse` of arbitrary
input, any "not found" sentinel at all.

### 4. No built-in DOM / `window` / `console.error` / event types

minfern's initial environment has `console.log`, `Math`, `JSON`, a few
numeric coercions, and nothing else. Every DOM surface has to be redeclared
inline in the user's file, which both bloats the file and means every user
re-invents a slightly different half-right DOM shape. It also forces gap 2:
you need to declare `document` *somehow*, and the only runtime-safe spelling
is a bare `var document;`.

I'd suggest either a pre-baked `dom.d.js` (with real module resolution) or
a `--lib dom` flag that seeds the initial environment.

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
