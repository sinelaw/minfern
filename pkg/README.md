# minfern

Type checker with full type inference for a subset of JavaScript.

Based on the type system developed for [infernu](https://github.com/sinelaw/infernu). See [infernu.md](infernu.md) for a partial formalization (incomplete: doesn't cover `this` resolution, rank restrictions on object properties, or value restriction).

See [mquickjs](https://github.com/bellard/mquickjs) for a relatively close subset of JavaScript.

## Strictest ever mode

minfern requires every variable (or function or object) to have a specific type, there are no union types. Every variable, expression, and function return must have exactly ONE type (though it may be polymorphic - see below). No type unions, no type changes, complete static type safety.

## Type System Features

- **Full type inference**: No type annotations required, all types inferred. You probably want them for documentation, minfern can add them for you.

- **Polymorphic ("generic") functions**: `function id(x) { return x; }` works with any type. See `tests/test_polymorphism.js` for more examples.
  
- **Structural typing**: Objects typed by their shape:

  ```javascript
  function getName(obj) {
      return obj.name;
  }
  
  var person = {name: "Alice", age: 30};
  var dog = {name: "Rover", breed: "Labrador"};
  var product = {name: "Widget", price: 9.99};
  
  var name1 = getName(person);   // "Alice"
  var name2 = getName(dog);      // "Rover"
  var name3 = getName(product);  // "Widget"
  ```

- **Type classes**: `+` on Number/String, `[]` on Array/String/Map/Object

- **Equi-recursive types**: Method chaining and builder patterns work naturally:

  ```javascript
  var requestBuilder = {
      url: "",
      method: "GET",

      setUrl: function(u) {
          this.url = u;
          return this;  // Returns the builder itself
      },

      setMethod: function(m) {
          this.method = m;
          return this;
      },

      send: function() {
          return this.method + " " + this.url;
      }
  };

  // Fluent chaining - each method returns the builder
  var response = requestBuilder
      .setUrl("/api/users")
      .setMethod("POST")
      .send();
  ```

  See `tests/test_builder_pattern.js` for more examples.

The rest is pretty basic (homogenous arrays, same-type conditionals and logical ops).

## Unsupported JavaScript Idioms

TLDR: Everything must have a specific type. An object can't sometimes have a value and sometimes be `null` or `undefined` or a change its type.

- No variable type changes: `var x = 1; x = "hello"`
- No union types: `return found ? obj : null` (Object vs Null)
- No mixed-type ternary: `condition ? 42 : "error"`
- No logical operators with different types: `obj && obj.property` (Object vs String)
- No default value pattern: `userName || "Guest"` (if types differ)
- No mixed-type arrays: `[1, "two", 3]`
- No type coercion: `"Count: " + 42` (String + Number)
- No multiple return types: `if (found) return obj; else return null;`
- No optional properties: `var u = {name: "Bob"}; u.age` → undefined (vs Number)
- No dynamic property access: `obj[key]` where `obj.name` is String, `obj.age` is Number
- No type guards: `if (typeof x === "string") return x.toUpperCase(); return x * 2;`

## Supported Syntax

Template literals, regex literals, getters/setters, method shorthand, `for-of`, `const`, `import`/`export` (parsed only - modules not resolved).

## Future Work

Some of the limitations above are annoying and may be worth supporting in some way or form. It would be nice to support nullable/optional-style union types, or explicit sum types. It would require some work to avoid losing the principal typing property (every expression has a single unambiguous most general type).

Not yet supported: import resolution, arrow functions, `let`, `class`, destructuring, spread/rest, `async`/`await`.

