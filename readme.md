# StrictScript

A toy compile-to-javascript language with a structural, fully inferred
type system.

Some of the current features:
  * Structural typing with row polymorphism (see below).
  * Simple Lua-like syntax without deliminators or whitespace sensitivity.
  * Translates fairly directly to understandable JavaScript.
  
Immediate problems:
  * Implicit 'this'
  * Primitive types such as String are also objects.

To do (short-term):
  * Sum types
  * Zero overhead 'Maybe' type (using null)
  * Extensive source comments
  * Module system


## Syntax

It's currently a mix of JavaScript and Lua. The JavaScript influence
means it's familiar, but it has some nicities of the Lua language,
including a lack of semicolons. The syntax is not whitespace
significant.

Example:

    var id = fn (x) { return x }

    var sayHello = fn (name) {
      console.log("Hello " ++ name ++ ".")
    }

    sayHello("to you")

    # Has type {name : String ...} -> ()
    var sayHelloPoly = fn (thing) {
      console.log("Hello " ++ thing.name ++ ".")
    }
    
    sayHelloPoly({name: "to me", age: 20})
    sayHelloPoly({name: "to me", nickname: "meh"})

The main differences from JavaScript here are the shortened 'function'
keyword, a lack of semicolons, and the comment character (#).


## Types

The type system is fully HM type-inferred.

See: [Algorithm W Step-By-Step](https://github.com/KMahoney/Algorithm-W-Step-By-Step)

Objects are constructed with curly braces and are structurally
typed. This means functions can take any object that implements a
required interface. The type system is row polymorphic, which takes
care of the loss of information problem.

See: [Objects and Aspects: Row Polymorphism](http://www.cs.cmu.edu/~aldrich/courses/819/slides/rows.pdf)

Object types are represented as `{field1: type1, field2: type2 ...}`.
The `...` means the type is polymorphic, i.e. `{name: String ...}`
will accept the object `{name: "Test", age: 20}`.

