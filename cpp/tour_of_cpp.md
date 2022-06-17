A Tour of C++

- Use {} initializer syntax  for explicitly typed variables since they avoid information loss in type conversions:
``
double a{1.2};
``

- `const` -> I promise not to change this value. `constexpr` -> i'll evaluate this at compile time.

- `*p` -> contents of, `&p` -> address of. `p&` -> reference to.

- references and pointer difference: reference is the route to the destination, pointer is the map of the route.

- gen: State intent in comments and implementation in code.
       OR
 gen: write why are you writing the code in the comments

- gen: `classes` separate implementation of types from their general user interface.

- a `struct` is simply a `class` with *public* members by default.

- `union` is a  `struct` in which all members are allocated at the same address.

- `union` can hold a value for only one member at a time.

- use *class enums* over *plain enums*.

- gen: Header files are actually interfaces for your source code.

- gen: Use precise types as the first wall of error handling.

- `noexcept` -> used in a function that should never through an error, if it does, call terminate() and exit.

- gen: when defining a function, consider it's preconditions and if feasible, test them.

- gen: class invariant/invariant is a statement which is assumed to be true.

- gen: constructors should establish the class invariants for the member functions.

- gen: member functions of a class should ensure that the class invariants holds when they exit.

- gen: it's better to find errors at compile-time (using `static_assert` etc ) than run-time.

- gen: develop error-handling strategy early in a design.

- Concrete type classes are basically the built-in types with additional functionality.

- The defining characteristic of a concrete type is that its representation is part of its definition.

-  gen: a function argument passed by value is copied.

- A container is an object holding a collection of elements e.g. `vector`.

- Avoid naked new and delete by  *RAII*.

- Using `{}` creates an object of type `std::initializer_list`.

- A `static_cast` does not check the value it is converting, programmer needs to check that.

- gen: Type casts implies that they are to prop up something that is broken.

- Abstract type classes completely insulates user from implementation details.

- gen: Inheritance is about de-coupling implementation and interface. Ergo, it will always include pointers or references to things.

- `dynamic_cast` can be used to check and do casting in one go.

- Use `vectors` to hold memory, `threads`  to hold system threads, `fstreams` to hold file handles.

- Templates are a compile-time mechanism.

- Templates can be used to:
pass types as arguments without loss of information.
delayed type checking
pass constant values as arguments.

- `vector` elements are stored contiguously in memory.

- `vector` does not guarantees range checking.

- `vector.at` method does do range checks by throwing an `out_of_range`  exception.

- Pass a container by reference and return a container by value.

- Iterators are a `concept`.

- `istream_iterator`s are used in pairs representing a sequence. (e.g. {inputstream, eos})

- A predicate for functions should not modify the elements to which it is applied.

- **Tag dispatching** is really useful for compile-time tag based dispatching of functions.

- `iterator_traits` can be used to implement tag dispatching.


