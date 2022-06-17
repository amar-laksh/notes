# Notes on Elements of Programming

## Chapter 1
This chapter builds the ontological foundation of the book.

### Entities
* There are two kinds of entities in the world.
    * **Abstract**: eternal and immutable
        * Example: Blue, 13, Physical matter.
    * **Concrete**: bounded and mutable.
        * Example: USA, Ada Lovelace

* **Attribute**: describes the connection between an Abstract and Concrete entity.
    * Example: Ada Lovelace's eye colour, USA's GDP

* **Identity**: abstraction of a concrete entity.
    * Example: Ada Lovelace as the first computer programmer, The American Dream

* Attributes of a concrete entity can change without affecting it's identity.
    * Example: Ada Lovelace's eye colour doesn't change her identity as the first computer programmer

* **Snapshot** of a concrete entity is a complete collection of it's attributes at a time *t*.
    * Example: USA's GDP number in year 2020 is a snapshot of all the components of its GDP in year 2020.

* **Abstract** **species**: describes common properties of similar abstract entities.
    * Example: 13 belongs to natural numbers species and Ada Lovelace's eye colour belongs to colour species.

* **Concrete** **species**: describes common properties of similar abstract entities.
    * Example: Ada Lovelace belongs to people species and USA belongs to countries species

* **Function(Pure Function)**: a rule that associates member(s) of an abstract species(arguments) with a member of another abstract species (result)
    * Example: The plus one (or successor) function associates each natural number with the one after it. succ(2) = 3

* **Abstract Genus**: describes common properties of similar abstract species
    * Example: arithmetic or binary operators on natural numbers

* **Concrete Genus**: describes common properties of similar concrete species
    * Example: Mammal genera, speech genera

* An entity only belongs to a single species that provides the rules for its construction/existence.
    * Example: Ada Lovelace's computer program only belongs to the written word species

* An entity could belong to several genera which might describe different attributes
    * Example: Ada Lovelace's computer program belongs to algorithms(which describe it as a set of instructions) or thoughts(which describe it as a set of ideas).

* **Objects** and **Values** represent entities.

* **Types** represent species.

* **Concepts** represent genera.

### Values

* **Datum**: a finite sequence of 0s and 1s.

* **Value-Type**:  a connection between a species and a set of datums.
    * Example: natural numbers species represented as int32_t in programming languages

* A datum corresponding to a particular entity is called **representation** of the entity.
    * Example: 0010 is the representation of natural number 2.

* A particular entity with a corresponding datum is called **interpretation** of the datum.
    * Example: the natural number entity 2 is the interpretation of the datum 0010.

* A datum is **well-formed** with respect to a value-type if and only if that datum represents an abstract entity.
    * Example: every sequence of 32 bits is well-formed when interpreted as an int32_t, datum *0* is not well-formed when interpreted as an ASCII string.

* A **proper subset** is a subset B of A that omits at least one value in the set of A.
    * Example: A is proper subset of B if A⊆B and A≠B.

* A value-type is **properly partial** if its values represent a proper subset of the abstract entities in the corresponding species.
    * Example: the type *int* is properly partial because:
        * int only represents values as datums (finite sequence) but the whole set integers Z is an infinite set, therefore it has to be a proper subset of the whole set of integers Z (inite set {1,2,3,4.. })

* A value-type is **total** if its values represent a subset of the abstract species which is also equal to the whole set of the abstract species.
    * Example: the type *bool* is total because:
        * bool represents values as datums (finite sequence) and the abstract species boolean is a set of only two values: true and false, therefore the subset of bool values is equal to the boolean whole set and bool is a total type.

* A value-type is **uniquely represented** if and only if at most one value corresponds to each abstract entity.
    * Counter-Example:
        * A type representing an integer as a sign bit and an unsigned magnitude byte does not uniquely represent zero. (since we can have +0 and -0, two different representations)
    * Example: A type representing an integer in two's compliment is uniquely represented because each abstract entity (Z) has at most one representation in two's compliment (there's only +0)

* A value-type is **ambiguous** if and only if a value of the type has more than one interpretation.
    * Example: A type representing a calendar year over multiple centuries as two decimal digits is ambiguous.
    * Counter-Example: `false` representing a boolean value of not true, is not ambiguous since it can have only one interpretation.

* Two values of a value-type are **equal** if and only if they represent the same abstract entity. (for javascript, this equality is denoted by typeof x == typeof y, for C++ it is typeid(x) == typeid(y))
    * Examples:
        * We shall denote this equality by the function `typeEquality()`
        * In C++, typeEquality(1,1.0) -> false. This makes sense since the values 1 and 1.0 do not represent the same abstract entity(they represent an integer and a floating point precision number respectively)
        * In Javascript, typeEquality(1,1.0) -> true. This does not make sense, since this assumes that the 1 and 1.0 represent the same abstract entity.

* Two values of a value-type are **representationally equal** if and only if their datums are identical.(for javascript, this equality is denoted by ==, C++  it is ==)
    * Example: 1 == 1 -> true in javascript and C++ because their datums are identical.

*  **Lemma 1.1**
    * if a value-type is uniquely represented, equality implies representational equality.

    * **Proof**:
        * Let **T** be a value-type that is uniquely represented and **A** be the corresponding abstract entity.
        * if a value-type is uniquely represented, for each **A**, **T** will have at most one value.
        * Assume equality between two values of **T**
        * if two values represent the same abstract entity, they must have the same value since **T** will have at most one value for representing the same **A**
        * Therefore, both values contain the identical datums.

    * **Example**: int a = 0, int b = 0; (a == b) -> representationally equal (since as `int` types can have only one representation of 0)

* **Lemma 1.2**
    * If a value-type is not ambiguous, representational equality implies equality.

    * **Proof**:
        * Let **T** be a value-type that is not ambiguous and **V** be value of **T**
        * if a value-type is not ambiguous, for each value **V** there is at most one interpreted abstract entity **A**
        * Assume representational equality between two values of **T**
        * if the datums are identical, there must be at most one interpreted abstract entity **A** for the same datum, then the two values must represent the same abstract entity.

    TODO work on these examples
    * **Counter-Example(Javascript)**: ("hello" === "hello") == "hello" -> false since "hello" is an ambiguous value-type (as it might represent more than one abstract type.)
    * **Example**: (true === true) == true -> true since both `true` are not ambiguous they both corresponds to the same abstract entity.

    * **Counter-Example(C++)**: (typeid('a') == typeid("a"))-> false since "a" is an ambiguous value-type (as it might represent more than one abstract type, `char` or `string`)
    * **Example**: (typeid(1) == typeid(1)) == true -> true since both `1` are not ambiguous they both corresponds to the same abstract entity.


* Computers implement functions on abstract entities as functions on values.

* A **regular function** defined on a value-type always gives *equal* result for an *equal* substitute value of the function argument (.i.e it respects *equality*).
    * Counter Example: type conversion functions (itoa(), atoi()). They all take arguments representing one abstract-type and return value-type representing another abstract-type.
    * Example: simple numeric functions like add, multiply, subtract (not divide)
 
* Regular functions allow **equational reasoning**: substituting equals for equals.

* Non-regular functions depend on the representation of their arguments.

* When designing the representation for a value-type, do two tasks: implement equality, and decide which functions will be regular.

### Objects

* An **object** is a representation of a concrete entity in memory.

* The object owns a set of resources to hold its state.

* The resources might not be stored in a contiguous fashion in memory.

* In terms of memory, It is the representation that gives unity to an object.

* An **object-type** is a pattern for storing and modifying values in memory.

* Every object belongs to an object-type.

* Values and objects play complementary roles.

* Values are unchanging and are independent of computer-specific implementation.

* Objects are changeable and have computer-specific implementation.

* Functional programming deals with values; imperative programming deals with objects.

* An object is well-formed iff its state is well-formed.

* An object-type is properly partial iff its value-type is properly partial; otherwise it is total.

* An object-type is uniquely represented iff its value-type is uniquely represented.


### Regular Types

* 
*
*
## Chapter Exercises

1. In the Javascript language, think and prove why:
```
    1 === 1.0000000000000001 -> true
    1 === 1.000000000000001 -> false
```
* What does this tell you about the value-types in Javascript?
* Do these hold for C++? What about your favourite programming language?

2. Why doesn't C++ have type equality operator `===` (as in Javascript) for built-in types?

3. Prove the statement "NaN is not a NaN" or "a NaN x is not representationally equal to NaN"
