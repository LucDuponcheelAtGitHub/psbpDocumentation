import VersoManual

import PsbpDocumentation.Meta.Lean

open Verso.Genre Manual

open Verso.Genre.Manual.InlineLean

open PsbpDocumentation

set_option pp.rawOnError true

#doc (Manual) "PSBP Documentation" =>

%%%
authors := ["Luc Duponcheel"]
%%%

# About

This document is about the
[`Lean` `PSBP` library](https://github.com/LucDuponcheelAtGitHub/PSBP),
in what follows, also simply called `PSBP`.

This document can be seen as a, somewhat special, not to say opinionated, programming course, especiallty useful for
mathematicians and/or computer scientists, researchers as well as students, possible future researchers, who are
interested in mathematical foundations of programming.

The code of this document can be seen as a programming course for `Lean` itself. `Lean` turns out to be both a very
demanding and very helpful student.

When I worked with Doatse Swierstra at the University of Utrecht, he once told me that, apart from sound proving
techniques like "proof by induction", there is also this unsound proving technique "proof by intimidation". When no
student complains about the correctness of a proof, the proof is correct. Of course, Doaitse did not apply this
technique when teaching. `Lean` would be this very demanding student asking you for more and more into proof details
before accepting the correctness of the proof. But `Lean` would also be this very helpful student that would be able to
infer the proof details for you if you are stuck.

The first sections of this course do not always explain all concepts that they deal with. Please keep on reading. All
concepts will, eventually, be explained. Starting from section "The PSBP Library type classes" the course is self
contained and requires, at least in theory, no previous knowledge.

So let's end this section with a, somewhat offensive, motivation to keep on reading this document.

If `Lean` can understand the course then you should be able to understand it as well.

# Naming conventions

In this document, `program`, intentionally written in typewriter font, has a specific, technical meaning while the word
programming has a general meaning. For general purposes, this document uses the word code instead of the word program.
As a consequence, in this document, the word programming means (the process of) writing code.

I hope that this naming convention does not lead to any confusion.

`PSBP` stands for "Program Specification Based Programming" (using the naming conventions above).

`PSBP` is a pointfree effectful functional programming library, written using the `Lean` programming language.

In what follows `Lean` will rarely explictly be mentioned. It will implicitly be taken for granted.

`PSBP` can be seen as a programming DSL, a Domain Specific Language for the programming domain.

`PSBP` has a variety of programming related binary type constructor classes, among others `Functional`, `Functorial`,
`Creational`, `Sequential` and `Conditional`. `PSBP` code consistently names their binary type constructor parameter
`program`.

The `PSPB` type constructor classes, just like all other classes, are specifications, also called interfaces in the
programming world. They have members that declare basic program specifications and basic program specification
combinators to combine program specifications to composite program specifications. Derived program specifications and
derived program specification combinators can then be defined in terms of (declared or defined) program specifications
and (declared or defined) program specification combinators. As such program specifications are components of a
component system.

The `Lean` standard library has a variety of computing related unary type constructor classes, among others `Functor`,
`Applicative` and `Monad`. `PSBP` code consistently names their unary type constructor parameter `computation`.

The `Lean` standard library type constructor classes above, just like all other classes, are specifications, also called
interfaces in the programming world. They have members that declare basic computation specifications and basic
computation specification combinators to combine computation specifications to composite computation specifications.
Derived computation specifications and derived computation specification combinators can then be defined in terms of
(declared or defined) computation specifications and (declared or defined) computation specification combinators. As
such computation specifications are components of a component system.

By now you may be asking why I used program specification resp. computation specification instead of simply using
program resp. computation. First of all, because program specifications resp. computation specifications are
specifications. Type constructor class instances, also called implemenations in the programming world, of the various
type constructor classes, in terms of whose members program specifications resp. computation specifications are defined,
need to be given to materialize program specifications resp. computation specifications. It are those materializations
that are called programs resp. computations.

In what follows, by abuse of language, I will often also use the word program (recall, a materialization of a program
specification corresponding to instances of programming related type constructor classes) instead of program
specification and often also use the word computation (recall, a materialization of a computation specification
corresponding to instances of computing related type constructor classes) instead of computation specification.

Hopefully this abuse of language does not lead to any confusion.

It is instructive to compare this abuse of language with the title
[Ceci n'est pas une pipe](https://en.wikipedia.org/wiki/The_Treachery_of_Images)
of the painting of René Magritte. The painting itself is, of course, not a pipe, it is a description of a pipe.
Yet, when you look at the painting, you might think of it as being a pipe. Much in the same way, when an architect is
showing you the specification of a house, for example as a 3D animation (a special kind of description), communication,
by abuse of language, happens using words like kitchen resp. bathroom instead of kitchen specification resp. bathroom
specification.

# Programs versus Computations

The standard `Lean` library already enables Computation Specification Based Programming.

So why promoting Program Specification Based Programming in the first place?

In short, informally, it is all a matter of taste
(you may wish to give up reading this document).

In short, formally, because of progressive insight into what programming, writing code, is all about.

Let's explain this in terms of the history about my progressive insight
(you may wish to ignore what follows, not being interested in my personal history).

I am a retired mathematician.

Mathematics is generally agreed upon to be useful to understand the reality we are all part of. For example to
understand problem domains some of us are, professionaly, confronted with.

Bridge building engineers may benefit form studying appropriate mathematics to understand what bridges are all about.
Vehicle building engineers may benefit form studying appropriate mathematics to understand what vehicles are all about.
Likewise, programming engineers (programmers) may benefit form studying appropriate mathematics to understand what
programs are all about.

Separating specifications from implementations is generally agreed upon to be useful to understand problem domains.

A bridge specification states, for example, that it must be able to carry the weight of a number of vehicles. How it is
able to carry that weight is an implementation concern. Maybe one bridge is more pleasing to look at as another one. A
vehicle specification states, for example, that it must be able to transport a number of passengers. How it is able to
transport that number of passengers is an implementation concern. Maybe one car is more comfortable than another one. A
program specification states, for example, that a program must be able to create composite data and to perform
conditional logic. How it is able to create composite data and to perform conditional logic is an implementation
concern. Maybe one program is more CPU (or GPU or NPU) effecient, or less RAM consuming than another one.

Note that I wrote bridge, resp. car, resp. program as an abbreviation of materialization corresponding to an implementation
of the bridge specification, resp. materialization corresponding to an implementation of the car specification, resp.
materialization corresponding to an implementation of the program specification.

I have always been interested in mathematics, so I followed the typical mathematics oriented education path from
secondary school all the way to obtaining a PhD in mathematics. Now I realize that obtaining a PhD (in mathematics) is
just a beginning. It shows that you are able to use 20% imagination and 80% transpiration to be creative in a particular
area (of mathematics).

I did, among others, mathematics research on
[Non-archimedean induced representations of compact zerodimensional groups](https://www.numdam.org/article/CM_1986__57_1_3_0.pdf).
Not that it matters much for the content of this document. What does matter is that I soon realized that, those days, in
Belgium, mathematics could mainly be done as a backyard ritual, so, in order to earn money for a living, I decided to
become a programmer. being adicted to mathematics research, I also decided to do computer science research as a late at
night hobby (typically starting at 9PM).

I studied function level programming systems, supported by the pointfree, effectfree functional programming language
[`FP`](https://en.wikipedia.org/wiki/FP_%28programming_language%29).

I published a paper
[Acceptable functional programming systems](https://link.springer.com/article/10.1007/BF00268076)
about a pointfree effectfree functional programming library. I wrote the paper together with my twin brother Marc, who
is also a mathematician. The library was written using the
[`Pascal`](https://en.wikipedia.org/wiki/Pascal_%28programming_language%29) programming language.

The first functional programming language I used extensively was
[`Miranda`](https://www.cs.kent.ac.uk/people/staff/dat/Miranda/).
`Miranda` turned out to be a perfect language to learn (not to say become addicted to) functional programming.

The next functional programming language I used extensively was `Gofer`, later evolving to
[`Hugs`](https://www.haskell.org/Hugs/).
`Gofer` was the first functional programming language supporting type constructor classes. They are appropriate to write
the computing related specifications of effectful pointful functional programming libraries. The mathematical
foundations of those specifications are [monads](https://en.wikipedia.org/wiki/Monad_%28functional_programming%29).

I published the following papers

1. [Composing monads](https://web.cecs.pdx.edu/~mpj/pubs/RR-1004.pdf),
2. [On the expressive power of constructor classes](https://link.springer.com/chapter/10.1007/978-1-4471-3573-9_3),
3. [Deterministic crror-correcting combinator parsers](https://www.cs.tufts.edu/~nr/cs257/archive/doaitse-swierstra/error-correcting.pdf), and
4. [Using catamorphisms, subtypes and monad transformers for writing modular functional interpeters](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=1cdebf7b88435a38156f62df1a7189b6d8ca3fe3).

The first paper was written, as a late night hobby, together with Mark P. Jones, the author of `Gofer`. What is
special about that paper is that we had never physically met each other. Those were the early days of the internet. We
wrote the paper (and the code that came with it) together by sending emails with attachments to each other. In fact, it
turned out to be an efficient way to work together. While one of us was sleeping (in Europe resp. the USA) the other one
was working (in the USA resp. Europe). The paper also contained some equational proofs. Those days, they were simply
encoded as lists of expressions representing equational proof steps. `Gofer` did not have a formal proof correctness
reviewing mechanism like Lean has, but, type correctness provided at least some formal proof correctness confidence.

The other papers were written when I was working, for two years, at the University of Utrecht. That was a unique
experience for me for which I am, forever, greatful to Doaitse Swierstrsa. Erik Meijer, also working at the University
of Utrecht, invited me for a lecture, as such introducing me to Doaitse. Apart from being an outstanding computer
scientist, Doaitse was the best people manager I have ever worked for. Erik does not need any introduction. He became
a living legend. Also Graham Hutton worked at the University of Utrecht when I was working there. I was in good company.

The second paper was written together with Erik Meijer and was presented at a conference organized by the Glasgow
University, the center of Functional Programming in the UK (Philip Wadler and Simon Peyton Jones worked there).

The third paper was written together with Doaitse Swierstra. It turned out to be a motivation for two type
constructor classes,
[arrows](https://link.springer.com/chapter/10.1007/11546382_2) and
[applicatives](https://www.researchgate.net/publication/215446169_Applicative_Programming_with_Effects),
based upon mathematical foundations: Arrow based libraries are pointfree effectful functional programming libraries.
Applicative based libraries are pointful effectful functional programming libraries. The relationship between them was
explored in
[Arrows and applicatives](https://homepages.inf.ed.ac.uk/wadler/papers/arrows-and-idioms/arrows-and-idioms.pdf)

The fourth paper was based on
[Monad transformers and modular interpreters](https://dl.acm.org/doi/pdf/10.1145/199448.199528).
I added catamorphisms and subtypes to make interpreter alternatives reusable. The paper has never been published.
Anyway, somehow, surprisingly, the paper turned out to be available online and it has been cited several times.

All this brings me to the progressive insight that motivates me to do this Lean `PSBP` project.

What are the most appropriate mathematical foundations and corresponding type classes for effectful functional
programming? The more powerful they are the less implementation flexibility they have. For example monadic parsers
can parse context sensitive grammars, while applicative parsers cannot parse context sensitive grammars, but,
applicative parsers allow for more flexible error handling than monadic parsers.

Programming is also about elegance and programming libraries are also about ease of use. I.m.h.o. pointfree programming
is more elegant than pointful programming and pointfree programming libraries are easier of use than pointful
programming libraries.

Of course this is a matter of taste, but let me motivate my taste ... .

The `Applicative` specification and the `Monad` specification specify computation capabilities. Think of computations as
effectful expressions. They are operational artifacts. They do not really have a meaning in the mathematical sense and
cannot be given a meaningful name. How, for example, would you name expression `x * x` in `λ x => x * x` ?
Just like expressions are evaluated to yield a value, computations, by somehow executing them, yield a value, but,
somehow executing computations may also perform side effects along the way. The "somehow" in the previous sentence is
important, because it depends on the materialization corresponding to instances of the type constructor classes in
terms of whose members the computations (recall, more precisely, computation specificatons) have been written.

The program specification of this document is similar to, but more powerful than, the arrow specification. Think of
programs as arrows with choice.

The program specification of this document specifies, not surprisingly, program capabilities. Think of programs as
effectful functions. They are denotational artifacts. They do have a meaning in the mathematical sense and can be given
a meaningful name. For example `λ x => x * x` can be given a the meaningful name `square`. Of course functions and
programs can also be looked at as operational artifacts. Just like functions, programs, by somehow running
them, transform an initial value to a final value, but, somehow running them may perform side effects along the way. The
"somehow" in the previous sentence is important, because it depends on the materialization corresponding to instances of
the type constructor classes in terms of whose members the programs (recall, more precisely, program specificaton) have
been written.

By the way, a value can be a basic-value or a composite-value, repesented as a (nested) tuple. As such
values are also components of a component system.

Why going for pointfree programs instead of pointful applicatives or pointful monads?
I.m.h.o. it is more natural to think denotationally, about "what", than to think operationally, about "how".

Let's try to illustrate this with some `Lean` code.

The computing related type class `Bind` has a member

```
bind :
  {α β : Type} →
  computation α → (α → computation β) → computation β
```

with an associativity law (`>>=` is infix notation for `bind`)

```
bind_assoc
  (cα : computation α)
  (αfcβ : α → computation β)
  (βfcγ : β → computation γ) :
    cα >>= αfcβ >>= βfcγ = cα >>=
      λ α => αfcβ α >>= βfcγ
```

The programming related type class `Sequential` has a member

```
andThen
  {α β γ : Type} :
  program α β → program β γ → program α γ
```

with an associativity law (`>=>` is infix notation for `andThen`)

```
andThen_assoc
  (αpβ : program α β)
  (βpγ : program β γ)
  (γpδ : program γ δ) :
  (αpβ >=> βpγ) >=> γpδ =
    αpβ >=> (βpγ >=> γpδ)
```

Let's first consider syntax.

I can more easily remember the definition of `andThen_assoc` than the definition of `bind_assoc`.

What about you?

Let's next consider semantics.

I can more easily explain `andThen` and `andThen_assoc` than `bind` and `bind_assoc`. I'll give it a try.

What about you? Give it a try.

Let's first deal with `Sequential`.

I think of a function as transforming an initial value yielding a final value (transforming an argument to a result).

Likewise, I think of a program as transforming an initial value yielding a final value (transforming an argument to a
result (potentially performing side effects along the way)).

Let's ignore side effects for now (you may wish to explain what happens with them).

`andThen` can be explained as:

transforming an initial value of type `α`, using a program of type `program α β`, yielding an intermediate value of type
`β`, and then transforming that intermediate value, using a program of type `program β γ`, yielding a final value of
type `γ`.

`andThen_assoc` can be explained as:

first transforming an initial value of type `α`, using `αpβ >=> βpγ`, yielding an intermediate value of type `γ`, and
then transforming that intermediate value, using `γpδ`, yields the same final value as first transforming the initia
value of type `α`, using `αpβ`, to an intermediate value of type `β`, and then transforming that intermediate value,
using `βpγ >=> γpδ`.

Let's second deal with `Bind`.

I think of a evaluating an expression as yielding a value.

Likewise, I think of a executing a computation as yielding a value (potentially performing side effects along the way).

Let's ignore side effects for now  (you may wish to explain what happens with them).

`bind` can be explained as:

executing an inner computation of type `computation α` yielding an intermediate value of type `α`, and then binding that
intermediate value to an outer computation valued function of type `α → computation β`, yields an outer computation of
type `computation β` (that when executing it yields a value of type `β`)

or

executing an inner computation of type `computation α` yielding an intermediate value of type `α`, and then transforming
that intermediate value using an outer computation valued function of type `α → computation β`, yields an outer
computation value of type `computation β` (that when executing it yields a value of type `β`)

`bind_assoc` can be explained as:

executing an inner computation of `cα` yielding a first intermediate value, and then transforming that intermediate
value using an intermediate computation valued function `αfcβ`, yielding an intermediate computation value that, when
executing it, yields a second intermediate value, and then transforming that intermediate value using a final
computation valued function `βfcγ` yields the same final outer computation value as executing the inner computation of
`cα` yielding a first intermediate value, and then transforming that intermediate value using the computation valued
function that first transforms that first intermediate value using the intermediate computation valued function `αfcβ`,
yielding an intermediate computation value that, when executing it, yields a second intermediate value, and then
transforming that intermediate value using a final computation valued function `βfcγ`.

As far as components of a component system is concerned, I also like programs more than computations.

Let's have a closer look at the associativity law code fragments

```
(αpβ >=> βpγ) >=> γpδ =
  αpβ >=> (βpγ >=> γpδ)
```

and

```
cα >>= αfcβ >>= βfcγ =
  cα >>= λ α => αfcβ α >>= βfcγ
```

Programs are closed components, while computations are open components. Computation `cα` needs to be opened to access
the value `α` yielded by executing it, so that it can be transformed using computation valued function `αfcβ`.
Programming with computations is pointful programming.

Programs do not need to be opened. Programming with programs is pointfree programming. Think of using them as playing
with Lego ariifacts.

I.m.h.o, it is a more elegant and easier to program pointfree than to program pointful. Likewise, i.m.h.o., it is more
elegant and easier to reason in terms of pointfree laws than to reason in terms of pointful laws. Of course, this is a
matter of taste. Nevertheless hope to convince you that pointfree is more better than pointful.

It is possible, and sometimes more elegant, use programs positionally. Positional programming is similar to pointful
programming. It is is useful for writing sequential recipe-like programs, where, starting from an initial value
(often a composite-value), intermediate values (mostly basic-values) are created, and, together with the initial value,
as a compositional value are passed to the next step of the recipe-like program, until a final value is yielded. The
initial value and intermediate values are accessed positionally. The creation of the intermediate values can involve
general programs. A sequential recipe-like program glues programs together, similar to an operating system scripting
language gluing operating system executables together.

# The `PSBP` library type classes

The `PSBP` library type classes are binary type constructor classes, but we also simply call them type classes.

## `class Functional`

Functions can, somehow, be used as programs. Functions that are used as programs are effectfree. We can define functions
as programs in a formal way by defining a type class. This is what we do with the `Functional` type class.

```lean
class Functional
    (program : Type → Type → Type) where
  asProgram {α β : Type} :
    (α → β) → program α β

export Functional (asProgram)
```

## `class Functorial`

Functions can act upon programs. They act in an effectfree way. We can define functions acting upon programs in a formal
way by defining a type class. This is what we do with the `Functorial` type class.

```lean
class Functorial
    (program : Type → Type → Type) where
  andThenF {α β γ : Type} :
    program α β → (β → γ) → program α γ

export Functorial (andThenF)

infixl:50 " >-> " => andThenF
```

`andThenF` also has infix notation `>->`.

## `class Sequential`

Programs can be sequentially combined. Sequentially combining programs can be seen as a second program acting upon a
first program. The difference with `Functorial` is that the second program may be effectful. Their effects are
accumulated from left to right. Effects of the second program can depend on the final value of the first program. We can
define programs that are sequentially combined in a formal way by defining a type class. This is what we do with the
`Sequential` type class.

```lean
class Sequential
    (program : Type → Type → Type) where
  andThen {α β γ : Type} :
    program α β → program β γ → program α γ

export Sequential (andThen)

infixl:50 " >=> " => andThen
```

`andThen` also has infix notation `>=>`.

## `class Creational`

Programs can be combined to create product values. Their effects are accumulated from left to right. We can define
programs that produce product values in a formal way by defining a type class. This is what we do with the `Creational`
type class.

```lean
class Creational
    (program : Type → Type → Type) where
  product {α β γ : Type} :
    program α β → program α γ → program α (β × γ)

export Creational (product)

infixl:60 " &&& " => product
```

`product` also has infix notation `&&&`.

## `class Conditional`

Programs can be combined to consume sum values. Only the left one and its effects or right one and its effects is used.
We can define programs that consume sum values in a formal way by defining a type class. This is what we do with the
`Conditional` type class.

```lean
class Conditional
    (program : Type → Type → Type) where
  sum {α β γ : Type} :
    program γ α → program β α → program (γ ⊕ β) α

export Conditional (sum)

infixl:55 " ||| " => sum
```

`sum` also has infix notation `|||`.

## Writing programs

Programs are written in terms of members of type classes like the ones defined so far. The type class members are not
defined, they are declared. Type classes are specifications. The programs written in terms of their members are program
specifications, but, by abuse of language we call them programs.

It is challenging to limit the expressiveness of the type class combinations in terms of whose members programs are
written. More specification expressivenes implies less implementation flexibility.

# `fibonacci` and `factorial`

It turns out that we ready now for defining `fibonacci` and `factorial`.

The `identity` program is a simple, but extremely useful function as a program.

```lean
def identity
    [Functional program] :
  program α α :=
    asProgram id
```

## `def let_`

Using the `let_` combinator an intermediate value can be constructed that is available for later use.

```lean
def let_
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α β → (program (α × β) γ → program α γ) :=
    λ αpβ αaβpγ => identity &&& αpβ >=> αaβpγ

 def in_ : α → α := id
```

Think of `let_` as a library level keyword. `in_` is also a library level keyword that, depending on your taste, may
make programs more readable.

## `def if_`

Using the `if_` combinator conditional boolean logic can be expressed. A helper function and corresponding program is
needed to define it.

```lean
def if_
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program α Bool →
  program α β →
  program α β →
  program α β :=
    λ αpb t_apβ f_apβ =>
      let_ αpb $
        in_ $
          asProgram (
            λ αab => match αab with
              | ⟨α, true⟩ => .inl α
              | ⟨α, false⟩ => .inr α
          ) >=>
          t_apβ ||| f_apβ

def else_ : α → α := id
```
For readability and reusability reasons it is useful to first define some primitive functions

```lean
def isZeroF: Nat → Bool :=
  λ n => n == 0

def isOneF : Nat → Bool :=
  λ n => n == 1

def oneF : Nat → Nat :=
  λ _ => 1

def minusOneF : Nat → Nat :=
  λ n => n - 1

def minusTwoF : Nat → Nat :=
  λ n => n - 2

def addF : Nat × Nat → Nat :=
  λ ⟨n, m⟩ => n + m

def multiplyF : Nat × Nat → Nat :=
  λ ⟨n, m⟩ => n * m
```

and corresponding primitive programs

```lean
def isZero
    [Functional program] :
  program Nat Bool :=
    asProgram isZeroF

def isOne
    [Functional program] :
  program Nat Bool :=
    asProgram isOneF

def one
    [Functional program] :
  program Nat Nat :=
    asProgram oneF

def minusOne
    [Functional program] :
  program Nat Nat :=
    asProgram minusOneF

def minusTwo
    [Functional program] :
  program Nat Nat :=
    asProgram minusTwoF

def add
    [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram addF

def multiply
    [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram multiplyF
```

Program `fibonacci` is defined as follows

```lean
unsafe def fibonacci
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      else_ $
        if_ isOne one $
          else_ $
            (minusOne >=> fibonacci) &&&
            (minusTwo >=> fibonacci) >=>
            add
```

Program `factorial` is defined as follows

```lean
unsafe def factorial
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      else_ $
        let_ (minusOne >=> factorial) $
          in_ $
            multiply
```

The `unsafe` keyword is used because the definitions above do not type check without them. `fibonacci` and `factorial`
are program specifications, they need to be materialized before they can be used. It is instructive to compare this with
the painting ["Ceci n'est pas une pipe"](https://en.wikipedia.org/wiki/The_Treachery_of_Images) of René Magritte. Much
in the same way, descriptions of effects are, luckily enough, also not side effects. So it is safe to hang a picture of
a bomb explosion on your wall.The painting is, of course, not a pipe, it is a description of a pipe. A specification is
also a (special kind of) description.

## What about `Functorial`?

You may wonder why neither `fibonacci` nor `factorial` use `Functorial`. A `Functorial` instance can be defined in terms
of `Function` and `Sequential`.

```lean
instance
    [Functional program]
    [Sequential program] :
    Functorial program where
  andThenF {α β γ: Type} :
    program α β → (β → γ) → program α γ :=
      λ αpβ => λ βfγ => αpβ >=> asProgram βfγ
```

So why introducing `Functorial` in the first place? Well, the combination of `Functorial` with `Functional`, and
`Creational` is sufficiently expressive to write interesting programs that are more flexible as far as implementation
and corresponding materialization is concerned than the ones using `Sequential`.

Below are two programs, `twiceMinusOne01` and `twiceMinusOne02`.

```lean
def twiceMinusOne01
    [Functional program]
    [Functorial program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusOne >-> addF

def twiceMinusOne02
    [Functional program]
    [Sequential program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusOne >=> add

```

`twiceMinusOne02` uses the full power of `Sequential` while `twiceMinusOne01` uses the less expressive `Functorial`.
Using `Sequential` is, in this case, an unnecessary overkill because the addition that is used in both cases is
effectfree.

That being said, you may argue that what you have read so far is also an unnecessary overkill because, after all, I only
showed (effectfree) functions. But think of replacing `minusOne` and/ or `minusTwo` by an effectful program. Having more
implementation and corresponding materialization flexibility when dealing with effects can really be useful. A standard
example is more flexible error handling when processing a submitted web form and even error correction when parsing a
document.

# Laws

## `def identity`

## `class LawfulFunctional`

`Functional` comes with laws.

```lean
class LawfulFunctional (program : Type → Type → Type)
    [Functional program]
    [Sequential program] : Prop where
  functional_identity :
    (asProgram id : program α α) =
      identity
  functional_sequential
      (αfβ : α → β)
      (βfγ : β → γ) :
    (asProgram αfβ >=> asProgram βfγ : program α γ) =
      asProgram (βfγ ∘ αfβ)
```

The `functional_identity` law relates function identity and program identity, and is `True` per definition.

The `functional_sequential` law relates function sequential combination and program sequential combination.

## `class LawfulFunctorial`

`Functorial` comes with laws.

```lean
class LawfulFunctorial (program : Type → Type → Type)
    [Functorial program] : Prop where
  functorial_identity
    (αpβ : program α β) :
    (αpβ >-> id : program α β) =
      αpβ
  functorial_sequential
      (αpβ : program α β)
      (βfγ : β → γ)
      (γfδ : γ → δ) :
    ((αpβ >-> βfγ) >-> γfδ) =
      (αpβ >-> (γfδ ∘ βfγ))
```

The `functorial_identity` law states that the identity function action on a program leaves the program intact.

The `functorial_sequential` law relates function sequential combination and function action sequential combination.

## `class LawfulSequential`

`Functorial` comes with laws.

```lean
class LawfulSequential (program : Type → Type → Type)
    [Functional program]
    [Sequential program] : Prop where
  sequential_right_identity
      (αpβ : program α β) :
    (αpβ >=> identity : program α β) =
      αpβ
  sequential_left_identity
      (αpβ : program α β) :
    (identity >=> αpβ : program α β) =
      αpβ
  sequential_associativity
      (αpβ : program α β)
      (βpγ : program β γ)
      (γpδ : program γ δ) :
    ((αpβ >=> βpγ) >=> γpδ) =
      (αpβ >=> (βpγ >=> γpδ))
```

The `sequential_right_identity` law states that the sequential combination of a program with the identity program at
right leaves the program intact.

The `sequential_left_identity` law states that the sequential combination of a program with the identity program at
left leaves the program intact.

The `sequential_associativity` law states that the sequential combination of programs is associative.

## `class LawfulCreational`

`applyAtFirst`, `applyAtSecond`, `onlyFirst` and `assoc` are used in the `Creational` laws below.

```lean
def applyAtFirst
    [Functional program] :
  (α → β) → program (α × γ) (β × γ) :=
    λ αfβ => asProgram λ (α, γ) => (αfβ α, γ)
```

```lean
def applyAtSecond
    [Functional program] :
  (β → γ) → program (α × β) (α × γ) :=
    λ βfγ => asProgram λ (α, β) => (α, βfγ β)
```

and, let

```lean
def first
    [Functional program] :
  program (α × β) α :=
    asProgram λ (α, _) => α
```

and

```lean
def second
    [Functional program] :
  program (α × β) β :=
    asProgram λ (_, β) => β
```

in

```lean
def onlyFirst
    [Functional program]
    [Creational program]
    [Sequential program] :
  program α β → program (α × γ) (β × γ) :=
    λ αpβ => (first >=> αpβ) &&& second
```

and

```lean
def assoc
    [Functional program] :
  program ((α × β) × γ) (α × (β × γ)) :=
    asProgram (λ ((a, b), c) => (a, (b, c)))
```

`Creational` comes with laws.

```lean
class LawfulCreational (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_onlyFirst_asProgram
      (αfβ : α → β) :
    (onlyFirst (asProgram αfβ)
      : program (α × γ) (β × γ)) =
      applyAtFirst αfβ
  creational_onlyFirst_sequential
      (αpβ : program α β)
      (βpγ : program β γ) :
    (onlyFirst (αpβ >=> βpγ) :
      program (α × δ) (γ × δ)) =
      (onlyFirst αpβ >=> onlyFirst βpγ)
  creational_onlyFirst_first
      (αpβ : program α β) :
    (onlyFirst αpβ >=> (first : program (β × γ) β)
      : program (α × γ) β) =
      ((first : program (α × γ) α) >=> αpβ)
  creational_onlyFirst_applyAtSecond
      (αpβ : program α β)
      (γfδ : γ → δ) :
    (onlyFirst αpβ >=> applyAtSecond γfδ
      : program (α × γ) (β × δ)) =
      (applyAtSecond γfδ >=> onlyFirst αpβ)
  creational_onlyFirst_assoc
      (αpβ : program α β) :
    (onlyFirst (onlyFirst αpβ) >=> assoc
      : program ((α × γ) × δ) (β × (γ × δ))) =
      (assoc >=> onlyFirst αpβ)
```

## `class LawfulConditional`

`left` and `right` are used in the `Conditional` laws below.

```lean
def left
    [Functional program] :
  program γ (γ ⊕ β) :=
    asProgram .inl
```

and

```lean
def right
    [Functional program] :
  program β (γ ⊕ β) :=
    asProgram .inr
```

`Conditional` comes with laws.

```lean
class LawfulConditional (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Conditional program] : Prop where
  conditional_left
      (γpα : program γ α)
      (βpα : program β α) :
    (left >=> γpα ||| βpα) =
      γpα
  conditional_right
      (γpα : program γ α)
      (βpα : program β α) :
    (right >=> γpα ||| βpα) =
      βpα
```

# Computation Valued Functions

Using computation valued functions is a generic way to implement the program related type classes in terms of the
computation related type classes

```lean
structure FromComputationValuedFunction
    (computation : (Type → Type)) (α β : Type) where
  toComputationValuedFunction : α → computation β

instance [Applicative computation] :
    Functional
      (FromComputationValuedFunction computation) where
  asProgram :=
    λ αfβ => ⟨λ α => pure $ αfβ α⟩

instance [Functor computation] :
    Functorial
      (FromComputationValuedFunction computation) where
  andThenF :=
    λ ⟨αfcβ⟩ βfγ => ⟨λ α => βfγ <$> αfcβ α⟩

instance [Applicative computation] :
    Creational
      (FromComputationValuedFunction computation) where
  product := λ ⟨αfcβ⟩ ⟨αfcγ⟩ =>
    -- ⟨λ α => pure .mk <*> αfcβ α <*> αfcγ α⟩
    ⟨λ α => .mk <$> αfcβ α <*> αfcγ α⟩

instance [Monad computation] :
    Sequential
      (FromComputationValuedFunction computation) where
  andThen :=
    λ ⟨αfcβ⟩ ⟨βfcγ⟩ => ⟨λ α => αfcβ α >>= βfcγ⟩

def foldSum {γ β α : Type}
    (γfα : γ → α)
    (βfα : β → α)
    (sum : γ ⊕ β) : α :=
  match sum with
  | .inl tc => γfα tc
  | .inr tb => βfα tb

instance :
    Conditional
      (FromComputationValuedFunction computation) where
  sum := λ ⟨γfγα⟩ ⟨βfγα⟩ => ⟨foldSum γfγα βfγα⟩
```

A word of warning, The code above sometimes uses `⟨` and `⟩`, different from `(` and `)`, to asemble and disassemble
`structure`'s like `FromComputationValuedFunction`.

# Theorems

The laws of the various type classes need to be proved for the various instances. First we prove them and next we let
`Lean` prove them for us.

## `Functional` theorems

Theorem `functional_identity` below is proved by definition using `by calc` and
`rfl`.

```lean
@[simp] theorem functional_identity
  {α : Type}
    [Applicative computation] :
    (identity :
      FromComputationValuedFunction computation α α)
      = asProgram id := by
  calc
    identity
        = asProgram id
          := rfl
```
Theorem `functional_sequential'` also uses `congrArg` and `funext`.

Theorem `functional_sequential'` uses the `pure_bind` law of `LawfulMonad`.

```lean
theorem functional_sequential'
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β)
  (βfγ : β → γ) :
    (asProgram αfβ >=> asProgram βfγ :
      FromComputationValuedFunction computation α γ)
      = asProgram (βfγ ∘ αfβ) := by
  calc
    (asProgram αfβ >=> asProgram βfγ :
      FromComputationValuedFunction computation α γ)
        = (⟨λ α => pure $ αfβ α⟩ >=> ⟨λ β => pure $ βfγ β⟩)
          := rfl
    _   = ⟨λ α => (pure $ αfβ α) >>= λ β => pure $ βfγ β⟩
          := rfl
    _   = ⟨λ α => pure $ βfγ (αfβ α)⟩
          := congrArg
               FromComputationValuedFunction.mk
               (funext λ α =>
                 pure_bind (αfβ α) (λ β => pure $ βfγ β))
    _   = ⟨λ α => pure $ (βfγ ∘ αfβ) α⟩
          := rfl
    _   = asProgram (βfγ ∘ αfβ)
          := rfl
```

Theorem `functional_sequential` uses `by simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem functional_sequential
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β)
  (βfγ : β → γ) :
    (asProgram αfβ >=> asProgram βfγ :
      FromComputationValuedFunction computation α γ)
      = asProgram (βfγ ∘ αfβ) := by
  simp[asProgram, andThen]
```

Note that `functional_sequential` is not annotated by `@[simp]` so that `functional_sequential'` cannot use it. As a
consequence it is necessary to tell `Lean` to unfold everything in order for it to see the real definitions involved.

## `Functorial` theorems

Theorem `functorial_identity'` the `id_map` law of `LawfulFunctor`.

```lean
theorem functorial_identity'
  {α β : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (αpβ >-> id :
      FromComputationValuedFunction computation α β)
      = αpβ := by
  let αfcβ := αpβ.toComputationValuedFunction
  calc
    (αpβ >-> id)
        = ⟨λ α => id <$> αfcβ α⟩
          := rfl
    _   = ⟨λ α => αfcβ α ⟩
          := congrArg
               FromComputationValuedFunction.mk
               (funext λ α => id_map (αfcβ α))
    _   = ⟨αfcβ⟩
          := rfl
```

Theorem `functorial_identity` uses `simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem functorial_identity
  {α β : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (αpβ >-> id :
      FromComputationValuedFunction computation α β)
      = αpβ := by
    simp[andThenF]
```

Theorem `functorial_sequential'` uses the `comp_map` law of `LawfulFunctor`.

```lean
theorem functorial_sequential'
  {α β γ δ : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βfγ : β → γ)
  (γfδ : γ → δ) :
    ((αpβ >-> βfγ) >-> γfδ :
      FromComputationValuedFunction computation α δ)
      = (αpβ >-> (γfδ ∘ βfγ)) := by
  let αfcβ := αpβ.toComputationValuedFunction
  calc
    ((αpβ >-> βfγ) >-> γfδ)
        = (⟨λ α => βfγ <$> αfcβ α⟩ >-> γfδ)
          := rfl
    _   = ⟨λ α => γfδ <$> (λ α => βfγ <$> αfcβ α) α⟩
          := rfl
    _   = ⟨λ α => γfδ <$> βfγ <$> αfcβ α⟩
          := rfl
    _   = ⟨λ α => (γfδ ∘ βfγ) <$> αfcβ α⟩
          := congrArg
               FromComputationValuedFunction.mk
               (funext λ α =>
                 Eq.symm (comp_map βfγ γfδ (αfcβ α)))
    _   = (αpβ >-> (γfδ ∘ βfγ))
          := rfl
```

Theorem `functorial_sequential` uses `simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem functorial_sequential
    {α β γ δ : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βfγ : β → γ)
  (γfδ : γ → δ) :
    ((αpβ >-> βfγ) >-> γfδ :
      FromComputationValuedFunction computation α δ)
      = (αpβ >-> (γfδ ∘ βfγ)) := by simp[andThenF, comp_map]
```

## `Sequential` theorems

Theorem `sequential_right_identity'` uses the `bind_pure_comp` law of `LawfulMonad` and the `comp_map` law of
`LawfulFunctor`.

```lean
theorem sequential_right_identity'
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (αpβ >=> asProgram id :
      FromComputationValuedFunction computation α β)
    = αpβ := by
   let αfcβ := αpβ.toComputationValuedFunction
   calc
      (αpβ >=> asProgram id :
          FromComputationValuedFunction computation α β)
          = ⟨λ α => αfcβ α >>= λ β => pure (id β)⟩
            := rfl
      _   = ⟨λ α => id <$> αfcβ α⟩
            := congrArg
                 FromComputationValuedFunction.mk
                 (funext λ α => bind_pure_comp id (αfcβ α))
      _   = ⟨λ α => αfcβ α⟩
            := congrArg
                 FromComputationValuedFunction.mk
                 (funext λ α => id_map (αfcβ α))
      _   = αpβ
            := rfl
```

Theorem `sequential_right_identity` uses `simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem sequential_right_identity
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    ((αpβ >=> asProgram id) :
      FromComputationValuedFunction computation α β)
      = αpβ := by simp[andThen]
```

Theorem `sequential_left_identity'` uses the `pure_bind` law of `LawfulMonad`.

```lean
@[simp] theorem sequential_left_identity'
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (asProgram id >=> αpβ :
      FromComputationValuedFunction computation α β)
    = αpβ := by
    let αfcβ := αpβ.toComputationValuedFunction
    calc
      (asProgram id >=> ⟨αfcβ⟩ :
        FromComputationValuedFunction computation α β)
          = ⟨λ α => pure α >>= αfcβ⟩
            := rfl
      _   = ⟨λ α => αfcβ α⟩
            := congrArg
                 FromComputationValuedFunction.mk
                 (funext λ α => pure_bind α αfcβ)
      _   = ⟨αfcβ⟩
            := rfl
```

Theorem `sequential_left_identity` uses `simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem sequential_left_identity
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (asProgram id >=> αpβ :
      FromComputationValuedFunction computation α β)
    = αpβ := by simp[andThen]
```

Theorem `sequential_associative'` uses the `pure_assoc` law of `LawfulMonad`.

```lean
@[simp] theorem sequential_associative'
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βpγ : FromComputationValuedFunction computation β γ)
  (γpδ : FromComputationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ :
      FromComputationValuedFunction computation α δ) =
      (αpβ >=> (βpγ >=> γpδ)) := by
  let αfcβ : α → computation β :=
    αpβ.toComputationValuedFunction
  let βfcγ : β → computation γ :=
    βpγ.toComputationValuedFunction
  let γfcδ : γ → computation δ :=
    γpδ.toComputationValuedFunction
  let βfcδ : β → computation δ :=
    λ β => βfcγ β >>= γfcδ
  calc
    ((αpβ >=> βpγ) >=> γpδ :
      FromComputationValuedFunction computation α δ)
        = (⟨λ α => αfcβ α >>= βfcγ⟩ >=> ⟨γfcδ⟩)
          := rfl
    _   = ⟨λ α => αfcβ α >>= βfcγ >>= γfcδ⟩
          := rfl
    _   = ⟨λ α => αfcβ α >>= (λ β => βfcγ β >>= γfcδ)⟩
          := congrArg
               FromComputationValuedFunction.mk
               (funext λ α => bind_assoc (αfcβ α) βfcγ γfcδ)
    _   = (⟨λ α => αfcβ α >>= βfcδ⟩ :
            FromComputationValuedFunction computation α δ)
          := rfl
    _   = (αpβ >=> (βpγ >=> γpδ):
            FromComputationValuedFunction computation α δ)
          := rfl
```

Theorem `sequential_associative` uses `simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem sequential_associative
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βpγ : FromComputationValuedFunction computation β γ)
  (γpδ : FromComputationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ :
      FromComputationValuedFunction computation α δ) =
      (αpβ >=> (βpγ >=> γpδ)) := by simp[andThen]
```

# `Creational` theorems

Theorem `creational_onlyFirst_asProgram'` uses the `pure_bind` law of `LawfulMonad` and the `map_pure` law of
`LawfulApplicative`.

```lean
theorem creational_onlyFirst_asProgram'
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β) :
    (onlyFirst (asProgram αfβ) :
      FromComputationValuedFunction
        computation (α × γ) (β × γ)) =
      (asProgram (λ (α, γ) => (αfβ α, γ))) := by
  calc
    (onlyFirst (asProgram αfβ))
        = onlyFirst ⟨λ α => pure $ αfβ α⟩ :=
        rfl
    _   = ((first :
           FromComputationValuedFunction
             computation (α × γ) α) >=>
            (⟨λ α => pure $ αfβ α⟩)) &&& second :=
        rfl
    _   = (((asProgram λ (α, _) => α) :
            FromComputationValuedFunction
              computation (α × γ) α) >=>
              (⟨λ α => pure $ αfβ α⟩)) &&& second :=
        rfl
    _   = ((⟨λ (α, _) => pure α⟩ :
            FromComputationValuedFunction
              computation (α × γ) α) >=>
              (⟨λ α => pure $ αfβ α⟩)) &&& second :=
        rfl
    _   = (⟨λ (α, _) => pure α >>= (λ α => pure $ αfβ α)⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&& second :=
        rfl
    _   = (⟨(λ (α, _) => pure $ αfβ α)⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&& second :=
        congrArg
          (λ (αfcβ : α → computation β) =>
            ((⟨λ (α, _) => αfcβ α⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&& second))
          (funext (λ α =>
            pure_bind α (λ α => pure $ αfβ α)))
    _   = (⟨λ (α, _) => pure $ αfβ α⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&& second :=
        rfl
    _   = (⟨λ (α, _) => pure $ αfβ α⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&&
              (asProgram (λ (_, γ) => γ) :
                FromComputationValuedFunction
                  computation (α × γ) γ) :=
        rfl
    _   = (⟨λ (α, _) => pure $ αfβ α⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&&
              (⟨λ (_, γ) => pure $ γ⟩ :
                FromComputationValuedFunction
                  computation (α × γ) γ) :=
        rfl
    _   = (⟨λ (α, γ) =>
            (Prod.mk <$> (pure $ αfβ α)) <*> (pure $ γ)⟩ :
             FromComputationValuedFunction
               computation (α × γ) (β × γ)) :=
        rfl
    _   = (⟨λ (α, γ) =>
            (pure $ Prod.mk (αfβ α)) <*> (pure $ γ)⟩ :
             FromComputationValuedFunction
             computation (α × γ) (β × γ)) :=
        congrArg
         (FromComputationValuedFunction.mk ∘
           ((λ αfβaγ =>
             λ (α, γ) => αfβaγ α <*> (pure $ γ)) :
             (α → computation (γ → (β × γ))) →
               ((α × γ) → computation (β × γ))))
          (funext λ α => (map_pure (Prod.mk) (αfβ α)))
    _   = (⟨λ (α, γ) => Prod.mk (αfβ α) <$> (pure $ γ)⟩) :=
        congrArg
         FromComputationValuedFunction.mk
          (funext λ (α, γ) =>
            (pure_seq (Prod.mk (αfβ α)) (pure $ γ)))
    _   = (⟨λ (α, γ) => pure (Prod.mk (αfβ α) γ)⟩) :=
        congrArg
         FromComputationValuedFunction.mk
          (funext λ (α, γ) =>
            (map_pure (Prod.mk (αfβ α)) γ))
    _   = (⟨λ (α, γ) => pure $ (αfβ α, γ)⟩) :=
        rfl
    _   = (asProgram (λ (α, γ) => (αfβ α, γ))) :=
        rfl
```

Theorem `creational_onlyFirst_asProgram` uses `simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem creational_onlyFirst_asProgram
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β) :
    (onlyFirst (asProgram αfβ) :
      FromComputationValuedFunction
        computation (α × γ) (β × γ)) =
      (asProgram (λ (α, γ) => (αfβ α, γ))) := by simp [
        onlyFirst, asProgram, product, first, second
        ]
```

By now you probably agree that `calc` based proofs can become tedious.

In what follows, I will, mostly, only show the `simp` based proofs.

```lean
@[simp] theorem creational_onlyFirst_sequential
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βpγ : FromComputationValuedFunction computation β γ) :
    (onlyFirst (αpβ >=> βpγ) :
      FromComputationValuedFunction
        computation (α × δ) (γ × δ)) =
      (onlyFirst αpβ >=> onlyFirst βpγ :
        FromComputationValuedFunction
          computation (α × δ) (γ × δ)) := by simp[
            onlyFirst, andThen, asProgram, product, first,
            second
            ]
```

```lean
@[simp] theorem creational_onlyFirst_first
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (onlyFirst αpβ >=> first :
      FromComputationValuedFunction computation (α × γ) β) =
      (first >=> αpβ) := by simp[
        onlyFirst, andThen, asProgram, product, first,
        second
        ]
```

```lean
@[simp] theorem creational_onlyFirst_applyAtSecond
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (γfδ : γ → δ) :
    (onlyFirst αpβ >=> applyAtSecond γfδ) =
      (applyAtSecond γfδ >=> onlyFirst αpβ) := by simp[
        onlyFirst, andThen, applyAtSecond, asProgram,
        product, first, second
        ]
```

```lean
@[simp] theorem creational_onlyFirst_assoc
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (onlyFirst (onlyFirst αpβ) >=> assoc :
      FromComputationValuedFunction
        computation ((α × γ) × δ) (β × (γ × δ))) =
      (assoc >=> onlyFirst αpβ) := by simp[
        onlyFirst, andThen, asProgram, product, first,
        second, assoc
        ]
```

## `ActiveProgram`

There is not a lot of work to be done for active implementations. The `Functor`, `Applicative` and `Monad`
implementations of `Id` can be used.

```savedLean
abbrev Active := Id

abbrev ActiveProgram α β :=
  FromComputationValuedFunction Active α β

def materializeActive :
    ActiveProgram α β → (α → β) :=
  λ ⟨αfaβ⟩ α => (αfaβ α).run
```

We can now run our programs in an active way.

```savedLean (name := activeFibonaccci)
#eval
  materializeActive
  fibonacci
  10
```
```leanOutput activeFibonaccci
89
```

```savedLean (name := activeFactorial)
#eval
  materializeActive
  factorial
  10
```
```leanOutput activeFactorial
3628800
```

```savedLean (name := activeTwiceMinusOne01)
#eval
  materializeActive
  twiceMinusOne01
  10
```
```leanOutput activeTwiceMinusOne01
18
```

```savedLean (name := activeTwiceMinusOne02)
#eval
  materializeActive
  twiceMinusOne02
  10
```
```leanOutput activeTwiceMinusOne02
18
```

# `ReactiveProgram`

There is much more work to be done for reactive implementations. They are callback handler, a.k.a. continuation based.

```savedLean
structure ReactiveT
    (ρ : Type)
    (computation: Type → Type)
    (α : Type) where
  runReactiveT : (α → computation ρ) → computation ρ

abbrev Reactive ρ := ReactiveT ρ Active

instance {ρ: Type} :
    Functor (ReactiveT ρ computation) where
  map :=
    λ αfβ ⟨rcα⟩ =>
      ⟨λ γ => rcα (γ ∘ αfβ)⟩

instance {ρ: Type} :
    Applicative (ReactiveT ρ computation) where
  pure := λ α => ReactiveT.mk (λ αfcρ => αfcρ α)
  seq :=
    λ ⟨rcαfβ⟩ ufrtρcα =>
      ⟨λ βfcρ =>
        rcαfβ $
          (λ αfβ =>
            (ufrtρcα ()).runReactiveT (βfcρ ∘ αfβ))⟩

instance {ρ: Type} :
    Monad (ReactiveT ρ computation) where
  bind :=
    λ ⟨rcα⟩ αfrtρcβ =>
      ⟨λ βfcρ =>
        rcα (λ α =>
        (αfrtρcβ α).runReactiveT βfcρ)⟩

abbrev ReactiveProgram ρ computation :=
  FromComputationValuedFunction (ReactiveT ρ computation)

def materializeReactive {α β : Type} :
    ReactiveProgram β Active α β → α → β :=
  λ ⟨αfrtaβcβ⟩ α =>
      (αfrtaβcβ α).runReactiveT id
```

The `ρ` stands for the "result" of callback handling.

We can now run our programs in a reactive way.

```savedLean (name := reactiveFibonaccci)
#eval
  materializeReactive
  fibonacci
  10
```

```leanOutput reactiveFibonaccci
89
```

```savedLean (name := reactiveFactorial)
#eval
  materializeReactive
  factorial
  10
```

```leanOutput reactiveFactorial
3628800
```

```savedLean (name := reactiveTwiceMinusOne01)
#eval
  materializeReactive
  twiceMinusOne01
  10
```

```leanOutput reactiveTwiceMinusOne01
18
```

```savedLean (name := reactiveTwiceMinusOne02)
#eval
  materializeReactive
  twiceMinusOne02
  10
```

```leanOutput reactiveTwiceMinusOne02
18
```

We did not change the definition of our programs, we only materialized them in another way!

# Positional Programming

## Using `Functorial`

Pointfree programming, like is done for `fibonacci` and `factorial` may be a elegant, but `PSBP` also enables, and,
for reasons of elegance, sometimes needs, positional programming. Let's first start with `Functorial` based positional
programming. Suppose we want to run the function that transforms an initial (argument) value `n` of type `Nat` to the
final (result) value `(((n-1, n-2), 2), 3) => (n-2) + 2 * (n-1) + 3`. `someProgram01` below could be a solution.
`someProgram01` makes use of `Functorial`.

Let

```savedLean (name := someProgram01)
def twoF : Nat → Nat := λ _ => 2

def threeF : Nat → Nat := λ _ => 3
```

and

```savedLean
def two [Functional program] :
  program Nat Nat :=
    asProgram twoF

def three [Functional program] :
  program Nat Nat :=
    asProgram threeF
```
in

```savedLean
def someProgram01
    [Functional program]
    [Functorial program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusTwo &&& two &&& three >->
      λ (((n1, n2), n3), n4) =>
        n2 + n3 * n1 + n4
```

```savedLean (name := activeSomeProgram01)
#eval
  materializeActive
  someProgram01
  10
```

```leanOutput activeSomeProgram01
29
```

```savedLean (name := reactiveSomeProgram01)
#eval
  materializeReactive
  someProgram01
  10
```
```leanOutput activeSomeProgram01
29
```

You may argue that, as for as the acting function involved is concerned, we are back to pointful programming. Well,
somehow you are right, but notice that all `n`'s involved have indices (`1`, `2`, `3` and `4`). They can be thought of
as positions. So we are essentially accessing values at positions of multi-values. For this example the multi-value
involved is homogeneous but it might as well be a heterogeneous one. More about this later.

## Using `Sequential`

`someProgram02` below could also be a solution. `someProgram02` makes use of `Sequential`.

```savedLean
def someProgram02
    [Functional program]
    [Sequential program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusTwo &&& two &&& three >=>
      asProgram (λ (((n1, n2), n3), n4) =>
        n2 + n3 * n1 + n4)
```

```savedLean (name := activeSomeProgram02)
#eval
  materializeActive
  someProgram02
  10
```
```leanOutput activeSomeProgram02
29
```

As already mentioned before, using `Sequential` is, in this case, an overkill.

## `class Positional`

```savedLean
class Positional
    (program : Type → Type → Type) where
  at_ {σ α β γ : Type} :
    program α β →
    program σ α →
    program (σ × β) γ →
    program σ γ

export Positional (at_)

infixl:45 " @ " => at_
```

`at_` also has infix notation `@`.


The `σ` stands for (runtime) "stack", and `σ × β` stands for `β` pushed onto `σ`. More about this later.

## `instance Positional`

The `at_` library level keyword of `Positional` can be defined in terms of `Functional`, `Creational` and `Sequential`.

```savedLean
instance
    [Functional program]
    [Creational program]
    [Sequential program] :
    Positional program where
  at_ {σ α β γ : Type} :
    program α β →
    program σ α →
    program (σ × β) γ →
    program σ γ :=
      λ αpβ σpα =>
        let_ (σpα >=> αpβ)
```

Think of `σpα` as accessing a (multi-)value, `α`, on the runtime stack, `σ`. Think of `αpβ` as transforming that
(multi-)value to `β`. `let_` then pushes `β` on `σ` obtaining `σ × β`. So, if it possible to transform the runtime stack
`σ` to `α`, to transform `α` to an intermediate value `β` and to transform `σ × β` to `γ`, then it is possible to
transform `σ` to `γ`.

## Some positions

Let's define some positions on a runtime stack `σ`.

```savedLean
def positionOne
    [Functional program] :
  program (σ × α) α :=
    asProgram  λ (_, α) => α

def positionTwo
    [Functional program] :
  program ((σ × β) × α) β :=
    asProgram λ ((_, β), _) => β

def positionOneAndTwo
    [Functional program] :
  program ((σ × β) × α) (α × β) :=
    asProgram λ ((_, β), α) => (α, β)

-- ...
```

`positionOne` and `positionTwo` are basic-value positions. `positionOneAndTwo` is a multi-value position.

## `positionalFactorialOfFibonacci` and `positionalSumOfFibonacciAndFactorial`

Below are two positional programs. The first one, `positionalFactorialOfFibonacci` uses only uses `positionOne`. The
second one `positionalSumOfFibonacciAndFactorial` uses three positions. Positions go up starting from their use site.

```savedLean
unsafe def positionalFactorialOfFibonacci
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program (σ × Nat) Nat :=
    fibonacci @ positionOne $
      factorial @ positionOne $
        positionOne
```

```savedLean
unsafe def positionalSumOfFibonacciAndFactorial
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program (σ × Nat) Nat :=
    fibonacci @ positionOne $
      factorial @ positionTwo $
        add @ positionOneAndTwo $
          positionOne
```

`Positional` is not part of the list of required type class instances because it can be inferred.

```savedLean (name := activePositionalFactorialOfFibonacci)
#eval
  materializeActive
  positionalFactorialOfFibonacci
  ((), 5)
```

```leanOutput activePositionalFactorialOfFibonacci
40320
```

```savedLean (name := activePositionalSumOfFibonacciAndFactorial)
#eval
  materializeActive
  positionalSumOfFibonacciAndFactorial
  ((), 10)
```

```leanOutput activePositionalSumOfFibonacciAndFactorial
3628889
```

## `positionalFactorialOfFibonacci'` and `positionalSumOfFibonacciAndFactorial'`

It is instructive to show the runtime stack using `identity`. This is done in `positionalFactorialOfFibonacci'` and
`positionalSumOfFibonacciAndFactorial'` below.

```savedLean
unsafe def positionalFactorialOfFibonacci'
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program (σ × Nat) (((σ × Nat) × Nat) × Nat) :=
    fibonacci @ positionOne $
      factorial @ positionOne $
        identity
```

```savedLean
unsafe def positionalSumOfFibonacciAndFactorial'
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program (σ × Nat) ((((σ × Nat) × Nat) × Nat) × Nat) :=
    fibonacci @ positionOne $
      factorial @ positionTwo $
        add @ positionOneAndTwo $
          identity
```

```savedLean (name := activePositionalFactorialOfFibonacci')
#eval
  materializeActive
  positionalFactorialOfFibonacci'
  ((), 5)
```

```leanOutput activePositionalFactorialOfFibonacci'
((((), 5), 8), 40320)
```

```savedLean (name := activePositionalSumOfFibonacciAndFactorial')
#eval
  materializeActive
  positionalSumOfFibonacciAndFactorial'
  ((), 10)
```

```leanOutput activePositionalSumOfFibonacciAndFactorial'
(((((), 10), 89), 3628800), 3628889)
```

# Programming With State

## `class WithState σ`

`PSBP` enables programming with state using the `WithState` class below.

```savedLean
class WithState
    (σ : outParam Type)
    (program : Type → Type → Type) where
  readState {α : Type} : program α σ
  writeState : program σ Unit

export WithState (readState writeState)
```

The `σ` above stands for "state".

### `def modifyStateWith`

Below is `modifyStateWith`, a useful programming with state capability.

-- Let

-- ```savedLean
-- def first
--     [Functional program] :
--   program (α × β) α :=
--     asProgram λ (α, _) => α

-- def second
--     [Functional program] :
--   program (α × β) β :=
--     asProgram  λ (_, β) => β
-- ```

-- in (only `first` above is used)

```savedLean
def modifyStateWith
    [Functional program]
    [Sequential program]
    [Creational program]
    [WithState σ program] :
  (σ → σ) → program α α :=
    λ σfσ =>
      let_ ((readState >=> asProgram σfσ) >=> writeState) $
        in_ $
          first
```

`modifyStateWith` modifies the state using a function.

## `withInitialStateAsInitialValue`

```savedLean
def withInitialStateAsInitialValue
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithState σ program] :
  program σ τ → program α τ :=
    λ σpτ =>
      readState >=> σpτ
```

Given a program `σpτ`, `withInitialStateAsInitialValue` transforms it to use the initial state as initial (argument)
value.

## `instance WithState σ`

`WithState σ` is implemented in terms of `MonadStateOf`.

```savedLean
instance [MonadStateOf σ computation] :
    WithState σ
      (FromComputationValuedFunction computation) where
  readState := ⟨λ _ => get⟩
  writeState := ⟨set⟩
```

## `ProgramWithState`

```savedLean
abbrev ProgramWithState σ computation :=
  FromComputationValuedFunction (StateT σ computation)

def materializeWithState
    [Monad computation] {α β : Type} :
  ProgramWithState σ computation α β →
  α →
  σ →
  computation β :=
    λ ⟨αfstσcβ⟩ =>
      λ α =>
        λ σ =>
          StateT.run (αfstσcβ α) σ >>=
            λ (β, _) => pure β

def materializeActiveWithState {α β : Type} :
  ProgramWithState σ Active α β → α → σ → β :=
    materializeWithState
```

## `fibonacciIncrementingArgumentPair`

Program `fibonacciIncrementingArgumentPair` below shows the effectfulness of programs with state by using the
initial state as initial (argument) value and modifying it.

Let

```savedLean
unsafe def fibonacciIncrementingArgument
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithState Nat program] :
  program Unit Nat :=
    withInitialStateAsInitialValue fibonacci >=>
    modifyStateWith (. + 1)
```

in

```savedLean
unsafe def fibonacciIncrementingArgumentPair
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithState Nat program] :
  program Unit (Nat × Nat) :=
    fibonacciIncrementingArgument &&&
    fibonacciIncrementingArgument
```

```savedLean (name := activeFibonacciIncrementingArgumentPair)
#eval
  materializeActiveWithState
    fibonacciIncrementingArgumentPair
    ()
    10
```

# Programming With Failure


## `WithFailure ε`

`PSBP` enables programming with failure using the `WithFailure` class below.

```savedLean
class WithFailure
    (ε : outParam Type)
    (program : Type → Type →Type) where
  failureWith {α β : Type} : (α → ε) → program α β

export WithFailure (failureWith)
```

## `instance WithFailure ε` (fail fast)

`WithFailure ε` is implemented in terms of `FailureT`, which is defined in terms of `⊕`. Given an initial (argument)
value, a program with failure may transform it to a final failure (result) value (at left) or a final succedd (result)
value (at right).

```savedLean
structure FailureT
    (ε : Type)
    (computation : Type → Type)
    (β : Type) : Type where
  toComputationOfSum : computation (ε ⊕ β)

instance [Monad computation] :
    Monad (FailureT ε computation) where
  map :=
  λ αfβ ⟨cεoα⟩  =>
    ⟨cεoα >>= λ εoα => match εoα with
      | (.inr α) => pure $ .inr (αfβ α)
      | (.inl ε) => pure $ .inl ε⟩
  pure :=
    λ α =>
      .mk (pure (Sum.inr α))
  bind :=
    λ ⟨cεoα⟩ αfftεcβ =>
      ⟨cεoα >>= λ εoα => match εoα with
        | .inr α  => (αfftεcβ α).toComputationOfSum
        | .inl ε  => pure (.inl ε)⟩

instance {ε : Type}
    [Applicative computation] :
  WithFailure ε
    (FromComputationValuedFunction
      (FailureT ε computation)) where
  failureWith :=
    λ αfε =>
      ⟨λ α =>
        ⟨pure $ Sum.inl $ αfε α⟩⟩
```

## `ProgramWithFailure`

```savedLean
abbrev ProgramWithFailure ε computation :=
  FromComputationValuedFunction (FailureT ε computation)

def materializeWithFailure
    [Monad computation] {α β : Type} :
  ProgramWithFailure ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ ⟨αftεcβ⟩ α =>
      (αftεcβ α).toComputationOfSum

def materializeActiveWithFailure {α β : Type} :
 ProgramWithFailure ε Active α β → α → (ε ⊕ β) :=
  materializeWithFailure
```

`instance Monad (FailureT ε computation)` and `materializeActiveWithFailure` above cause programs to a fail fast when a
first exception has been encountered. The examples below illustrate this.

## `safeDiv`

Let

```savedLean
def isNotZeroF: Nat → Bool :=
  λ n => n != 0

def unsafeDivF : Nat × Nat → Nat :=
  λ ⟨n, m⟩ => n / m
```

and

```savedLean
def isNotZero [Functional program] :
  program Nat Bool :=
    asProgram isNotZeroF

def unsafeDiv [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram unsafeDivF
```

in

```savedLean
def safeDiv
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure String program] :
  program (Nat × Nat) Nat :=
    if_ (second >=> isNotZero) unsafeDiv $
      else_ $
        failureWith (λ (n, m) =>
          s!"tried to divide {n} by {m}")
```

```savedLean (name := activeSafeDiv)
#eval
  materializeActiveWithFailure
    safeDiv
    (10, 5)
```

```leanOutput activeSafeDiv
Sum.inr 2
```

```savedLean (name := activeFailingSafeDiv)
#eval
  materializeActiveWithFailure
    safeDiv
    (10, 0)
```

```leanOutput activeFailingSafeDiv
Sum.inl "tried to divide 10 by 0"
```

## `safeDivIsOne`

```savedLean
def safeDivIsOne
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure String program] :
  program (Nat × Nat) Bool :=
    safeDiv >=> isOne
```

```savedLean (name := activeSafeDivIsOne)
#eval
  materializeActiveWithFailure
    safeDivIsOne
    (10, 10)
```

```leanOutput activeSafeDivIsOne
Sum.inr true
```

```savedLean (name := activeFailingSafeDivIsOne)
#eval
  materializeActiveWithFailure
    safeDivIsOne
    (10, 0)
```

```leanOutput activeFailingSafeDivIsOne
Sum.inl "tried to divide 10 by 0"
```

## `twiceSafeDiv`

```savedLean
def twiceSafeDiv
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure String program] :
  program ((Nat × Nat) × Nat) Nat :=
    (first >=> safeDiv) &&& second >=> safeDiv
```

```savedLean (name := activeTwiceSafeDiv)
#eval
  materializeActiveWithFailure
    twiceSafeDiv
    ((10, 5), 2)
```

```leanOutput activeTwiceSafeDiv
Sum.inr 1
```

```savedLean (name := activeTwiceSafeDiv01)
#eval
  materializeActiveWithFailure
    twiceSafeDiv
    ((10, 2), 0)
```

```leanOutput activeTwiceSafeDiv01
Sum.inl "tried to divide 5 by 0"
```

```savedLean (name := activeTwiceSafeDiv02)
#eval
  materializeActiveWithFailure
    twiceSafeDiv
    ((10, 0), 2)
```

```leanOutput activeTwiceSafeDiv02
Sum.inl "tried to divide 10 by 0"
```

```savedLean (name := activeTwiceSafeDiv03)
#eval
  materializeActiveWithFailure
    twiceSafeDiv
    ((10, 0), 0)
```

```leanOutput activeTwiceSafeDiv03
Sum.inl "tried to divide 10 by 0"
```

## `instance WithFailure ε` (validation)

What about accumulating exceptions instead of failing fast?

Accumulation is specified using a `Monoid` type class. For now, the neutral element `ν` is used, this means that only a
semigroup is required instead of a monoid.

```savedLean
class Monoid (μ : Type) where
  ν : μ
  combine : μ → μ → μ

export Monoid (ν combine)

infixl:60 " * " => combine
```

Accumulation can, for example, be implemented using the `List α` type.

```savedLean
instance : Monoid (List α) where
  ν := []
  combine := .append
```

```savedLean
instance
    [Functor computation] :
  Functor (FailureT ε computation) where
    map :=
     λ αfβ ⟨cεoα⟩ =>
       ⟨(λ εoα =>
           match εoα with
            | .inl ε => .inl ε
            | .inr α => .inr (αfβ α)) <$> cεoα
       ⟩

instance
    [Applicative computation]
    [Monoid ε] :
  Applicative (FailureT ε computation) where
    pure :=
      λ α =>
        ⟨pure $ .inr α⟩
    seq :=
      λ ⟨cεoαfβ⟩ ufftεcα =>
        let cεoα :=
          (ufftεcα ()).toComputationOfSum
        let εoαfεoαfβfεoβ {α β : Type} :
          (ε ⊕ α) → (ε ⊕ (α → β)) → (ε ⊕ β) :=
            λ εoα εoαfβ =>
              match εoα with
                | .inl ε =>
                  match εoαfβ with
                    | .inr _  => .inl ε
                    | .inl ε' => .inl (ε' * ε)
                | .inr α =>
                  match εoαfβ with
                    | .inr αfβ  => .inr (αfβ α)
                    | .inl ε' => .inl ε'
        ⟨εoαfεoαfβfεoβ <$> cεoα <*> cεoαfβ⟩
```

## `ProgramWithValidation`

```savedLean
abbrev ProgramWithValidation ε computation :=
  FromComputationValuedFunction (FailureT ε computation)

def materializeWithValidation
    [Monad computation]
    [Monoid ε] {α β : Type} :
  ProgramWithValidation ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ ⟨αftεcβ⟩ α =>
      (αftεcβ α).toComputationOfSum

def materializeActiveWithValidation
    [Monoid ε] {α β : Type} :
 ProgramWithValidation ε Active α β → α → (ε ⊕ β) :=
  materializeWithValidation
```

`instance Functor (FailureT ε computation)`, `instance Applicative (FailureT ε computation)` and
`materializeActiveWithValidation` above cause programs to accumulate all exceptions that are encountered.

The examples below illustrate this accumulation behavior.

## `accumulatingSafeDiv` revisited

```savedLean
def accumulatingSafeDiv
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program (Nat × Nat) Nat :=
    if_ (second >=> isNotZero) unsafeDiv $
      else_ $
        failureWith (λ (n, m) =>
          [s!"tried to divide {n} by {m}"])
```

```savedLean (name := activeAccumulatingSafeDiv)
#eval
materializeActiveWithValidation
  accumulatingSafeDiv
  (10, 5)
```

```leanOutput activeAccumulatingSafeDiv
Sum.inr 2
```

```savedLean (name := activeAccumulatingFailingSafeDiv)
#eval
materializeActiveWithValidation
  accumulatingSafeDiv
  (10, 0)
```

```leanOutput activeAccumulatingFailingSafeDiv
Sum.inl ["tried to divide 10 by 0"]
```

## `accumulatingSafeDivIsOne`

```savedLean
def accumulatingSafeDivIsOne
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program (Nat × Nat) Bool :=
    accumulatingSafeDiv >=> isOne
```

```savedLean (name := activeAccumulatingSafeDivIsOne)
#eval
  materializeActiveWithValidation
    accumulatingSafeDivIsOne
    (10, 10)
```

```leanOutput activeAccumulatingSafeDivIsOne
Sum.inr true
```

```savedLean (name := activeAccumulatingFailingSafeDivIsOne)
#eval
  materializeActiveWithValidation
    accumulatingSafeDivIsOne
    (10, 0)
```

```leanOutput activeFailingSafeDivIsOne
Sum.inl "tried to divide 10 by 0"
```

## `twiceAccumulatingSafeDivIsOne`

```savedLean
def twiceAccumulatingSafeDivIsOne
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program ((Nat × Nat) × Nat) Nat :=
    ((first >=> accumulatingSafeDiv) &&& second) >=>
    accumulatingSafeDiv
```

```savedLean (name := activeAccumulatingTwiceSafeDiv)
#eval
  materializeActiveWithValidation
    twiceAccumulatingSafeDivIsOne
    ((10, 5), 2)
```

```leanOutput activeAccumulatingTwiceSafeDiv
Sum.inr 1
```

```savedLean (name := activeAccumulatingTwiceSafeDiv01)
#eval
  materializeActiveWithValidation
    twiceAccumulatingSafeDivIsOne
    ((10, 2), 0)
```

```leanOutput activeTwiceSafeDiv01
Sum.inl "tried to divide 5 by 0"
```

```savedLean (name := activeAccumulatingTwiceSafeDiv02)
#eval
  materializeActiveWithValidation
    twiceAccumulatingSafeDivIsOne
    ((10, 0), 2)
```

```leanOutput activeAccumulatingTwiceSafeDiv02
Sum.inl ["tried to divide 10 by 0"]
```

```savedLean (name := activeAccumulatingTwiceSafeDiv03)
#eval
  materializeActiveWithValidation
    twiceAccumulatingSafeDivIsOne
    ((10, 0), 0)
```

```leanOutput activeAccumulatingTwiceSafeDiv03
Sum.inl ["tried to divide 10 by 0"]
```

## `accumulatingSafeDivProduct`

```savedLean
def accumulatingSafeDivProduct
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program ((Nat × Nat) × (Nat × Nat)) (Nat × Nat) :=
    (first >=> accumulatingSafeDiv) &&& (second >=>
    accumulatingSafeDiv)
```

```savedLean (name := activeAccumulatingSafeDivProduct01)
#eval
  materializeActiveWithValidation
    accumulatingSafeDivProduct
    ((10, 5), (8, 2))
```

```leanOutput activeAccumulatingSafeDivProduct01
Sum.inr (2, 4)
```

```savedLean (name := activeAccumulatingSafeDivProduct02)
#eval
  materializeActiveWithValidation
    accumulatingSafeDivProduct
    ((10, 0), (8, 2))
```

```leanOutput activeAccumulatingSafeDivProduct02
Sum.inl ["tried to divide 10 by 0"]
```

```savedLean (name := activeAccumulatingSafeDivProduct03)
#eval
  materializeActiveWithValidation
    accumulatingSafeDivProduct
    ((10, 5), (8, 0))
```

```leanOutput activeAccumulatingSafeDivProduct03
Sum.inl ["tried to divide 8 by 0"]
```

```savedLean (name := activeAccumulatingSafeDivProduct04)
#eval
  materializeActiveWithValidation
    accumulatingSafeDivProduct
    ((10, 0), (8, 0))
```

```leanOutput activeAccumulatingSafeDivProduct04
Sum.inl ["tried to divide 10 by 0", "tried to divide 8 by 0"]
```

## `addAccumulatingSafeDivProduct`

```savedLean
def addAccumulatingSafeDivProduct
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program ((Nat × Nat) × (Nat × Nat)) Nat :=
    (first >=> accumulatingSafeDiv) &&&
    (second >=> accumulatingSafeDiv) >=>
    add
```

```savedLean (name := activeAddAccumulatingSafeDivProduct01)
#eval
  materializeActiveWithValidation
    addAccumulatingSafeDivProduct
    ((10, 5), (8, 2))
```

```leanOutput activeAddAccumulatingSafeDivProduct01
Sum.inr 6
```

```savedLean (name := activeAddAccumulatingSafeDivProduct02)
#eval
  materializeActiveWithValidation
    addAccumulatingSafeDivProduct
    ((10, 0), (8, 2))
```

```leanOutput activeAccumulatingSafeDivProduct02
Sum.inl ["tried to divide 10 by 0"]
```

```savedLean (name := activeAddAccumulatingSafeDivProduct03)
#eval
  materializeActiveWithValidation
    addAccumulatingSafeDivProduct
    ((10, 5), (8, 0))
```

```leanOutput activeAccumulatingSafeDivProduct03
Sum.inl ["tried to divide 8 by 0"]
```

```savedLean (name := activeAddAccumulatingSafeDivProduct04)
#eval
  materializeActiveWithValidation
    addAccumulatingSafeDivProduct
    ((10, 0), (8, 0))
```

```leanOutput activeAccumulatingSafeDivProduct04
Sum.inl ["tried to divide 10 by 0", "tried to divide 8 by 0"]
```
