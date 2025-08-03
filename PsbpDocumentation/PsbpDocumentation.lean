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

# Warning

This is work in progress.

The book and it's code is far from complete and may change.

For now I do not use the universe capabilities of `Lean`.

I (try to) format code is a consistent way.

# Introduction

Lean defines type classes `Functor`, `Applicative` and `Monad`. They specify computation capabilities. Think of
computations as effectful expressions. They are operational artifacts. They do not really have a meaning in the
mathematical sense. Functions, on the other hand, are denotational artifacts. They do have a meaning in the mathematical
sense.

This document refers to effectful function as programs. So how to specify programs? That is what the `PSBP` library is
all about. `PSPB` stands for "Program Specification Based Programming". `PSBP` is a library that allows you to specify
programs, and then materialize them according to specific instances of the type classes in terms of which they are
specified. `PSBP` can be seen as a library level programming language. In what follows we will, by abuse of language,
often write "program" instead of "program specification" or "program (specification) materialization". Hopefully this
will not lead to confusion.

Just like expressions are evaluated to yield a result value, computations are executed to a result value, but, they may
executing them may also perform side effects along the way. Just like functions, programs, by running them,transform
argument values to result values, but, they may perform side effects along the way.

Why programming in terms of program specifications instead of in terms of computations? Both programs and computations
form a component system. The difference is that programs are closed components, while computations are open components.
Programming with programs is pointfree programming while programming with computations is pointful programming.

In my opinion, it is a more elegant to program pointfree than to program pointful. Likewise, in my opinion, it is more
elegant to reason in terms of pointffree laws than to reason in terms of with pointful laws. Of course, this is all a
matter of taste. I hope to convince you that pointfree is more elegant than pointful.

By the way, it is alse possible, and sometimes necessary, to program positional using `PSBP`. Positional programming is
similar to pointful programming. It is is useful for writing recipe-like programs, where, starting from an initial
value, intermediate values are constructed, and. together with the initial value, are passed to the next step, until a
final value is produced. The initial value and intermediate values are accessed positionally. More about this later.

# The `PSBP` Library type classes

## `class Functional`

Functions can, somehow, be used as programs. Well, functions are what pure functional programming is all about.
Functions that are used as programs are effectless. We can define functions as programs in a formal way by defining a
type class. This is what we do with the `Functional` type class.

```savedLean
class Functional (program : Type → Type → Type) where
  asProgram {α β : Type} :
    (α → β) → program α β

export Functional (asProgram)
```

A program, just like a function, transforms an initial value, its argument, to a final one, its result. By the way, a
value can repesent many values as a (nested) product. Such a value is called a multi-value. More about this later.

## `class Functorial`

Functions can act upon programs. They do that in an effectfree way. We can define functions acting upon programs in a
formal way by defining a type class. This is what we do with the `Functorial` type class.

```savedLean
class Functorial (program : Type → Type → Type) where
  andThenF {α β γ : Type} :
    program α β → (β → γ) → program α γ

export Functorial (andThenF)

infixl:50 " >-> " => andThenF
```

`andThenF` also has infix notation `>->`.

We have, and will continue to, use suffux `F` to distinguish functions from corresponding programs.

## `class Creational`

Programs can produce product values. Their effects are accumulated from left to right. We can define programs that
produce product values in a formal way by defining a type class. This is what we do with the `Creational` type class.

```savedLean
class Creational (program : Type → Type → Type) where
  product {α β γ : Type} :
    program α β → program α γ → program α (β × γ)

export Creational (product)

infixl:60 " &&& " => product
```

`product` also has infix notation `&&&`.

Nested product values are called multi-values.

## `class Sequential`

Programs can be sequentially composed. Their effects are accumulated from left to right. Sequentially composing programs
can be seen as a second program acting upon a first program. The difference with `Functorial` is that the second program
may be effectful. Effects of the second program can depend on the final value of the first program. We can define
programs that are sequentially composed in a formal way by defining a type class. This is what we do with the
`Sequential` type class.

```savedLean
class Sequential (program : Type → Type → Type) where
  andThen {α β γ : Type} :
    program α β → program β γ → program α γ

export Sequential (andThen)

infixl:50 " >=> " => andThen
```

`andThen` also has infix notation `>=>`.

## `class Conditional`

Programs can consume sum values. Only the effect of left or right one is performed.

```savedLean
class Conditional (program : Type → Type → Type) where
  sum {α β γ : Type} :
    program γ α → program β α → program (γ ⊕ β) α

export Conditional (sum)

infixl:55 " ||| " => sum
```

`sum` also has infix notation `|||`.


## Capability combinations

The idea behind writing programs in terms of the type classes like the ones defined so far is to use exactly those
capabilities that are really needed to write the programs. Some combinations of type classes are more expressive than
other combinations. This implies that what you can do with them is more than what you can do the less expressive ones.
But this comes with a price. They are less flexible as far as implementation and corresponding materialization is
concerned than the less expressive ones.

## Programs and program combinators defined in terms of the basic ones

### `def identity`

Below is a simple, but extremely useful function as a program, the `identity` program.

```savedLean
def identity
    [Functional program] :
  program α α :=
    asProgram id
```

### `def let_`

Using the `let_` combinator an intermediate value can be constructed that is available for later use.

```savedLean
def let_
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α β → (program (α × β) γ → program α γ) :=
    λ αpβ αaβpγ => identity &&& αpβ >=> αaβpγ

 def in_ : α → α := id
```

The `p` in `αpβ` and `αaβpγ` stands for "program", and that the `a` in `αaβpγ` stands for "and". Think of `let_` as a
library level keyword. `in_` is also a library level keyword that, depending on your taste, may make programs more
readable.

### `def if_`

Using the `if_` combinator conditional boolean logic can be expressed. A helper function and corresponding program is
needed to define it.

```savedLean
def trueToLeftFalseToRightF : α × Bool → α ⊕ α
  | ⟨α, true⟩ => .inl α
  | ⟨α, false⟩ => .inr α

def trueToLeftFalseToRight
    [Functional program] :
  program (α × Bool) (α ⊕ α) :=
    asProgram trueToLeftFalseToRightF

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
          trueToLeftFalseToRight >=> t_apβ ||| f_apβ

def else_ {α} : α → α := id
```

The `b` in `αpb` stands for `Bool`.

# `fibonacci` and `factorial`

It turns out that we ready now for defining `fibonacci` and `factorial`.

For readability and reusability reasons it is useful to first define some primitive functions

```savedLean
def isZeroF: Nat → Bool :=
  λ ν => ν == 0

def isOneF : Nat → Bool :=
  λ ν => ν == 1

def oneF : Nat → Nat :=
  λ _ => 1

def minusOneF : Nat → Nat :=
  λ ν => ν - 1

def minusTwoF : Nat → Nat :=
  λ ν => ν - 2

def addF : Nat × Nat → Nat :=
  λ ⟨ν, μ⟩ => ν + μ

def multiplyF : Nat × Nat → Nat :=
  λ ⟨ν, μ⟩ => ν * μ
```

and corresponding primitive programs

```savedLean
def isZero [Functional program] :
  program Nat Bool :=
    asProgram isZeroF

def isOne [Functional program] :
  program Nat Bool :=
    asProgram isOneF

def one [Functional program] :
  program Nat Nat :=
    asProgram oneF

def minusOne [Functional program] :
  program Nat Nat :=
    asProgram minusOneF

def minusTwo [Functional program] :
  program Nat Nat :=
    asProgram minusTwoF

def add [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram addF

def multiply [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram multiplyF
```

Program `fibonacci` is defined as follows

```savedLean
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

```savedLean
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
the painting ["Ceci n'est pas une pipe"](https://en.wikipedia.org/wiki/The_Treachery_of_Images) of René Magritte. The
painting is, of course, not a pipe, it is a description of a pipe. A specification is also a special kind of
description. Much in the same way, descriptions of effects are, luckily enough, also not side effects. So it is safe to
hang a picture of a bomb explosion on your wall.

# What about `Functorial`?

You may wonder why neither `fibonacci` nor `factorial` use `Functorial`. A `Functorial` instance can be defined in terms
of `Function` and `Sequential`.

```savedLean
instance
    [Functional program]
    [Sequential program] :
    Functorial program where
  andThenF {α β γ: Type} :
    program α β → (β → γ) → program α γ :=
      λ αpβ => λ βfγ => αpβ >=> asProgram βfγ
```

The `f` in `βfγ` stands for "function".

So why introducing `Functorial` in the first place? Well, the combination of `Functorial` with `Functional`, and
`Creational` is sufficiently expressive to write interesting programs and that are more flexible as far as
implementation and corresponding materialization is concerned than the ones using `Sequential`.

Below are two programs, `twiceMinusOne01` and `twiceMinusOne02`.

```savedLean
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
showed (effectfree) functions. But think of replacing `minusOne` and/ or `minusTwO` by an effectful program. Having more
implementation and corresponding materialization flexibility when dealing with effects can really be useful. A standard
example is more flexible error handling when processing a submitted web form and even error correction when parsing a
document.

# Computation Valued Functions

Using computation valued functions is a generic way to implement the program related type classes in terms of the
computation related type classes

```savedLean
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
    λ ⟨αfcβ⟩ => λ βfγ => ⟨λ α => βfγ <$> αfcβ α⟩

instance [Applicative computation] :
    Creational
      (FromComputationValuedFunction computation) where
  product := λ ⟨αfcβ⟩ ⟨αfcγ⟩ =>
    ⟨λ α => pure Prod.mk <*> αfcβ α <*> αfcγ α⟩

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

The the `c` in `αfcβ` stands for "computation".

# `ActiveProgram`

There is not a lot of work to be done for active implementations. The `Functor`, `Applicative` and `Monad`
implementations of `Id` can be used.

```savedLean
abbrev Active := Id

abbrev ActiveProgram α β :=
  FromComputationValuedFunction Active α β

def materializeActive :
    ActiveProgram α β → (α → β) :=
  λ ⟨αpβ⟩ => λ α => (αpβ α).run
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
  map : {α β : Type} →
      (α → β) →
      (ReactiveT ρ computation α →
      ReactiveT ρ computation β) :=
    λ αfβ rpa =>
      ReactiveT.mk (λ γ => rpa.runReactiveT (γ ∘ αfβ))

instance {ρ: Type} :
    Applicative (ReactiveT ρ computation) where
  pure := λ α => ReactiveT.mk (λ afcr => afcr α)
  seq: {α β : Type} →
      (ReactiveT ρ computation (α → β)) →
      (Unit → (ReactiveT ρ computation α)) →
      (ReactiveT ρ computation β) :=
    λ rpafc ufrpa =>
      ReactiveT.mk (λ bfcr =>
        rpafc.runReactiveT $
          (λ αfβ =>
            (ufrpa ()).runReactiveT (bfcr ∘ αfβ)))

instance {ρ: Type} :
    Monad (ReactiveT ρ computation) where
  bind: {α β : Type} →
      (ReactiveT ρ computation α) →
      (α → ReactiveT ρ computation β) →
      (ReactiveT ρ computation β) :=
    λ rpa afrpb =>
      ReactiveT.mk (λ bfcr =>
        rpa.runReactiveT (λ α =>
        (afrpb α).runReactiveT bfcr))

abbrev ReactiveProgram ρ computation :=
  FromComputationValuedFunction (ReactiveT ρ computation)

def materializeReactive {α β : Type} :
    ReactiveProgram β Active α β → α → β :=
  λ αpβ =>
    λ α =>
      (αpβ.toComputationValuedFunction α).runReactiveT id
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

We did not change the definition of our programs, we only materialized them in another way.

# Positional Programming

## Using `Functorial`

Pointfree programming, like is done for `fibonacci` and `factorial` may be a elegant, but `PSBP` also enables, and,
for reasons of elegance, sometimes needs, positional programming. Let's first start with `Functorial` based positional
programming. Suppose we want to run the function that transforms an argument value `ν` of type `Nat` to result value
`(((ν-1, ν-2), 2), 3) => (ν-2) + 2 * (ν-1) + 3`. The `someProgram01` below could be a solution. `someProgram01` makes
use of `Functorial`.

```savedLean (name := someProgram01)
def twoF : Nat → Nat := λ _ => 2

def threeF : Nat → Nat := λ _ => 3

def two [Functional program] :
  program Nat Nat :=
    asProgram twoF

def three [Functional program] :
  program Nat Nat :=
    asProgram threeF

def someProgram01
    [Functional program]
    [Functorial program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusTwo &&& two &&& three >->
      λ (((ν1, ν2), ν3), ν4) =>
        ν2 + ν3 * ν1 + ν4
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
somehow you are right, but notice that all `ν`'s involved have indices (`1`, `2`, `3` and `4`). They can be thought of
as positions. So we are essentially accessing values at positions of multi-values. For this example the multi-value
involved is homogeneous but it might as well be a heterogeneous one. More about this later.

## Using `Sequential`

The `someProgram02` below could also be a solution. `someProgram02` makes use of `Sequential`.

```savedLean
def someProgram02
    [Functional program]
    [Sequential program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusTwo &&& two &&& three >=>
      asProgram (λ (((ν1, ν2), ν3), ν4) =>
        ν2 + ν3 * ν1 + ν4)
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

As already mentioned before using `Sequential` is, in this case, an overkill.

## `class Positional`

```savedLean
class Positional (program : Type → Type → Type) where
  at_ {σ α β γ : Type} :
    program α β →
    program σ α →
    program (σ × β) γ →
    program σ γ

export Positional (at_)

infixl:45 " @ " => at_
```

The `σ` stands for (runtime) "stack", and `σ × β` stands for `β` pushed onto `σ`. More about this later.

The `at_` library level keyword of `Positional` can be defined in terms of `Functional`, `Creational` and `Sequential`.

## `instance Positional`

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

`positionOne` and `positionTwo` are single-value positions. `positionOneAndTwo` is a multi-value position.

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

It is instructive to show the runtime stack using `identity` as in `positionalFactorialOfFibonacci'` and
`positionalSumOfFibonacciAndFactorial'`

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

# Stateful Programming

## `class Stateful σ`

`PSBP` enables stateful programming using the `Stateful` class below.

```savedLean
class Stateful
    (σ : Type)
    (program : Type → Type → Type) where
  readState {α : Type} : program α σ
  writeState : program σ Unit

export Stateful (readState writeState)
```

### `def modifyStateWith`

Below is `modifyStateWith` a useful stateful programming capability.

Let

```savedLean
def first
    [Functional program] :
  program (α × β) α :=
    asProgram λ (α, _) => α

def second
    [Functional program] :
  program (α × β) β :=
    asProgram  λ (_, β) => β
```

in (only `first` above is used)

```savedLean
def modifyStateWith
    [Functional program]
    [Sequential program]
    [Creational program]
    [Stateful σ program] :
  (σ → σ) → program α α :=
    λ σfσ =>
      let_ ((readState >=> asProgram σfσ) >=> writeState) $
        in_ $
          first
```

`modifyStateWith` modifies the state and does not transform the initial value (argument).

### `def usingAndModifyingStateAsArgumentWith`

Below is `usingAndModifyingStateAsArgumentWith` a useful stateful programming capability.

```savedLean
def readingInitialStateAsInitialValueAndModifyingItWith
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [Stateful σ program] :
  (σ → σ) → program σ σ → program α σ :=
    λ σfσ =>
      λ σpσ =>
        (readState >=> modifyStateWith σfσ) >=> σpσ
```

Given a program `σpσ`, `readState >=> modifyStateWith` reads the state so that it becomes the initial value
(argument of) `σpσ`, modifies the state (not transforming the initial value (argument)), and then `σpσ`
transforms that initial value to a final value.

## `instance Stateful σ`

`Stateful σ` is implemented in terms of `MonadStateOf`.

```savedLean
instance [MonadStateOf σ computation] :
    Stateful σ
      (FromComputationValuedFunction computation) where
  readState := .mk λ _ => get
  writeState := .mk set
```

## `StatefulProgram`

```savedLean
abbrev StatefulProgram σ computation :=
  FromComputationValuedFunction (StateT σ computation)

def materializeStateful
    [Monad computation] {α β : Type} :
      StatefulProgram σ computation α β →
      α →
      σ →
      computation β :=
  λ αpβ =>
    λ α =>
      λ σ =>
        StateT.run (αpβ.toComputationValuedFunction α) σ >>=
          λ (β, _) => pure β

def materializeActiveStateful {α β : Type} :
  StatefulProgram σ Active α β → α → σ → β :=
    materializeStateful
```

## `fibonacciIncrementingArgumentPair`

Program `fibonacciIncrementingArgumentPair` below shows the effectfulness of stateful programs.

Let

```savedLean
unsafe def fibonacciIncrementingArgument
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [Stateful Nat program] :
  program Nat Nat :=
    readingInitialStateAsInitialValueAndModifyingItWith
    (. + 1)
    fibonacci
```

in

```savedLean
unsafe def fibonacciIncrementingArgumentPair
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [Stateful Nat program] :
  program Nat (Nat × Nat) :=
    fibonacciIncrementingArgument &&&
    fibonacciIncrementingArgument
```

```savedLean (name := activeFibonacciIncrementingArgumentPair)
#eval
  materializeActiveStateful
    fibonacciIncrementingArgumentPair
    0
    10
```

```leanOutput activeFibonacciIncrementingArgumentPair
(89, 144)
```
