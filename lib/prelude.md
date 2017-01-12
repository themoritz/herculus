# Herculus Formula Manual

This manual explains the _Hexl language_ that you use in Herculus to express formulas.
A _formula_ describes how the values of the cells in a column are depending on other
columns. For example, a formula for some column might be as simple as `$Quantity * $Price`,
indicating that the cells are the products of the cells from the "Quantity" and the "Price" columns.

Every formula consists of _expressions_ that are combined into larger expressions. The formula above
consists of two sub-expressions, `$Quantity` and `$Price`, which are combined by the `*` sign.
Every sub-expression _evaluates_ to a _value_, which is then used to evaluate the larger expression.

The Hexl language can also be used inside of [report templates](#ReportTemplates).
Any expression that can appear in the formula of a column can also appear in the control
blocks of a template, for example in an if-then-else block:

``` handlebars
{% if $Quantity > 0 %} Something {% else %} Nothing {% endif %}
```

## The Hexl Language

### Types

In the same way that every column in Herculus has a specific _type_ (e.g., `String` or `Number`),
every expression in Hexl has a type. You can think of an expression's type
as the set of values that it can evaluate to.

The formula of a column is only valid if its
type is the same as that of its column. The type of an expression is automatically determined by Herculus.
For example, the expression `2 + 2` has type `Number`, since it's the sum of two `Number`s.

Currently, the columns of tables in Herculus can have one of the following types:

* `Number`. The set of all decimal numbers, for example `-4`, or `3.8`.
* `String`. The set of all strings, for example `"Hello"` or `""`.
* `Bool`. The set of the two boolean values `True` and `False`.
* `Time`. The set of all points in time.
* `Row t`, where `t` is a table in the current project. The set of all rows from table `t`.
* `Maybe a`, where `a` is again a type. The set of all values of type `a` plus the value `Nothing`.
   This type is used to express that a value may not be there.
* `List a`, where `a` is another type. The set of all lists with elements of type `a`.
   For example `List Number` is the set of all lists with numbers in them.

### Basic Arithmetic

You can use basic arithmetic operators to combine two expressions of type `Number`:

* `+` adds two numbers
* `-` subtracts the second number from the first
* `*` multiplies two numers
* `/` divides the first by the second number

For example, to subtract `a` from `b` you would simply write `b - a`.

Every one of these operators takes two `Number` values as inputs, and returns a new `Number`.
We express this fact as follows:

``` idris
(+) : Number -> Number -> Number
```

We call this the _signature_ of the operator `+`.

### Conditionals

Hexl has an if-then-else expression to determine which one of two sub-expressions
is used given a value of type `Bool`:

``` idris
if condition then trueBranch else falseBranch
```

In this expression, `condition` is a sub-expression of type `Bool`. When it evaluates to the value
`True`, the whole expression will have the value of `trueBranch`, otherwise it will 
have the value of `falseBranch`. Therefore, if `trueBranch` and `falseBranch` have the same type `a`,
the whole if-then-else expression will also be of this type.

For example, the following is a valid if-then-else expression when `x` is a `Number`:

``` idris
if x > 0 then x - 1 else x
```

Since it's comparing two numbers, the condition `x > 0` has type `Bool`. Both sub-expression branches
`x - 1` and `x` are of type `Number`, so the whole if-then-else expression has type `Number`.

### Using Functions

For a list of included functions, see [Prelude](##PreludeFunctions).

### Defining Your Own Functions and Variables

```idris
let a = 1 + 2;
let f x = x + 1;
```

### Access Data From Other Tables / Columns

* Row-by-row: `$Quantity` returns a value with the type of column `Quantity`.
* Whole columms (from same or another table): `#Books.Price` returns a value of type `List a`, where `a` is the type of column `Price` from table `Books`.
* Whole tables: `#Books` returns a value of type `List a`, where `a` is a record with one field for every column from table `Books`.

### Records

### Anonymous Functions

## Report Templates

### Printing Values

```handlebars
My name is {{ foo }}.
```

### For Loops

```handlebars
{% for x in xs %}
  Wuseldusel {{ show x }}
{% endfor %}
```

### Conditionals

```handlebars
{% if a > b %}
  Hue
{% else %}
  Hopp
{% endif %}
```

## Prelude Functions

Herculus ships with a bunch of functions that are ready to use.

### sum

```idris
sum : List Number -> Number
```

Calculates the sum over a list of numbers.

### length

```idris
length : forall a. List a -> Number
```

Get the length of a list.

### const

```idris
const : forall a b. a -> b -> a
```

The function `const a` always returns `a`.

### not

```idris
not : Bool -> Bool
```

Negate a boolean value.

### ||

```idris
(||) : Bool -> Bool -> Bool
```

### &&

```idris
(&&) : Bool -> Bool -> Bool
```

### Show

```idris
class Show a where
```

#### Members

* `show : a -> String`

#### Instances

* `Show Number`
* `Show Bool`

### Eq

```idris
class Eq a where
```

#### Members

* `(==) : a -> a -> Bool`
* `(!=) : a -> a -> Bool`

#### Instances

* `Eq Number`
* `Eq Bool`
* `Eq Time`
* `Eq String`

### Ord

```idris
class Ord a where
```

#### Members

* `(<=) : a -> a -> Bool`
* `(>=) : a -> a -> Bool`
* `(<) : a -> a -> Bool`
* `(>) : a -> a -> Bool`

#### Instances

* `Ord Number`
* `Ord Time`

### Semigroup

```idris
class Semigroup a where
```

#### Members

* `(<>) : a -> a -> a`

  Appends two values of `a` to get a new value of `a`. Example: `"Hello " <> name`.

#### Instances

* `Semigroup String`

### Functor

``` idris
class Functor f where
```

#### Members

* `map : forall a b. (a -> b) -> f a -> f b`

  Apply a function to every element in the container `f` (e.g., a list), and return the result.

#### Instances

* `Functor List`
* `Functor Maybe`

### formatNumber

```idris
formatNumber : String -> Number -> String
```

A call like `formatNumber s x` formats the number `x` according to the format string `s`. For a list of formatting characters, see [here](https://hackage.haskell.org/package/base-4.9.0.0/docs/Text-Printf.html#v:printf).

### formatTime

```idris
formatTime : String -> Time -> String
```

`formatTime s t` formats time `t` according to the format string `s`. For a list of formatting characters see [here](https://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Format.html#v:formatTime).

### filter

```idris
filter : forall a. (a -> Bool) -> List a -> List a
```

Filter a list according to the given predicate.

### find

```idris
find : forall a. (a -> Bool) -> List a -> Maybe a
```

Find the first element in a list that matches the given predicate, or return `Nothing`.

### fromMaybe

``` idris
fromMaybe : forall a. a -> Maybe a -> a
```

### maybe

``` idris
maybe : forall a b. b -> (a -> b) -> Maybe a -> b
```
