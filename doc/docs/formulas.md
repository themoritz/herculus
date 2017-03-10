# Formulas

This manual explains the _Hexl language_ that you use in Herculus to express
formulas.  A _formula_ describes how the values of the cells in a column are
depending on other columns. For example, a formula for some column might be as
simple as `$Quantity * $Price`, indicating that the cells are the products of
the cells from the "Quantity" and the "Price" columns.

Every formula consists of _expressions_ that are combined into larger
expressions. The formula above consists of two sub-expressions, `$Quantity` and
`$Price`, which are combined by the `*` sign.  Every sub-expression _evaluates_
to a _value_, which is then used to evaluate the larger expression.

The Hexl language can also be used inside
of [report templates](#report-templates).  Any expression that can appear in the
formula of a column can also appear in the control blocks of a template, for
example in an if-then-else block:

``` handlebars
{% if $Quantity > 0 %} Something {% else %} Nothing {% endif %}
```

## The Hexl Language

### Types

In the same way that every column in Herculus has a specific _type_ (e.g.,
`String` or `Number`), every expression in Hexl has a type. You can think of an
expression's type as the set of values that it can evaluate to.

The formula of a column is only valid if its type is the same as that of its
column. The type of an expression is automatically determined by Herculus.  For
example, the expression `2 + 2` has type `Number`, since it's the sum of two
`Number`s.

Currently, the columns of tables in Herculus can have one of the following
types:

* `Number`. The set of all decimal numbers, for example `-4`, or `3.8`.
* `String`. The set of all strings, for example `"Hello"` or `""`.
* `Bool`. The set of the two boolean values `True` and `False`.
* `Time`. The set of all points in time.
* `Row t`, where `t` is a table in the current project. The set of all rows from
  table `t`.
* `Maybe a`, where `a` is again a type. The set of all values of type `a` plus
  the value `Nothing`.  This type is used to express that a value may not be
  there.
* `List a`, where `a` is another type. The set of all lists with elements of
  type `a`.  For example `List Number` is the set of all lists with numbers in
  them.

### Basic Arithmetic

You can use basic arithmetic operators to combine two expressions of type
`Number`:

* `+` adds two numbers
* `-` subtracts the second number from the first
* `*` multiplies two numers
* `/` divides the first by the second number

For example, to subtract `a` from `b` you would simply write `b - a`.

Every one of these operators takes two `Number` values as inputs, and returns a
new `Number`.  We express this fact as follows:

``` haskell
(+) : Number -> Number -> Number
```

We call this the _signature_ of the operator `+`.

### Conditionals

Hexl has an if-then-else expression to determine which one of two
sub-expressions is used given a value of type `Bool`:

``` haskell
if condition then trueBranch else falseBranch
```

In this expression, `condition` is a sub-expression of type `Bool`. When it
evaluates to the value `True`, the whole expression will have the value of
`trueBranch`, otherwise it will have the value of `falseBranch`. Therefore, if
`trueBranch` and `falseBranch` have the same type `a`, the whole if-then-else
expression will also be of this type.

For example, the following is a valid if-then-else expression when `x` is a
`Number`:

``` haskell
if x > 0 then x - 1 else x
```

Since it's comparing two numbers, the condition `x > 0` has type `Bool`. Both
sub-expression branches `x - 1` and `x` are of type `Number`, so the whole
if-then-else expression has type `Number`.

### Using Functions

A function can be applied to a value by just writing it in front of it. For
example, f `x` is a boolean value, we write `not x` to negate it.

If a function takes more than one argument, the arguments are just listed one
after another:

``` haskell
formatNumber "%.2f" 2
```

If you want an argument to be a more complicated sub-expression, just put
parentheses around it:

``` haskell
formatNumber "%.2f" (1 / 7)
```

For a list of functions that are included,
see [Included Functions](#included-functions).

### Defining Your Own Variables and Functions

New variables can be introduced using a _let_ expression, followed by another
expression:

```haskell
let x = 1 / 7;
let y = x * x;
if y > 0.5 then "Foo" else "Bar"
```

Own functions can also be defined using let expressions, by introducing the
arguments between the function name and the `=`:

``` haskell
let square x = x * x;
square (13 - 4)
```

#### Anonymous Functions

A function can also be defined without giving it a name, by using a _lambda_
expression.  For example, the expression `\x -> x >= 0` is a function, which
takes one argument, a `Number`, and returns the `Bool`ean value `True` exactly
when it is greater than `0`.

This is often useful when using other functions that take a function as an
argument, such as [`filter`](#filter):

``` haskell
filter (\s -> s.Quantity >= 10) #Sales
```

### Accessing Data From Other Cells / Tables / Columns

There are three ways to access data an other places in your project:

#### A Cell of Another Column of the Same Table

The prefix `$` is used to reference the value of a cell in another column (which
has to be part of the same table as the column you are writing the formula
for). Let's say I'm writing the formula of column "Revenue" as `$Quantity *
$Price`. Then every cell of the "Revenue" column will be the product of the
corresponding cells from the "Quantity" and the "Price" columns.

The type of expression `$Foo` will always be the type of the column "Foo".

#### A Whole Table

The prefix `#` is used to reference whole tables, represented as a `List` of
rows. For example, `#Books` returns a value of type `List a`, where `a` is a
record with one field for every column from the table
`Books`. See [Records](#records) for more details on records.

#### A Whole Column

You can also only reference one particular column of a table by joining the
table name with the desired column name via a dot: The expression
`#Sales.Quantity` refers to the "Quantity" column from the "Sales" table. It
will evaluate to a value of type `List a`, where `a` is the type of the
"Quantity" column.

### Records

A record consists of a set of named fields, each of which has a certain
type. For example, a row from a table "Customers" with the columns "Name" and
"Age" might have the record type `{ Name : String, Age : Number }`.

Given a record value, you can access the values of its individual fields using
the `.` operator. For example, to access the name of one of the "Customer" rows
`customer` as described above, one writes `customer.Name`.

## Report Templates

Herculus features so-called "Report Columns" which can generate, for each of
that column's cell, a report in various formats (PDF, HTML, Plaintext). The
report is generated according to a template which is written in various markup
languages (e.g., Markdown, HTML, LaTeX).

Independent of the markup language, the following constructs can be used to
access and print values, as well as to control what to render.

### Printing Values

To print a value use: `{{ expression }}`, where `expression` needs to be of type
`String`.

```handlebars
Customer: {{ $Name }}
Revenue: {{ show $Revenue }}
```

### For Loops

To render some part of the template for every element of a list, use:

``` handlebars
{% for element in list %}
  Body (element is in scope)
  {{ show element }}
{% endfor %}
```

Here, `list` is an expression of type `List a`, for some type `a`. The variable
`element` will be available in the _body_ of that block, and whenever used, will
evaluate to a value of type `a`.

For example, we can use this to loop over the items of a bill:

``` handlebars
<ol>
{% for item in $Items %}
  <li> <b>{{ item.Description }}</b>, quantity: {{ show item.Quantity }}
{% endfor %}
</ol>
```

### Conditionals

To render one or another part of the template, depending on the truth value of
some expression, use:

```handlebars
{% if condition %}
  Condition evaluated to true
{% else %}
  Condition evaluated to false
{% endif %}
```

Here, `condition` can be any expression which has a type `Bool`.

<!---
Ideas for more template features:

<h1>Posts</h1> }}

{{#decl let f = \i -> i * 2;}}

{%for r in filter p #Posts%}
  <h2>{{ r.Title }}</h2>
  {%case r.Prop%}
  {%of True%}
      True {{ show $A }}
  {%of False%}
      False
  {%case%}
{%for%}

{{#define template a}}
  {{#for r in a}}
    {{#instantiate template this.b}}
  {{/for}}
{{/define}}

{{#instantiate template "Moritz"}}
-->

## Included Functions

Herculus ships with a number of functions that are ready to use. For every
function, its signature is given to indicate the number and types of arguments
it takes, as well as the type of values it returns.

For example, the function `length` takes a list of elements of any type (this is
what the `forall` says), and returns a `Number` (the length of the list).

Some functions are part of a _type class_, which is a set of types that
implement that functionality. This is a way of overloading functions for use
with different types. For example, the function `show` is part of the
type class called `Show`. There are two types which implement this type class
(`Number` and `Bool`). We also say that these two types provide _instances_ for
the `Show` typeclass.

If a function is written in parentheses it means that it should be used _infix_,
meaning it should be put between its two arguments. Examples are the comparison
operators, but also `||` or `<>`.

### sum

```haskell
sum : List Number -> Number
```

Calculates the sum over a list of numbers.

### length

```haskell
length : forall a. List a -> Number
```

Get the length of a list.

### const

```haskell
const : forall a b. a -> b -> a
```

The function `const a` always returns `a`.

### not

```haskell
not : Bool -> Bool
```

Negate a boolean value.

### (||)

```haskell
(||) : Bool -> Bool -> Bool
```

Disjunction of two booleans. The result is true if the first, the second, or
both arguments are true.

_right-associative, precedence: 2_

### (&&)

```haskell
(&&) : Bool -> Bool -> Bool
```

Conjunction of two booleans. The result is true if both arguments are true.

_right-associative, precedence: 3_

### class Show

Class of types that can be converted into a `String`.

```haskell
class Ord a where
```

#### Members

* `show : a -> String`

  Convert the argument into a string.

#### Instances

* `Show Number`
* `Show Bool`

### class Eq

Class of types where elements can be compared to each other.

```haskell
class Ord a where
```

#### Members

* `(==) : a -> a -> Bool`

  Returns true if both arguments are equal.

  _non-associative, precedence: 4_
* `(!=) : a -> a -> Bool`

  Returns true if both arguments are not equal.

  _non-associative, precedence: 4_

#### Instances

* `Eq Number`
* `Eq Bool`
* `Eq Time`
* `Eq String`

### class Ord

Class of types that can be ordered.

```haskell
class Ord a where
```

#### Members

* `(<=) : a -> a -> Bool`

  Returns true if the first argument is less than or equal to the second.

  _left-associative, precedence: 4_
* `(>=) : a -> a -> Bool`

  Returns true if the first argument is more than or equal to the second.

  _left-associative, precedence: 4_
* `(<) : a -> a -> Bool`

  Returns true if the first argument is less than the second.

  _left-associative, precedence: 4_
* `(>) : a -> a -> Bool`

  Returns true if the first argument is more than the second.

  _left-associative, precedence: 4_

#### Instances

* `Ord Number`
* `Ord Time`

### class Semigroup

Class of types where elements can be "appended" to one another.

```haskell
class Semigroup a where
```

#### Members

* `(<>) : a -> a -> a`

  Appends two values of `a` to get a new value of `a`. Example: `"Hello " <>
  name`.

  _right-associative, precedence: 5_

#### Instances

* `Semigroup String`

### class Functor

Class of types that represent "containers" which can be mapped over.

``` haskell
class Functor f where
```

#### Members

* `map : forall a b. (a -> b) -> f a -> f b`

  Apply a function to every element in the container `f` (e.g., a list), and
  return the result.

#### Instances

* `Functor List`
* `Functor Maybe`

### formatNumber

```haskell
formatNumber : String -> Number -> String
```

A call like `formatNumber s x` formats the number `x` according to the format string `s`. For a list of formatting characters, see [here](https://hackage.haskell.org/package/base-4.9.0.0/docs/Text-Printf.html#v:printf).

### roundTo

``` haskell
roundTo : Number -> Number -> Number
```

For example, the expression `roundTo 2 n` rounds the number `n` to a precision of `2`.

### formatTime

```haskell
formatTime : String -> Time -> String
```

`formatTime s t` formats time `t` according to the format string `s`. For a list of formatting characters see [here](https://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Format.html#v:formatTime).

### year

``` haskell
year : Time -> Number
```

Returns the year of a given time.

### month

``` haskell
month : Time -> Number
```

Returns the month of the year (1-12) of a given time.

### day

``` haskell
day : Time -> Number
```

Returns the day of the month (1-31) of a given time.

### filter

```haskell
filter : forall a. (a -> Bool) -> List a -> List a
```

Filter a list according to the given predicate. For example,

``` haskell
let predicate x = x.Temperature >= 0 && x.Temperature <= 10;
filter predicate #Cities
```

would filter the rows of the "Cities" table to include only those where the
temperature is between `0` and `10` degrees.

### find

```haskell
find : forall a. (a -> Bool) -> List a -> Maybe a
```

Find the first element in a list that matches the given predicate, or return `Nothing`.

### fromMaybe

``` haskell
fromMaybe : forall a. a -> Maybe a -> a
```

The expression `fromMaybe 0 x` returns the content of x if there is one, or `0`
in case of `Nothing`.

### maybe

``` haskell
maybe : forall a b. b -> (a -> b) -> Maybe a -> b
```

The expression `maybe b f x` applies the function `f` to the content of x, or
just returns `b`, if x is `Nothing`.

