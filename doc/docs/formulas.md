# Formulas

This manual explains the _Hexl language_ that you use in Herculus to express
formulas.  A _formula_ describes how the values of the cells in a column are
depending on other columns. For example, a formula for some column might be as
simple as `$Quantity * $Price`, indicating that the cells are the products of
the cells from the "Quantity" and the "Price" columns.

Every formula consists of _expressions_ that are combined into larger
expressions. The formula above consists of two sub-expressions, `$Quantity` and
`$Price`, which are combined by the `*` operator.  Every sub-expression
_evaluates_ to a _value_, which is then used to evaluate the larger expression.

The Hexl language can also be used inside
of [report templates](#report-templates).  Any expression that can appear in the
formula of a column can also appear in the control blocks of a template, for
example in an if-then-else block:

``` handlebars
{% if $Quantity > 0 %} Something {% else %} Another thing {% endif %}
```

## The Hexl Language

### Types

In the same way that every column in Herculus has a specific _type_ (e.g.,
`String` or `Number`), every expression in Hexl has a type. You can think of an
expression's type as the set of values that it can evaluate to.

The formula of a column is only valid if its type is the same as that of its
column. The type of an expression is automatically determined by Herculus.  For
example, the expression `2 + 2` has type `Integer`, since it's the sum of two
`Integer`s.

Currently, the columns of tables in Herculus can have one of the following
types:

* `Number`. The set of all decimal numbers, for example `-4.0` or `3.8`.
* `Integer`. The set of all natural numbers, for example `0` or `-14`.
* `String`. The set of all strings, for example `"Hello"` or `""`.
* `Boolean`. The set of the two boolean values `True` and `False`.
* `DateTime`. The set of all points in time.
* `Row t`, where `t` is a table in the current project. The set of all rows from
  table `t`.
* `Maybe a`, where `a` is again a type. The set of all values of type `a` plus
  the value `Nothing`.  This type is used to express that a value may not be
  there.
* `List a`, where `a` is another type. The set of all lists with elements of
  type `a`.  For example `List Number` is the set of all lists with numbers in
  them.

### Basic Arithmetic

You can use basic arithmetic operators to combine two expressions with a type
that implements the `Num` interface.

* `+` adds two numbers
* `-` subtracts the second number from the first
* `*` multiplies two numers
* `/` divides the first by the second number

For example, to subtract `a` from `b` you would simply write `b - a`.

### Conditionals

Hexl has an if-then-else expression to determine which one of two
sub-expressions is used given a value of type `Boolean`:

``` haskell
if condition then trueBranch else falseBranch
```

In this expression, `condition` is a sub-expression of type `Boolean`. When it
evaluates to the value `True`, the whole expression will have the value of
`trueBranch`, otherwise it will have the value of `falseBranch`. Therefore, if
`trueBranch` and `falseBranch` have the same type `a`, the whole if-then-else
expression will also be of this type.

For example, the following is a valid if-then-else expression when `x` is a
`Number`:

``` haskell
if x > 0 then x - 1 else x
```

Since it's comparing two numbers, the condition `x > 0` has type `Boolean`. Both
sub-expression branches `x - 1` and `x` are of type `Number`, so the whole
if-then-else expression has type `Number`.

### Using Functions

A function can be applied to a value by just writing it in front of it. For
example, if `x` is a boolean value, we write `not x` to negate it.

The `not` function takes a `Boolean` as input, and returns a
new `Boolean`.  We express this fact as follows:

``` haskell
not : Boolean -> Boolean
```

We call this the _signature_ of the function `not`.

If a function takes more than one argument, the arguments are just listed one
after another:

``` haskell
formatNumber "%.2f" 2.0
```

Since the `formatNumber` function takes a value of type `String`, and then a
value of type `Number`, and returns a value of type `String` (the formatted
number), the sugnature is:

``` haskell
formatNumber : String -> Number -> Number
```

If you want an argument to be a more complicated sub-expression, just put
parentheses around it:

``` haskell
formatNumber "%.2f" (1 / 7)
```

For a list of functions that are included, see
the [Function Reference](reference.md).

### Defining Your Own Variables and Functions

New variables can be introduced on new lines above the formula:

```haskell
x = 1 / 7
y = x * x

if y > 0.5 then "Foo" else "Bar"
```

Own functions can also be defined in the same way, by introducing the arguments
between the function name and the `=`:

``` haskell
square x = x * x

square (13 - 4)
```

#### Anonymous Functions

A function can also be defined without giving it a name, by using a _lambda_
expression.  For example, the expression `\x -> x > 0` is a function, which
takes one argument, a `Number`, and returns the `Boolean` value `True` exactly
when it is greater than `0`.

This is often useful when using other functions that take a function as an
argument, such as [`filter`](#filter):

``` haskell
filter (\s -> s.Quantity >= 10) #Sales
```

### Accessing Data From Other Cells / Tables / Columns

There are three ways to access data in other places in your project:

#### A Cell of Another Column of the Same Table

The prefix `$` is used to reference the value of a cell in another column (which
has to be part of the same table as the column you are writing the formula
for). Let's say I'm writing the formula of column "Revenue" as `$Quantity *
$Price`. Then every cell of the "Revenue" column will be the product of the
corresponding cells from the "Quantity" and the "Price" columns.

The type of expression `$Foo` will always be the type of the column "Foo".

#### A Whole Table

The prefix `#` is used to reference whole tables, represented as a `List` of
rows. For example, `#Books` returns a value of type `List a`, where `a` is a row
from the table `Books`.

#### A Whole Column

You can also only reference one particular column of a table by joining the
table name with the desired column name via a dot: The expression
`#Sales.Quantity` refers to the "Quantity" column from the "Sales" table. It
will evaluate to a value of type `List a`, where `a` is the type of the
"Quantity" column.

### Records

A record consists of a set of named fields, each of which has a certain
type. For example, the record type `{ Name : String, Age : Number }` has two
fields, `Name` and `Age`, of typ `String` and `Number`, respectively.

Given a record value, you can access the values of its individual fields using
the `.` operator. For example, to access the `Name` field of one the above
record, one would write `r.Name`, where `r` is the record.

Rows of tables can also be treated as records. For example, if there is a table
with a column `Quantity` of type `Number`, and `row` is one of its rows, we can
access the value in the "Quantity" column of that row by writing `row.Quantity`.

## Report Templates

Herculus features so-called "Report Columns" which can generate, for each of
that column's cell, a report in various formats (PDF, HTML, Plaintext). The
report is generated according to a template which is written in various markup
languages (e.g., Markdown, HTML, LaTeX).

Independent of the markup language, the following constructs can be used to
access and print values, as well as to control what to render.

### Printing Values

To print a value use `{{ expression }}`, where the type of `expression` needs
to implement the `Print` interface.

```handlebars
Customer: {{ $Name }}
Revenue: {{ $Revenue }}
```

### For Loops

To render some part of the template for every element of a list, use:

``` handlebars
{% for element in list %}
  Body (element is in scope)
  {{ element }}
{% endfor %}
```

Here, `list` is an expression of type `List a`, for some type `a`. The variable
`element` will be available in the _body_ of that block, and whenever used, will
evaluate to a value of type `a`.

For example, we can use this to loop over the items of a bill:

``` handlebars
<ol>
{% for item in $Items %}
  <li> <b>{{ item.Description }}</b>, quantity: {{ item.Quantity }}
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

Here, `condition` can be any expression which has a type `Boolean`.
