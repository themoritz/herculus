# Herculus Formula Manual

## The Hexl Language

### Basic Arithmetic

* `+, -, *`

### Using Functions

For a list of included functions, see [Prelude](##PreludeFunctions).

### Defining Functions and Variables

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

### Conditionals

* If-then-else

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
