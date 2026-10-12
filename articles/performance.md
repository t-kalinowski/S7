# Performance

``` r

library(S7)
```

The dispatch performance should be roughly on par with S3 and S4, though
as this is implemented in a package there is some overhead due to
`.Call` vs `.Primitive`.

``` r

Text := new_class(parent = class_character)
Number := new_class(parent = class_double)

x <- Text("hi")
y <- Number(1)

foo_S7 := new_generic("x")
method(foo_S7, Text) <- function(x, ...) paste0(x, "-foo")

foo_S3 <- function(x, ...) {
  UseMethod("foo_S3")
}

foo_S3.Text <- function(x, ...) {
  paste0(x, "-foo")
}

library(methods)
setOldClass(c("Number", "numeric", "S7_object"))
setOldClass(c("Text", "character", "S7_object"))

setGeneric("foo_S4", function(x, ...) standardGeneric("foo_S4"))
#> [1] "foo_S4"
setMethod("foo_S4", c("Text"), function(x, ...) paste0(x, "-foo"))

# Measure performance of single dispatch
bench::mark(foo_S7(x), foo_S3(x), foo_S4(x))
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 foo_S7(x)    6.65µs    7.4µs   127593.    10.9KB     25.5
#> 2 foo_S3(x)     2.6µs   2.92µs   310393.        0B     31.0
#> 3 foo_S4(x)    2.79µs   3.16µs   304544.        0B     30.5

bar_S7 := new_generic(c("x", "y"))
method(bar_S7, list(Text, Number)) <- function(x, y, ...) paste0(x, "-", y, "-bar")

setGeneric("bar_S4", function(x, y, ...) standardGeneric("bar_S4"))
#> [1] "bar_S4"
setMethod("bar_S4", c("Text", "Number"), function(x, y, ...) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_S7(x, y), bar_S4(x, y))
#> # A tibble: 2 × 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_S7(x, y)  13.65µs  15.03µs    63841.        0B     25.5
#> 2 bar_S4(x, y)   7.34µs   8.36µs   116431.        0B     23.3
```

A potential optimization is caching based on the class names, but lookup
should be fast without this.

The following benchmark generates a class hierarchy of different levels
and lengths of class names and compares the time to dispatch on the
first class in the hierarchy vs the time to dispatch on the last class.

We find that even in very extreme cases (e.g. 100 deep hierarchy 100 of
character class names) the overhead is reasonable, and for more
reasonable cases (e.g. 10 deep hierarchy of 15 character class names)
the overhead is basically negligible.

``` r

library(S7)

gen_character <- function (n, min = 5, max = 25, values = c(letters, LETTERS, 0:9)) {
  lengths <- sample(min:max, replace = TRUE, size = n)
  values <- sample(values, sum(lengths), replace = TRUE)
  starts <- c(1, cumsum(lengths)[-n] + 1)
  ends <- cumsum(lengths)
  mapply(function(start, end) paste0(values[start:end], collapse=""), starts, ends)
}

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text := new_class(parent = class_character)
    parent <- Text
    classes <- gen_character(num_classes, min = class_nchar, max = class_nchar)
    env <- new.env()
    for (x in classes) {
      assign(x, new_class(x, parent = parent), env)
      parent <- get(x, env)
    }

    # Get the last defined class
    cls <- parent

    # Construct an object of that class
    x <- do.call(cls, list("hi"))

    # Define a generic and a method for the last class (best case scenario)
    foo_S7 := new_generic("x")
    method(foo_S7, cls) <- function(x, ...) paste0(x, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 := new_generic("x")
    method(foo2_S7, S7_object) <- function(x, ...) paste0(x, "-foo")

    bench::mark(
      best = foo_S7(x),
      worst = foo2_S7(x)
    )
  }
)
#> # A tibble: 20 × 8
#>    expression num_classes class_nchar      min   median `itr/sec` mem_alloc `gc/sec`
#>    <bch:expr>       <dbl>       <dbl> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#>  1 best                 3          15   6.45µs   7.41µs   129880.        0B    26.0 
#>  2 worst                3          15   6.71µs   7.56µs   127439.        0B    25.5 
#>  3 best                 5          15   6.48µs   7.35µs   131302.        0B    26.3 
#>  4 worst                5          15    6.9µs   7.74µs   123966.        0B    24.8 
#>  5 best                10          15   6.61µs    7.4µs   130047.        0B    26.0 
#>  6 worst               10          15    7.1µs   7.92µs   121901.        0B    24.4 
#>  7 best                50          15   6.78µs   7.55µs   127706.        0B    25.5 
#>  8 worst               50          15   8.78µs   9.66µs    98264.        0B    19.7 
#>  9 best               100          15   7.08µs   7.84µs   122971.        0B    24.6 
#> 10 worst              100          15  11.14µs     12µs    80638.        0B    16.1 
#> 11 best                 3         100   6.55µs   7.45µs   128900.        0B    25.8 
#> 12 worst                3         100   7.06µs   7.92µs   120260.        0B    24.1 
#> 13 best                 5         100   6.69µs   7.53µs   127052.        0B    25.4 
#> 14 worst                5         100   7.18µs   8.04µs   118943.        0B    23.8 
#> 15 best                10         100   6.72µs   7.59µs   126323.        0B    25.3 
#> 16 worst               10         100   7.55µs   8.42µs   113881.        0B    22.8 
#> 17 best                50         100   6.78µs   7.79µs   122794.        0B    24.6 
#> 18 worst               50         100  12.27µs  13.28µs    72777.        0B    14.6 
#> 19 best               100         100   7.27µs   8.05µs   119466.        0B    11.9 
#> 20 worst              100         100  18.86µs  19.83µs    48759.        0B     9.75
```

And the same benchmark using double-dispatch

``` r

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text := new_class(parent = class_character)
    parent <- Text
    classes <- gen_character(num_classes, min = class_nchar, max = class_nchar)
    env <- new.env()
    for (x in classes) {
      assign(x, new_class(x, parent = parent), env)
      parent <- get(x, env)
    }

    # Get the last defined class
    cls <- parent

    # Construct an object of that class
    x <- do.call(cls, list("hi"))
    y <- do.call(cls, list("ho"))

    # Define a generic and a method for the last class (best case scenario)
    foo_S7 := new_generic(c("x", "y"))
    method(foo_S7, list(cls, cls)) <- function(x, y, ...) paste0(x, y, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 := new_generic(c("x", "y"))
    method(foo2_S7, list(S7_object, S7_object)) <- function(x, y, ...) paste0(x, y, "-foo")

    bench::mark(
      best = foo_S7(x, y),
      worst = foo2_S7(x, y)
    )
  }
)
#> # A tibble: 20 × 8
#>    expression num_classes class_nchar      min   median `itr/sec` mem_alloc `gc/sec`
#>    <bch:expr>       <dbl>       <dbl> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#>  1 best                 3          15   9.36µs  10.59µs    89880.        0B    27.0 
#>  2 worst                3          15   9.86µs  11.05µs    86344.        0B    25.9 
#>  3 best                 5          15   9.55µs   10.8µs    88052.        0B    26.4 
#>  4 worst                5          15  10.11µs  11.31µs    84736.        0B    25.4 
#>  5 best                10          15   9.42µs  10.62µs    89773.        0B    26.9 
#>  6 worst               10          15  10.35µs  11.54µs    82528.        0B    24.8 
#>  7 best                50          15     10µs  11.17µs    84694.        0B    25.4 
#>  8 worst               50          15  13.72µs  14.95µs    64543.        0B    12.9 
#>  9 best               100          15  10.29µs  11.58µs    82088.        0B    24.6 
#> 10 worst              100          15  18.61µs  19.97µs    48041.        0B    14.4 
#> 11 best                 3         100   9.55µs  10.82µs    88076.        0B    26.4 
#> 12 worst                3         100  10.47µs  11.76µs    80588.        0B    24.2 
#> 13 best                 5         100   9.09µs  10.43µs    92027.        0B    27.6 
#> 14 worst                5         100  10.24µs  10.91µs    88825.        0B    26.7 
#> 15 best                10         100   9.25µs   9.89µs    98181.        0B    29.5 
#> 16 worst               10         100  11.54µs  12.28µs    79273.        0B    23.8 
#> 17 best                50         100   9.83µs  10.45µs    93029.        0B    27.9 
#> 18 worst               50         100  19.82µs  20.58µs    47300.        0B    14.2 
#> 19 best               100         100  10.22µs  10.91µs    88930.        0B    26.7 
#> 20 worst              100         100  30.64µs  31.61µs    30843.        0B     9.26
```
