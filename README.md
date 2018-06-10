
# purrrplus

The purrr package expands the functionality of purrr to maintain data
frame structure and run functions safely.

The beauty of the tidyverse is that it allows you to more often work
within data frames which have a clean structure. However, both running a
function safely (which returns a list) and using pmap make it a bit more
troublesome to keep analysis within the structure of a data frame.
purrrplus adds functionality to purrr that keeps analyis cleanly within
the structure of a data frame. This is useful for running simulations
and specific types of data analysis tasks.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("stenhaug/purrrplus")
```

## Getting started

``` r
library(purrrplus)
library(tidyverse) # examples below will require the tidyverse
```

### Example

Imagine you have a function (which returns a named list or a named
vector) and might throw an error:

``` r
calculate_if_positive <- function(a, b){
  if(a < 0 & b < 0) {stop("Both numbers are negative.")}
  else if(a < 0) {stop("Just the first number is negative")}
  else if(b < 0) {stop("Just the second number is negative")}
  
  list(add = a + b,
       subtract = a - b,
       multiply = a * b,
       divide = a / b)
}
```

And you want to apply this function to each row of a data frame (which
might contain irrelevant variables):

``` r
(numbers <- data_frame(a = c(-1, 0, 1, 2),
                      b = c(2, 1, 0, -1),
                      irrelevant = c("minneapolis", "st_paul", "minneapolis", "st_paul")))
## # A tibble: 4 x 3
##       a     b irrelevant 
##   <dbl> <dbl> <chr>      
## 1    -1     2 minneapolis
## 2     0     1 st_paul    
## 3     1     0 minneapolis
## 4     2    -1 st_paul
```

### The Old Way

The irrelevant variable causes pmap to throw an error:

``` r
output <- pmap(numbers, calculate_if_positive)
## Error in .f(a = .l[[c(1L, i)]], b = .l[[c(2L, i)]], irrelevant = .l[[c(3L, : unused argument (irrelevant = .l[[c(3, i)]])
```

One way around this is to remove the irrelevant variables. However, now
calculate\_if\_positive function throws an error that stops everything
if *any* of the rows contains a negative number.

``` r
numbers2 <- select(numbers, -irrelevant)

output <- pmap(numbers2, calculate_if_positive)
## Error in .f(a = .l[[c(1L, i)]], b = .l[[c(2L, i)]], ...): Just the first number is negative
```

We are applying the function calculate\_if\_positive 4 times (once for
each row in the data frame numbers). It should work for rows 2 and 3 and
throw an error for rows 1 and 4. The purrrr function safely allows us to
capture the results when it works and the error when it doesn’t.

However, a function wrapped in safely returns a list which can be
difficult to work with.

``` r
output <- pmap(numbers2, safely(calculate_if_positive))
str(output)
## List of 4
##  $ :List of 2
##   ..$ result: NULL
##   ..$ error :List of 2
##   .. ..$ message: chr "Just the first number is negative"
##   .. ..$ call   : language .f(...)
##   .. ..- attr(*, "class")= chr [1:3] "simpleError" "error" "condition"
##  $ :List of 2
##   ..$ result:List of 4
##   .. ..$ add     : num 1
##   .. ..$ subtract: num -1
##   .. ..$ multiply: num 0
##   .. ..$ divide  : num 0
##   ..$ error : NULL
##  $ :List of 2
##   ..$ result:List of 4
##   .. ..$ add     : num 1
##   .. ..$ subtract: num 1
##   .. ..$ multiply: num 0
##   .. ..$ divide  : num Inf
##   ..$ error : NULL
##  $ :List of 2
##   ..$ result: NULL
##   ..$ error :List of 2
##   .. ..$ message: chr "Just the second number is negative"
##   .. ..$ call   : language .f(...)
##   .. ..- attr(*, "class")= chr [1:3] "simpleError" "error" "condition"
```

We can connect the output back to the original data frame, but any
subsequent analysis will be quite tricky given the structure of the
output column:

``` r
mutate(numbers, output = output)
## # A tibble: 4 x 4
##       a     b irrelevant  output    
##   <dbl> <dbl> <chr>       <list>    
## 1    -1     2 minneapolis <list [2]>
## 2     0     1 st_paul     <list [2]>
## 3     1     0 minneapolis <list [2]>
## 4     2    -1 st_paul     <list [2]>
```
