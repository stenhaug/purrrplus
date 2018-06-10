
# purrrplus

The tidyverse makes working with data frames particularly easy. However,
using pmap and running a function safely returns a list. purrrplus adds
functionality to purrr that keeps analyis within the structure of a data
frame, which is particularly useful for running simulations and specific
types of data analysis tasks.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("stenhaug/purrrplus")
```

## Getting started

``` r
library(purrrplus)
library(tidyverse) # most useful with the tidyverse
```

### Example

Imagine you have a function (which returns a named list or a named
vector and might throw an error):

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

### The old way

The irrelevant variable causes pmap to throw an error:

``` r
output <- pmap(numbers, calculate_if_positive)
## Error in .f(a = .l[[c(1L, i)]], b = .l[[c(2L, i)]], irrelevant = .l[[c(3L, : unused argument (irrelevant = .l[[c(3, i)]])
```

One way around this is to remove the irrelevant variables:

``` r
numbers2 <- select(numbers, -irrelevant)
```

However, now calculate\_if\_positive function throws an error that stops
everything if *any* of the rows contain a negative number:

``` r
output <- pmap(numbers2, calculate_if_positive)
## Error in .f(a = .l[[c(1L, i)]], b = .l[[c(2L, i)]], ...): Just the first number is negative
```

We are applying the function calculate\_if\_positive 4 times (once for
each row in the data frame numbers). It should work for rows 2 and 3 and
throw an error for rows 1 and 4.

The purrrr function safely allows us to capture the results when it
works and the error when it doesn’t:

``` r
output <- pmap(numbers2, safely(calculate_if_positive))
```

However, a function wrapped in safely returns a list which is difficult
to work with:

``` r
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

### The purrrplus way

pmap\_safely is the key function in purrrplus. pmap\_safely takes a data
frame (which might contain irrelevant variables) and applies a function
(which returns a named list or a named vector and might throw an error)
to each row.

pmap\_safely returns the data frame that was inputted but with an error
and a result column which come from applying the function:

``` r
(output <- pmap_safely(numbers, calculate_if_positive))
## Note that the function does not use the following variables: irrelevant
## # A tibble: 4 x 5
##       a     b irrelevant  error                              result    
##   <dbl> <dbl> <chr>       <chr>                              <list>    
## 1    -1     2 minneapolis Just the first number is negative  <NULL>    
## 2     0     1 st_paul     <NA>                               <list [4]>
## 3     1     0 minneapolis <NA>                               <list [4]>
## 4     2    -1 st_paul     Just the second number is negative <NULL>
```

#### get\_errors

get\_errors allows for quick analysis of errors:

``` r
get_errors(output)
## # A tibble: 10 x 5
##    variable   value       n_errors count error_rate
##    <chr>      <chr>          <int> <int>      <dbl>
##  1 a          -1                 1     1        1  
##  2 a          2                  1     1        1  
##  3 a          0                  0     1        0  
##  4 a          1                  0     1        0  
##  5 b          -1                 1     1        1  
##  6 b          2                  1     1        1  
##  7 b          0                  0     1        0  
##  8 b          1                  0     1        0  
##  9 irrelevant minneapolis        1     2        0.5
## 10 irrelevant st_paul            1     2        0.5
```

get\_errors with specific = TRUE breaks down the analysis by the
specific error:

``` r
get_errors(output, specific = TRUE)
## # A tibble: 12 x 6
##    variable   value       error                              n count  rate
##    <chr>      <chr>       <chr>                          <int> <int> <dbl>
##  1 a          -1          Just the first number is nega…     1     1   1  
##  2 a          0           <NA>                               1     1   1  
##  3 a          1           <NA>                               1     1   1  
##  4 a          2           Just the second number is neg…     1     1   1  
##  5 b          -1          Just the second number is neg…     1     1   1  
##  6 b          0           <NA>                               1     1   1  
##  7 b          1           <NA>                               1     1   1  
##  8 b          2           Just the first number is nega…     1     1   1  
##  9 irrelevant minneapolis Just the first number is nega…     1     2   0.5
## 10 irrelevant minneapolis <NA>                               1     2   0.5
## 11 irrelevant st_paul     Just the second number is neg…     1     2   0.5
## 12 irrelevant st_paul     <NA>                               1     2   0.5
```

get\_results filters out rows with errors and unnests results such that
each item in the list that the function returns has its own column:

#### get\_results

``` r
get_results(output)
## Removed 2 errors out of 4 rows.
## # A tibble: 2 x 7
##       a     b irrelevant  add_result subtract_result multiply_result
##   <dbl> <dbl> <chr>            <dbl>           <dbl>           <dbl>
## 1     0     1 st_paul              1              -1               0
## 2     1     0 minneapolis          1               1               0
## # ... with 1 more variable: divide_result <dbl>
```

Notice that "\_result" is appended to each of these columns so any
subsequent analysis can easily differentiate between variables the
function produced and variables it didn’t.
