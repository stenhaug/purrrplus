library(tidyverse)
library(testthat)
library(purrrplus)

context("gives messages")

f <- function(a){c(name = a + 1)}

test_that("argument matching messages", {
    expect_message(data_frame(a = 1, b = 2) %>% pmap_safely(f),
                   "Note that the function does not use the following variables: b")

    expect_message(data_frame(a = 1) %>% pmap_safely(f),
                   "Great! Perfect match between function arguments and variables.")
})
