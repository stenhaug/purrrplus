library(tidyverse)
library(testthat)
library(purrrplus)

context("throws errors")

f <- function(a){a + 1}

test_that("must return a named vector or list", {
    expect_error(data_frame(a = 1) %>% pmap_safely(f) %>% get_results(); 2 + 2,
                 "Evaluation error: The function needs to return a named list or vector.")
})

test_that("requires matching arguments", {
    expect_error(data_frame(b = 1) %>% pmap_safely(f),
                 "Data frame does not not contain all function arguments")
})
