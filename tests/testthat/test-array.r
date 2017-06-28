context("array_datatypes")

test_that("contracts for array element datatypes work", {

  integer_fun <- function(@:integer[3] val) @:integer[3] { val + 1L }
  double_fun <- function(@:double[3,4] val) @:double[3,4] { val * 2 }
  numeric_fun <- function(@:numeric[1,1,1,] val) @:numeric[1,1,1,] { val/2L }
  logical_fun <- function(@:logical[,8,,9] val) @:logical[7,8,,] { !val }
  character_fun <- function(@:character[,,] val) @:character { paste0("hi", val) }
  complex_fun <- function(@:complex[1,,5] val) @:complex[1,,5] { Conj(val) }
  any_fun <- function(@:any[,,,] val) @:any[,,] { val }
  raw_fun <- function(@:raw[,] val) @:raw[2,3] { as.raw(0) & val }

  annotatr:::process_annotations()

  
  ## integer
  expect_equal(integer_fun(c(1L, 2L, 3L)), c(2L, 3L, 4L))
  expect_error(
    integer_fun(2L),
    "argument type mismatch.*expected.*integer\\[3\\].*received.*integer\\[1\\]",
    perl = TRUE
  )
  expect_error(
    integer_fun(3),
    "argument type mismatch.*expected.*integer\\[3\\].*received.*double\\[1\\]",
    perl = TRUE
  )
  expect_error(
    integer_fun(3.0),
    "argument type mismatch.*expected.*integer\\[3\\].*received.*double\\[1\\]"
  )

})
