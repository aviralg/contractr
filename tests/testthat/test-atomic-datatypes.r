context("atomic_datatypes")

test_that("contracts for atomic datatypes work", {
  integer_fun <- function(@:integer val) @:integer { val + 1L }
  double_fun <- function(@:double val) @:double { val * 2 }
  numeric_fun <- function(@:numeric val) @:numeric { val/2L }
  logical_fun <- function(@:logical val) @:logical { !val }
  character_fun <- function(@:character val) @:character { paste0("hi", val) }
  complex_fun <- function(@:complex val) @:complex { Conj(val) }
  any_fun <- function(@:any val) @:any { val }
  raw_fun <- function(@:raw val) @:raw { as.raw(0) & val }

  annotatr:::process_annotations()

  ## integer
  expect_equal(integer_fun(3L), 4L)
  expect_error(
    integer_fun(3),
    "argument type mismatch.*expected.*integer.*received.*double"
  )
  expect_error(
    integer_fun(3.0),
    "argument type mismatch.*expected.*integer.*received.*double"
  )
  expect_error(
    integer_fun(TRUE),
    "argument type mismatch.*expected.*integer.*received.*logical"
  )
  expect_error(
    integer_fun("a"),
    "argument type mismatch.*expected.*integer.*received.*character"
  )
  expect_error(
    integer_fun(1 - 2i),
    "argument type mismatch.*expected.*integer.*received.*complex"
  )
  expect_error(
    integer_fun(charToRaw("a")),
    "argument type mismatch.*expected.*integer.*received.*raw"
  )

  ## double
  expect_equal(double_fun(3), 6)
  expect_equal(double_fun(3.0), 6.0)
  expect_error(
    double_fun(3L),
    "argument type mismatch.*expected.*double.*received.*integer"
  )
  expect_error(
    double_fun(TRUE),
    "argument type mismatch.*expected.*double.*received.*logical"
  )
  expect_error(
    double_fun("a"),
    "argument type mismatch.*expected.*double.*received.*character"
  )
  expect_error(
    double_fun(1 - 2i),
    "argument type mismatch.*expected.*double.*received.*complex"
  )
  expect_error(
    double_fun(charToRaw("a")),
    "argument type mismatch.*expected.*double.*received.*raw"
  )

  ## numeric
  expect_equal(numeric_fun(6), 3.0)
  expect_equal(numeric_fun(6L), 3.0)
  expect_error(
    numeric_fun(TRUE),
    "argument type mismatch.*expected.*numeric.*received.*logical"
  )
  expect_error(
    numeric_fun("a"),
    "argument type mismatch.*expected.*numeric.*received.*character"
  )
  expect_error(
    numeric_fun(1 - 2i),
    "argument type mismatch.*expected.*numeric.*received.*complex"
  )
  expect_error(
    numeric_fun(charToRaw("a")),
    "argument type mismatch.*expected.*numeric.*received.*raw"
  )

  ## logical
  expect_equal(logical_fun(TRUE), FALSE)
  expect_equal(logical_fun(FALSE), TRUE)
  expect_error(
    logical_fun(3L),
    "argument type mismatch.*expected.*logical.*received.*integer"
  )
  expect_error(
    logical_fun(3.0),
    "argument type mismatch.*expected.*logical.*received.*double"
  )
  expect_error(
    logical_fun("a"),
    "argument type mismatch.*expected.*logical.*received.*character"
  )
  expect_error(
    logical_fun(1 - 2i),
    "argument type mismatch.*expected.*logical.*received.*complex"
  )
  expect_error(
    logical_fun(charToRaw("a")),
    "argument type mismatch.*expected.*logical.*received.*raw"
  )

  ## character
  expect_equal(character_fun("a"), "hia")
  expect_error(
    character_fun(3),
    "argument type mismatch.*expected.*character.*received.*double"
  )
  expect_error(
    character_fun(3L),
    "argument type mismatch.*expected.*character.*received.*integer"
  )
  expect_error(
    character_fun(3.0),
    "argument type mismatch.*expected.*character.*received.*double"
  )
  expect_error(
    character_fun(TRUE),
    "argument type mismatch.*expected.*character.*received.*logical"
  )
  expect_error(
    character_fun(1 - 2i),
    "argument type mismatch.*expected.*character.*received.*complex"
  )
  expect_error(
    character_fun(charToRaw("a")),
    "argument type mismatch.*expected.*character.*received.*raw"
  )

  ## complex
  expect_equal(complex_fun(1 + 2i), 1 - 2i)
  expect_error(
    complex_fun(3),
    "argument type mismatch.*expected.*complex.*received.*double"
  )
  expect_error(
    complex_fun(3L),
    "argument type mismatch.*expected.*complex.*received.*integer"
  )
  expect_error(
    complex_fun(3.0),
    "argument type mismatch.*expected.*complex.*received.*double"
  )
  expect_error(
    complex_fun(TRUE),
    "argument type mismatch.*expected.*complex.*received.*logical"
  )
  expect_error(
    complex_fun("a"),
    "argument type mismatch.*expected.*complex.*received.*character"
  )
  expect_error(
    complex_fun(charToRaw("a")),
    "argument type mismatch.*expected.*complex.*received.*raw"
  )

  ## any
  expect_equal(any_fun(1 + 2i), 1 + 2i)
  expect_equal(any_fun(1), 1)
  expect_equal(any_fun(1L), 1L)
  expect_equal(any_fun(1.0), 1.0)
  expect_equal(any_fun("a"), "a")
  expect_equal(any_fun(TRUE), TRUE)

  ## raw
  expect_equal(raw_fun(charToRaw("A")), as.raw(0))
  expect_error(
    raw_fun(1 + 2i),
    "argument type mismatch.*expected.*raw.*received.*complex"
  )
  expect_error(
    raw_fun(3),
    "argument type mismatch.*expected.*raw.*received.*double"
  )
  expect_error(
    raw_fun(3L),
    "argument type mismatch.*expected.*raw.*received.*integer"
  )
  expect_error(
    raw_fun(3.0),
    "argument type mismatch.*expected.*raw.*received.*double"
  )
  expect_error(
    raw_fun(TRUE),
    "argument type mismatch.*expected.*raw.*received.*logical"
  )
  expect_error(
    raw_fun("a"),
    "argument type mismatch.*expected.*raw.*received.*character"
  )
})
