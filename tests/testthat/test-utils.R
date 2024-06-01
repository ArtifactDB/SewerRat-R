# Test the utility functions.
# library(testthat); library(SewerRat); source("test-utils.R")

test_that("clean_path works as expected", {
    x <- SewerRat:::clean_path("foo/bar")
    expect_true(endsWith(x, "/foo/bar"))
    expect_true(startsWith(x, "/"))

    x <- SewerRat:::clean_path("//absd//a//foo/bar")
    expect_identical(x, "/absd/a/foo/bar")

    x <- SewerRat:::clean_path("//absd//a//../foo/bar")
    expect_identical(x, "/absd/foo/bar")

    x <- SewerRat:::clean_path("/xxxx/bbb/../../foo/bar")
    expect_identical(x, "/foo/bar")

    x <- SewerRat:::clean_path("/../absd")
    expect_identical(x, "/absd")

    x <- SewerRat:::clean_path("/absd/./bar/./")
    expect_identical(x, "/absd/bar")

    x <- SewerRat:::clean_path("/a/b/c/d/")
    expect_identical(x, "/a/b/c/d")
})
