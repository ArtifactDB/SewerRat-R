# library(testthat); library(SewerRat); source("setup.R"); source("test-listFields.R")

(function() {

config <- basic_config()
info <- config$info
mydir <- config$mydir
on.exit(deregister(mydir, url=info$url), add=TRUE, after=FALSE)

test_that("listFields works as expected", {
    all <- listFields(info$url)
    expect_identical(sort(vapply(all, FUN=function(x) x$field, "")), c("first", "ingredients", "last", "meal"))

    all <- listFields(info$url, pattern="fir*")
    expect_identical(sort(vapply(all, FUN=function(x) x$field, "")), c("first"))

    all <- listFields(info$url, count=TRUE)
    expect_identical(sort(vapply(all, FUN=function(x) x$field, "")), c("first", "ingredients", "last", "meal"))
    expect_identical(vapply(all, FUN=function(x) x$count, 1L), rep(1L, length(all)))
})

test_that("truncated listings work", {
    expect_message(res <- listFields(info$url, number=0), "truncated")
    expect_identical(length(res), 0L)

    expect_warning(res <- listFields(info$url, number=0, on.truncation="warning"), "truncated")
    expect_identical(length(res), 0L)

    expect_message(res <- listFields(info$url, number=Inf, on.truncation="none"), NA)
    expect_gt(length(res), 0L)

    res <- listFields(info$url, number=Inf)
    expect_gt(length(res), 0L)
})

})()
