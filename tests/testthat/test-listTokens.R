# library(testthat); library(SewerRat); source("setup.R"); source("test-listTokens.R")

(function() {

config <- basic_config()
info <- config$info
mydir <- config$mydir
on.exit(deregister(mydir, url=info$url), add=TRUE, after=FALSE)

test_that("listTokens works as expected", {
    all <- listTokens(info$url)
    expect_identical(sort(vapply(all, FUN=function(x) x$token, "")), c("aaron", "lun", "lunch", "water"))

    all <- listTokens(info$url, pattern="lun*")
    expect_identical(sort(vapply(all, FUN=function(x) x$token, "")), c("lun", "lunch"))

    all <- listTokens(info$url, field="first")
    expect_identical(sort(vapply(all, FUN=function(x) x$token, "")), c("aaron"))

    all <- listTokens(info$url, count=TRUE)
    expect_identical(sort(vapply(all, FUN=function(x) x$token, "")), c("aaron", "lun", "lunch", "water"))
    expect_identical(vapply(all, FUN=function(x) x$count, 1L), rep(1L, length(all)))
})

test_that("truncated listings work", {
    expect_message(res <- listTokens(info$url, number=0), "truncated")
    expect_identical(length(res), 0L)

    expect_warning(res <- listTokens(info$url, number=0, on.truncation="warning"), "truncated")
    expect_identical(length(res), 0L)

    expect_message(res <- listTokens(info$url, number=Inf, on.truncation="none"), NA)
    expect_gt(length(res), 0L)

    res <- listTokens(info$url, number=Inf)
    expect_gt(length(res), 0L)
})

})()
