# library(testthat); library(SewerRat); source("setup.R"); source("test-register.R")

(function(){

config <- basic_config()
info <- config$info
mydir <- config$mydir
on.exit(deregister(mydir, url=info$url), add=TRUE, after=FALSE)

test_that("(de)registration works as expected", {
    res <- query("aaron", url=info$url)
    expect_identical(length(res), 1L)

    deregister(mydir, url=info$url)
    res <- query("aaron", url=info$url)
    expect_identical(length(res), 0L)
})

})()
