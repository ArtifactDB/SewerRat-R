# library(testthat); library(SewerRat); source("setup.R"); source("test-query.R")

(function(){

config <- basic_config()
info <- config$info
mydir <- config$mydir
on.exit(deregister(mydir, url=info$url), add=TRUE, after=FALSE)

test_that("basic queries work", {
    q <- query("aaron", url=info$url)
    expect_identical(length(q), 1L)

    q <- query("lun*", url=info$url)
    expect_identical(length(q), 2L)

    expect_message(q <- query("lun*", url=info$url, number=Inf), NA) # no warnings when number=Inf.
    expect_identical(length(q), 2L)
})

test_that("truncated queries work", {
    expect_message(q <- query("lun*", url=info$url, number=0), "truncated")
    expect_identical(length(q), 0L)

    expect_warning(q <- query("lun*", url=info$url, number=0, on.truncation="warning"), "truncated")
    expect_identical(length(q), 0L)

    expect_warning(q <- query("lun*", url=info$url, number=0, on.truncation="none"), NA)
    expect_identical(length(q), 0L)
})

test_that("formatting of query results works properly", {
    q <- query("lun*", url=info$url)
    res <- formatQueryResults(q)

    expect_gte(nrow(res), 2L)
    expect_identical(nrow(res), length(q))
    expect_equal(as.double(res$time), vapply(q, function(y) y$time, 0))
    expect_identical(res$metadata[[1]], q[[1]]$metadata)
})

})()
