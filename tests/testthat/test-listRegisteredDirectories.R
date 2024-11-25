# library(testthat); library(SewerRat); source("setup.R"); source("test-listRegisteredDirectories.R")

(function(){

config <- basic_config()
info <- config$info
mydir <- config$mydir
on.exit(deregister(mydir, url=info$url), add=TRUE, after=FALSE)

test_that("listRegisteredDirectories works as expected", {
    all <- listRegisteredDirectories(info$url)
    expect_true(length(all) > 0L)

    found <- FALSE
    for (x in all) {
        if (normalizePath(x$path) == normalizePath(mydir)) {
            found <- TRUE
            expect_identical(x$names, list("metadata.json"))
        }
    }
    expect_true(found)

    # Filtering by user.
    filtered <- listRegisteredDirectories(info$url, user=TRUE)
    expect_identical(all, filtered)

    filtered <- listRegisteredDirectories(info$url, user=paste0(all[[1]]$user, "asdasdasdasdasd"))
    expect_identical(length(filtered), 0L)

    # Filter by contains.
    filtered <- listRegisteredDirectories(info$url, contains=file.path(mydir, "diet"))
    expect_identical(all, filtered)

    filtered <- listRegisteredDirectories(info$url, contains=tempfile())
    expect_identical(length(filtered), 0L)

    # Filter by prefix.
    filtered <- listRegisteredDirectories(info$url, prefix=dirname(mydir))
    expect_identical(all, filtered)

    filtered <- listRegisteredDirectories(info$url, prefix=paste0(dirname(mydir), "-asdasdad"))
    expect_identical(length(filtered), 0L)

    # Multiple filters work.
    filtered <- listRegisteredDirectories(info$url, prefix=dirname(mydir), user=TRUE, contains=file.path(mydir, "diet"))
    expect_identical(all, filtered)
})

})()
