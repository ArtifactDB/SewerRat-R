# library(testthat); library(SewerRat); source("setup.R"); source("test-listFiles.R")

(function() {

config <- basic_config()
info <- config$info
mydir <- config$mydir
on.exit(deregister(mydir, url=info$url), add=TRUE, after=FALSE)

test_that("listFiles works as expected", {
    all <- listFiles(mydir, info$url)
    expect_identical(sort(all), c("diet/metadata.json", "metadata.json"))

    all <- listFiles(mydir, info$url, recursive=FALSE)
    expect_identical(sort(all), c("diet/", "metadata.json"))

    all <- listFiles(paste0(mydir, "/diet"), info$url)
    expect_identical(sort(all), "metadata.json")

    all <- listFiles(mydir, info$url, forceRemote=TRUE)
    expect_identical(sort(all), c("diet/metadata.json", "metadata.json"))
})

})()
