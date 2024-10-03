# Test the listing function.
# library(testthat); library(SewerRat); source("test-list.R")

# Starting up an example SewerRat service:
info <- startSewerRat()

# Mocking up a directory of stuff to query.
mydir <- tempfile()
dir.create(mydir)
write(file=file.path(mydir, "metadata.json"), '{ "first": "Aaron", "last": "Lun" }')
dir.create(file.path(mydir, "diet"))
write(file=file.path(mydir, "diet", "metadata.json"), 
   '{ "meal": "lunch", "ingredients": "water" }')

# Registering it:
register(mydir, "metadata.json", url=info$url)

test_that("listing works as expected", {
    expect_identical(sort(listFiles(mydir, url=info$url)), sort(c("diet/metadata.json", "metadata.json")))
    expect_identical(sort(listFiles(paste0(mydir, "/diet"), url=info$url)), "metadata.json")

    # Forcing remote access.
    expect_identical(sort(listFiles(mydir, url=info$url, forceRemote=TRUE)), sort(c("diet/metadata.json", "metadata.json")))
})

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

    # Forcing filtering.
    filtered <- listRegisteredDirectories(info$url, user=TRUE)
    expect_identical(all, filtered)

    filtered <- listRegisteredDirectories(info$url, user=paste0(all[[1]]$user, "asdasdasdasdasd"))
    expect_identical(length(filtered), 0L)
})

deregister(mydir, url=info$url)
