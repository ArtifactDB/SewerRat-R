# Test the retrieval functions.
# library(testthat); library(SewerRat); source("test-retrieve.R")

info <- startSewerRat()
                                                                                     
mydir <- tempfile()
dir.create(mydir)
write(file=file.path(mydir, "metadata.json"), '{ "first": "Aaron", "last": "Lun" }')
dir.create(file.path(mydir, "diet"))
write(file=file.path(mydir, "diet", "metadata.json"), '{ "meal": "lunch", "ingredients": "water" }')
register(mydir, "metadata.json", url=info$url)

test_that("retrieveFile works as expected", {
    p <- retrieveFile(paste0(mydir, "/metadata.json"), url=info$url)
    expect_identical(jsonlite::fromJSON(p, simplifyVector=FALSE)$first, "Aaron")

    cache <- tempfile()
    p <- retrieveFile(paste0(mydir, "/metadata.json"), url=info$url, cache=cache, forceRemote=TRUE)
    expect_true(startsWith(p, cache))
    expect_identical(jsonlite::fromJSON(p, simplifyVector=FALSE)$first, "Aaron")

    # Let's try overwriting the cached file and confirming that the cache is unchanged: 
    write(file=p, "AARON WAS HERE")
    p2 <- retrieveFile(paste0(mydir, "/metadata.json"), url=info$url, cache=cache, forceRemote=TRUE)
    expect_identical(p, p2)
    expect_identical(readLines(p), "AARON WAS HERE")

    # Unless we force an overwrite:
    p2 <- retrieveFile(paste0(mydir, "/metadata.json"), url=info$url, cache=cache, forceRemote=TRUE, overwrite=TRUE)
    expect_identical(p, p2)
    expect_false(identical(readLines(p), "AARON WAS HERE"))

    # Or unless the cached file is out of date...
    write(file=p, "AARON WAS HERE")
    Sys.setFileTime(p, Sys.time() - 3601)
    p2 <- retrieveFile(paste0(mydir, "/metadata.json"), url=info$url, cache=cache, forceRemote=TRUE)
    expect_identical(p, p2)
    expect_false(identical(readLines(p), "AARON WAS HERE"))
})

test_that("retrieveMetadata works as expected", {
    fpath <- paste0(mydir, "/diet/metadata.json")
    meta <- retrieveMetadata(fpath, url=info$url)
    expect_identical(normalizePath(meta$path), normalizePath(fpath))
    expect_identical(meta$metadata$meal, "lunch")
})

test_that("retrieveDirectory works as expected", {
    dir <- retrieveDirectory(mydir, url=info$url)
    expect_identical(jsonlite::fromJSON(file.path(dir, "metadata.json"))$first, "Aaron")

    cache <- tempfile()
    subpath <- paste0(mydir, "/diet")
    rdir <- retrieveDirectory(subpath, url=info$url, cache=cache, forceRemote=TRUE)
    expect_true(startsWith(rdir, cache))
    expect_identical(jsonlite::fromJSON(file.path(rdir, "metadata.json"))$meal, "lunch")

    # Subsequent requests are no-ops.
    write(file=file.path(rdir, "metadata.json"), '{ "meal": "dinner" }')
    rdir2 <- retrieveDirectory(subpath, url=info$url, cache=cache, forceRemote=TRUE)
    expect_identical(rdir, rdir2)
    expect_identical(jsonlite::fromJSON(file.path(rdir2, "metadata.json"))$meal, "dinner")

    # Unless we force an overwrite.
    rdir2 <- retrieveDirectory(subpath, url=info$url, cache=cache, forceRemote=TRUE, overwrite=TRUE)
    expect_identical(rdir, rdir2)
    expect_identical(jsonlite::fromJSON(file.path(rdir2, "metadata.json"))$meal, "lunch")

    # Or unless the cached directory AND file is out of date...
    write(file=file.path(rdir, "metadata.json"), '{ "meal": "dinner" }')
    Sys.setFileTime(file.path(rdir, "metadata.json"), Sys.time() - 3601)
    Sys.setFileTime(file.path(SewerRat:::local_root(cache, info$url), "SUCCESS", subpath, "....OK"), Sys.time() - 3601)
    rdir2 <- retrieveDirectory(subpath, url=info$url, cache=cache, forceRemote=TRUE)
    expect_identical(rdir, rdir2)
    expect_identical(jsonlite::fromJSON(file.path(rdir2, "metadata.json"))$meal, "lunch")

    # Trying with multiple cores.
    cache <- tempfile()
    rdir2 <- retrieveDirectory(mydir, url=info$url, cache=cache, forceRemote=TRUE, concurrent=2)
    expect_identical(jsonlite::fromJSON(file.path(rdir2, "diet", "metadata.json"))$meal, "lunch")
})

deregister(mydir, url=info$url)
