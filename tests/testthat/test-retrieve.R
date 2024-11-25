# library(testthat); library(SewerRat); source("setup.R"); source("test-retrieve.R")

(function(){

config <- basic_config()
info <- config$info
mydir <- config$mydir
on.exit(deregister(mydir, url=info$url), add=TRUE, after=FALSE)

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

test_that("retrieveDirectory works with remote updates", {
    mydir2 <- tempfile()
    dir.create(mydir2)
    write(file=file.path(mydir2, "metadata.json"), '{ "first": "Kanon", "last": "Shibuya" }')
    dir.create(file.path(mydir2, "2"))
    write(file=file.path(mydir2, "2", "metadata.json"), '{ "first": "Kinako", "last": "Sakurakouji" }')
    dir.create(file.path(mydir2, "3"))
    write(file=file.path(mydir2, "3", "metadata.json"), '{ "first": "Margarete", "last": "Wien" }')

    register(mydir2, "metadata.json", url=info$url)
    on.exit(deregister(mydir2, url=info$url))

    cache <- tempfile()
    dir <- retrieveDirectory(mydir2, url=info$url, cache=cache, forceRemote=TRUE)
    expect_identical(jsonlite::fromJSON(file.path(dir, "2", "metadata.json"))$first, "Kinako")
    expect_true(file.exists(file.path(dir, "3", "metadata.json")))

    # Checking if it responds correctly to remote updates.
    Sys.sleep(1.5)
    unlink(file.path(mydir2, "3"), recursive=TRUE)
    write(file=file.path(mydir2, "2", "metadata.json"), '{ "first": "Mei", "last": "Yoneme" }')
    Sys.sleep(1.5)

    dir2 <- retrieveDirectory(mydir2, url=info$url, cache=cache, forceRemote=TRUE, updateDelay=0)
    expect_identical(dir, dir2)
    expect_identical(jsonlite::fromJSON(file.path(dir, "2", "metadata.json"))$first, "Mei")
    expect_false(file.exists(file.path(dir, "3", "metadata.json")))
})

})()
