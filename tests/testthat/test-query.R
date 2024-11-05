# library(testthat); library(SewerRat); source("test-query.R")

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

test_that("formatQueryResults works properly", {
    q <- query("lun*", url=info$url)
    res <- formatQueryResults(q)

    expect_gte(nrow(res), 2L)
    expect_identical(nrow(res), length(q))
    expect_equal(as.double(res$time), vapply(q, function(y) y$time, 0))
    expect_identical(res$metadata[[1]], q[[1]]$metadata)
})
