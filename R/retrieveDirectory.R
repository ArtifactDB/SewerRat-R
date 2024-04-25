#' Retrieve a directory
#'
#' Retrieve the path to a registered directory (or its subdirectories), 
#' possibly creating a local copy of the directory's contents if the caller is not on the same filesystem.
#'
#' @param path String containing the absolute path to a directory.
#' @param url String containing the URL to the SewerRat REST API.
#' @param cache String containing a path to a cache directory.
#' If \code{NULL}, an appropriate location is automatically chosen.
#' Only used for remote access.
#' @param forceRemote Logical scalar indicating whether to force remote access.
#' This will download all files in the \code{path} via the REST API and cache them locally,
#' even if \code{path} is on the same filesystem as the caller.
#' @param overwrite Logical scalar indicating whether to overwrite the existing cache.
#' Only used for remote access.
#' @param concurrent Integer specifying the number of concurrent downloads.
#' Only used for remote access.
#'
#' @return Path to the subdirectory on the caller's filesystem.
#' This is either a path to the registered (sub)directory if it is accessible,
#' or a path to a local cache of the directory's contents otherwise.
#'
#' @author Aaron Lun
#' 
#' @examples
#' info <- startSewerRat()
#'
#' # Mocking up a directory of stuff to query.
#' mydir <- tempfile()
#' dir.create(mydir)
#' write(file=file.path(mydir, "foo"), '{ "first": "Aaron", "last": "Lun" }')
#' dir.create(file.path(mydir, "diet"))
#' write(file=file.path(mydir, "diet", "bar"), 
#'    '{ "meal": "lunch", "ingredients": "water" }')
#'
#' # Registering it:
#' register(mydir, "metadata.json", url=info$url)
#'
#' # Fetching it, either directly or via the API.
#' retrieveDirectory(mydir, url=info$url)
#' retrieveDirectory(paste0(mydir, "/diet"), url=info$url, forceRemote=TRUE)
#' 
#' @export
#' @import httr2
retrieveDirectory <- function(path, url, cache=NULL, forceRemote=FALSE, overwrite=FALSE, concurrent=1) {
    if (!forceRemote && file.exists(path)) {
        return(path)
    }

    cache <- local_root(cache, url)
    final <- file.path(cache, "LOCAL", path)
    ok <- file.path(cache, "SUCCESS", path, "....OK")
    if (!overwrite && file.exists(ok) && file.exists(final)) {
        return(final)
    }

    req <- request(paste0(url, "/list?path=", URLencode(path), "&recursive=true"))
    req <- handle_error(req)
    res <- req_perform(req)
    listing <- resp_body_json(res)

    if (concurrent == 1L) {
        lapply(listing, acquire_file, cache=cache, path=path, url=url, overwrite=overwrite)
    } else {
        cl <- parallel::makeCluster(concurrent)
        on.exit(parallel::stopCluster(cl), add=TRUE, after=FALSE)
        parallel::parLapply(cl, listing, acquire_file, cache=cache, path=path, url=url, overwrite=overwrite)
    }

    # We use a directory-level OK file to avoid having to scan through all 
    # the directory contents to indicate that it's complete.
    dir.create(dirname(ok), showWarnings=FALSE, recursive=TRUE)
    write(file=ok, character(0))
    final
}

#' @importFrom utils download.file
acquire_file_raw <- function(cache, path, url, overwrite) {
    target <- file.path(cache, "LOCAL", path)

    if (overwrite || !file.exists(target)) {
        tempdir <- file.path(cache, "TEMP")
        dir.create(tempdir, recursive=TRUE, showWarnings=FALSE)
        tempf <- tempfile(tmpdir=tempdir)
        on.exit(unlink(tempf), add=TRUE, after=FALSE)

        if (download.file(paste0(url, "/retrieve/file?path=", URLencode(path, reserved=TRUE)), tempf)) {
            stop("failed to download '", path, "'")
        }
        dir.create(dirname(target), recursive=TRUE, showWarnings=FALSE)
        file.rename(tempf, target) # this should be more or less atomic, so no need for locks.
    }

    target
}

acquire_file <- function(cache, path, name, url, overwrite) {
    acquire_file_raw(cache, paste0(path, "/", name), url, overwrite)
}

#' @importFrom utils URLencode
local_root <- function(cache, url) {
    if (is.null(cache)) {
        cache <- tools::R_user_dir("SewerRat", "data")
    }
    file.path(cache, URLencode(url, reserved=TRUE))
}
