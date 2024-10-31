#' Retrieve a directory
#'
#' Retrieve the path to a registered directory (or its subdirectories), 
#' possibly creating a local copy of the directory's contents if the caller is not on the same filesystem.
#'
#' @param path String containing the absolute path to a registered directory.
#' @param url String containing the URL to the SewerRat REST API.
#' @param cache String containing a path to a cache directory.
#' If \code{NULL}, an appropriate location is automatically chosen.
#' Only used for remote access.
#' @param forceRemote Logical scalar indicating whether to force remote access.
#' This will download all files in the \code{path} via the REST API and cache them locally,
#' even if \code{path} is on the same filesystem as the caller.
#' @param overwrite Logical scalar indicating whether to overwrite the existing cache.
#' Only used for remote access.
#' @param concurrent Integer scalar specifying the number of concurrent downloads.
#' Only used for remote access.
#' @param updateDelay Integer scalar specifying the interval before checking for updates in a cached directory, in seconds.
#' Only used for remote access.
#'
#' @return Path to the subdirectory on the caller's filesystem.
#' This is either a path to the registered (sub)directory if it is accessible,
#' or a path to a local cache of the directory's contents otherwise.
#'
#' @details
#' During remote access, this function exhibits the following behavior:
#' \itemize{
#' \item It will only check for updates to the directory contents after \code{updateDelay} seconds have passed since the previous check.
#' This avoids unnecessarily frequent requests to the SewerRat API.
#' \item If a file in \code{path} has already been locally cached, \code{retrieveDirectory} will be automatically check the SewerRat API for updates.
#' Any updates on the remote will cause the new file to be re-downloaded to the cache.
#' \item Any cached files that are no longer in the remote \code{path} will be deleted from the cache.
#' }
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
retrieveDirectory <- function(path, url, cache=NULL, forceRemote=FALSE, overwrite=FALSE, concurrent=1, updateDelay=3600) { 
    if (!forceRemote && file.exists(path)) {
        return(path)
    }

    cache <- local_root(cache, url)
    final <- file.path(cache, "LOCAL", path)
    ok <- file.path(cache, "SUCCESS", path, "....OK")

    if (!overwrite && file.exists(ok) && file.exists(final)) {
        # Only check for updates every 'updateDelay' since the last check, so
        # as to avoid excessive queries to the API.
        if (file.info(ok)$mtime + updateDelay >= Sys.time()) {
            return(final)
        }
    }

    req <- request(paste0(url, "/list?path=", URLencode(path), "&recursive=true"))
    req <- handle_error(req)
    res <- req_perform(req)
    listing <- resp_body_json(res)

    # Removing files that no longer exist.
    existing <- .quick_list(final)
    unlink(file.path(final, setdiff(existing, listing)))

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

#' @import httr2 
#' @importFrom utils URLencode
acquire_file_raw <- function(cache, path, url, overwrite) {
    target <- file.path(cache, "LOCAL", path)
    url <- paste0(url, "/retrieve/file?path=", URLencode(path, reserved=TRUE))

    if (!file.exists(target)) {
        overwrite <- TRUE
    } else if (!overwrite) {
        req <- request(url)
        req <- req_method(req, "HEAD")
        req <- handle_error(req)
        res <- try(req_perform(req), silent=TRUE)

        if (!is(res, "try-error")) { # don't fail if the HEAD didn't work, e.g., no internet but we already have a cached file.
            last_mod <- file.info(target)$mtime
            remote_mod <- parse_remote_last_modified(res)
            if (!is.null(remote_mod) && remote_mod > last_mod) {
                overwrite <- TRUE
            }
        }
    }

    if (overwrite) {
        tempdir <- file.path(cache, "TEMP")
        dir.create(tempdir, recursive=TRUE, showWarnings=FALSE)
        tempf <- tempfile(tmpdir=tempdir)
        on.exit(unlink(tempf), add=TRUE, after=FALSE)

        download_file(url, tempf)
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
