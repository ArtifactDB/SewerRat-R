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
#' @param updateDelay Integer scalar specifying the maximum age of a cached file, in seconds.
#' Older files will be automatically checked for updates.
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

    if (concurrent == 1L) {
        lapply(listing, acquire_file, cache=cache, path=path, url=url, overwrite=overwrite, updateDelay=updateDelay)
    } else {
        cl <- parallel::makeCluster(concurrent)
        on.exit(parallel::stopCluster(cl), add=TRUE, after=FALSE)
        parallel::parLapply(cl, listing, acquire_file, cache=cache, path=path, url=url, overwrite=overwrite, updateDelay=updateDelay)
    }

    # We use a directory-level OK file to avoid having to scan through all 
    # the directory contents to indicate that it's complete.
    dir.create(dirname(ok), showWarnings=FALSE, recursive=TRUE)
    write(file=ok, character(0))
    final
}

full_file_url <- function(url, path) {
    paste0(url, "/retrieve/file?path=", URLencode(path, reserved=TRUE))
}

get_remote_last_modified <- function(url, path) {
    req <- request(full_file_url(url, path))
    req <- req_method(req, "HEAD")
    req <- handle_error(req)
    res <- req_perform(req)
    remote_mod <- resp_header(res, "last-modified")

    if (is.null(remote_mod)) {
        warning("failed to find 'last-modified' header from the SewerRat API")
        return(NULL)
    }

    remote_mod <- as.POSIXct(remote_mod, format="%a, %d %b %Y %H:%M:%S", tz="GMT")
    if (is.na(remote_mod)) {
        warning("invalid 'last-modified' header from the SewerRat API")
        return(NULL)
    }

    return(remote_mod) 
}

#' @import httr2 
#' @importFrom utils download.file
acquire_file_raw <- function(cache, path, url, overwrite, updateDelay) {
    target <- file.path(cache, "LOCAL", path)

    if (!file.exists(target)) {
        overwrite <- TRUE
    } else if (!overwrite) {
        last_mod <- file.info(target)$mtime
        if (last_mod + updateDelay < Sys.time()) { # only check older files for updates, to avoid excessive queries.
            remote_mod <- get_remote_last_modified(url, path)
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

        if (download.file(full_file_url(url, path), tempf)) {
            stop("failed to download '", path, "'")
        }
        dir.create(dirname(target), recursive=TRUE, showWarnings=FALSE)
        file.rename(tempf, target) # this should be more or less atomic, so no need for locks.
    }

    target
}

acquire_file <- function(cache, path, name, url, overwrite, updateDelay) {
    acquire_file_raw(cache, paste0(path, "/", name), url, overwrite, updateDelay)
}

#' @importFrom utils URLencode
local_root <- function(cache, url) {
    if (is.null(cache)) {
        cache <- tools::R_user_dir("SewerRat", "data")
    }
    file.path(cache, URLencode(url, reserved=TRUE))
}
