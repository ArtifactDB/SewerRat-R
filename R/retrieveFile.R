#' Retrieve a single file
#'
#' Retrieve the path to a single file in a registered directory. 
#' This will call the REST API to obtain and cache a copy of the file if the caller is not on the same filesystem. 
#'
#' @param path String containing the absolute path to a file in a registered directory.
#' @param forceRemote Logical scalar indicating whether to force remote access.
#' This will download \code{path} via the REST API and cache it locally,
#' even if \code{path} is on the same filesystem as the caller.
#' @inheritParams retrieveDirectory
#'
#' @author Aaron Lun
#' 
#' @return String containing the path to the file on the caller's filesystem.
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
#' retrieveFile(paste0(mydir, "/foo"), url=info$url)
#' retrieveFile(paste0(mydir, "/diet/bar"), url=info$url, forceRemote=TRUE)
#' 
#' @export
retrieveFile <- function(path, url, cache=NULL, forceRemote=FALSE, overwrite=FALSE) {
    if (!forceRemote && file.exists(path)) {
        path
    } else {
        cache <- local_root(cache, url)
        acquire_file_raw(cache, path, url=url, overwrite=overwrite)
    }
}
