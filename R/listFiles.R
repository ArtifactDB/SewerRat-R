#' List files in a registered directory
#'
#' List files in a registered directory or a subdirectory thereof.
#' This will call the REST API if the caller is not on the same filesystem. 
#'
#' @param path String containing the path to the registered (sub)directory.
#' @param url String containing the URL of the SewerRat REST API.
#' @param recursive Logical scalar indicating whether to list files recursively.
#' @param forceRemote Logical scalar indicating whether to force remote access,
#' even if \code{path} is on the same filesystem as the caller.
#'
#' @author Aaron Lun
#'
#' @return Character vector of relative paths of files in \code{path}.
#'
#' @examples
#' # Starting up an example SewerRat service:
#' info <- startSewerRat()
#'
#' # Mocking up a directory of stuff to query.
#' mydir <- tempfile()
#' dir.create(mydir)
#' write(file=file.path(mydir, "metadata.json"), '{ "first": "Aaron", "last": "Lun" }')
#' dir.create(file.path(mydir, "diet"))
#' write(file=file.path(mydir, "diet", "metadata.json"), 
#'    '{ "meal": "lunch", "ingredients": "water" }')
#'
#' # Registering it:
#' register(mydir, "metadata.json", url=info$url)
#'
#' # List files:
#' listFiles(mydir, url=info$url)
#' listFiles(paste0(mydir, "/diet"), url=info$url)
#'
#' # Forcing remote access.
#' listFiles(mydir, url=info$url, forceRemote=TRUE)
#' 
#' @export
#' @import httr2
listFiles <- function(path, url, recursive=TRUE, forceRemote=FALSE) {
    if (!forceRemote && file.exists(path)) {
        if (recursive) {
            list.files(path, recursive=TRUE, all.files=TRUE)
        } else {
            current <- list.files(path, all.files=TRUE, no..=TRUE)
            dirs <- list.dirs(path, recursive=TRUE, full.names=FALSE)
            is.dir <- current %in% dirs
            current[is.dir] <- paste0(current[is.dir], "/")
            current
        }
    } else {
        req <- request(paste0(url, "/list?path=", URLencode(path, reserved=TRUE), "&recursive=", if (recursive) "true" else "false"))
        req <- handle_error(req)
        res <- req_perform(req)
        unlist(resp_body_json(res))
    }
}
