#' List registered directories
#'
#' List the directories that were registered in SewerRat.
#'
#' @param url String containing the URL of the SewerRat REST API.
#' @param user String containing the name of a user.
#' If supplied, results are filtered to directories registered by this user.
#' If \code{TRUE}, this is set to the current user.
#'
#' @author Aaron Lun
#'
#' @return List of named lists, where each inner list corresponds to a registered directory and contains:
#' \itemize{
#' \item \code{path}, string containing a path to the directory.
#' \item \code{user}, string containing the name of the user who registered this directory.
#' \item \code{time}, numeric scalar specifying the the Unix epoch time when this directory was registered.
#' \item \code{names}, a character vector containing the base names of the JSON files to be indexed.
#' }
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
#' # List the registered directories:
#' listRegisteredDirectories(url=info$url)
#' listRegisteredDirectories(url=info$url, user=TRUE)
#' 
#' @export
#' @import httr2
listRegisteredDirectories <- function(url, user=NULL) {
    url <- paste0(url, "/registered")

    if (isTRUE(user)) {
        user <- Sys.info()["user"]
    }
    if (!is.null(user) && !isFALSE(user)) {
        url <- paste0(url, "?user=", user)
    }
    req <- request(url)
    req <- handle_error(req)
    res <- req_perform(req)
    resp_body_json(res)
}

