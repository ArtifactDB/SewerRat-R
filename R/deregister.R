#' Deregister a directory from the index
#'
#' Remove a directory on the shared filesystem from the search index.
#' This assumes that either the directory does not exist,
#' or that the directory is world-readable and you have write access to it.
#'
#' @param dir String containing the path to the directory to be registered.
#' @param url String containing the URL to the SewerRat REST API.
#'
#' @return On success, the directory is registered and NULL is invisibly returned.
#' A warning is raised if any particular metadata file cannot be indexed.
#'
#' @author Aaron Lun
#'
#' @examples
#' # Start up an example SewerRat service:
#' startSewerRat()
#'
#' tmp <- tempfile()
#' dir.create(tmp)
#' write(file=file.path(tmp, "whee.json"), '{ "foo": "bar" }')
#' register(tmp, "whee.json")
#' query("bar")
#'
#' # After deregistration, the files cannot be queried.
#' deregister(tmp)
#' query("bar")
#'
#' # Okay, stop the service:
#' stopSewerRat()
#' @export
#' @import httr2
deregister <- function(dir, url=restUrl()) {
    dir <- normalizePath(dir, mustWork=TRUE)

    req <- request(paste0(url, "/deregister/start"))
    req <- req_method(req, "POST")
    req <- req_body_json(req, list(path=dir))
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)
    res <- req_perform(req)

    payload <- resp_body_json(res)
    if (payload$status == "PENDING") {
        target <- file.path(dir, payload$code)
        write(file=target, character(0))
        on.exit(unlink(target))
        Sys.sleep(1) # some leeway to allow slow network shares to sync.

        req <- request(paste0(url, "/deregister/finish"))
        req <- req_method(req, "POST")
        req <- req_body_json(req, list(path=dir))
        req <- req_error(req, body = function(res) resp_body_json(res)$reason)
        res <- req_perform(req)
    }

    invisible(NULL)
}
