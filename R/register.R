#' Register a directory for indexing
#'
#' Register a directory on a shared filesystem for inclusion into the search index.
#' This assumes that the directory is world-readable and you have write access to it.
#'
#' @param dir String containing the path to the directory to be registered.
#' @param names Character vector containing the base names of the metadata files.
#' @param url String containing the URL to the SewerRat REST API.
#' @param wait Numeric scalar specifying the number of seconds to wait for the filesystem to synchronize.
#'
#' @return On success, the directory is registered and NULL is invisibly returned.
#' A warning is raised if any particular metadata file cannot be indexed.
#'
#' @author Aaron Lun
#'
#' @examples
#' # Start up an example SewerRat service:
#' info <- startSewerRat()
#'
#' # Mocking up a directory:
#' tmp <- tempfile()
#' dir.create(tmp)
#' write(file=file.path(tmp, "metadata.json"), '{ "name": "Aaron" }')
#'
#' # Once we register it, we can query its contents.
#' register(tmp, "metadata.json", url=info$url)
#' query("Aaron", url=info$url)
#' @export
#' @import httr2
register <- function(dir, names, url, wait=1) {
    dir <- normalizePath(dir, mustWork=TRUE)
    stopifnot(length(names) > 0)

    req <- request(paste0(url, "/register/start"))
    req <- req_method(req, "POST")
    req <- req_body_json(req, list(path=dir))
    req <- handle_error(req)
    res <- req_perform(req)

    payload <- resp_body_json(res)
    target <- file.path(dir, payload$code)
    write(file=target, character(0))
    on.exit(unlink(target))

    Sys.sleep(wait) # some leeway to allow slow network shares to sync.

    req <- request(paste0(url, "/register/finish"))
    req <- req_method(req, "POST")
    req <- req_body_json(req, list(path=dir, base=as.list(names)))
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)
    res <- req_perform(req)

    payload <- resp_body_json(res)
    for (w in payload$comments) {
        warning(w)
    }

    invisible(NULL)
}
