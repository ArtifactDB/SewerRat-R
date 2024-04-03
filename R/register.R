#' Register a directory for indexing
#'
#' Register a directory on a shared filesystem for inclusion into the search index.
#' This assumes that the directory is world-readable and you have write access to it.
#'
#' @param dir String containing the path to the directory to be registered.
#' @param names Character vector containing the base names of the metadata files, e.g., \code{metadata.json}.
#' @param url String containing the URL to the SewerRat REST API.
#'
#' @return On success, the directory is registered and NULL is invisibly returned.
#' A warning is raised if any particular metadata file cannot be indexed.
#'
#' @author Aaron Lun
#'
#' @examples
#' if (interactive()) {
#'      register(paste0("/gstore/user/scratch/", Sys.user()), "metadata.json")
#' }
#' @export
#' @import httr2
register <- function(dir, names, url=restUrl()) {
    dir <- normalizePath(dir, mustWork=TRUE)
    stopifnot(length(names) > 0)

    req <- request(paste0(url, "/register/start"))
    req <- req_method(req, "POST")
    req <- req_body_json(req, list(path=dir))
    req <- req_error(req, body = function(res) resp_body_json(res)$reason)
    res <- req_perform(req)

    payload <- resp_body_json(res)
    target <- file.path(dir, payload$code)
    write(file=target, character(0))
    on.exit(unlink(target))

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
