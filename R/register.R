#' Register a directory for indexing
#'
#' Register a directory on a shared filesystem for inclusion into the search index.
#' This assumes that the directory is world-readable and the caller has write access to it.
#'
#' @param dir String containing the path to the directory to be registered.
#' This may be a relative or absolute path.
#' @param names Character vector containing the base names of the metadata files.
#' @param url String containing the URL to the SewerRat REST API.
#' @param retry,wait Deprecated arguments, ignored.
#' @param block Logical scalar indicating whether to block on successful registration.
#'
#' @return On success, the directory is registered and NULL is invisibly returned.
#' A warning is raised if any particular metadata file cannot be indexed.
#'
#' If \code{block=FALSE}, the function returns before confirmation of successful registration from the SewerRat API.
#' This can be useful for asynchronous processing of directories with many files. 
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
register <- function(dir, names, url, retry=3, wait=1, block=TRUE) {
    dir <- clean_path(dir)
    stopifnot(length(names) > 0)

    req <- request(paste0(url, "/register/start"))
    req <- req_method(req, "POST")
    req <- req_body_json(req, list(path=dir))
    req <- redirect_post(req)
    req <- handle_error(req)
    res <- req_perform(req)

    payload <- resp_body_json(res)
    target <- file.path(dir, payload$code)
    write(file=target, character(0))
    on.exit(unlink(target))

    req <- request(paste0(url, "/register/finish"))
    req <- req_method(req, "POST")
    req <- req_body_json(req, list(path=dir, base=as.list(names), block=block))
    req <- redirect_post(req)
    req <- handle_error(req)
    res <- req_perform(req)

    payload <- resp_body_json(res)
    for (w in payload$comments) {
        warning(w)
    }

    invisible(NULL)
}
