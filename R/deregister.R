#' Deregister a directory from the index
#'
#' Remove a directory on the shared filesystem from the search index.
#' This assumes that either the directory does not exist,
#' or that the directory is world-readable and the caller has write access to it.
#'
#' @param dir String containing the path to the directory to be deregistered.
#' This may be a relative or absolute path.
#' @inheritParams register
#'
#' @return On success, the directory is deregistered and NULL is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @examples
#' # Start up an example SewerRat service:
#' info <- startSewerRat()
#'
#' tmp <- tempfile()
#' dir.create(tmp)
#' write(file=file.path(tmp, "whee.json"), '{ "foo": "bar" }')
#' register(tmp, "whee.json", url=info$url)
#' query("bar", url=info$url)
#'
#' # After deregistration, the files cannot be queried.
#' deregister(tmp, url=info$url)
#' query("bar", url=info$url)
#' @export
#' @import httr2
deregister <- function(dir, url, wait=1) {
    dir <- clean_path(dir)

    req <- request(paste0(url, "/deregister/start"))
    req <- req_method(req, "POST")
    req <- req_body_json(req, list(path=dir))
    req <- redirect_post(req)
    req <- handle_error(req)
    res <- req_perform(req)

    payload <- resp_body_json(res)
    if (payload$status == "PENDING") {
        target <- file.path(dir, payload$code)
        write(file=target, character(0))
        on.exit(unlink(target))

        Sys.sleep(wait) # some leeway to allow slow network shares to sync.

        req <- request(paste0(url, "/deregister/finish"))
        req <- req_method(req, "POST")
        req <- req_body_json(req, list(path=dir))
        req <- redirect_post(req)
        req <- handle_error(req)
        res <- req_perform(req)
    }

    invisible(NULL)
}
