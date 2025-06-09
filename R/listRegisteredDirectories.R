#' List registered directories
#'
#' List the directories that were registered in SewerRat.
#'
#' @inheritParams query
#' @param user String containing the name of a user.
#' If not \code{NULL}, results are filtered to directories registered by this user.
#' If \code{TRUE}, this is set to the current user.
#' @param contains String containing an absolute path.
#' If not \code{NULL}, results are filtered to directories that contain this path.
#' @param within String containing an absolute path.
#' If not \code{NULL}, results are filtered to directories that are equal to or lie within this path.
#' @param prefix String containing an absolute path or a prefix thereof.
#' If not \code{NULL}, results are filtered to directories that start with this string.
#' This is soft-deprecated and users should prefer to use \code{within}.
#' @param exists Logical scalar indicating whether to only report directories that exist on the filesystem.
#' If \code{FALSE}, only non-existent directories are reported, and if \code{NULL}, no filtering is applied based on existence.
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
listRegisteredDirectories <- function(url, user=NULL, contains=NULL, prefix=NULL, within=NULL, exists=NULL, number=100, on.truncation=c("message", "warning", "none")) {
    query <- character(0)
    if (!is.null(user) && !isFALSE(user)) {
        if (isTRUE(user)) {
            user <- Sys.info()["user"]
        }
        query <- c(query, paste0("user=", user))
    }
    if (!is.null(contains)) {
        query <- c(query, paste0("contains_path=", URLencode(contains, reserved=TRUE)))
    }
    if (!is.null(within)) {
        query <- c(query, paste0("within_path=", URLencode(within, reserved=TRUE)))
    }
    if (!is.null(prefix)) {
        query <- c(query, paste0("path_prefix=", URLencode(prefix, reserved=TRUE)))
    }
    if (!is.null(exists)) {
        query <- c(query, paste0("exists=", tolower(exists)))
    }

    on.truncation <- match.arg(on.truncation)
    original.number <- number
    if (on.truncation != "none") {
        number <- number + 1L
    }

    stub <- "/registered"
    use.question <- TRUE
    if (length(query)) {
        stub <- paste0(stub, "?", paste(query, collapse="&"))
        use.question <- FALSE
    }

    collected <- list()
    while (length(collected) < number) {
        current.url <- paste0(url, stub)
        if (!is.infinite(number)) {
            sep <- if (use.question) "?" else "&"
            current.url <- paste0(current.url, sep, "limit=", number - length(collected))
        }

        req <- request(current.url)
        req <- handle_error(req)
        res <- req_perform(req)
        payload <- resp_body_json(res)
        collected <- c(collected, payload$results)

        stub <- payload$`next`
        if (is.null(stub)) {
            break
        }
        use.question <- FALSE
    }

    handle_truncated_pages(on.truncation, original.number, collected)
}
