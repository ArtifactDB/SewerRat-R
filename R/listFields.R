#' List metadata fields
#'
#' List all known metadata fields in the SewerRat database.
#'
#' @inheritParams query
#' @param pattern String specifying a pattern for filtering fields, using the usual \code{*} and \code{?} wildcards.
#' Only fields matching to the pattern will be returned. 
#' If \code{NULL}, no filtering is performed.
#' @param count Logical scalar indicating whether to count the number of metadata files associated with each field.
#' 
#' @return List of named lists, where each inner list corresponds to a field and contains:
#' \itemize{
#' \item \code{field}, string containing the field.
#' \item \code{count}, integer scalar specifying the number of files associated with the field.
#' This is only present if \code{count=TRUE} in the arguments.
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
#' # Pulling out all the fields.
#' listFields(info$url)
#' listFields(info$url, pattern="fir*")
#' listFields(info$url, count=TRUE)
#'
#' @export
#' @import httr2
#' @importFrom utils URLencode
listFields <- function(url, pattern=NULL, count=FALSE, number=1000, on.truncation=c("message", "warning", "none")) {
    query <- character(0)

    if (!is.null(pattern)) {
        query <- c(query, paste0("pattern=", URLencode(pattern, reserved=TRUE)))
    }
    if (count) {
        query <- c(query, "count=true")
    }

    on.truncation <- match.arg(on.truncation)
    original.number <- number
    if (on.truncation != "none") {
        number <- number + 1L
    }

    stub <- "/fields"
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
