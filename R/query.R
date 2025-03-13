#' Query metadata 
#'
#' Query the metadata in the SewerRat backend based on free text, the owner, creation time, etc.
#' This function does not require access to the same filesystem as the SewerRat API. 
#'
#' @param text String containing a free-text query, see below for details.
#' If missing, no filtering is applied based on the metadata text.
#' @param user String containing the name of the user who generated the metadata.
#' If missing, no filtering is applied based on the user.
#' @param path String containing any component of the path to the metadata file.
#' If missing, no filtering is applied based on the path.
#' @param from A \link{POSIXt} object to filter out older files, i.e., only files newer than \code{from} will be retained.
#' If missing, no filtering is applied to remove old files.
#' @param until A \link{POSIXt} object to filter out newer files, i.e., only files older than \code{until} will be retained.
#' If missing, no filtering is applied to remove new files.
#' @param number Integer specifying the maximum number of results to return.
#' This can also be \code{Inf} to return all results.
#' @param on.truncation String specifying what to do when the number of results exceeds \code{number}.
#' Either \code{"warning"}, \code{"message"}, or \code{"none"}.
#' @param url String containing the URL to the SewerRat REST API.
#' @param results List containing the output of \code{query}.
#'
#' @return List of lists where each inner list corresponds to a metadata file and contains:
#' \itemize{
#' \item \code{path}, a string containing the path to the file.
#' \item \code{user}, the identity of the file owner.
#' \item \code{time}, the Unix time of most recent file modification.
#' \item \code{metadata}, a list representing the JSON contents of the file.
#' }
#'
#' For \code{formatQueryResults}, a data frame containing \code{path}, \code{user}, \code{time} and \code{metadata}.
#' Each row corresponds to one of the search results in \code{results}.
#' Each \code{time} is now a \link{POSIXct} object.
#'
#' @section Formatting the text query:
#' For simple use cases, just enter one or more search terms, and the backend search for metadata files that match all the terms.
#'
#' Advanced users can use the \code{AND} and \code{OR} keywords to perform binary logical operations.
#' (Make sure to use all-caps for these keywords.)
#' This can be combined with parentheses to control precedence, e.g., \code{(a b OR c d) AND (e f)};
#' otherwise, \code{AND} takes precedence over \code{OR}.
#' Note that any sequence of adjacent search terms are implicitly \code{AND},
#' i.e., the query above can be expanded as \code{((a AND b) OR (c AND d)) AND (e AND f))}.
#'
#' On a similar note, the \code{NOT} keyword can be used for unary negation.
#' This should be put before any search terms, e.g., \code{(NOT a b) AND (c d)}.
#' If there are no parentheses, any \code{NOT} will take precedence over the other boolean operations,
#' i.e., the above query is the same as \code{NOT a b AND c d}.
#'
#' Users can prefix any sequence of search terms with the name of a metadata field, 
#' to only search for matches within that field of the metadata file, e.g.,
#' \code{(title: prostate cancer) AND (genome: GRCh38 OR genome: GRCm38)}.
#' Note that this does not extend to the \code{AND}, \code{OR} and \code{NOT} keywords,
#' e.g., \code{title:foo OR bar} will not limit the search for \code{bar} to the \code{title} field.
#'
#' Users can attach the usual \code{*} or \code{?} wildcards to any term to enable pattern matching.
#' For example, \code{neur*} will match files with \code{neuron}, \code{neural}, \code{neurological}, etc.,
#' while \code{cd?} will match files with \code{cd4}, \code{cd8}, \code{cd9}, etc.
#' 
#' @author Aaron Lun
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
#' query("aaron", url=info$url)
#' query("lun*", url=info$url) 
#' query("lun* AND aaron", url=info$url)
#' query("meal:bar%", url=info$url)
#'
#' query(path="diet/", url=info$url) # has 'diet/' in the path
#' query(user=Sys.info()["user"], url=info$url) # created by the current user
#' query(from=Sys.time() - 60, url=info$url) # no more than 1 minute old
#'
#' q <- query("lun*", url=info$url)
#' formatQueryResults(q)
#' @export
#' @import httr2
#' @importFrom utils head
query <- function(text, user, path, from, until, url, number=100, on.truncation=c("message", "warning", "none")) {
    conditions <- list()

    if (!missing(text)) {
        conditions <- c(conditions, list(list(type="text", text=text)))
    }

    if (!missing(user)) {
        conditions <- c(conditions, list(list(type="user", user=user)))
    }

    if (!missing(path)) {
        conditions <- c(conditions, list(list(type="path", path=path)))
    }

    if (!missing(from)) {
        conditions <- c(conditions, list(list(type="time", time=round(as.double(from)), after=TRUE)))
    }

    if (!missing(until)) {
        conditions <- c(conditions, list(list(type="time", time=round(as.double(until)))))
    }

    if (length(conditions) > 1) {
        query <- list(type="and", children=conditions)
    } else if (length(conditions) == 1) {
        query <- conditions[[1]]
    } else {
        stop("at least one search filter must be present")
    }

    on.truncation <- match.arg(on.truncation)
    if (on.truncation != "none") {
        original.number <- number
        number <- number + 1L
    }

    stub <- paste0("/query?translate=true")
    collected <- list()

    while (length(collected) < number) {
        current.url <- paste0(url, stub)
        if (!is.infinite(number)) {
            current.url <- paste0(current.url, "&limit=", number - length(collected))
        }

        req <- request(current.url)
        req <- req_method(req, "POST")
        req <- req_body_json(req, query)
        req <- redirect_post(req)
        req <- handle_error(req)
        res <- req_perform(req)

        payload <- resp_body_json(res)
        collected <- c(collected, payload$results)
        stub <- payload$`next`
        if (is.null(stub)) {
            break
        }
    }

    if (on.truncation != "none") {
        if (!is.infinite(original.number) && original.number < length(collected)) {
            msg <- sprintf("truncated query results to the first %i matches", original.number)
            if (on.truncation == "warning") {
                warning(msg)
            } else {
                message(msg)
            }
            collected <- head(collected, original.number)
        }
    }

    collected
}

#' @export
#' @rdname query
formatQueryResults <- function(results) {
    N <- length(results)
    all.paths <- character(N)
    all.times <- double(N)
    all.users <- character(N)
    all.meta <- vector("list", N)

    for (i in seq_along(results)) {
        y <- results[[i]]
        all.paths[i] <- y$path
        all.times[i] <- y$time
        all.users[i] <- y$user
        all.meta[[i]] <- y$metadata
    }

    data.frame(
        path=all.paths,
        user=all.users,
        time=as.POSIXct(all.times),
        metadata=I(all.meta)
    )
}
