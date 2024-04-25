#' @import httr2
handle_error <- function(req) {
    req_error(req, body = function(res) {
        ct <- resp_content_type(res)
        if (ct == "application/json") {
            resp_body_json(res)$reason
        } else if (ct == "text/plain") {
            resp_body_string(res)
        } else {
            NULL
        }
    })
}

#' @import httr2
redirect_post <- function(req) {
    req_options(req, postredir=7) # see https://curl.se/libcurl/c/CURLOPT_POSTREDIR.html.
}

clean_path <- function(path) {
    if (!startsWith(path, "/")) {
        path <- paste0(getwd(), "/", path)
    }
    path <- gsub("/+", "/", path)

    components <- strsplit(path, "/")[[1]]
    keep <- logical(length(components))
    for (i in seq_along(components)) {
        comp <- components[i]
        if (comp == "..") {
            keep[i-1] <- FALSE
        } else if (comp != "") {
            keep[i] <- TRUE
        }
    }

    keep[1] <- TRUE # keep as an absolute path.
    components <- components[keep]
    paste(components, collapse="/")
}
