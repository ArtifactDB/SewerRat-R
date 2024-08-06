#' @import httr1
extract_error_message <- function(res) {
    ct <- resp_content_type(res)
    if (ct == "application/json") {
        resp_body_json(res)$reason
    } else if (ct == "text/plain") {
        resp_body_string(res)
    } else {
        NULL
    }
}

#' @import httr2
handle_error <- function(req) {
    req_error(req, body = extract_error_message)
}

#' @import httr2
redirect_post <- function(req) {
    req_options(req, postredir=7) # see https://curl.se/libcurl/c/CURLOPT_POSTREDIR.html.
}

#' @importFrom utils head
clean_path <- function(path) {
    # Don't use normalizePath() here, as that may end up resolving symlinks
    # that the user wanted to keep (e.g., for aliased drives). Rather, we do
    # the bare minimum required to obtain a clean absolute path, analogous
    # to Golang's filepath.Clean().
    if (!startsWith(path, "/")) {
        path <- paste0(getwd(), "/", path)
    }

    components <- strsplit(path, "/")[[1]]
    keep <- character(0)
    for (i in seq_along(components)) {
        comp <- components[i]
        if (comp == "..") {
            # 'head()' is robust to '..' at the start of the path.
            keep <- head(keep, -1)
        } else if (comp == "") {
            # no-op, it's a redundant '//' or we're at the start.
        } else if (comp == ".") {
            # no-op as well.
        } else {
            keep <- c(keep, comp)
        }
    }

    keep <- c("", keep) # add back the root.
    paste(keep, collapse="/")
}
