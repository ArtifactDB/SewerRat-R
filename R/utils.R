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
