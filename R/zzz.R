.onLoad <- function(lib, pkg) {
    url <- Sys.getenv("SEWERRAT_REST_URL", NA_character_)
    if (!is.na(url)) {
        restUrl(url)
    }
}
