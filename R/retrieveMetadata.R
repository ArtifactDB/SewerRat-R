#' Retrieve registered metadata 
#'
#' Retrieve a single metadata entry in a registered directory from the SewerRat API.
#'
#' @param path String containing the absolute path to a file in a registered directory.
#' @inheritParams retrieveDirectory
#'
#' @author Aaron Lun
#' 
#' @return String containing the path to the file on the caller's filesystem.
#' 
#' @examples
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
#' retrieveMetadata(paste0(mydir, "/metadata.json"), url=info$url)
#' retrieveMetadata(paste0(mydir, "/diet/metadata.json"), url=info$url)
#' 
#' @export
#' @import httr2
retrieveMetadata <- function(path, url) {
    req <- request(paste0(url, "/retrieve/metadata?path=", URLencode(path, reserved=TRUE)))
    req <- handle_error(req)
    resp <- req_perform(req)
    resp_body_json(resp)
}
