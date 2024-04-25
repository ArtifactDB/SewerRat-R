#' Retrieve registered metadata 
#'
#' Retrieve a single metadata entry in a registered directory from the SewerRat API.
#'
#' @param path String containing the absolute path to a metadata file in a registered directory.
#' @param url String containing the URL to the SewerRat REST API.
#'
#' @author Aaron Lun
#' 
#' @return A named list containing \code{path}, the path to the metadata file;
#' \code{user}, the identity of the owning user;
#' \code{time}, the Unix time at which the file was modified;
#' and \code{metadata}, the loaded metadata, typically as a named list representing a JSON object.
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
