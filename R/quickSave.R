#' Quickly save and read objects from disk
#'
#' Quickly save and read Bioconductor objects from their on-disk representations (based on the \pkg{alabaster.base} package).
#'
#' @param x Some Bioconductor object.
#' @param metadata A list of metadata describing the object.
#' Any JSON-compatible data can be stored, in any structure; though it is conventional to have a \code{title} and \code{description} field.
#' @param path String containing the path to the directory in which to save \code{x}.
#' Note that this should be world-readable if it is to be registered in \code{\link{register}}.
#'
#' @author Aaron Lun
#'
#' @return \code{quickSave} will save \code{x} to \code{path}.
#' Metadata will be saved inside \code{path} in a \code{_metadata.json} file.
#'
#' \code{quickRead} returns a list containing \code{x}, the Bioconductor object; and \code{metadata}, a list containing the metadata.
#' 
#' @examples
#' # Staring up a test service:
#' startSewerRat()
#'
#' # Mocking up a Bioconductor object:
#' library(S4Vectors)
#' df <- DataFrame(A=1:10, B=LETTERS[1:10])
#' meta <- list(title="A data frame", description="This is a data frame")
#'
#' # Saving it inside a directory to be registered:
#' dir <- tempfile()
#' dir.create(dir)
#' quickSave(df, meta, file.path(dir, "my_df"))
#'
#' # Then we just register that directory:
#' register(dir)
#'
#' # And now we can find interesting things:
#' res <- query("data frame")
#' 
#' # ... and load them back in:
#' quickRead(res[[1]]$path)
#'
#' # Shutting down our test service:
#' stopSewerRat()
#' @export
quickSave <- function(x, metadata, path) {
    alabaster.base::saveObject(x, path)
    write(file=file.path(path, "_metadata.json"), jsonlite::toJSON(metadata, pretty=4, auto_unbox=TRUE))
    invisible(NULL)
}

#' @export
#' @rdname quickSave
quickRead <- function(path) {
    x <- alabaster.base::readObject(dirname(path))
    meta <- jsonlite::fromJSON(path, simplifyVector=FALSE)
    list(x=x, metadata=meta)
}
