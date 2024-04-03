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
#' @examples
#' library(S4Vectors)
#' df <- DataFrame(A=1:10, B=LETTERS[1:10])
#' meta <- list(title="A data frame", description="This is a data frame")
#' tmp <- tempfile()
#' quicksave(df, meta, tmp)
#' quickread(tmp)
#'
#' @export
quicksave <- function(x, metadata, path) {
    alabaster.base::saveObject(x, path)
    write(file=file.path(path, "_metadata.json"), jsonlite::toJSON(metadata, pretty=4, auto_unbox=TRUE))
    invisible(NULL)
}

#' @export
quickread <- function(path) {
    x <- alabaster.base::readObject(path)
    meta <- jsonlite::fromJSON(file.path(path, "_metadata.json"), simplifyVector=FALSE)
    list(x=x, metadata=meta)
}
