#' Start and stop a SewerRat service
#'
#' This sets up a single SewerRat service for the entire R session, is intended for examples and tests.
#' Real SewerRat deployments should operate outside of R.
#'
#' @param db String containing a path to the SQLite database.
#' @param port Integer specifying the port to use for hosting the service.
#' If \code{NULL}, a free port is randomly selected.
#' @param setUrl Logical scalar indicating whether \code{\link{restUrl}} should be updated to use the new service.
#'
#' @return For \code{startSewerRat}, a list indicating whether a new service was set up, along with the port number.
#' If \code{setUrl=TRUE}, the REST URL is updated to this new service.
#'
#' For \code{stopSewerRat}, any existing service is shut down, and \code{NULL} is invisibly returned.
#' The REST URL is also reset to its previous value if it was changed by \code{startSewerRat}.
#'
#' @seealso
#' \url{https://github.com/ArtifactDB/SewerRat}, for source code and binaries to build and run a SewerRat service.
#' 
#' @examples
#' startSewerRat()
#' restUrl()
#' stopSewerRat()
#' 
#' @export
#' @importFrom utils download.file
startSewerRat <- function(db=tempfile(fileext=".sqlite3"), port=NULL, setUrl=TRUE) {
    if (!is.null(running$active)) {
        return(list(new=FALSE, port=running$port))
    }

    # This should really be the cache directory, but our HPC deployment does
    # naughty things with mounting .cache, so we'll just use data instead. 
    cache <- tools::R_user_dir("SewerRat", "data")

    sinfo <- Sys.info()
    sysname <- sinfo["sysname"]
    if (sysname == "Darwin") {
        os <- "darwin"
    } else if (sysname == "Linux") {
        os <- "linux"
    } else {
        stop("unsupported operating system '", sysname, "'")
    }

    sysmachine <- sinfo["machine"]
    if (sysmachine == "arm64") {
        arch <- "arm64"
    } else if (sysmachine == "x86_64") {
        arch <- "amd64"
    } else {
        stop("unsupported architecture '", sysmachine, "'")
    }

    desired <- sprintf("SewerRat-%s-%s", os, arch)
    exe <- file.path(cache, desired)
    if (!file.exists(exe)) {
        url <- paste0("https://github.com/ArtifactDB/SewerRat/releases/download/latest/", desired)
        tmp <- tempfile()
        if (download.file(url, tmp)) {
            stop("failed to download the SewerRat binary")
        }
        Sys.chmod(tmp, "0755")

        # Using a write-and-rename paradigm to provide some atomicity. Note
        # that renaming doesn't work across different filesystems so in that
        # case we just fall back to copying.
        dir.create(cache, recursive=TRUE, showWarnings=FALSE)
        if (!file.rename(tmp, exe) && !file.copy(tmp, exe)) { 
            stop("cannot transfer file from '", tmp, "' to '", exe, "'")
        }
    }

    if (is.null(port)) {
        port <- choose_port()
    }

    script <- system.file("scripts", "deploy.sh", package="SewerRat", mustWork=TRUE)
    pid <- system2(script, c(shQuote(exe), shQuote(db), shQuote(port)), stdout=TRUE) 

    process <- new.env()
    process$pid <- pid
    reg.finalizer(process, kill_SewerRat, onexit=TRUE)

    running$active <- process
    running$port <- port
    if (setUrl) {
        running$url <- restUrl(paste0("http://0.0.0.0:", port))
    }

    list(new=TRUE, port=port)
}

#' @import methods
choose_port <- function() {
    # Based on the same logic as shiny::runApp. 
    choices <- 3000:8000
    choices <- setdiff(choices, c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)) 

    for (i in 1:10) {
        port <- sample(choices, 1)
        soc <- try(serverSocket(port), silent=TRUE)
        if (!is(soc, "try-error")) {
            close(soc)
            return(port)
        }
    }
}

kill_SewerRat <- function(process) {
    if (!is.null(process$pid)) {
        system2("kill", c("-9", process$pid))
    }
}

running <- new.env()

#' @export
#' @rdname startSewerRat
stopSewerRat <- function() {
    if (!is.null(running$active)) {
        kill_SewerRat(running$active)
        running$active <- NULL
        running$port <- NULL
        if (!is.null(running$url)) {
            restUrl(running$url)
            running$url <- NULL
        }
    }
    invisible(NULL)
}
