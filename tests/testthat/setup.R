basic_config <- function() {
    # Starting up an example SewerRat service:
    info <- SewerRat::startSewerRat()

    # Mocking up a directory of stuff to query.
    mydir <- tempfile()
    dir.create(mydir)
    write(file=file.path(mydir, "metadata.json"), '{ "first": "Aaron", "last": "Lun" }')

    dir.create(file.path(mydir, "diet"))
    write(file=file.path(mydir, "diet", "metadata.json"), '{ "meal": "lunch", "ingredients": "water" }')

    # Registering it:
    SewerRat::register(mydir, "metadata.json", url=info$url)

    list(info=info, mydir=mydir)
}
