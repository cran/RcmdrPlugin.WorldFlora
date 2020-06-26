`WorldFlora.get.started` <-
function()
{
    start.file <- file.path(system.file(package = "RcmdrPlugin.WorldFlora"), "/doc/Getting-started-WorldFlora.txt")
    file.show(start.file)
}

