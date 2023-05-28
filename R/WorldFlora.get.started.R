`WorldFlora.get.started` <-
function()
{
    start.file <- file.path(system.file(package = "RcmdrPlugin.WorldFlora"), "etc/Getting-started-WorldFlora.txt")
    file.show(start.file)
}


`browseWFOmanual` <- function() {
    utils::browseURL("https://www.researchgate.net/publication/342657019_WORLDFLORA_User_Guide_for_Graphical_User_Interface")
}

`browseRpub2023` <- function() {
  utils::browseURL("https://rpubs.com/Roeland-KINDT/996500")
}

`browseRpub` <- function() {
  utils::browseURL("https://rpubs.com/Roeland-KINDT")
}

`helpWorldFlora` <- function() {
    print(utils::help(package="WorldFlora", help_type="html"))
}




