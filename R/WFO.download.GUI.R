

WFO.downloadGUI <- function(){
#    .activeDataSet <- ActiveDataSet()
    doItAndPrint(paste("WFO.download()", sep=""))
    doItAndPrint(paste("str(WFO.data)", sep=""))
}

WFO.rememberGUI <- function(){
#    .activeDataSet <- ActiveDataSet()
  logger(paste("Please be patient while the WFO data loads."))
    doItAndPrint(paste("WFO.remember()", sep=""))
    doItAndPrint(paste("str(WFO.data)", sep=""))
}

WFO.remember.chooseGUI <- function(){
#    .activeDataSet <- ActiveDataSet()
  logger(paste("Please be patient while the WFO data loads."))
    doItAndPrint(paste("WFO.remember(file.choose())", sep=""))
    doItAndPrint(paste("str(WFO.data)", sep=""))
}

exportExcelGUI <- function(){
    availablePackages <- sort(setdiff(.packages(all.available=TRUE), .packages()))
    if (("writexl" %in% availablePackages) == FALSE) {
        logger(paste("WARNING: First you must install the writexl package", sep=""))
    }else{
        .activeDataSet <- ActiveDataSet()
        file.location <- paste(getwd(), "//", .activeDataSet, ".xlsx", sep="")
        doItAndPrint(paste("writexl::write_xlsx(", .activeDataSet, ", path='", file.location, "')", sep=""))
    }
}

