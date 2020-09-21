WFO.exploreGUI <- function(){
    dataSets1 <- listDataSets()

    initializeDialog(title=gettextRcmdr("Explore taxon"))

    variablesFrame <- tkframe(top)
    taxonName <- tclVar("Taxon")
    taxon <- ttkentry(variablesFrame, width="50", textvariable=taxonName)

    optionsFrame <- tkframe(top)
    optionsLFrame <- tkframe(optionsFrame)
    optionsRFrame <- tkframe(optionsFrame)

    matchFrame <- tkframe(optionsLFrame)
    matchVariable <- tclVar("1")
    matchCheckBox <- ttkcheckbutton(matchFrame, variable = matchVariable)
    
    oneFrame <- tkframe(optionsLFrame)
    oneVariable <- tclVar("0")
    oneCheckBox <- ttkcheckbutton(oneFrame, variable = oneVariable)

    urlFrame <- tkframe(optionsLFrame)
    urlVariable <- tclVar("0")
    urlCheckBox <- ttkcheckbutton(urlFrame, variable = urlVariable)

    browseFrame <- tkframe(optionsLFrame)
    browseVariable <- tclVar("0")
    browseCheckBox <- ttkcheckbutton(browseFrame, variable = browseVariable)

    synonymsFrame <- tkframe(optionsLFrame)
    synonymsVariable <- tclVar("0")
    synonymsCheckBox <- ttkcheckbutton(synonymsFrame, variable = synonymsVariable)

    familyFrame <- tkframe(optionsLFrame)
    familyVariable <- tclVar("0")
    familyCheckBox <- ttkcheckbutton(familyFrame, variable = familyVariable)

    onlyLogFrame <- tkframe(optionsRFrame)
    onlyLogVariable <- tclVar("0")
    onlyLogCheckBox <- ttkcheckbutton(onlyLogFrame, variable = onlyLogVariable)
      
    onOK <- function(){
        TaxonValue <- tclvalue(taxonName)
        TaxonValue.gsub <- gsub(pattern=" ", replacement="_", TaxonValue)

        matching <- FALSE
        if (tclvalue(matchVariable) == 1) {matching <- TRUE}
        oining <- FALSE
        if (tclvalue(oneVariable) == 1) {oining <- TRUE}
        urling <- FALSE
        if (tclvalue(urlVariable) == 1) {urling <- TRUE}
        browse <- FALSE
        if (tclvalue(browseVariable) == 1) {browse <- TRUE}
        synonyms <- FALSE
        if (tclvalue(synonymsVariable) == 1) {synonyms <- TRUE}
        family <- FALSE
        if (tclvalue(familyVariable) == 1) {family <- TRUE}

        .newActive <- paste(trim.blanks(TaxonValue), ".browse", sep="")

        if (matching == TRUE) {
        .newActive <- paste(TaxonValue.gsub, ".match", sep="")      
        command <- paste("WFO.match('", TaxonValue, 
            "', WFO.data=WFO.data)", sep="")
        logger(paste(.newActive, " <- ", command, sep=""))
        
        if (tclvalue(onlyLogVariable) == 0) {
            if (("WFO.data" %in% dataSets1) == FALSE) {
                logger(paste("WARNING: First you must load WFO.data (Use WFO.download or WFO.remember)", sep=""))
            }else{     
                gassign(.newActive, justDoIt(command))
                logger(paste("Results available in data set: ", .newActive, sep=""))
                doItAndPrint(paste(.newActive))
            }
        }
        
        } # matching

        if (oining == TRUE) {
        .newActive <- paste(TaxonValue.gsub, ".one", sep="")      
        command <- paste("WFO.one(WFO.match('", TaxonValue, 
            "', WFO.data=WFO.data))", sep="")
        logger(paste(.newActive, " <- ", command, sep=""))
        
        if (tclvalue(onlyLogVariable) == 0) {
            if (("WFO.data" %in% dataSets1) == FALSE) {
                logger(paste("WARNING: First you must load WFO.data (Use WFO.download or WFO.remember)", sep=""))
            }else{     
                gassign(.newActive, justDoIt(command))
                logger(paste("Results available in data set: ", .newActive, sep=""))
                doItAndPrint(paste(.newActive))
            }
        }
        
        } # oining

        if (urling == TRUE) {
        .newActive <- paste(TaxonValue.gsub, ".url", sep="")      
        command <- paste("WFO.url(WFO.one(WFO.match('", TaxonValue, 
            "', WFO.data=WFO.data)), browse=TRUE)", sep="")
        logger(paste(.newActive, " <- ", command, sep=""))
        
        if (tclvalue(onlyLogVariable) == 0) {
            if (("WFO.data" %in% dataSets1) == FALSE) {
                logger(paste("WARNING: First you must load WFO.data (Use WFO.download or WFO.remember)", sep=""))
            }else{     
                gassign(.newActive, justDoIt(command))
                logger(paste("Results available in data set: ", .newActive, sep=""))
                doItAndPrint(paste(.newActive))
            }
        }
        
        } # urling

        if (browse == TRUE) {
        .newActive <- paste(TaxonValue.gsub, ".browse", sep="")      
        command <- paste("WFO.browse('", TaxonValue, 
            "', WFO.data=WFO.data, accepted.only=FALSE)", sep="")
        logger(paste(.newActive, " <- ", command, sep=""))
        
        if (tclvalue(onlyLogVariable) == 0) {
            if (("WFO.data" %in% dataSets1) == FALSE) {
                logger(paste("WARNING: First you must load WFO.data (Use WFO.download or WFO.remember)", sep=""))
            }else{     
                gassign(.newActive, justDoIt(command))
                logger(paste("Results available in data set: ", .newActive, sep=""))
                doItAndPrint(paste(.newActive))
            }
        }
        
        } # browse

        if (synonyms == TRUE) {
        .newActive <- paste(TaxonValue.gsub, ".synonyms", sep="")      
        command <- paste("WFO.synonyms('", TaxonValue, 
            "', WFO.data=WFO.data)", sep="")
        logger(paste(.newActive, " <- ", command, sep=""))
        
        if (tclvalue(onlyLogVariable) == 0) {
            if (("WFO.data" %in% dataSets1) == FALSE) {
                logger(paste("WARNING: First you must load WFO.data (Use WFO.download or WFO.remember)", sep=""))
            }else{     
                gassign(.newActive, justDoIt(command))
                logger(paste("Results available in data set: ", .newActive, sep=""))
                doItAndPrint(paste(.newActive))
            }
        }
        
        } # synonyms

        if (family == TRUE) {
        .newActive <- paste(TaxonValue.gsub, ".family", sep="")      
        command <- paste("WFO.family('", TaxonValue, 
            "', WFO.data=WFO.data)", sep="")
        logger(paste(.newActive, " <- ", command, sep=""))
        
        if (tclvalue(onlyLogVariable) == 0) {
            if (("WFO.data" %in% dataSets1) == FALSE) {
                logger(paste("WARNING: First you must load WFO.data (Use WFO.download or WFO.remember)", sep=""))
            }else{     
                gassign(.newActive, justDoIt(command))
                logger(paste("Results available in data set: ", .newActive, sep=""))
                doItAndPrint(paste(.newActive))
            }
        }
        
        } # family
        
        
    }

    OKCancelHelp(helpSubject="WFO.synonyms", reset="WFO.exploreGUI", apply="WFO.exploreGUI")
    tkgrid(labelRcmdr(variablesFrame, text=gettextRcmdr("Explore taxon:")), taxon, sticky="w")
    tkgrid(variablesFrame, sticky = "w")
    tkgrid(labelRcmdr(top, text=""))

    tkgrid(matchCheckBox, labelRcmdr(matchFrame, text=gettextRcmdr("WFO.match")), sticky="w")
    tkgrid(matchFrame, sticky="w")
    tkgrid(oneCheckBox, labelRcmdr(oneFrame, text=gettextRcmdr("WFO.one")), sticky="w")
    tkgrid(oneFrame, sticky="w")
    tkgrid(urlCheckBox, labelRcmdr(urlFrame, text=gettextRcmdr("WFO.url")), sticky="w")
    tkgrid(urlFrame, sticky="w")
    tkgrid(browseCheckBox, labelRcmdr(browseFrame, text=gettextRcmdr("browse")), sticky="w")
    tkgrid(browseFrame, sticky="w")
    tkgrid(synonymsCheckBox, labelRcmdr(synonymsFrame, text=gettextRcmdr("synonyms")), sticky="w")
    tkgrid(synonymsFrame, sticky="w")
    tkgrid(familyCheckBox, labelRcmdr(familyFrame, text=gettextRcmdr("family")), sticky="w")
    tkgrid(familyFrame, sticky="w")
    tkgrid(onlyLogCheckBox, labelRcmdr(onlyLogFrame, text=gettextRcmdr("Only generate script")), sticky="w")
    tkgrid(onlyLogFrame, sticky="w")
    tkgrid(optionsLFrame, labelRcmdr(optionsFrame, text = "                    "), optionsRFrame, sticky = "nw")
    tkgrid(optionsFrame, sticky="w") 
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

