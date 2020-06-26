WFO.matchGUI <- function(){
    .activeDataSet <- ActiveDataSet()
    .newActive <- paste(.activeDataSet, ".match", sep="")
    defaults <- list(initial.spec.name="spec.name", initial.Authorship="Authorship")
    dialog.values <- getDialog("WFO.matchGUI", defaults)
    initializeDialog(title=gettextRcmdr("WFO.match"))

    variablesFrame <- tkframe(top)
    spec.nameBox <- variableListBox(variablesFrame, c(Variables(), "<NONE>"), selectmode="single",
        title=gettextRcmdr("spec.name"), listHeight=10,
        initialSelection=varPosn(dialog.values$initial.spec.name, type="all", vars=c(Variables(), "<NONE>")))
    AuthorshipBox <- variableListBox(variablesFrame, c(Variables(), "<NONE>"), selectmode="single",
        title=gettextRcmdr("Authorship (optional)"), listHeight=10,
        initialSelection=varPosn(dialog.values$initial.Authorship, type="all", vars=c(Variables(), "<NONE>")))

    optionsFrame <- tkframe(top)
    optionsLFrame <- tkframe(optionsFrame)
    optionsRFrame <- tkframe(optionsFrame)
    
    First.distFrame <- tkframe(optionsLFrame)
    First.distVariable <- tclVar("1")
    First.distCheckBox <- ttkcheckbutton(First.distFrame, variable = First.distVariable)

    Fuzzy.minFrame <- tkframe(optionsLFrame)
    Fuzzy.minVariable <- tclVar("1")
    Fuzzy.minCheckBox <- ttkcheckbutton(Fuzzy.minFrame, variable = Fuzzy.minVariable)

    Fuzzy.twoFrame <- tkframe(optionsLFrame)
    Fuzzy.twoVariable <- tclVar("1")
    Fuzzy.twoCheckBox <- ttkcheckbutton(Fuzzy.twoFrame, variable = Fuzzy.twoVariable)

    Fuzzy.oneFrame <- tkframe(optionsLFrame)
    Fuzzy.oneVariable <- tclVar("1")
    Fuzzy.oneCheckBox <- ttkcheckbutton(Fuzzy.oneFrame, variable = Fuzzy.oneVariable)

    squishFrame <- tkframe(optionsLFrame)
    squishVariable <- tclVar("1")
    squishCheckBox <- ttkcheckbutton(squishFrame, variable = squishVariable)

    spec.name.tolowerFrame <- tkframe(optionsLFrame)
    spec.name.tolowerVariable <- tclVar("0")
    spec.name.tolowerCheckBox <- ttkcheckbutton(spec.name.tolowerFrame, variable = spec.name.tolowerVariable)

    spec.name.nonumberFrame <- tkframe(optionsLFrame)
    spec.name.nonumberVariable <- tclVar("1")
    spec.name.nonumberCheckBox <- ttkcheckbutton(spec.name.nonumberFrame, variable = spec.name.nonumberVariable)

    spec.name.nobracketsFrame <- tkframe(optionsLFrame)
    spec.name.nobracketsVariable <- tclVar("1")
    spec.name.nobracketsCheckBox <- ttkcheckbutton(spec.name.nobracketsFrame, variable = spec.name.nobracketsVariable)

    exclude.infraspecificFrame <- tkframe(optionsLFrame)
    exclude.infraspecificVariable <- tclVar("0")
    exclude.infraspecificCheckBox <- ttkcheckbutton(exclude.infraspecificFrame, variable = exclude.infraspecificVariable)

    onlyLogFrame <- tkframe(optionsRFrame)
    onlyLogVariable <- tclVar("0")
    onlyLogCheckBox <- ttkcheckbutton(onlyLogFrame, variable = onlyLogVariable)
      
    onOK <- function(){    
        dataSets1 <- listDataSets()
        logger(paste("Using active data set: ", .activeDataSet, sep=""))
        spec.name <- getSelection(spec.nameBox)
        if (length(spec.name) > 0) {if (spec.name == "<NONE>") {spec.name <- ""}}
        Authorship <- getSelection(AuthorshipBox)
        if (length(Authorship) > 0) {if (Authorship == "<NONE>") {Authorship <- ""}}
        First.dist <- FALSE
        if (tclvalue(First.distVariable) == 1) {First.dist <- TRUE}
        Fuzzy.min <- FALSE
        if (tclvalue(Fuzzy.minVariable) == 1) {Fuzzy.min <- TRUE}
        Fuzzy.two <- FALSE
        if (tclvalue(Fuzzy.twoVariable) == 1) {Fuzzy.two <- TRUE}
        Fuzzy.one <- FALSE
        if (tclvalue(Fuzzy.oneVariable) == 1) {Fuzzy.one <- TRUE}
        squish <- FALSE
        if (tclvalue(squishVariable) == 1) {squish <- TRUE}
        spec.name.tolower <- FALSE
        if (tclvalue(spec.name.tolowerVariable) == 1) {spec.name.tolower <- TRUE}
        spec.name.nonumber <- FALSE
        if (tclvalue(spec.name.nonumberVariable) == 1) {spec.name.nonumber <- TRUE}
        spec.name.nobrackets <- FALSE
        if (tclvalue(spec.name.nobracketsVariable) == 1) {spec.name.nobrackets <- TRUE}
        exclude.infraspecific <- FALSE
        if (tclvalue(exclude.infraspecificVariable) == 1) {exclude.infraspecific <- TRUE}
        
        command <- paste("WFO.match(spec.data=", .activeDataSet, ", WFO.data=WFO.data,
            spec.name='", spec.name, "', Authorship='", Authorship, "', First.dist=", First.dist, ", Fuzzy.min=", Fuzzy.min,
            ", Fuzzy=0.1, Fuzzy.max=250, Fuzzy.two=", Fuzzy.two, ", Fuzzy.one=", Fuzzy.one, 
            ", squish=", squish, ", spec.name.tolower=", spec.name.tolower, ", spec.name.nonumber=", spec.name.nonumber,
            ", spec.name.nobrackets=", spec.name.nobrackets, ", exclude.infraspecific=", exclude.infraspecific,
            ", verbose=TRUE, counter=1000)", sep="")
        logger(paste(.newActive, " <- ", command, sep=""))
        
        if (tclvalue(onlyLogVariable) == 0) {
            if (("WFO.data" %in% dataSets1) == FALSE) {
                logger(paste("WARNING: First you must load WFO.data (Use WFO.download or WFO.remember)", sep=""))
            }else{     
                gassign(.newActive, justDoIt(command))
                activeDataSet(.newActive)
                logger(paste(.newActive, "now is the new active data set"))
                doItAndPrint(paste("print(data.table::as.data.table(", .newActive , "))", sep=""))                
            }
        }
        
    }

    OKCancelHelp(helpSubject="WFO.match", reset="WFO.matchGUI", apply="WFO.matchGUI")
    tkgrid(getFrame(spec.nameBox), labelRcmdr(variablesFrame, text = "          "), 
         getFrame(AuthorshipBox), sticky = "nw")
    tkgrid(variablesFrame, sticky = "w")
    tkgrid(labelRcmdr(top, text=""))
    tkgrid(First.distCheckBox, labelRcmdr(First.distFrame, text=gettextRcmdr("First.dist")), sticky="w")
    tkgrid(First.distFrame, sticky="w")
    tkgrid(Fuzzy.minCheckBox, labelRcmdr(Fuzzy.minFrame, text=gettextRcmdr("Fuzzy.min")), sticky="w")
    tkgrid(Fuzzy.minFrame, sticky="w")
    tkgrid(Fuzzy.twoCheckBox, labelRcmdr(Fuzzy.twoFrame, text=gettextRcmdr("Fuzzy.two")), sticky="w")
    tkgrid(Fuzzy.twoFrame, sticky="w")
    tkgrid(Fuzzy.oneCheckBox, labelRcmdr(Fuzzy.oneFrame, text=gettextRcmdr("Fuzzy.one")), sticky="w")
    tkgrid(Fuzzy.oneFrame, sticky="w")
    tkgrid(squishCheckBox, labelRcmdr(squishFrame, text=gettextRcmdr("squish")), sticky="w")
    tkgrid(squishFrame, sticky="w")
    tkgrid(spec.name.tolowerCheckBox, labelRcmdr(spec.name.tolowerFrame, text=gettextRcmdr("spec.name.tolower")), sticky="w")
    tkgrid(spec.name.tolowerFrame, sticky="w")
    tkgrid(spec.name.nonumberCheckBox, labelRcmdr(spec.name.nonumberFrame, text=gettextRcmdr("spec.name.nonumber")), sticky="w")
    tkgrid(spec.name.nonumberFrame, sticky="w")
    tkgrid(spec.name.nobracketsCheckBox, labelRcmdr(spec.name.nobracketsFrame, text=gettextRcmdr("spec.name.nobrackets")), sticky="w")
    tkgrid(spec.name.nobracketsFrame, sticky="w")
    tkgrid(exclude.infraspecificCheckBox, labelRcmdr(exclude.infraspecificFrame, text=gettextRcmdr("exclude.infraspecific")), sticky="w")
    tkgrid(exclude.infraspecificFrame, sticky="w")
    tkgrid(onlyLogCheckBox, labelRcmdr(onlyLogFrame, text=gettextRcmdr("Only generate script")), sticky="w")
    tkgrid(onlyLogFrame, sticky="w")
    tkgrid(optionsLFrame, labelRcmdr(optionsFrame, text = "                    "), optionsRFrame, sticky = "nw")
    tkgrid(optionsFrame, sticky="w")  
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

