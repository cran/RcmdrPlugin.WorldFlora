WFO.acceptable.matchGUI <- function(){
    .activeDataSet <- ActiveDataSet()
    .newActive <- paste(.activeDataSet, ".acceptable", sep="")
    defaults <- list(initial.spec.name="spec.name")
    dialog.values <- getDialog("WFO.acceptable.matchGUI", defaults)
    initializeDialog(title=gettextRcmdr("WFO.acceptable.match"))

    variablesFrame <- tkframe(top)
    spec.nameBox <- variableListBox(variablesFrame, c(Variables(), "<NONE>"), selectmode="single",
        title=gettextRcmdr("spec.name"), listHeight=10,
        initialSelection=varPosn(dialog.values$initial.spec.name, type="all", vars=c(Variables(), "<NONE>")))

    optionsFrame <- tkframe(top)
    optionsLFrame <- tkframe(optionsFrame)
    optionsRFrame <- tkframe(optionsFrame)

    no.vowelFrame <- tkframe(optionsLFrame)
    no.vowelVariable <- tclVar("1")
    no.vowelCheckBox <- ttkcheckbutton(no.vowelFrame, variable = no.vowelVariable)

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

    onlyLogFrame <- tkframe(optionsRFrame)
    onlyLogVariable <- tclVar("0")
    onlyLogCheckBox <- ttkcheckbutton(onlyLogFrame, variable = onlyLogVariable)

    onOK <- function(){
        dataSets1 <- listDataSets()
        logger(paste("Using active data set: ", .activeDataSet, sep=""))
        spec.name <- getSelection(spec.nameBox)
        if (length(spec.name) > 0) {if (spec.name == "<NONE>") {spec.name <- ""}}
        no.vowel <- FALSE
        if (tclvalue(no.vowelVariable) == 1) {no.vowel <- TRUE}

        command <- paste("cbind(", .activeDataSet,
            ", Acceptable.match=WFO.acceptable.match(x=", .activeDataSet, ",
            spec.name='", spec.name, "', no.vowel=", no.vowel,
            "))", sep="")
        logger(paste(.newActive, " <- ", command, sep=""))

        if (tclvalue(onlyLogVariable) == 0) {
                gassign(.newActive, justDoIt(command))
                activeDataSet(.newActive)
                logger(paste(.newActive, "now is the new active data set"))
                doItAndPrint(paste("print(data.table::as.data.table(", .newActive , "))", sep=""))
        }

    }

    OKCancelHelp(helpSubject="WFO.acceptable.match", reset="WFO.acceptable.matchGUI", apply="WFO.acceptable.matchGUI")
    tkgrid(getFrame(spec.nameBox), labelRcmdr(variablesFrame, text = "          "), sticky = "nw")
    tkgrid(variablesFrame, sticky = "w")
    tkgrid(labelRcmdr(top, text=""))
    tkgrid(no.vowelCheckBox, labelRcmdr(no.vowelFrame, text=gettextRcmdr("no.vowel")), sticky="w")
    tkgrid(no.vowelFrame, sticky="w")
    tkgrid(onlyLogCheckBox, labelRcmdr(onlyLogFrame, text=gettextRcmdr("Only generate script")), sticky="w")
    tkgrid(onlyLogFrame, sticky="w")
    tkgrid(optionsLFrame, labelRcmdr(optionsFrame, text = "                    "), optionsRFrame, sticky = "nw")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

