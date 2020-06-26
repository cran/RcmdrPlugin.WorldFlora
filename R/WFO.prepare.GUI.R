WFO.prepareGUI <- function(){
    .activeDataSet <- ActiveDataSet()
    .newActive <- paste(.activeDataSet, ".prep", sep="")
    trinomial.vars <- c("cultivar.", "f.", "sect.", "subf.", "subg.", "subsp.", "subvar.", "var.",
                           "CULTIVAR.",       "SECT.", "SUBF.", "SUBG.", "SUBSP.", "SUBVAR.", "VAR.")
    defaults <- list(initial.spec.full="spec.full", initial.trinomial=trinomial.vars)
    dialog.values <- getDialog("WFO.prepareGUI", defaults)
    initializeDialog(title=gettextRcmdr("WFO.prepare"))

    variablesFrame <- tkframe(top)
    spec.fullBox <- variableListBox(variablesFrame, c(Variables(), "<NONE>"), selectmode="single",
        title=gettextRcmdr("spec.full"), listHeight=10,
        initialSelection=varPosn(dialog.values$initial.spec.full, type="all", vars=c(Variables(), "<NONE>")))
    trinomialBox <- variableListBox(variablesFrame, trinomial.vars, selectmode="multiple",
        title=gettextRcmdr("trinomial (CTRL-A to select all)"), listHeight=10,
        initialSelection=varPosn(dialog.values$initial.trinomial, type="all", vars=trinomial.vars))

    optionsFrame <- tkframe(top)
    optionsLFrame <- tkframe(optionsFrame)
    optionsRFrame <- tkframe(optionsFrame)

    squishFrame <- tkframe(optionsLFrame)
    squishVariable <- tclVar("1")
    squishCheckBox <- ttkcheckbutton(squishFrame, variable = squishVariable)

    spec.name.nonumberFrame <- tkframe(optionsLFrame)
    spec.name.nonumberVariable <- tclVar("1")
    spec.name.nonumberCheckBox <- ttkcheckbutton(spec.name.nonumberFrame, variable = spec.name.nonumberVariable)

    genus.2.flagFrame <- tkframe(optionsLFrame)
    genus.2.flagVariable <- tclVar("1")
    genus.2.flagCheckBox <- ttkcheckbutton(genus.2.flagFrame, variable = genus.2.flagVariable)

    species.2.flagFrame <- tkframe(optionsLFrame)
    species.2.flagVariable <- tclVar("1")
    species.2.flagCheckBox <- ttkcheckbutton(species.2.flagFrame, variable = species.2.flagVariable)

    punctuation.flagFrame <- tkframe(optionsLFrame)
    punctuation.flagVariable <- tclVar("1")
    punctuation.flagCheckBox <- ttkcheckbutton(punctuation.flagFrame, variable = punctuation.flagVariable)

    pointless.flagFrame <- tkframe(optionsLFrame)
    pointless.flagVariable <- tclVar("1")
    pointless.flagCheckBox <- ttkcheckbutton(pointless.flagFrame, variable = pointless.flagVariable)

    onlyLogFrame <- tkframe(optionsRFrame)
    onlyLogVariable <- tclVar("0")
    onlyLogCheckBox <- ttkcheckbutton(onlyLogFrame, variable = onlyLogVariable)
      
    onOK <- function(){
        logger(paste("Using active data set: ", .activeDataSet, sep=""))
        spec.full <- getSelection(spec.fullBox)
        if (length(spec.full) > 0) {if (spec.full == "<NONE>") {spec.full <- ""}}
        trinomial <- getSelection(trinomialBox)
        trinomial.paste <- paste("c('", paste(trinomial, collapse="', '"), "')", sep="")

        squish <- FALSE
        if (tclvalue(squishVariable) == 1) {squish <- TRUE}
        spec.name.nonumber <- FALSE
        if (tclvalue(spec.name.nonumberVariable) == 1) {spec.name.nonumber <- TRUE}
        genus.2.flag <- FALSE
        if (tclvalue(genus.2.flagVariable) == 1) {genus.2.flag <- TRUE}
        species.2.flag <- FALSE
        if (tclvalue(species.2.flagVariable) == 1) {species.2.flag <- TRUE}
        punctuation.flag <- FALSE
        if (tclvalue(punctuation.flagVariable) == 1) {punctuation.flag <- TRUE}
        pointless.flag <- FALSE
        if (tclvalue(pointless.flagVariable) == 1) {pointless.flag <- TRUE}

        command <- paste("WFO.prepare(", .activeDataSet, 
            ", spec.full='", spec.full, 
            "', squish=", squish, ", spec.name.nonumber=", spec.name.nonumber,
            ", genus.2.flag=", genus.2.flag, ", species.2.flag=", species.2.flag,
            ", punctuation.flag=", punctuation.flag, ", pointless.flag=", pointless.flag,
            ", trinomial=", trinomial.paste,
            ", verbose=TRUE, counter=1000)", sep="")
        logger(paste(.newActive, " <- ", command, sep=""))
        
        if (tclvalue(onlyLogVariable) == 0) {
            gassign(.newActive, justDoIt(command))
            activeDataSet(.newActive)
            logger(paste(.newActive, "now is the new active data set"))
            doItAndPrint(paste("print(data.table::as.data.table(", .newActive , "))", sep=""))  
        }
    }

    OKCancelHelp(helpSubject="WFO.prepare", reset="WFO.prepareGUI", apply="WFO.prepareGUI")
    tkgrid(getFrame(spec.fullBox), labelRcmdr(variablesFrame, text = "          "), 
         getFrame(trinomialBox), sticky = "nw")
    tkgrid(variablesFrame, sticky = "w")
    tkgrid(labelRcmdr(top, text=""))
    tkgrid(squishCheckBox, labelRcmdr(squishFrame, text=gettextRcmdr("squish")), sticky="w")
    tkgrid(squishFrame, sticky="w")
    tkgrid(spec.name.nonumberCheckBox, labelRcmdr(spec.name.nonumberFrame, text=gettextRcmdr("spec.name.nonumber")), sticky="w")
    tkgrid(spec.name.nonumberFrame, sticky="w")
    tkgrid(genus.2.flagCheckBox, labelRcmdr(genus.2.flagFrame, text=gettextRcmdr("genus.2.flag")), sticky="w")
    tkgrid(genus.2.flagFrame, sticky="w")
    tkgrid(species.2.flagCheckBox, labelRcmdr(species.2.flagFrame, text=gettextRcmdr("species.2.flag")), sticky="w")
    tkgrid(species.2.flagFrame, sticky="w")
    tkgrid(punctuation.flagCheckBox, labelRcmdr(punctuation.flagFrame, text=gettextRcmdr("punctuation.flag")), sticky="w")
    tkgrid(punctuation.flagFrame, sticky="w")
    tkgrid(pointless.flagCheckBox, labelRcmdr(pointless.flagFrame, text=gettextRcmdr("pointless.flag")), sticky="w")
    tkgrid(pointless.flagFrame, sticky="w")
    tkgrid(onlyLogCheckBox, labelRcmdr(onlyLogFrame, text=gettextRcmdr("Only generate script")), sticky="w")
    tkgrid(onlyLogFrame, sticky="w")
    tkgrid(optionsLFrame, labelRcmdr(optionsFrame, text = "                    "), optionsRFrame, sticky = "nw")
    tkgrid(optionsFrame, sticky="w") 
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

