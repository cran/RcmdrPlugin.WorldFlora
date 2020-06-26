WFO.oneGUI <- function(){
    .activeDataSet <- ActiveDataSet()
    .newActive <- paste(.activeDataSet, ".one", sep="")
    defaults <- list(initial.spec.name="spec.name", initial.Auth.dist="Auth.dist", initial.First.dist="First.dist")
    dialog.values <- getDialog("WFO.oneGUI", defaults)
    initializeDialog(title=gettextRcmdr("WFO.one"))

    variablesFrame <- tkframe(top)
    spec.nameBox <- variableListBox(variablesFrame, c(Variables(), "<NONE>"), selectmode="single",
        title=gettextRcmdr("spec.name (optional)"), listHeight=10,
        initialSelection=varPosn(dialog.values$initial.spec.name, type="all", vars=c(Variables(), "<NONE>")))
    Auth.distBox <- variableListBox(variablesFrame, c(Variables(), "<NONE>"), selectmode="single",
        title=gettextRcmdr("Auth.dist (optional)"), listHeight=10,
        initialSelection=varPosn(dialog.values$initial.Auth.dist, type="all", vars=c(Variables(), "<NONE>")))
    First.distBox <- variableListBox(variablesFrame, c(Variables(), "<NONE>"), selectmode="single",
        title=gettextRcmdr("First.dist (optional)"), listHeight=10,
        initialSelection=varPosn(dialog.values$initial.First.dist, type="all", vars=c(Variables(), "<NONE>")))

    optionsFrame <- tkframe(top)

    onlyLogFrame <- tkframe(optionsFrame)
    onlyLogVariable <- tclVar("0")
    onlyLogCheckBox <- ttkcheckbutton(onlyLogFrame, variable = onlyLogVariable)
      
    onOK <- function(){
        logger(paste("Using active data set: ", .activeDataSet, sep=""))
        spec.name <- getSelection(spec.nameBox)
        if (length(spec.name) > 0) {if (spec.name == "<NONE>") {spec.name <- ""}}
        Auth.dist <- getSelection(Auth.distBox)
        if (length(Auth.dist) > 0) {if (Auth.dist == "<NONE>") {Auth.dist <- ""}}
        First.dist <- getSelection(First.distBox)
        if (length(First.dist) > 0) {if (First.dist == "<NONE>") {First.dist <- ""}}
        
        command <- paste("WFO.one(", .activeDataSet, 
            ", priority='Accepted', spec.name='", spec.name, 
            "', Auth.dist='", Auth.dist, 
            "', First.dist='", First.dist, 
            "', verbose=TRUE, counter=1000)", sep="")
        logger(paste(.newActive, " <- ", command, sep=""))
        
        if (tclvalue(onlyLogVariable) == 0) {
            gassign(.newActive, justDoIt(command))
            activeDataSet(.newActive)
            logger(paste(.newActive, "now is the new active data set"))
            doItAndPrint(paste("print(data.table::as.data.table(", .newActive , "))", sep=""))  
        }
    }

    OKCancelHelp(helpSubject="WFO.match", reset="WFO.oneGUI", apply="WFO.oneGUI")
    tkgrid(getFrame(spec.nameBox), labelRcmdr(variablesFrame, text = "          "), 
         getFrame(Auth.distBox), labelRcmdr(variablesFrame, text = "          "),
         getFrame(First.distBox), sticky = "nw")
    tkgrid(variablesFrame, sticky = "w")
    tkgrid(labelRcmdr(top, text=""))
    tkgrid(onlyLogCheckBox, labelRcmdr(onlyLogFrame, text=gettextRcmdr("Only generate script")), sticky="w")
    tkgrid(onlyLogFrame, sticky="w")
    tkgrid(optionsFrame, sticky="w")  
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

