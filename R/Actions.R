#######################################################
# Miscellanea Actions performed when interacting with #
# widgets belonging to ReGenesees.GUI.                #
#######################################################

change.options <- function(){
####################################################
# Change some options when starting ReGenesees.GUI #
# and return a list with the old options (in order #
# to restore them on exit).                        #
####################################################
# Change some options at startup...
 # Language: english
   # NOTE: tcltk2 functions below would generate 2 ugly warnings
   #       despite working correctly: avoid it switching off warnings
old.warn <- options("warn"= -1) # Switch OFF
   oldlang <- getLanguage()
   setLanguage("en")
options(old.warn)               # Switch ON again

 # Max char to print (for show and edit functions)
   old.max.print <- options("max.print"=10^9)
 # Decimal point char is "."
   old.OutDec <- options("OutDec"=".")
 # Digits value is 7
   old.digits <- options("digits"=7)
 # Scipen penalty is 0
   old.scipen <- options("scipen"=0)
 # Prompt string is "> "
   old.prompt <- options("prompt"="> ")
 # Contrasts are identical for unordered and ordered factors
   old.contrasts <- options("contrasts"=c(unordered="contr.treatment",
                                          ordered="contr.treatment"))
 # ReGenesees options
   old.RG.lonely.psu <- options("RG.lonely.psu" = "fail")
   old.RG.ultimate.cluster <- options("RG.ultimate.cluster" = FALSE)
   old.RG.adjust.domain.lonely <- options("RG.adjust.domain.lonely" = FALSE)
   old.RG.warn.domain.lonely <- options("RG.warn.domain.lonely" = FALSE)

old.options <- list(oldlang = oldlang,
                    base.opt = c(old.max.print, old.OutDec, old.digits, old.scipen, old.prompt, old.contrasts),
                    RG.opt = c(old.RG.lonely.psu, old.RG.ultimate.cluster,
                               old.RG.adjust.domain.lonely, old.RG.warn.domain.lonely)
                    )
old.options
}


Upd.act.funs <- function(functionsMenu, surveydesignMenu, calibrationMenu){
###################################
# Updates the Functions menu tree #
###################################
# Are there objects of a given kind?
surveydata  <- length(listDataSets()) > 0
designobj   <- fIs.there.class("analytic")
dfpop       <- length(listDfpop()) > 0
pop.tot.obj <- fIs.there.class("pop.totals")
known.tot.obj <- fIs.there.data.class("pop.totals")
cal.obj <- fIs.there.class("cal.analytic")

# Start with ALL disabled
tkentryconfigure(functionsMenu,2,state="disabled")
tkentryconfigure(functionsMenu,3,state="disabled")
tkentryconfigure(functionsMenu,4,state="disabled")
# A global flag: is there any active entry?
any.active <- FALSE

if (surveydata && !designobj && !dfpop){
    # Activate ONLY e.svydesign in Survey Design menu
    tkentryconfigure(functionsMenu,2,state="active")
    tkentryconfigure(surveydesignMenu,0,state="active")
    tkentryconfigure(surveydesignMenu,1,state="disabled")
    tkentryconfigure(surveydesignMenu,2,state="disabled")
    tkentryconfigure(surveydesignMenu,3,state="disabled")
    any.active <- TRUE
      }

if (surveydata && !designobj && dfpop){
    # Activate ONLY e.svydesign in Survey Design menu
    tkentryconfigure(functionsMenu,2,state="active")
    tkentryconfigure(surveydesignMenu,0,state="active")
    tkentryconfigure(surveydesignMenu,1,state="disabled")
    tkentryconfigure(surveydesignMenu,2,state="disabled")
    tkentryconfigure(surveydesignMenu,3,state="disabled")

    # If dfpop is REALLY of class pop.totals,
    # activate ONLY fill.template (and possibly pop.desc) in Calibration menu
    if (pop.tot.obj){
         tkentryconfigure(functionsMenu,3,state="active")
         tkentryconfigure(calibrationMenu,0,state="disabled")
         if (known.tot.obj) {
             tkentryconfigure(calibrationMenu,1,state="active")
            }
         else {
             tkentryconfigure(calibrationMenu,1,state="disabled")
            }
         tkentryconfigure(calibrationMenu,2,state="active")
         tkentryconfigure(calibrationMenu,3,state="disabled")
         tkentryconfigure(calibrationMenu,4,state="disabled")
         tkentryconfigure(calibrationMenu,5,state="disabled")
        }
    any.active <- TRUE
      }

if (!surveydata && designobj && !dfpop){
    # Activate ONLY des.addvars and collapse.strata in Survey Design menu
    tkentryconfigure(functionsMenu,2,state="active")
    tkentryconfigure(surveydesignMenu,0,state="disabled")
    tkentryconfigure(surveydesignMenu,1,state="active")
    tkentryconfigure(surveydesignMenu,2,state="active")
    tkentryconfigure(surveydesignMenu,3,state="disabled")

    # Activate ONLY pop.template in Calibration menu
    tkentryconfigure(functionsMenu,3,state="active")
    tkentryconfigure(calibrationMenu,0,state="active")
    tkentryconfigure(calibrationMenu,1,state="disabled")
    tkentryconfigure(calibrationMenu,2,state="disabled")
    tkentryconfigure(calibrationMenu,3,state="disabled")
    if (cal.obj) {
        tkentryconfigure(calibrationMenu,4,state="active")
        tkentryconfigure(calibrationMenu,5,state="active")
    }
    else {
        tkentryconfigure(calibrationMenu,4,state="disabled")
        tkentryconfigure(calibrationMenu,5,state="disabled")
    }


    # Activate FULLY Estimates and Errors menu
    tkentryconfigure(functionsMenu,4,state="active")

    any.active <- TRUE
    }

if (surveydata && designobj && dfpop){
    # Activate (almost) ALL
      # Survey Design menu
    tkentryconfigure(functionsMenu,2,state="active")
    tkentryconfigure(surveydesignMenu,0,state="active")
    tkentryconfigure(surveydesignMenu,1,state="active")
    tkentryconfigure(surveydesignMenu,2,state="active")
    tkentryconfigure(surveydesignMenu,3,state="active")
     # Calibration menu
    tkentryconfigure(functionsMenu,3,state="active")
    tkentryconfigure(calibrationMenu,0,state="active")
    if (pop.tot.obj){
         if (known.tot.obj) {
             tkentryconfigure(calibrationMenu,1,state="active")
            }
         else {
             tkentryconfigure(calibrationMenu,1,state="disabled")
            }
         tkentryconfigure(calibrationMenu,2,state="active")
        }
    else {
         tkentryconfigure(calibrationMenu,1,state="disabled")
         tkentryconfigure(calibrationMenu,2,state="disabled")
        }
    tkentryconfigure(calibrationMenu,3,state="active")
    if (cal.obj) {
        tkentryconfigure(calibrationMenu,4,state="active")
        tkentryconfigure(calibrationMenu,5,state="active")
    }
    else {
        tkentryconfigure(calibrationMenu,4,state="disabled")
        tkentryconfigure(calibrationMenu,5,state="disabled")
    }

     # Estimates and Errors menu
    tkentryconfigure(functionsMenu,4,state="active")

    any.active <- TRUE
      }

if (surveydata && designobj && !dfpop){
    # Activate ALL Survey Design menu
    tkentryconfigure(functionsMenu,2,state="active")
    tkentryconfigure(surveydesignMenu,0,state="active")
    tkentryconfigure(surveydesignMenu,1,state="active")
    tkentryconfigure(surveydesignMenu,2,state="active")
    tkentryconfigure(surveydesignMenu,3,state="active")

    # Activate ONLY pop.template in Calibration menu
    tkentryconfigure(functionsMenu,3,state="active")
    tkentryconfigure(calibrationMenu,0,state="active")
    tkentryconfigure(calibrationMenu,1,state="disabled")
    tkentryconfigure(calibrationMenu,2,state="disabled")
    tkentryconfigure(calibrationMenu,3,state="disabled")
    tkentryconfigure(calibrationMenu,4,state="disabled")
    tkentryconfigure(calibrationMenu,5,state="disabled")

    # Activate Estimates and Errors menu
    tkentryconfigure(functionsMenu,4,state="active")

    any.active <- TRUE
      }

if (!surveydata && designobj && dfpop){
    # Activate ONLY des.addvars and collapse.strata in Survey Design menu
    tkentryconfigure(functionsMenu,2,state="active")
    tkentryconfigure(surveydesignMenu,0,state="disabled")
    tkentryconfigure(surveydesignMenu,1,state="active")
    tkentryconfigure(surveydesignMenu,2,state="active")
    tkentryconfigure(surveydesignMenu,3,state="active")

    # Activate ALL Calibration menu but fill.template and (possibly) pop.desc
    tkentryconfigure(functionsMenu,3,state="active")
    tkentryconfigure(calibrationMenu,0,state="active")
    if (known.tot.obj) {
         tkentryconfigure(calibrationMenu,1,state="active")
        }
    else {
         tkentryconfigure(calibrationMenu,1,state="disabled")
        }
    tkentryconfigure(calibrationMenu,2,state="disabled")
    tkentryconfigure(calibrationMenu,3,state="active")
    if (cal.obj) {
        tkentryconfigure(calibrationMenu,4,state="active")
        tkentryconfigure(calibrationMenu,5,state="active")
    }
    else {
        tkentryconfigure(calibrationMenu,4,state="disabled")
        tkentryconfigure(calibrationMenu,5,state="disabled")
    }

    # Activate ALL Estimates and Errors menu
    tkentryconfigure(functionsMenu,4,state="active")

    any.active <- TRUE
      }

# Reset activity flag...
assignTemp("any.active.menu", any.active, replace.existing = TRUE)
}


fOnCancel <- function(A_tt){
##############################################
# Actions performed when pressing CANCEL in  #
# several widgets created by ReGenesees.GUI. #
##############################################
#ReturnExit <- tclvalue(tkmessageBox(title="Exit",
#                                    message="Are you sure to exit?",
#                                    icon="question",
#                                    type="yesno",
#                                    default="yes"))
#if (ReturnExit=="yes"){
    tkgrab.release(A_tt)
    tkdestroy(A_tt)
#    }
}


fOnEXIT <- function(A_tt, textH, oldopt){
##############################################
# Actions performed when using the EXIT menu #
# of the main ReGenesees.GUI widget.         #
##############################################
SaveWorkspace <-tclvalue(tkmessageBox(title="Save Workspace",
                                      message="Do you want to save the current Workspace?",
                                      icon="question",
                                      type="yesno",
                                      default="yes",
                                      parent=A_tt))
if (SaveWorkspace=="yes"){
    fSave_Workspace_as(textH)
    }
SaveCommands <-tclvalue(tkmessageBox(title="Save Commands",
                                     message="Do you want to save the Commands History?",
                                     icon="question",
                                     type="yesno",
                                     default="yes",
                                     parent=A_tt))
if (SaveCommands=="yes"){
    fSave_History_as(textH)
    }
ReturnExit <-tclvalue(tkmessageBox(title="Exit",
                                   message="Are you sure to exit?",
                                   icon="question",
                                   type="yesno",
                                   default="yes",
                                   parent=A_tt))
if (ReturnExit=="yes"){
    tkgrab.release(A_tt)
    # restore old options
      # Language:
      # NOTE: tcltk2 function below would generate 2 ugly warnings
      #       despite working correctly: avoid it switching off warnings
old.warn <- options("warn"= -1) # Switch OFF
    setLanguage(oldopt$oldlang)
options(old.warn)               # Switch ON again
    options(oldopt$base.opt)
    options(oldopt$RG.opt)
      # change all widgets icons from RG icon to original TK icon
      img.path <- system.file("images", package = "ReGenesees.GUI")
      TK_icon <- tclVar()
      # tkimage.create("photo", TK_icon, file= paste(img.path, "//TK_icon.gif", sep="")) fix for OS independence
      tkimage.create("photo", TK_icon, file= file.path(img.path, "TK_icon.gif"))
      tcl("wm", "iconphoto", A_tt, "-default", TK_icon)
    tkdestroy(A_tt)
    }
}


RGflyer <- function(){
################################################
# Actions performed when pressing the Help     #
# menu of the ReGenesees system.               #
################################################
PKG.version <- packageDescription("ReGenesees")$Version
browseURL(paste(file.path(path.package(package="ReGenesees")[1], "doc"), "//",
                "ReGenesees", PKG.version, ".FLYER.pdf", sep=""))
# tkmessageBox(title ="User Guide", message = "Sorry, this item is still under construction", icon = "info")
}


fHelpGUI <- function(){
################################################
# Actions performed when pressing the          #
# "Help -> ReGenesees.GUI help" menu  of the   #
# ReGenesees system.                           #
################################################
print(help("ReGenesees.GUI"))
}


RGwebsite <- function(){
################################################
# Actions performed when pressing the Website  #
# menu of the ReGenesees system.               #
################################################
# NOTE: This is ReGenesees' GITHUB PAGES website
browseURL("https://diegozardetto.github.io/ReGenesees")
}



fOnFunctionHelp <- function(x) {
################################################
# Actions performed when pressing the ? button #
# several widgets created by ReGenesees.GUI.   #
################################################
print(help(x, try.all.packages=TRUE))
}


fSet_WRKDIR <- function(textHistory){
##########################################
# Widget for setting the user directory. #
##########################################
wrkdir <- tclvalue(tkchooseDirectory(initialdir=getwd()))
if (wrkdir != "") {
     setwd(wrkdir)
     # Print on the Commands Window
     commands <- paste('setwd("', wrkdir, '")\n', sep="")
     tkconfigure(textHistory, state="normal")
     tkinsert(textHistory, "end", commands)
     tkinsert(textHistory, "end", "\n")
     tkyview.moveto(textHistory, 1.0)
     tkconfigure(textHistory, state="disabled")
     # End

     # Flush commands to Rhistory file
     upd.Rhistory(commands, RG.stamp = TRUE)
     # End
    }
}


fSave_History_as <- function(textH){
#########################################
# Actions performed when asking to save #
# the current Commands History.         #
#########################################
historyFile <- tclvalue(tkgetSaveFile(filetypes="{{Script Files} {.Rhistory}}",
                                      defaultextension="Rhistory",
                                      initialfile="ReGenesees"))
if (historyFile != ""){
    script <- tclvalue(tkget(textH,"1.0", "end"))
    if (script == "\n"){
        tkmessageBox(title="Save Commands", message="No commands to be saved!", icon="warning")
        }
    else{
        historyFileW <- file(historyFile, "w")
        # Finalize commands log with a SESSION timestamp and R history file
        # with a MESSAGE
        msg <- "## ReGenesees commands history saved"
        session <- "## ReGenesees session timestamp:\n"
        # Flush save commands timestamp to Rhistory file
        # NOTE: This would identify univocally inside the Rhistory file *when*
        #       (i.e. at what line) ReGenesees commands have been saved
        upd.Rhistory(c(msg, session))
        # Function timestamp() flushes automatically to Rhistory file
        stamp <- paste(timestamp(prefix = "## ", suffix = "", quiet = TRUE), "\n")
        script <- paste(script, session, stamp, sep="")
        # done
        cat(script, file = historyFileW)
        close(historyFileW)
        tkmessageBox(title ="Save Commands", message = "Operation executed", icon = "info")
        }
    }
}


fSave_Workspace_as <- function(textHistory){
#########################################
# Actions performed when asking to save #
# the current Workspace.                #
#########################################

saveFile <- tclvalue(tkgetSaveFile(filetypes='{"All Files" {"*"}}',
                     defaultextension="",
                     initialfile=".RData"))
if (saveFile == "") return()
obj.names <- ls(envir=.GlobalEnv)
if (!length(obj.names)){
     tkmessageBox(title="Save Workspace", message="No objects to be saved!", icon="warning")
    }
else{
     save(list = obj.names, file=saveFile)
     # Print on the Commands Window
     commands <- paste('save(list= ls(), file= "',saveFile,'")\n', sep="")
     tkconfigure(textHistory, state="normal")
     tkinsert(textHistory, "end", commands)
     tkinsert(textHistory, "end", "\n")
     tkyview.moveto(textHistory, 1.0)
     tkconfigure(textHistory, state="disabled")
     # End

     # Flush commands to Rhistory file
     upd.Rhistory(commands, RG.stamp = TRUE)
     # End

     tkmessageBox(title ="Save Workspace",message = "Operation executed", icon = "info")
    }
}


fCarica_WS <- function(functionsMenu, surveydesignMenu, calibrationMenu, textHistory){
#########################################
# Actions performed when asking to load #
# an existing Workspace.                #
#########################################
RDataFile<-tclvalue(tkgetOpenFile(filetypes='{{R Data Files} {".RData" ".rda"}}'))

#choiceFile <- TRUE
#while (!nchar(RDataFile) && choiceFile){
#       ReturnValue <-tkmessageBox(title="Load Workspace",
#                                  message="No workspace file chosen! Do you want to exit?",
#                                  icon="question",
#                                  type="yesno",
#                                  default="yes")
#       if (tclvalue(ReturnValue)=="yes"){
#           choiceFile <- FALSE
#        }
#        else {
#           RDataFile<-tclvalue(tkgetOpenFile(filetypes="{{R Data Files} {.RData}} {{All files} *}"))
#        }
#    }
if (nchar(RDataFile)){
    load(RDataFile,envir = .GlobalEnv)
    Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)
    # Print on the Commands Window
    commands <- paste('load("',RDataFile,'")\n', sep="")
    tkconfigure(textHistory, state="normal")
    tkinsert(textHistory, "end", commands)
    tkinsert(textHistory, "end", "\n")
    tkyview.moveto(textHistory, 1.0)
    tkconfigure(textHistory, state="disabled")
    # End

    # Flush commands to Rhistory file
    upd.Rhistory(commands, RG.stamp = TRUE)
    # End

    tkmessageBox(title ="Load Workspace",message = "Operation executed", icon = "info")
#    if (length(ls(envir = .GlobalEnv)) == 0) tkmessageBox(title="Workspace",
#                                                          message = "It's empty!", icon = "warning")
    }
}


SetVarianceOptions <- function(textHistory) {
########################################
# Actions performed when asking to set #
# Variance Estimation options.         #
########################################
# Find path to the images folder...
img.path <- system.file("images", package = "ReGenesees.GUI")
# ... and get the ? OK nad CANCEL images
# 1) help question mark
image_qm <- tclVar()
# tkimage.create("photo", image_qm, file=paste(img.path, "//help.gif", sep="")) fix for OS independence
tkimage.create("photo", image_qm, file=file.path(img.path, "help.gif"))
# 2) ok flag
image_ok <- tclVar()
# tkimage.create("photo", image_ok, file=paste(img.path, "//ok.gif", sep="")) fix for OS independence
tkimage.create("photo", image_ok, file=file.path(img.path, "ok.gif"))
# 3) cancel
image_cancel <- tclVar()
# tkimage.create("photo", image_cancel, file=paste(img.path, "//cancel.gif", sep="")) fix for OS independence
tkimage.create("photo", image_cancel, file=file.path(img.path, "cancel.gif"))

# Create fonts
optTextLabel <- tkfont.create(family="Helvetica", weight="bold", size=10)

    tt <- tktoplevel()
    fDesignDialog(tt)
    tkwm.title(tt,"Set Variance Estimation Options")
    optionsFrame <- tkframe(tt)
    buttonsFrame <- tkframe(tt)

    # ultimate cluster approximation
    UC.curr <- unlist(options("RG.ultimate.cluster"))
    UC <- tclVar(UC.curr)
    UC.CheckBox <- ttkcheckbutton(optionsFrame, variable=UC)

    # lonely PSUs
    lpsu.curr <- unlist(options("RG.lonely.psu"))
    lpsu <- tclVar(lpsu.curr)

    lpsu.fail <- ttkradiobutton(optionsFrame)
    lpsu.remove <- ttkradiobutton(optionsFrame)
    lpsu.adjust <- ttkradiobutton(optionsFrame)
    lpsu.average <- ttkradiobutton(optionsFrame)

    lab.lpsu.fail <- ttklabel(optionsFrame,text="  fail", justify="left")
    lab.lpsu.remove <- ttklabel(optionsFrame,text="  remove", justify="left")
    lab.lpsu.adjust <- ttklabel(optionsFrame,text="  adjust", justify="left")
    lab.lpsu.average <- ttklabel(optionsFrame,text="  average", justify="left")

    tkconfigure(lpsu.fail, variable=lpsu, value="fail")
    tkconfigure(lpsu.remove, variable=lpsu, value="remove")
    tkconfigure(lpsu.adjust, variable=lpsu, value="adjust")
    tkconfigure(lpsu.average, variable=lpsu, value="average")

    # domain lonely PSUs
    domlpsu.curr <- unlist(options("RG.adjust.domain.lonely"))
    domlpsu <- tclVar(domlpsu.curr)
    domlpsu.CheckBox <- ttkcheckbutton(optionsFrame, variable=domlpsu)

    warn.domlpsu.curr <- unlist(options("RG.warn.domain.lonely"))
    warn.domlpsu <- tclVar(warn.domlpsu.curr)
    warn.domlpsu.CheckBox <- ttkcheckbutton(optionsFrame, variable=warn.domlpsu)

    # Create Buttons
    ok.but <- tk2button(buttonsFrame, text="OK",
                        image=image_ok, compound="left",
                        command=function() fOnRun())
    Cancel.but <- tk2button(buttonsFrame, text="Cancel",
                            image=image_cancel, compound="left",
                            command=function() fOnCancel(tt))
    FunctionHelp.but <- tk2button(buttonsFrame, text="Function Help",
                                  image=image_qm, compound="left", tip=descFun("ReGenesees.options"),
                                  command=function() fOnFunctionHelp("ReGenesees.options"))

    tkgrid(ttklabel(optionsFrame, text="\n"))
    tkgrid(ttklabel(optionsFrame, text=" Variance Estimation for Multistage Designs",
                    font=optTextLabel, foreground= "blue"), sticky="w")
    tkgrid(ttklabel(optionsFrame, text="  Ultimate Cluster Approximation:  "),
           UC.CheckBox, sticky="w")

    tkgrid(ttklabel(optionsFrame, text="\n"))
    tkgrid(ttklabel(optionsFrame, text=" Lonely PSUs Treatment", font=optTextLabel,
                    foreground= "blue"), sticky="w")
    tkgrid(lab.lpsu.fail, lpsu.fail, sticky="w")
    tkgrid(lab.lpsu.remove, lpsu.remove, sticky="w")
    tkgrid(lab.lpsu.adjust, lpsu.adjust, sticky="w")
    tkgrid(lab.lpsu.average, lpsu.average, sticky="w")

    tkgrid(ttklabel(optionsFrame, text="\n"))
    tkgrid(ttklabel(optionsFrame, text=" Domain Estimation",
                    font=optTextLabel, foreground= "blue"), sticky="w")
    tkgrid(ttklabel(optionsFrame, text="  Adjust lonely PSUs in domains:  "),
                    domlpsu.CheckBox, sticky="w")
    tkgrid(ttklabel(optionsFrame, text="  Warn of lonely PSUs in domains:  "),
                    warn.domlpsu.CheckBox, sticky="w")

    tkgrid(ttklabel(optionsFrame, text="\n"))
    tkgrid(ok.but, Cancel.but, FunctionHelp.but)

    tkgrid(optionsFrame)
    tkgrid(buttonsFrame)
    tkgrid.configure(buttonsFrame, pady=c(0, "0.2c"), padx=5)
    tkgrid(ok.but, Cancel.but, FunctionHelp.but)
    tkgrid.configure(optionsFrame, sticky="w")

    fOnRun <- function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        UC.new <- tclvalue(UC) == 1
        options("RG.ultimate.cluster" = UC.new)
        lpsu.new <- tclvalue(lpsu)
        options("RG.lonely.psu" = lpsu.new)
        domlpsu.new <- tclvalue(domlpsu) == 1
        options("RG.adjust.domain.lonely" = domlpsu.new)
        warn.domlpsu.new <- tclvalue(warn.domlpsu) == 1
        options("RG.warn.domain.lonely" = warn.domlpsu.new)
        # Print on the Commands Window
          UC.chr <- paste("options(\"RG.ultimate.cluster\" = ", UC.new, ")\n", sep="")
          lpsu.chr <- paste("options(\"RG.lonely.psu\" = \"", lpsu.new, "\")\n", sep="")
          dom.lpsu.chr <- paste("options(\"RG.adjust.domain.lonely\" = ", domlpsu.new, ")\n", sep="")
          warn.dom.lpsu.chr <- paste("options(\"RG.warn.domain.lonely\" = ", warn.domlpsu.new, ")\n", sep="")
          tkconfigure(textHistory, state="normal")
          tkinsert(textHistory, "end", UC.chr)
          tkinsert(textHistory, "end", lpsu.chr)
          tkinsert(textHistory, "end", dom.lpsu.chr)
          tkinsert(textHistory, "end", warn.dom.lpsu.chr)
          tkinsert(textHistory, "end", "\n")
          tkyview.moveto(textHistory, 1.0)
          tkconfigure(textHistory, state="disabled")
        # End

        # Flush commands to Rhistory file
          upd.Rhistory(c(UC.chr, lpsu.chr, dom.lpsu.chr, warn.dom.lpsu.chr), RG.stamp = TRUE)
        # End
    }
}


SetEnvOptions <- function(textHistory){
########################################
# Actions performed when asking to set #
# general Environment options.         #
# Currently manages the following:     #
# - digits                             #
# - scipen                             #
# - prompt                             #
########################################

# Find path to the images folder...
img.path <- system.file("images", package = "ReGenesees.GUI")
# ... and get the ? OK nad CANCEL images
# 1) help question mark
image_qm <- tclVar()
# tkimage.create("photo", image_qm, file=paste(img.path, "//help.gif", sep="")) fix for OS independence
tkimage.create("photo", image_qm, file=file.path(img.path, "help.gif"))
# 2) ok flag
image_ok <- tclVar()
# tkimage.create("photo", image_ok, file=paste(img.path, "//ok.gif", sep="")) fix for OS independence
tkimage.create("photo", image_ok, file=file.path(img.path, "ok.gif"))
# 3) cancel
image_cancel <- tclVar()
# tkimage.create("photo", image_cancel, file=paste(img.path, "//cancel.gif", sep="")) fix for OS independence
tkimage.create("photo", image_cancel, file=file.path(img.path, "cancel.gif"))

# Create font
optTextLabel <- tkfont.create(family="Helvetica", weight="bold",size=10)

tt <- tktoplevel()
fDesignDialog(tt)
tkwm.title(tt,"Set R Environment Options")
optionsFrame <- tkframe(tt)
buttonsFrame <- tkframe(tt)

########################################
# Slider for parameter 'digits' STARTS #
########################################
labellblfDigits <- ttklabel(optionsFrame,text="  digits  ", compound="right",
                            font=optTextLabel, foreground= "blue")
lblfDigits<- ttklabelframe(optionsFrame, labelwidget=labellblfDigits)
# Slider for parameter 'digits'
# Meaningful integer values for digits
allDigits <- 1:15
# Get current value
curr.digits <- unlist(options("digits"))
# Check if current value belongs to the meaningful set
in.digits <- curr.digits %in% allDigits

# Slider variable (identifies the actual element of allDigits...)
  # Initialization:
  digits.start <- if (in.digits) {
                      as.character(which(allDigits==curr.digits))
                    }
                  else {
                      as.character(which(allDigits==7))
                    }

  SliderValue.D <- tclVar(digits.start)
  Digits <- tclVar(as.character(allDigits[as.integer(tclvalue(SliderValue.D))]))
  # DEBUG 08/05/2020: due to R 4.0.0 had to explicitly use as.character(tclvalue())
  entry.Digits <- tklabel(lblfDigits, text=as.character(tclvalue(Digits)), foreground="red")
# Update Digits when using the slider...
upd.D <- function(...) {
 # NOTE: global tcl variable Digits will be then used by
     Digits <<- tclVar(as.character(allDigits[as.integer(tclvalue(SliderValue.D))]))
     tkconfigure(entry.Digits, textvariable=Digits)
    }
# Generate the slider widget
DigitsSlider <- tkscale(lblfDigits, from=1, to=length(allDigits), command = upd.D,
    showvalue=F, variable=SliderValue.D,
    resolution=1, orient="horizontal")
######################################
# Slider for parameter 'digits' ENDS #
######################################

########################################
# Slider for parameter 'scipen' STARTS #
########################################
labellblfScipen <- ttklabel(optionsFrame,text="  scipen  ", compound="right",
                            font=optTextLabel, foreground= "blue")
lblfScipen<- ttklabelframe(optionsFrame, labelwidget=labellblfScipen)
# Meaningful integer values for scipen
# NOTE: Must tell to tcltk2 maintainer that if scipen <= -5 !!!!!!!!!!!!!!!!!!!!
#       the whole package crashes (at least for R 2.13.0)!!!!!!!!!!!!!!!!!!!!!!!
# allScipen <- c(-20, -15, -12, -10:10, 12, 15, 20)
allScipen <- c(rep(-4:-1, each=3), 0, 1:10, 12, 15, 20, 999)
# Get current value
curr.scipen <- unlist(options("scipen"))
# Check if current value belongs to the meaningful set
in.scipen <- curr.scipen %in% allScipen

# Slider variable (identifies the actual element of allScipen...)
  # Initialization:
  scipen.start <- if (in.scipen) {
                      # DEBUG 17/02/2015
                      # As allScipen has repeated value, must pick only one
                      # among possibly multiple matches, e.g. the first
                      as.character(which(allScipen==curr.scipen)[1])
                    }
                  else {
                      as.character(which(allScipen==0))
                    }
  SliderValue.S <- tclVar(scipen.start)
  Scipen <- tclVar(as.character(allScipen[as.integer(tclvalue(SliderValue.S))]))
  # DEBUG 08/05/2020: due to R 4.0.0 had to explicitly use as.character(tclvalue())
  entry.Scipen <- tklabel(lblfScipen, text=as.character(tclvalue(Scipen)), foreground="red")
# Update Scipen when using the slider...
upd.S <- function(...) {
 # NOTE: global tcl variable Scipen will be then used by
     Scipen <<- tclVar(as.character(allScipen[as.integer(tclvalue(SliderValue.S))]))
     tkconfigure(entry.Scipen, textvariable=Scipen)
    }
# Generate the slider widget
ScipenSlider <- tkscale(lblfScipen, from=1, to=length(allScipen), command = upd.S,
    showvalue=F, variable=SliderValue.S,
    resolution=1, orient="horizontal")
######################################
# Slider for parameter 'scipen' ENDS #
######################################

###########################################
# Slider for parameter 'font size' STARTS #
###########################################
labellblfFontSize <- ttklabel(optionsFrame, text="  font size  ", compound="right",
                              font=optTextLabel, foreground="blue")
lblfFontSize <- ttklabelframe(optionsFrame, labelwidget=labellblfFontSize)
# Slider for parameter 'font size'
# Meaningful integer values for font size
allFontSize <- 8:12
# Get current values of TkDefaultFont and TkTextFont: should they differ,
# get both and try to push toward their minimum
s.TDF <- get.fontsize("TkDefaultFont")
s.TTF <- get.fontsize("TkTextFont")
s.curr <- min(s.TDF, s.TTF)
# Check if current values belongs to the meaningful set
in.size <- all(c(s.TDF, s.TTF) %in% allFontSize)

# Slider variable (identifies the actual element of allFontSize...)
  # Initialization:
  size.start <- if (in.size) {
                      as.character(which(allFontSize==s.curr))
                    }
                else {
                      # Get intro TDF size and choose
                      # the nearest allFontSize value
                      s.TDF.intro <- getTemp("TDF.intro")$size
                      nearest <- which.min(abs(allFontSize - s.TDF.intro))
                      # as.character(which(allFontSize==8))
                      as.character(nearest)
                    }

  SliderValue.FS <- tclVar(size.start)
  FontSize <- tclVar(as.character(allFontSize[as.integer(tclvalue(SliderValue.FS))]))
  # DEBUG 08/05/2020: due to R 4.0.0 had to explicitly use as.character(tclvalue())
  entry.FontSize <- tklabel(lblfFontSize, text=as.character(tclvalue(FontSize)), foreground="red")
# Update FontSize when using the slider...
upd.D <- function(...) {
 # NOTE: global tcl variable FontSize will be then used by
     FontSize <<- tclVar(as.character(allFontSize[as.integer(tclvalue(SliderValue.FS))]))
     # DEBUG 08/05/2020: due to R 4.0.0 had to explicitly use as.character(tclvalue())
     tkconfigure(entry.FontSize, textvariable=FontSize)
    }
# Generate the slider widget
FontSizeSlider <- tkscale(lblfFontSize, from=1, to=length(allFontSize), command = upd.D,
    showvalue=F, variable=SliderValue.FS,
    resolution=1, orient="horizontal")
#########################################
# Slider for parameter 'font size' ENDS #
#########################################

##########################################
# Combobox for parameter 'prompt' STARTS #
##########################################
labellblfPrompt <- ttklabel(optionsFrame,text="  prompt  ", compound="right",
                            font=optTextLabel, foreground= "blue")
lblfPrompt<- ttklabelframe(optionsFrame, labelwidget=labellblfPrompt)
listaPrompt <- tclVar()

# Meaningful values for prompt
allPrompt <- c("> ", "RG> ")
# Get current value
curr.prompt <- unlist(options("prompt"))
# Check if current value belongs to the meaningful set
in.prompt <- curr.prompt %in% allPrompt
# Initialization:
prompt.start <- if (in.prompt) {
                     curr.prompt
                    }
                else {
                      allPrompt[1]
                    }
tclObj(listaPrompt) <- allPrompt
cbPrompt <- tk2combobox(lblfPrompt, values = allPrompt, state="readonly",
                        justify = "center", width = 10, foreground="red")
PromptDef <- tclVar(prompt.start)
########################################
# Combobox for parameter 'prompt' ENDS #
########################################

#################
# Buttons START #
#################
ok.but <- tk2button(buttonsFrame, text="OK",
                    image=image_ok, compound="left",
                    command=function() fOnRun())
Cancel.but <- tk2button(buttonsFrame, text="Cancel",
                        image=image_cancel, compound="left",
                        command=function() fOnCancel(tt))
FunctionHelp.but <- tk2button(buttonsFrame, text="Function Help",
                              image=image_qm, compound="left", tip=descFun("options"),
                              command=function() fOnFunctionHelp("options"))
###############
# Buttons END #
###############


digits.text <- "Controls the number of digits\nto print when printing numeric\nvalues. It is a suggestion only."
tkgrid(lblfDigits, ttklabel(optionsFrame, text=digits.text, foreground= "blue"), sticky="ns")
tkgrid.configure(lblfDigits, padx="0.4c", pady=c("0.8c","0.4c"))
tkgrid(entry.Digits)
tkgrid.configure(entry.Digits, sticky="we")
tkgrid(DigitsSlider)
tkgrid.configure(DigitsSlider, sticky="we", padx="0.2c", pady=c(0,"0.2c"))

scipen.text <- "A penalty to be applied when\ndeciding to print numeric values\nin fixed or scientific notation:\npositive    ->   fixed\nnegative  ->   scientific"
tkgrid(lblfScipen, ttklabel(optionsFrame, text=scipen.text, foreground= "blue"), sticky="ns")
tkgrid.configure(lblfScipen, padx="0.4c", pady=c("0.4c","0.4c"))
tkgrid(entry.Scipen)
tkgrid.configure(entry.Scipen, sticky="we")
tkgrid(ScipenSlider)
tkgrid.configure(ScipenSlider, sticky="we", padx="0.2c", pady=c(0,"0.2c"))

fontsize.text <- "Controls the size of the fonts\nused in many (yet not all) text\nfields of the GUI. Values > 10\ncan make the GUI less pretty."
tkgrid(lblfFontSize, ttklabel(optionsFrame, text=fontsize.text, foreground= "blue"), sticky="ns")
tkgrid.configure(lblfFontSize, padx="0.4c", pady=c("0.4c","0.4c"))
tkgrid(entry.FontSize)
tkgrid.configure(entry.FontSize, sticky="we")
tkgrid(FontSizeSlider)
tkgrid.configure(FontSizeSlider, sticky="we", padx="0.2c", pady=c(0,"0.2c"))

prompt.text <- "A non-empty string to be used\nfor R's command line prompt.\n"
tkgrid(lblfPrompt, ttklabel(optionsFrame, text=prompt.text, foreground= "blue"), sticky="ns")
tkgrid.configure(lblfPrompt,padx="0.4c", pady=c("0.4c","0.8c"))
tkconfigure(cbPrompt, textvariable = PromptDef)
tkgrid(cbPrompt)
tkgrid.configure(cbPrompt, sticky="we", padx="0.4c", pady="0.3c")

tkgrid(ok.but, Cancel.but, FunctionHelp.but)

tkgrid(optionsFrame)
tkgrid(buttonsFrame)
tkgrid.configure(buttonsFrame, pady="0.2c", padx="0.4c")
tkgrid(ok.but, Cancel.but, FunctionHelp.but)
tkgrid.configure(optionsFrame, sticky="w")

fOnRun <- function(){
  tkgrab.release(tt)
  digits.new <- as.integer(tclvalue(Digits))
  options("digits" = digits.new)
  scipen.new <- as.integer(tclvalue(Scipen))
  options("scipen" = scipen.new)
  fontsize.new <- as.integer(tclvalue(FontSize))
  prompt.new <- as.character(tclvalue(PromptDef))
  options("prompt" = prompt.new)
  tkfont.configure("TkDefaultFont", size = fontsize.new)
  tkfont.configure("TkTextFont",    size = fontsize.new)

    # Print on the Commands Window
    digits.chr   <- paste("options(\"digits\" = ", digits.new, ")\n", sep="")
    scipen.chr   <- paste("options(\"scipen\" = ", scipen.new, ")\n", sep="")
    prompt.chr   <- paste("options(\"prompt\" = \"", prompt.new, "\")\n", sep="")
    fontsize.chr <- paste("# tkfont.configure(\"TkDefaultFont\", size = ", fontsize.new,
                          ")\n# tkfont.configure(\"TkTextFont\", size = ", fontsize.new,
                          ")\n", sep="")
    tkconfigure(textHistory, state="normal")
    tkinsert(textHistory, "end", digits.chr)
    tkinsert(textHistory, "end", scipen.chr)
    tkinsert(textHistory, "end", prompt.chr)
    tkinsert(textHistory, "end", fontsize.chr)
    tkinsert(textHistory, "end", "\n")
    tkyview.moveto(textHistory, 1.0)
    tkconfigure(textHistory, state="disabled")
    # End

    # Flush commands to Rhistory file
    upd.Rhistory(c(digits.chr, scipen.chr, prompt.chr, fontsize.chr), RG.stamp = TRUE)
    # End

  tkdestroy(tt)
  }
}


SetCalmodelOptions <- function(textHistory){
########################################
# Actions performed when asking to set #
# Calibration Models options.          #
# Currently manages the following:     #
# - Contrast                           #
########################################

# Find path to the images folder...
img.path <- system.file("images", package = "ReGenesees.GUI")
# ... and get the ? OK nad CANCEL images
# 1) help question mark
image_qm <- tclVar()
# tkimage.create("photo", image_qm, file=paste(img.path, "//help.gif", sep="")) fix for OS independence
tkimage.create("photo", image_qm, file=file.path(img.path, "help.gif"))
# 2) ok flag
image_ok <- tclVar()
# tkimage.create("photo", image_ok, file=paste(img.path, "//ok.gif", sep="")) fix for OS independence
tkimage.create("photo", image_ok, file=file.path(img.path, "ok.gif"))
# 3) cancel
image_cancel <- tclVar()
# tkimage.create("photo", image_cancel, file=paste(img.path, "//cancel.gif", sep="")) fix for OS independence
tkimage.create("photo", image_cancel, file=file.path(img.path, "cancel.gif"))

# Create font
optTextLabel <- tkfont.create(family="Helvetica", weight="bold",size=10)

tt <- tktoplevel()
fDesignDialog(tt)
tkwm.title(tt,"Set Calibration Models Options")
optionsFrame <- tkframe(tt)
buttonsFrame <- tkframe(tt)

####################################
# Radiobutton for Contrasts STARTS #
####################################
labellblfContrasts <- ttklabel(optionsFrame,text="  Contrasts  ", compound="right",
                               font=optTextLabel, foreground= "blue")
lblfContrasts <- ttklabelframe(optionsFrame, labelwidget=labellblfContrasts)

ContrastsON  <- ttkradiobutton(lblfContrasts)
ContrastsOFF <- ttkradiobutton(lblfContrasts)

# Get current value
curr.Contrasts <- unique(unlist(options("contrasts")))
if ((length(curr.Contrasts) > 1) || (curr.Contrasts!="contr.off")){
     ValueContrasts <- tclVar("ON")
}
else {
     ValueContrasts <- tclVar("OFF")
}

tkconfigure(ContrastsON, variable=ValueContrasts,value="ON")
tkconfigure(ContrastsOFF,variable=ValueContrasts,value="OFF")

labelContrastsON  <- ttklabel(lblfContrasts,text="ON  ")
labelContrastsOFF <- ttklabel(lblfContrasts,text="OFF ")

##################################
# Radiobutton for Contrasts ENDS #
##################################

#################
# Buttons START #
#################
ok.but <- tk2button(buttonsFrame, text="OK",
                    image=image_ok, compound="left",
                    command=function() fOnRun())
Cancel.but <- tk2button(buttonsFrame, text="Cancel",
                        image=image_cancel, compound="left",
                        command=function() fOnCancel(tt))
FunctionHelp.but <- tk2button(buttonsFrame, text="Function Help",
                              image=image_qm, compound="left", tip=descFun("contrasts.RG"),
                              command=function() fOnFunctionHelp("contrasts.RG"))
###############
# Buttons END #
###############

Contrasts.text <- "Contrasts control the way symbolic\ncalibration models are translated\nto numeric model-matrices.\n\nON  ->  EFFICIENT option\nOFF ->  SAFE option\n"

tkgrid(lblfContrasts, ttklabel(optionsFrame, text=Contrasts.text, foreground= "blue"), sticky="s")
tkgrid.configure(lblfContrasts, padx="0.4c", pady=c("0.8c","0.8c"))

tkgrid(labelContrastsON, ContrastsON)
tkgrid(labelContrastsOFF,ContrastsOFF)

tkgrid.configure(labelContrastsON, sticky="w", padx=c("0.6c",0))
tkgrid.configure(labelContrastsOFF, sticky="w", padx=c("0.6c",0))
tkgrid.configure(lblfContrasts,padx=c("0.4c", "0.6c"))

tkgrid(ok.but, Cancel.but, FunctionHelp.but)

tkgrid(optionsFrame)
tkgrid(buttonsFrame)
tkgrid.configure(buttonsFrame, pady="0.2c", padx="0.4c")
tkgrid(ok.but, Cancel.but, FunctionHelp.but)
tkgrid.configure(optionsFrame, sticky="w")

fOnRun <- function(){
  tkgrab.release(tt)
  Contrasts.new <- as.character(tclvalue(ValueContrasts))
  Contrasts.cmd <- switch(Contrasts.new, "ON"  = "contrasts.RG()",
                                         "OFF" = "contrasts.off()")
  # Execute statement
  eval(parse(text=Contrasts.cmd))
    # Print on the Commands Window
    commands <- paste(Contrasts.cmd, "\n", sep="")
    tkconfigure(textHistory, state="normal")
    tkinsert(textHistory, "end", commands)
    tkinsert(textHistory, "end", "\n")
    tkyview.moveto(textHistory, 1.0)
    tkconfigure(textHistory, state="disabled")
    # End

    # Flush commands to Rhistory file
    upd.Rhistory(commands, RG.stamp = TRUE)
    # End

  tkdestroy(tt)
  }
}


