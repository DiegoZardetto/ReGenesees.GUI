#####################################################
# Miscellanea Tools: utility functions exploited by #
# the ReGenesees.GUI driver function.               #
#####################################################

fDesignDialog <- function(ttDialog){
#########################################
# Builds a "standard" dialog box: it is #
# modal, not resizable and focused.     #
#########################################
frameGlobal<- tkframe(ttDialog, borderwidth= 2)
tkwm.deiconify(ttDialog)
tkgrab.set(ttDialog)
tkfocus(ttDialog)
tkwm.resizable(ttDialog, 0, 0)
}


MsgR <- function(message,
                 icon=c("info", "question", "warning", "error"),
                 type=c("okcancel", "yesno", "ok"), title="", parent){
###########################################################################
# Draws a (modal) message box. Argument parent must be passed in order to #
# prevent the focus to be restituted to the window generating the message #
# BEFORE having detroyed the message box.                                 #
###########################################################################
if (missing(parent)){
     return(tkmessageBox(message=message, icon=icon, type=type, title=title))
    }
else {
     return(tkmessageBox(message=message, icon=icon, type=type, title=title, parent=parent))
    }
}


fIs.there.class <- function(class) {
#############################################
# Check for objects inheriting from a       #
# given class inside the current Workspace. #
#############################################
any(sapply(ls(.GlobalEnv), function(x) inherits(eval(parse(text=x), envir = .GlobalEnv), class)))
}

fIs.there.data.class <- function(class) {
#########################################
# Check for objects of given main class #
# inside the current Workspace.         #
#########################################
any(sapply(ls(.GlobalEnv), function(x) data.class(eval(parse(text=x), envir = .GlobalEnv)) == class))
}

inherits.obj <- function(class) {
#########################################
# Flags objects inheriting from a given #
# class inside the current Workspace.   #
#########################################
sapply(ls(.GlobalEnv), function(x) inherits(eval(parse(text=x), envir = .GlobalEnv), class))
}


fTransfer <- function(lstTransfer, lstEC_DataFrame, x){
#######################################################
# Transfers variables (one or more) from a listbox to #
# another: used to select variables in order to build #
# formulae.                                           #
#######################################################
selection <- as.integer(tkcurselection(lstEC_DataFrame))
sleng <- length(selection)
if (sleng==0){
    tkmessageBox(title="Selection", message = "Please select a variable", icon = "warning")
    }
else{
    for (i in 1:sleng){
         choice <- as.character(tclObj(x))[as.integer(tkcurselection(lstEC_DataFrame))+1]
         tkinsert(lstTransfer, "end", choice[i])
        }
    while (sleng>0){
           choice <- as.character(tclObj(x))[as.integer(tkcurselection(lstEC_DataFrame))+1]
           tkdelete(lstEC_DataFrame, selection[sleng])
           sleng <- sleng - 1
        }
    }
}


fDetransfer <- function(lstRitransfer, lstEC_DataFrame, x){
num <- as.integer(tksize(lstRitransfer))
if  (num ==0){
     tkmessageBox(title="Selection", message="No element transferred",icon = "warning")
    }
else {
     indicesel <- as.integer(tkcurselection(lstRitransfer))
     sleng <- length(indicesel)
     if (sleng==0){
         tkmessageBox(title="Selection", message = "Please select an element", icon = "warning")
        }
     else{
          for (i in 1:sleng){
               sel <- as.character(tclObj(x))[as.integer(tkcurselection(lstRitransfer))+1]
               tkinsert(lstEC_DataFrame, "end", sel[i])
            }
          while (sleng>0){
                 sel <- as.character(tclObj(x))[as.integer(tkcurselection(lstRitransfer))+1]
                 tkdelete(lstRitransfer, indicesel[sleng])
                 sleng <- sleng - 1
            }
        }
    }
}


fTransfer_ND <- function(lstTransfer, lstEC_DataFrame, x){
selection <- as.integer(tkcurselection(lstEC_DataFrame))
sleng <- length(selection)
if (sleng==0){
    tkmessageBox(title="Selection", message = "Please select a variable", icon = "warning")
    }
else{
    for (i in 1:sleng){
        choice <- as.character(tclObj(x))[as.integer(tkcurselection(lstEC_DataFrame))+1]
        tkinsert(lstTransfer, "end", choice[i])
        }
    }
}


fCancel <- function(lstDelete, lstEC_DataFrame, x){
num <- as.integer(tksize(lstDelete))
if  (num == 0){
     tkmessageBox(title="Selection", message="No element cancelled",icon = "warning")
    }
else{
    indicesel <- as.integer(tkcurselection(lstDelete))
    sleng <- length(indicesel)
    if (sleng == 0)
        tkmessageBox(title="Selection", message = "Please select an element", icon = "warning")
    else{
        while (sleng>0){
               sel <- as.character(tclObj(x))[as.integer(tkcurselection(lstDelete))+1]
               tkdelete(lstDelete, indicesel[sleng])
               sleng <- sleng - 1
            }
        }
    }
}


Lancia <- function(expr, WarnWindow, parent = "."){
#############################################
# Basic facility for executing commands and #
# handling exceptions (errors + warnings).  #
#############################################
# Reset Warning flag...
assignTemp("there.are.warnings", FALSE, replace.existing = TRUE)
# Sink the message stream
old.op <- options("warn"=1)
messages.connection <- file(open="w+")
sink(messages.connection, type="message")
on.exit({
          display.warn(messages.connection, WarnWindow)
          sink(type="message")
          close(messages.connection)
          options(old.op)
          # After computing, just before returning, let's
          # collect garbage to free memory...
          gc()
          # ...should help when data get huge
          # Try to fight the "loosing WarnWindoW" bug, which has
          # anyway to be better fixed in the future (see 'BUGS' file)
          # closeAllConnections()
        })
result <- try(expr,TRUE)

if (data.class(result)=="try-error"){
    # 1) cut the piece involving the invoked Fun...
    msgResult <- unlist(strsplit(as.character(result),": "))
    msgFun <- msgResult[1]
    # 2) remove actual arguments (which are not meaningful for the user)...
    msgFun <- unlist(strsplit(as.character(msgFun),"\\("))[1]
    # 3) the rest is the recomposed Error description
    msgErr <- msgResult[-1]
    msgErr <- paste(msgErr, collapse=": ")
    # 4) paste all with a carriage return
    msgOut <- paste(msgFun, msgErr, sep="\n")
    # 5) remove double '\n' (if any)
    msgOut <- gsub('\n\n', '\n', msgOut)
    tkmessageBox(title="An error has occurred!", message=msgOut, icon="error", type="ok", parent = parent)
    }
return(result)
}


fremoveObjs <- function(functionsMenu, surveydesignMenu, calibrationMenu, textHistory){
###########################################
# Actions performed when asking to remove #
# some objects in the current Workspace.  #
###########################################
rm.them <- function(objs){
 more <- length(objs) > 1
 if (more) {
     msg <- "Do you really want to remove objects: "
    }
 else {
     msg <- "Do you really want to remove object: "
    }
 ans <- tclvalue(tkmessageBox(title="Remove Objects",
                              message=paste(msg, paste(objs, collapse=", "), "?", sep=""),
                              icon="question",
                              type="yesno",
                              default="yes"))
if (ans=="yes"){
    rm(list=objs, envir=.GlobalEnv)
    Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

# Print on the Commands Window
commands <- paste("rm(",paste(objs,collapse=", ",sep=""),")\n", sep="")
tkconfigure(textHistory, state="normal")
tkinsert(textHistory, "end", commands)
tkinsert(textHistory, "end", "\n")
tkyview.moveto(textHistory, 1.0)
tkconfigure(textHistory, state="disabled")
# End

# Flush commands to Rhistory file
upd.Rhistory(commands, RG.stamp = TRUE)
# End

    tkmessageBox(title ="Remove Objects", message = "Operation executed", icon = "info")
    }
else{
    selectObjs(action = rm.them, selectmode="extended")
    }
 }
selectObjs(action = rm.them, selectmode="extended")
}


selectObjs <- function(action=print, selectmode, ...){
##################################################
# Build a List Box (with scrollbar) storing the  #
# names of the objects found inside the current  #
# Workspace.                                     #
# One or more objects are eventually selected    #
# and a given action is performed on them.       #
##################################################

# Seek object in Workspace...
obj.names <- ls(.GlobalEnv)
if (length(obj.names)==0) {
    tkmessageBox(title="Remove Objects",
                 message = "No objects found in the current Workspace!",
                 icon = "warning")
    return(invisible(NULL))
    }
objects <- sort(obj.names)
# Build the List Box
listBox(objects, title = "Remove Objects",
        label = " Please select one or more objects to be deleted",
        action=action, selectmode=selectmode, ...)
}


Obj.browser <- function() {
#################################
# Browse the current Workspace. #
#################################

# Seek object in Workspace...
obj.names <- ls(.GlobalEnv)
if (length(obj.names)==0) {
    tkmessageBox(title="Object Browser",
                 message = "No objects found in the current Workspace!",
                 icon = "warning")
    return(invisible(NULL))
    }
obj.mainclass <- sapply(obj.names, function(x) data.class(eval(parse(text=x), envir = .GlobalEnv)))
obj.size <- sapply(obj.names, function(x) as.integer(object.size(eval(parse(text=x), envir = .GlobalEnv))))
obj.size <- prettyNum(obj.size, big.mark = ",")
Workspace <- data.frame(Object=obj.names, Class=obj.mainclass, Size=obj.size)
Size.tag <- "Size(bytes)"
names(Workspace)[which(names(Workspace)=="Size")] <- Size.tag
Workspace <- Workspace[order(Workspace$Class, Workspace$Object), ]
rownames(Workspace) <- NULL
showDataRG(Workspace, title="Current Workspace content")
}


fshowData <- function(){
###########################################
# Actions performed when asking to show   #
# some Datasets in the current Workspace. #
###########################################
showit <- function(obj){
 Dataset <- get(obj, envir=.GlobalEnv)
 showDataRG(Dataset, title=obj)
 }
selectDataSet(action = showit)
}


selectDataSet <- function(action=print, WarnWindow = NULL, ...){ #MODIFIED
##################################################
# Build a List Box (with scrollbar) storing the  #
# names of the objects inheriting from classes   #
# data.frame or analytic which are found inside  #
# the current Workspace.                         #
# One Dataset is eventually selected and a given #
# action is performed on it.                     #
##################################################

# Seek object in Workspace...
obj.names <- ls(.GlobalEnv)
if (length(obj.names)==0) {
    tkmessageBox(title="Available Datasets",
                 message = "No objects found in the current Workspace!",
                 icon = "warning")
    return(invisible(NULL))
    }
# Check for objects inheriting from data.frame or analytic...
df.obj <- obj.names[inherits.obj("data.frame")]
an.obj <- obj.names[inherits.obj("analytic")]
if ( (length(df.obj)==0)  &&  (length(an.obj)==0) ) {
    tkmessageBox(title="Available Datasets",
                 message = "No Datasets found in the current Workspace!",
                 icon = "warning")
    return(invisible(NULL))
    }
datasets <- sort(c(df.obj,an.obj))
# Build the List Box
listBox(datasets, title = "Available Datasets",
        label = " Please select a Dataset (only one)", action=action,
        selectmode="single", WarnWindow = WarnWindow, ...)
}


listBox <- function(list, title="",
                    label=" Please select an item from the list (only one)",
                    action=print, selectmode="single", WarnWindow = NULL, ...){
#################################################
# List Box (with scrollbar): select one element #
# and perform an action on it.                  #
# Note: ... are further parameters to 'action'  #
#################################################
# Find path to the images folder...
img.path <- system.file("images", package = "ReGenesees.GUI")
# ... and get the images needed by the GUI
  # 1) ok flag
  image_ok <- tclVar()
  tkimage.create("photo", image_ok, file=paste(img.path, "//ok.gif", sep=""))
  # 2) cancel
  image_cancel <- tclVar()
  tkimage.create("photo", image_cancel, file=paste(img.path, "//cancel.gif", sep=""))
# end images

tt <- tktoplevel()
tkwm.title(tt, title)
box <- tkframe(tt)
butts <- tkframe(tt)
scr <- tkscrollbar(box, repeatinterval=5,
                   command=function(...)tkyview(tl,...))
tl <- tklistbox(box, height=10, selectmode=selectmode,
                yscrollcommand=function(...)tkset(scr,...), background="white")

for (el in list){
    tkinsert(tl,"end", el)
    }
# Default Dataset is the first of the list
tkselection.set(tl, 0)

tkgrid(tklabel(tt, text = " "))
tkgrid(tklabel(tt, text = label))
tkgrid(tklabel(tt, text = " "))
tkgrid(box)
tkgrid(tl, scr)
tkgrid.configure(tl, sticky="e", padx=c("1c",0))
tkgrid.configure(scr, sticky="nsw", padx=c(0,"1c"))

## Sometimes it happened that the list did not appear into the widget:
## it seems that placing the instructions upper fixes this strange behaviour
#for (el in list){
#    tkinsert(tl,"end", el)
#    }
## Default Dataset is the first of the list
#tkselection.set(tl, 0)

OnOK <- function(action, WarnWindow, ...) {
    Choice <- list[as.numeric(tkcurselection(tl))+1]
    tkdestroy(tt)
    if ( any(names(formals(action))=="WarnWindow") ) {
        action(Choice, WarnWindow, ...)
        }
    else action(Choice, ...)
    }
OK.but <- tk2button(butts,text="   OK   ", image=image_ok,
                    compound="left", command = function() OnOK(action = action,
                                                               WarnWindow = WarnWindow,
                                                               ...) )
Canc.but <- tk2button(butts,text="Cancel", image=image_cancel,
                      compound="left", command = function() fOnCancel(tt))
tkgrid(butts)
tkgrid.configure(butts, pady=c("0.4c", "0.2c"))
tkgrid(OK.but, Canc.but)
tkfocus(tt)
tkwm.resizable(tt, 0, 0)
}


is.big <- function(obj, ncol.lim = 10, dim.lim = 300){
###########################################################
# Flags BIG objects, which are better shown by showDataRG #
# and should not be printed 'as is' on screen.            #
###########################################################
( ncol(obj) > ncol.lim ) || ( prod(dim(obj)) > dim.lim )
}


is.large <- function(obj, ncol.lim = 10){
#######################################
# Asses if a BIG object is LARGE too. #
#######################################
is.big(obj, ncol.lim = ncol.lim) && ( ncol(obj) > ncol.lim )
}


printonscreen <- function(obj, obj.name){
###################################################
# BIG objects are shown by showDataRG and only    #
# their "head" is printed on screen.              #
# SMALL objects are printed on screen.            #
###################################################
if (is.big(obj)) {
    showDataRG(obj, title=obj.name)
    print(head(obj))
    }
else print(obj)
}


PopTemp.onscreen <- function(obj, obj.name, show = FALSE){
###############################################
# BIG "pop.totals"-like objects are shown by  #
# showDataRG and only the "head" of their     #
# first 10 columns is printed on screen.      #
# SMALL objects are printed on screen.        #
###############################################
if (is.big(obj)) {
     if (show) showDataRG(obj, title=obj.name)
     if (is.large(obj)) {
         print(head(obj[, 1:10]))
        }
     else {
         print(head(obj))
        }
    }
else print(obj)
}


formatW <- function(Wstring){
############################################
# Formats a warning string (handling short #
# arglists).                               #
############################################
# 1) cut the piece involving the invoked Fun...
msgWstring <- unlist(strsplit(as.character(Wstring),":"))
msgFun <- msgWstring[1]
# 2) remove actual arguments (which are not meaningful for the user)...
msgFun <- unlist(strsplit(as.character(msgFun),"\\("))[1]
# 3) the rest is the recomposed Warning description
msgWar <- msgWstring[-1]
msgWar <- paste(msgWar, collapse=":")
if (msgWar != "") c(msgFun, msgWar) else msgFun
}


display.warn <- function(msg.conn, WarnWindow){
##############################################
# Facility for displaying warnings generated #
# when commands are executed trough Lancia.  #
##############################################

# First: enable the Warnings Window and clean it...
tkconfigure(WarnWindow, state="normal")
tkdelete(WarnWindow, "1.0", "end")

# Then process the messages stream
msg.lines <- readLines(msg.conn)
warn.lines <- grep("Warning", msg.lines)
nwarn <- nwarn1 <- length(warn.lines)
nml <- nml1 <- length(msg.lines)

# if no warnings have been generated,
# disable the WarnWindow and exit
if (nwarn == 0) {
    tkconfigure(WarnWindow, state="disabled")
    return(invisible(NULL))
    }

# Warning have been generated: update flag...
assignTemp("there.are.warnings", TRUE, replace.existing = TRUE)
# ...then write warnings inside the WarnWindow:
# first remove actual arguments (which are not meaningful for the user)
msg.lines2 <- NULL
j.warn <- 1
for (i in 1:nml) {
    if  (i == warn.lines[j.warn]) {
         msg.lines2 <- c(msg.lines2, formatW(msg.lines[i]))
         if (j.warn < nwarn) j.warn <- j.warn + 1
        }
    else{
         msg.lines2 <- c(msg.lines2, msg.lines[i])
        }
    }

# Recompute "Warning" positions, number and messages lines
warn.lines <- grep("Warning", msg.lines2)
nwarn <- length(warn.lines)
nml <- length(msg.lines2)
# Some checks
if (nwarn > nwarn1) tkmessageBox(title="An error has occurred!",
                                 message="Warning stream reading failed [1]",
                                 icon="error", type="ok")
if (nml < nml1) tkmessageBox(title="An error has occurred!",
                             message="Warning stream reading failed [2]",
                             icon="error", type="ok")

if (nwarn > 10){
    msg.lines2 <- c(paste(nwarn, "warnings have been generated."),
                          "First and last 5 warnings:",
                          msg.lines2[1:(warn.lines[6]-1)], ". . .", msg.lines2[(warn.lines[nwarn-4]):nml])
    }
message <- paste(msg.lines2, collapse="\n")
tkinsert(WarnWindow, "end", message)
# Now disable the WarnWindow and exit
tkconfigure(WarnWindow, state="disabled")
}


is.ok.name <- function(name, go.on = TRUE, parent = "."){
#################################################################
# Checks if an identifier is good (i.e. syntactically valid and #
# unique) as name of an output object.                          #
# NOTE: Argument parent can be used to identify the window that #
#       triggered the check, so to prevent te focus to go back  #
#       before destroying the message box.                      #
#################################################################

# test if validity checks are not
# needed, due to previous errors
if (!go.on) return(FALSE)

ok <- ( length(name) == 1  &&
        is.character(name) &&
        name == make.names(name)
      )
if (!ok) {
          tkmessageBox(title="An error has occurred!",
                       message="Invalid output object name",
                       icon="error", type="ok", parent=parent)
         }
else {
      if ( name %in% ls(.GlobalEnv) ) {
          ans <- tclvalue(tkmessageBox(title="Output object name",
                                       message=paste("An object named '",
                                                     name,
                                                     "' already exists!\nDo you want to overwrite it?", sep=""),
                                       icon="question", type="yesno", default="no", parent=parent))
          if (ans == "no") ok <- FALSE
        }
    }
ok
}


is.ok.varname <- function(name, parent = "."){
#################################################################
# Checks if an identifier is good (i.e. syntactically valid and #
# unique) as name of a variable.                                #
# NOTE: Argument parent can be used to identify the window that #
#       triggered the check, so to prevent te focus to go back  #
#       before destroying the message box.                      #
#################################################################
ok <- ( length(name) == 1  &&
        is.character(name) &&
        name == make.names(name)
      )
if (!ok) {
          tkmessageBox(title="An error has occurred!",
                       message="Invalid variable name",
                       icon="error", type="ok", parent=parent)
         }
ok
}


form.to.char <- function(formula){
##################################################
# Turns a formula into a character, avoiding the #
# truncation problem of as.character for very    #
# long formulae.                                 #
##################################################
# remove initial white spaces generated while deparsing
char.form <- gsub("    ", "", paste(deparse(formula), collapse=""))
# put a space after ~ (our standard)
char.form <- gsub("~", "~ ", char.form)
char.form
}


twosideform.to.char <- function(formula){
##################################################
# Turns a formula into a character, avoiding the #
# truncation problem of as.character for very    #
# long formulae.                                 #
##################################################
# remove initial white spaces generated while deparsing
char.form <- gsub("    ", "", paste(deparse(formula), collapse=""))
# put a space after ~ (our standard)
# char.form <- gsub("~", "~ ", char.form)
char.form
}


RG_select.list <- function(list, title="",
                           label=" Please select an item from the list (only one)",
                           selectmode="single"){
#################################################
# List Box (with scrollbar): select one element #
# and returns it.                               #
#################################################
# Find path to the images folder...
img.path <- system.file("images", package = "ReGenesees.GUI")
# ... and get the images needed by the GUI
  # 1) ok flag
  image_ok <- tclVar()
  tkimage.create("photo", image_ok, file=paste(img.path, "//ok.gif", sep=""))
  # 2) cancel
  image_cancel <- tclVar()
  tkimage.create("photo", image_cancel, file=paste(img.path, "//cancel.gif", sep=""))
# end images

tt <- tktoplevel()
tkwm.title(tt, title)
box <- tkframe(tt)
butts <- tkframe(tt)
scr <- tkscrollbar(box, repeatinterval=5,
                   command=function(...)tkyview(tl,...))
tl <- tklistbox(box, height=10, selectmode=selectmode,
                yscrollcommand=function(...)tkset(scr,...), background="white")

for (el in list){
    tkinsert(tl,"end", el)
    }
# Default Dataset is the first of the list
tkselection.set(tl, 0)

tkgrid(tklabel(tt, text = " "))
tkgrid(tklabel(tt, text = label))
tkgrid(tklabel(tt, text = " "))
tkgrid(box)
tkgrid(tl, scr)
tkgrid.configure(tl, sticky="e", padx=c("1c",0))
tkgrid.configure(scr, sticky="nsw", padx=c(0,"1c"))

Choice <- ""
OnOK <- function(tt) {
    Choice <<- list[as.numeric(tkcurselection(tl))+1]
    tkdestroy(tt)
    }

OK.but <- tk2button(butts,text="   OK   ", image=image_ok,
                    compound="left", command =function() OnOK(tt))
Canc.but <- tk2button(butts,text="Cancel", image=image_cancel,
                      compound="left", command = function() fOnCancel(tt))
tkgrid(butts)
tkgrid.configure(butts, pady=c("0.4c", "0.2c"))
tkgrid(OK.but, Canc.but)
tkfocus(tt)
tkwm.resizable(tt, 0, 0)
tkwait.window(tt)
return(Choice)
}


ctrl.c <- function(textBox){
##########################################################################
# Bind to right click on textBox upon selection the COPY event (code     #
# from http://bioinf.wehi.edu.au/~wettenhall/RTclTkExamples/menus.html   #
##########################################################################
  copyText <- function() tcl("event","generate",.Tk.ID(textBox),"<<Copy>>")

  editPopupMenu <- tkmenu(textBox, tearoff=FALSE)
  tkadd(editPopupMenu, "command", label="Copy <Ctrl-C>", command=copyText)

  RightClick <- function(x,y){
    rootx <- as.integer(tkwinfo("rootx",textBox))
    rooty <- as.integer(tkwinfo("rooty",textBox))
    xTxt <- as.integer(x)+rootx
    yTxt <- as.integer(y)+rooty
    tcl("tk_popup",editPopupMenu,xTxt,yTxt)
    }

  tkbind(textBox, "<Button-3>",RightClick)
  }


ctrl.cv <- function(textBox){
#################################################
# Bind to right click on textBox upon selection #
# BOTH the COPY and PASTE events.               #
#################################################
  copyText  <- function() tcl("event","generate",.Tk.ID(textBox),"<<Copy>>")
  pasteText <- function() tcl("event","generate",.Tk.ID(textBox),"<<Paste>>")

  editPopupMenu <- tkmenu(textBox, tearoff=FALSE)
  tkadd(editPopupMenu, "command", label="Copy  <Ctrl-C>", command=copyText)
#  tkadd(editPopupMenu,"separator")
  tkadd(editPopupMenu, "command", label="Paste <Ctrl-V>", command=pasteText)

  RightClick <- function(x,y){
    rootx <- as.integer(tkwinfo("rootx",textBox))
    rooty <- as.integer(tkwinfo("rooty",textBox))
    xTxt <- as.integer(x)+rootx
    yTxt <- as.integer(y)+rooty
    tcl("tk_popup",editPopupMenu,xTxt,yTxt)
    }

  tkbind(textBox, "<Button-3>",RightClick)
  }


ScreenResolution <- function() {
####################################
# Return the screen resolution as  #
# a length 2 integer vector with   #
# components 'width' and 'height'. #
####################################
  sr <- as.integer(strsplit(tclvalue(tkwm.maxsize('.'))," ")[[1]])
  names(sr) <- c("width", "height")
  sr
}


get.fontsize <- function(font){
###########################################
# Return the current size of an available #
# font (e.g. TkDefaultFont).              #
###########################################
  font.charvect <- as.character(tkfont.configure(font))
  size <- as.integer(font.charvect[1 + match("-size", font.charvect)])
  size
}


get.fontfamily <- function(font){
##########################################
# Return the family of an available font #
# (e.g. TkDefaultFont).                  #
##########################################
  font.charvect <- as.character(tkfont.configure(font))
  family <- font.charvect[1 + match("-family", font.charvect)]
  family
}


##################################################
# Assign into the .GlobalEnv (in such a way that #
# R CMD check --as-cran is happy)                #
#                                                #
# NOTE: I don't like this, it makes me feel like #
#       I'm not actually programming in R but    #
#       rather in some CRAN dialect of the R     #
#       language...                              #
##################################################
`assign2GE` <- function(x, value){
     GE <- .GlobalEnv
     assign(x, value, envir = GE, inherits = FALSE, immediate = TRUE)
}


upd.Rhistory <- function(commands, RG.stamp = FALSE){
####################################################
# Flushes a R commands triggered by ReGenesees GUI #
# (as represented by the string vector argument    #
# 'commands') to the Rhistory file, possibly after #
# flushing the ReGeneeses stamp string "## RG".    #
#                                                  #
# NOTE: Currently works only for Windows, as OSs   #
#       of the Linux/Unix family and Mac have      #
#       different mechanisms to handle R history,  #
#       which I'm not willing to dig into.         #
####################################################
# Os check
  if (.Platform$OS.type != "windows") return(invisible(NULL))
# Open a temporary file
  tmphistory <- tempfile()
  hist.comm <- file(tmphistory, open = "a")
# Write the current commands in the temporary file...
  # ...if needed, adding the ReGenesees stamp in advance
  txt <- if (!RG.stamp) commands else c("## RG", commands)
  writeLines(text = txt, con = hist.comm)
  close(hist.comm)
# Append the temporary file to the R hystory file
  loadhistory(tmphistory)
# Destroy the the temporary file
  unlink(tmphistory)
}

