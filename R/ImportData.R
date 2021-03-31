#########################################################################
# Facilities for:                                                       #
# - importing datasets from Excel and/or Access                         #
# - importing datasets from Text files (.txt, .csv, .dat)               #
# - exporting dataframes to Text files (.txt, .csv, .dat)               #
#########################################################################

fImportData <- function(functionsMenu, surveydesignMenu, calibrationMenu, textHistory, WarnWindow){
########################################################################################
# Function fImportData retrieved from Rcmdr, with some modifications.                  #
# NOTE: images needed for widget's elements are referenced as global                   #
#       variables (they have been stored into the NameSpace).                          #
########################################################################################

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

    #load the RODBC package and stops the program if not available
#    if(!require(RODBC)) # NOT NECESSARY due to 'Depends' in Description file
#    tkmessageBox(title="Data import", message = "This function requires the RODBC package.\n", icon = "warning")
    on.exit(odbcCloseAll())

    # Enter the name of data set, by default : Dataset
    ttImportdata <- tktoplevel()
    fDesignDialog(ttImportdata)
    tkwm.title(ttImportdata,"Import data from Excel or Access file")
    frameEnter <- tkframe(ttImportdata, borderwidth=0)
    frameButtons <- tkframe(ttImportdata, borderwidth=0)

    dsname <- tclVar("Dataset")

    entry.Dsname <- ttkentry(frameEnter, width = "25")
    tkgrid.configure(frameEnter, padx="0.5c", pady=c("0.5c",0))
    tkgrid(tklabel(frameEnter, text="Enter name of data set:  "), entry.Dsname)

    tkconfigure(entry.Dsname, textvariable=dsname)
    Ok.but <- tk2button(frameButtons, text="OK",
                        image=image_ok, compound="left",
                        command=function() Lancia(fOnOK(ttImportdata,entry.Dsname,functionsMenu, surveydesignMenu,
                                                        calibrationMenu, textHistory),
                                                  WarnWindow))
    Cancel.but <- tk2button(frameButtons,text="Cancel",
                            image=image_cancel, compound="left",
                            command=function() fOnCancel(ttImportdata))
    FunctionHelp.but <- tk2button(frameButtons,text="Function Help",
                                image=image_qm, compound="left", tip=descFun("odbcConnect"),
                                command=function() fOnFunctionHelp("odbcConnect"))
    tkgrid.configure(frameButtons, pady=c("0.5c","0.2c"))

    tkgrid(Ok.but, Cancel.but, FunctionHelp.but)
}

fReadDataSet <- function(WarnWindow, functionsMenu, surveydesignMenu, calibrationMenu, textHistory){
########################################################################################
# Function fReadDataSet retrieved from Rcmdr, with some modifications.                 #
# NOTE: images needed for widget's elements are referenced as global                   #
#       variables (they have been stored into the NameSpace).                          #
########################################################################################
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

    ttReadDataSet <- tktoplevel()
    fDesignDialog(ttReadDataSet)
    tkwm.title(ttReadDataSet,"Import data from Text file")
    optionsFrame <- tkframe(ttReadDataSet)
    optionsLocations <- tkframe(ttReadDataSet)
    buttonsFrame <- tkframe(ttReadDataSet)

    dsname <- tclVar("Dataset")
    entryDsname <- ttkentry(optionsFrame, width = "20", textvariable=dsname)

    headerVariable <- tclVar("1")
    headerCheckBox <- ttkcheckbutton(optionsFrame, variable=headerVariable)

    missingVariable <- tclVar("NA")
    missingEntry <- ttkentry(optionsFrame, width="8", textvariable=missingVariable)

    ###################
    # Field Separator #
    ###################
    rbDelimiterW <- ttkradiobutton(optionsLocations)
    rbDelimiterC <- ttkradiobutton(optionsLocations)
    rbDelimiterT <- ttkradiobutton(optionsLocations)
    rbDelimiterO <- ttkradiobutton(optionsLocations)

    rbValueDelimiter <- tclVar("whitespace")

    labelDelimiterW <- ttklabel(optionsLocations,text="  White space", justify="left")
    labelDelimiterC <- ttklabel(optionsLocations,text="  Commas", justify="left")
    labelDelimiterT <- ttklabel(optionsLocations,text="  Tabs", justify="left")
    labelDelimiterO <- ttklabel(optionsLocations,text="  Other", justify="left")

    labelOtherEntry <- ttklabel(optionsLocations, text="Specify:")
    otherVariable <- tclVar("")
    OtherEntry <- ttkentry(optionsLocations, width="4", textvariable=otherVariable)

    # When default delimiter applies, no other can be specified
    tkconfigure(OtherEntry, state = "disabled")

    ###########################
    # Decimal-Point Character #
    ###########################
    rbDecimalP <- ttkradiobutton(optionsLocations)
    rbDecimalC <- ttkradiobutton(optionsLocations)

    rbValueDecimal <- tclVar("period")
    labelDecimalP <- ttklabel(optionsLocations,text="  Period [.]")
    labelDecimalC <- ttklabel(optionsLocations,text="  Comma [,]")


    ok.but <- tk2button(buttonsFrame, text="OK",
                        image=image_ok, compound="left",
                        command=function() fOnRun())
    Cancel.but <- tk2button(buttonsFrame, text="Cancel",
                            image=image_cancel, compound="left",
                            command=function() fOnCancel(ttReadDataSet))
    FunctionHelp.but <- tk2button(buttonsFrame, text="Function Help",
                                  image=image_qm, compound="left", tip=descFun("read.table"),
                                  command=function() fOnFunctionHelp("read.table"))

    tkgrid(ttklabel(optionsFrame, text="\n"))
    tkgrid(ttklabel(optionsFrame, text="  Enter name for data set:  "), entryDsname, sticky="w")
    tkgrid(ttklabel(optionsFrame, text="  Variable names in file:   "), headerCheckBox, sticky="w")
    tkgrid(ttklabel(optionsFrame, text="  Missing data indicator:   "), missingEntry, sticky="w")
    tkgrid(ttklabel(optionsFrame, text="\n"))

    tkgrid(ttklabel(optionsLocations, text="  Field Separator  ", foreground= "blue"), sticky="w")

    tkconfigure(rbDelimiterW, variable=rbValueDelimiter, value="whitespace")
    tkconfigure(rbDelimiterC, variable=rbValueDelimiter, value="commas")
    tkconfigure(rbDelimiterT, variable=rbValueDelimiter, value="tabs")
    tkconfigure(rbDelimiterO, variable=rbValueDelimiter, value="other")

    tkbind(rbDelimiterW, "<ButtonPress>", function() {
                                          tkdelete(OtherEntry, 0, "end")
                                          tkconfigure(OtherEntry, state = "disabled")
                                          })
    tkbind(rbDelimiterC, "<ButtonPress>", function() {
                                          tkdelete(OtherEntry, 0, "end")
                                          tkconfigure(OtherEntry, state = "disabled")
                                          })
    tkbind(rbDelimiterT, "<ButtonPress>", function() {
                                          tkdelete(OtherEntry, 0, "end")
                                          tkconfigure(OtherEntry, state = "disabled")
                                          })
    tkbind(rbDelimiterO, "<ButtonPress>", function() tkconfigure(OtherEntry, state = "normal"))

    tkgrid(labelDelimiterW, rbDelimiterW, sticky="w")
    tkgrid(labelDelimiterC, rbDelimiterC, sticky="w")
    tkgrid(labelDelimiterT, rbDelimiterT, sticky="w")
    tkgrid(labelDelimiterO, rbDelimiterO, labelOtherEntry, OtherEntry, sticky="w")
    tkgrid.configure(labelOtherEntry, column=3)
    tkgrid.configure(OtherEntry, column=4, sticky="w")

    tkgrid(ttklabel(optionsLocations, text="\n"))
    tkgrid(ttklabel(optionsLocations, text="  Decimal-Point Character  ", foreground= "blue"), sticky="w")

    tkconfigure(rbDecimalP, variable=rbValueDecimal, value="period")
    tkconfigure(rbDecimalC, variable=rbValueDecimal, value="comma")

    tkgrid(labelDecimalP, rbDecimalP, sticky="w")
    tkgrid(labelDecimalC, rbDecimalC, sticky="w")
    tkgrid(ttklabel(optionsLocations, text="\n"))

    tkgrid(ok.but, Cancel.but, FunctionHelp.but)

    tkgrid(optionsFrame)
    tkgrid(optionsLocations)
    tkgrid(buttonsFrame)
    tkgrid.configure(buttonsFrame, padx="0.2c", pady=c("0.0c", "0.3c"))
    tkgrid.configure(optionsFrame, sticky="w")
    tkgrid.configure(optionsLocations, sticky="w")

    fOnRun <- function(){
            dsnameValue <- trim.blanks(tclvalue(tkget(entryDsname)))
            if(dsnameValue == ""){
                MsgR("The dataset name is empty", "error", "ok", "read.table", parent=ttReadDataSet) #PROVA!
                return()
            }
            if (!is.valid.name(dsnameValue)) {
                tkmessageBox(title="Data import",
                             message= paste('"', dsnameValue, '" ',"is not a valid name", sep=""),
                             icon="error", parent=ttReadDataSet)
                return()
            }

            if (is.element(dsnameValue, listDataSets())){
                ReturnExit <- tclvalue(tkmessageBox(title="Data import",
                                       message=paste("A Dataset named ", dsnameValue,
                                                     " already exists. Do you want to overwrite it?", sep = ""),
                                       icon="question", type="yesno",default="no", parent=ttReadDataSet))
                if (ReturnExit=="no"){
                    return()
                }
            }

            tkgrab.release(ttReadDataSet)
            tkdestroy(ttReadDataSet)

#           Old Code: in Win 7, before R 3.1, this happened to invert the display order. (order ok in 64bit, ko in 32bit)
            File <- tclvalue(tkgetOpenFile(filetypes = "{{Text Files}
                                                        {.txt .TXT .csv .CSV .dat .DAT}} {{All files} *}"))
#           Temporary fix (order ok in 32bit, ko in 64bit):
#           File <- tclvalue(tkgetOpenFile(filetypes = '{"All Files" {"*"}} {"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}}'))


            if(File == ""){
                return()
            }
            head <- tclvalue(headerVariable) == "1"
            delimiter <- tclvalue(rbValueDelimiter)
            del <- if (delimiter == "whitespace") ""
                else if (delimiter == "commas") ","
                else if (delimiter == "tabs") "\t"
                else tclvalue(otherVariable)
            miss <- tclvalue(missingVariable)
            dec <- if (tclvalue(rbValueDecimal) == "period") "." else ","

            # DEBUG 08/05/2020: due to R 4.0.0 had to explicitly use stringsAsFactors = TRUE
            valReadDataSet <- Lancia(read.table(File, header = head, sep = del, na.strings = miss,
                                                quote="\"", dec = dec, strip.white = TRUE, comment.char = "", stringsAsFactors = TRUE),
                                                WarnWindow)

            if (!inherits(valReadDataSet,"try-error")){
                # assign(dsnameValue, valReadDataSet, envir=.GlobalEnv)
                assign2GE(dsnameValue, valReadDataSet)
                Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)
                # Print on the Commands Window
                del.print <- ifelse(del=="\t", "\\t", del)
                # DEBUG 08/05/2020: due to R 4.0.0 had to explicitly use stringsAsFactors = TRUE
                statement <- paste('read.table(file= "', File,'", header= ', head,
                ', sep= "', del.print, '", na.strings= ', miss, ', quote="\\\"", dec= "', dec,
                '", strip.white= TRUE, comment.char= "", stringsAsFactors= TRUE)', sep="")
                commands <- paste(dsnameValue, " <- ", statement,"\n", sep="")
                tkconfigure(textHistory, state="normal")
                tkinsert(textHistory, "end", commands)
                tkinsert(textHistory, "end", "\n")
                tkyview.moveto(textHistory, 1.0)
                tkconfigure(textHistory, state="disabled")
                # End

                # Flush commands to Rhistory file
                upd.Rhistory(commands, RG.stamp = TRUE)
                # End

                tkmessageBox(title ="Import data",message = "Operation executed", icon = "info")
            }

    }
}


fOnOK <- function(OR_ttImportdata, OR_entry.Dsname, functionsMenu,
                  surveydesignMenu, calibrationMenu, textHistory){
#############################################
# Actions performed when pressing OK in the #
# widget created by fImportData.            #
#############################################

dsnameValue <- trim.blanks(tclvalue(tkget(OR_entry.Dsname)))
if (dsnameValue == ""){
    MsgR("The dataset name is empty", "error", "ok", "Data import", parent=OR_ttImportdata) #PROVA!
    return()
    }
if (!is.valid.name(dsnameValue)) {
    tkmessageBox(title="Data import",
                 message= paste('"', dsnameValue, '" ',"is not a valid name", sep=""), icon="error",
                 parent=OR_ttImportdata)
    return()
    }
if (is.element(dsnameValue, listDataSets())){
    ReturnExit<- tclvalue(tkmessageBox(title="Data import",
                          message=paste("A Dataset named ", dsnameValue,
                                        " already exists. Do you want to overwrite it?", sep = ""),
                          icon="question", type="yesno",default="no", parent=OR_ttImportdata)) #PROVA!
    if (ReturnExit=="no"){
        return()
        }
    }

tkgrab.release(OR_ttImportdata)
tkdestroy(OR_ttImportdata)

# Old Code: in Win 7, before R 3.1, this happened to invert the display order. (order ok in 64bit, ko in 32bit)
File <- tclvalue(tkgetOpenFile(filetypes = "{{MS Excel file} {*.xls .XLS}} {{MS Excel 2007 file} {*.xlsx .XLSX}}
                                            {{MS Access database} {*.mdb .MDB}}
                                            {{MS Access 2007 database} {*.accdb .ACCDB}} {{All files} *}"))
# Temporary fix (order ok in 32bit, ko in 64bit):
# File <- tclvalue(tkgetOpenFile(filetypes = '{"All Files" {"*"}} {"MS Access database" {".mdb" ".MDB"}} {"MS Access 2007 database" {".accdb" ".ACCDB"}}
#                                             {"MS Excel file" {".xls" ".XLS"}} {"MS Excel 2007 file" {".xlsx" ".XLSX"}}'))

if (File == ""){
    return()
    }
sop <- match(".", rev(strsplit(File, NULL)[[1]]))[1]
ext <- tolower(substring(File, nchar(File) - sop + 2, nchar(File)))
channel <- switch(EXPR = ext,
                  xls = odbcConnectExcel(File),
                  xlsx = odbcConnectExcel2007(File),
                  mdb = odbcConnectAccess(File),
                  accdb = odbcConnectAccess2007(File))

mk.chan.cmd <- switch(EXPR = ext,
                  xls = paste("channel <- odbcConnectExcel('",File,"')\n",sep=""),
                  xlsx = paste("channel <- odbcConnectExcel2007('",File,"')\n",sep=""),
                  mdb = paste("channel <- odbcConnectAccess('",File,"')\n",sep=""),
                  accdb = paste("channel <- odbcConnectAccess2007('",File,"')\n",sep=""))

# For Excel and Access cases, need to select a particular sheet or table
tabdat <- sqlTables(channel)
names(tabdat) <- tolower(names(tabdat))
if (ext == "mdb" || ext == "accdb")
    tabdat <- tabdat[tabdat$table_type == "TABLE", 3]
if (ext == "xls" || ext == "xlsx"){
    tabname <- tabdat$table_name
    tabdat <- ifelse(tabdat$table_type =="TABLE",
        substring(tabname, 1, nchar(tabname) - 2),
        substring(tabname, 1, nchar(tabname) - 1))
    }
# if there are several tables
tabdat <- unique(tabdat)
if (length(tabdat)>1)
     fil <- RG_select.list(sort(tabdat), title="Data import",
                           label="Please select one table", selectmode="single")
#   fil <- tk_select.list(sort(tabdat), title = "Select one table")
#   fil <- select.list(sort(unique(tabdat)), title = "Select one table")
else
    fil <- tabdat
if (fil == ""){
     MsgR("No table selected", "error", "ok", "Data import")
     odbcCloseAll()
     return()
    }
if (ext == "xls" || ext == "xlsx")
    fil <- paste("[", fil, "$]", sep = "")

# Retrieve the data
dat <- sqlQuery(channel = channel, query = paste("select * from", fil))
# Check for empty/corrupted datasets
if (!is.data.frame(dat)) {
     MsgR("The dataset is empty or unreadable", "error", "ok", "Data import")
     odbcCloseAll()
     return()
    }

names(dat)<- trim.blanks(names(dat))
dat <- trim.col.na(dat)
odbcCloseAll()
# Check for empty datasets after trimming NAs
if (ncol(dat) < 1) {
     MsgR("The dataset is empty", "error", "ok", "Data import")
     return()
    }
# assign(dsnameValue, as.data.frame(dat), envir = .GlobalEnv)
# DEBUG 08/05/2020: due to R 4.0.0 had to explicitly use stringsAsFactors = TRUE
assign2GE(dsnameValue, as.data.frame(dat, stringsAsFactors = TRUE))
Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

# Print on the Commands Window
sqlcmd <- paste("sqlQuery(channel = channel, query = 'select * from ", fil,"')", sep="")
command <- paste(dsnameValue, " <- ", sqlcmd,"\n", sep="")
tkconfigure(textHistory, state="normal")
tkinsert(textHistory, "end", mk.chan.cmd)
tkinsert(textHistory, "end", command)
tkinsert(textHistory, "end", "rm(channel)\n")
tkinsert(textHistory, "end", "odbcCloseAll()\n")
tkinsert(textHistory, "end", "\n")
tkyview.moveto(textHistory, 1.0)
tkconfigure(textHistory, state="disabled")
# End

# Flush commands to Rhistory file
upd.Rhistory(c(mk.chan.cmd, command, "rm(channel)\n", "odbcCloseAll()\n"), RG.stamp = TRUE)
# End

MsgR("Operation executed", "info", "ok", "Import data")
}

##############################################
# Utility functions used by fImportData, all #
# retrieved from Rcmdr:                      #
# - trim.blanks                              #
# - is.valid.name                            #
# - trim.col.na                              #
##############################################

listDataSets <- function(envir=.GlobalEnv, ...) {
################################################
# List dataframe-like objects that can be used #
# as SAMPLE DATA, thus excluding those from    #
# classes:      - pop.totals    |              #
#               - aux.estimates |              #
#               - svyby                        #
#               - svystatTM                    #
#               - svystatTM.by                 #
#               - svystatR                     #
#               - svystatR.by                  #
#               - svystatS                     #
#               - svystatS.by                  #
#               - svystatSR                    #
#               - svystatSR.by                 #
#               - svystatQ                     #
#               - svystatQ.by                  #
#               - svystatL                     #
#               - svystatL.by                  #
#               - svystatB                     #
#               - svystatB.by                  #
#               - CoV                          #
#               - CoV.by                       #
#               - Corr                         #
#               - Corr.by                      #
################################################
    Objs <- ls(envir = envir, all.names = TRUE)
    if (length(Objs) == 0) return(Objs)
    excluded.cl <- c("pop.totals", "svystatTM", "svystatR", "svystatS", "svystatSR", "svystatQ", "svystatL",
                     "svystatB", "CoV", "Corr", "svyby")
    names(which(sapply(Objs, function(.x) { is.data.frame(get(.x, envir=envir)) &&
                                            !any( class(get(.x, envir=envir)) %in%  excluded.cl )} )))
}

listDfpop <- function(envir=.GlobalEnv, ...) {
################################################
# List dataframe-like objects that can be used #
# as POPULATION TOTALS, thus excluding those   #
# from classes: - svyby                        #
#               - svystatTM                    #
#               - svystatTM.by                 #
#               - svystatR                     #
#               - svystatR.by                  #
#               - svystatQ                     #
#               - svystatQ.by                  #
#               - svystatL                     #
#               - svystatL.by                  #
#               - svystatB                     #
#               - svystatB.by                  #
#               - CoV                          #
#               - CoV.by                       #
#               - Corr                         #
#               - Corr.by                      #
################################################
    Objs <- ls(envir = envir, all.names = TRUE)
    if (length(Objs) == 0) return(Objs)
    excluded.cl <- c("svystatTM", "svystatR", "svystatQ", "svystatS", "svystatSR", "svystatL",
                     "svystatB", "CoV", "Corr", "svyby")
    names(which(sapply(Objs, function(.x) { is.data.frame(get(.x, envir=envir)) &&
                                            !any( class(get(.x, envir=envir)) %in%  excluded.cl )} )))
}

trim.blanks <- function(text){
    gsub("^\ *", "", gsub("\ *$", "", text))
}

is.valid.name <- function(x){
    length(x) == 1 && is.character(x) && x == make.names(x)
}

# The following function was contributed by Matthieu Lesnoff (added 20 July 06)
trim.col.na <- function(dat){
# Remove variables with only missing values (occurs sometimes with modified Excel file)
    colsup <- NULL
    for (i in 1:ncol(dat))
    {
        if (length(dat[is.na(dat[,i])==TRUE,i]) ==length(dat[,i]))
            colsup <- c(colsup,i)
    }
    if (length(colsup) > 0)
        dat <- dat[,-colsup]
    dat
}

fExportData <- function(WarnWindow, textHistory){
#############################################
# Actions performed when asking to export   #
# some Datasets from the current Workspace. #
#############################################
selectDataSet(action = exportDataSet,
              WarnWindow = WarnWindow, textHistory = textHistory) #MODIFIED
}

exportDataSet <- function(dsname, WarnWindow, textHistory) {
#############################################
# Actions performed when asking to export   #
# some Datasets from the current Workspace. #
#############################################
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

    ttExpDataSet <- tktoplevel()
    fDesignDialog(ttExpDataSet)
    tkwm.title(ttExpDataSet,"Export selected Dataset")
    optionsFrame <- tkframe(ttExpDataSet)
    optionsLocations <- tkframe(ttExpDataSet)
    buttonsFrame <- tkframe(ttExpDataSet)

    # col.names
    colnamesVariable <- tclVar("1")
    colnamesCheckBox <- ttkcheckbutton(optionsFrame, variable=colnamesVariable)

    # row.names
    rownamesVariable <- tclVar("0")
    rownamesCheckBox <- ttkcheckbutton(optionsFrame, variable=rownamesVariable)

    # quote
    quotesVariable <- tclVar("0")
    quotesCheckBox <- ttkcheckbutton(optionsFrame, variable=quotesVariable)

    # NA
    missingVariable <- tclVar("NA")
    missingEntry <- ttkentry(optionsFrame, width="8", textvariable=missingVariable)

    # Field Separator
    rbDelimiterW <- ttkradiobutton(optionsLocations)
    rbDelimiterC <- ttkradiobutton(optionsLocations)
    rbDelimiterT <- ttkradiobutton(optionsLocations)
    rbDelimiterO <- ttkradiobutton(optionsLocations)

    rbValueDelimiter <- tclVar("spaces")

    labelDelimiterW <- ttklabel(optionsLocations,text="  Spaces", justify="left")
    labelDelimiterC <- ttklabel(optionsLocations,text="  Commas", justify="left")
    labelDelimiterT <- ttklabel(optionsLocations,text="  Tabs", justify="left")
    labelDelimiterO <- ttklabel(optionsLocations,text="  Other", justify="left")

    labelOtherEntry <- ttklabel(optionsLocations, text="Specify:")
    otherVariable <- tclVar("")
    OtherEntry <- ttkentry(optionsLocations, width="4", textvariable=otherVariable)

    # When default delimiter applies, no other can be specified
    tkconfigure(OtherEntry, state = "disabled")

    # Create Buttons
    ok.but <- tk2button(buttonsFrame, text="OK",
                        image=image_ok, compound="left",
                        command=function(...) fOnRun(...))
    Cancel.but <- tk2button(buttonsFrame, text="Cancel",
                            image=image_cancel, compound="left",
                            command=function() fOnCancel(ttExpDataSet))
    FunctionHelp.but <- tk2button(buttonsFrame, text="Function Help",
                                  image=image_qm, compound="left", tip=descFun("write.table"),
                                  command=function() fOnFunctionHelp("write.table"))

    tkgrid(ttklabel(optionsFrame, text="\n"))
    tkgrid(ttklabel(optionsFrame, text="  Write variable names:   "), colnamesCheckBox, sticky="w")
    tkgrid(ttklabel(optionsFrame, text="  Write row names:   "), rownamesCheckBox, sticky="w")
    tkgrid(ttklabel(optionsFrame, text="  Quotes around character values:   "), quotesCheckBox, sticky="w")
    tkgrid(ttklabel(optionsFrame, text="\n"))

    tkgrid(ttklabel(optionsLocations, text="  Field Separator  ", foreground= "blue"), sticky="w")

    tkconfigure(rbDelimiterW, variable=rbValueDelimiter, value="spaces")
    tkconfigure(rbDelimiterC, variable=rbValueDelimiter, value="commas")
    tkconfigure(rbDelimiterT, variable=rbValueDelimiter, value="tabs")
    tkconfigure(rbDelimiterO, variable=rbValueDelimiter, value="other")

    tkbind(rbDelimiterW, "<ButtonPress>", function() {
                                          tkdelete(OtherEntry, 0, "end")
                                          tkconfigure(OtherEntry, state = "disabled")
                                          })
    tkbind(rbDelimiterC, "<ButtonPress>", function() {
                                          tkdelete(OtherEntry, 0, "end")
                                          tkconfigure(OtherEntry, state = "disabled")
                                          })
    tkbind(rbDelimiterT, "<ButtonPress>", function() {
                                          tkdelete(OtherEntry, 0, "end")
                                          tkconfigure(OtherEntry, state = "disabled")
                                          })
    tkbind(rbDelimiterO, "<ButtonPress>", function() tkconfigure(OtherEntry, state = "normal"))

    tkgrid(labelDelimiterW, rbDelimiterW, sticky="w")
    tkgrid(labelDelimiterC, rbDelimiterC, sticky="w")
    tkgrid(labelDelimiterT, rbDelimiterT, sticky="w")
    tkgrid(labelDelimiterO, rbDelimiterO, labelOtherEntry, OtherEntry, sticky="w")
    tkgrid.configure(labelOtherEntry, column=3)
    tkgrid.configure(OtherEntry, column=4, sticky="w")

    tkgrid(tklabel(buttonsFrame, text= "\n"))
    tkgrid(ok.but, Cancel.but, FunctionHelp.but)

    tkgrid(optionsFrame)
    tkgrid(optionsLocations)

    tkgrid.configure(buttonsFrame, padx="0.2c", pady=c("0.0c", "0.3c"))
    tkgrid.configure(optionsFrame, sticky="w")
    tkgrid.configure(optionsLocations, sticky="w")

    fOnRun <- function(...){
        tkgrab.release(ttExpDataSet)
        tkdestroy(ttExpDataSet)
        col <- tclvalue(colnamesVariable) == 1
        row <- tclvalue(rownamesVariable) == 1
        #NEW: Handle correctly the rownames, as write.csv does (see ?write.table)
        if (row && col) col <- NA
        #END NEW
        quote <- tclvalue(quotesVariable) == 1
        missing <- tclvalue(missingVariable)
        delim <- tclvalue(rbValueDelimiter)
        sep <- if (delim == "tabs") "\\t"
            else if (delim == "spaces") " "
            else if (delim == "commas") ","
            else trim.blanks(tclvalue(otherVariable))

#       Old Code: in Win 7, before R 3.1, this happened to invert the display order. (order ok in 64bit, ko in 32bit)
        saveFile <- tclvalue(tkgetSaveFile(filetypes='{"Text Files" {".txt" ".TXT" ".csv" ".CSV" ".dat" ".DAT"}}
                                                      {"All Files" {"*"}}',
                                           defaultextension="txt", initialfile=paste(dsname, ".txt", sep="")))
#       Temporary fix (order ok in 32bit, ko in 64bit):
#       saveFile <- tclvalue(tkgetSaveFile(filetypes='{"All Files" {"*"}} {"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}}',
#                                          defaultextension="txt", initialfile=paste(dsname, ".txt", sep="")))


        if (saveFile == "") {
            return()
        }
        # Check the class of object named dsname
        dsobj <- eval(parse(text=dsname), envir = .GlobalEnv)
        svystat.classes <- c("svystatTM", "svystatR", "svystatL", "svystatQ", "svystatB", "svystatS", "svystatSR", "CoV", "Corr", "svyby")
        check.classes <- sapply(svystat.classes, function(class) inherits(dsobj, class))
        if (any(check.classes)) {
              command <- paste("write.svystat(", dsname, ', file= "', saveFile, '", sep= "', sep,
                               '", na= "', missing, '")', sep="")
            }
        else {
              if (inherits(dsobj, "analytic")){
                  data <- paste(dsname,"$variables",sep="")
                  command <- paste("write.table(", data, ', file= "', saveFile, '", sep= "', sep,
                                   '", col.names= ', col, ", row.names= ", row, ", quote= ", quote,
                                   ', na= "', missing, '")', sep="")
                }
              else{
                  command <- paste("write.table(", dsname, ', file= "', saveFile, '", sep= "', sep,
                                   '", col.names= ', col, ", row.names= ", row, ", quote= ", quote,
                                   ', na= "', missing, '")', sep="")
                }
            }
        exported <- Lancia(eval(parse(text = command)), WarnWindow)
        if (!inherits(exported,"try-error")){
            # Print on the Commands Window
            tkconfigure(textHistory, state="normal")
            tkinsert(textHistory, "end", paste(command, "\n", sep=""))
            tkinsert(textHistory, "end", "\n")
            tkyview.moveto(textHistory, 1.0)
            tkconfigure(textHistory, state="disabled")
            # End

            # Flush commands to Rhistory file
            upd.Rhistory(command, RG.stamp = TRUE)
            # End

            tkmessageBox(title ="Export data",message = "Operation executed", icon = "info")
        }
    }
}
