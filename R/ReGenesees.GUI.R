ReGenesees.GUI <-
function(){
########################################################
# Driver of the graphical interface for the ReGenesees #
# package.                                             #
########################################################

# Check/Load required packages (NOT NECESSARY)
# require(tcltk)
# require(tcltk2)
# require(svMisc)
# require(RODBC)
# require(ReGenesees)

# Launch intro window
Intro.RG()

# Get versions of packages ReGenesees and ReGenesees.GUI (for title)
PKG.version <- packageDescription("ReGenesees")$Version
GUI.version <- packageDescription("ReGenesees.GUI")$Version
# Compose GUI title string
titleRG <- paste("ReGenesees ",PKG.version, " [pkg] - ", GUI.version, " [gui]", sep="")

# Find path to the images folder...
img.path <- system.file("images", package = "ReGenesees.GUI")
# ... and get the images needed by the GUI
  # 1) green sx arrow
  image_sx <- tclVar()
  # tkimage.create("photo", image_sx, file=paste(img.path, "//green.gif", sep="")) fix for OS independence
  tkimage.create("photo", image_sx, file=file.path(img.path, "green.gif"))
  # 2) red dx arrow
  image_dx <- tclVar()
  # tkimage.create("photo", image_dx, file=paste(img.path, "//red.gif", sep="")) fix for OS independence
  tkimage.create("photo", image_dx, file=file.path(img.path, "red.gif"))
  # 3) help question mark
  image_qm <- tclVar()
  # tkimage.create("photo", image_qm, file=paste(img.path, "//help.gif", sep="")) fix for OS independence
  tkimage.create("photo", image_qm, file=file.path(img.path, "help.gif"))
  # 4) ok flag
  image_ok <- tclVar()
  # tkimage.create("photo", image_ok, file=paste(img.path, "//ok.gif", sep="")) fix for OS independence
  tkimage.create("photo", image_ok, file=file.path(img.path, "ok.gif"))
  # 5) cancel
  image_cancel <- tclVar()
  # tkimage.create("photo", image_cancel, file=paste(img.path, "//cancel.gif", sep="")) fix for OS independence
  tkimage.create("photo", image_cancel, file=file.path(img.path, "cancel.gif"))
  # 6) ReGenesees icon
  image_RG.ico <- tclVar()
  # tkimage.create("photo", image_RG.ico, file=paste(img.path, "//RG.20.gif", sep="")) fix for OS independence
  tkimage.create("photo", image_RG.ico, file=file.path(img.path, "RG.20.gif"))
# end images

  # Set default options for ReGenesees.GUI session
  old.opt <- change.options()

  # Did last call generate warnings?
  assignTemp("there.are.warnings", FALSE, replace.existing = TRUE)

  count <- FALSE
  count_DesAddvars <- FALSE
  count_mergeDes <- FALSE #NEW
  count_mergeData <- FALSE #NEW
  count_trimDes <- FALSE #NEW
  count_SvystatTM <- FALSE
  count_Ecalibrate <- FALSE
  count_Dfpopulation <- FALSE
  count_poptotals <- FALSE
  count_scelta <- FALSE
  count_SvystatR <- FALSE
  count_SvystatS <- FALSE
  count_SvystatSR <- FALSE
  count_SvystatQ <- FALSE
  count_SvystatL <- FALSE
  count_SvystatB <- FALSE
  count_FillTemplate <- FALSE

  AddVarsObjectName <- tclVar("")
  DesMergeObjectName <- tclVar("") #NEW
  TrimcalObjectName <- tclVar("") #NEW
  rbValueCalPart <- tclVar("")
  VarESvyDataframe <- tclVar("")
  VarESvyDesign <- tclVar("")
  VarECalDfpopulation <- tclVar("")
  VarECalDesignObj <- tclVar("")
  VarPopSurveyDataObj <- tclVar("")
  VarESvystatDesignObj <- tclVar("")
  VarCalmodel <- tclVar("")
  VarPartition <- tclVar("")
  ObjectCalmodelF <- tclVar("")
  ObjectPartitionF <- tclVar("")
  VarFillDfpopulation <- tclVar("")
  VarFillUniv <- tclVar("")
  VarAuxSurveyDataObj <- tclVar("") #NEW
  oldchooseTempl <- "NULL" #NEW
  merge_VarDesign <- tclVar("") #NEW
  merge_VarData <- tclVar("") #NEW
  merge_Design_vars <- NULL #NEW
  merge_Data_vars <- NULL #NEW
  trim_VarDesign <- tclVar("") #NEW
  Index_dataframe_old <- 0
  Index_dataframe_old_Svyby <- 0
  Index_dataframe_DesAddvars <- 0
  Index_dataframe_Ecalibrate <- 0
  Index_choice_Ecalibrate <- 0
  Index_choice_FillTemplate <- 0
  Index_dataframe_FillTemplate <- 0
  Index_choice_DesMerge <- 0 #NEW
  Index_dataframe_DesMerge <- 0 #NEW
  Index_choice_Trimcal <- 0 #NEW

  # Create fonts:
   ## All the titles of sections inside dialogs:
   fontTextTitle <- tkfont.create(family="Helvetica", weight="bold",size=11)
   # fontTextTitle <- tkfont.create(family="Helvetica", weight="bold",size=10)
   ## All the labels inside dialogs:
   fontTextLabel <- tkfont.create(family="Helvetica", weight="bold",size=10)
   ## Fixed type fonts (use TkFixedFont, i.e. courier or similar):
      fixed.family <- get.fontfamily("TkFixedFont")
      ## All the texts inside textwindows (Commands, Warnings)
      fontTextWindow <- tkfont.create(family=fixed.family, size=10)
      ## All the texts inside expression windows (calmodel, partition, ...)
      fontTextExp <- tkfont.create(family=fixed.family, size=10)
      # fontTextExp <- tkfont.create(family="courier", size=9)

  tt <- tktoplevel()

  # Bind an action to the event of closing ReGenesees
  # by pushing the x on the window.
  tcl("wm", "protocol", tt, "WM_DELETE_WINDOW", function() fOnEXIT(tt, textHistory, old.opt))
  tkwm.title(tt, titleRG)
  tkwm.resizable(tt, 0, 0)
  label_Esvydesign <- "e.svydesign"
  label_CollapseStrata <- "collapse.strata"
  label_DesAddvars <- "des.addvars"
  label_SvystatTM <- "svystatTM"
  label_SvystatR <- "svystatR"
  label_SvystatS <- "svystatS"
  label_SvystatSR <- "svystatSR"
  label_SvystatQ <- "svystatQ"
  label_SvystatL <- "svystatL"
  label_SvystatB <- "svystatB"
  label_Ecalibrate <- "e.calibrate"
  label_BoundsHint <- "bounds.hint"
  label_Poptemplate <- "pop.template"
  label_Into <- "%into%"
  label_CheckCal <- "check.cal"
  label_AuxEstimates <- "aux.estimates"
  label_PopDesc <- "pop.desc"
  label_FillTemplate <- "fill.template"
  label_DesMerge <- "des.merge"
  label_Trimcal <- "trimcal" # NEW

  commands <- tclVar("")
  # Here layout in pixels: should translate in cm (which is our standard)...
  frameHistory<- tkframe(tt, borderwidth= 2, padx=5, pady=15)
  lblframeHistory <- tk2label(frameHistory,text="Commands Window", font=fontTextTitle, foreground= "blue")

  scrtextHistory <- tkscrollbar(frameHistory, orient = "vertical",
                                 command=function(...) tkyview(textHistory, ...))
  textHistory <- tktext(frameHistory, height=22, width=90,
                        yscrollcommand=function(...) tkset(scrtextHistory, ...),
                        foreground= "navy", background= "white", state="disabled", wrap="word", font=fontTextWindow)

  tkgrid(lblframeHistory)
  tkgrid(textHistory,scrtextHistory)
  tkgrid.configure(textHistory, pady=c(0,"0.1c"))
  tkgrid.configure(scrtextHistory, pady=c(0,"0.1c"), sticky ="nsw")

  # Enable ctrl-c on textHistory
  ctrl.c(textHistory)
  tkgrid(frameHistory)


  # Here layout in pixels: should translate in cm (which is our standard)...
  frameWarnings <- tkframe(tt, borderwidth=2, padx=5, pady=15)
  lblframeWarnings <- tk2label(frameWarnings, text="Warnings Window", font=fontTextTitle, foreground= "blue")
  scrtextWarnings <- tkscrollbar(frameWarnings, orient = "vertical",
                                  command=function(...) tkyview(textWarnings, ...))
  textWarnings <- tktext(frameWarnings, height=9, width=90,
                         yscrollcommand=function(...) tkset(scrtextWarnings, ...),
                         foreground= "forestgreen", background= "white", state="disabled", wrap="word", font=fontTextWindow) # OLD fg was: "olivedrab4"
  tkgrid(lblframeWarnings)
  tkgrid(textWarnings,scrtextWarnings)
  tkgrid.configure(scrtextWarnings, pady=c(0,"0.2c"), sticky ="nsw")
  tkgrid.configure(textWarnings, pady=c(0,"0.2c"))

  # Enable ctrl-c on textWarnings
  ctrl.c(textWarnings)
  tkgrid(frameWarnings)

  topMenu <- tkmenu(tt)
  tkconfigure(tt, menu=topMenu)

  fileMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(fileMenu,"command",label="Change Directory",command=function() fSet_WRKDIR(textHistory))
  tkadd(fileMenu,"separator")
  tkadd(fileMenu,"command",label="Load Workspace",
                command=function() fCarica_WS(functionsMenu, surveydesignMenu, calibrationMenu, textHistory))
  tkadd(fileMenu,"command",label="Save Workspace",command=function() fSave_Workspace_as(textHistory))
  tkadd(fileMenu,"separator")
  tkadd(fileMenu,"command",label="Save Commands",command=function() fSave_History_as(textHistory))
  tkadd(fileMenu,"separator")
  tkadd(fileMenu,"command",label="Exit",command=function() fOnEXIT(tt, textHistory, old.opt))

  dataMenu <- tkmenu(topMenu,tearoff=FALSE)

  importdataMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(importdataMenu,"command",label="from Text Files",
        command=function() fReadDataSet(textWarnings, functionsMenu, surveydesignMenu, calibrationMenu, textHistory))
  tkadd(importdataMenu,"command",label="from Excel or Access Files",
        command=function() fImportData(functionsMenu, surveydesignMenu, calibrationMenu, textHistory, textWarnings))

  functionsMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(functionsMenu,"command",label="Update Active Functions",
        command=function() fUpdate_active_functions(functionsMenu, surveydesignMenu, calibrationMenu))
  tkadd(functionsMenu,"separator")

  surveydesignMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(surveydesignMenu,"command",label="Specify a Survey Design",
        command=function() fE.svydesign_listaDataFrame())
  tkadd(surveydesignMenu,"command",label="Collapse Strata to Eliminate Lonely PSUs",
        command=function() fCollapseStrata())
  tkadd(surveydesignMenu,"command",label="Add New Variables to a Survey Design",
        command=function() fDesAddvars())
  tkadd(surveydesignMenu,"command",label="Merge New Data into a Survey Design",
        command=function() fDesMerge())

  estimaterrorsMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(estimaterrorsMenu,"command",label="Totals or Means",command=function() fSvystatTM())
  tkadd(estimaterrorsMenu,"command",label="Ratios", command=function() fSvystatR())
  tkadd(estimaterrorsMenu,"command",label="Shares", command=function() fSvystatS())
  tkadd(estimaterrorsMenu,"command",label="Share Ratios", command=function() fSvystatSR())
  tkadd(estimaterrorsMenu,"command",label="Regression Coefficients", command=function() fSvystatB())
  tkadd(estimaterrorsMenu,"command",label="Quantiles", command=function() fSvystatQ())
  tkadd(estimaterrorsMenu,"command",label="Complex Estimators", command=function() fSvystatL())
  tkadd(estimaterrorsMenu,"command",label="Auxiliary Variables Totals", command=function() fAuxEst())
  tkadd(estimaterrorsMenu,"separator")
  tkadd(estimaterrorsMenu,"command",label="Variance Estimation Options",
        command=function() SetVarianceOptions(textHistory))

  calibrationMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(calibrationMenu,"command",label="Build a Template for Population Totals", command=function() fPop.template())
  tkadd(calibrationMenu,"command",label="Describe a Population Totals Template", command=function() fPop.desc())
  tkadd(calibrationMenu,"command",label="Fill a Population Totals Template", command=function() fFill.template())
  tkadd(calibrationMenu,"command",label="Calibrate a Survey Design",command=function() fE.calibrate())
  tkadd(calibrationMenu,"command",label="Check for Calibration Convergence", command=function() fCheck.cal())
  tkadd(calibrationMenu,"command",label="Trim Calibration Weights", command=function() fTrimcal())
  tkadd(calibrationMenu,"separator")
  tkadd(calibrationMenu,"command",label="Calibration Models Options",
        command=function() SetCalmodelOptions(textHistory))

  toolsMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(toolsMenu,"command",label="Browse Workspace",command=function() Obj.browser())
  tkadd(toolsMenu,"command",label="Show Datasets",command=function() fshowData())
  tkadd(toolsMenu,"command",label="Remove Objects",
        command=function() fremoveObjs(functionsMenu, surveydesignMenu, calibrationMenu, textHistory))

  OptionsMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(OptionsMenu,"command",label="Environment Options", command=function() SetEnvOptions(textHistory))

  helpMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(helpMenu,"command",label="ReGenesees Reference Manual", command=function() RGhelp())
  tkadd(helpMenu,"command",label="ReGenesees Flyer", command=function() RGflyer())
  tkadd(helpMenu,"command",label="About the GUI", command=function() fHelpGUI())

  websiteMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(websiteMenu,"command",label="ReGenesees website on GITHUB", command=function() RGwebsite())

  tkadd(topMenu,"cascade",label="File",menu=fileMenu)

  tkadd(topMenu,"cascade",label="Data",menu=dataMenu)
  tkadd(dataMenu,"cascade",label="Import Datasets",menu=importdataMenu)
  tkadd(dataMenu,"command",label="Export Datasets",command=function() fExportData(textWarnings, textHistory))

  tkadd(topMenu,"cascade",label="Functions",menu=functionsMenu)
  tkadd(functionsMenu,"cascade",label="Survey Design",menu=surveydesignMenu, state= "disabled")
  tkadd(functionsMenu,"cascade",label="Calibration",menu=calibrationMenu, state= "disabled")
  tkadd(functionsMenu,"cascade",label="Estimates and Errors",menu=estimaterrorsMenu, state= "disabled")

  tkadd(topMenu,"cascade",label="Tools",menu=toolsMenu)
  tkadd(topMenu,"cascade",label="Options",menu=OptionsMenu)
  tkadd(topMenu,"cascade",label="Help",menu=helpMenu)
  tkadd(topMenu,"cascade",label="Website",menu=websiteMenu)


# OLD CODE (debug 26/04/2016) #
# NOTE: With R 3.4.0 (and new Tcl/Tk 8.6.4) no longer works due to tkpack being
#       now forbidden on widgets with slaves managed by tkgrid
#  RGico.frame <- tkframe(tt, borderwidth= 2, relief = "groove")
#  lblRGico <- tk2label(RGico.frame, image=image_RG.ico,
#                       tip="R Evolved Generalized Software for Sampling Estimates and Errors in Surveys")
#  tkgrid(RGico.frame)
  # Impose a small delay in order to prevent the very rare "intermitting-icon behavior"
#  Sys.sleep(0.0005)
#  tkpack(RGico.frame, lblRGico, anchor="center")
  # Here layout in pixels: should translate in cm (which is our standard)...
#  tkgrid.configure(RGico.frame, sticky="ew", padx=5, pady=c(0, "0.2c"))  
# OLD CODE (debug 26/04/2016) #

# NEW CODE (debug 26/04/2016) #
# NOTE: With R 3.4.0 (and new Tcl/Tk 8.6.4) NOW works BUT the geometry management
#       is VERY INELEGANT (see below).
  RGico.frame <- tkframe(tt, borderwidth= 2, relief = "groove")
  lblRGico <- tk2label(RGico.frame, image=image_RG.ico,
                       tip="R Evolved Generalized Software for Sampling Estimates and Errors in Surveys", anchor="center")
  tkgrid(lblRGico)
  tkgrid(RGico.frame)
  tkgrid.configure(RGico.frame, sticky="ew", padx=5, pady=c(0, "0.2c"))
  # NOTE: Here the ReGenesees name image would naturally be aligned to the left,
  #       instead of being centered (as it has always been in the past, using
  #       tkpack). This bothers me a lot, but I still haven't found any elegant
  #       workaround: I'm currently just adding manually 280 pixels of inner
  #       horizontal space to lblRGico in order to shift it to the center.
  tkgrid.configure(lblRGico, ipadx=280)
# NEW CODE (debug 26/04/2016) #


fUpdate_active_functions <- function(functionsMenu, surveydesignMenu, calibrationMenu){
# Update the Functions Menu Tree
Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)
# Warn if no functions are active
if (!getTemp("any.active.menu", default = FALSE))
    tkmessageBox(title="Active functions", message = "No function enabled for the current workspace objects!",
                 icon = "warning", parent=tt)
#if (!fIs.there.class("data.frame") && !fIs.there.class("analytic"))
#    tkmessageBox(title="Active functions", message = "No function available for the current workspace objects!",
#    icon = "warning")
}


# Warn if the MDI is on
  if (!isSDI()) {
        MDIwarn <- "ReGenesees GUI would work better with the single-document interface (SDI)!"
      tkconfigure(textWarnings, state="normal")
      tkdelete(textWarnings, "1.0", "end")
      tkinsert(textWarnings, "end", paste("Warning:\n", MDIwarn, sep=""))
      tkconfigure(textWarnings, state="disabled")
      }


# Initialize commands window with a SESSION START message and timestamp
startsession <- paste("## ", titleRG, " session start:\n", sep="")
# Flush SESSION START message to Rhistory file
upd.Rhistory(startsession)
# Function timestamp() flushes automatically to Rhistory file
stamp <- paste(timestamp(prefix = "## ", suffix = "", quiet = TRUE),"\n")
# Print on the Commands Window
tkconfigure(textHistory, state="normal")
tkinsert(textHistory, "end", startsession)
tkinsert(textHistory, "end", stamp)
tkinsert(textHistory, "end", "\n")
tkinsert(textHistory, "end", "\n")
tkyview.moveto(textHistory, 1.0)
tkconfigure(textHistory, state="disabled")


# Place main-form with bottom-right corner at screen's bottom-right corner
# NOTE: it seems that moving code below here kills the strange behavior
#       of GUI being SOMETIMES misplaced...
# tkwm.geometry(tt, "-20-40")
# Place main-form with top-right corner at screen's top-right corner #PROVA!#
# tkwm.geometry(tt, "-20+40") #PROVA!#
tkwm.geometry(tt, "-0+0") #PROVA!#
#PROVA!#
# print(tclvalue(tkwm.geometry(tt))) # To asses the size of the window...
tkfocus(tt)
# Drawing and placement of main window ends here.


mk.class.list <- function(class){
######################################
# Build a list of .GlobalEnv objects #
# inheriting from a given class.     #
######################################
  names(which( sapply(ls(.GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), class)) ))
}


# --------------------------------------
# > START building e.svydesign window. >
# --------------------------------------


fE.svydesign_listaDataFrame <- function(){
listaDataF <- listDataSets()

ttEsvydesign <- tktoplevel()
tcl("wm", "protocol", ttEsvydesign, "WM_DELETE_WINDOW", function() fOnCancel(ttEsvydesign))

frameGlobal<- tkframe(ttEsvydesign, borderwidth= 2)
tkwm.deiconify(ttEsvydesign)
tkgrab.set(ttEsvydesign)
tkfocus(ttEsvydesign)
tkwm.title(ttEsvydesign,label_Esvydesign)
tkwm.resizable(ttEsvydesign, 0, 0)

frameTop     <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
frameDown    <- tkframe(frameGlobal, relief="groove", borderwidth=2, padx="0.1c", pady=c("0.2c","0.4c"))
frameOutput  <- tkframe(frameGlobal, borderwidth=0)
frameButtons <- tkframe(frameGlobal, borderwidth=0)

scrDataSets <- tkscrollbar(frameTop, orient = "vertical",
                            command = function(...) tkyview(lstDataFrame,...))
lista <- tclVar()
tclObj(lista) <- listaDataF
lstDataFrame <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="single",
                          yscrollcommand = function (...) tkset(scrDataSets,...), background = "white")

scrVariables <- tkscrollbar(frameTop, orient = "vertical",
                             command = function(...) tkyview(lstVariables,...))

listaVariables <- tclVar()
lstVariables <- tklistbox(frameTop,height = 4, listvariable= listaVariables, selectmode="extended",
                          yscrollcommand = function (...)tkset(scrVariables,...), background = "white")

lblfDataSet<- tk2labelframe(frameTop)

count <<- FALSE
tkbind(lstDataFrame, "<ButtonRelease-1>",
function() fElencoCampi(lista, lstDataFrame, lblVariables, listaVariables, lstVariables,
                               scrVariables, lblESvyDataframe, ids.but, weights.but, strata.but,
                               fpc.but, self.rep.str.but, ids.ri.but, weights.ri.but, strata.ri.but,
                               fpc.ri.but, self.rep.str.ri.but, ok.but, lstIds, lstWeights, lstStrata,
                               lstFpc, lstSelf.rep.str, entry.ObjectName))

lblDataframe <- ttklabel(frameTop, text="Select a survey dataframe", font=fontTextLabel)
lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")

lblESvyDataframe <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarESvyDataframe)),foreground="red")

labellblfIds <- tk2label(frameCentral,text="  ids  ", font=fontTextLabel, image=image_qm, compound="right")
descfunz <- descArgs("e.svydesign", args = "ids")
tk2tip(labellblfIds,descfunz)

lblfIds<- tk2labelframe(frameCentral, labelwidget= labellblfIds)

frameIds <- tkframe(lblfIds, borderwidth=0)
frameIds.but <- tkframe(lblfIds, borderwidth=0)
scrIds <- tkscrollbar(frameIds, repeatinterval= 5, command = function(...) tkyview(lstIds,...))

listaIds <- tclVar()
lstIds <- tklistbox(frameIds, height=4, listvariable=listaIds, selectmode="extended",
                    yscrollcommand = function (...)tkset(scrIds,...), background = "white")

labellblfWeights <- tk2label(frameCentral,text="  weights  ", font=fontTextLabel, image=image_qm, compound="right")
descfunz <- descArgs("e.svydesign", args = "weights")
tk2tip(labellblfWeights,descfunz)

lblfWeights<- tk2labelframe(frameCentral, labelwidget=labellblfWeights)
frameWeights <- tkframe(lblfWeights, borderwidth=0)
frameWeights.but <- tkframe(lblfWeights, borderwidth=0)
scrWeights <- tkscrollbar(frameWeights, repeatinterval=5, command = function(...) tkyview(lstWeights,...))
listaWeights <- tclVar()
lstWeights <- tklistbox(frameWeights, height=4, listvariable=listaWeights, selectmode="extended",
                        yscrollcommand = function (...)tkset(scrWeights,...), background = "white")

labellblfStrata <- tk2label(frameDown,text="  strata  ", font=fontTextLabel, image=image_qm, compound="right")
descfunz <- descArgs("e.svydesign", args = "strata")
tk2tip(labellblfStrata,descfunz)

lblfStrata<- tk2labelframe(frameDown, labelwidget= labellblfStrata)
frameStrata <- tkframe(lblfStrata, borderwidth=0)
frameStrata.but <- tkframe(lblfStrata, borderwidth=0)
scrStrata <- tkscrollbar(frameStrata, repeatinterval= 5, command = function(...) tkyview(lstStrata,...))
listaStrata <- tclVar()
lstStrata <- tklistbox(frameStrata, height=4, listvariable=listaStrata, selectmode="extended",
                       yscrollcommand = function (...)tkset(scrStrata,...), background = "white")

labellblfFpc <- tk2label(frameDown,text="  fpc  ", font=fontTextLabel, image=image_qm, compound="right")
descfunz <- descArgs("e.svydesign", args = "fpc")
tk2tip(labellblfFpc,descfunz)

lblfFpc<- tk2labelframe(frameDown, labelwidget=labellblfFpc)
frameFpc <- tkframe(lblfFpc, borderwidth=0)
frameFpc.but <- tkframe(lblfFpc, borderwidth=0)
scrFpc <- tkscrollbar(frameFpc, repeatinterval= 5, command = function(...) tkyview(lstFpc,...))
listaFpc <- tclVar()
lstFpc <- tklistbox(frameFpc, height=4, listvariable=listaFpc, selectmode="extended",
                    yscrollcommand = function (...)tkset(scrFpc,...), background = "white")

labellblfSelf.rep.str <- tk2label(frameDown,text="  self.rep.str  ", font=fontTextLabel, image=image_qm,
                                  compound="right")
descfunz <- descArgs("e.svydesign", args = "self.rep.str")
tk2tip(labellblfSelf.rep.str,descfunz)

lblfSelf.rep.str<- tk2labelframe(frameDown, labelwidget=labellblfSelf.rep.str)
frameSelf.rep.str <- tkframe(lblfSelf.rep.str, borderwidth=0)
frameSelf.rep.str.but <- tkframe(lblfSelf.rep.str, borderwidth=0)
scrSelf.rep.str <- tkscrollbar(frameSelf.rep.str, repeatinterval= 5,
                               command = function(...) tkyview(lstSelf.rep.str,...))
listaSelf.rep.str <- tclVar()
lstSelf.rep.str <- tklistbox(frameSelf.rep.str, height=4, listvariable=listaSelf.rep.str, selectmode="extended",
                             yscrollcommand = function (...)tkset(scrSelf.rep.str,...), background = "white")

ids.but <- tk2button(frameIds.but, image=image_sx, state= "disabled",
                     command = function()fTransfer(lstIds, lstVariables, listaVariables))
ids.ri.but <- tk2button(frameIds.but, image=image_dx, state="disabled",
                        command = function()fDetransfer(lstIds, lstVariables, listaIds))

weights.but <- tk2button(frameWeights.but, image=image_sx, state= "disabled",
                         command = function()fTransfer(lstWeights, lstVariables, listaVariables))
weights.ri.but <- tk2button(frameWeights.but, image=image_dx, state= "disabled",
                            command = function()fDetransfer(lstWeights, lstVariables, listaWeights))

strata.but <- tk2button(frameStrata.but, image=image_sx, state= "disabled",
                        command= function()fTransfer(lstStrata, lstVariables, listaVariables))
strata.ri.but <- tk2button(frameStrata.but, image=image_dx, state= "disabled",
                           command = function()fDetransfer(lstStrata, lstVariables, listaStrata))

fpc.but <- tk2button(frameFpc.but, image=image_sx, state= "disabled",
                     command = function()fTransfer(lstFpc, lstVariables, listaVariables))
fpc.ri.but <- tk2button(frameFpc.but, image=image_dx, state= "disabled",
                        command = function()fDetransfer(lstFpc, lstVariables, listaFpc))

self.rep.str.but <- tk2button(frameSelf.rep.str.but, image=image_sx, state= "disabled",
                              command = function()fTransfer(lstSelf.rep.str, lstVariables, listaVariables))
self.rep.str.ri.but <- tk2button(frameSelf.rep.str.but, image=image_dx, state= "disabled",
                                 command = function()fDetransfer(lstSelf.rep.str, lstVariables, listaSelf.rep.str))

lblMandatory <- tk2label(frameGlobal,text="Mandatory Fields", font=fontTextTitle, foreground= "blue")
lblOptional <-tk2label(frameGlobal,text="Optional Fields", font=fontTextTitle, foreground= "blue")

labellblfCheck.data <- ttklabel(frameDown,text="  check.data  ", font=fontTextLabel, image=image_qm, compound="right")
descfunz <- descArgs("e.svydesign", args = "check.data")
tk2tip(labellblfCheck.data,descfunz)
lblfCheck.data<- ttklabelframe(frameDown, labelwidget=labellblfCheck.data)

rbCheck.dataF <- ttkradiobutton(lblfCheck.data)
rbCheck.dataT <- ttkradiobutton(lblfCheck.data)
rbValueCheck.data <- tclVar("TRUE")

tkconfigure(rbCheck.dataF ,variable=rbValueCheck.data,value="FALSE")
tkconfigure(rbCheck.dataT,variable=rbValueCheck.data,value="TRUE")

labelCheck.dataF <- ttklabel(lblfCheck.data,text="False ")
labelCheck.dataT <- ttklabel(lblfCheck.data,text="True ")

labellblfEsvydesignObj <- ttklabel(frameOutput,text="  Output object name  ", font=fontTextTitle, foreground= "blue")
lblfEsvydesignObj<- ttklabelframe(frameOutput, labelwidget = labellblfEsvydesignObj)

ObjectName <- tclVar("")
entry.ObjectName <- ttkentry(lblfEsvydesignObj, width="20", text=ObjectName, state= "disabled", font="TkDefaultFont")
ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                    command=function() fOnRun(lstIds,lstStrata,lstWeights,lstFpc,lstSelf.rep.str,rbValueCheck.data,
                                              ObjectName,ttEsvydesign))
Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left",
                        command=function() fOnCancel(ttEsvydesign))
FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left",
                              tip=descFun(label_Esvydesign), command=function() fOnFunctionHelp(label_Esvydesign))


tkgrid.configure(tk2label(frameGlobal, text="Sample Data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))
tkgrid(lblfDataSet)
tkgrid(lblDataframe, lblVariables)
tkgrid(lblESvyDataframe)
tkgrid(lstDataFrame, scrDataSets)

tkgrid.configure(lblDataframe, column=1, sticky ="e")
tkgrid.configure(lblVariables, column=4)
tkgrid.configure(lblESvyDataframe, row=2, column=1, sticky ="e")

tkgrid.configure(lstDataFrame, column=1, row=3, sticky ="e", padx=c("1.7c","0c"), pady=c(0,"0.2c"))
tkgrid.configure(scrDataSets, column=2, row=3, sticky ="nsw", padx=c(0,"1.9c"), pady=c(0,"0.2c"))

tkgrid(lblfIds,lblfWeights)
tkgrid.configure(lblfIds,padx=c("0.5c",0), pady=c(0,"0.2c"))
tkgrid.configure(lblfWeights,padx=c("2c","0.5c"), pady=c(0,"0.2c"))

tkgrid(frameIds.but, frameIds)
tkgrid.configure(frameIds.but, padx= c("0.1c"), pady=c(0,"0.3c"))
tkgrid.configure(frameIds, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
tkgrid(ids.but)
tkgrid(ids.ri.but)
tkgrid(lstIds, scrIds)

tkgrid(frameWeights.but, frameWeights)
tkgrid.configure(frameWeights.but, padx= c("0.1c"), pady=c(0,"0.3c"))
tkgrid.configure(frameWeights, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
tkgrid(weights.but)
tkgrid(weights.ri.but)
tkgrid(lstWeights, scrWeights)

tkgrid.configure(ids.but, pady=c(0,"0.2c"))
tkgrid.configure(ids.ri.but, pady=c("0.2c",0))

tkgrid.configure(scrIds, sticky ="nsw")

tkgrid.configure(weights.but, pady=c(0,"0.2c"))
tkgrid.configure(weights.ri.but, pady=c("0.2c",0))

tkgrid.configure(lstWeights, sticky ="e")
tkgrid.configure(scrWeights, sticky ="nsw")

tkgrid(frameStrata.but, frameStrata)
tkgrid.configure(frameStrata.but, padx= c("0.1c"), pady=c(0,"0.3c"))
tkgrid.configure(frameStrata, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
tkgrid(strata.but)
tkgrid(strata.ri.but)
tkgrid(lstStrata, scrStrata)

tkgrid(frameFpc.but, frameFpc)
tkgrid.configure(frameFpc.but, padx= c("0.1c"), pady=c(0,"0.3c"))
tkgrid.configure(frameFpc, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
tkgrid(fpc.but)
tkgrid(fpc.ri.but)
tkgrid(lstFpc, scrFpc)

tkgrid(frameSelf.rep.str.but, frameSelf.rep.str)
tkgrid.configure(frameSelf.rep.str.but, padx= c("0.1c"), pady=c(0,"0.3c"))
tkgrid.configure(frameSelf.rep.str, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
tkgrid(self.rep.str.but)
tkgrid(self.rep.str.ri.but)
tkgrid(lstSelf.rep.str, scrSelf.rep.str)

tkgrid.configure(strata.but, pady=c(0,"0.2c"))
tkgrid.configure(strata.ri.but, pady=c("0.2c",0))
tkgrid.configure(lstStrata, sticky ="w")
tkgrid.configure(scrStrata, sticky ="nsw")

tkgrid.configure(fpc.but, pady=c(0,"0.2c"))
tkgrid.configure(fpc.ri.but, pady=c("0.2c",0))
tkgrid.configure(lstFpc, sticky ="e")
tkgrid.configure(scrFpc, sticky ="nsw")

tkgrid.configure(self.rep.str.but, pady=c(0,"0.2c"))
tkgrid.configure(self.rep.str.ri.but, pady=c("0.2c",0))
tkgrid.configure(lstSelf.rep.str, sticky ="e")
tkgrid.configure(scrSelf.rep.str, sticky ="nsw")

tkgrid(labelCheck.dataF,rbCheck.dataF)
tkgrid(labelCheck.dataT,rbCheck.dataT)

tkgrid.configure(labelCheck.dataF, sticky="w", padx=c("0.3c",0))
tkgrid.configure(labelCheck.dataT, sticky="w", padx=c("0.3c",0))

tkgrid(lblfStrata, lblfFpc, lblfSelf.rep.str, lblfCheck.data)
tkgrid.configure(lblfStrata, padx=c("0.5c","0.3c"), pady=c(0,"0.2c"))
tkgrid.configure(lblfFpc, padx="0.3c", pady=c(0,"0.2c"))
tkgrid.configure(lblfSelf.rep.str, padx="0.3c", pady=c(0,"0.2c"))
tkgrid.configure(lblfCheck.data, padx=c("0.3c","0.5c"))

tkgrid(frameTop)
tkgrid(lblMandatory, pady=c("0.2c",0))

tkgrid(frameCentral)
tkgrid(lblOptional, pady=c("0.2c",0))

tkgrid(frameDown)
tkgrid.configure(frameDown, padx="0.5c")

tkgrid(lblfEsvydesignObj)
tkgrid.configure(entry.ObjectName, padx=c("0.5c","7c"),pady=c(0,"0.3c"))

tkgrid(frameOutput)
tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")

tkgrid(frameButtons)
tkgrid.configure(frameButtons, sticky="ne")
tkgrid(ok.but, Cancel.but, FunctionHelp.but)
tkgrid.configure(Cancel.but, padx=("0.5c"))
tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))

tkgrid(frameGlobal)
#PROVA!#
# print(tclvalue(tkwm.geometry(ttEsvydesign))) # To asses the size of the window...
# tkfocus(ttEsvydesign)
}


fElencoCampi <- function(EC_lista, EC_lstDataFrame, EC_lblVariables, x, lstEC_DataFrame, scrEC,
                         EC_lblESvyDataframe, EC_ids.but, EC_weights.but, EC_strata.but, EC_fpc.but,
                         EC_self.rep.str.but, EC_ids.ri.but, EC_weights.ri.but, EC_strata.ri.but,
                         EC_fpc.ri.but, EC_self.rep.str.ri.but, EC_ok.but, EC_lstIds, EC_lstWeights,
                         EC_lstStrata, EC_lstFpc, EC_lstSelf.rep.str, EC_entry.ObjectName){

EC_indicesel <- tclvalue(tkcurselection(EC_lstDataFrame))

assignTemp("DF_Esvydesign_Name", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstDataFrame))+1])

# DIRE A RAFFAELLA 16/07/10
# NO!!! Copying may become a serious concern when data get big
# (can tolerate it for names, because they are small: see above)
# assignTemp("DF_Esvydesign_Value", get(DF_Esvydesign_Name, envir=.GlobalEnv))
DF_Esvydesign_Value <- get(DF_Esvydesign_Name, envir=.GlobalEnv)
# DIRE A RAFFAELLA 16/07/10

VarESvyDataframe <<- tclVar(DF_Esvydesign_Name)
tkconfigure(EC_lblESvyDataframe, textvariable= VarESvyDataframe)

if (Index_dataframe_old !=EC_indicesel)
    count <<- FALSE
if (count == FALSE){
    count <<- TRUE
    Index_dataframe_old <<- EC_indicesel

    tkdelete(EC_lstIds, 0, "end")
    tkdelete(EC_lstWeights, 0, "end")
    tkdelete(EC_lstStrata, 0, "end")
    tkdelete(EC_lstFpc, 0, "end")
    tkdelete(EC_lstSelf.rep.str, 0, "end")
    tkdelete(EC_entry.ObjectName, 0, "end")

# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#    for (n in names(DF_Esvydesign_Value)){
#         tclObj(x) <- c(names(DF_Esvydesign_Value))
#        }
    tclObj(x) <- names(DF_Esvydesign_Value)
    tkgrid(lstEC_DataFrame, scrEC)
    tkgrid.configure(lstEC_DataFrame, column=4, row=3, sticky ="e", pady=c(0,"0.2c"))
    tkgrid.configure(scrEC, column=5, row=3, padx=c("0c","1.7c"), pady=c(0,"0.2c"), sticky ="nsw")
    tkconfigure(EC_ids.but, state = "normal")
    tkconfigure(EC_weights.but, state = "normal")
    tkconfigure(EC_strata.but, state = "normal")
    tkconfigure(EC_fpc.but, state = "normal")
    tkconfigure(EC_self.rep.str.but, state = "normal")
    tkconfigure(EC_ids.ri.but, state = "normal")
    tkconfigure(EC_weights.ri.but, state = "normal")
    tkconfigure(EC_strata.ri.but, state = "normal")
    tkconfigure(EC_fpc.ri.but, state = "normal")
    tkconfigure(EC_self.rep.str.ri.but, state = "normal")
    tkconfigure(EC_ok.but, state = "normal")
    tkconfigure(EC_entry.ObjectName, state = "normal")
    tkconfigure(EC_lblVariables, text="Variables", state = "normal")
    }
}


fOnRun <- function(OR_lstIds,OR_lstStrata,OR_lstWeights,OR_lstFpc,OR_lstSelf.rep.str,
                   OR_rbValueCheck.data,OR_ObjectName, ttEsvydesign){
campobb <- TRUE

all.lstIds <- tclvalue(tkget(OR_lstIds, 0, "end"))
if  (all.lstIds =="") {
    tkmessageBox(title="Ids list empty ", message = "The list is empty. Transfer at least one element",
                 icon = "error", parent=ttEsvydesign)
    campobb <- FALSE
    }
else{
    idssum <- gsub(" ", "+",all.lstIds)
    ids <- as.formula(paste("~", idssum), env = .GlobalEnv)
    prnIds <- paste("ids=", idssum, sep =" ~ ")
    }

num <- as.integer(tksize(OR_lstWeights))
if  (num == 0) {
    tkmessageBox(title="Weights list empty", message = "The list is empty. Transfer one element", icon = "error",
                 parent=ttEsvydesign)
    campobb <- FALSE
    }
else{
    if  (num>1){
        tkmessageBox(title="Weights list", message = "The list must contain only one variable", icon = "error",
                     parent=ttEsvydesign)
        campobb <- FALSE
        }
    else{
        all.lstWeights <- tclvalue(tkget(OR_lstWeights, 0, 0))
        weights <- as.formula(paste("~", all.lstWeights), env = .GlobalEnv)
        prnWeights <- paste("weights=", all.lstWeights, sep =" ~ ")
        }
    }

all.lstStrata <- tclvalue(tkget(OR_lstStrata, 0, "end"))
if  (all.lstStrata ==""){
    strata <- NULL
    prnStrata <- "strata= NULL"
    }
else{
    stratasum <- gsub(" ", "+",all.lstStrata)
    strata <- as.formula(paste("~", stratasum), env = .GlobalEnv)
    prnStrata <- paste("strata=", stratasum, sep =" ~ ")
    }

all.lstFpc <- tclvalue(tkget(OR_lstFpc, 0, "end"))
if  (all.lstFpc ==""){
    fpc <- NULL
    prnFpc <- "fpc= NULL"
    }
else{
    fpcsum <- gsub(" ", "+",all.lstFpc)
    fpc <- as.formula(paste("~", fpcsum), env = .GlobalEnv)
    prnFpc<- paste("fpc=", fpcsum, sep =" ~ ")
    }

all.lstSelf.rep.str <- tclvalue(tkget(OR_lstSelf.rep.str, 0, "end"))
if  (all.lstSelf.rep.str ==""){
    self.rep.str <- NULL
    prnSelf.rep.str <- "self.rep.str= NULL"
    }
else{
    self.rep.strsum <- gsub(" ", "+",all.lstSelf.rep.str)
    self.rep.str <- as.formula(paste("~", self.rep.strsum), env = .GlobalEnv)
    prnSelf.rep.str <- paste("self.rep.str=", self.rep.strsum, sep =" ~ ")
    }

check.data <- as.logical(tclvalue(OR_rbValueCheck.data))
if  (!check.data) {
    prnCheckData <- "check.data= FALSE"
    }
else{
    prnCheckData <- "check.data= TRUE"
    }

if  (tclvalue(OR_ObjectName)==""){
    tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error",
                 parent=ttEsvydesign)
    campobb <- FALSE
    }
else{
    if   ( !is.ok.name(tclvalue(OR_ObjectName), go.on=campobb, parent=ttEsvydesign) ){
         campobb <- FALSE
         }
    else {
         OR_ObjectName <- tclvalue(OR_ObjectName)
         }
    }

if  (campobb == TRUE){
    # change the cursor to the hourglass to tell work is in progress...
    tkconfigure(ttEsvydesign, cursor="watch")

    # DIRE A RAFFAELLA 16/07/10
    # DF_Esvydesign_Value no more exists into TempEnv: we left inside its name only
    DF_Esvydesign_Value <- get(DF_Esvydesign_Name, envir=.GlobalEnv)
    # DIRE A RAFFAELLA 16/07/10

    valEsvydesign <- Lancia(e.svydesign(DF_Esvydesign_Value,ids,strata,weights,fpc,
                            self.rep.str,check.data), textWarnings, parent = ttEsvydesign)

    if  (!inherits(valEsvydesign,"try-error")) {
        attr(valEsvydesign,"data") <- as.symbol(DF_Esvydesign_Name)
        prnData <- paste("data=", DF_Esvydesign_Name)
        valEsvydesign[["call"]] <- paste("e.svydesign(", prnData, ", ", prnIds,    ", ",
                                                         prnStrata, ", ", prnWeights, ", ",
                                                         prnFpc, ", ", prnSelf.rep.str,    ", ",
                                                         prnCheckData, ")", sep="")

        # assign(OR_ObjectName, valEsvydesign, envir = .GlobalEnv)
        assign2GE(OR_ObjectName, valEsvydesign)
        Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

        cat("\n")
        cat(paste("# ",OR_ObjectName,"\n",sep=""))
        print(valEsvydesign)
        cat("\n")

        prnEsvydesign <- paste(" <- e.svydesign(", prnData, ", ", prnIds, ", ", prnStrata, ", ",
                                                   prnWeights, ", ", prnFpc, ", ", prnSelf.rep.str, ", ",
                                                   prnCheckData, ")", sep="")

        commands <- paste(OR_ObjectName, prnEsvydesign, "\n", sep="")

        # Print on the Commands Window
        tkconfigure(textHistory, state="normal")
        tkinsert(textHistory, "end", commands)
        tkinsert(textHistory, "end", "\n")
        tkyview.moveto(textHistory, 1.0)
        tkconfigure(textHistory, state="disabled")
        # End

        # Flush commands to Rhistory file
        upd.Rhistory(commands, RG.stamp = TRUE)
        # End

        if (getTemp("there.are.warnings", default = FALSE)){
            tkmessageBox(title ="e.svydesign",
                         message = "Operation executed\nNote: Warnings have been generated!",
                         icon = "info", parent=ttEsvydesign)
            }
        else {
              tkmessageBox(title ="e.svydesign",message = "Operation executed", icon = "info", parent=ttEsvydesign)
             }
        tkgrab.release(ttEsvydesign)
        }
    # get back the standard arrow cursor
    tkconfigure(ttEsvydesign, cursor="arrow")
    # Collect garbage to free memory
    gc()
    }
}


# ------------------------------------
# < END building e.svydesign window. <
# ------------------------------------

# ------------------------------------------
# > START building collapse.strata window. >
# ------------------------------------------

fCollapseStrata <- function(){
listEsvydesignObj <-  mk.class.list("analytic")

ttClpsStrat <- tktoplevel()
tcl("wm", "protocol", ttClpsStrat, "WM_DELETE_WINDOW", function() fOnCancel(ttClpsStrat))

frameGlobal<- tkframe(ttClpsStrat, borderwidth= 2)
tkwm.deiconify(ttClpsStrat)
tkgrab.set(ttClpsStrat)
tkfocus(ttClpsStrat)
tkwm.title(ttClpsStrat,label_CollapseStrata)
tkwm.resizable(ttClpsStrat, 0, 0)

frameTop     <- tkframe(frameGlobal, relief="groove", borderwidth=2, padx="0.8c", pady=c("0.2c","0.4c"))
frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, padx="0.8c", pady=c("0.2c","0.4c"))
frameOutput  <- tkframe(frameGlobal, borderwidth=0)
frameButtons <- tkframe(frameGlobal, borderwidth=0)

scrEsvydesignObj <- tkscrollbar(frameTop, orient = "vertical",
                            command = function(...) tkyview(lstDesign,...))
lista <- tclVar()
tclObj(lista) <- listEsvydesignObj
lstDesign <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="single",
                          yscrollcommand = function (...) tkset(scrEsvydesignObj,...), background = "white")

scrVariables <- tkscrollbar(frameTop, orient = "vertical",
                             command = function(...) tkyview(lstVariables,...))

listaVariables <- tclVar()
lstVariables <- tklistbox(frameTop,height = 4, listvariable= listaVariables, selectmode="extended",
                          yscrollcommand = function (...)tkset(scrVariables,...), background = "white")

lblfDesign<- tk2labelframe(frameTop)

count <<- FALSE
tkbind(lstDesign, "<ButtonRelease-1>",
function() fElencoCampiCollapseStrata(lista, lstDesign, lblVariables, listaVariables, lstVariables,
                               scrVariables, lblESvyDesign, block.vars.but, sim.score.but,
                               block.vars.ri.but, sim.score.ri.but,
                               ok.but, lstBlockVars, lstSimScore,
                               entry.ObjectName))

lblEsvydesignObj <- ttklabel(frameTop, text="Select a survey design object", font=fontTextLabel)
lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")

lblESvyDesign <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarESvyDesign)),foreground="red")

labellblfBlockVars <- tk2label(frameCentral,text="  block.vars  ", font=fontTextLabel, image=image_qm, compound="right")
descfunz <- descArgs("collapse.strata", args = "block.vars")
tk2tip(labellblfBlockVars,descfunz)

lblfBlockVars<- tk2labelframe(frameCentral, labelwidget= labellblfBlockVars)

frameBlockVars <- tkframe(lblfBlockVars, borderwidth=0)
frameBlockVars.but <- tkframe(lblfBlockVars, borderwidth=0)
scrBlockVars <- tkscrollbar(frameBlockVars, repeatinterval= 5, command = function(...) tkyview(lstBlockVars,...))

listaBlockVars <- tclVar()
lstBlockVars <- tklistbox(frameBlockVars, height=4, listvariable=listaBlockVars, selectmode="extended",
                    yscrollcommand = function (...)tkset(scrBlockVars,...), background = "white")

labellblfSimScore <- tk2label(frameCentral,text="  sim.score  ", font=fontTextLabel, image=image_qm, compound="right")
descfunz <- descArgs("collapse.strata", args = "sim.score")
tk2tip(labellblfSimScore,descfunz)

lblfSimScore<- tk2labelframe(frameCentral, labelwidget=labellblfSimScore)
frameSimScore <- tkframe(lblfSimScore, borderwidth=0)
frameSimScore.but <- tkframe(lblfSimScore, borderwidth=0)
scrSimScore <- tkscrollbar(frameSimScore, repeatinterval=5, command = function(...) tkyview(lstSimScore,...))
listaSimScore <- tclVar()
lstSimScore <- tklistbox(frameSimScore, height=4, listvariable=listaSimScore, selectmode="extended",
                        yscrollcommand = function (...)tkset(scrSimScore,...), background = "white")

block.vars.but <- tk2button(frameBlockVars.but, image=image_sx, state= "disabled",
                     command = function()fTransfer(lstBlockVars, lstVariables, listaVariables))
block.vars.ri.but <- tk2button(frameBlockVars.but, image=image_dx, state="disabled",
                        command = function()fDetransfer(lstBlockVars, lstVariables, listaBlockVars))

sim.score.but <- tk2button(frameSimScore.but, image=image_sx, state= "disabled",
                         command = function()fTransfer(lstSimScore, lstVariables, listaVariables))
sim.score.ri.but <- tk2button(frameSimScore.but, image=image_dx, state= "disabled",
                            command = function()fDetransfer(lstSimScore, lstVariables, listaSimScore))

lblOptional <- tk2label(frameGlobal,text="Optional Fields", font=fontTextTitle, foreground= "blue")

labellblfClpsStratObj <- ttklabel(frameOutput,text="  Output object name  ", font=fontTextTitle, foreground= "blue")
lblfClpsStratObj<- ttklabelframe(frameOutput, labelwidget = labellblfClpsStratObj)

ObjectName <- tclVar("")
entry.ObjectName <- ttkentry(lblfClpsStratObj,width="20",text=ObjectName, state= "disabled", font="TkDefaultFont")
ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                    command=function() fOnRun_CollapseStrata(lstBlockVars,lstSimScore,
                                              ObjectName,ttClpsStrat))
Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left",
                        command=function() fOnCancel(ttClpsStrat))
FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left",
                              tip=descFun(label_CollapseStrata), command=function() fOnFunctionHelp(label_CollapseStrata))


tkgrid.configure(tk2label(frameGlobal, text="Sample Data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))
tkgrid(lblfDesign)
tkgrid(lblEsvydesignObj, lblVariables)
tkgrid(lblESvyDesign)
tkgrid(lstDesign, scrEsvydesignObj)

tkgrid.configure(lblEsvydesignObj, column=1, sticky ="e")
tkgrid.configure(lblVariables, column=4)
tkgrid.configure(lblESvyDesign, row=2, column=1, sticky ="e")

tkgrid.configure(lstDesign, column=1, row=3, sticky ="e", padx=c("1.7c","0c"), pady=c(0,"0.2c"))
tkgrid.configure(scrEsvydesignObj, column=2, row=3, sticky ="nsw", padx=c(0,"1.9c"), pady=c(0,"0.2c"))

tkgrid(lblfBlockVars,lblfSimScore)
tkgrid.configure(lblfBlockVars,padx=c("0.5c",0), pady=c(0,"0.2c"))
tkgrid.configure(lblfSimScore,padx=c("2c","0.5c"), pady=c(0,"0.2c"))

tkgrid(frameBlockVars.but, frameBlockVars)
tkgrid.configure(frameBlockVars.but, padx= c("0.1c"), pady=c(0,"0.3c"))
tkgrid.configure(frameBlockVars, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
tkgrid(block.vars.but)
tkgrid(block.vars.ri.but)
tkgrid(lstBlockVars, scrBlockVars)

tkgrid(frameSimScore.but, frameSimScore)
tkgrid.configure(frameSimScore.but, padx= c("0.1c"), pady=c(0,"0.3c"))
tkgrid.configure(frameSimScore, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
tkgrid(sim.score.but)
tkgrid(sim.score.ri.but)
tkgrid(lstSimScore, scrSimScore)

tkgrid.configure(block.vars.but, pady=c(0,"0.2c"))
tkgrid.configure(block.vars.ri.but, pady=c("0.2c",0))

tkgrid.configure(scrBlockVars, sticky ="nsw")

tkgrid.configure(sim.score.but, pady=c(0,"0.2c"))
tkgrid.configure(sim.score.ri.but, pady=c("0.2c",0))

tkgrid.configure(lstSimScore, sticky ="e")
tkgrid.configure(scrSimScore, sticky ="nsw")

tkgrid(frameTop)
tkgrid(lblOptional, pady=c("0.2c",0))

tkgrid(frameCentral)
tkgrid.configure(frameCentral, padx="0.5c")

tkgrid(lblfClpsStratObj)
tkgrid.configure(entry.ObjectName, padx=c("0.5c", "1.5c"), pady=c(0,"0.3c"))

tkgrid(frameOutput)
tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")

tkgrid(frameButtons)
tkgrid.configure(frameButtons, sticky="ne")
tkgrid(ok.but, Cancel.but, FunctionHelp.but)
tkgrid.configure(ok.but, padx=c("6c",0))
tkgrid.configure(Cancel.but, padx=("0.5c"))
tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))

tkgrid(frameGlobal)
#PROVA!#
# print(tclvalue(tkwm.geometry(ttClpsStrat))) # To asses the size of the window...
# tkfocus(ttClpsStrat)
}


fElencoCampiCollapseStrata <- function(EC_lista, EC_lstDesign, EC_lblVariables, x, lstEC_DataFrame, scrEC,
                         EC_lblESvyDesign, EC_block.vars.but, EC_sim.score.but,
                         EC_block.vars.ri.but, EC_sim.score.ri.but,
                         EC_ok.but, EC_lstBlockVars, EC_lstSimScore,
                         EC_entry.ObjectName){

EC_indicesel <- tclvalue(tkcurselection(EC_lstDesign))

assignTemp("Des_ClpsStrat_Name", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstDesign))+1])

# DIRE A RAFFAELLA 16/07/10
# NO!!! Copying may become a serious concern when data get big
# (can tolerate it for names, because they are small: see above)
# assignTemp("Des_ClpsStrat_Value", get(Des_ClpsStrat_Name, envir=.GlobalEnv))
Des_ClpsStrat_Value <- get(Des_ClpsStrat_Name, envir=.GlobalEnv)
# DIRE A RAFFAELLA 16/07/10

VarESvyDesign <<- tclVar(Des_ClpsStrat_Name)
tkconfigure(EC_lblESvyDesign, textvariable= VarESvyDesign)

if (Index_dataframe_old !=EC_indicesel)
    count <<- FALSE
if (count == FALSE){
    count <<- TRUE
    Index_dataframe_old <<- EC_indicesel

    tkdelete(EC_lstBlockVars, 0, "end")
    tkdelete(EC_lstSimScore, 0, "end")
    tkdelete(EC_entry.ObjectName, 0, "end")

# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#    for (n in names(Des_ClpsStrat_Value$variables)){
#         tclObj(x) <- c(names(Des_ClpsStrat_Value$variables))
#        }
    tclObj(x) <- names(Des_ClpsStrat_Value$variables)
    tkgrid(lstEC_DataFrame, scrEC)
    tkgrid.configure(lstEC_DataFrame, column=4, row=3, sticky ="e", pady=c(0,"0.2c"))
    tkgrid.configure(scrEC, column=5, row=3, padx=c("0c","1.7c"), pady=c(0,"0.2c"), sticky ="nsw")
    tkconfigure(EC_block.vars.but, state = "normal")
    tkconfigure(EC_sim.score.but, state = "normal")
    tkconfigure(EC_block.vars.ri.but, state = "normal")
    tkconfigure(EC_sim.score.ri.but, state = "normal")
    tkconfigure(EC_ok.but, state = "normal")
    tkconfigure(EC_entry.ObjectName, state = "normal")
    tkconfigure(EC_lblVariables, text="Variables", state = "normal")
    }
}


fOnRun_CollapseStrata <- function(OR_lstBlockVars,OR_lstSimScore,
                   OR_ObjectName, ttClpsStrat){
campobb <- TRUE

all.lstBlockVars <- tclvalue(tkget(OR_lstBlockVars, 0, "end"))
if  (all.lstBlockVars==""){
     block.vars <- NULL
     prnBlockVars <- "block.vars= NULL"
    }
else {
      block.varscross <- gsub(" ", ":",all.lstBlockVars)
      block.vars <- as.formula(paste("~", block.varscross), env = .GlobalEnv)
      prnBlockVars <- paste("block.vars=", block.varscross, sep =" ~ ")
    }

num <- as.integer(tksize(OR_lstSimScore))
if  (num<1){
     sim.score <- NULL
     prnSimScore <- "sim.score= NULL"
    }
else {
     if  (num>1){
         tkmessageBox(title="sim.score list", message = "The list must contain only one variable", icon = "error",
                      parent = ttClpsStrat)
         campobb <- FALSE
        }
     else{
         all.lstSimScore <- tclvalue(tkget(OR_lstSimScore, 0, 0))
         sim.score <- as.formula(paste("~", all.lstSimScore), env = .GlobalEnv)
         prnSimScore <- paste("sim.score=", all.lstSimScore, sep =" ~ ")
        }
    }

if  (tclvalue(OR_ObjectName)==""){
    tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error",
                 parent = ttClpsStrat)
    campobb <- FALSE
    }
else{
    if   ( !is.ok.name(tclvalue(OR_ObjectName), go.on=campobb, parent=ttClpsStrat) ){
         campobb <- FALSE
         }
    else {
         OR_ObjectName <- tclvalue(OR_ObjectName)
         }
    }

if  (campobb == TRUE){
    # change the cursor to the hourglass to tell work is in progress...
    tkconfigure(ttClpsStrat, cursor="watch")

    # DIRE A RAFFAELLA 16/07/10
    # Des_ClpsStrat_Value no more exists into TempEnv: we left inside its name only
    Des_ClpsStrat_Value <- get(Des_ClpsStrat_Name, envir=.GlobalEnv)
    # DIRE A RAFFAELLA 16/07/10

    valClpsStrat <- Lancia(collapse.strata(Des_ClpsStrat_Value,block.vars,sim.score), textWarnings, parent = ttClpsStrat)

    if  (!inherits(valClpsStrat,"try-error")) {
        prnDesign <- paste("design=", Des_ClpsStrat_Name)
        attr(valClpsStrat,"collapse.strata") <- paste("collapse.strata(", prnDesign, ", ", prnBlockVars,    ", ",
                                                prnSimScore, ")", sep="")

        # assign(OR_ObjectName, valClpsStrat, envir = .GlobalEnv)
        assign2GE(OR_ObjectName, valClpsStrat)
        Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

        cat("\n")
        cat(paste("# ",OR_ObjectName,"\n",sep=""))
        print(valClpsStrat)
        cat("\n")

        prnClpsStrat <- paste(" <- collapse.strata(", prnDesign, ", ", prnBlockVars, ", ",
                                                   prnSimScore, ")", sep="")

        commands <- paste(OR_ObjectName, prnClpsStrat, "\n", sep="")

        # Print on the Commands Window
        tkconfigure(textHistory, state="normal")
        tkinsert(textHistory, "end", commands)
        tkinsert(textHistory, "end", "\n")
        tkyview.moveto(textHistory, 1.0)
        tkconfigure(textHistory, state="disabled")
        # End

        # Flush commands to Rhistory file
        upd.Rhistory(commands, RG.stamp = TRUE)
        # End

        if (getTemp("there.are.warnings", default = FALSE)){
            tkmessageBox(title ="collapse.strata",
                         message = "Operation executed\nNote: Warnings have been generated!",
                         icon = "info", parent = ttClpsStrat)
            }
        else {
              tkmessageBox(title ="collapse.strata",message = "Operation executed", icon = "info", parent = ttClpsStrat)
             }
        tkgrab.release(ttClpsStrat)
        }
    # get back the standard arrow cursor
    tkconfigure(ttClpsStrat, cursor="arrow")
    # Collect garbage to free memory
    gc()
    }
}


# ----------------------------------------
# < END building collapse.strata window. <
# ----------------------------------------

# --------------------------------------
# > START building des.addvars window. >
# --------------------------------------

            fDesAddvars <- function(){
                    listEsvydesignObj <-  mk.class.list("analytic")
                    ttDesAddvars <- tktoplevel()
                    tcl("wm", "protocol", ttDesAddvars, "WM_DELETE_WINDOW", function() fOnCancel(ttDesAddvars))
                    frameGlobal<- tkframe(ttDesAddvars, borderwidth= 2)
                    tkwm.deiconify(ttDesAddvars)
                    tkgrab.set(ttDesAddvars)
                    tkfocus(ttDesAddvars)
                    tkwm.resizable(ttDesAddvars, 0, 0)
                    tkwm.title(ttDesAddvars,label_DesAddvars)

                    frameTop <- tkframe(frameGlobal, relief="groove", borderwidth=2, padx="0.8c", pady=c("0.2c","0.4c"))
                    frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, padx="0.8c", pady=c("0.2c","0.4c"))
                    frameOutput  <- tkframe(frameGlobal, borderwidth=0)
                    frameButtons <- tkframe(frameGlobal, borderwidth=0)

                    scrEsvydesignObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstEsvydesignObj,...))
                    lista <- tclVar()
                    tclObj(lista) <- listEsvydesignObj
                    lstEsvydesignObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="browse", yscrollcommand = function (...)tkset(scrEsvydesignObj,...), background = "white")

                    scrVariables<- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstVariables,...))
                    listaVariables <- tclVar()
                    lstVariables <- tklistbox(frameTop, height = 4, listvariable= listaVariables, selectmode="extended", yscrollcommand = function (...)tkset(scrVariables,...), background = "white")


                    count_DesAddvars<<- FALSE
                    tkbind(lstEsvydesignObj, "<ButtonRelease-1>", function() fElencoCampiDesAddvars(lista, lstEsvydesignObj,
                            lblVariables, listaVariables, lstVariables, scrVariables, Plus.but, Times.but, Colon.but, Minus.but, Power.but, LeftParen.but, RightParen.but, Collapse.but, Into.but, HelpInto.but, lblTagName, lblExpr, entry.TagName, lblEqual, textExpr, entry.ObjectName, lblChoiceDesignObj, ok.but))

                    tkbind(lstVariables, "<Double-Button-1>", function() {
                                        Variables_name <- as.character(tclObj(listaVariables))[as.integer(tkcurselection(lstVariables))+1]
                                        # tkinsert(textExpr, "end -1 chars", Variables_name)
                                        # It's better to insert where cursor is, and keep the focus  i.e.:
                                        tkfocus(textExpr)
                                        tkinsert(textExpr, "insert", Variables_name)
                                        })

                    lblDesignObj <- ttklabel(frameTop, text="Select a survey design object",font=fontTextLabel)
                    lblChoiceDesignObj <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarESvystatDesignObj)),foreground="red")
                    lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")

                    lblMandatory <- tk2label(frameGlobal,text="New Variable", font=fontTextTitle, foreground= "blue")

                    lblfExpr <- tkframe(frameCentral)
                    AllOperatorsFrame <- tkframe(lblfExpr)
                    OperatorsFrame <- tkframe(AllOperatorsFrame)
                    framelblExpr <- tkframe(lblfExpr)
                    frameExpr <- tkframe(lblfExpr)

                    lblTagName <- tklabel(framelblExpr, text="Name", font=fontTextLabel, state= "disabled")
                    lblExpr <- tklabel(framelblExpr, text="Expression", font=fontTextLabel, state= "disabled")

                    TagName <- tclVar("")
                    entry.TagName <-tkentry(frameExpr,width="25",text=TagName, state= "disabled", background="white")

                    lblEqual<- ttklabel(frameExpr,text="=", state="disabled")

                    scrtextExpr <- tkscrollbar(frameExpr, orient = "vertical", command=function(...) tkyview(textExpr, ...))

                    textExpr <- tktext(frameExpr, foreground="red", background= "white", height=4, width=25, yscrollcommand=function(...) tkset(scrtextExpr, ...),
                    wrap="word", state="disabled", font=fontTextExp)

                    Plus.but <- tk2button(OperatorsFrame, text="+", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, " + "))
                    Times.but <- tk2button(OperatorsFrame, text="*", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, "*"))
                    Colon.but <- tk2button(OperatorsFrame, text="/", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, "/"))
                    Minus.but <- tk2button(OperatorsFrame, text="-", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, " - "))
                    Power.but <- tk2button(OperatorsFrame, text="^", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, "^"))
                    LeftParen.but <- tk2button(OperatorsFrame, text="(", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, "("))
                    RightParen.but <- tk2button(OperatorsFrame, text=")", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, ")"))
                    Collapse.but <- tk2button(OperatorsFrame, text="+...+", width="5", state="disabled", command=function() fOnCollapseOperatorExpr(lstVariables, textExpr, parent = ttDesAddvars))

                    frameInto <- tkframe(AllOperatorsFrame)
                    Into.but <- tk2button(frameInto, text="%into%", width="8", state="disabled", command=function() fOnOperatorExpr(textExpr, " %into% "))
                    descInto <- descFun(label_Into)
                    tk2tip(Into.but, descInto)
                    HelpInto.but <- tk2button(frameInto, image=image_qm, state="disabled", tip=descFun(label_Into), command=function() fOnFunctionHelp(label_Into))

                    labellblfDesAddvarsObj <- ttklabel(frameOutput,text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfDesAddvarsObj<- ttklabelframe(frameOutput, labelwidget = labellblfDesAddvarsObj)

                    entry.ObjectName <-ttkentry(lblfDesAddvarsObj, width="20", text=as.character(tclvalue(AddVarsObjectName)), state= "disabled", font="TkDefaultFont")

                    ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_DesAddvars(TagName, textExpr, ttDesAddvars))

                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttDesAddvars))

                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_DesAddvars), command=function() fOnFunctionHelp(label_DesAddvars))

                    tkgrid.configure(tk2label(frameGlobal, text="Sample Data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))

                    tkgrid(frameTop)
                    tkgrid(lblDesignObj, lblVariables)
                    tkgrid(lstEsvydesignObj, scrEsvydesignObj)

                    tkgrid.configure(lblDesignObj, column=1, sticky ="e")
                    tkgrid.configure(lblVariables, column=4)
                    tkgrid.configure(lblChoiceDesignObj, row=2, column=1, sticky ="e")

                    tkgrid.configure(lstEsvydesignObj, column=1, row=3, sticky ="e", padx=c("1.5c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrEsvydesignObj, column=2, row=3, sticky ="nsw", padx=c(0,"1.9c"), pady=c(0,"0.2c"))

                    tkgrid.configure(lblMandatory, pady=c("0.2c",0))
                    tkgrid(frameCentral)
                    tkgrid.configure(frameCentral, padx="0.5c")

                    tkgrid(lblfExpr)
                    tkgrid.configure(lblfExpr, pady=c(0,"0.2c"))
                    tkgrid(OperatorsFrame, frameInto, sticky="ne")
                    tkgrid.configure(frameInto, padx=c("0.2c", 0))
                    tkgrid(AllOperatorsFrame)
                    tkgrid(framelblExpr)
                    tkgrid(frameExpr)

                    tkgrid(Plus.but, Times.but, Colon.but, Minus.but, Power.but, LeftParen.but, RightParen.but, Collapse.but, sticky ="e")

                    tkgrid(Into.but)
                    tkgrid(HelpInto.but, sticky ="ew")

                    tkgrid(lblTagName, lblExpr)
                    tkgrid(entry.TagName, lblEqual, textExpr, scrtextExpr)
                    tkgrid.configure(lblTagName, padx=c("1.5c","2c"), pady=c("0.4c",0))
                    tkgrid.configure(lblExpr, padx=c("3c","3c"), pady=c("0.4c",0))

                    tkgrid.configure(entry.TagName, padx="0.4c", pady=c(0,"1c"))
                    tkgrid.configure(lblEqual, padx=c(0, "0.5c"), pady=c(0,"1c"))
                    tkgrid.configure(scrtextExpr, sticky ="nsw", padx=c(0,"0.4c"), pady=c("0.3c",0))
                    tkgrid.configure(textExpr, pady=c("0.3c",0))
                    # Enable ctrl-c and ctrl-v on textExpr
                    ctrl.cv(textExpr)

                    tkgrid(lblfDesAddvarsObj)
                    tkgrid.configure(entry.ObjectName, padx=c("0.5c", "1.5c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")
                    tkgrid.configure(frameButtons, sticky="ne")

                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)
                    tkgrid.configure(ok.but, padx=c("6c",0))
                    tkgrid.configure(Cancel.but, padx=("0.5c"))
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))

                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttDesAddvars))) # To asses the size of the window...
                    # tkfocus(ttDesAddvars)
            }

            fElencoCampiDesAddvars <- function(EC_lista, EC_lstEsvydesignObj, EC_lblVariables, x, lstEC_EsvydesignObj,
                    scrEC, EC_Plus.but, EC_Times.but, EC_Colon.but, EC_Minus.but, EC_Power.but, EC_LeftParen.but, EC_RightParen.but, EC_Collapse.but, EC_Into.but, EC_HelpInto.but, EC_lblTagName, EC_lblExpr, EC_entry.TagName, EC_lblEqual, EC_textExpr, EC_entry.ObjectName, EC_lblChoiceDesignObj, EC_ok.but){


                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstEsvydesignObj)))

                    assignTemp("DF_DesAddvars_Name", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstEsvydesignObj))+1])

                    # DIRE A RAFFAELLA 16/07/10
                    # NO!!! Copying may become a serious concern when data get big
                    # (can tolerate it for names, because they are small: see above)
                    # assignTemp("DF_DesAddvars_Value", get(DF_DesAddvars_Name, envir=.GlobalEnv))
                    DF_DesAddvars_Value <- get(DF_DesAddvars_Name, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    VarESvystatDesignObj <<- tclVar(DF_DesAddvars_Name)
                    tkconfigure(EC_lblChoiceDesignObj, textvariable= VarESvystatDesignObj)

                    AddVarsObjectName <<- tclVar(DF_DesAddvars_Name)

                    if (Index_dataframe_DesAddvars !=EC_indicesel){
                            count_DesAddvars <<- FALSE
                    }
                    if (count_DesAddvars == FALSE){
                        count_DesAddvars <<- TRUE
                        Index_dataframe_DesAddvars <<- EC_indicesel

                        tkdelete(EC_textExpr, "1.0", "end")
                        tkdelete(EC_entry.TagName, 0, "end")

# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                        for  (n in names(DF_DesAddvars_Value$variables)){
#                                tclObj(x) <- c(names(DF_DesAddvars_Value$variables))
#                        }
                        tclObj(x) <- names(DF_DesAddvars_Value$variables)
                        tkgrid(lstEC_EsvydesignObj, scrEC)
                        tkgrid.configure(lstEC_EsvydesignObj, column=4, row=3, sticky ="e", pady=c(0,"0.2c"))
                        tkgrid.configure(scrEC, column=5, row=3, padx=c("0c","1.5c"), pady=c(0,"0.2c"), sticky ="nsw")

                        tkconfigure(EC_Plus.but, state = "normal")
                        tkconfigure(EC_Times.but, state = "normal")
                        tkconfigure(EC_Colon.but, state = "normal")
                        tkconfigure(EC_Minus.but, state = "normal")
                        tkconfigure(EC_Power.but, state = "normal")
                        tkconfigure(EC_LeftParen.but, state = "normal")
                        tkconfigure(EC_RightParen.but, state = "normal")
                        tkconfigure(EC_Collapse.but, state = "normal")
                        tkconfigure(EC_Into.but, state = "normal")
                        tkconfigure(EC_HelpInto.but, state = "normal")


                        tkconfigure(EC_lblTagName, state = "normal")
                        tkconfigure(EC_lblExpr, state = "normal")

                        tkconfigure(EC_entry.TagName, state = "normal")
                        tkconfigure(EC_lblEqual, state = "normal")
                        tkconfigure(EC_textExpr, state = "normal")

                        tkconfigure(EC_entry.ObjectName, textvariable= AddVarsObjectName, state = "normal")
                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_lblVariables, text="Variables", state = "normal")
                    }
            }

            fOnRun_DesAddvars <- function(OR_TagName, OR_textExpr, ttDesAddvars){

                    campobb <- TRUE
                    varname.char <- tclvalue(OR_TagName)
                    if (varname.char ==""){
                        tkmessageBox(title="New variable name", message = "Please give a name to the new variable", icon = "error", parent = ttDesAddvars)
                        campobb <- FALSE
                    }
                    else{
                        if (!is.ok.varname(varname.char, parent=ttDesAddvars)){
                           campobb <- FALSE
                        }
                    }

                    expr.char <- tclvalue(tkget(OR_textExpr, "1.0", "end -1 chars"))

                    if (expr.char ==""){
                        tkmessageBox(title="New variable expression",
                                     message = "Please specify the new variable expression", icon = "error", parent = ttDesAddvars)
                        campobb <- FALSE
                    }
                    else{
                        expr <- Lancia(parse(text=expr.char), textWarnings, parent = ttDesAddvars)
                        if (inherits(expr,"try-error")){
                              campobb <- FALSE
                        }
                    }

                    if (tclvalue(AddVarsObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = ttDesAddvars)
                        campobb <- FALSE
                    }
                    else{
                        if(!is.ok.name(tclvalue(AddVarsObjectName), go.on=campobb, parent = ttDesAddvars)){
                             campobb <- FALSE
                        }
                        else {
                             OR_DesAddvarsObjectName <- tclvalue(AddVarsObjectName)
                        }
                    }

                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(ttDesAddvars, cursor="watch")

                        command <- paste("des.addvars(", "design= ", DF_DesAddvars_Name, ", ",
                                paste(varname.char, "= ", expr.char, sep=""), ")", sep="")

                        new.des <- Lancia(eval(parse(text=command)), textWarnings, parent = ttDesAddvars)

                         if (!inherits(new.des, "try-error")){
                            # assign(OR_DesAddvarsObjectName, new.des, envir = .GlobalEnv)
                            assign2GE(OR_DesAddvarsObjectName, new.des)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            cat(paste("# ", OR_DesAddvarsObjectName, "\n", sep=""))
                            print(new.des)
                            cat("\n")

                            commands <- paste(OR_DesAddvarsObjectName, " <- des.addvars(design= ", DF_DesAddvars_Name, ", ",
                            paste(varname.char, "= ", expr.char, sep=""), ")", "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                  tkmessageBox(title ="des.addvars",
                                               message = "Operation executed\nNote: Warnings have been generated!",
                                               icon = "info", parent = ttDesAddvars)
                                 }
                            else {
                                  tkmessageBox(title ="des.addvars",message = "Operation executed", icon = "info", parent = ttDesAddvars)
                                 }
                            tkgrab.release(ttDesAddvars)
                        }
                    # get back the standard arrow cursor
                    tkconfigure(ttDesAddvars, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# ------------------------------------
# < END building des.addvars window. <
# ------------------------------------


# ------------------------------------
# > START building des.merge window. >
# ------------------------------------

#fDesMerge <- function(){
#                     tkmessageBox(title ="des.merge", message = "Sorry, this item is still under construction!\nSee ?des.merge to use the function from the command line.",
#                     icon = "info")
#                    }

            fDesMerge <- function(){
                    listDesign <- mk.class.list("analytic")
                    listDataObj <- listDataSets()
                    ttDesMerge <- tktoplevel()
                    tcl("wm", "protocol", ttDesMerge, "WM_DELETE_WINDOW", function() fOnCancel(ttDesMerge))
                    frameGlobal<- tkframe(ttDesMerge, borderwidth= 2)
                    tkwm.deiconify(ttDesMerge)
                    tkgrab.set(ttDesMerge)
                    tkfocus(ttDesMerge)
                    tkwm.title(ttDesMerge,label_DesMerge)
                    tkwm.resizable(ttDesMerge, 0, 0)

                    frameTop <- tkframe(frameGlobal, relief="groove", borderwidth=2, padx="0.8c",
                                        pady=c("0.2c","0.4c"))
                    frameDown <- tkframe(frameGlobal, relief="groove", borderwidth=2,
                                        pady=c("0.2c","0.4c"))
                    frameOutput <- tkframe(frameGlobal, borderwidth=0)
                    frameButtons <- tkframe(frameGlobal, borderwidth=0)

                    lblDesign <- ttklabel(frameTop, text="Select a survey design object",
                                                font=fontTextLabel)
                    lblData <- ttklabel(frameTop, text="Select new survey data",
                                        font=fontTextLabel)

                    lblmergeDesign <- ttklabel(frameTop,
                                                    textvariable=as.character(tclvalue(merge_VarDesign)),
                                                    foreground="red")
                    lblmergeData <- ttklabel(frameTop,
                                            textvariable=as.character(tclvalue(merge_VarData)),
                                            foreground="red")

                    lblMandatory <- tk2label(frameGlobal,text="Merge Variable",
                                            font=fontTextTitle, foreground= "blue")

                    scrDesign <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstDesign,...))
                    listaDes <- tclVar()
                    tclObj(listaDes) <- listDesign
                    lstDesign <- tklistbox(frameTop,height = 4, listvariable= listaDes, selectmode="single", yscrollcommand = function (...)tkset(scrDesign,...), background = "white")

                    scrDataObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstDataObj,...))
                    lista <- tclVar()
                    tclObj(lista) <- listDataObj
                    lstDataObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="single", yscrollcommand = function (...)tkset(scrDataObj,...), background = "white")


                    tkbind(lstDesign, "<ButtonRelease-1>", function() fmerge_VarDesign(lstDesign, listaDes, merge_VarDesign, lblmergeDesign, ok.but, entry.ObjectName, listaKey, cbKey, KeyDef))
                    count_mergeDes <<- FALSE

                    tkbind(lstDataObj, "<ButtonRelease-1>", function() fmerge_VarData(lista, lstDataObj, lblmergeData, merge_VarData, ok.but, entry.ObjectName, listaKey, cbKey, KeyDef))
                    count_mergeData <<- FALSE

                    labellblfKey <- ttklabel(frameDown,text="  key  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("des.merge", args = "key")
                    tk2tip(labellblfKey,descfunz)
                    lblfKey<- ttklabelframe(frameDown, labelwidget=labellblfKey)

                    listaKey <- tclVar()
                    Key <- c("NULL")
                    tclObj(listaKey) <- Key
                    cbKey <- tk2combobox(lblfKey, values = Key, state="readonly")
                    KeyDef <- tclVar("NULL")

                    labellblfDesMergeObj <- ttklabel(frameOutput,text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfDesMergeObj<- ttklabelframe(frameOutput, labelwidget = labellblfDesMergeObj)

                    entry.ObjectName <-ttkentry(lblfDesMergeObj,width="20",text=as.character(tclvalue(DesMergeObjectName)), state= "disabled", font="TkDefaultFont")

                    ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_DesMerge(cbKey, ttDesMerge))

                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttDesMerge))

                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_DesMerge), command=function() fOnFunctionHelp(label_DesMerge))


                    tkgrid(tk2label(frameGlobal, text="Sample Data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))

                    tkgrid(lblDesign, lblData)
                    tkgrid(lblmergeDesign, lblmergeData)

                    tkgrid(lstDesign, scrDesign, lstDataObj, scrDataObj)

                    tkgrid.configure(lblDesign, column=1, sticky ="e")
                    tkgrid.configure(lblData, column=3, sticky ="e")

                    tkgrid.configure(lblmergeDesign, column=1, sticky ="e")
                    tkgrid.configure(lblmergeData, column= 3, sticky ="e")

                    tkgrid.configure(lstDesign, column=1, row=3, sticky ="e", padx=c("1.7c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrDesign, column=2, row=3, sticky ="nsw", padx=c(0,"1.9c"), pady=c(0,"0.2c"))

                    tkgrid.configure(lstDataObj, column=3, row=3, sticky ="e", padx=c("1c",0), pady=c(0,"0.2c"))
                    tkgrid.configure(scrDataObj, column=4, row=3, sticky ="nsw", padx=c(0,"1.5c"), pady=c(0,"0.2c"))

#--------------------
                    tkconfigure(cbKey, textvariable = KeyDef)
                    tkgrid(cbKey)

                    tkgrid.configure(cbKey,padx="0.2c", pady=c(0,"0.2c"))
                    tkgrid.configure(lblfKey,padx="0.3c", pady=c(0,"0.2c"))
#--------------------

                    tkgrid(frameTop, padx="0.2c")
                    tkgrid(lblMandatory, pady=c("0.2c",0))

                    tkgrid(frameDown)
                    tkgrid.configure(frameDown, padx="0.5c")

                    tkgrid(lblfDesMergeObj)

                    tkgrid.configure(entry.ObjectName, padx="0.5c",pady=c(0,"0.3c"))
                    tkgrid.configure(frameOutput, padx=c("0.2c",0), pady=c("0.2c","0.2c"), sticky="w")

                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)
                    tkgrid.configure(Cancel.but, padx="0.2c")
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.2c"))
                    tkgrid.configure(frameButtons, sticky="ne")

                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttDesMerge))) # To asses the size of the window...
                    # tkfocus(ttDesMerge)
            }

            Merge_intersect <- function(Design_vars, Data_vars){
             if (is.null(Design_vars)) out <- Data_vars
             if (is.null(Data_vars))   out <- Design_vars
             if (!is.null(Design_vars) && !is.null(Data_vars)) out <- intersect(Design_vars, Data_vars)
             if (is.null(out)) out <- "NULL"
             out
            }

            fmerge_VarDesign <- function(EC_lstDesign, xDes, merge_VarDesign, EC_lblmergeDesign,
                    EC_ok.but, EC_entry.ObjectName,
                    EC_listaKey, EC_cbKey, EC_KeyDef){

                    EC_indicescelta <- as.character(tclvalue(tkcurselection(EC_lstDesign)))

                    assignTemp("choiceDes", as.character(tclObj(xDes))[as.integer(tkcurselection(EC_lstDesign))+1])

                    # Move it below to obtain a small delay...
                    # sceltaDes <- get(choiceDes, envir=.GlobalEnv)

                    merge_VarDesign <<- tclVar(choiceDes)
                    tkconfigure(EC_lblmergeDesign, textvariable= merge_VarDesign)

                    sceltaDes <- get(choiceDes, envir=.GlobalEnv)

                    merge_Design_vars <<- names(sceltaDes$variables)
                    DesMergeObjectName <<- tclVar(choiceDes)

                    if (Index_choice_DesMerge!=EC_indicescelta){
                        count_mergeDes <<- FALSE
                    }
                    if (count_mergeDes == FALSE){
                        count_mergeDes <<- TRUE
                        Index_choice_DesMerge <<- EC_indicescelta
                    }

                    tkconfigure(EC_entry.ObjectName, textvariable=DesMergeObjectName, state="normal")

                    if (count_mergeDes==TRUE && count_mergeData==TRUE){
                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")

                        # Fill list with common variables
                        merge_Intersect_vars <- Merge_intersect(merge_Design_vars, merge_Data_vars)
                        EC_Key <- merge_Intersect_vars
                        tclObj(EC_listaKey) <- as.character(EC_Key)
                        tkconfigure(EC_cbKey, values = EC_Key)
                        # Code below should restore initial value NULL when changing
                        # design object: works well
                        tclvalue(EC_KeyDef) <- "NULL"
                    }
                    else{
                        tkconfigure(EC_ok.but, state = "disabled")
                        tkconfigure(EC_entry.ObjectName, state = "disabled")
                    }
            }


            fmerge_VarData <- function(EC_lista, EC_lstDataObj,
                    EC_lblmergeData, merge_VarData,
                    EC_ok.but, EC_entry.ObjectName,
                    EC_listaKey, EC_cbKey, EC_KeyDef){

                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstDataObj)))

                    assignTemp("choiceData", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstDataObj))+1])

                    # Move it below to obtain a small delay...
                    # sceltaData <- get(choiceData, envir=.GlobalEnv)

                    merge_VarData <<- tclVar(choiceData)
                    tkconfigure(EC_lblmergeData, textvariable= merge_VarData)

                    sceltaData <- get(choiceData, envir=.GlobalEnv)
                    merge_Data_vars <<- names(sceltaData)

                    if (Index_dataframe_DesMerge !=EC_indicesel){
                        count_mergeData <<- FALSE
                    }
                    if (count_mergeData == FALSE){
                        count_mergeData <<- TRUE
                        Index_dataframe_DesMerge <<- EC_indicesel
                    }

                    if (count_mergeDes==TRUE && count_mergeData==TRUE){
                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")

                        # Fill list with common variables
                        merge_Intersect_vars <- Merge_intersect(merge_Design_vars, merge_Data_vars)
                        EC_Key <- merge_Intersect_vars
                        tclObj(EC_listaKey) <- as.character(EC_Key)
                        tkconfigure(EC_cbKey, values = EC_Key)
                        # Code below should restore initial value NULL when changing
                        # design object: works well
                        tclvalue(EC_KeyDef) <- "NULL"
                    }
                    else{
                        tkconfigure(EC_ok.but, state = "disabled")
                        tkconfigure(EC_entry.ObjectName, state = "disabled")
                    }
            }


            fOnRun_DesMerge <- function(OR_cbKey, ttDesMerge){
                    campobb <- TRUE
                    sceltaDes  <- get(choiceDes, envir=.GlobalEnv)
                    sceltaData <- get(choiceData, envir=.GlobalEnv)

                    keychar <- tclvalue(tclVar(tkget(OR_cbKey)))
                    if (keychar != "NULL"){
                        key <- as.formula(paste("~", keychar), env = .GlobalEnv)
                        prnKey <- paste("key=", keychar, sep =" ~ ")
                    }
                    else{
                        tkmessageBox(title="Merge variable ", message="Please specify a common key variable to be used for merging",icon="error", parent = ttDesMerge)
                        campobb <- FALSE
                    }

                    if (tclvalue(DesMergeObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = ttDesMerge)
                        campobb <- FALSE
                    }
                    else{
                        if   ( !is.ok.name(tclvalue(DesMergeObjectName), go.on=campobb,
                                           parent = ttDesMerge) ){
                             campobb <- FALSE
                             }
                        else {
                             OR_DesMergeObjectName <- tclvalue(DesMergeObjectName)
                             }
                        }

                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(ttDesMerge, cursor="watch")

                        outDesMerge <- Lancia(des.merge(sceltaDes, sceltaData, key), textWarnings, parent = ttDesMerge)

                        if (!inherits(outDesMerge, "try-error")) {
                            prnDesign <- paste("design=",choiceDes)
                            prnData <- paste("data=",choiceData)

                            # assign(OR_DesMergeObjectName, outDesMerge, envir = .GlobalEnv)
                            assign2GE(OR_DesMergeObjectName, outDesMerge)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            cat(paste("# ", OR_DesMergeObjectName, "\n", sep=""))
                            print(outDesMerge)
                            cat("\n")

                            prnDesMerge<- paste(" <- des.merge(", prnDesign, ", ", prnData, ", ",
                                                prnKey, ")", sep="")
                            commands <- paste(OR_DesMergeObjectName, prnDesMerge, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                  tkmessageBox(title ="des.merge",
                                               message = "Operation executed\nNote: Warnings have been generated!",
                                               icon = "info", parent = ttDesMerge)
                                 }
                            else {
                                  tkmessageBox(title ="des.merge",message = "Operation executed", icon = "info", parent = ttDesMerge)
                                 }
                            tkgrab.release(ttDesMerge)
                            }
                    # get back the standard arrow cursor
                    tkconfigure(ttDesMerge, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# ----------------------------------
# < END building des.merge window. <
# ----------------------------------


# --------------------------------------
# > START building e.svystatTM window. >
# --------------------------------------

fSvystatTM <- function(){
                    listEsvydesignObj <-  mk.class.list("analytic")
                    ttSvystatTM <- tktoplevel()
                    tcl("wm", "protocol", ttSvystatTM, "WM_DELETE_WINDOW", function() fOnCancel(ttSvystatTM))
                    frameGlobal<- tkframe(ttSvystatTM, borderwidth= 2)
                    tkwm.deiconify(ttSvystatTM)
                    tkgrab.set(ttSvystatTM)
                    tkfocus(ttSvystatTM)
                    tkwm.title(ttSvystatTM,label_SvystatTM)
                    tkwm.resizable(ttSvystatTM, 0, 0)


                    frameTop     <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameDown    <- tkframe(frameGlobal, relief="groove", borderwidth=2, padx="0.1c", pady=c("0.2c","0.4c"))
                    frameOutput  <- tkframe(frameGlobal, borderwidth=0)
                    frameButtons <- tkframe(frameGlobal, borderwidth=0)


                    scrEsvydesignObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstEsvydesignObj,...))
                    lista <- tclVar()
                    tclObj(lista) <- listEsvydesignObj
                    lstEsvydesignObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="browse", yscrollcommand = function (...)tkset(scrEsvydesignObj,...), background = "white")

                    scrVariables<- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstVariables,...))
                    listaVariables <- tclVar()
                    lstVariables <- tklistbox(frameTop,height = 4, listvariable= listaVariables, selectmode="extended", yscrollcommand = function (...)tkset(scrVariables,...), background = "white")

                    lblfEsvydesignObj<- tk2labelframe(frameTop)

                    count_SvystatTM <<- FALSE
                    tkbind(lstEsvydesignObj, "<ButtonRelease-1>", function() fElencoCampiSvystatTM(lista, lstEsvydesignObj,
                            lblVariables, listaVariables, lstVariables, scrVariables, y.but, by.but, y.ri.but, by.ri.but,
                            lblESvystatDesignObj, ok.but, lstY, lstBy, entry.ObjectName))

                    lblEsvydesignObj <- ttklabel(frameTop, text="Select a survey design object",font=fontTextLabel)
                    lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")


                    lblESvystatDesignObj <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarESvystatDesignObj)),foreground="red")


                    labellblfY <- tk2label(frameCentral,text="  y  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatTM", args = "y")
                    tk2tip(labellblfY,descfunz)

                    lblfY<- tk2labelframe(frameCentral, labelwidget= labellblfY)
                    frameY <- tkframe(lblfY, borderwidth=0)
                    frameY.but <- tkframe(lblfY, borderwidth=0)
                    scrY <- tkscrollbar(frameY, repeatinterval= 5, command = function(...) tkyview(lstY,...))
                    listaY <- tclVar()
                    lstY <- tklistbox(frameY, height=4, listvariable=listaY, selectmode="extended", yscrollcommand = function (...)tkset(scrY,...), background = "white")


                    labellblfBy <- tk2label(frameDown,text="  by  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatTM", args = "by")
                    tk2tip(labellblfBy,descfunz)
                    lblfBy<- tk2labelframe(frameDown, labelwidget=labellblfBy)
                    frameBy <- tkframe(lblfBy, borderwidth=0)
                    frameBy.but <- tkframe(lblfBy, borderwidth=0)
                    scrBy <- tkscrollbar(frameBy, repeatinterval= 5, command = function(...) tkyview(lstBy,...))

                    listaBy <- tclVar()
                    lstBy <- tklistbox(frameBy, height=4, listvariable=listaBy, selectmode="extended", yscrollcommand = function (...)tkset(scrBy,...), background = "white")

                    y.but <- tk2button(frameY.but, image=image_sx, state= "disabled", command = function()fTransfer(lstY, lstVariables, listaVariables))
                    y.ri.but <- tk2button(frameY.but,image=image_dx, state= "disabled", command = function()fDetransfer(lstY, lstVariables, listaY))

                    by.but <- tk2button(frameBy.but,image=image_sx, state= "disabled", command = function()fTransfer(lstBy, lstVariables, listaVariables))
                    by.ri.but <- tk2button(frameBy.but, image=image_dx, state= "disabled", command = function()fDetransfer(lstBy, lstVariables, listaBy))

                    lblMandatory <- tk2label(frameGlobal,text="Mandatory Fields", font=fontTextTitle, foreground= "blue")
                    lblOptional <-tk2label(frameGlobal,text="Optional Fields", font=fontTextTitle, foreground= "blue")


                    labellblfEstimator <- ttklabel(frameDown,text="  estimator  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatTM", args = "estimator")
                    tk2tip(labellblfEstimator,descfunz)
                    lblfEstimator<- ttklabelframe(frameDown, labelwidget=labellblfEstimator)

                    rbEstimatorT <- ttkradiobutton(lblfEstimator)
                    rbEstimatorM <- ttkradiobutton(lblfEstimator)
                    rbValueEstimator <- tclVar("Total")

                    tkconfigure(rbEstimatorT,variable=rbValueEstimator,value="Total")
                    tkconfigure(rbEstimatorM,variable=rbValueEstimator,value="Mean")

                    labelEstimatorT <- ttklabel(lblfEstimator,text="Total ")
                    labelEstimatorM <- ttklabel(lblfEstimator,text="Mean ")

                    labellblfDeff <- ttklabel(frameDown,text="  deff  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatTM", args = "deff")
                    tk2tip(labellblfDeff,descfunz)
                    lblfDeff<- ttklabelframe(frameDown, labelwidget=labellblfDeff)

                    rbDeffF <- ttkradiobutton(lblfDeff)
                    rbDeffT <- ttkradiobutton(lblfDeff)
                    rbDeffR <- ttkradiobutton(lblfDeff)

                    rbValueDeff <- tclVar("FALSE")

                    tkconfigure(rbDeffF,variable=rbValueDeff,value="FALSE")
                    tkconfigure(rbDeffT,variable=rbValueDeff,value="TRUE")
                    tkconfigure(rbDeffR,variable=rbValueDeff,value="replace")

                    labelDeffF <- ttklabel(lblfDeff,text="False ")
                    labelDeffT <- ttklabel(lblfDeff,text="True ")
                    labelDeffR <- ttklabel(lblfDeff,text="Replace")


                    labellblfVartype <- ttklabel(frameDown,text="  vartype  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatTM", args = "vartype")
                    tk2tip(labellblfVartype,descfunz)
                    lblfVartype<- ttklabelframe(frameDown, labelwidget=labellblfVartype)
                    vartype_name <- c("se", "cv", "cvpct", "var")
                    vartype_value <- c("1", "0", "0", "0")
                    checkbuttonWidget <- list()
                    checkbuttonVar <- list()

                    lvar <-length(vartype_name)

                    for (i in 1:lvar){
                        checkbuttonWidget[[i]] <- ttkcheckbutton(lblfVartype)
                        checkbuttonVar[[i]] <- tclVar(vartype_value[i])
                    }


                    labellblfConf.int <- ttklabel(frameDown,text="  conf.int  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatTM", args = "conf.int")
                    tk2tip(labellblfConf.int,descfunz)
                    lblfConf.int<- ttklabelframe(frameDown, labelwidget=labellblfConf.int)

                    rbConf.intF <- ttkradiobutton(lblfConf.int)
                    rbConf.intT <- ttkradiobutton(lblfConf.int)
                    rbValueConf.int <- tclVar("FALSE")

                    tkconfigure(rbConf.intF,variable=rbValueConf.int,value="FALSE")
                    tkconfigure(rbConf.intT,variable=rbValueConf.int,value="TRUE")

                    labelConf.intF <- ttklabel(lblfConf.int,text="False ")
                    labelConf.intT <- ttklabel(lblfConf.int,text="True ")


                    labellblfConf.lev <- ttklabel(frameDown,text="  conf.lev  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatTM", args = "conf.lev")
                    tk2tip(labellblfConf.lev,descfunz)
                    lblfConf.lev<- ttklabelframe(frameDown, labelwidget=labellblfConf.lev)

                    tkbind(rbConf.intT, "<ButtonPress>", function() {tkconfigure(s, state = "normal")})
                    tkbind(rbConf.intF, "<ButtonPress>", function() {tkconfigure(s, state = "disabled")})

                    s <- tk2spinbox(lblfConf.lev, from=0.00, to=1.00, increment=0.01, state="disabled", width="4", background= "white")

                    tkconfigure(s, textvariable=tclVar(0.95))

                    labellblfNa.rm <- ttklabel(frameDown,text="  na.rm  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatTM", args = "na.rm")
                    tk2tip(labellblfNa.rm,descfunz)
                    lblfNa.rm<- ttklabelframe(frameDown, labelwidget=labellblfNa.rm)

                    rbNa.rmF <- ttkradiobutton(lblfNa.rm)
                    rbNa.rmT <- ttkradiobutton(lblfNa.rm)
                    rbValueNa.rm <- tclVar("FALSE")

                    tkconfigure(rbNa.rmF,variable=rbValueNa.rm,value="FALSE")
                    tkconfigure(rbNa.rmT,variable=rbValueNa.rm,value="TRUE")

                    labelNa.rmF <- ttklabel(lblfNa.rm,text="False ")
                    labelNa.rmT <- ttklabel(lblfNa.rm,text="True ")


                    labellblfSvystatTMObj <- ttklabel(frameOutput,text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfSvystatTMObj<- ttklabelframe(frameOutput, labelwidget = labellblfSvystatTMObj)

                    ObjectName <- tclVar("")
                    entry.ObjectName <-ttkentry(lblfSvystatTMObj,width="20",text=ObjectName, state= "disabled", font="TkDefaultFont")

                    ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_SvystatTM(lstY,lstBy,rbValueEstimator, rbValueDeff, checkbuttonVar,
                              lvar, vartype_name, rbValueConf.int, rbValueNa.rm, s, ObjectName, ttSvystatTM))


                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttSvystatTM))

                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_SvystatTM), command=function() fOnFunctionHelp(label_SvystatTM))


                    tkgrid(tk2label (frameGlobal, text="Sample Data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))
                    tkgrid(lblfEsvydesignObj)
                    tkgrid(lblEsvydesignObj, lblVariables)
                    tkgrid(lstEsvydesignObj, scrEsvydesignObj )

                    tkgrid.configure(lblEsvydesignObj, column=1, sticky ="e")
                    tkgrid.configure(lblVariables, column=4)
                    tkgrid.configure(lblESvystatDesignObj, row=2, column=1, sticky ="e")

                    tkgrid.configure(lstEsvydesignObj , column=1, row=3, sticky ="e", padx=c("1.7c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrEsvydesignObj , column=2, row=3, sticky ="nsw", padx=c(0,"1.9c"), pady=c(0,"0.2c"))

                    tkgrid.configure(lblfY,padx="0.5c", pady=c(0,"0.2c"))
                    tkgrid(frameY.but, frameY)
                    tkgrid.configure(frameY.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameY, padx= c(0,"0.1c"), pady=c(0,"0.3c"))

                    tkgrid(y.but)
                    tkgrid(y.ri.but)
                    tkgrid(lstY, scrY)

                    tkgrid.configure(y.but, pady=c(0,"0.2c"))
                    tkgrid.configure(y.ri.but, pady=c("0.2c",0))

                    tkgrid.configure(scrY, sticky ="nsw")

                    tkgrid(lblfBy,lblfEstimator,lblfDeff,lblfVartype, lblfConf.int, lblfConf.lev, lblfNa.rm)

                    tkgrid.configure(lblfBy,padx=c("0.4c","0.3c"), pady=c(0,"0.2c"))
                    tkgrid(frameBy.but, frameBy)
                    tkgrid.configure(frameBy.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameBy, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
                    tkgrid(by.but)
                    tkgrid(by.ri.but)
                    tkgrid(lstBy, scrBy)

                    tkgrid.configure(by.but, pady=c(0,"0.2c"))
                    tkgrid.configure(by.ri.but, pady=c("0.2c",0))

                    tkgrid.configure(lstBy, sticky ="e")
                    tkgrid.configure(scrBy, sticky ="nsw")

                    tkgrid(labelEstimatorT,rbEstimatorT)
                    tkgrid(labelEstimatorM,rbEstimatorM)
                    tkgrid.configure(labelEstimatorT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelEstimatorM, sticky="w", padx=c("0.3c",0))

                    tkgrid(labelDeffF,rbDeffF)
                    tkgrid(labelDeffT,rbDeffT)
                    tkgrid(labelDeffR,rbDeffR)

                    tkgrid.configure(labelDeffF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelDeffT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelDeffR, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfDeff,padx="0.3c")

                    for (i in 1:length(vartype_name)){
                        tkgrid(tklabel(lblfVartype,text=vartype_name[i]), checkbuttonWidget[[i]], sticky="w", padx=c("0.3c",0))
                        tkgrid(checkbuttonWidget[[i]] )
                        tkconfigure(checkbuttonWidget[[i]] ,variable=checkbuttonVar[[i]])
                    }

                    tkgrid.configure(lblfVartype,padx=c(0,10))

                    tkgrid(labelConf.intF,rbConf.intF)
                    tkgrid(labelConf.intT,rbConf.intT)

                    tkgrid.configure(labelConf.intF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelConf.intT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfConf.int,padx=c(0,"0.3c"))

                    tkgrid.configure(s, padx=c("0.7c",0), pady=c(0,"0.2c"))

                    tkgrid(labelNa.rmF,rbNa.rmF)
                    tkgrid(labelNa.rmT,rbNa.rmT)

                    tkgrid.configure(labelNa.rmF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelNa.rmT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfNa.rm,padx=c("0.3c","0.4c"))

                    tkgrid(frameTop)
                    tkgrid(lblMandatory, pady=c("0.2c",0))

                    tkgrid(frameCentral)
                    tkgrid(lblOptional, pady=c("0.2c",0))

                    tkgrid(frameDown)
                    tkgrid.configure(frameDown, padx="0.5c")

                    tkgrid(lblfSvystatTMObj)
                    tkgrid.configure(entry.ObjectName, padx=c("0.5c","10.5c"),pady=c(0,"0.3c"))

                    tkgrid(frameOutput)
                    tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")
                    tkgrid(frameButtons)
                    tkgrid.configure(frameButtons, sticky="ne")
                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)

                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)
                    tkgrid.configure(Cancel.but, padx=("0.5c"))
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))
                    tkgrid(frameButtons)

                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttSvystatTM))) # To asses the size of the window...
                    # tkfocus(ttSvystatTM)
            }


                    fElencoCampiSvystatTM <- function(EC_lista, EC_lstEsvydesignObj, EC_lblVariables, x, lstEC_EsvydesignObj, scrEC, EC_y.but, EC_by.but, EC_y.ri.but, EC_by.ri.but,
                    EC_lblESvystatDesignObj, EC_ok.but, EC_lstY, EC_lstBy, EC_entry.ObjectName){
                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstEsvydesignObj)))


                    assignTemp("Scelta_EsvydesignObj", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstEsvydesignObj))+1])

                    # DIRE A RAFFAELLA 16/07/10
                    # NO!!! Copying may become a serious concern when data get big
                    # (can tolerate it for names, because they are small: see above)
                    # assignTemp("EsvydesignObj", get(Scelta_EsvydesignObj, envir=.GlobalEnv))
                    EsvydesignObj <- get(Scelta_EsvydesignObj, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    VarESvystatDesignObj <<- tclVar(Scelta_EsvydesignObj)
                    tkconfigure(EC_lblESvystatDesignObj, textvariable= VarESvystatDesignObj)

                    if (Index_dataframe_old_Svyby !=EC_indicesel){
                            count_SvystatTM <<- FALSE
                    }
                    if (count_SvystatTM == FALSE){
                        count_SvystatTM <<- TRUE
                        Index_dataframe_old_Svyby <<- EC_indicesel

                        tkdelete(EC_lstY, 0, "end")
                        tkdelete(EC_lstBy, 0, "end")
                        tkdelete(EC_entry.ObjectName, 0, "end")

# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                        for  (n in names(EsvydesignObj$variables)){
#                                tclObj(x) <- c(names(EsvydesignObj$variables))
#                        }
                        tclObj(x) <- names(EsvydesignObj$variables)

                        tkgrid(lstEC_EsvydesignObj, scrEC)

                        tkgrid.configure(lstEC_EsvydesignObj, column=4, row=3, pady=c(0,"0.2c"), sticky ="e")
                        tkgrid.configure(scrEC, column=5, row=3, padx=c("0c","1.7c"), pady=c(0,"0.2c"), sticky ="nsw")
                        tkconfigure(EC_y.but, state = "normal")
                        tkconfigure(EC_by.but, state = "normal")

                        tkconfigure(EC_y.ri.but, state = "normal")
                        tkconfigure(EC_by.ri.but, state = "normal")

                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")
                        tkconfigure(EC_lblVariables, text="Variables", state = "normal")
                    }
            }

            fOnRun_SvystatTM <- function(OR_lstY,OR_lstBy,OR_rbValueEstimator,OR_rbValueDeff,
                    OR_checkbuttonVar, OR_lvar, OR_vartype_name, OR_rbValueConf.int,
                    OR_rbValueNa.rm, OR_s, OR_SvystatTMObjectName, ttSvystatTM){

                    campobb <- TRUE

                    all.lstY <- tclvalue(tkget(OR_lstY, 0, "end"))

                    if  (all.lstY =="") {
                        tkmessageBox(title="List y empty", message = "The list is empty. Transfer at least an element", icon = "error", parent = ttSvystatTM)
                        campobb <- FALSE
                    }
                    else {
                        ysum <- gsub(" ", "+",all.lstY)

                        y <- as.formula(paste("~", ysum), env = .GlobalEnv)
                        prnY <- paste("y=", ysum, sep =" ~ ")
                    }

                    all.lstBy <- tclvalue(tkget(OR_lstBy, 0, "end"))

                    if  (all.lstBy =="") {
                        by <- NULL
                        prnBy <- "by= NULL"
                        }
                    else {
                        bysum <- gsub(" ", ":",all.lstBy)

                        by <- as.formula(paste("~", bysum), env = .GlobalEnv)
                        prnBy <- paste("by=", bysum, sep =" ~ ")
                    }

                    estimator <- tclvalue(OR_rbValueEstimator)

                    if (estimator=="Total") {
                        prnEstimator <- 'estimator= "Total"'

                    }
                    else{
                        prnEstimator <- 'estimator= "Mean"'
                    }

                    if ( (deff.tmp <-  tclvalue(OR_rbValueDeff))=="replace" ){
                        deff <- deff.tmp
                        prnDeff <- 'deff= "replace"'
                    }
                    else {
                         deff <- as.logical(deff.tmp)
                         if (!deff) {
                             prnDeff <- "deff= FALSE"
                         }
                         else {
                             prnDeff <- "deff= TRUE"
                         }
                    }

                    choices <- c()
                    j<- 1
                    for (i in 1:OR_lvar){
                          if(tclvalue(OR_checkbuttonVar[[i]]) == "1") {
                          choices[j] <- OR_vartype_name[i]
                          j <- j + 1
                        }
                    }

                    if ((j-1) == 0) {
                        prnVartype <- 'vartype= "se"'
                        vartype <- "se"
                    }
                    else {
                        vartype <- choices
                        if ((j-1) == 1) {
                           prnVartype <- paste('vartype= "', choices[1], '"', sep="")
                        }
                        else{
                            prnVartype <- "vartype= c("
                            for(i in 1:(j-1)){
                                prnVartype <- paste(prnVartype,'"', choices[i],'"', sep="")
                                if (i<(j-1)) {prnVartype <- paste(prnVartype,', ',sep="")}
                            }
                            prnVartype <- paste (prnVartype,  ')', sep = "")
                        }
                    }

                    conf.int<- as.logical(tclvalue(OR_rbValueConf.int))
                    if (!conf.int) {
                        prnConf.int <- "conf.int= FALSE"
                        conf.lev <- 0.95
                        prnConf.lev <- "conf.lev= 0.95"
                    }
                    else{
                        prnConf.int <- "conf.int= TRUE"
                        conf.lev <- tclvalue(tkget(OR_s))
                        if (conf.lev ==""){
                            tkmessageBox(title="conf.lev", message="The field is empty!",icon="error", parent = ttSvystatTM)
                            campobb <- FALSE
                        }
                        else{
                            conf.lev <- as.numeric(tkget(OR_s))
                            if (is.na(conf.lev)){
                                tkmessageBox(title="conf.lev", message="Please insert a numeric value",icon="error", parent = ttSvystatTM)
                                campobb <- FALSE
                            }
                            else{
                                prnConf.lev <- paste("conf.lev= ", conf.lev, sep="")
                            }
                        }
                    }

                    na.rm <- as.logical(tclvalue(OR_rbValueNa.rm))
                    if (!na.rm) {
                        prnNa.rm <- "na.rm= FALSE"
                    }
                    else{
                        prnNa.rm <- "na.rm= TRUE"
                    }

                    if (tclvalue(OR_SvystatTMObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = ttSvystatTM)
                        campobb <- FALSE
                    }
                    else{
                        if   ( !is.ok.name(tclvalue(OR_SvystatTMObjectName), go.on=campobb, parent = ttSvystatTM) ){
                             campobb <- FALSE
                             }
                        else {
                             OR_SvystatTMObjectName <- tclvalue(OR_SvystatTMObjectName)
                             }
                        }
                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(ttSvystatTM, cursor="watch")

                        # DIRE A RAFFAELLA 16/07/10
                        # EsvydesignObj no more exists into TempEnv: we left inside its name only
                        EsvydesignObj <- get(Scelta_EsvydesignObj, envir=.GlobalEnv)
                        # DIRE A RAFFAELLA 16/07/10

                        outSvystatTM <- Lancia(svystatTM(EsvydesignObj, y, by, estimator, vartype,
                                                            conf.int, conf.lev, deff, na.rm), textWarnings, parent = ttSvystatTM)

                        if (!inherits(outSvystatTM,"try-error")) {
                            attr(outSvystatTM,"design") <- as.symbol(Scelta_EsvydesignObj)
                            # assign(OR_SvystatTMObjectName, outSvystatTM, envir = .GlobalEnv)
                            assign2GE(OR_SvystatTMObjectName, outSvystatTM)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            if (!is.big(outSvystatTM)) {
                                cat(paste("# ",OR_SvystatTMObjectName,"\n",sep=""))
                            }
                            else {
                                cat(paste("# head(",OR_SvystatTMObjectName,")\n",sep=""))
                            }
                            printonscreen(outSvystatTM, OR_SvystatTMObjectName)
                            cat("\n")


                            prnDesign <- paste("design=",Scelta_EsvydesignObj)

                            prnSvystatTM<- paste(" <- svystatTM(",    prnDesign, ", ", prnY, ", ", prnBy,
                                        ", ", prnEstimator, ", ",    prnVartype, ", ", prnConf.int,
                                        ", ", prnConf.lev, ", ", prnDeff, ", ", prnNa.rm, ")", sep="")


                            commands <- paste(OR_SvystatTMObjectName, prnSvystatTM, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                  tkmessageBox(title ="svystatTM",
                                               message = "Operation executed\nNote: Warnings have been generated!",
                                               icon = "info", parent = ttSvystatTM)
                                 }
                            else {
                                  tkmessageBox(title ="svystatTM",message = "Operation executed", icon = "info", parent = ttSvystatTM)
                                 }
                            tkgrab.release(ttSvystatTM)
                        }
                    # get back the standard arrow cursor
                    tkconfigure(ttSvystatTM, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# ----------------------------------
# < END building svystatTM window. <
# ----------------------------------

# -----------------------------------
# > START building svystatR window. >
# -----------------------------------

            fSvystatR <- function(){
                    listEsvydesignObj <-  mk.class.list("analytic")
                    ttSvystatR <- tktoplevel()
                    tcl("wm", "protocol", ttSvystatR, "WM_DELETE_WINDOW", function() fOnCancel(ttSvystatR))
                    frameGlobal<- tkframe(ttSvystatR, borderwidth= 2)
                    tkwm.deiconify(ttSvystatR)
                    tkgrab.set(ttSvystatR)
                    tkfocus(ttSvystatR)
                    tkwm.title(ttSvystatR,label_SvystatR)
                    tkwm.resizable(ttSvystatR, 0, 0)

                    frameTop <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameDown <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameOutput  <- tkframe(frameGlobal, borderwidth=0)
                    frameButtons <- tkframe(frameGlobal, borderwidth=0)


                    scrEsvydesignObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstEsvydesignObj,...))
                    lista <- tclVar()
                    tclObj(lista) <- listEsvydesignObj
                    lstEsvydesignObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="browse", yscrollcommand = function (...)tkset(scrEsvydesignObj,...), background = "white")

                    scrVariables<- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstVariables,...))
                    listaVariables <- tclVar()
                    lstVariables <- tklistbox(frameTop,height = 4, listvariable= listaVariables, selectmode="extended", yscrollcommand = function (...)tkset(scrVariables,...), background = "white")

                    lblfEsvydesignObj<- tk2labelframe(frameTop)

                    count_SvystatR <<- FALSE
                    tkbind(lstEsvydesignObj, "<ButtonRelease-1>", function() fElencoCampiSvystatR(lista, lstEsvydesignObj,
                            lblVariables, listaVariables, lstVariables, scrVariables, num.but, den.but, by.but, num.ri.but, den.ri.but, by.ri.but,
                            lblESvystatDesignObj, ok.but, lstNum, lstDen, lstBy, entry.ObjectName))

                    lblEsvydesignObj <- ttklabel(frameTop, text="Select a survey design object",font=fontTextLabel)
                    lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")

                    lblESvystatDesignObj <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarESvystatDesignObj)),foreground="red")


                    labellblfNum <- tk2label(frameCentral,text="  num  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatR", args = "num")
                    tk2tip(labellblfNum,descfunz)

                    lblfNum<- tk2labelframe(frameCentral, labelwidget= labellblfNum)
                    frameNum <- tkframe(lblfNum, borderwidth=0)
                    frameNum.but <- tkframe(lblfNum, borderwidth=0)
                    scrNum <- tkscrollbar(frameNum, repeatinterval= 5, command = function(...) tkyview(lstNum,...))
                    listaNum <- tclVar()
                    lstNum <- tklistbox(frameNum, height=4, listvariable=listaNum, selectmode="extended", yscrollcommand = function (...)tkset(scrNum,...), background = "white")


                    labellblfDen <- tk2label(frameCentral,text="  den  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatR", args = "den")
                    tk2tip(labellblfDen,descfunz)

                    lblfDen<- tk2labelframe(frameCentral, labelwidget= labellblfDen)
                    frameDen <- tkframe(lblfDen, borderwidth=0)
                    frameDen.but <- tkframe(lblfDen, borderwidth=0)

                    scrDen <- tkscrollbar(frameDen, repeatinterval= 5, command = function(...) tkyview(lstDen,...))
                    listaDen <- tclVar()
                    lstDen <- tklistbox(frameDen, height=4, listvariable=listaDen, selectmode="extended", yscrollcommand = function (...)tkset(scrDen,...), background = "white")


                    labellblfBy <- tk2label(frameDown,text="  by  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatR", args = "by")
                    tk2tip(labellblfBy,descfunz)
                    lblfBy<- tk2labelframe(frameDown, labelwidget=labellblfBy)
                    frameBy <- tkframe(lblfBy, borderwidth=0)
                    frameBy.but <- tkframe(lblfBy, borderwidth=0)
                    scrBy <- tkscrollbar(frameBy, repeatinterval= 5, command = function(...) tkyview(lstBy,...))

                    listaBy <- tclVar()
                    lstBy <- tklistbox(frameBy, height=4, listvariable=listaBy, selectmode="extended", yscrollcommand = function (...)tkset(scrBy,...), background = "white")

                    num.but <- tk2button(frameNum.but, image=image_sx, state= "disabled", command = function()fTransfer_ND(lstNum, lstVariables, listaVariables))
                    num.ri.but <- tk2button(frameNum.but, image=image_dx, state= "disabled", command = function()fCancel(lstNum, lstVariables, listaNum))

                    den.but <- tk2button(frameDen.but, image=image_sx, state= "disabled", command = function()fTransfer_ND (lstDen, lstVariables, listaVariables))
                    den.ri.but <- tk2button(frameDen.but, image=image_dx, state= "disabled", command = function()fCancel(lstDen, lstVariables, listaDen))

                    by.but <- tk2button(frameBy.but, image=image_sx, state= "disabled", command = function()fTransfer(lstBy, lstVariables, listaVariables))
                    by.ri.but <- tk2button(frameBy.but, image=image_dx, state= "disabled", command = function()fDetransfer(lstBy, lstVariables, listaBy))

                    lblMandatory <- tk2label(frameGlobal,text="Mandatory fields", font=fontTextTitle, foreground= "blue")
                    lblOptional <-tk2label(frameGlobal,text="Optional fields", font=fontTextTitle, foreground= "blue")


                    labellblfDeff <- ttklabel(frameDown,text="  deff  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatTM", args = "deff")
                    tk2tip(labellblfDeff,descfunz)
                    lblfDeff<- ttklabelframe(frameDown, labelwidget=labellblfDeff)

                    rbDeffF <- ttkradiobutton(lblfDeff)
                    rbDeffT <- ttkradiobutton(lblfDeff)
                    rbDeffR <- ttkradiobutton(lblfDeff)

                    rbValueDeff <- tclVar("FALSE")

                    tkconfigure(rbDeffF,variable=rbValueDeff,value="FALSE")
                    tkconfigure(rbDeffT,variable=rbValueDeff,value="TRUE")
                    tkconfigure(rbDeffR,variable=rbValueDeff,value="replace")

                    labelDeffF <- ttklabel(lblfDeff,text="False ")
                    labelDeffT <- ttklabel(lblfDeff,text="True ")
                    labelDeffR <- ttklabel(lblfDeff,text="Replace")


                    labellblfVartype <- ttklabel(frameDown,text="  vartype  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatR", args = "vartype")
                    tk2tip(labellblfVartype,descfunz)
                    lblfVartype<- ttklabelframe(frameDown, labelwidget=labellblfVartype)
                    vartype_name <- c("se", "cv", "cvpct", "var")
                    vartype_value <- c("1", "0", "0", "0")
                    checkbuttonWidget <- list()
                    checkbuttonVar <- list()

                    lvar <-length(vartype_name)

                    for (i in 1:lvar){
                        checkbuttonWidget[[i]] <- ttkcheckbutton(lblfVartype)
                        checkbuttonVar[[i]] <- tclVar(vartype_value[i])
                    }


                    labellblfConf.int <- ttklabel(frameDown,text="  conf.int  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatR", args = "conf.int")
                    tk2tip(labellblfConf.int,descfunz)
                    lblfConf.int<- ttklabelframe(frameDown, labelwidget=labellblfConf.int)

                    rbConf.intF <- ttkradiobutton(lblfConf.int)
                    rbConf.intT <- ttkradiobutton(lblfConf.int)
                    rbValueConf.int <- tclVar("FALSE")

                    tkconfigure(rbConf.intF,variable=rbValueConf.int,value="FALSE")
                    tkconfigure(rbConf.intT,variable=rbValueConf.int,value="TRUE")

                    labelConf.intF <- ttklabel(lblfConf.int,text="False ")
                    labelConf.intT <- ttklabel(lblfConf.int,text="True ")


                    labellblfConf.lev <- ttklabel(frameDown,text="  conf.lev  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatR", args = "conf.lev")
                    tk2tip(labellblfConf.lev,descfunz)
                    lblfConf.lev<- ttklabelframe(frameDown, labelwidget=labellblfConf.lev)

                    tkbind(rbConf.intT, "<ButtonPress>", function() {tkconfigure(s, state = "normal")})
                    tkbind(rbConf.intF, "<ButtonPress>", function() {tkconfigure(s, state = "disabled")})

                    s <- tk2spinbox(lblfConf.lev, from=0.00, to=1.00, increment=0.01, state="disabled", width="4", background= "white")

                    tkconfigure(s, textvariable=tclVar(0.95))


                    labellblfNa.rm <- ttklabel(frameDown,text="  na.rm  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatR", args = "na.rm")
                    tk2tip(labellblfNa.rm,descfunz)
                    lblfNa.rm<- ttklabelframe(frameDown, labelwidget=labellblfNa.rm)

                    rbNa.rmF <- ttkradiobutton(lblfNa.rm)
                    rbNa.rmT <- ttkradiobutton(lblfNa.rm)
                    rbValueNa.rm <- tclVar("FALSE")

                    tkconfigure(rbNa.rmF,variable=rbValueNa.rm,value="FALSE")
                    tkconfigure(rbNa.rmT,variable=rbValueNa.rm,value="TRUE")

                    labelNa.rmF <- ttklabel(lblfNa.rm,text="False ")
                    labelNa.rmT <- ttklabel(lblfNa.rm,text="True ")


                    labellblfCross <- ttklabel(frameDown,text="  cross  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatR", args = "cross")
                    tk2tip(labellblfCross,descfunz)
                    lblfCross<- ttklabelframe(frameDown, labelwidget=labellblfCross)

                    rbCrossF <- ttkradiobutton(lblfCross)
                    rbCrossT <- ttkradiobutton(lblfCross)
                    rbValueCross <- tclVar("FALSE")

                    tkconfigure(rbCrossF,variable=rbValueCross,value="FALSE")
                    tkconfigure(rbCrossT,variable=rbValueCross,value="TRUE")

                    labelCrossF <- ttklabel(lblfCross,text="False ")
                    labelCrossT <- ttklabel(lblfCross,text="True ")


                    labellblfSvystatRObj <- ttklabel(frameOutput,text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfSvystatRObj<- ttklabelframe(frameOutput, labelwidget = labellblfSvystatRObj)

                    ObjectName <- tclVar("")
                    entry.ObjectName <-ttkentry(lblfSvystatRObj,width="20",text=ObjectName, state= "disabled", font="TkDefaultFont")

                    ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_SvystatR(lstNum, lstDen, lstBy, rbValueCross, rbValueDeff,
                              checkbuttonVar, lvar, vartype_name, rbValueConf.int, rbValueNa.rm, s, ObjectName,
                              ttSvystatR))


                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttSvystatR))

                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_SvystatR), command=function() fOnFunctionHelp(label_SvystatR))


                    tkgrid(tk2label (frameGlobal, text="Sample data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))
                    tkgrid(lblfEsvydesignObj)
                    tkgrid(lblEsvydesignObj, lblVariables)
                    tkgrid(lstEsvydesignObj, scrEsvydesignObj )

                    tkgrid.configure(lblEsvydesignObj, sticky ="e")
                    tkgrid.configure(lblVariables, column=4, sticky ="we")
                    tkgrid.configure(lblESvystatDesignObj, row=2, sticky="e")

                    tkgrid.configure(lstEsvydesignObj, row=3, sticky ="e", padx=c("1.5c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrEsvydesignObj, row=3, sticky ="nsw", padx=c(0,"2c"), pady=c(0,"0.2c"))

                    tkgrid(lblfNum, lblfDen)
                    tkgrid.configure(lblfNum,padx=c("0.4c",0), pady=c(0,"0.2c"))
                    tkgrid.configure(lblfDen,padx=c("2c","0.4c"), pady=c(0,"0.2c"))

                    tkgrid(frameNum.but, frameNum)
                    tkgrid.configure(frameNum.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameNum, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
                    tkgrid(num.but)
                    tkgrid(num.ri.but)
                    tkgrid(lstNum, scrNum)

                    tkgrid.configure(num.but, pady=c(0,"0.2c"))
                    tkgrid.configure(num.ri.but, pady=c("0.2c",0))
                    tkgrid.configure(scrNum, sticky ="nsw")

                    tkgrid(frameDen.but, frameDen)
                    tkgrid.configure(frameDen.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameDen, padx= c(0,"0.1c"), pady=c(0,"0.3c"))

                    tkgrid(den.but)
                    tkgrid(den.ri.but)
                    tkgrid(lstDen, scrDen)

                    tkgrid.configure(den.but, pady=c(0,"0.2c"))
                    tkgrid.configure(den.ri.but, pady=c("0.2c",0))
                    tkgrid.configure(scrDen, sticky ="nsw")

                    tkgrid(lblfBy, lblfCross,lblfDeff, lblfVartype, lblfConf.int, lblfConf.lev, lblfNa.rm)


                    tkgrid(frameBy.but, frameBy)
                    tkgrid.configure(frameBy.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameBy, padx= c(0,"0.1c"), pady=c(0,"0.3c"))

                    tkgrid(by.but)
                    tkgrid(by.ri.but)
                    tkgrid(lstBy, scrBy)

                    tkgrid.configure(by.but, pady=c(0,"0.2c"))
                    tkgrid.configure(by.ri.but, pady=c("0.2c",0))

                    tkgrid.configure(scrBy, sticky ="nsw")
                    tkgrid.configure(lblfBy,padx=c("0.5c","0.3c"))

                    tkgrid(labelDeffF,rbDeffF)
                    tkgrid(labelDeffT,rbDeffT)
                    tkgrid(labelDeffR,rbDeffR)

                    tkgrid.configure(labelDeffF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelDeffT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelDeffR, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfDeff,padx="0.3c")

                    for (i in 1:length(vartype_name)){
                        tkgrid(tklabel(lblfVartype,text=vartype_name[i]), checkbuttonWidget[[i]], sticky="w", padx=c("0.3c",0))
                        tkgrid(checkbuttonWidget[[i]] )
                        tkconfigure(checkbuttonWidget[[i]] ,variable=checkbuttonVar[[i]])
                    }

                    tkgrid.configure(lblfVartype,padx=c(0,"0.3c"))

                    tkgrid(labelConf.intF,rbConf.intF)
                    tkgrid(labelConf.intT,rbConf.intT)

                    tkgrid.configure(labelConf.intF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelConf.intT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfConf.int,padx=c(0,"0.3c"))

                    tkgrid.configure(s, padx=c("0.7c", 0), pady=c(0,"0.2c"))

                    tkgrid(labelNa.rmF,rbNa.rmF)
                    tkgrid(labelNa.rmT,rbNa.rmT)

                    tkgrid.configure(labelNa.rmF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelNa.rmT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfNa.rm,padx=c("0.3c","0.5c"))

                    tkgrid(labelCrossF,rbCrossF)
                    tkgrid(labelCrossT,rbCrossT)

                    tkgrid.configure(labelCrossF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelCrossT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfCross,padx=c(0, "0.3c"))

                    tkgrid(frameTop)
                    tkgrid(lblMandatory, pady=c("0.2c",0))

                    tkgrid(frameCentral)
                    tkgrid(lblOptional, pady=c("0.2c",0))

                    tkgrid(frameDown)
                    tkgrid.configure(frameDown, padx="0.5c")

                    tkgrid(lblfSvystatRObj)
                    tkgrid.configure(entry.ObjectName, padx=c("0.5c","7c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")
                    tkgrid.configure(frameButtons, sticky="ne")
                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)
                    tkgrid.configure(Cancel.but, padx=("0.5c"))
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))
                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttSvystatR))) # To asses the size of the window...
                    # tkfocus(ttSvystatR)
            }

            fElencoCampiSvystatR <- function(EC_lista, EC_lstEsvydesignObj, EC_lblVariables, x, lstEC_EsvydesignObj,
                    scrEC, EC_num.but, EC_den.but, EC_by.but, EC_num.ri.but, EC_den.ri.but, EC_by.ri.but,
                    EC_lblESvystatDesignObj, EC_ok.but, EC_lstNum, EC_lstDen, EC_lstBy, EC_entry.ObjectName){

                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstEsvydesignObj)))

                    assignTemp("SvystatR_Scelta_EsvydesignObj", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstEsvydesignObj))+1])


                    # DIRE A RAFFAELLA 16/07/10
                    # NO!!! Copying may become a serious concern when data get big
                    # (can tolerate it for names, because they are small: see above)
                    # assignTemp("SvystatR_EsvydesignObj", get(SvystatR_Scelta_EsvydesignObj, envir=.GlobalEnv))
                    SvystatR_EsvydesignObj <- get(SvystatR_Scelta_EsvydesignObj, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    VarESvystatDesignObj <<- tclVar(SvystatR_Scelta_EsvydesignObj)
                    tkconfigure(EC_lblESvystatDesignObj, textvariable= VarESvystatDesignObj)

                    if (Index_dataframe_old_Svyby !=EC_indicesel){
                            count_SvystatR <<- FALSE
                    }
                    if (count_SvystatR == FALSE){
                        count_SvystatR <<- TRUE
                        Index_dataframe_old_Svyby <<- EC_indicesel

                        tkdelete(EC_lstNum, 0, "end")
                        tkdelete(EC_lstDen, 0, "end")
                        tkdelete(EC_lstBy, 0, "end")
                        tkdelete(EC_entry.ObjectName, 0, "end")

# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                        for  (n in names(SvystatR_EsvydesignObj$variables)){
#                                tclObj(x) <- c(names(SvystatR_EsvydesignObj$variables))
#                        }
                        tclObj(x) <- names(SvystatR_EsvydesignObj$variables)

                        tkgrid(lstEC_EsvydesignObj, scrEC)

                        tkgrid.configure(lstEC_EsvydesignObj, column=4, row=3, sticky ="e", pady=c(0,"0.2c"))
                        tkgrid.configure(scrEC, column=5, row=3, padx=c("0c","1.6c"), pady=c(0,"0.2c"), sticky ="nsw")
                        tkconfigure(EC_num.but, state = "normal")
                        tkconfigure(EC_den.but, state = "normal")
                        tkconfigure(EC_by.but, state = "normal")

                        tkconfigure(EC_num.ri.but, state = "normal")
                        tkconfigure(EC_den.ri.but, state = "normal")
                        tkconfigure(EC_by.ri.but, state = "normal")

                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_lblVariables, text="Variables", state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")
                    }
            }


            fOnRun_SvystatR <- function(OR_lstNum, OR_lstDen, OR_lstBy, OR_rbValueCross, OR_rbValueDeff,
                                        OR_checkbuttonVar, OR_lvar, OR_vartype_name, OR_rbValueConf.int,
                                        OR_rbValueNa.rm, OR_s, OR_SvystatRObjectName, ttSvystatR){
                    campobb <- TRUE

                    all.lstNum <- tclvalue(tkget(OR_lstNum, 0, "end"))

                    if  (all.lstNum =="") {
                        tkmessageBox(title="List num empty", message = "The list is empty. Transfer at least an element", icon = "error", parent = ttSvystatR)
                        campobb <- FALSE
                    }
                    else {
                        numsum <- gsub(" ", "+",all.lstNum)

                        num <- as.formula(paste("~", numsum), env = .GlobalEnv)
                        prnNum <- paste("num=", numsum, sep =" ~ ")
                    }

                    all.lstDen <- tclvalue(tkget(OR_lstDen, 0, "end"))

                    if  (all.lstDen =="") {
                        tkmessageBox(title="List den empty", message = "The list is empty. Transfer at least an element", icon = "error", parent = ttSvystatR)
                        campobb <- FALSE
                    }
                    else {
                        densum <- gsub(" ", "+",all.lstDen)

                        den <- as.formula(paste("~", densum), env = .GlobalEnv)
                        prnDen <- paste("den=", densum, sep =" ~ ")
                    }

                    all.lstBy <- tclvalue(tkget(OR_lstBy, 0, "end"))

                    if  (all.lstBy =="") {
                        by <- NULL
                        prnBy <- "by= NULL"
                        }
                    else {
                        bysum <- gsub(" ", ":",all.lstBy)

                        by <- as.formula(paste("~", bysum), env = .GlobalEnv)
                        prnBy <- paste("by=", bysum, sep =" ~ ")
                    }

                    if ( (deff.tmp <-  tclvalue(OR_rbValueDeff))=="replace" ){
                        deff <- deff.tmp
                        prnDeff <- 'deff= "replace"'
                    }
                    else {
                         deff <- as.logical(deff.tmp)
                         if (!deff) {
                             prnDeff <- "deff= FALSE"
                         }
                         else {
                             prnDeff <- "deff= TRUE"
                         }
                    }

                    choices <- c()
                    j<- 1
                    for (i in 1:OR_lvar){
                          if(tclvalue(OR_checkbuttonVar[[i]]) == "1") {
                          choices[j] <- OR_vartype_name[i]
                          j <- j + 1
                        }
                    }

                    if ((j-1) == 0) {
                        prnVartype <- 'vartype= "se"'
                        vartype <- "se"
                    }
                    else {
                        vartype <- choices
                        if ((j-1) == 1) {
                           prnVartype <- paste('vartype= "', choices[1], '"', sep="")
                        }
                        else{
                            prnVartype <- "vartype= c("
                            for(i in 1:(j-1)){
                                prnVartype <- paste(prnVartype,'"',choices[i],'"', sep="")
                                if (i<(j-1)) {prnVartype <- paste(prnVartype,', ', sep="")}
                            }
                            prnVartype <- paste (prnVartype,  ')', sep = "")
                        }
                    }

                    conf.int<- as.logical(tclvalue(OR_rbValueConf.int))
                    if (!conf.int) {
                        prnConf.int <- "conf.int= FALSE"
                        conf.lev <- 0.95
                        prnConf.lev <- "conf.lev= 0.95"
                    }
                    else{
                        prnConf.int <- "conf.int= TRUE"
                        conf.lev <- tclvalue(tkget(OR_s))
                        if (conf.lev ==""){
                            tkmessageBox(title="conf.lev", message="The field is empty!",icon="error", parent = ttSvystatR)
                            campobb <- FALSE
                        }
                        else{
                            conf.lev <- as.numeric(tkget(OR_s))
                            if (is.na(conf.lev)){
                                tkmessageBox(title="conf.lev", message="Please insert a numeric value",icon="error", parent = ttSvystatR)
                                campobb <- FALSE
                            }
                            else{
                                prnConf.lev <- paste("conf.lev= ", conf.lev, sep="")
                            }
                        }
                    }

                    na.rm <- as.logical(tclvalue(OR_rbValueNa.rm))
                    if (!na.rm) {
                        prnNa.rm <- "na.rm= FALSE"
                    }
                    else{
                        prnNa.rm <- "na.rm= TRUE"
                    }

                    cross <- as.logical(tclvalue(OR_rbValueCross))
                    if (!cross) {
                        prnCross <- "cross= FALSE"
                    }
                    else{
                        prnCross <- "cross= TRUE"
                    }

                    if (tclvalue(OR_SvystatRObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = ttSvystatR)
                        campobb <- FALSE
                    }
                    else{
                        if   ( !is.ok.name(tclvalue(OR_SvystatRObjectName), go.on=campobb, parent= ttSvystatR) ){
                              campobb <- FALSE
                             }
                        else {
                             OR_SvystatRObjectName <- tclvalue(OR_SvystatRObjectName)
                             }
                        }
                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(ttSvystatR, cursor="watch")

                        # DIRE A RAFFAELLA 16/07/10
                        # SvystatR_EsvydesignObj no more exists into TempEnv: we left inside its name only
                        SvystatR_EsvydesignObj <- get(SvystatR_Scelta_EsvydesignObj, envir=.GlobalEnv)
                        # DIRE A RAFFAELLA 16/07/10

                        outSvystatR <- Lancia(svystatR(SvystatR_EsvydesignObj, num, den, by, cross, vartype,
                                                        conf.int, conf.lev, deff, na.rm), textWarnings, parent = ttSvystatR)

                        if (!inherits(outSvystatR,"try-error")) {
                            attr(outSvystatR,"design") <- as.symbol(SvystatR_Scelta_EsvydesignObj)
                            # assign(OR_SvystatRObjectName, outSvystatR, envir = .GlobalEnv)
                            assign2GE(OR_SvystatRObjectName, outSvystatR)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            if (!is.big(outSvystatR)) {
                                cat(paste("# ",OR_SvystatRObjectName,"\n",sep=""))
                            }
                            else {
                                cat(paste("# head(",OR_SvystatRObjectName,")\n",sep=""))
                            }
                            printonscreen(outSvystatR, OR_SvystatRObjectName)
                            cat("\n")

                            prnDesign <- paste("design=",SvystatR_Scelta_EsvydesignObj)

                            prnSvystatR<- paste(" <- svystatR(", prnDesign, ", ", prnNum, ", ", prnDen, ", ", prnBy,
                                        ", ",    prnCross, ", ",    prnVartype, ", ", prnConf.int,
                                        ", ", prnConf.lev, ", ", prnDeff, ", ", prnNa.rm, ")", sep="")

                            commands <- paste(OR_SvystatRObjectName, prnSvystatR, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                  tkmessageBox(title ="svystatR",
                                               message = "Operation executed\nNote: Warnings have been generated!",
                                               icon = "info", parent = ttSvystatR)
                                 }
                            else {
                                  tkmessageBox(title ="svystatR",message = "Operation executed", icon = "info", parent = ttSvystatR)
                                 }
                            tkgrab.release(ttSvystatR)
                        }
                    # get back the standard arrow cursor
                    tkconfigure(ttSvystatR, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# ---------------------------------
# < END building svystatR window. >
# ---------------------------------

# -----------------------------------
# > START building svystatS window. >
# -----------------------------------

            fSvystatS <- function(){
                    listEsvydesignObj <-  mk.class.list("analytic")
                    ttSvystatS <- tktoplevel()
                    tcl("wm", "protocol", ttSvystatS, "WM_DELETE_WINDOW", function() fOnCancel(ttSvystatS))
                    frameGlobal<- tkframe(ttSvystatS, borderwidth= 2)
                    tkwm.deiconify(ttSvystatS)
                    tkgrab.set(ttSvystatS)
                    tkfocus(ttSvystatS)
                    tkwm.title(ttSvystatS,label_SvystatS)
                    tkwm.resizable(ttSvystatS, 0, 0)

                    frameTop <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameDown <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameOutput  <- tkframe(frameGlobal, borderwidth=0)
                    frameButtons <- tkframe(frameGlobal, borderwidth=0)


                    scrEsvydesignObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstEsvydesignObj,...))
                    lista <- tclVar()
                    tclObj(lista) <- listEsvydesignObj
                    lstEsvydesignObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="browse", yscrollcommand = function (...)tkset(scrEsvydesignObj,...), background = "white")

                    scrVariables<- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstVariables,...))
                    listaVariables <- tclVar()
                    lstVariables <- tklistbox(frameTop,height = 4, listvariable= listaVariables, selectmode="extended", yscrollcommand = function (...)tkset(scrVariables,...), background = "white")

                    lblfEsvydesignObj<- tk2labelframe(frameTop)

                    count_SvystatS <<- FALSE
                    tkbind(lstEsvydesignObj, "<ButtonRelease-1>", function() fElencoCampiSvystatS(lista, lstEsvydesignObj,
                            lblVariables, listaVariables, lstVariables, scrVariables, y.but, classes.but, by.but, y.ri.but, classes.ri.but, by.ri.but,
                            lblESvystatDesignObj, ok.but, lstY, lstClasses, lstBy, entry.ObjectName))

                    lblEsvydesignObj <- ttklabel(frameTop, text="Select a survey design object",font=fontTextLabel)
                    lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")

                    lblESvystatDesignObj <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarESvystatDesignObj)),foreground="red")


                    labellblfY <- tk2label(frameCentral,text="  y  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatS", args = "y")
                    tk2tip(labellblfY,descfunz)

                    lblfY<- tk2labelframe(frameCentral, labelwidget= labellblfY)
                    frameY <- tkframe(lblfY, borderwidth=0)
                    frameY.but <- tkframe(lblfY, borderwidth=0)
                    scrY <- tkscrollbar(frameY, repeatinterval= 5, command = function(...) tkyview(lstY,...))
                    listaY <- tclVar()
                    lstY <- tklistbox(frameY, height=4, listvariable=listaY, selectmode="extended", yscrollcommand = function (...)tkset(scrY,...), background = "white")


                    labellblfClasses <- tk2label(frameCentral,text="  classes  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatS", args = "classes")
                    tk2tip(labellblfClasses,descfunz)

                    lblfClasses<- tk2labelframe(frameCentral, labelwidget= labellblfClasses)
                    frameClasses <- tkframe(lblfClasses, borderwidth=0)
                    frameClasses.but <- tkframe(lblfClasses, borderwidth=0)

                    scrClasses <- tkscrollbar(frameClasses, repeatinterval= 5, command = function(...) tkyview(lstClasses,...))
                    listaClasses <- tclVar()
                    lstClasses <- tklistbox(frameClasses, height=4, listvariable=listaClasses, selectmode="extended", yscrollcommand = function (...)tkset(scrClasses,...), background = "white")


                    labellblfBy <- tk2label(frameDown,text="  by  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatS", args = "by")
                    tk2tip(labellblfBy,descfunz)
                    lblfBy<- tk2labelframe(frameDown, labelwidget=labellblfBy)
                    frameBy <- tkframe(lblfBy, borderwidth=0)
                    frameBy.but <- tkframe(lblfBy, borderwidth=0)
                    scrBy <- tkscrollbar(frameBy, repeatinterval= 5, command = function(...) tkyview(lstBy,...))

                    listaBy <- tclVar()
                    lstBy <- tklistbox(frameBy, height=4, listvariable=listaBy, selectmode="extended", yscrollcommand = function (...)tkset(scrBy,...), background = "white")

                    y.but <- tk2button(frameY.but, image=image_sx, state= "disabled", command = function()fTransfer_ND(lstY, lstVariables, listaVariables))
                    y.ri.but <- tk2button(frameY.but, image=image_dx, state= "disabled", command = function()fCancel(lstY, lstVariables, listaY))

                    classes.but <- tk2button(frameClasses.but, image=image_sx, state= "disabled", command = function()fTransfer_ND (lstClasses, lstVariables, listaVariables))
                    classes.ri.but <- tk2button(frameClasses.but, image=image_dx, state= "disabled", command = function()fCancel(lstClasses, lstVariables, listaClasses))

                    by.but <- tk2button(frameBy.but, image=image_sx, state= "disabled", command = function()fTransfer(lstBy, lstVariables, listaVariables))
                    by.ri.but <- tk2button(frameBy.but, image=image_dx, state= "disabled", command = function()fDetransfer(lstBy, lstVariables, listaBy))

                    lblMandatory <- tk2label(frameGlobal,text="Mandatory fields", font=fontTextTitle, foreground= "blue")
                    lblOptional <-tk2label(frameGlobal,text="Optional fields", font=fontTextTitle, foreground= "blue")


                    labellblfDeff <- ttklabel(frameDown,text="  deff  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatTM", args = "deff")
                    tk2tip(labellblfDeff,descfunz)
                    lblfDeff<- ttklabelframe(frameDown, labelwidget=labellblfDeff)

                    rbDeffF <- ttkradiobutton(lblfDeff)
                    rbDeffT <- ttkradiobutton(lblfDeff)
                    rbDeffR <- ttkradiobutton(lblfDeff)

                    rbValueDeff <- tclVar("FALSE")

                    tkconfigure(rbDeffF,variable=rbValueDeff,value="FALSE")
                    tkconfigure(rbDeffT,variable=rbValueDeff,value="TRUE")
                    tkconfigure(rbDeffR,variable=rbValueDeff,value="replace")

                    labelDeffF <- ttklabel(lblfDeff,text="False ")
                    labelDeffT <- ttklabel(lblfDeff,text="True ")
                    labelDeffR <- ttklabel(lblfDeff,text="Replace")


                    labellblfVartype <- ttklabel(frameDown,text="  vartype  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatS", args = "vartype")
                    tk2tip(labellblfVartype,descfunz)
                    lblfVartype<- ttklabelframe(frameDown, labelwidget=labellblfVartype)
                    vartype_name <- c("se", "cv", "cvpct", "var")
                    vartype_value <- c("1", "0", "0", "0")
                    checkbuttonWidget <- list()
                    checkbuttonVar <- list()

                    lvar <-length(vartype_name)

                    for (i in 1:lvar){
                        checkbuttonWidget[[i]] <- ttkcheckbutton(lblfVartype)
                        checkbuttonVar[[i]] <- tclVar(vartype_value[i])
                    }


                    labellblfConf.int <- ttklabel(frameDown,text="  conf.int  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatS", args = "conf.int")
                    tk2tip(labellblfConf.int,descfunz)
                    lblfConf.int<- ttklabelframe(frameDown, labelwidget=labellblfConf.int)

                    rbConf.intF <- ttkradiobutton(lblfConf.int)
                    rbConf.intT <- ttkradiobutton(lblfConf.int)
                    rbValueConf.int <- tclVar("FALSE")

                    tkconfigure(rbConf.intF,variable=rbValueConf.int,value="FALSE")
                    tkconfigure(rbConf.intT,variable=rbValueConf.int,value="TRUE")

                    labelConf.intF <- ttklabel(lblfConf.int,text="False ")
                    labelConf.intT <- ttklabel(lblfConf.int,text="True ")


                    labellblfConf.lev <- ttklabel(frameDown,text="  conf.lev  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatS", args = "conf.lev")
                    tk2tip(labellblfConf.lev,descfunz)
                    lblfConf.lev<- ttklabelframe(frameDown, labelwidget=labellblfConf.lev)

                    tkbind(rbConf.intT, "<ButtonPress>", function() {tkconfigure(s, state = "normal")})
                    tkbind(rbConf.intF, "<ButtonPress>", function() {tkconfigure(s, state = "disabled")})

                    s <- tk2spinbox(lblfConf.lev, from=0.00, to=1.00, increment=0.01, state="disabled", width="4", background= "white")

                    tkconfigure(s, textvariable=tclVar(0.95))


                    labellblfNa.rm <- ttklabel(frameDown,text="  na.rm  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatS", args = "na.rm")
                    tk2tip(labellblfNa.rm,descfunz)
                    lblfNa.rm<- ttklabelframe(frameDown, labelwidget=labellblfNa.rm)

                    rbNa.rmF <- ttkradiobutton(lblfNa.rm)
                    rbNa.rmT <- ttkradiobutton(lblfNa.rm)
                    rbValueNa.rm <- tclVar("FALSE")

                    tkconfigure(rbNa.rmF,variable=rbValueNa.rm,value="FALSE")
                    tkconfigure(rbNa.rmT,variable=rbValueNa.rm,value="TRUE")

                    labelNa.rmF <- ttklabel(lblfNa.rm,text="False ")
                    labelNa.rmT <- ttklabel(lblfNa.rm,text="True ")


                    labellblfSvystatSObj <- ttklabel(frameOutput,text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfSvystatSObj<- ttklabelframe(frameOutput, labelwidget = labellblfSvystatSObj)

                    ObjectName <- tclVar("")
                    entry.ObjectName <-ttkentry(lblfSvystatSObj,width="20",text=ObjectName, state= "disabled", font="TkDefaultFont")

                    ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_SvystatS(lstY, lstClasses, lstBy, rbValueDeff,
                              checkbuttonVar, lvar, vartype_name, rbValueConf.int, rbValueNa.rm, s, ObjectName,
                              ttSvystatS))


                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttSvystatS))

                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_SvystatS), command=function() fOnFunctionHelp(label_SvystatS))


                    tkgrid(tk2label (frameGlobal, text="Sample data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))
                    tkgrid(lblfEsvydesignObj)
                    tkgrid(lblEsvydesignObj, lblVariables)
                    tkgrid(lstEsvydesignObj, scrEsvydesignObj )

                    tkgrid.configure(lblEsvydesignObj, sticky ="e")
                    tkgrid.configure(lblVariables, column=4, sticky ="we")
                    tkgrid.configure(lblESvystatDesignObj, row=2, sticky="e")

                    tkgrid.configure(lstEsvydesignObj, row=3, sticky ="e", padx=c("1.5c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrEsvydesignObj, row=3, sticky ="nsw", padx=c(0,"2c"), pady=c(0,"0.2c"))

                    tkgrid(lblfY, lblfClasses)
                    tkgrid.configure(lblfY,padx=c("0.4c",0), pady=c(0,"0.2c"))
                    tkgrid.configure(lblfClasses,padx=c("2c","0.4c"), pady=c(0,"0.2c"))

                    tkgrid(frameY.but, frameY)
                    tkgrid.configure(frameY.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameY, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
                    tkgrid(y.but)
                    tkgrid(y.ri.but)
                    tkgrid(lstY, scrY)

                    tkgrid.configure(y.but, pady=c(0,"0.2c"))
                    tkgrid.configure(y.ri.but, pady=c("0.2c",0))
                    tkgrid.configure(scrY, sticky ="nsw")

                    tkgrid(frameClasses.but, frameClasses)
                    tkgrid.configure(frameClasses.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameClasses, padx= c(0,"0.1c"), pady=c(0,"0.3c"))

                    tkgrid(classes.but)
                    tkgrid(classes.ri.but)
                    tkgrid(lstClasses, scrClasses)

                    tkgrid.configure(classes.but, pady=c(0,"0.2c"))
                    tkgrid.configure(classes.ri.but, pady=c("0.2c",0))
                    tkgrid.configure(scrClasses, sticky ="nsw")

                    tkgrid(lblfBy,lblfDeff,lblfVartype, lblfConf.int, lblfConf.lev, lblfNa.rm)

                    tkgrid.configure(lblfBy,padx=c("0.4c","0.3c"), pady=c(0,"0.2c"))
                    tkgrid(frameBy.but, frameBy)
                    tkgrid.configure(frameBy.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameBy, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
                    tkgrid(by.but)
                    tkgrid(by.ri.but)
                    tkgrid(lstBy, scrBy)
                    tkgrid.configure(by.but, pady=c(0,"0.2c"))
                    tkgrid.configure(by.ri.but, pady=c("0.2c",0))
                    tkgrid.configure(scrBy, sticky ="nsw")

                    tkgrid(labelDeffF,rbDeffF)
                    tkgrid(labelDeffT,rbDeffT)
                    tkgrid(labelDeffR,rbDeffR)

                    tkgrid.configure(labelDeffF, sticky ="w", padx=c("0.3c",0))
                    tkgrid.configure(labelDeffT, sticky ="w", padx=c("0.3c",0))
                    tkgrid.configure(labelDeffR, sticky ="w", padx=c("0.3c",0))


                    for (i in 1:length(vartype_name)){
                        tkgrid(tklabel(lblfVartype,text=vartype_name[i]), checkbuttonWidget[[i]], sticky="w", padx=c("0.3c",0))
                        tkgrid(checkbuttonWidget[[i]] )
                        tkconfigure(checkbuttonWidget[[i]] ,variable=checkbuttonVar[[i]])
                    }

                    tkgrid.configure(lblfVartype,padx=c("0.3c","0.3c"))

                    tkgrid(labelConf.intF,rbConf.intF)
                    tkgrid(labelConf.intT,rbConf.intT)

                    tkgrid.configure(labelConf.intF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelConf.intT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfConf.int,padx=c(0,"0.3c"))

                    tkgrid.configure(s, padx=c("0.7c", 0), pady=c(0,"0.2c"))

                    tkgrid(labelNa.rmF,rbNa.rmF)
                    tkgrid(labelNa.rmT,rbNa.rmT)

                    tkgrid.configure(labelNa.rmF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelNa.rmT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfNa.rm,padx=c("0.3c","0.5c"))


                    tkgrid(frameTop)
                    tkgrid(lblMandatory, pady=c("0.2c",0))

                    tkgrid(frameCentral)
                    tkgrid(lblOptional, pady=c("0.2c",0))

                    tkgrid(frameDown)
                    tkgrid.configure(frameDown, padx="0.5c")

                    tkgrid(lblfSvystatSObj)
                    tkgrid.configure(entry.ObjectName, padx=c("0.5c","7c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")
                    tkgrid.configure(frameButtons, sticky="ne")
                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)
                    tkgrid.configure(Cancel.but, padx=("0.5c"))
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))
                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttSvystatS))) # To asses the size of the window...
                    # tkfocus(ttSvystatS)
            }

            fElencoCampiSvystatS <- function(EC_lista, EC_lstEsvydesignObj, EC_lblVariables, x, lstEC_EsvydesignObj,
                    scrEC, EC_y.but, EC_classes.but, EC_by.but, EC_y.ri.but, EC_classes.ri.but, EC_by.ri.but,
                    EC_lblESvystatDesignObj, EC_ok.but, EC_lstY, EC_lstClasses, EC_lstBy, EC_entry.ObjectName){

                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstEsvydesignObj)))

                    assignTemp("SvystatS_Scelta_EsvydesignObj", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstEsvydesignObj))+1])


                    # DIRE A RAFFAELLA 16/07/10
                    # NO!!! Copying may become a serious concern when data get big
                    # (can tolerate it for names, because they are small: see above)
                    # assignTemp("SvystatS_EsvydesignObj", get(SvystatS_Scelta_EsvydesignObj, envir=.GlobalEnv))
                    SvystatS_EsvydesignObj <- get(SvystatS_Scelta_EsvydesignObj, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    VarESvystatDesignObj <<- tclVar(SvystatS_Scelta_EsvydesignObj)
                    tkconfigure(EC_lblESvystatDesignObj, textvariable= VarESvystatDesignObj)

                    if (Index_dataframe_old_Svyby !=EC_indicesel){
                            count_SvystatS <<- FALSE
                    }
                    if (count_SvystatS == FALSE){
                        count_SvystatS <<- TRUE
                        Index_dataframe_old_Svyby <<- EC_indicesel

                        tkdelete(EC_lstY, 0, "end")
                        tkdelete(EC_lstClasses, 0, "end")
                        tkdelete(EC_lstBy, 0, "end")
                        tkdelete(EC_entry.ObjectName, 0, "end")

# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                        for  (n in names(SvystatS_EsvydesignObj$variables)){
#                                tclObj(x) <- c(names(SvystatS_EsvydesignObj$variables))
#                        }
                        tclObj(x) <- names(SvystatS_EsvydesignObj$variables)

                        tkgrid(lstEC_EsvydesignObj, scrEC)

                        tkgrid.configure(lstEC_EsvydesignObj, column=4, row=3, sticky ="e", pady=c(0,"0.2c"))
                        tkgrid.configure(scrEC, column=5, row=3, padx=c("0c","1.6c"), pady=c(0,"0.2c"), sticky ="nsw")
                        tkconfigure(EC_y.but, state = "normal")
                        tkconfigure(EC_classes.but, state = "normal")
                        tkconfigure(EC_by.but, state = "normal")

                        tkconfigure(EC_y.ri.but, state = "normal")
                        tkconfigure(EC_classes.ri.but, state = "normal")
                        tkconfigure(EC_by.ri.but, state = "normal")

                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_lblVariables, text="Variables", state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")
                    }
            }


            fOnRun_SvystatS <- function(OR_lstY, OR_lstClasses, OR_lstBy, OR_rbValueDeff,
                                        OR_checkbuttonVar, OR_lvar, OR_vartype_name, OR_rbValueConf.int,
                                        OR_rbValueNa.rm, OR_s, OR_SvystatSObjectName, ttSvystatS){
                    campobb <- TRUE

                    all.lstY <- tclvalue(tkget(OR_lstY, 0, "end"))

                    if  (all.lstY =="") {
                        tkmessageBox(title="List y empty", message = "The list is empty. Transfer at least an element", icon = "error", parent = ttSvystatS)
                        campobb <- FALSE
                    }
                    else {
                        ysum <- gsub(" ", "+",all.lstY)

                        y <- as.formula(paste("~", ysum), env = .GlobalEnv)
                        prnY <- paste("y=", ysum, sep =" ~ ")
                    }

                    all.lstClasses <- tclvalue(tkget(OR_lstClasses, 0, "end"))

                    if  (all.lstClasses =="") {
                        tkmessageBox(title="List classes empty", message = "The list is empty. Transfer at least an element", icon = "error", parent = ttSvystatS)
                        campobb <- FALSE
                    }
                    else {
                        classessum <- gsub(" ", "+",all.lstClasses)

                        classes <- as.formula(paste("~", classessum), env = .GlobalEnv)
                        prnClasses <- paste("classes=", classessum, sep =" ~ ")
                    }

                    all.lstBy <- tclvalue(tkget(OR_lstBy, 0, "end"))

                    if  (all.lstBy =="") {
                        by <- NULL
                        prnBy <- "by= NULL"
                        }
                    else {
                        bysum <- gsub(" ", ":",all.lstBy)

                        by <- as.formula(paste("~", bysum), env = .GlobalEnv)
                        prnBy <- paste("by=", bysum, sep =" ~ ")
                    }

                    if ( (deff.tmp <-  tclvalue(OR_rbValueDeff))=="replace" ){
                        deff <- deff.tmp
                        prnDeff <- 'deff= "replace"'
                    }
                    else {
                         deff <- as.logical(deff.tmp)
                         if (!deff) {
                             prnDeff <- "deff= FALSE"
                         }
                         else {
                             prnDeff <- "deff= TRUE"
                         }
                    }

                    choices <- c()
                    j<- 1
                    for (i in 1:OR_lvar){
                          if(tclvalue(OR_checkbuttonVar[[i]]) == "1") {
                          choices[j] <- OR_vartype_name[i]
                          j <- j + 1
                        }
                    }

                    if ((j-1) == 0) {
                        prnVartype <- 'vartype= "se"'
                        vartype <- "se"
                    }
                    else {
                        vartype <- choices
                        if ((j-1) == 1) {
                           prnVartype <- paste('vartype= "', choices[1], '"', sep="")
                        }
                        else{
                            prnVartype <- "vartype= c("
                            for(i in 1:(j-1)){
                                prnVartype <- paste(prnVartype,'"',choices[i],'"', sep="")
                                if (i<(j-1)) {prnVartype <- paste(prnVartype,', ', sep="")}
                            }
                            prnVartype <- paste (prnVartype,  ')', sep = "")
                        }
                    }

                    conf.int<- as.logical(tclvalue(OR_rbValueConf.int))
                    if (!conf.int) {
                        prnConf.int <- "conf.int= FALSE"
                        conf.lev <- 0.95
                        prnConf.lev <- "conf.lev= 0.95"
                    }
                    else{
                        prnConf.int <- "conf.int= TRUE"
                        conf.lev <- tclvalue(tkget(OR_s))
                        if (conf.lev ==""){
                            tkmessageBox(title="conf.lev", message="The field is empty!",icon="error", parent = ttSvystatS)
                            campobb <- FALSE
                        }
                        else{
                            conf.lev <- as.numeric(tkget(OR_s))
                            if (is.na(conf.lev)){
                                tkmessageBox(title="conf.lev", message="Please insert a numeric value",icon="error", parent = ttSvystatS)
                                campobb <- FALSE
                            }
                            else{
                                prnConf.lev <- paste("conf.lev= ", conf.lev, sep="")
                            }
                        }
                    }

                    na.rm <- as.logical(tclvalue(OR_rbValueNa.rm))
                    if (!na.rm) {
                        prnNa.rm <- "na.rm= FALSE"
                    }
                    else{
                        prnNa.rm <- "na.rm= TRUE"
                    }


                    if (tclvalue(OR_SvystatSObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = ttSvystatS)
                        campobb <- FALSE
                    }
                    else{
                        if   ( !is.ok.name(tclvalue(OR_SvystatSObjectName), go.on=campobb, parent= ttSvystatS) ){
                              campobb <- FALSE
                             }
                        else {
                             OR_SvystatSObjectName <- tclvalue(OR_SvystatSObjectName)
                             }
                        }
                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(ttSvystatS, cursor="watch")

                        # DIRE A RAFFAELLA 16/07/10
                        # SvystatS_EsvydesignObj no more exists into TempEnv: we left inside its name only
                        SvystatS_EsvydesignObj <- get(SvystatS_Scelta_EsvydesignObj, envir=.GlobalEnv)
                        # DIRE A RAFFAELLA 16/07/10

                        outSvystatS <- Lancia(svystatS(SvystatS_EsvydesignObj, y, classes, by, vartype,
                                                        conf.int, conf.lev, deff, na.rm), textWarnings, parent = ttSvystatS)

                        if (!inherits(outSvystatS,"try-error")) {
                            attr(outSvystatS,"design") <- as.symbol(SvystatS_Scelta_EsvydesignObj)
                            # assign(OR_SvystatSObjectName, outSvystatS, envir = .GlobalEnv)
                            assign2GE(OR_SvystatSObjectName, outSvystatS)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            if (!is.big(outSvystatS)) {
                                cat(paste("# ",OR_SvystatSObjectName,"\n",sep=""))
                            }
                            else {
                                cat(paste("# head(",OR_SvystatSObjectName,")\n",sep=""))
                            }
                            printonscreen(outSvystatS, OR_SvystatSObjectName)
                            cat("\n")

                            prnDesign <- paste("design=",SvystatS_Scelta_EsvydesignObj)

                            prnSvystatS<- paste(" <- svystatS(", prnDesign, ", ", prnY, ", ", prnClasses, ", ", prnBy,
                                        ", ",  prnVartype, ", ", prnConf.int,
                                        ", ", prnConf.lev, ", ", prnDeff, ", ", prnNa.rm, ")", sep="")

                            commands <- paste(OR_SvystatSObjectName, prnSvystatS, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                  tkmessageBox(title ="svystatS",
                                               message = "Operation executed\nNote: Warnings have been generated!",
                                               icon = "info", parent = ttSvystatS)
                                 }
                            else {
                                  tkmessageBox(title ="svystatS",message = "Operation executed", icon = "info", parent = ttSvystatS)
                                 }
                            tkgrab.release(ttSvystatS)
                        }
                    # get back the standard arrow cursor
                    tkconfigure(ttSvystatS, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# ---------------------------------
# < END building svystatS window. >
# ---------------------------------

# ------------------------------------
# > START building svystatSR window. >
# ------------------------------------

            fSvystatSR <- function(){
                    listEsvydesignObj <-  mk.class.list("analytic")
                    ttSvystatSR <- tktoplevel()
                    tcl("wm", "protocol", ttSvystatSR, "WM_DELETE_WINDOW", function() fOnCancel(ttSvystatSR))
                    frameGlobal<- tkframe(ttSvystatSR, borderwidth= 2)
                    tkwm.deiconify(ttSvystatSR)
                    tkgrab.set(ttSvystatSR)
                    tkfocus(ttSvystatSR)
                    tkwm.title(ttSvystatSR,label_SvystatSR)
                    tkwm.resizable(ttSvystatSR, 0, 0)

                    frameTop <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameDown <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameOutput  <- tkframe(frameGlobal, borderwidth=0)
                    frameButtons <- tkframe(frameGlobal, borderwidth=0)


                    scrEsvydesignObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstEsvydesignObj,...))
                    lista <- tclVar()
                    tclObj(lista) <- listEsvydesignObj
                    lstEsvydesignObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="browse", yscrollcommand = function (...)tkset(scrEsvydesignObj,...), background = "white")

                    scrVariables<- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstVariables,...))
                    listaVariables <- tclVar()
                    lstVariables <- tklistbox(frameTop,height = 4, listvariable= listaVariables, selectmode="extended", yscrollcommand = function (...)tkset(scrVariables,...), background = "white")

                    lblfEsvydesignObj<- tk2labelframe(frameTop)

                    count_SvystatSR <<- FALSE
                    tkbind(lstEsvydesignObj, "<ButtonRelease-1>", function() fElencoCampiSvystatSR(lista, lstEsvydesignObj,
                            lblVariables, listaVariables, lstVariables, scrVariables, y.but, classes.but, by.but, y.ri.but, classes.ri.but, by.ri.but,
                            lblESvystatDesignObj, ok.but, lstY, lstClasses, lstBy, entry.ObjectName))

                    lblEsvydesignObj <- ttklabel(frameTop, text="Select a survey design object",font=fontTextLabel)
                    lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")

                    lblESvystatDesignObj <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarESvystatDesignObj)),foreground="red")


                    labellblfY <- tk2label(frameCentral,text="  y  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatSR", args = "y")
                    tk2tip(labellblfY,descfunz)

                    lblfY<- tk2labelframe(frameCentral, labelwidget= labellblfY)
                    frameY <- tkframe(lblfY, borderwidth=0)
                    frameY.but <- tkframe(lblfY, borderwidth=0)
                    scrY <- tkscrollbar(frameY, repeatinterval= 5, command = function(...) tkyview(lstY,...))
                    listaY <- tclVar()
                    lstY <- tklistbox(frameY, height=4, listvariable=listaY, selectmode="extended", yscrollcommand = function (...)tkset(scrY,...), background = "white")


                    labellblfClasses <- tk2label(frameCentral,text="  classes  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatSR", args = "classes")
                    tk2tip(labellblfClasses,descfunz)

                    lblfClasses<- tk2labelframe(frameCentral, labelwidget= labellblfClasses)
                    frameClasses <- tkframe(lblfClasses, borderwidth=0)
                    frameClasses.but <- tkframe(lblfClasses, borderwidth=0)

                    scrClasses <- tkscrollbar(frameClasses, repeatinterval= 5, command = function(...) tkyview(lstClasses,...))
                    listaClasses <- tclVar()
                    lstClasses <- tklistbox(frameClasses, height=4, listvariable=listaClasses, selectmode="extended", yscrollcommand = function (...)tkset(scrClasses,...), background = "white")


                    labellblfBy <- tk2label(frameDown,text="  by  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatSR", args = "by")
                    tk2tip(labellblfBy,descfunz)
                    lblfBy<- tk2labelframe(frameDown, labelwidget=labellblfBy)
                    frameBy <- tkframe(lblfBy, borderwidth=0)
                    frameBy.but <- tkframe(lblfBy, borderwidth=0)
                    scrBy <- tkscrollbar(frameBy, repeatinterval= 5, command = function(...) tkyview(lstBy,...))

                    listaBy <- tclVar()
                    lstBy <- tklistbox(frameBy, height=4, listvariable=listaBy, selectmode="extended", yscrollcommand = function (...)tkset(scrBy,...), background = "white")

                    y.but <- tk2button(frameY.but, image=image_sx, state= "disabled", command = function()fTransfer_ND(lstY, lstVariables, listaVariables))
                    y.ri.but <- tk2button(frameY.but, image=image_dx, state= "disabled", command = function()fCancel(lstY, lstVariables, listaY))

                    classes.but <- tk2button(frameClasses.but, image=image_sx, state= "disabled", command = function()fTransfer_ND (lstClasses, lstVariables, listaVariables))
                    classes.ri.but <- tk2button(frameClasses.but, image=image_dx, state= "disabled", command = function()fCancel(lstClasses, lstVariables, listaClasses))

                    by.but <- tk2button(frameBy.but, image=image_sx, state= "disabled", command = function()fTransfer(lstBy, lstVariables, listaVariables))
                    by.ri.but <- tk2button(frameBy.but, image=image_dx, state= "disabled", command = function()fDetransfer(lstBy, lstVariables, listaBy))

                    lblMandatory <- tk2label(frameGlobal,text="Mandatory fields", font=fontTextTitle, foreground= "blue")
                    lblOptional <-tk2label(frameGlobal,text="Optional fields", font=fontTextTitle, foreground= "blue")


                    labellblfDeff <- ttklabel(frameDown,text="  deff  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatTM", args = "deff")
                    tk2tip(labellblfDeff,descfunz)
                    lblfDeff<- ttklabelframe(frameDown, labelwidget=labellblfDeff)

                    rbDeffF <- ttkradiobutton(lblfDeff)
                    rbDeffT <- ttkradiobutton(lblfDeff)
                    rbDeffR <- ttkradiobutton(lblfDeff)

                    rbValueDeff <- tclVar("FALSE")

                    tkconfigure(rbDeffF,variable=rbValueDeff,value="FALSE")
                    tkconfigure(rbDeffT,variable=rbValueDeff,value="TRUE")
                    tkconfigure(rbDeffR,variable=rbValueDeff,value="replace")

                    labelDeffF <- ttklabel(lblfDeff,text="False ")
                    labelDeffT <- ttklabel(lblfDeff,text="True ")
                    labelDeffR <- ttklabel(lblfDeff,text="Replace")


                    labellblfVartype <- ttklabel(frameDown,text="  vartype  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatSR", args = "vartype")
                    tk2tip(labellblfVartype,descfunz)
                    lblfVartype<- ttklabelframe(frameDown, labelwidget=labellblfVartype)
                    vartype_name <- c("se", "cv", "cvpct", "var")
                    vartype_value <- c("1", "0", "0", "0")
                    checkbuttonWidget <- list()
                    checkbuttonVar <- list()

                    lvar <-length(vartype_name)

                    for (i in 1:lvar){
                        checkbuttonWidget[[i]] <- ttkcheckbutton(lblfVartype)
                        checkbuttonVar[[i]] <- tclVar(vartype_value[i])
                    }


                    labellblfConf.int <- ttklabel(frameDown,text="  conf.int  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatSR", args = "conf.int")
                    tk2tip(labellblfConf.int,descfunz)
                    lblfConf.int<- ttklabelframe(frameDown, labelwidget=labellblfConf.int)

                    rbConf.intF <- ttkradiobutton(lblfConf.int)
                    rbConf.intT <- ttkradiobutton(lblfConf.int)
                    rbValueConf.int <- tclVar("FALSE")

                    tkconfigure(rbConf.intF,variable=rbValueConf.int,value="FALSE")
                    tkconfigure(rbConf.intT,variable=rbValueConf.int,value="TRUE")

                    labelConf.intF <- ttklabel(lblfConf.int,text="False ")
                    labelConf.intT <- ttklabel(lblfConf.int,text="True ")


                    labellblfConf.lev <- ttklabel(frameDown,text="  conf.lev  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatSR", args = "conf.lev")
                    tk2tip(labellblfConf.lev,descfunz)
                    lblfConf.lev<- ttklabelframe(frameDown, labelwidget=labellblfConf.lev)

                    tkbind(rbConf.intT, "<ButtonPress>", function() {tkconfigure(s, state = "normal")})
                    tkbind(rbConf.intF, "<ButtonPress>", function() {tkconfigure(s, state = "disabled")})

                    s <- tk2spinbox(lblfConf.lev, from=0.00, to=1.00, increment=0.01, state="disabled", width="4", background= "white")

                    tkconfigure(s, textvariable=tclVar(0.95))


                    labellblfNa.rm <- ttklabel(frameDown,text="  na.rm  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatSR", args = "na.rm")
                    tk2tip(labellblfNa.rm,descfunz)
                    lblfNa.rm<- ttklabelframe(frameDown, labelwidget=labellblfNa.rm)

                    rbNa.rmF <- ttkradiobutton(lblfNa.rm)
                    rbNa.rmT <- ttkradiobutton(lblfNa.rm)
                    rbValueNa.rm <- tclVar("FALSE")

                    tkconfigure(rbNa.rmF,variable=rbValueNa.rm,value="FALSE")
                    tkconfigure(rbNa.rmT,variable=rbValueNa.rm,value="TRUE")

                    labelNa.rmF <- ttklabel(lblfNa.rm,text="False ")
                    labelNa.rmT <- ttklabel(lblfNa.rm,text="True ")


                    labellblfSvystatSRObj <- ttklabel(frameOutput,text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfSvystatSRObj<- ttklabelframe(frameOutput, labelwidget = labellblfSvystatSRObj)

                    ObjectName <- tclVar("")
                    entry.ObjectName <-ttkentry(lblfSvystatSRObj,width="20",text=ObjectName, state= "disabled", font="TkDefaultFont")

                    ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_SvystatSR(lstY, lstClasses, lstBy, rbValueDeff,
                              checkbuttonVar, lvar, vartype_name, rbValueConf.int, rbValueNa.rm, s, ObjectName,
                              ttSvystatSR))


                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttSvystatSR))

                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_SvystatSR), command=function() fOnFunctionHelp(label_SvystatSR))


                    tkgrid(tk2label (frameGlobal, text="Sample data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))
                    tkgrid(lblfEsvydesignObj)
                    tkgrid(lblEsvydesignObj, lblVariables)
                    tkgrid(lstEsvydesignObj, scrEsvydesignObj )

                    tkgrid.configure(lblEsvydesignObj, sticky ="e")
                    tkgrid.configure(lblVariables, column=4, sticky ="we")
                    tkgrid.configure(lblESvystatDesignObj, row=2, sticky="e")

                    tkgrid.configure(lstEsvydesignObj, row=3, sticky ="e", padx=c("1.5c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrEsvydesignObj, row=3, sticky ="nsw", padx=c(0,"2c"), pady=c(0,"0.2c"))

                    tkgrid(lblfY, lblfClasses)
                    tkgrid.configure(lblfY,padx=c("0.4c",0), pady=c(0,"0.2c"))
                    tkgrid.configure(lblfClasses,padx=c("2c","0.4c"), pady=c(0,"0.2c"))

                    tkgrid(frameY.but, frameY)
                    tkgrid.configure(frameY.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameY, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
                    tkgrid(y.but)
                    tkgrid(y.ri.but)
                    tkgrid(lstY, scrY)

                    tkgrid.configure(y.but, pady=c(0,"0.2c"))
                    tkgrid.configure(y.ri.but, pady=c("0.2c",0))
                    tkgrid.configure(scrY, sticky ="nsw")

                    tkgrid(frameClasses.but, frameClasses)
                    tkgrid.configure(frameClasses.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameClasses, padx= c(0,"0.1c"), pady=c(0,"0.3c"))

                    tkgrid(classes.but)
                    tkgrid(classes.ri.but)
                    tkgrid(lstClasses, scrClasses)

                    tkgrid.configure(classes.but, pady=c(0,"0.2c"))
                    tkgrid.configure(classes.ri.but, pady=c("0.2c",0))
                    tkgrid.configure(scrClasses, sticky ="nsw")

                    tkgrid(lblfBy,lblfDeff,lblfVartype, lblfConf.int, lblfConf.lev, lblfNa.rm)

                    tkgrid.configure(lblfBy,padx=c("0.4c","0.3c"), pady=c(0,"0.2c"))
                    tkgrid(frameBy.but, frameBy)
                    tkgrid.configure(frameBy.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameBy, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
                    tkgrid(by.but)
                    tkgrid(by.ri.but)
                    tkgrid(lstBy, scrBy)
                    tkgrid.configure(by.but, pady=c(0,"0.2c"))
                    tkgrid.configure(by.ri.but, pady=c("0.2c",0))
                    tkgrid.configure(scrBy, sticky ="nsw")

                    tkgrid(labelDeffF,rbDeffF)
                    tkgrid(labelDeffT,rbDeffT)
                    tkgrid(labelDeffR,rbDeffR)

                    tkgrid.configure(labelDeffF, sticky ="w", padx=c("0.3c",0))
                    tkgrid.configure(labelDeffT, sticky ="w", padx=c("0.3c",0))
                    tkgrid.configure(labelDeffR, sticky ="w", padx=c("0.3c",0))


                    for (i in 1:length(vartype_name)){
                        tkgrid(tklabel(lblfVartype,text=vartype_name[i]), checkbuttonWidget[[i]], sticky="w", padx=c("0.3c",0))
                        tkgrid(checkbuttonWidget[[i]] )
                        tkconfigure(checkbuttonWidget[[i]] ,variable=checkbuttonVar[[i]])
                    }

                    tkgrid.configure(lblfVartype,padx=c("0.3c","0.3c"))

                    tkgrid(labelConf.intF,rbConf.intF)
                    tkgrid(labelConf.intT,rbConf.intT)

                    tkgrid.configure(labelConf.intF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelConf.intT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfConf.int,padx=c(0,"0.3c"))

                    tkgrid.configure(s, padx=c("0.7c", 0), pady=c(0,"0.2c"))

                    tkgrid(labelNa.rmF,rbNa.rmF)
                    tkgrid(labelNa.rmT,rbNa.rmT)

                    tkgrid.configure(labelNa.rmF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelNa.rmT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfNa.rm,padx=c("0.3c","0.5c"))


                    tkgrid(frameTop)
                    tkgrid(lblMandatory, pady=c("0.2c",0))

                    tkgrid(frameCentral)
                    tkgrid(lblOptional, pady=c("0.2c",0))

                    tkgrid(frameDown)
                    tkgrid.configure(frameDown, padx="0.5c")

                    tkgrid(lblfSvystatSRObj)
                    tkgrid.configure(entry.ObjectName, padx=c("0.5c","7c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")
                    tkgrid.configure(frameButtons, sticky="ne")
                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)
                    tkgrid.configure(Cancel.but, padx=("0.5c"))
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))
                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttSvystatSR))) # To asses the size of the window...
                    # tkfocus(ttSvystatSR)
            }

            fElencoCampiSvystatSR <- function(EC_lista, EC_lstEsvydesignObj, EC_lblVariables, x, lstEC_EsvydesignObj,
                    scrEC, EC_y.but, EC_classes.but, EC_by.but, EC_y.ri.but, EC_classes.ri.but, EC_by.ri.but,
                    EC_lblESvystatDesignObj, EC_ok.but, EC_lstY, EC_lstClasses, EC_lstBy, EC_entry.ObjectName){

                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstEsvydesignObj)))

                    assignTemp("SvystatSR_Scelta_EsvydesignObj", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstEsvydesignObj))+1])


                    # DIRE A RAFFAELLA 16/07/10
                    # NO!!! Copying may become a serious concern when data get big
                    # (can tolerate it for names, because they are small: see above)
                    # assignTemp("SvystatSR_EsvydesignObj", get(SvystatSR_Scelta_EsvydesignObj, envir=.GlobalEnv))
                    SvystatSR_EsvydesignObj <- get(SvystatSR_Scelta_EsvydesignObj, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    VarESvystatDesignObj <<- tclVar(SvystatSR_Scelta_EsvydesignObj)
                    tkconfigure(EC_lblESvystatDesignObj, textvariable= VarESvystatDesignObj)

                    if (Index_dataframe_old_Svyby !=EC_indicesel){
                            count_SvystatSR <<- FALSE
                    }
                    if (count_SvystatSR == FALSE){
                        count_SvystatSR <<- TRUE
                        Index_dataframe_old_Svyby <<- EC_indicesel

                        tkdelete(EC_lstY, 0, "end")
                        tkdelete(EC_lstClasses, 0, "end")
                        tkdelete(EC_lstBy, 0, "end")
                        tkdelete(EC_entry.ObjectName, 0, "end")

# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                        for  (n in names(SvystatSR_EsvydesignObj$variables)){
#                                tclObj(x) <- c(names(SvystatSR_EsvydesignObj$variables))
#                        }
                        tclObj(x) <- names(SvystatSR_EsvydesignObj$variables)

                        tkgrid(lstEC_EsvydesignObj, scrEC)

                        tkgrid.configure(lstEC_EsvydesignObj, column=4, row=3, sticky ="e", pady=c(0,"0.2c"))
                        tkgrid.configure(scrEC, column=5, row=3, padx=c("0c","1.6c"), pady=c(0,"0.2c"), sticky ="nsw")
                        tkconfigure(EC_y.but, state = "normal")
                        tkconfigure(EC_classes.but, state = "normal")
                        tkconfigure(EC_by.but, state = "normal")

                        tkconfigure(EC_y.ri.but, state = "normal")
                        tkconfigure(EC_classes.ri.but, state = "normal")
                        tkconfigure(EC_by.ri.but, state = "normal")

                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_lblVariables, text="Variables", state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")
                    }
            }


            fOnRun_SvystatSR <- function(OR_lstY, OR_lstClasses, OR_lstBy, OR_rbValueDeff,
                                        OR_checkbuttonVar, OR_lvar, OR_vartype_name, OR_rbValueConf.int,
                                        OR_rbValueNa.rm, OR_s, OR_SvystatSRObjectName, ttSvystatSR){
                    campobb <- TRUE

                    all.lstY <- tclvalue(tkget(OR_lstY, 0, "end"))

                    if  (all.lstY =="") {
                        tkmessageBox(title="List y empty", message = "The list is empty. Transfer at least an element", icon = "error", parent = ttSvystatSR)
                        campobb <- FALSE
                    }
                    else {
                        ysum <- gsub(" ", "+",all.lstY)

                        y <- as.formula(paste("~", ysum), env = .GlobalEnv)
                        prnY <- paste("y=", ysum, sep =" ~ ")
                    }

                    all.lstClasses <- tclvalue(tkget(OR_lstClasses, 0, "end"))

                    if  (all.lstClasses =="") {
                        tkmessageBox(title="List classes empty", message = "The list is empty. Transfer at least an element", icon = "error", parent = ttSvystatSR)
                        campobb <- FALSE
                    }
                    else {
                        classessum <- gsub(" ", "+",all.lstClasses)

                        classes <- as.formula(paste("~", classessum), env = .GlobalEnv)
                        prnClasses <- paste("classes=", classessum, sep =" ~ ")
                    }

                    all.lstBy <- tclvalue(tkget(OR_lstBy, 0, "end"))

                    if  (all.lstBy =="") {
                        by <- NULL
                        prnBy <- "by= NULL"
                        }
                    else {
                        bysum <- gsub(" ", ":",all.lstBy)

                        by <- as.formula(paste("~", bysum), env = .GlobalEnv)
                        prnBy <- paste("by=", bysum, sep =" ~ ")
                    }

                    if ( (deff.tmp <-  tclvalue(OR_rbValueDeff))=="replace" ){
                        deff <- deff.tmp
                        prnDeff <- 'deff= "replace"'
                    }
                    else {
                         deff <- as.logical(deff.tmp)
                         if (!deff) {
                             prnDeff <- "deff= FALSE"
                         }
                         else {
                             prnDeff <- "deff= TRUE"
                         }
                    }

                    choices <- c()
                    j<- 1
                    for (i in 1:OR_lvar){
                          if(tclvalue(OR_checkbuttonVar[[i]]) == "1") {
                          choices[j] <- OR_vartype_name[i]
                          j <- j + 1
                        }
                    }

                    if ((j-1) == 0) {
                        prnVartype <- 'vartype= "se"'
                        vartype <- "se"
                    }
                    else {
                        vartype <- choices
                        if ((j-1) == 1) {
                           prnVartype <- paste('vartype= "', choices[1], '"', sep="")
                        }
                        else{
                            prnVartype <- "vartype= c("
                            for(i in 1:(j-1)){
                                prnVartype <- paste(prnVartype,'"',choices[i],'"', sep="")
                                if (i<(j-1)) {prnVartype <- paste(prnVartype,', ', sep="")}
                            }
                            prnVartype <- paste (prnVartype,  ')', sep = "")
                        }
                    }

                    conf.int<- as.logical(tclvalue(OR_rbValueConf.int))
                    if (!conf.int) {
                        prnConf.int <- "conf.int= FALSE"
                        conf.lev <- 0.95
                        prnConf.lev <- "conf.lev= 0.95"
                    }
                    else{
                        prnConf.int <- "conf.int= TRUE"
                        conf.lev <- tclvalue(tkget(OR_s))
                        if (conf.lev ==""){
                            tkmessageBox(title="conf.lev", message="The field is empty!",icon="error", parent = ttSvystatSR)
                            campobb <- FALSE
                        }
                        else{
                            conf.lev <- as.numeric(tkget(OR_s))
                            if (is.na(conf.lev)){
                                tkmessageBox(title="conf.lev", message="Please insert a numeric value",icon="error", parent = ttSvystatSR)
                                campobb <- FALSE
                            }
                            else{
                                prnConf.lev <- paste("conf.lev= ", conf.lev, sep="")
                            }
                        }
                    }

                    na.rm <- as.logical(tclvalue(OR_rbValueNa.rm))
                    if (!na.rm) {
                        prnNa.rm <- "na.rm= FALSE"
                    }
                    else{
                        prnNa.rm <- "na.rm= TRUE"
                    }


                    if (tclvalue(OR_SvystatSRObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = ttSvystatSR)
                        campobb <- FALSE
                    }
                    else{
                        if   ( !is.ok.name(tclvalue(OR_SvystatSRObjectName), go.on=campobb, parent= ttSvystatSR) ){
                              campobb <- FALSE
                             }
                        else {
                             OR_SvystatSRObjectName <- tclvalue(OR_SvystatSRObjectName)
                             }
                        }
                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(ttSvystatSR, cursor="watch")

                        # DIRE A RAFFAELLA 16/07/10
                        # SvystatSR_EsvydesignObj no more exists into TempEnv: we left inside its name only
                        SvystatSR_EsvydesignObj <- get(SvystatSR_Scelta_EsvydesignObj, envir=.GlobalEnv)
                        # DIRE A RAFFAELLA 16/07/10

                        outSvystatSR <- Lancia(svystatSR(SvystatSR_EsvydesignObj, y, classes, by, vartype,
                                                        conf.int, conf.lev, deff, na.rm), textWarnings, parent = ttSvystatSR)

                        if (!inherits(outSvystatSR,"try-error")) {
                            attr(outSvystatSR,"design") <- as.symbol(SvystatSR_Scelta_EsvydesignObj)
                            # assign(OR_SvystatSRObjectName, outSvystatSR, envir = .GlobalEnv)
                            assign2GE(OR_SvystatSRObjectName, outSvystatSR)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            if (!is.big(outSvystatSR)) {
                                cat(paste("# ",OR_SvystatSRObjectName,"\n",sep=""))
                            }
                            else {
                                cat(paste("# head(",OR_SvystatSRObjectName,")\n",sep=""))
                            }
                            printonscreen(outSvystatSR, OR_SvystatSRObjectName)
                            cat("\n")

                            prnDesign <- paste("design=",SvystatSR_Scelta_EsvydesignObj)

                            prnSvystatSR<- paste(" <- svystatSR(", prnDesign, ", ", prnY, ", ", prnClasses, ", ", prnBy,
                                        ", ",  prnVartype, ", ", prnConf.int,
                                        ", ", prnConf.lev, ", ", prnDeff, ", ", prnNa.rm, ")", sep="")

                            commands <- paste(OR_SvystatSRObjectName, prnSvystatSR, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                  tkmessageBox(title ="svystatSR",
                                               message = "Operation executed\nNote: Warnings have been generated!",
                                               icon = "info", parent = ttSvystatSR)
                                 }
                            else {
                                  tkmessageBox(title ="svystatSR",message = "Operation executed", icon = "info", parent = ttSvystatSR)
                                 }
                            tkgrab.release(ttSvystatSR)
                        }
                    # get back the standard arrow cursor
                    tkconfigure(ttSvystatSR, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# ----------------------------------
# < END building svystatSR window. >
# ----------------------------------

# -----------------------------------
# > START building svystatB window. >
# -----------------------------------

            fSvystatB <- function(){
                    listEsvydesignObj <-  mk.class.list("analytic")
                    ttSvystatB <- tktoplevel()
                    tcl("wm", "protocol", ttSvystatB, "WM_DELETE_WINDOW", function() fOnCancel(ttSvystatB))
                    frameGlobal<- tkframe(ttSvystatB, borderwidth= 2)
                    tkwm.deiconify(ttSvystatB)
                    tkgrab.set(ttSvystatB)
                    tkfocus(ttSvystatB)
                    tkwm.resizable(ttSvystatB, 0, 0)
                    tkwm.title(ttSvystatB,label_SvystatB)

                    frameTop     <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameDown    <- tkframe(frameGlobal, relief="groove", borderwidth=2, padx="0.1c", pady=c("0.2c","0.4c"))
                    frameOutput  <- tkframe(frameGlobal, borderwidth=0)
                    frameButtons <- tkframe(frameGlobal, borderwidth=0)

                    scrEsvydesignObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstEsvydesignObj,...))
                    lista <- tclVar()
                    tclObj(lista) <- listEsvydesignObj
                    lstEsvydesignObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="browse", yscrollcommand = function (...)tkset(scrEsvydesignObj,...), background = "white")

                    scrVariables <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstVariables,...))
                    listaVariables <- tclVar()
                    lstVariables <- tklistbox(frameTop,height = 4, listvariable= listaVariables, selectmode="extended", yscrollcommand = function (...)tkset(scrVariables,...), background = "white")

                    lblfEsvydesignObj<- tk2labelframe(frameTop)

                    count_SvystatB <<- FALSE
                    tkbind(lstEsvydesignObj, "<ButtonRelease-1>", function() fElencoCampiSvystatB(lista, lstEsvydesignObj,
                            lblVariables, listaVariables, lstVariables, scrVariables, Tilde.but, Plus.but, Times.but, Colon.but, Minus.but, Power.but, LeftParen.but, RightParen.but,
                            Collapse.but, textModel, lblESvystatDesignObj, ok.but, entry.ObjectName))


                    tkbind(lstVariables, "<Double-Button-1>", function() {
                                        assignTemp("SvystatB_Scelta_Variables", as.character(tclObj(listaVariables))[as.integer(tkcurselection(lstVariables))+1])
                                        # tkinsert(textModel, "end -1 chars", SvystatB_Scelta_Variables)
                                        # It's better to insert where cursor is, and keep the focus  i.e.:
                                        tkfocus(textModel)
                                        tkinsert(textModel, "insert", SvystatB_Scelta_Variables)
                                        })

                    lblEsvydesignObj <- ttklabel(frameTop, text="Select a survey design object",font=fontTextLabel)
                    lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")
                    lblESvystatDesignObj <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarESvystatDesignObj)),foreground="red")

                    labellblfModel <- tk2label(frameCentral,text="  model  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatB", args = "model")
                    tk2tip(labellblfModel,descfunz)

                    lblfModel<- tk2labelframe(frameCentral, labelwidget= labellblfModel)
                    OperatorsFrame <- tkframe(lblfModel)
                    frameModel <- tkframe(lblfModel)

                    scrtextModel <- tkscrollbar(frameModel, orient = "vertical", command=function(...) tkyview(textModel, ...))

                    textModel <- tktext(frameModel, foreground="red", background= "white", height=4, width=30, yscrollcommand=function(...) tkset(scrtextModel, ...),
                    wrap="word", state="disabled", font=fontTextExp)

                    Tilde.but <- tk2button(OperatorsFrame, text="~", width="3", state="disabled", command=function() fOnOperatorModel(textModel, " ~ "))
                    Plus.but <- tk2button(OperatorsFrame, text="+", width="3", state="disabled", command=function() fOnOperatorModel(textModel, " + "))
                    Times.but <- tk2button(OperatorsFrame, text="*", width="3", state="disabled", command=function() fOnOperatorModel(textModel, "*"))
                    Colon.but <- tk2button(OperatorsFrame, text=":", width="3", state="disabled", command=function() fOnOperatorModel(textModel, ":"))
                    Minus.but <- tk2button(OperatorsFrame, text="-", width="3", state="disabled", command=function() fOnOperatorModel(textModel, " - "))
                    Power.but <- tk2button(OperatorsFrame, text="^", width="3", state="disabled", command=function() fOnOperatorModel(textModel, "^"))
                    LeftParen.but <- tk2button(OperatorsFrame, text="(", width="3", state="disabled", command=function() fOnOperatorModel(textModel, "("))
                    RightParen.but <- tk2button(OperatorsFrame, text=")", width="3", state="disabled", command=function() fOnOperatorModel (textModel, ")"))
                    Collapse.but <- tk2button(OperatorsFrame, text="+...+", width="5", state="disabled", command=function() fOnCollapseOperatorModel(lstVariables, textModel, parent = ttSvystatB))

                    lblMandatory <- tk2label(frameGlobal,text="Mandatory Fields", font=fontTextTitle, foreground= "blue")
                    lblOptional <-tk2label(frameGlobal,text="Optional Fields", font=fontTextTitle, foreground= "blue")

                    labellblfDeff <- ttklabel(frameDown,text="  deff  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatB", args = "deff")
                    tk2tip(labellblfDeff,descfunz)
                    lblfDeff<- ttklabelframe(frameDown, labelwidget=labellblfDeff)

                    rbDeffF <- ttkradiobutton(lblfDeff)
                    rbDeffT <- ttkradiobutton(lblfDeff)
                    rbDeffR <- ttkradiobutton(lblfDeff)

                    rbValueDeff <- tclVar("FALSE")

                    tkconfigure(rbDeffF,variable=rbValueDeff,value="FALSE")
                    tkconfigure(rbDeffT,variable=rbValueDeff,value="TRUE")
                    tkconfigure(rbDeffR,variable=rbValueDeff,value="replace")

                    labelDeffF <- ttklabel(lblfDeff,text="False ")
                    labelDeffT <- ttklabel(lblfDeff,text="True ")
                    labelDeffR <- ttklabel(lblfDeff,text="Replace")


                    labellblfVartype <- ttklabel(frameDown,text="  vartype  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatB", args = "vartype")
                    tk2tip(labellblfVartype,descfunz)
                    lblfVartype<- ttklabelframe(frameDown, labelwidget=labellblfVartype)
                    vartype_name <- c("se", "cv", "cvpct", "var")
                    vartype_value <- c("1", "0", "0", "0")
                    checkbuttonWidget <- list()
                    checkbuttonVar <- list()

                    lvar <-length(vartype_name)

                    for (i in 1:lvar){
                        checkbuttonWidget[[i]] <- ttkcheckbutton(lblfVartype)
                        checkbuttonVar[[i]] <- tclVar(vartype_value[i])
                    }


                    labellblfConf.int <- ttklabel(frameDown,text="  conf.int  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatB", args = "conf.int")
                    tk2tip(labellblfConf.int,descfunz)
                    lblfConf.int<- ttklabelframe(frameDown, labelwidget=labellblfConf.int)

                    rbConf.intF <- ttkradiobutton(lblfConf.int)
                    rbConf.intT <- ttkradiobutton(lblfConf.int)
                    rbValueConf.int <- tclVar("FALSE")

                    tkconfigure(rbConf.intF,variable=rbValueConf.int,value="FALSE")
                    tkconfigure(rbConf.intT,variable=rbValueConf.int,value="TRUE")

                    labelConf.intF <- ttklabel(lblfConf.int,text="False ")
                    labelConf.intT <- ttklabel(lblfConf.int,text="True ")


                    labellblfConf.lev <- ttklabel(frameDown,text="  conf.lev  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatB", args = "conf.lev")
                    tk2tip(labellblfConf.lev,descfunz)
                    lblfConf.lev<- ttklabelframe(frameDown, labelwidget=labellblfConf.lev)

                    tkbind(rbConf.intT, "<ButtonPress>", function() {tkconfigure(s, state = "normal")})
                    tkbind(rbConf.intF, "<ButtonPress>", function() {tkconfigure(s, state = "disabled")})

                    s <- tk2spinbox(lblfConf.lev, from=0.00, to=1.00, increment=0.01, state="disabled", width="4", background= "white")

                    tkconfigure(s, textvariable=tclVar(0.95))

                    labellblfNa.rm <- ttklabel(frameDown,text="  na.rm  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatB", args = "na.rm")
                    tk2tip(labellblfNa.rm,descfunz)
                    lblfNa.rm<- ttklabelframe(frameDown, labelwidget=labellblfNa.rm)

                    rbNa.rmF <- ttkradiobutton(lblfNa.rm)
                    rbNa.rmT <- ttkradiobutton(lblfNa.rm)
                    rbValueNa.rm <- tclVar("FALSE")

                    tkconfigure(rbNa.rmF,variable=rbValueNa.rm,value="FALSE")
                    tkconfigure(rbNa.rmT,variable=rbValueNa.rm,value="TRUE")

                    labelNa.rmF <- ttklabel(lblfNa.rm,text="False ")
                    labelNa.rmT <- ttklabel(lblfNa.rm,text="True ")

                    labellblfSvystatBObj <- ttklabel(frameOutput, text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfSvystatBObj<- ttklabelframe(frameOutput, labelwidget = labellblfSvystatBObj)

                    ObjectName <- tclVar("")
                    entry.ObjectName <-ttkentry(lblfSvystatBObj, width="20", text=ObjectName, state= "disabled", font="TkDefaultFont")

                    ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_SvystatB(textModel, rbValueDeff, checkbuttonVar,
                              lvar, vartype_name, rbValueConf.int, rbValueNa.rm, s, ObjectName, ttSvystatB))


                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttSvystatB))

                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_SvystatB), command=function() fOnFunctionHelp(label_SvystatB))

                    tkgrid(tk2label (frameGlobal, text="Sample Data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))
                    tkgrid(lblfEsvydesignObj)
                    tkgrid(lblEsvydesignObj, lblVariables)
                    tkgrid(lstEsvydesignObj, scrEsvydesignObj )

                    tkgrid.configure(lblEsvydesignObj, column=1, sticky ="e")
                    tkgrid.configure(lblVariables, column=4)
                    tkgrid.configure(lblESvystatDesignObj, row=2, column=1, sticky ="e")

                    tkgrid.configure(lstEsvydesignObj, column=1, row=3, sticky ="e", padx=c("1.7c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrEsvydesignObj, column=2, row=3, sticky ="nsw", padx=c(0,"1.9c"), pady=c(0,"0.2c"))

                    tkgrid.configure(lblfModel,padx="1.3c")

                    tkgrid(Tilde.but, Plus.but, Times.but, Colon.but, Minus.but, Power.but, LeftParen.but, RightParen.but, Collapse.but, sticky ="e")

                    tkgrid.configure(Tilde.but, padx=c("0.4c",0))
                    tkgrid.configure(Collapse.but, padx=c(0,"0.4c"))

                    tkgrid(textModel,scrtextModel)
                    tkgrid.configure(scrtextModel, sticky ="nsw")
                    tkgrid(tklabel(lblfModel, text= "       "))
                    # Enable ctrl-c and ctrl-v on textModel
                    ctrl.cv(textModel)

                    tkgrid(lblfDeff, lblfVartype, lblfConf.int, lblfConf.lev, lblfNa.rm)

                    tkgrid.configure(lblfDeff, padx=c("0.4c",0), pady=c(0,"0.2c"))
                    # tkgrid(frameBy.but, frameBy)
                    # tkgrid.configure(frameBy.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    # tkgrid.configure(frameBy, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
                    # tkgrid(by.but)
                    # tkgrid(by.ri.but)
                    # tkgrid(lstBy, scrBy)
                    # tkgrid.configure(by.but, pady=c(0,"0.2c"))
                    # tkgrid.configure(by.ri.but, pady=c("0.2c",0))
                    # tkgrid.configure(scrBy, sticky ="nsw")

                    tkgrid(labelDeffF,rbDeffF)
                    tkgrid(labelDeffT,rbDeffT)
                    tkgrid(labelDeffR,rbDeffR)

                    tkgrid.configure(labelDeffF, sticky ="w", padx=c("0.3c",0))
                    tkgrid.configure(labelDeffT, sticky ="w", padx=c("0.3c",0))
                    tkgrid.configure(labelDeffR, sticky ="w", padx=c("0.3c",0))

                    for (i in 1:length(vartype_name)){
                        tkgrid(tklabel(lblfVartype,text=vartype_name[i]), checkbuttonWidget[[i]], sticky="w", padx=c("0.3c",0))
                        tkgrid(checkbuttonWidget[[i]] )
                        tkconfigure(checkbuttonWidget[[i]] ,variable=checkbuttonVar[[i]])
                    }

                    tkgrid.configure(lblfVartype,padx=c(10,10))

                    tkgrid(labelConf.intF,rbConf.intF)
                    tkgrid(labelConf.intT,rbConf.intT)

                    tkgrid.configure(labelConf.intF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelConf.intT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfConf.int,padx=c(0,10))

                    tkgrid.configure(s, padx=c("0.7c", 0), pady=c(0,"0.2c"))

                    tkgrid(labelNa.rmF,rbNa.rmF)
                    tkgrid(labelNa.rmT,rbNa.rmT)

                    tkgrid.configure(labelNa.rmF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelNa.rmT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfNa.rm,padx=c("0.3c","0.4c"))

                    tkgrid(frameTop)
                    tkgrid(lblMandatory, pady=c("0.2c",0))

                    tkgrid(frameCentral)

                    tkgrid(OperatorsFrame)

                    tkgrid(frameModel)
                    tkgrid.configure(frameModel,pady=c(20,20))

                    tkgrid(lblOptional, pady=c("0.2c",0))
                    tkgrid(frameDown)
                    tkgrid.configure(frameDown, padx="0.5c")

                    tkgrid(lblfSvystatBObj)
                    tkgrid.configure(entry.ObjectName, padx=c("0.5c","7c"),pady=c(0,"0.3c"))
                    tkgrid(frameOutput)
                    tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")
                    tkgrid(frameButtons)
                    tkgrid.configure(frameButtons, sticky="ne")

                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)

                    tkgrid.configure(Cancel.but, padx=("0.5c"))
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))

                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttSvystatB))) # To asses the size of the window...
                    # tkfocus(ttSvystatB)
            }

            fOnOperatorModel <- function(O_textModel, O_Operator){
                        # tkinsert(O_textModel, "end -1 chars", O_Operator)
                        # It's better to insert where cursor is, and keep the focus  i.e.:
                        tkfocus(O_textModel)
                        tkinsert(O_textModel, "insert", O_Operator)
            }

            fOnCollapseOperatorModel <- function(O_lstVariables, O_textModel, parent="."){
                    Vars <- as.character(tkget(O_lstVariables, 0, "end"))
                    selection <- 1L + as.integer(tkcurselection(O_lstVariables))
                    sleng <- length(selection)
                    selVars <- Vars[selection]
                    if (sleng==0){
                        tkmessageBox(title="Empty Select List", message = "Please select at least one element", icon = "error", parent = parent)
                    }
                    else{
                        lstVariablessum <- paste(selVars, collapse = " + ")
                        fOnOperatorModel(O_textModel, lstVariablessum)
                    }
            }

            fElencoCampiSvystatB <- function(EC_lista, EC_lstEsvydesignObj, EC_lblVariables, x, lstEC_EsvydesignObj,
                    scrEC, EC_Tilde.but, EC_Plus.but, EC_Times.but, EC_Colon.but, EC_Minus.but, EC_Power.but, EC_LeftParen.but, EC_RightParen.but,
                    EC_Collapse.but, EC_textModel,
                    EC_lblESvystatDesignObj, EC_ok.but, EC_entry.ObjectName){

                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstEsvydesignObj)))

                    assignTemp("SvystatB_Scelta_EsvydesignObj", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstEsvydesignObj))+1])

                    # DIRE A RAFFAELLA 16/07/10
                    # NO!!! Copying may become a serious concern when data get big
                    # (can tolerate it for names, because they are small: see above)
                    # assignTemp("SvystatB_EsvydesignObj", get(SvystatB_Scelta_EsvydesignObj, envir=.GlobalEnv))
                    SvystatB_EsvydesignObj <- get(SvystatB_Scelta_EsvydesignObj, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    VarESvystatDesignObj <<- tclVar(SvystatB_Scelta_EsvydesignObj)
                    tkconfigure(EC_lblESvystatDesignObj, textvariable= VarESvystatDesignObj)

                    if (Index_dataframe_old_Svyby !=EC_indicesel){
                            count_SvystatB <<- FALSE
                    }
                    if (count_SvystatB == FALSE){
                        count_SvystatB <<- TRUE
                        Index_dataframe_old_Svyby <<- EC_indicesel

                        tkdelete(EC_textModel, "1.0", "end")
                        tkdelete(EC_entry.ObjectName, 0, "end")

# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                        for  (n in names(SvystatB_EsvydesignObj$variables)){
#                                tclObj(x) <- c(names(SvystatB_EsvydesignObj$variables))
#                        }
                        tclObj(x) <- names(SvystatB_EsvydesignObj$variables)

                        tkgrid(lstEC_EsvydesignObj, scrEC)

                        tkgrid.configure(lstEC_EsvydesignObj, column=4, row=3, pady=c(0,"0.2c"), sticky ="e")
                        tkgrid.configure(scrEC, column=5, row=3, padx=c("0c","1.7c"), pady=c(0,"0.2c"), sticky ="nsw")

                        tkconfigure(EC_Tilde.but, state = "normal")
                        tkconfigure(EC_Plus.but, state = "normal")
                        tkconfigure(EC_Times.but, state = "normal")
                        tkconfigure(EC_Colon.but, state = "normal")
                        tkconfigure(EC_Minus.but, state = "normal")
                        tkconfigure(EC_Power.but, state = "normal")
                        tkconfigure(EC_LeftParen.but, state = "normal")
                        tkconfigure(EC_RightParen.but, state = "normal")
                        tkconfigure(EC_Collapse.but, state = "normal")

                        tkconfigure(EC_textModel, state = "normal")

                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_lblVariables, text="Variables", state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")
                    }
            }

            fOnRun_SvystatB <- function(OR_textModel, OR_rbValueDeff,
                    OR_checkbuttonVar, OR_lvar, OR_vartype_name, OR_rbValueConf.int,
                    OR_rbValueNa.rm, OR_s, OR_SvystatBObjectName, ttSvystatB){

                    campobb <- TRUE

                    all.textModel <- tclvalue(tkget(OR_textModel, "1.0", "end -1 chars"))

                    if  (all.textModel =="") {
                        tkmessageBox(title="Model is empty", message = "The field is empty. Transfer at least an element", icon = "error", parent = ttSvystatB)
                        campobb <- FALSE
                    }
                    else {
                        model <- Lancia(as.formula(all.textModel, env = .GlobalEnv), textWarnings, parent = ttSvystatB)
                        if (inherits(model,"try-error"))
                            campobb <- FALSE
                        else
                        prnModel <- paste("model= ", twosideform.to.char(model), sep ="")
                    }


                    if ( (deff.tmp <-  tclvalue(OR_rbValueDeff))=="replace" ){
                        deff <- deff.tmp
                        prnDeff <- 'deff= "replace"'
                    }
                    else {
                         deff <- as.logical(deff.tmp)
                         if (!deff) {
                             prnDeff <- "deff= FALSE"
                         }
                         else {
                             prnDeff <- "deff= TRUE"
                         }
                    }

                    choices <- c()
                    j<- 1
                    for (i in 1:OR_lvar){
                          if(tclvalue(OR_checkbuttonVar[[i]]) == "1") {
                          choices[j] <- OR_vartype_name[i]
                          j <- j + 1
                        }
                    }

                    if ((j-1) == 0) {
                        prnVartype <- 'vartype= "se"'
                        vartype <- "se"
                    }
                    else {
                        vartype <- choices
                        if ((j-1) == 1) {
                           prnVartype <- paste('vartype= "', choices[1], '"', sep="")
                        }
                        else{
                            prnVartype <- "vartype= c("
                            for(i in 1:(j-1)){
                                prnVartype <- paste(prnVartype,'"', choices[i],'"', sep="")
                                if (i<(j-1)) {prnVartype <- paste(prnVartype,', ',sep="")}
                            }
                            prnVartype <- paste (prnVartype,  ')', sep = "")
                        }
                    }

                    conf.int<- as.logical(tclvalue(OR_rbValueConf.int))
                    if (!conf.int) {
                        prnConf.int <- "conf.int= FALSE"
                        conf.lev <- 0.95
                        prnConf.lev <- "conf.lev= 0.95"
                    }
                    else{
                        prnConf.int <- "conf.int= TRUE"
                        conf.lev <- tclvalue(tkget(OR_s))
                        if (conf.lev ==""){
                            tkmessageBox(title="conf.lev", message="The field is empty!",icon="error", parent = ttSvystatB)
                            campobb <- FALSE
                        }
                        else{
                            conf.lev <- as.numeric(tkget(OR_s))
                            if (is.na(conf.lev)){
                                tkmessageBox(title="conf.lev", message="Please insert a numeric value",icon="error", parent = ttSvystatB)
                                campobb <- FALSE
                            }
                            else{
                                prnConf.lev <- paste("conf.lev= ", conf.lev, sep="")
                            }
                        }
                    }

                    na.rm <- as.logical(tclvalue(OR_rbValueNa.rm))
                    if (!na.rm) {
                        prnNa.rm <- "na.rm= FALSE"
                    }
                    else{
                        prnNa.rm <- "na.rm= TRUE"
                    }

                    if (tclvalue(OR_SvystatBObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = ttSvystatB)
                        campobb <- FALSE
                    }
                    else{
                        if   ( !is.ok.name(tclvalue(OR_SvystatBObjectName), go.on=campobb, parent= ttSvystatB) ){
                             campobb <- FALSE
                             }
                        else {
                             OR_SvystatBObjectName <- tclvalue(OR_SvystatBObjectName)
                             }
                        }

                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(ttSvystatB, cursor="watch")

                        # DIRE A RAFFAELLA 16/07/10
                        # SvystatB_EsvydesignObj no more exists into TempEnv: we left inside its name only
                        SvystatB_EsvydesignObj <- get(SvystatB_Scelta_EsvydesignObj, envir=.GlobalEnv)
                        # DIRE A RAFFAELLA 16/07/10

                        outSvystatB <- Lancia(svystatB(SvystatB_EsvydesignObj, model, vartype,
                                                            conf.int, conf.lev, deff, na.rm), textWarnings, parent = ttSvystatB)

                        if (!inherits(outSvystatB,"try-error")) {
                            attr(outSvystatB,"design") <- as.symbol(SvystatB_Scelta_EsvydesignObj)
                            # assign(OR_SvystatBObjectName, outSvystatB, envir = .GlobalEnv)
                            assign2GE(OR_SvystatBObjectName, outSvystatB)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            if (!is.big(outSvystatB)) {
                                cat(paste("# ",OR_SvystatBObjectName,"\n",sep=""))
                            }
                            else {
                                cat(paste("# head(",OR_SvystatBObjectName,")\n",sep=""))
                            }
                            printonscreen(outSvystatB, OR_SvystatBObjectName)
                            cat("\n")

                            prnDesign <- paste("design=", SvystatB_Scelta_EsvydesignObj)

                            prnSvystatB<- paste(" <- svystatB(",    prnDesign, ", ", prnModel,
                                        ", ", prnVartype, ", ", prnConf.int, ", ", prnConf.lev,
                                        ", ", prnDeff, ", ", prnNa.rm, ")", sep="")


                            commands <- paste(OR_SvystatBObjectName, prnSvystatB, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                  tkmessageBox(title ="svystatB",
                                               message = "Operation executed\nNote: Warnings have been generated!",
                                               icon = "info", parent = ttSvystatB)
                                 }
                            else {
                                  tkmessageBox(title ="svystatB",message = "Operation executed", icon = "info", parent = ttSvystatB)
                                 }
                            tkgrab.release(ttSvystatB)
                        }
                    # get back the standard arrow cursor
                    tkconfigure(ttSvystatB, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# ---------------------------------
# < END building svystatB window. <
# ---------------------------------

# -----------------------------------
# > START building svystatQ window. >
# -----------------------------------

            fSvystatQ <- function(){
                    listEsvydesignObj <-  mk.class.list("analytic")
                    ttSvystatQ <- tktoplevel()
                    tcl("wm", "protocol", ttSvystatQ, "WM_DELETE_WINDOW", function() fOnCancel(ttSvystatQ))
                    frameGlobal<- tkframe(ttSvystatQ, borderwidth= 2)
                    tkwm.deiconify(ttSvystatQ)
                    tkgrab.set(ttSvystatQ)
                    tkfocus(ttSvystatQ)
                    tkwm.title(ttSvystatQ,label_SvystatQ)
                    tkwm.resizable(ttSvystatQ, 0, 0)

                    frameTop     <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameDown    <- tkframe(frameGlobal, relief="groove", borderwidth=2, padx="0.1c", pady=c("0.2c","0.4c"))
                    frameOutput  <- tkframe(frameGlobal, borderwidth=0)
                    frameButtons <- tkframe(frameGlobal, borderwidth=0)

                    scrEsvydesignObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstEsvydesignObj,...))
                    lista <- tclVar()
                    tclObj(lista) <- listEsvydesignObj
                    lstEsvydesignObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="browse", yscrollcommand = function (...)tkset(scrEsvydesignObj,...), background = "white")

                    scrVariables<- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstVariables,...))
                    listaVariables <- tclVar()
                    lstVariables <- tklistbox(frameTop,height = 4, listvariable= listaVariables, selectmode="browse", yscrollcommand = function (...)tkset(scrVariables,...), background = "white")

                    lblfEsvydesignObj<- tk2labelframe(frameTop)

                    count_SvystatQ <<- FALSE
                    tkbind(lstEsvydesignObj, "<ButtonRelease-1>", function() fElencoCampiSvystatQ(lista, lstEsvydesignObj,
                            lblVariables, listaVariables, lstVariables, scrVariables, y.but, by.but, y.ri.but, by.ri.but,
                            lblESvystatDesignObj, ok.but, lstY, lstBy, entry.ObjectName))

                    lblEsvydesignObj <- ttklabel(frameTop, text="Select a survey design object",font=fontTextLabel)
                    lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")
                    lblESvystatDesignObj <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarESvystatDesignObj)),foreground="red")

                    labellblfY <- tk2label(frameCentral,text="  y  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatQ", args = "y")
                    tk2tip(labellblfY,descfunz)

                    lblfY<- tk2labelframe(frameCentral, labelwidget= labellblfY)
                    frameY <- tkframe(lblfY, borderwidth=0)
                    frameY.but <- tkframe(lblfY, borderwidth=0)
                    scrY <- tkscrollbar(frameY, repeatinterval= 5, command = function(...) tkyview(lstY,...))

                    listaY <- tclVar()
                    lstY <- tklistbox(frameY, height=4, listvariable=listaY, selectmode="extended", yscrollcommand = function (...)tkset(scrY,...), background = "white")

                    labellblfProbs <- ttklabel(frameCentral,text="  probs  ", font=fontTextLabel, image=image_qm, compound="right")
                                        descfunz <- descArgs("svystatQ", args = "probs")
                    tk2tip(labellblfProbs,descfunz)
                    lblfProbs<- ttklabelframe(frameCentral, labelwidget=labellblfProbs)


                    probs_name <- c("Median", "Quartiles", "Deciles", "Percentiles", "Other")
                    n_probs_value <-list(0.5, c(0.25, 0.50, 0.75),  seq(0.1, 0.9, by=0.1), seq(0.01, 0.99, by=0.01))
                    probs_value <-list("0.5", "c(0.25, 0.50, 0.75)",  "seq(0.1, 0.9, by=0.1)", "seq(0.01, 0.99, by=0.01)")
                    lvar_probs <-length(probs_name)
                    radiobuttonWidget <- list()
                    radiobuttonVar <- list()

                    for (i in 1:lvar_probs){
                        radiobuttonWidget[[i]] <- ttkradiobutton(lblfProbs)
                    }

                    labelSpecify <- ttklabel(lblfProbs, text="Specify:")
                    entry.Specify <- tkentry(lblfProbs, state="disabled", width="20", background="white")


                    rbValueProbs <- tclVar("Quartiles")
                    for (i in 1:lvar_probs){
                        tkconfigure(radiobuttonWidget[[i]],variable=rbValueProbs,value = probs_name[i])
                    }

                    tkbind(radiobuttonWidget[[1]], "<ButtonPress>", function() {tkdelete(entry.Specify, 0, "end")
                                                                                tkconfigure(entry.Specify, state = "disabled")})
                    tkbind(radiobuttonWidget[[2]], "<ButtonPress>", function() {tkdelete(entry.Specify, 0, "end")
                                                                                tkconfigure(entry.Specify, state = "disabled")})
                    tkbind(radiobuttonWidget[[3]], "<ButtonPress>", function() {tkdelete(entry.Specify, 0, "end")
                                                                                tkconfigure(entry.Specify, state = "disabled")})
                    tkbind(radiobuttonWidget[[4]], "<ButtonPress>", function() {tkdelete(entry.Specify, 0, "end")
                                                                                tkconfigure(entry.Specify, state = "disabled")})
                    tkbind(radiobuttonWidget[[5]], "<ButtonPress>", function() {tkdelete(entry.Specify, 0, "end")
                                                                                tkconfigure(entry.Specify, state = "normal")})


                    labellblfBy <- tk2label(frameDown,text="  by  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatQ", args = "by")
                    tk2tip(labellblfBy,descfunz)
                    lblfBy<- tk2labelframe(frameDown, labelwidget=labellblfBy)
                    frameBy <- tkframe(lblfBy, borderwidth=0)
                    frameBy.but <- tkframe(lblfBy, borderwidth=0)
                    scrBy <- tkscrollbar(frameBy, repeatinterval= 5, command = function(...) tkyview(lstBy,...))

                    listaBy <- tclVar()
                    lstBy <- tklistbox(frameBy, height=4, listvariable=listaBy, selectmode="extended", yscrollcommand = function (...)tkset(scrBy,...), background = "white")

                    y.but <- tk2button(frameY.but, image=image_sx, state= "disabled", command = function()fTransfer(lstY, lstVariables, listaVariables))
                    y.ri.but <- tk2button(frameY.but, image=image_dx, state= "disabled", command = function()fDetransfer(lstY, lstVariables, listaY))

                    by.but <- tk2button(frameBy.but, image=image_sx, state= "disabled", command = function()fTransfer(lstBy, lstVariables, listaVariables))
                    by.ri.but <- tk2button(frameBy.but, image=image_dx, state= "disabled", command = function()fDetransfer(lstBy, lstVariables, listaBy))

                    labellblfTies <- ttklabel(frameDown,text="  ties  ", font=fontTextLabel, image=image_qm, compound="right")
                                        descfunz <- descArgs("svystatQ", args = "ties")
                                        tk2tip(labellblfTies,descfunz)
                                        lblfTies<- ttklabelframe(frameDown, labelwidget=labellblfTies)

                    rbTiesD <- ttkradiobutton(lblfTies)
                    rbTiesR <- ttkradiobutton(lblfTies)
                    rbValueTies <- tclVar("discrete")

                    tkconfigure(rbTiesD,variable=rbValueTies,value="discrete")
                    tkconfigure(rbTiesR,variable=rbValueTies,value="rounded")

                    labelTiesD <- ttklabel(lblfTies,text="discrete ")
                    labelTiesR <- ttklabel(lblfTies,text="rounded ")


                    labellblfVartype <- ttklabel(frameDown,text="  vartype  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatQ", args = "vartype")
                    tk2tip(labellblfVartype,descfunz)
                    lblfVartype<- ttklabelframe(frameDown, labelwidget=labellblfVartype)
                    vartype_name <- c("se", "cv", "cvpct", "var")
                    vartype_value <- c("1", "0", "0", "0")
                    checkbuttonWidget <- list()
                    checkbuttonVar <- list()

                    lvar <-length(vartype_name)

                    for (i in 1:lvar){
                        checkbuttonWidget[[i]] <- ttkcheckbutton(lblfVartype)
                        checkbuttonVar[[i]] <- tclVar(vartype_value[i])
                    }


                    labellblfConf.lev <- ttklabel(frameDown,text="  conf.lev  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatQ", args = "conf.lev")
                    tk2tip(labellblfConf.lev,descfunz)
                    lblfConf.lev<- ttklabelframe(frameDown, labelwidget=labellblfConf.lev)


                    sConf.lev = tk2spinbox(lblfConf.lev, from=0.00, to=1.00, increment=0.01, state="normal", width="4", background= "white")
                    tkconfigure(sConf.lev, textvariable=tclVar(0.95))

                    labellblfNa.rm <- ttklabel(frameDown,text="  na.rm  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatQ", args = "na.rm")
                    tk2tip(labellblfNa.rm,descfunz)
                    lblfNa.rm<- ttklabelframe(frameDown, labelwidget=labellblfNa.rm)

                    rbNa.rmF <- ttkradiobutton(lblfNa.rm)
                    rbNa.rmT <- ttkradiobutton(lblfNa.rm)
                    rbValueNa.rm <- tclVar("FALSE")

                    tkconfigure(rbNa.rmF,variable=rbValueNa.rm,value="FALSE")
                    tkconfigure(rbNa.rmT,variable=rbValueNa.rm,value="TRUE")

                    labelNa.rmF <- ttklabel(lblfNa.rm,text="False ")
                    labelNa.rmT <- ttklabel(lblfNa.rm,text="True ")


                    labellblfSvystatQObj <- ttklabel(frameOutput,text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfSvystatQObj<- ttklabelframe(frameOutput, labelwidget = labellblfSvystatQObj)

                    ObjectName <- tclVar("")
                    entry.ObjectName <-ttkentry(lblfSvystatQObj,width="20",text=ObjectName, state= "disabled", font="TkDefaultFont")

                    ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_SvystatQ(lstY, rbValueProbs, probs_value, probs_name, n_probs_value, entry.Specify, lstBy, rbValueTies, checkbuttonVar,
                              lvar, vartype_name, sConf.lev, rbValueNa.rm, ObjectName, ttSvystatQ))


                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttSvystatQ))

                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_SvystatQ), command=function() fOnFunctionHelp(label_SvystatQ))


                    tkgrid(tk2label (frameGlobal, text="Sample Data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))
                    tkgrid(lblfEsvydesignObj)
                    tkgrid(lblEsvydesignObj, lblVariables)
                    tkgrid(lstEsvydesignObj, scrEsvydesignObj )

                    tkgrid.configure(lblEsvydesignObj, column=1, sticky ="e")
                    tkgrid.configure(lblVariables, column=4)
                    tkgrid.configure(lblESvystatDesignObj, row=2, column=1, sticky ="e")

                    tkgrid.configure(lstEsvydesignObj, column=1, row=3, sticky ="e", padx=c("1.7c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrEsvydesignObj, column=2, row=3, sticky ="nsw", padx=c(0,"1.9c"), pady=c(0,"0.2c"))

                    tkgrid(lblfY, lblfProbs)
                    tkgrid.configure(lblfY, padx="0.5c", pady=c(0,"0.2c"))
                    tkgrid.configure(lblfProbs,padx=c(0,"0.5c"))

                    tkgrid(frameY.but, frameY)
                    tkgrid.configure(frameY.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameY, padx= c(0,"0.1c"), pady=c(0,"0.3c"))

                    tkgrid(y.but)
                    tkgrid(y.ri.but)
                    tkgrid(lstY, scrY)

                    tkgrid.configure(y.but, pady=c(0,"0.2c"))
                    tkgrid.configure(y.ri.but, pady=c("0.2c",0))
                    tkgrid.configure(scrY, sticky ="nsw")

                    for (i in 1:length(probs_name)){
                        # Here layout in pixels: should translate in cm (which is our standard)...
                        tkgrid(ttklabel(lblfProbs,text=probs_name[i]), radiobuttonWidget[[i]], sticky="w", padx=c(15,0))
                    }

                    tkgrid.configure(labelSpecify, row=4, column=2)
                    tkgrid.configure(entry.Specify, row=4, column=3, padx=10, pady=c(0,"0.1c"))

                    tkgrid(lblfBy, lblfTies, lblfVartype, lblfConf.lev, lblfNa.rm)

                    tkgrid.configure(lblfBy,padx=c("0.4c","0.3c"), pady=c(0,"0.2c"))
                    tkgrid(frameBy.but, frameBy)
                    tkgrid.configure(frameBy.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameBy, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
                    tkgrid(by.but)
                    tkgrid(by.ri.but)
                    tkgrid(lstBy, scrBy)
                    tkgrid.configure(by.but, pady=c(0,"0.2c"))
                    tkgrid.configure(by.ri.but, pady=c("0.2c",0))
                    tkgrid.configure(scrBy, sticky ="nsw")

                    tkgrid(labelTiesD,rbTiesD)
                    tkgrid(labelTiesR,rbTiesR)
                    tkgrid.configure(labelTiesD, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelTiesR, sticky="w", padx=c("0.3c",0))


                    for (i in 1:length(vartype_name)){
                        # Here layout in pixels: should translate in cm (which is our standard)...
                        tkgrid(tklabel(lblfVartype,text=vartype_name[i]), checkbuttonWidget[[i]], sticky="w", padx=c(15,0))
                        tkgrid(checkbuttonWidget[[i]] )
                        tkconfigure(checkbuttonWidget[[i]] ,variable=checkbuttonVar[[i]])
                    }

                    tkgrid.configure(lblfVartype,padx="0.3c")

                    tkgrid.configure(sConf.lev, padx=c("0.7c", 0), pady=c(0,"0.2c"))

                    tkgrid(labelNa.rmF,rbNa.rmF)
                    tkgrid(labelNa.rmT,rbNa.rmT)

                    tkgrid.configure(labelNa.rmF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelNa.rmT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfNa.rm,padx=c("0.3c","0.4c"))

                    tkgrid(frameTop)
                    tkgrid(tk2label(frameGlobal, text="Mandatory Fields", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))

                    tkgrid(frameCentral)
                    tkgrid(tk2label(frameGlobal, text="Optional Fields", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))

                    tkgrid(frameDown)
                    tkgrid.configure(frameDown, padx="0.5c")

                    tkgrid(lblfSvystatQObj)
                    tkgrid.configure(entry.ObjectName, padx=c("0.5c","4c"),pady=c(0,"0.3c"))
                    tkgrid(frameOutput)
                    tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")
                    tkgrid.configure(frameButtons, sticky="ne")

                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)

                    tkgrid.configure(Cancel.but, padx=("0.5c"))
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))

                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttSvystatQ))) # To asses the size of the window...
                    # tkfocus(ttSvystatQ)
            }

                    fElencoCampiSvystatQ <- function(EC_lista, EC_lstEsvydesignObj, EC_lblVariables, x, lstEC_EsvydesignObj,
                    scrEC, EC_y.but, EC_by.but, EC_y.ri.but, EC_by.ri.but, EC_lblESvystatDesignObj, EC_ok.but, EC_lstY,
                    EC_lstBy, EC_entry.ObjectName){

                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstEsvydesignObj)))


                    assignTemp("SvystatQ_Scelta_EsvydesignObj", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstEsvydesignObj))+1])

                    # DIRE A RAFFAELLA 16/07/10
                    # NO!!! Copying may become a serious concern when data get big
                    # (can tolerate it for names, because they are small: see above)
                    # assignTemp("SvystatQ_EsvydesignObj", get(SvystatQ_Scelta_EsvydesignObj, envir=.GlobalEnv))
                    SvystatQ_EsvydesignObj <- get(SvystatQ_Scelta_EsvydesignObj, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    VarESvystatDesignObj <<- tclVar(SvystatQ_Scelta_EsvydesignObj)
                    tkconfigure(EC_lblESvystatDesignObj, textvariable= VarESvystatDesignObj)

                    if (Index_dataframe_old_Svyby !=EC_indicesel){
                            count_SvystatQ <<- FALSE
                    }
                    if (count_SvystatQ == FALSE){
                        count_SvystatQ <<- TRUE
                        Index_dataframe_old_Svyby <<- EC_indicesel

                        tkdelete(EC_lstY, 0, "end")
                        tkdelete(EC_lstBy, 0, "end")
                        tkdelete(EC_entry.ObjectName, 0, "end")

# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                        for  (n in names(SvystatQ_EsvydesignObj$variables)){
#                                tclObj(x) <- c(names(SvystatQ_EsvydesignObj$variables))
#                        }
                        tclObj(x) <- names(SvystatQ_EsvydesignObj$variables)

                        tkgrid(lstEC_EsvydesignObj, scrEC)

                        tkgrid.configure(lstEC_EsvydesignObj, column=4, row=3, pady=c(0,"0.2c"), sticky ="e")
                        tkgrid.configure(scrEC, column=5, row=3, padx=c("0c","1.7c"), pady=c(0,"0.2c"), sticky ="nsw")
                        tkconfigure(EC_y.but, state = "normal")
                        tkconfigure(EC_by.but, state = "normal")

                        tkconfigure(EC_y.ri.but, state = "normal")
                        tkconfigure(EC_by.ri.but, state = "normal")

                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_lblVariables, text="Variables", state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")
                    }
            }
            fOnRun_SvystatQ <- function(OR_lstY, OR_rbValueProbs, OR_probs_value, OR_probs_name, OR_n_probs_value, OR_entry.Specify, OR_lstBy, OR_rbValueTies,
                    OR_checkbuttonVar, OR_lvar, OR_vartype_name, OR_sConf.lev,
                    OR_rbValueNa.rm, OR_SvystatQObjectName, OR_ttSvystatQ){

                    campobb <- TRUE

                    all.lstY <- tclvalue(tkget(OR_lstY, 0, "end"))

                    if  (all.lstY =="") {
                        tkmessageBox(title="List y empty", message = "The list is empty. Transfer one element", icon = "error", parent = OR_ttSvystatQ)
                        campobb <- FALSE
                    }
                    else {
                        ysum <- gsub(" ", "+",all.lstY)

                        y <- as.formula(paste("~", ysum), env = .GlobalEnv)
                        prnY <- paste("y=", ysum, sep =" ~ ")
                    }


                    probs <- tclvalue(OR_rbValueProbs)

                    if (probs == OR_probs_name[1]){
                        probs <- OR_n_probs_value[[1]]
                        prnProbs <- paste("probs= ", OR_probs_value[1], sep="")
                    }
                    else if (probs == OR_probs_name[2]){
                        probs <- OR_n_probs_value[[2]]
                        prnProbs <- paste("probs= ", OR_probs_value[2], sep="")
                    }
                    else if (probs == OR_probs_name[3]){
                        probs <- OR_n_probs_value[[3]]
                        prnProbs <- paste("probs= ", OR_probs_value[3], sep="")
                    }
                    else if (probs == OR_probs_name[4]){
                        probs <- OR_n_probs_value[[4]]
                        prnProbs <- paste("probs= ", OR_probs_value[4], sep="")
                    }
                    else if (probs == OR_probs_name[5]){
                        probs <- tclvalue(tkget(OR_entry.Specify))
                        if (probs ==""){
                            tkmessageBox(title="probs", message="The field is empty!",icon="error", parent = OR_ttSvystatQ)
                            campobb <- FALSE
                        }
                        else{
                            probs.in <- probs
                            probs.txt <- paste("as.numeric(",probs,")",sep="")
                            probs <- Lancia(eval(parse(text=probs.txt)), textWarnings, parent = OR_ttSvystatQ)
                            if (!inherits(probs,"try-error")){
                                if (any(is.na(probs))){
                                    tkmessageBox(title="probs", message="Please insert a numeric value",icon="error", parent = OR_ttSvystatQ)
                                    campobb <- FALSE
                                }
                                else{
                                     prnProbs <- paste("probs= ", probs.in, sep="")
                                }
                            }
                            else campobb <- FALSE
                        }
                    }


                    all.lstBy <- tclvalue(tkget(OR_lstBy, 0, "end"))

                    if  (all.lstBy =="") {
                        by <- NULL
                        prnBy <- "by= NULL"
                        }
                    else {
                        bysum <- gsub(" ", ":",all.lstBy)

                        by <- as.formula(paste("~", bysum), env = .GlobalEnv)
                        prnBy <- paste("by=", bysum, sep =" ~ ")
                    }


                    ties <- tclvalue(OR_rbValueTies)

                    if (ties=="discrete") {
                        prnTies <- 'ties= "discrete"'
                    }
                    else{
                        prnTies <- 'ties= "rounded"'
                    }


                    choices <- c()
                    j<- 1
                    for (i in 1:OR_lvar){
                          if(tclvalue(OR_checkbuttonVar[[i]]) == "1") {
                          choices[j] <- OR_vartype_name[i]
                          j <- j + 1
                        }
                    }

                    if ((j-1) == 0) {
                        prnVartype <- 'vartype="se"'
                        vartype <- "se"
                    }
                    else {
                        vartype <- choices
                        if ((j-1) == 1) {
                           prnVartype <- paste('vartype="', choices[1], '"', sep="")
                        }
                        else{
                            prnVartype <- "vartype=c("
                            for(i in 1:(j-1)){
                                prnVartype <- paste(prnVartype,'"',choices[i],'"', sep="")
                                if (i<(j-1)) {prnVartype <- paste(prnVartype,',')}
                            }
                            prnVartype <- paste (prnVartype,  ')')
                        }
                    }



                    conf.lev <- tclvalue(tkget(OR_sConf.lev))
                    if (conf.lev ==""){
                        tkmessageBox(title="conf.lev", message="The field is empty!",icon="error", parent = OR_ttSvystatQ)
                        campobb <- FALSE
                    }
                    else{
                        conf.lev <- as.numeric(tkget(OR_sConf.lev))
                        if (is.na(conf.lev)){
                            tkmessageBox(title="conf.lev", message="Please insert a numeric value",icon="error", parent = OR_ttSvystatQ)
                            campobb <- FALSE
                        }
                        else{
                            prnConf.lev <- paste("conf.lev= ", conf.lev, sep="")
                        }
                    }


                    na.rm <- as.logical(tclvalue(OR_rbValueNa.rm))
                    if (!na.rm) {
                        prnNa.rm <- "na.rm= FALSE"
                    }
                    else{
                        prnNa.rm <- "na.rm= TRUE"
                    }

                    if (tclvalue(OR_SvystatQObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = OR_ttSvystatQ)
                        campobb <- FALSE
                    }
                    else{
                        if   ( !is.ok.name(tclvalue(OR_SvystatQObjectName), go.on=campobb, parent= OR_ttSvystatQ) ){
                              campobb <- FALSE
                             }
                        else {
                             OR_SvystatQObjectName <- tclvalue(OR_SvystatQObjectName)
                             }
                        }

                    if (campobb == TRUE){
                                # change the cursor to the hourglass to tell work is in progress...
                                tkconfigure(OR_ttSvystatQ, cursor="watch")

                                # DIRE A RAFFAELLA 16/07/10
                                # SvystatQ_EsvydesignObj no more exists into TempEnv: we left inside its name only
                                SvystatQ_EsvydesignObj <- get(SvystatQ_Scelta_EsvydesignObj, envir=.GlobalEnv)
                                # DIRE A RAFFAELLA 16/07/10

                                outSvystatQ <- Lancia(svystatQ(SvystatQ_EsvydesignObj, y, probs, by, vartype,
                                             conf.lev, na.rm, ties), textWarnings, parent = OR_ttSvystatQ)

                        if (!inherits(outSvystatQ,"try-error")) {
                            attr(outSvystatQ,"design") <- as.symbol(SvystatQ_Scelta_EsvydesignObj)
                            # assign(OR_SvystatQObjectName, outSvystatQ, envir = .GlobalEnv)
                            assign2GE(OR_SvystatQObjectName, outSvystatQ)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            if (!is.big(outSvystatQ)) {
                                cat(paste("# ",OR_SvystatQObjectName,"\n",sep=""))
                            }
                            else {
                                cat(paste("# head(",OR_SvystatQObjectName,")\n",sep=""))
                            }
                            printonscreen(outSvystatQ, OR_SvystatQObjectName)
                            cat("\n")

                            prnDesign <- paste("design=",SvystatQ_Scelta_EsvydesignObj)

                            prnSvystatQ<- paste(" <- svystatQ(",    prnDesign, ", ", prnY, ", ", prnProbs,
                                        ", ", prnBy, ", ",    prnVartype, ", ", prnConf.lev,
                                        ", ", prnNa.rm, ", ", prnTies, ")", sep="")

                            commands <- paste(OR_SvystatQObjectName, prnSvystatQ, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                tkmessageBox(title ="svystatQ",
                                             message = "Operation executed\nNote: Warnings have been generated!",
                                             icon = "info", parent = OR_ttSvystatQ)
                                }
                            else {
                                  tkmessageBox(title ="svystatQ",message = "Operation executed", icon = "info", parent = OR_ttSvystatQ)
                                }
                            tkgrab.release(OR_ttSvystatQ)
                        }
                    # get back the standard arrow cursor
                    tkconfigure(OR_ttSvystatQ, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# ---------------------------------
# < END building svystatQ window. <
# ---------------------------------

# -----------------------------------
# > START building svystatL window. >
# -----------------------------------

            fSvystatL <- function(){
                    listEsvydesignObj <-  mk.class.list("analytic")
                    ttSvystatL <- tktoplevel()
                    tcl("wm", "protocol", ttSvystatL, "WM_DELETE_WINDOW", function() fOnCancel(ttSvystatL))
                    frameGlobal<- tkframe(ttSvystatL, borderwidth= 2)
                    tkwm.deiconify(ttSvystatL)
                    tkgrab.set(ttSvystatL)
                    tkfocus(ttSvystatL)
                    tkwm.resizable(ttSvystatL, 0, 0)
                    tkwm.title(ttSvystatL,label_SvystatL)

                    frameTop     <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameDown    <- tkframe(frameGlobal, relief="groove", borderwidth=2, padx="0.1c", pady=c("0.2c","0.4c"))
                    frameOutput  <- tkframe(frameGlobal, borderwidth=0)
                    frameButtons <- tkframe(frameGlobal, borderwidth=0)

                    scrEsvydesignObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstEsvydesignObj,...))
                    lista <- tclVar()
                    tclObj(lista) <- listEsvydesignObj
                    lstEsvydesignObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="browse", yscrollcommand = function (...)tkset(scrEsvydesignObj,...), background = "white")

                    scrVariables <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstVariables,...))
                    listaVariables <- tclVar()
                    lstVariables <- tklistbox(frameTop,height = 4, listvariable= listaVariables, selectmode="extended", yscrollcommand = function (...)tkset(scrVariables,...), background = "white")

                    lblfEsvydesignObj<- tk2labelframe(frameTop)

                    count_SvystatL <<- FALSE
                    tkbind(lstEsvydesignObj, "<ButtonRelease-1>", function() fElencoCampiSvystatL(lista, lstEsvydesignObj,
                            lblVariables, listaVariables, lstVariables, scrVariables, Plus.but, Times.but, Colon.but, Minus.but, Power.but, LeftParen.but, RightParen.but,
                            Ones.but, Collapse.but, textExpr, by.but, by.ri.but, lblESvystatDesignObj, ok.but, lstBy, entry.ObjectName))


                    tkbind(lstVariables, "<Double-Button-1>", function() {
                                        assignTemp("SvystatL_Scelta_Variables", as.character(tclObj(listaVariables))[as.integer(tkcurselection(lstVariables))+1])
                                        # tkinsert(textExpr, "end -1 chars", SvystatL_Scelta_Variables)
                                        # It's better to insert where cursor is, and keep the focus  i.e.:
                                        tkfocus(textExpr)
                                        tkinsert(textExpr, "insert", SvystatL_Scelta_Variables)
                                        })

                    lblEsvydesignObj <- ttklabel(frameTop, text="Select a survey design object",font=fontTextLabel)
                    lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")
                    lblESvystatDesignObj <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarESvystatDesignObj)),foreground="red")

                    labellblfExpr <- tk2label(frameCentral,text="  expr  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatL", args = "expr")
                    tk2tip(labellblfExpr,descfunz)

                    lblfExpr<- tk2labelframe(frameCentral, labelwidget= labellblfExpr)
                    OperatorsFrame <- tkframe(lblfExpr)
                    frameExpr <- tkframe(lblfExpr)

                    scrtextExpr <- tkscrollbar(frameExpr, orient = "vertical", command=function(...) tkyview(textExpr, ...))

                    textExpr <- tktext(frameExpr, foreground="red", background= "white", height=4, width=30, yscrollcommand=function(...) tkset(scrtextExpr, ...),
                    wrap="word", state="disabled", font=fontTextExp)

                    Plus.but <- tk2button(OperatorsFrame, text="+", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, " + "))
                    Times.but <- tk2button(OperatorsFrame, text="*", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, "*"))
                    Colon.but <- tk2button(OperatorsFrame, text="/", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, "/"))
                    Minus.but <- tk2button(OperatorsFrame, text="-", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, " - "))
                    Power.but <- tk2button(OperatorsFrame, text="^", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, "^"))
                    LeftParen.but <- tk2button(OperatorsFrame, text="(", width="3", state="disabled", command=function() fOnOperatorExpr(textExpr, "("))
                    RightParen.but <- tk2button(OperatorsFrame, text=")", width="3", state="disabled", command=function() fOnOperatorExpr (textExpr, ")"))
                    Ones.but <- tk2button(OperatorsFrame, text="ones", width="5", state="disabled", command=function() fOnOperatorExpr (textExpr, "ones"))
                    Collapse.but <- tk2button(OperatorsFrame, text="+...+", width="5", state="disabled", command=function() fOnCollapseOperatorExpr(lstVariables, textExpr, parent = ttSvystatL))

                    labellblfBy <- tk2label(frameDown,text="  by  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatL", args = "by")
                    tk2tip(labellblfBy,descfunz)
                    lblfBy<- tk2labelframe(frameDown, labelwidget=labellblfBy)
                    frameBy <- tkframe(lblfBy, borderwidth=0)
                    frameBy.but <- tkframe(lblfBy, borderwidth=0)
                    scrBy <- tkscrollbar(frameBy, repeatinterval= 5, command = function(...) tkyview(lstBy,...))

                    listaBy <- tclVar()
                    lstBy <- tklistbox(frameBy, height=4, listvariable=listaBy, selectmode="extended", yscrollcommand = function (...)tkset(scrBy,...), background = "white")
                    by.but <- tk2button(frameBy.but,image=image_sx, state= "disabled", command = function()fTransfer(lstBy, lstVariables, listaVariables))
                    by.ri.but <- tk2button(frameBy.but, image=image_dx, state= "disabled", command = function()fDetransfer(lstBy, lstVariables, listaBy))

                    lblMandatory <- tk2label(frameGlobal,text="Mandatory Fields", font=fontTextTitle, foreground= "blue")
                    lblOptional <-tk2label(frameGlobal,text="Optional Fields", font=fontTextTitle, foreground= "blue")

                    labellblfDeff <- ttklabel(frameDown,text="  deff  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatL", args = "deff")
                    tk2tip(labellblfDeff,descfunz)
                    lblfDeff<- ttklabelframe(frameDown, labelwidget=labellblfDeff)

                    rbDeffF <- ttkradiobutton(lblfDeff)
                    rbDeffT <- ttkradiobutton(lblfDeff)
                    rbDeffR <- ttkradiobutton(lblfDeff)

                    rbValueDeff <- tclVar("FALSE")

                    tkconfigure(rbDeffF,variable=rbValueDeff,value="FALSE")
                    tkconfigure(rbDeffT,variable=rbValueDeff,value="TRUE")
                    tkconfigure(rbDeffR,variable=rbValueDeff,value="replace")

                    labelDeffF <- ttklabel(lblfDeff,text="False ")
                    labelDeffT <- ttklabel(lblfDeff,text="True ")
                    labelDeffR <- ttklabel(lblfDeff,text="Replace")


                    labellblfVartype <- ttklabel(frameDown,text="  vartype  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatL", args = "vartype")
                    tk2tip(labellblfVartype,descfunz)
                    lblfVartype<- ttklabelframe(frameDown, labelwidget=labellblfVartype)
                    vartype_name <- c("se", "cv", "cvpct", "var")
                    vartype_value <- c("1", "0", "0", "0")
                    checkbuttonWidget <- list()
                    checkbuttonVar <- list()

                    lvar <-length(vartype_name)

                    for (i in 1:lvar){
                        checkbuttonWidget[[i]] <- ttkcheckbutton(lblfVartype)
                        checkbuttonVar[[i]] <- tclVar(vartype_value[i])
                    }


                    labellblfConf.int <- ttklabel(frameDown,text="  conf.int  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatL", args = "conf.int")
                    tk2tip(labellblfConf.int,descfunz)
                    lblfConf.int<- ttklabelframe(frameDown, labelwidget=labellblfConf.int)

                    rbConf.intF <- ttkradiobutton(lblfConf.int)
                    rbConf.intT <- ttkradiobutton(lblfConf.int)
                    rbValueConf.int <- tclVar("FALSE")

                    tkconfigure(rbConf.intF,variable=rbValueConf.int,value="FALSE")
                    tkconfigure(rbConf.intT,variable=rbValueConf.int,value="TRUE")

                    labelConf.intF <- ttklabel(lblfConf.int,text="False ")
                    labelConf.intT <- ttklabel(lblfConf.int,text="True ")


                    labellblfConf.lev <- ttklabel(frameDown,text="  conf.lev  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatL", args = "conf.lev")
                    tk2tip(labellblfConf.lev,descfunz)
                    lblfConf.lev<- ttklabelframe(frameDown, labelwidget=labellblfConf.lev)

                    tkbind(rbConf.intT, "<ButtonPress>", function() {tkconfigure(s, state = "normal")})
                    tkbind(rbConf.intF, "<ButtonPress>", function() {tkconfigure(s, state = "disabled")})

                    s <- tk2spinbox(lblfConf.lev, from=0.00, to=1.00, increment=0.01, state="disabled", width="4", background= "white")

                    tkconfigure(s, textvariable=tclVar(0.95))

                    labellblfNa.rm <- ttklabel(frameDown,text="  na.rm  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("svystatL", args = "na.rm")
                    tk2tip(labellblfNa.rm,descfunz)
                    lblfNa.rm<- ttklabelframe(frameDown, labelwidget=labellblfNa.rm)

                    rbNa.rmF <- ttkradiobutton(lblfNa.rm)
                    rbNa.rmT <- ttkradiobutton(lblfNa.rm)
                    rbValueNa.rm <- tclVar("FALSE")

                    tkconfigure(rbNa.rmF,variable=rbValueNa.rm,value="FALSE")
                    tkconfigure(rbNa.rmT,variable=rbValueNa.rm,value="TRUE")

                    labelNa.rmF <- ttklabel(lblfNa.rm,text="False ")
                    labelNa.rmT <- ttklabel(lblfNa.rm,text="True ")

                    labellblfSvystatLObj <- ttklabel(frameOutput, text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfSvystatLObj<- ttklabelframe(frameOutput, labelwidget = labellblfSvystatLObj)

                    ObjectName <- tclVar("")
                    entry.ObjectName <-ttkentry(lblfSvystatLObj, width="20", text=ObjectName, state= "disabled", font="TkDefaultFont")

                    ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_SvystatL(textExpr, lstBy, rbValueDeff, checkbuttonVar,
                              lvar, vartype_name, rbValueConf.int, rbValueNa.rm, s, ObjectName, ttSvystatL))


                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttSvystatL))

                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_SvystatL), command=function() fOnFunctionHelp(label_SvystatL))

                    tkgrid(tk2label (frameGlobal, text="Sample Data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))
                    tkgrid(lblfEsvydesignObj)
                    tkgrid(lblEsvydesignObj, lblVariables)
                    tkgrid(lstEsvydesignObj, scrEsvydesignObj )

                    tkgrid.configure(lblEsvydesignObj, column=1, sticky ="e")
                    tkgrid.configure(lblVariables, column=4)
                    tkgrid.configure(lblESvystatDesignObj, row=2, column=1, sticky ="e")

                    tkgrid.configure(lstEsvydesignObj, column=1, row=3, sticky ="e", padx=c("1.7c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrEsvydesignObj, column=2, row=3, sticky ="nsw", padx=c(0,"1.9c"), pady=c(0,"0.2c"))

                    tkgrid.configure(lblfExpr,padx="1.3c")

                    tkgrid(Plus.but, Times.but, Colon.but, Minus.but, Power.but, LeftParen.but, RightParen.but, Ones.but, Collapse.but, sticky ="e")

                    tkgrid.configure(Plus.but, padx=c("0.4c",0))
                    tkgrid.configure(Collapse.but, padx=c(0,"0.4c"))

                    tkgrid(textExpr,scrtextExpr)
                    tkgrid.configure(scrtextExpr, sticky ="nsw")
                    tkgrid(tklabel(lblfExpr, text= "       "))
                    # Enable ctrl-c and ctrl-v on textExpr
                    ctrl.cv(textExpr)

                    tkgrid(lblfBy,lblfDeff,lblfVartype, lblfConf.int, lblfConf.lev, lblfNa.rm)

                    tkgrid.configure(lblfBy,padx=c("0.4c","0.3c"), pady=c(0,"0.2c"))
                    tkgrid(frameBy.but, frameBy)
                    tkgrid.configure(frameBy.but, padx= c("0.1c"), pady=c(0,"0.3c"))
                    tkgrid.configure(frameBy, padx= c(0,"0.1c"), pady=c(0,"0.3c"))
                    tkgrid(by.but)
                    tkgrid(by.ri.but)
                    tkgrid(lstBy, scrBy)
                    tkgrid.configure(by.but, pady=c(0,"0.2c"))
                    tkgrid.configure(by.ri.but, pady=c("0.2c",0))
                    tkgrid.configure(scrBy, sticky ="nsw")

                    tkgrid(labelDeffF,rbDeffF)
                    tkgrid(labelDeffT,rbDeffT)
                    tkgrid(labelDeffR,rbDeffR)

                    tkgrid.configure(labelDeffF, sticky ="w", padx=c("0.3c",0))
                    tkgrid.configure(labelDeffT, sticky ="w", padx=c("0.3c",0))
                    tkgrid.configure(labelDeffR, sticky ="w", padx=c("0.3c",0))

                    for (i in 1:length(vartype_name)){
                        tkgrid(tklabel(lblfVartype,text=vartype_name[i]), checkbuttonWidget[[i]], sticky="w", padx=c("0.3c",0))
                        tkgrid(checkbuttonWidget[[i]] )
                        tkconfigure(checkbuttonWidget[[i]] ,variable=checkbuttonVar[[i]])
                    }

                    tkgrid.configure(lblfVartype,padx=c(10,10))

                    tkgrid(labelConf.intF,rbConf.intF)
                    tkgrid(labelConf.intT,rbConf.intT)

                    tkgrid.configure(labelConf.intF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelConf.intT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfConf.int,padx=c(0,10))

                    tkgrid.configure(s, padx=c("0.7c", 0), pady=c(0,"0.2c"))

                    tkgrid(labelNa.rmF,rbNa.rmF)
                    tkgrid(labelNa.rmT,rbNa.rmT)

                    tkgrid.configure(labelNa.rmF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelNa.rmT, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfNa.rm,padx=c("0.3c","0.4c"))

                    tkgrid(frameTop)
                    tkgrid(lblMandatory, pady=c("0.2c",0))

                    tkgrid(frameCentral)

                    tkgrid(OperatorsFrame)

                    tkgrid(frameExpr)
                    tkgrid.configure(frameExpr,pady=c(20,20))

                    tkgrid(lblOptional, pady=c("0.2c",0))
                    tkgrid(frameDown)
                    tkgrid.configure(frameDown, padx="0.5c")

                    tkgrid(lblfSvystatLObj)
                    tkgrid.configure(entry.ObjectName, padx=c("0.5c","7c"),pady=c(0,"0.3c"))
                    tkgrid(frameOutput)
                    tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")
                    tkgrid(frameButtons)
                    tkgrid.configure(frameButtons, sticky="ne")

                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)

                    tkgrid.configure(Cancel.but, padx=("0.5c"))
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))

                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttSvystatL))) # To asses the size of the window...
                    # tkfocus(ttSvystatL)
            }

            fOnOperatorExpr <- function(O_textExpr, O_Operator){
                        # tkinsert(O_textExpr, "end -1 chars", O_Operator)
                        # It's better to insert where cursor is, and keep the focus  i.e.:
                        tkfocus(O_textExpr)
                        tkinsert(O_textExpr, "insert", O_Operator)
            }

            fOnCollapseOperatorExpr <- function(O_lstVariables, O_textExpr, parent="."){
                    Vars <- as.character(tkget(O_lstVariables, 0, "end"))
                    selection <- 1L + as.integer(tkcurselection(O_lstVariables))
                    sleng <- length(selection)
                    selVars <- Vars[selection]
                    if (sleng==0){
                        tkmessageBox(title="Empty Select List", message = "Please select at least one element", icon = "error", parent = parent)
                    }
                    else{
                        lstVariablessum <- paste(selVars, collapse = " + ")
                        fOnOperatorExpr(O_textExpr, lstVariablessum)
                    }
            }

            fElencoCampiSvystatL <- function(EC_lista, EC_lstEsvydesignObj, EC_lblVariables, x, lstEC_EsvydesignObj,
                    scrEC, EC_Plus.but, EC_Times.but, EC_Colon.but, EC_Minus.but, EC_Power.but, EC_LeftParen.but, EC_RightParen.but,
                    EC_Ones.but, EC_Collapse.but, EC_textExpr, EC_by.but, EC_by.ri.but,
                    EC_lblESvystatDesignObj, EC_ok.but, EC_lstBy, EC_entry.ObjectName){

                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstEsvydesignObj)))

                    assignTemp("SvystatL_Scelta_EsvydesignObj", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstEsvydesignObj))+1])

                    # DIRE A RAFFAELLA 16/07/10
                    # NO!!! Copying may become a serious concern when data get big
                    # (can tolerate it for names, because they are small: see above)
                    # assignTemp("SvystatL_EsvydesignObj", get(SvystatL_Scelta_EsvydesignObj, envir=.GlobalEnv))
                    SvystatL_EsvydesignObj <- get(SvystatL_Scelta_EsvydesignObj, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    VarESvystatDesignObj <<- tclVar(SvystatL_Scelta_EsvydesignObj)
                    tkconfigure(EC_lblESvystatDesignObj, textvariable= VarESvystatDesignObj)

                    if (Index_dataframe_old_Svyby !=EC_indicesel){
                            count_SvystatL <<- FALSE
                    }
                    if (count_SvystatL == FALSE){
                        count_SvystatL <<- TRUE
                        Index_dataframe_old_Svyby <<- EC_indicesel

                        tkdelete(EC_textExpr, "1.0", "end")
                        tkdelete(EC_lstBy, 0, "end")
                        tkdelete(EC_entry.ObjectName, 0, "end")

# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                        for  (n in names(SvystatL_EsvydesignObj$variables)){
#                                tclObj(x) <- c(names(SvystatL_EsvydesignObj$variables))
#                        }
                        tclObj(x) <- names(SvystatL_EsvydesignObj$variables)

                        tkgrid(lstEC_EsvydesignObj, scrEC)

                        tkgrid.configure(lstEC_EsvydesignObj, column=4, row=3, pady=c(0,"0.2c"), sticky ="e")
                        tkgrid.configure(scrEC, column=5, row=3, padx=c("0c","1.7c"), pady=c(0,"0.2c"), sticky ="nsw")

                        tkconfigure(EC_Plus.but, state = "normal")
                        tkconfigure(EC_Times.but, state = "normal")
                        tkconfigure(EC_Colon.but, state = "normal")
                        tkconfigure(EC_Minus.but, state = "normal")
                        tkconfigure(EC_Power.but, state = "normal")
                        tkconfigure(EC_LeftParen.but, state = "normal")
                        tkconfigure(EC_RightParen.but, state = "normal")
                        tkconfigure(EC_Ones.but, state = "normal")
                        tkconfigure(EC_Collapse.but, state = "normal")

                        tkconfigure(EC_textExpr, state = "normal")

                        tkconfigure(EC_by.but, state = "normal")
                        tkconfigure(EC_by.ri.but, state = "normal")

                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_lblVariables, text="Variables", state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")
                    }
            }

            fOnRun_SvystatL <- function(OR_textExpr, OR_lstBy, OR_rbValueDeff,
                    OR_checkbuttonVar, OR_lvar, OR_vartype_name, OR_rbValueConf.int,
                    OR_rbValueNa.rm, OR_s, OR_SvystatLObjectName, ttSvystatL){

                    campobb <- TRUE

                    all.textExpr <- tclvalue(tkget(OR_textExpr, "1.0", "end -1 chars"))

                    if  (all.textExpr =="") {
                        tkmessageBox(title="Expression is empty", message = "The field is empty. Transfer at least an element", icon = "error", parent = ttSvystatL)
                        campobb <- FALSE
                    }
                    else {
                        expr <- Lancia(parse(text=all.textExpr), textWarnings, parent = ttSvystatL)
                        if (inherits(expr,"try-error"))
                            campobb <- FALSE
                        else
                        prnExpr <- paste("expression(", gsub(" ", "", all.textExpr), ")", sep ="")
                    }


                    all.lstBy <- tclvalue(tkget(OR_lstBy, 0, "end"))

                    if  (all.lstBy =="") {
                        by <- NULL
                        prnBy <- "by= NULL"
                        }
                    else {
                        bysum <- gsub(" ", ":",all.lstBy)

                        by <- as.formula(paste("~", bysum), env = .GlobalEnv)
                        prnBy <- paste("by=", bysum, sep =" ~ ")
                    }


                    if ( (deff.tmp <-  tclvalue(OR_rbValueDeff))=="replace" ){
                        deff <- deff.tmp
                        prnDeff <- 'deff= "replace"'
                    }
                    else {
                         deff <- as.logical(deff.tmp)
                         if (!deff) {
                             prnDeff <- "deff= FALSE"
                         }
                         else {
                             prnDeff <- "deff= TRUE"
                         }
                    }

                    choices <- c()
                    j<- 1
                    for (i in 1:OR_lvar){
                          if(tclvalue(OR_checkbuttonVar[[i]]) == "1") {
                          choices[j] <- OR_vartype_name[i]
                          j <- j + 1
                        }
                    }

                    if ((j-1) == 0) {
                        prnVartype <- 'vartype= "se"'
                        vartype <- "se"
                    }
                    else {
                        vartype <- choices
                        if ((j-1) == 1) {
                           prnVartype <- paste('vartype= "', choices[1], '"', sep="")
                        }
                        else{
                            prnVartype <- "vartype= c("
                            for(i in 1:(j-1)){
                                prnVartype <- paste(prnVartype,'"', choices[i],'"', sep="")
                                if (i<(j-1)) {prnVartype <- paste(prnVartype,', ',sep="")}
                            }
                            prnVartype <- paste (prnVartype,  ')', sep = "")
                        }
                    }

                    conf.int<- as.logical(tclvalue(OR_rbValueConf.int))
                    if (!conf.int) {
                        prnConf.int <- "conf.int= FALSE"
                        conf.lev <- 0.95
                        prnConf.lev <- "conf.lev= 0.95"
                    }
                    else{
                        prnConf.int <- "conf.int= TRUE"
                        conf.lev <- tclvalue(tkget(OR_s))
                        if (conf.lev ==""){
                            tkmessageBox(title="conf.lev", message="The field is empty!",icon="error", parent = ttSvystatL)
                            campobb <- FALSE
                        }
                        else{
                            conf.lev <- as.numeric(tkget(OR_s))
                            if (is.na(conf.lev)){
                                tkmessageBox(title="conf.lev", message="Please insert a numeric value",icon="error", parent = ttSvystatL)
                                campobb <- FALSE
                            }
                            else{
                                prnConf.lev <- paste("conf.lev= ", conf.lev, sep="")
                            }
                        }
                    }

                    na.rm <- as.logical(tclvalue(OR_rbValueNa.rm))
                    if (!na.rm) {
                        prnNa.rm <- "na.rm= FALSE"
                    }
                    else{
                        prnNa.rm <- "na.rm= TRUE"
                    }

                    if (tclvalue(OR_SvystatLObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = ttSvystatL)
                        campobb <- FALSE
                    }
                    else{
                        if   ( !is.ok.name(tclvalue(OR_SvystatLObjectName), go.on=campobb, parent = ttSvystatL) ){
                             campobb <- FALSE
                             }
                        else {
                             OR_SvystatLObjectName <- tclvalue(OR_SvystatLObjectName)
                             }
                        }

                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(ttSvystatL, cursor="watch")

                        # DIRE A RAFFAELLA 16/07/10
                        # SvystatL_EsvydesignObj no more exists into TempEnv: we left inside its name only
                        SvystatL_EsvydesignObj <- get(SvystatL_Scelta_EsvydesignObj, envir=.GlobalEnv)
                        # DIRE A RAFFAELLA 16/07/10

                        outSvystatL <- Lancia(svystatL(SvystatL_EsvydesignObj, expr, by, vartype,
                                                            conf.int, conf.lev, deff, na.rm), textWarnings, parent = ttSvystatL)

                        if (!inherits(outSvystatL,"try-error")) {
                            attr(outSvystatL,"design") <- as.symbol(SvystatL_Scelta_EsvydesignObj)
                            # assign(OR_SvystatLObjectName, outSvystatL, envir = .GlobalEnv)
                            assign2GE(OR_SvystatLObjectName, outSvystatL)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            if (!is.big(outSvystatL)) {
                                cat(paste("# ",OR_SvystatLObjectName,"\n",sep=""))
                            }
                            else {
                                cat(paste("# head(",OR_SvystatLObjectName,")\n",sep=""))
                            }
                            printonscreen(outSvystatL, OR_SvystatLObjectName)
                            cat("\n")

                            prnDesign <- paste("design=", SvystatL_Scelta_EsvydesignObj)

                            prnSvystatL<- paste(" <- svystatL(",    prnDesign, ", ", prnExpr, ", ", prnBy,
                                        ", ", prnVartype, ", ", prnConf.int, ", ", prnConf.lev,
                                        ", ", prnDeff, ", ", prnNa.rm, ")", sep="")


                            commands <- paste(OR_SvystatLObjectName, prnSvystatL, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                  tkmessageBox(title ="svystatL",
                                               message = "Operation executed\nNote: Warnings have been generated!",
                                               icon = "info", parent = ttSvystatL)
                                 }
                            else {
                                  tkmessageBox(title ="svystatL",message = "Operation executed", icon = "info", parent = ttSvystatL)
                                 }
                            tkgrab.release(ttSvystatL)
                        }
                    # get back the standard arrow cursor
                    tkconfigure(ttSvystatL, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# ---------------------------------
# < END building svystatL window. <
# ---------------------------------

# --------------------------------------
# > START building e.calibrate window. >
# --------------------------------------

            fE.calibrate <- function(){
                    listEsvydesignObj <-  mk.class.list("analytic")
                    ttEcalibrate <- tktoplevel()
                    tcl("wm", "protocol", ttEcalibrate, "WM_DELETE_WINDOW", function() fOnCancel(ttEcalibrate))
                    frameGlobal<- tkframe(ttEcalibrate, borderwidth= 2)
                    tkwm.deiconify(ttEcalibrate)
                    tkgrab.set(ttEcalibrate)
                    # tkfocus(ttEcalibrate)
                    tkwm.title(ttEcalibrate,label_Ecalibrate)
                    tkwm.resizable(ttEcalibrate, 0, 0)

                    frameTop <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameDown <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameOutput <- tkframe(frameGlobal, borderwidth=0)
                    frameButtons <- tkframe(frameGlobal, borderwidth=0)

                    lblfSampleData<- tk2labelframe(frameTop)

                    lblDfpopulation <- ttklabel(frameTop, text="Select population totals", font=fontTextLabel)
                    lblDesignObj <- ttklabel(frameTop, text="Select a survey design object",
                                             font=fontTextLabel)
                    lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")

                    lblECalDfpopulation <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarECalDfpopulation)),foreground="red")
                    lblECalDesignObj <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarECalDesignObj)),foreground="red")

                    listDfpopulation <- listDfpop()

                    lblMandatory <- tk2label(frameGlobal,text="Formula Fields", font=fontTextTitle, foreground= "blue")
                    lblOptional <-tk2label(frameGlobal,text="Optional Fields", font=fontTextTitle, foreground= "blue")

                    scrDfpopulation <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstDfpopulation,...))
                    listaDfp <- tclVar()
                    tclObj(listaDfp) <- listDfpopulation
                    lstDfpopulation <- tklistbox(frameTop,height = 4, listvariable= listaDfp, selectmode="single", yscrollcommand = function (...)tkset(scrDfpopulation,...), background = "white")

                    scrEsvydesignObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstEsvydesignObj,...))

                    lista <- tclVar()
                    tclObj(lista) <- listEsvydesignObj
                    lstEsvydesignObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="single", yscrollcommand = function (...)tkset(scrEsvydesignObj,...), background = "white")


                    scrVariables<- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstVariables,...))
                    listaVariables <- tclVar()
                    lstVariables <- tklistbox(frameTop,height = 4, listvariable= listaVariables, selectmode="extended", yscrollcommand = function (...)tkset(scrVariables,...), background = "white")

                    count_Dfpopulation <<- FALSE
                    count_poptotals <<- FALSE
                    count_Ecalibrate <<- FALSE

                    tkbind(lstDfpopulation, "<ButtonRelease-1>",
                           function() fClassControl(lstDfpopulation, listaDfp, #lblDesignObj,
                           rbCalmodel, rbPartition, lblPartitionForm, entry.PartitionForm, OkPartitionForm.but,
                           lblCalmodelForm, entry.CalmodelForm,OkCalmodelForm.but, Plus.but, Times.but, Colon.but,
                           Minus.but, LeftParen.but, RightParen.but, Collapse.but, textCalmodel, textPartition,
                           VarECalDfpopulation, lblECalDfpopulation, BoundsHint.but, ok.but, entry.ObjectName))

                    tkbind(lstEsvydesignObj, "<ButtonRelease-1>",
                           function() fElencoCampiEcalibrate(lista, lstEsvydesignObj, lblVariables,
                           listaVariables, lstVariables, scrVariables, lblECalDesignObj, VarECalDesignObj,
                           Aggregate, listaAggregate, cbAggregate, AggregateDef,
                           Sigma2, listaSigma2, cbSigma2, Sigma2Def,
                           entry.CalmodelForm, OkCalmodelForm.but, entry.PartitionForm, OkPartitionForm.but,
                           rbCalmodel, rbPartition, lblPartitionForm, lblCalmodelForm, BoundsHint.but,
                           ok.but, textCalmodel, entry.ObjectName))

                    tkbind(lstVariables, "<Double-Button-1>",
                           function() fInsertVariable(lstVariables, listaVariables, entry.CalmodelForm,
                           entry.PartitionForm, ObjectPartitionF, VarECalDesignObj, lblECalDesignObj,
                           VarECalDfpopulation, lblECalDfpopulation, ObjectCalmodelF, parent = ttEcalibrate))


                    labellblfCalmodelPartition <- ttklabel(frameCentral,text="Formulae", font=fontTextLabel)
                    lblfCalmodelPartition<- ttklabelframe(frameCentral, labelwidget=labellblfCalmodelPartition)

                    labellblfCalmodel <- ttklabel(frameCentral,text="  calmodel  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("e.calibrate", args = "calmodel")
                    tk2tip(labellblfCalmodel,descfunz)
                    lblfCalmodel<- ttklabelframe(lblfCalmodelPartition, labelwidget=labellblfCalmodel)

                    # Here layout in pixels: should translate in cm (which is our standard)...
                    frameCalmodel<- tkframe(lblfCalmodel, borderwidth= 2, padx=5)
                    scrtextCalmodel <- tkscrollbar(frameCalmodel, orient = "vertical", command=function(...) tkyview(textCalmodel, ...))
                    textCalmodel <- tktext(frameCalmodel, foreground="red", background= "white", height=4, width=40, yscrollcommand=function(...) tkset(scrtextCalmodel, ...),
                    state="disabled", wrap="word", font=fontTextExp)

                    labellblfPartition <- ttklabel(frameCentral,text="   partition   ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("e.calibrate", args = "partition")
                    tk2tip(labellblfPartition,descfunz)
                    lblfPartition<- ttklabelframe(lblfCalmodelPartition, labelwidget=labellblfPartition)

                    # Here layout in pixels: should translate in cm (which is our standard)...
                    framePartition<- tkframe(lblfPartition, borderwidth= 2, padx=5)
                    scrtextPartition <- tkscrollbar(framePartition, orient = "vertical", command=function(...) tkyview(textPartition, ...))
                    textPartition <- tktext(framePartition, foreground="red", background= "white", height=3, width=40, yscrollcommand=function(...) tkset(scrtextPartition, ...),
                    state="disabled", wrap="word", font=fontTextExp)


                    labellblfFormulaComp <- ttklabel(frameCentral,text="Formula composer", font=fontTextLabel)
                    lblfFormulaComp<- ttklabelframe(frameCentral, labelwidget=labellblfFormulaComp)

                    outerOperatorsFrame <- tkframe(lblfFormulaComp)
                    FormulaFrame <- tkframe(lblfFormulaComp)

                    ObjectCalmodelF <- tclVar("")

                    entry.CalmodelForm <- tkentry(FormulaFrame,textvariable=ObjectCalmodelF, state="disabled", width=34, background="white")

                    OkCalmodelForm.but <- tk2button(FormulaFrame, image=image_sx, state="disabled",
                                          command=function() fOnCalmodelForm(ObjectCalmodelF,
                                          entry.CalmodelForm, ok.but, textCalmodel, BoundsHint.but, entry.ObjectName, parent = ttEcalibrate))

                    ObjectPartitionF <- tclVar("")
                    entry.PartitionForm <- tkentry(FormulaFrame,text=ObjectPartitionF, state="disabled", width=34, background="white")


                    OkPartitionForm.but <- tk2button(FormulaFrame, image=image_sx, state="disabled",
                    command=function() fOnPartitionForm(ObjectPartitionF, entry.PartitionForm,
                    VarECalDfpopulation, lblECalDfpopulation, textPartition))

                    rbCalmodel <- ttkradiobutton(FormulaFrame, state="disabled")
                    rbPartition <- ttkradiobutton(FormulaFrame, state="disabled")

                    rbValueCalPart <<- tclVar("CalChoice")

                    tkconfigure(rbCalmodel,variable=rbValueCalPart,value="CalChoice")
                    tkconfigure(rbPartition,variable=rbValueCalPart,value="ParChoice")

                    lblCalmodelForm<- ttklabel(FormulaFrame,text="Calmodel = ~", state="disabled")
                    lblPartitionForm <- ttklabel(FormulaFrame,text="Partition = ~", state="disabled")

                    tkbind(rbCalmodel, "<ButtonPress>", function(){
                                                                    if (count_Dfpopulation){
                                                                        if (!count_poptotals){
                                                                            tkconfigure(lblPartitionForm, state = "disabled")
                                                                            tkconfigure(entry.PartitionForm, state = "disabled")
                                                                            tkconfigure(OkPartitionForm.but, state = "disabled")

                                                                            tkconfigure(lblCalmodelForm, state = "normal")
                                                                            tkconfigure(entry.CalmodelForm, state = "normal")
                                                                            tkconfigure(OkCalmodelForm.but, state = "normal")
                                                                            rbValueCalPart <<- tclVar("CalChoice")
                                                                        }
                                                                    }
                                                                })
                    tkbind(rbPartition, "<ButtonPress>", function(){
                                                                    if (count_Dfpopulation){
                                                                        if (!count_poptotals){
                                                                             tkconfigure(lblCalmodelForm, state = "disabled")
                                                                             tkconfigure(entry.CalmodelForm, state = "disabled")
                                                                             tkconfigure(OkCalmodelForm.but, state = "disabled")
                                                                             tkconfigure(lblPartitionForm, state = "normal")
                                                                             tkconfigure(entry.PartitionForm, state = "normal")
                                                                             tkconfigure(OkPartitionForm.but, state = "normal")
                                                                             rbValueCalPart <<- tclVar("ParChoice")
                                                                        }
                                                                    }
                                                                 })

                    Plus.but <- tk2button(outerOperatorsFrame, text="+", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, " + "))
                    Times.but <- tk2button(outerOperatorsFrame, text="*", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, "*"))
                    Colon.but <- tk2button(outerOperatorsFrame, text=":", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, ":"))
                    Minus.but <- tk2button(outerOperatorsFrame, text="-", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, " - "))
                    LeftParen.but <- tk2button(outerOperatorsFrame, text="(", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, "("))
                    RightParen.but <- tk2button(outerOperatorsFrame, text=")", width="3", state="disabled", command=function() fOnOperator (ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, ")"))
                    Collapse.but <- tk2button(outerOperatorsFrame, text="+...+", width="5", state="disabled", command=function() fOnCollapseOperator(lstVariables, entry.CalmodelForm, entry.PartitionForm, parent = ttEcalibrate))

                    labellblfCalfun <- ttklabel(frameDown,text="  calfun  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("e.calibrate", args = "calfun")
                    tk2tip(labellblfCalfun,descfunz)
                    lblfCalfun<- ttklabelframe(frameDown, labelwidget=labellblfCalfun)

                    rbCalfunL <- ttkradiobutton(lblfCalfun)
                    rbCalfunR <- ttkradiobutton(lblfCalfun)
                    rbCalfunLo <- ttkradiobutton(lblfCalfun)

                    rbValueCalfun <- tclVar("linear")

                    tkconfigure(rbCalfunL,variable=rbValueCalfun,value="linear")
                    tkconfigure(rbCalfunR,variable=rbValueCalfun,value="raking")
                    tkconfigure(rbCalfunLo,variable=rbValueCalfun,value="logit")

                    labelCalfunL <- ttklabel(lblfCalfun,text="linear ")
                    labelCalfunR <- ttklabel(lblfCalfun,text="raking")
                    labelCalfunLo <- ttklabel(lblfCalfun,text="logit")



                    labellblfBounds <- ttklabel(frameDown,text="  bounds  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("e.calibrate", args = "bounds")
                    tk2tip(labellblfBounds,descfunz)
                    lblfBounds<- ttklabelframe(frameDown, labelwidget=labellblfBounds)

                    LowHighFrame <- tkframe(lblfBounds)
                    ButtonsFrame <- tkframe(lblfBounds)

                    labelBoundsHigh <- ttklabel(LowHighFrame,text="high")
                    labelBoundsLow <- ttklabel(LowHighFrame,text="low")

                    BoundsHigh <- tclVar(Inf)
                    BoundsLow <- tclVar(-Inf)

                    entry.BoundsHigh <- tkentry(LowHighFrame,width="8", background="white") # Here switching to 'ttkentry' would give the desired white bg
                    entry.BoundsLow <- tkentry(LowHighFrame,width="8", background="white")  # but would cause bounds values to be lost upon execution!
                                                                                            # WHY????

                    # DIRE A RAFFAELLA 16/07/10
                    # EcalibrateObj and scelta no more exist into TempEnv: we left inside their names only
                    # see below: get(...)
                    BoundsHint.but <- tk2button(ButtonsFrame, text="Hint", image=image_ok, compound="left", state= "disabled", command=function() fOnBoundsHint(textCalmodel, textPartition, entry.BoundsLow, entry.BoundsHigh, EC_EcalibrateObj=get(Scelta_EcalibrateObj, envir=.GlobalEnv), EC_scelta=get(choiceDf, envir=.GlobalEnv), ttEcalibrate))
                    # DIRE A RAFFAELLA 16/07/10
                    descBoundsHint <- descFun(label_BoundsHint)
                    tk2tip(BoundsHint.but, descBoundsHint)

                    HelpBoundsHint.but <- tk2button(ButtonsFrame, image=image_qm, tip=descFun(label_BoundsHint), command=function() fOnFunctionHelp(label_BoundsHint))

                    labellblfAggregate <- ttklabel(frameDown,text="  aggregate.stage  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("e.calibrate", args = "aggregate.stage")
                    tk2tip(labellblfAggregate,descfunz)
                    lblfAggregate<- ttklabelframe(frameDown, labelwidget=labellblfAggregate)

                    listaAggregate <- tclVar()
                    Aggregate <- c("NULL")
                    tclObj(listaAggregate) <- Aggregate
                    cbAggregate <- tk2combobox(lblfAggregate, values = Aggregate, state="readonly")
                    AggregateDef <- tclVar("NULL")

                    labellblfSigma2 <- ttklabel(frameDown,text="  sigma2  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("e.calibrate", args = "sigma2")
                    tk2tip(labellblfSigma2,descfunz)
                    lblfSigma2<- ttklabelframe(frameDown, labelwidget=labellblfSigma2)

                    listaSigma2 <- tclVar()
                    Sigma2 <- c("NULL")
                    tclObj(listaSigma2) <- Sigma2
                    cbSigma2 <- tk2combobox(lblfSigma2, values = Sigma2, state="readonly")
                    Sigma2Def <- tclVar("NULL")

                    labellblfMaxit <- ttklabel(frameDown,text="  maxit  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("e.calibrate", args = "maxit")
                    tk2tip(labellblfMaxit,descfunz)
                    lblfMaxit<- ttklabelframe(frameDown, labelwidget=labellblfMaxit)

                    Maxit <- tclVar(50)
                    entry.Maxit <-tkentry(lblfMaxit,width="4",textvariable=Maxit, background="white")

                    labellblfEpsilon <- ttklabel(frameDown,text="  epsilon  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("e.calibrate", args = "epsilon")
                    tk2tip(labellblfEpsilon,descfunz)
                    lblfEpsilon<- ttklabelframe(frameDown, labelwidget=labellblfEpsilon)

                    Epsilon <- tclVar(1e-07)
                    entry.Epsilon <-tkentry(lblfEpsilon,width="5",textvariable=Epsilon, background="white")

                    labellblfForce <- ttklabel(frameDown,text="  force  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("e.calibrate", args = "force")
                    tk2tip(labellblfForce,descfunz)
                    lblfForce<- ttklabelframe(frameDown, labelwidget=labellblfForce)

                    rbForceF <- ttkradiobutton(lblfForce)
                    rbForceT <- ttkradiobutton(lblfForce)
                    rbValueForce <- tclVar("TRUE")

                    tkconfigure(rbForceF,variable=rbValueForce,value="FALSE")
                    tkconfigure(rbForceT,variable=rbValueForce,value="TRUE")

                    labelForceF <- ttklabel(lblfForce,text="False ")
                    labelForceT <- ttklabel(lblfForce,text="True ")

                    labellblfEcalibrateObj <- ttklabel(frameOutput,text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfEcalibrateObj<- ttklabelframe(frameOutput, labelwidget = labellblfEcalibrateObj)

                    ObjectName <- tclVar("")
                    entry.ObjectName <-ttkentry(lblfEcalibrateObj,width="20",text=ObjectName, state= "disabled", font="TkDefaultFont")

                    ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_Ecalibrate(textCalmodel, textPartition, rbValueCalfun,
                                                 entry.BoundsHigh, entry.BoundsLow, cbAggregate, cbSigma2,
                                                 entry.Maxit, entry.Epsilon, rbValueForce, ObjectName,
                                                 ttEcalibrate))

                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttEcalibrate))

                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_Ecalibrate), command=function() fOnFunctionHelp(label_Ecalibrate))

                    tkgrid(tk2label(frameGlobal, text="Population and Survey Data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))

                    tkgrid(lblfSampleData)
                    tkgrid(lblDfpopulation, lblDesignObj, lblVariables)

                    tkgrid(lblECalDfpopulation,lblECalDesignObj)

                    tkgrid(lstDfpopulation, scrDfpopulation, lstEsvydesignObj, scrEsvydesignObj)

                    tkgrid.configure(lblDfpopulation, column=1, sticky ="e")
                    #padx=c("1.4c",0) sopra
                    tkgrid.configure(lblDesignObj, column=3, sticky ="e")
                    tkgrid.configure(lblVariables, column=5, padx=c("1c",0))

                    tkgrid.configure(lblECalDfpopulation, column=1, sticky ="e")
                    tkgrid.configure(lblECalDesignObj, column= 3, sticky ="e")

                    tkgrid.configure(lstDfpopulation, column=1, row=3, sticky ="e", padx=c("1.7c",0), pady=c(0,"0.2c"))
                    tkgrid.configure(scrDfpopulation, column=2, row=3, sticky ="nsw", pady=c(0,"0.2c"))

                    tkgrid.configure(lstEsvydesignObj, column=3, row=3, sticky ="e", padx=c("1.7c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrEsvydesignObj, column=4, row=3, sticky ="nsw", padx=c(0,"0.7c"), pady=c(0,"0.2c"))

                    tkgrid(Plus.but, Times.but, Colon.but, Minus.but, LeftParen.but, RightParen.but, Collapse.but, sticky ="e")
                    tkgrid(rbCalmodel,lblCalmodelForm, entry.CalmodelForm, OkCalmodelForm.but)

                    tkgrid.configure(rbCalmodel, padx=c("0.2c",0), pady=c(0,"1.9c"))
                    tkgrid.configure(lblCalmodelForm, pady=c(0,"1.9c"))
                    tkgrid.configure(entry.CalmodelForm, pady=c(0,"1.9c"))
                    # Here layout in pixels: should translate in cm (which is our standard)...
                    tkgrid.configure(OkCalmodelForm.but, padx=c(5,5), pady=c(0,"1.9c"))

                    tkgrid(rbPartition,lblPartitionForm, entry.PartitionForm, OkPartitionForm.but)

                    tkgrid.configure(rbPartition, padx=c("0.2c",0), pady=c(0,"1.1c"))
                    tkgrid.configure(lblPartitionForm, pady=c(0,"1.1c"))
                    tkgrid.configure(entry.PartitionForm, pady=c(0,"1.1c"))
                    # Here layout in pixels: should translate in cm (which is our standard)...
                    tkgrid.configure(OkPartitionForm.but, padx=c(5,5), pady=c(0,"1.1c"))

                    tkgrid(lblfCalfun, lblfBounds, lblfAggregate, lblfSigma2, lblfMaxit,lblfEpsilon, lblfForce)

                    tkgrid(labelCalfunL,rbCalfunL)
                    tkgrid(labelCalfunR,rbCalfunR)
                    tkgrid(labelCalfunLo,rbCalfunLo)

                    tkgrid.configure(labelCalfunL, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelCalfunR, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelCalfunLo, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(lblfCalfun,padx=c("0.7c","0.5c"))

                    tkgrid(LowHighFrame)
                    tkgrid(ButtonsFrame)
                    tkgrid.configure(LowHighFrame, sticky="w")

                    tkgrid(labelBoundsLow, entry.BoundsLow)
                    tkgrid.configure(labelBoundsLow, sticky="w", padx=c("0.3c",0))
                    tkconfigure(entry.BoundsLow, textvariable=BoundsLow)

                    tkgrid.configure(entry.BoundsLow, column=1, padx=c(8,4))
                    tkgrid(labelBoundsHigh,entry.BoundsHigh)
                    tkgrid.configure(labelBoundsHigh, sticky="w", padx=c("0.3c",0))
                    tkconfigure(entry.BoundsHigh, textvariable=BoundsHigh)
                    tkgrid.configure(entry.BoundsHigh, column=1, padx=c(8,4))

                    tkgrid.configure(BoundsHint.but,HelpBoundsHint.but, pady=c(10,0))

                    tkbind(rbCalfunL, "<ButtonPress>", function(){
                                                                  BoundsHigh <- tclVar("Inf")
                                                                  BoundsLow <- tclVar("-Inf")
                                                                  tkconfigure(entry.BoundsHigh, textvariable=BoundsHigh)
                                                                  tkconfigure(entry.BoundsLow, textvariable=BoundsLow)
                                                                })

                    tkbind(rbCalfunR, "<ButtonPress>", function(){
                                                                  BoundsHigh <- tclVar("Inf")
                                                                  BoundsLow <- tclVar("-Inf")
                                                                  tkconfigure(entry.BoundsHigh, textvariable=BoundsHigh)
                                                                  tkconfigure(entry.BoundsLow, textvariable=BoundsLow)
                                                                })

                    tkbind(rbCalfunLo, "<ButtonPress>", function(){
                                                                   BoundsHigh <- tclVar("1E6")
                                                                   BoundsLow <- tclVar("-1E6")
                                                                   tkconfigure(entry.BoundsHigh, textvariable=BoundsHigh)
                                                                   tkconfigure(entry.BoundsLow, textvariable=BoundsLow)
                                                                })

                    tkconfigure(cbAggregate, textvariable = AggregateDef)
                    tkgrid(cbAggregate)

                    tkgrid.configure(cbAggregate,padx="0.2c", pady=c(0,"0.2c"))
                    tkgrid.configure(lblfAggregate,padx="0.5c")

                    tkconfigure(cbSigma2, textvariable = Sigma2Def)
                    tkgrid(cbSigma2)

                    tkgrid.configure(cbSigma2,padx="0.2c", pady=c(0,"0.2c"))
                    tkgrid.configure(lblfSigma2,padx=c(0,"0.5c"))

                    tkgrid(labelForceF,rbForceF)
                    tkgrid(labelForceT,rbForceT)
                    tkgrid.configure(lblfForce,padx=c(0,"0.7c"))

                    tkgrid.configure(labelForceF, sticky="w", padx=c("0.3c",0))
                    tkgrid.configure(labelForceT, sticky="w", padx=c("0.3c",0))

                    tkgrid(entry.Maxit)
                    tkconfigure(entry.Maxit, textvariable=Maxit)
                    tkgrid.configure(entry.Maxit, padx=c("0.7c",0), pady=c(0,"0.2c"))

                    tkgrid(entry.Epsilon)
                    tkconfigure(entry.Epsilon, textvariable=Epsilon)
                    tkgrid.configure(entry.Epsilon,  padx=c("0.8c",0), pady=c(0,"0.2c"))
                    tkgrid.configure(lblfEpsilon,padx=("0.5c"))

                    tkgrid(frameTop)
                    tkgrid(lblMandatory, pady=c("0.2c",0))

                    tkgrid(frameCentral)

                    tkgrid(lblfFormulaComp, lblfCalmodelPartition, sticky="ns")

                    tkgrid.configure(lblfCalmodelPartition,padx=c("0.25c","0.3c"))

                    tkgrid(lblfCalmodel)

                    tkgrid(lblfPartition)
                    tkgrid.configure(lblfCalmodel,pady=c(20,10))
                    tkgrid.configure(lblfPartition,padx=c(15,15))

                    tkgrid(frameCalmodel)
                    tkgrid(textCalmodel,scrtextCalmodel)
                    # Enable ctrl-c on textCalmodel
                    ctrl.c(textCalmodel)
                    tkgrid.configure(scrtextCalmodel, sticky ="nsw")

                    tkgrid(framePartition)
                    tkgrid(textPartition,scrtextPartition)
                    # Enable ctrl-c on textPartition
                    ctrl.c(textPartition)
                    tkgrid.configure(scrtextPartition, sticky ="nsw")

                    tkgrid.configure(lblfFormulaComp,padx=c("0.3c","0.25c"))

                    tkgrid(outerOperatorsFrame)
                    tkgrid.configure(outerOperatorsFrame, pady=c("0.3c","0.2c"))
                    tkgrid(FormulaFrame)

                    tkgrid(lblOptional, pady=c("0.2c",0))
                    tkgrid(frameDown)
                    tkgrid.configure(frameDown, padx="0.5c")

                    tkgrid(lblfEcalibrateObj)

                    tkgrid.configure(entry.ObjectName, padx=c("0.5c","13.5c"),pady=c(0,"0.3c"))
                    tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")

                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)
                    tkgrid.configure(Cancel.but, padx=("0.5c"))
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))
                    tkgrid.configure(frameButtons, sticky="ne")

                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttEcalibrate))) # To asses the size of the window...
                    # tkfocus(ttEcalibrate)
            }


            fElencoCampiEcalibrate <- function(EC_lista, EC_lstEsvydesignObj, EC_lblVariables, x,
                    lstEC_EsvydesignObj, scrEC, EC_lblECalDesignObj, VarECalDesignObj,
                    EC_Aggregate, EC_listaAggregate, EC_cbAggregate, EC_AggregateDef,
                    EC_Sigma2, EC_listaSigma2, EC_cbSigma2, EC_Sigma2Def,
                    EC_entry.CalmodelForm, EC_OkCalmodelForm.but, EC_entry.PartitionForm, EC_OkPartitionForm.but,
                    EC_rbCalmodel, EC_rbPartition, EC_lblPartitionForm, EC_lblCalmodelForm, EC_BoundsHint.but,
                    EC_ok.but, EC_textCalmodel, EC_entry.ObjectName){

                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstEsvydesignObj)))

                    assignTemp("Scelta_EcalibrateObj", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstEsvydesignObj))+1])

                    VarECalDesignObj <<- tclVar(Scelta_EcalibrateObj)
                    tkconfigure(EC_lblECalDesignObj, textvariable= VarECalDesignObj)

                    # DIRE A RAFFAELLA 16/07/10
                    # NO!!! Copying may become a serious concern when data get big
                    # (can tolerate it for names, because they are small: see above)
                    # assignTemp("EcalibrateObj", get(Scelta_EcalibrateObj, envir=.GlobalEnv))
                    EcalibrateObj <- get(Scelta_EcalibrateObj, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    if (Index_dataframe_Ecalibrate !=EC_indicesel){
                        count_Ecalibrate <<- FALSE
                    }
                    if (count_Ecalibrate == FALSE){
                            count_Ecalibrate <<- TRUE
                            Index_dataframe_Ecalibrate <<- EC_indicesel

# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                            for  (n in names(EcalibrateObj$variables)){
#                                        tclObj(x) <- c(names(EcalibrateObj$variables))
#                            }
                            tclObj(x) <- names(EcalibrateObj$variables)

                    }

                    if (tclvalue(rbValueCalPart)=="ParChoice"){

                            rbValueCalPart <<- tclVar("CalChoice")
                            tkconfigure(EC_rbPartition,variable=rbValueCalPart,value="ParChoice")
                            tkconfigure(EC_rbCalmodel,variable=rbValueCalPart, value="CalChoice")

                            tkconfigure(EC_entry.PartitionForm, state = "disabled")
                            tkconfigure(EC_OkPartitionForm.but, state = "disabled")
                            tkconfigure(EC_lblPartitionForm, state = "disabled")
                            tkconfigure(EC_entry.CalmodelForm, state = "normal")
                            tkconfigure(EC_OkCalmodelForm.but, state = "normal")
                            tkconfigure(EC_lblCalmodelForm, state = "normal")
                            tkfocus(EC_entry.CalmodelForm)
                    }

                    ids <- attr(EcalibrateObj, "ids")
                    ids.char <- names(model.frame(ids, EcalibrateObj$variables[1, ]))
                    stages <- length(ids.char)

                    EC_Aggregate <- c("NULL",1:stages)
                    tclObj(EC_listaAggregate) <- EC_Aggregate
                    tkconfigure(EC_cbAggregate, values = EC_Aggregate)
                    # Code below should restore initial value NULL when changing
                    # design object: works well
                    tclvalue(EC_AggregateDef) <- "NULL"

                    vars <- names(EcalibrateObj$variables)
                    isnum <- sapply(vars, function(v) is.numeric(EcalibrateObj$variables[, v]))
                    EC_Sigma2 <- c("NULL", vars[isnum])
                    tclObj(EC_listaSigma2) <- EC_Sigma2
                    tkconfigure(EC_cbSigma2, values = EC_Sigma2)
                    # Code below should restore initial value NULL when changing
                    # design object: works well
                    tclvalue(EC_Sigma2Def) <- "NULL"

                    valore <- tclvalue(tkget(EC_textCalmodel, "1.0", "end"))
                    nvalore <- nchar(valore)

                    if     (nvalore!= 1 && count_Dfpopulation){
                        tkconfigure(EC_ok.but, state= "normal")
                        tkconfigure(EC_BoundsHint.but, state= "normal")
                        tkdelete(EC_entry.ObjectName, 0, "end")
                        tkconfigure(EC_entry.ObjectName, state = "normal")
                    }

                    tkgrid.configure(lstEC_EsvydesignObj, column=5, row=3, sticky ="e", padx=c("1.c",0), pady=c(0,"0.2c"))
                    tkgrid.configure(scrEC, column=6, row=3, padx=c(0,"1.7c"), pady=c(0,"0.2c"), sticky ="nsw")
                    tkconfigure(EC_lblVariables, text="Variables", state = "normal")
            }

            fClassControl <- function(lstEC_Dfpopulation, xDf, #EC_lblDesignObj,
                    EC_rbCalmodel, EC_rbPartition,  EC_lblPartitionForm,
                    EC_entry.PartitionForm, EC_OkPartitionForm.but, EC_lblCalmodelForm, EC_entry.CalmodelForm,
                    EC_OkCalmodelForm.but, EC_Plus.but, EC_Times.but,
                    EC_Colon.but, EC_Minus.but, EC_LeftParen.but, EC_RightParen.but, EC_Collapse.but,
                    EC_textCalmodel, EC_textPartition, VarECalDfpopulation, EC_lblECalDfpopulation,
                    EC_BoundsHint.but, EC_ok.but, EC_entry.ObjectName){

                    EC_indicescelta <- as.character(tclvalue(tkcurselection(lstEC_Dfpopulation)))

                    # Don't understand the usefulness of command below: commented!
                    #tkconfigure(EC_lblDesignObj, state = "normal")

                    count_Dfpopulation <<- TRUE

                    assignTemp("choiceDf", as.character(tclObj(xDf))[as.integer(tkcurselection(lstEC_Dfpopulation))+1])

                    # DIRE A RAFFAELLA 16/07/10
                    # NO!!! Copying may become a serious concern when data get big
                    # (can tolerate it for names, because they are small: see above)
                    # assignTemp("scelta", get(choiceDf, envir=.GlobalEnv))
                    scelta <- get(choiceDf, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    VarECalDfpopulation <<- tclVar(choiceDf)
                    tkconfigure(EC_lblECalDfpopulation, textvariable= VarECalDfpopulation)

                    if (Index_choice_Ecalibrate!=EC_indicescelta){
                        count_scelta <<- FALSE
                    }

                    if (count_scelta==TRUE && count_Ecalibrate==TRUE){
                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_BoundsHint.but, state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")
                    }
                    else{
                        tkconfigure(EC_ok.but, state = "disabled")
                        tkconfigure(EC_BoundsHint.but, state = "disabled")
                        tkdelete(EC_entry.ObjectName, 0, "end")
                        tkconfigure(EC_entry.ObjectName, state = "disabled")
                    }

                    if (count_scelta == FALSE){
                        count_scelta <<- TRUE

                        Index_choice_Ecalibrate <<- EC_indicescelta
                        tkconfigure(EC_textCalmodel, state="normal")
                        tkdelete(EC_textCalmodel, "1.0", "end")
                    }

                    if (inherits(scelta, "pop.totals")){

                        count_poptotals <<- TRUE

                        if (count_Ecalibrate){
                            tkconfigure(EC_ok.but, state = "normal")
                            tkconfigure(EC_BoundsHint.but, state = "normal")
                            tkdelete(EC_entry.ObjectName, 0, "end")
                            tkconfigure(EC_entry.ObjectName, state = "normal")
                        }

                        tkconfigure(EC_rbCalmodel, state = "disabled")
                        tkconfigure(EC_rbPartition, state = "disabled")
                        tkconfigure(EC_lblPartitionForm, state = "disabled")
                        tkconfigure(EC_OkPartitionForm.but, state = "disabled")


                        tkconfigure(EC_entry.CalmodelForm, textvariable=tclVar(""), state = "disabled")
                        tkconfigure(EC_entry.PartitionForm, textvariable=tclVar(""), state = "disabled")

                        tkconfigure(EC_entry.PartitionForm, state = "disabled")
                        tkconfigure(EC_lblCalmodelForm, state = "disabled")
                        tkconfigure(EC_OkCalmodelForm.but, state = "disabled")


                        tkconfigure(EC_Plus.but, state = "disabled")
                        tkconfigure(EC_Times.but, state = "disabled")
                        tkconfigure(EC_Colon.but, state = "disabled")
                        tkconfigure(EC_Minus.but, state = "disabled")
                        tkconfigure(EC_LeftParen.but, state = "disabled")
                        tkconfigure(EC_RightParen.but, state = "disabled")
                        tkconfigure(EC_Collapse.but, state = "disabled")

                        tkconfigure(EC_textCalmodel, state="normal")
                        tkdelete(EC_textCalmodel, "1.0", "end")
                        tkconfigure(EC_textPartition, state="normal")
                        tkdelete(EC_textPartition, "1.0", "end")

                        VarCalmodel <<- attr(scelta,"calmodel")
                        VarCalmodel <<- tclvalue(tclVar( form.to.char(VarCalmodel) ))

                        tkinsert(EC_textCalmodel, "end", VarCalmodel)
                        tkconfigure(EC_textCalmodel, state="disabled")

                        VarPartition <<- attr(scelta,"partition")

                        if (VarPartition=="FALSE"){
                            VarPartition <<- tclvalue(tclVar(as.character(VarPartition)))

                            tkinsert(EC_textPartition, "end", VarPartition)
                            tkconfigure(EC_textPartition, state="disabled")
                        }
                        else{
                            VarPartition <<- tclvalue(tclVar( form.to.char(VarPartition) ))

                            tkconfigure(EC_textPartition, state="normal")
                            tkinsert(EC_textPartition, "end", VarPartition)
                            tkconfigure(EC_textPartition, state="disabled")
                        }
                    }
                    else{

                        count_poptotals <<- FALSE

                        tkconfigure(EC_rbPartition, state = "normal")
                        tkconfigure(EC_rbCalmodel, state = "normal")

                        rbValueCalPart <<- tclVar("CalChoice")

                        tkconfigure(EC_rbCalmodel,variable=rbValueCalPart,value="CalChoice")
                        tkconfigure(EC_rbPartition,variable=rbValueCalPart,value="ParChoice")

                        tkconfigure(EC_lblCalmodelForm, state = "normal")
                        tkconfigure(EC_entry.CalmodelForm, state = "normal")
                        tkconfigure(EC_Plus.but, state = "normal")
                        tkconfigure(EC_Times.but, state = "normal")
                        tkconfigure(EC_Colon.but, state = "normal")
                        tkconfigure(EC_Minus.but, state = "normal")
                        tkconfigure(EC_LeftParen.but, state = "normal")
                        tkconfigure(EC_RightParen.but, state = "normal")
                        tkconfigure(EC_Collapse.but, state = "normal")

                        tkconfigure(EC_OkCalmodelForm.but, state = "normal")
                        tkconfigure(EC_textCalmodel, state="disabled")
                        tkconfigure(EC_textPartition, state="disabled")
                        tkconfigure(EC_textPartition, state="normal")
                        tkdelete(EC_textPartition, "1.0", "end")
                        tkinsert(EC_textPartition, "end", "FALSE")
                        tkconfigure(EC_textPartition, state="disabled")
                    }
            }

            fOnOperator <- function(O_ObjectCalmodelF, O_entry.CalmodelForm, O_ObjectPartitionF, O_entry.PartitionForm, O_Operator){

                    CalParType <- tclvalue(rbValueCalPart)
                    if (CalParType=="CalChoice"){

                        tkfocus(O_entry.CalmodelForm)
                        entry <- tclvalue(tkget(O_entry.CalmodelForm))

                        lenghtentry <- nchar(entry)

                        if (lenghtentry > 0){
                            tclvalue(O_ObjectCalmodelF) <- paste(entry, O_Operator, sep="")
                        }
                        else{
                            tclvalue(O_ObjectCalmodelF) <- paste(O_Operator, sep="")
                        }

                        tkconfigure(O_entry.CalmodelForm, textvariable=tclVar(tclvalue(O_ObjectCalmodelF) ))
                        tkicursor(O_entry.CalmodelForm , "end")
                        tkxview.moveto(O_entry.CalmodelForm, 1.0)
                    }
                    else{
                        tkfocus(O_entry.PartitionForm)
                        entry <- tclvalue(tkget(O_entry.PartitionForm))

                        lenghtentry <- nchar(entry)

                        if (lenghtentry > 0){
                            tclvalue(O_ObjectPartitionF) <- paste(entry, O_Operator, sep="")
                        }
                        else{
                            tclvalue(O_ObjectPartitionF) <- paste(O_Operator, sep="")
                        }

                        tkconfigure(O_entry.PartitionForm, textvariable=tclVar(tclvalue(O_ObjectPartitionF) ))
                        tkicursor(O_entry.PartitionForm , "end")
                        tkxview.moveto(O_entry.PartitionForm, 1.0)
                    }
                }


            fOnCollapseOperator <- function(O_lstVariables, O_entry.CalmodelForm, O_entry.PartitionForm, parent="."){
                    Vars <- as.character(tkget(O_lstVariables, 0, "end"))
                    selection <- 1L + as.integer(tkcurselection(O_lstVariables))
                    sleng <- length(selection)
                    selVars <- Vars[selection]
                    if (sleng==0){
                        tkmessageBox(title="Empty Select List", message = "Please select at least one element", icon = "error", parent = parent)
                    }
                    else{
                        lstVariablessum <- paste(selVars, collapse = " + ")
                        fOnOperator(ObjectCalmodelF,O_entry.CalmodelForm,ObjectPartitionF,O_entry.PartitionForm, lstVariablessum)
                    }
            }


            fInsertVariable <- function(EC_lstVariables, EC_listaVariables, EC_entry.CalmodelForm,
                    EC_entry.PartitionForm, EC_ObjectPartitionF, VarECalDesignObj, EC_lblECalDesignObj,
                    VarECalDfpopulation, EC_lblECalDfpopulation, EC_ObjectCalmodelF, parent="."){

                    if (!count_Dfpopulation) {
                                    tkmessageBox(title="Variables ", message="Must specify population totals!",icon="error", parent = parent)
                    }
                    else{
                        if (count_poptotals){
                            tkmessageBox(title="Variables ", message="Chosen population totals dataframe has its own formula!",icon="error", parent = parent)
                        }
                        else{
                            VarECalDesignObj <<- tclVar(Scelta_EcalibrateObj)
                            tkconfigure(EC_lblECalDesignObj, textvariable= VarECalDesignObj)

                            VarECalDfpopulation <<- tclVar(as.character(choiceDf))
                            tkconfigure(EC_lblECalDfpopulation, textvariable= VarECalDfpopulation)

                            EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstVariables)))

                            assignTemp("Scelta_Variables", as.character(tclObj(EC_listaVariables))[as.integer(tkcurselection(EC_lstVariables))+1])
                            if (tclvalue(rbValueCalPart)=="CalChoice"){
                                tkfocus(EC_entry.CalmodelForm)
                                entry <- tclvalue(tkget(EC_entry.CalmodelForm))
                                lenghtentry <- nchar(entry)

                                if (lenghtentry > 0){
                                    tclvalue(EC_ObjectCalmodelF) <- paste(entry, Scelta_Variables, sep="")
                                }
                                else{
                                    tclvalue(EC_ObjectCalmodelF) <- paste(Scelta_Variables, sep="")
                                }
                                tkconfigure(EC_entry.CalmodelForm, textvariable=tclVar(tclvalue(EC_ObjectCalmodelF)))

                                lenghtEC_ObjectCalmodelF <- nchar(EC_ObjectCalmodelF)
                                tkicursor(EC_entry.CalmodelForm , "end")
                                tkxview.moveto(EC_entry.CalmodelForm , 1.0)
                            }
                            else{
                                tkfocus(EC_entry.PartitionForm)
                                entry <- tclvalue(tkget(EC_entry.PartitionForm))
                                lenghtentry <- nchar(entry)

                                if (lenghtentry > 0){
                                    tclvalue(EC_ObjectPartitionF) <- paste(entry, Scelta_Variables, sep="")
                                }
                                else{
                                    tclvalue(EC_ObjectPartitionF) <- paste(Scelta_Variables, sep="")
                                }
                                tkconfigure(EC_entry.PartitionForm, textvariable=tclVar(tclvalue(EC_ObjectPartitionF)))
                                lenghtEC_ObjectPartitionF <- nchar(EC_ObjectPartitionF)
                                tkicursor(EC_entry.PartitionForm , "end")
                                tkxview.moveto(EC_entry.PartitionForm , 1.0)
                            }
                        }
                    }
            }


            fOnCalmodelForm <- function(EC_ObjectCalmodelF, EC_entry.CalmodelForm, EC_ok.but,EC_textCalmodel, EC_BoundsHint.but, EC_entry.ObjectName, parent = "."){

                    EC_ObjectCalmodelF <- tkget(EC_entry.CalmodelForm)

                    if (tclvalue(EC_ObjectCalmodelF) == ""){
                        tkmessageBox(title="Calmodel formula",message="Calmodel formula is empty!",icon="error",type="ok", parent = parent)
                    }
                    else{
                        CalmodelFormula <- tclvalue(EC_ObjectCalmodelF)
                        CalmodelFormula <- Lancia(as.formula(paste("~", CalmodelFormula), env = .GlobalEnv), textWarnings, parent = parent)
                        if (!inherits(CalmodelFormula, "try-error")){
                            CalmodelFormula <- form.to.char(CalmodelFormula)
                        }
                        else return()

                        tkconfigure(EC_textCalmodel, state="normal")
                        tkdelete(EC_textCalmodel, "1.0", "end")
                        tkinsert(EC_textCalmodel, "end", CalmodelFormula)
                        tkconfigure(EC_textCalmodel, state="disabled")

                        tkconfigure(EC_entry.CalmodelForm, textvariable=tclVar(""))

                        if (count_Ecalibrate)
                            tkconfigure(EC_ok.but, state = "normal")
                            tkconfigure(EC_BoundsHint.but, state = "normal")
                            tkdelete(EC_entry.ObjectName, 0, "end")
                            tkconfigure(EC_entry.ObjectName, state = "normal")
                    }
            }

            fOnPartitionForm <- function(EC_ObjectPartitionF, EC_entry.PartitionForm,
                    VarECalDfpopulation, EC_lblECalDfpopulation, EC_textPartition){

                    VarECalDfpopulation <<- tclVar(choiceDf)

                    tkconfigure(EC_lblECalDfpopulation, textvariable= VarECalDfpopulation)

                    EC_ObjectPartitionF <- tkget(EC_entry.PartitionForm)

                    if (tclvalue(EC_ObjectPartitionF) == ""){
                        PartitionFormula <- "FALSE"
                    }
                    else{
                        PartitionFormula <- tclvalue(EC_ObjectPartitionF)
                        PartitionFormula <- paste("~", PartitionFormula)
                        }
                        tkconfigure(EC_textPartition, state="normal")
                        tkdelete(EC_textPartition, "1.0", "end")
                        tkinsert(EC_textPartition, "end", PartitionFormula)
                        tkconfigure(EC_textPartition, state="disabled")
                        tkconfigure(EC_entry.PartitionForm, textvariable=tclVar(""))
            }


            fOnBoundsHint <- function(EC_textCalmodel, EC_textPartition, EC_entry.BoundsLow, EC_entry.BoundsHigh, EC_EcalibrateObj, EC_scelta, ttEcalibrate){
                    if (count_poptotals){
                        calmodel <- attr(EC_scelta,"calmodel")

                        partition <- attr(EC_scelta,"partition")
                    }
                    else{
                        calmodel <- tclvalue(tkget(EC_textCalmodel, "1.0", "end"))

                        calmodel  <- Lancia(as.formula(calmodel, env = .GlobalEnv), textWarnings, parent = ttEcalibrate)

                        partition <- as.character(tclvalue(tkget(EC_textPartition, "1.0", "end -1 chars")))

                        if (partition!= "FALSE"){
                            partition  <- Lancia(as.formula(partition, env = .GlobalEnv), textWarnings, parent = ttEcalibrate)
                        }
                        else{
                            partition <- FALSE
                        }
                    }

                    # change the cursor to the hourglass to tell work is in progress...
                    tkconfigure(ttEcalibrate, cursor="watch")
                    hint <- Lancia(bounds.hint(EC_EcalibrateObj, EC_scelta, calmodel, partition,FALSE), textWarnings, parent = ttEcalibrate)

                    if (!inherits(hint, "try-error")){
                        BoundsHigh <- tclVar(hint[2])
                        BoundsLow <- tclVar(hint[1])
                        tkconfigure(EC_entry.BoundsHigh, textvariable=BoundsHigh)
                        tkconfigure(EC_entry.BoundsLow, textvariable=BoundsLow)
                        # assign("last.hint", hint, envir=.GlobalEnv)
                        assign2GE("last.hint", hint)
                        cat("\n")
                        cat("# last.hint\n")
                        print(hint)
                        cat("\n")
                    }
                # get back the standard arrow cursor
                tkconfigure(ttEcalibrate, cursor="arrow")
                # Collect garbage to free memory
                gc()
            }

            fOnRun_Ecalibrate <- function(OR_textCalmodel, OR_textPartition, OR_rbValueCalfun, OR_entry.BoundsHigh,
                                          OR_entry.BoundsLow, OR_cbAggregate, OR_cbSigma2, OR_entry.Maxit,
                                          OR_entry.Epsilon, OR_rbValueForce, OR_EcalibrateObjectName, ttEcalibrate){

                    campobb <- TRUE

                    # DIRE A RAFFAELLA 16/07/10
                    # scelta no more exists into TempEnv: we left inside its name only
                    scelta <- get(choiceDf, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    if (count_poptotals){
                        calmodel <- attr(scelta,"calmodel")

                        prnCalmodel <- form.to.char(calmodel)
                        prnCalmodel <- paste("calmodel=", prnCalmodel)

                        partition <- attr(scelta,"partition")
                        if (partition=="FALSE"){
                            prnPartition <- paste("partition=", "FALSE")
                        }
                        else{
                            prnPartition <- form.to.char(partition)
                            prnPartition <- paste("partition=", prnPartition)
                        }

                    }
                    else{
                        calmodel <- tclvalue(tkget(OR_textCalmodel, "1.0", "end -1 chars"))

                        calmodel  <- Lancia(as.formula(calmodel, env = .GlobalEnv), textWarnings, parent = ttEcalibrate)

                        if (!inherits(calmodel,"try-error")){
                            prnCalmodel <- tclvalue(tkget(OR_textCalmodel, "1.1", "end -1 chars"))
                            prnCalmodel <- paste("calmodel=", prnCalmodel, sep =" ~")
                        }
                        else{
                            campobb <- FALSE
                        }

                        partition <- as.character(tclvalue(tkget(OR_textPartition, "1.0", "end -1 chars")))

                        if (partition!= "FALSE"){
                            partition  <- Lancia(as.formula(partition, env = .GlobalEnv), textWarnings, parent = ttEcalibrate)

                            if (!inherits(partition,"try-error")){
                                prnPartition <- tclvalue(tkget(OR_textPartition, "1.1", "end -1 chars"))
                                prnPartition <- paste("partition=", prnPartition, sep =" ~")
                            }
                            else{
                                campobb <- FALSE
                            }
                        }
                        else{
                            partition <- FALSE
                            prnPartition <- paste("partition=", "FALSE")
                        }
                    }

                    calfun <- tclvalue(OR_rbValueCalfun)

                    if (calfun=="linear")
                        prnCalfun <- 'calfun= "linear"'

                    else if (calfun=="raking")
                        prnCalfun <- 'calfun= "raking"'
                    else
                        prnCalfun <- 'calfun= "logit"'

                    if (calfun=="logit"){
                        boundsH <- tclvalue(tkget(OR_entry.BoundsHigh))
                        boundsL <- tclvalue(tkget(OR_entry.BoundsLow))

                        if (boundsH!="1E6"){
                            if (boundsH ==""){
                                boundsH <- 1E6
                            }
                            else{
                                boundsH <- as.numeric(tkget(OR_entry.BoundsHigh))
                                if (is.na(boundsH)){
                                    tkmessageBox(title="high bound", message="Please insert a numeric value",icon="error", parent = ttEcalibrate)
                                    campobb <- FALSE
                                }
                            }
                        }
                        else{
                            boundsH <- as.numeric(tkget(OR_entry.BoundsHigh))
                        }

                        if (boundsL!="-1E6"){
                            if (boundsL ==""){
                                boundsL <- -1E6
                            }
                            else{
                                boundsL <- as.numeric(tkget(OR_entry.BoundsLow))
                                if (is.na(boundsL)){
                                    tkmessageBox(title="low bound", message="Please insert a numeric value",icon="error", parent = ttEcalibrate)
                                    campobb <- FALSE
                                }
                            }
                        }
                        else{
                            boundsL <- as.numeric(tkget(OR_entry.BoundsLow))
                        }
                    }
                    else{
                        boundsH <- tclvalue(tkget(OR_entry.BoundsHigh))
                        boundsL <- tclvalue(tkget(OR_entry.BoundsLow))

                        if (boundsH!=Inf){
                            if (boundsH ==""){
                                boundsH <- Inf
                            }
                            else{
                                boundsH <- as.numeric(tkget(OR_entry.BoundsHigh))
                                if (is.na(boundsH)){
                                    tkmessageBox(title="high bound", message="Please insert a numeric value",icon="error", parent = ttEcalibrate)
                                    campobb <- FALSE
                                }
                            }
                        }
                        else{
                            boundsH <- as.numeric(tkget(OR_entry.BoundsHigh))
                        }

                        if (boundsL!=-Inf){
                            if (boundsL ==""){
                                boundsL <- -Inf
                            }
                            else{
                                boundsL <- as.numeric(tkget(OR_entry.BoundsLow))
                                if (is.na(boundsL)){
                                    tkmessageBox(title="low bound", message="Please insert a numeric value",icon="error", parent = ttEcalibrate)
                                    campobb <- FALSE
                                }
                            }
                        }
                        else{
                            boundsL <- as.numeric(tkget(OR_entry.BoundsLow))
                        }
                    }

                    if (campobb == TRUE){
                        bounds <- c(boundsL,boundsH)
                        prnBounds <- paste("bounds= c(",boundsL,", ",boundsH,")",sep="")
                    }

                    aggregate.stage <- tclvalue(tclVar(tkget(OR_cbAggregate)))
                    if (aggregate.stage != "NULL"){
                        aggregate.stage <- as.integer(aggregate.stage)
                        prnAggregate.stage <- paste("aggregate.stage=", aggregate.stage)
                    }
                    else{
                            aggregate.stage <- NULL
                            prnAggregate.stage <- paste("aggregate.stage=", "NULL")
                    }


                    sigma2char <- tclvalue(tclVar(tkget(OR_cbSigma2)))
                    if (sigma2char != "NULL"){
                        sigma2 <- as.formula(paste("~", sigma2char), env = .GlobalEnv)
                        prnSigma2 <- paste("sigma2=", sigma2char, sep =" ~ ")
                    }
                    else{
                            sigma2 <- NULL
                            prnSigma2 <- paste("sigma2=", "NULL")
                    }


                    maxit <- tclvalue(tkget(OR_entry.Maxit))
                    if (maxit ==""){
                        prnMaxit <- paste("maxit=", 50)
                        maxit<- tclVar(50)
                    }
                    else{
                        maxit <- as.numeric(tkget(OR_entry.Maxit))

                        if (is.na(maxit)){
                            tkmessageBox(title="maxit", message="Please insert a numeric value",icon="error", parent = ttEcalibrate)
                            campobb <- FALSE
                        }
                        else{
                            prnMaxit <- paste("maxit=", maxit)
                        }
                    }


                    epsilon <- tclvalue(tkget(OR_entry.Epsilon))
                    if (epsilon ==""){
                        prnEpsilon <- paste("epsilon=", 1e-7)
                        epsilon<- tclVar(1e-7)
                    }
                    else{
                        epsilon <- as.numeric(tkget(OR_entry.Epsilon))

                        if (is.na(epsilon)){
                                tkmessageBox(title="epsilon", message="Please insert a numeric value",icon="error", parent = ttEcalibrate)
                                campobb <- FALSE
                        }
                        else{
                            prnEpsilon <- paste("epsilon=", epsilon)
                        }
                    }

                    force <- as.logical(tclvalue(OR_rbValueForce))
                    if (!force) {
                            prnForce <- "force= FALSE"
                    }
                    else{
                        prnForce <- "force= TRUE"
                    }

                    if (tclvalue(OR_EcalibrateObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = ttEcalibrate)
                        campobb <- FALSE
                    }
                    else{
                        if   ( !is.ok.name(tclvalue(OR_EcalibrateObjectName), go.on=campobb,
                                           parent = ttEcalibrate) ){
                             campobb <- FALSE
                             }
                        else {
                             OR_EcalibrateObjectName <- tclvalue(OR_EcalibrateObjectName)
                             }
                        }

                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(ttEcalibrate, cursor="watch")

                        # DIRE A RAFFAELLA 16/07/10
                        # EcalibrateObj no more exists into TempEnv: we left inside its name only
                        EcalibrateObj <- get(Scelta_EcalibrateObj, envir=.GlobalEnv)
                        # DIRE A RAFFAELLA 16/07/10

                        outEcalibrate <- Lancia(e.calibrate(EcalibrateObj, scelta, calmodel, partition, calfun,
                        bounds, aggregate.stage, sigma2, maxit, epsilon, force), textWarnings, parent = ttEcalibrate)

                        if (!inherits(outEcalibrate,"try-error")) {
                            prnDesign <- paste("design=",Scelta_EcalibrateObj)
                            prnDf.population <- paste("df.population=",choiceDf)
                            if (is.list(outEcalibrate[["call"]])){
                                outEcalibrate[["call"]][[1]] <- paste("e.calibrate(", prnDesign, ", ", prnDf.population, ", ", prnCalmodel,
                                ", ", prnPartition, ", ",    prnCalfun, ", ", prnBounds,
                                ", ", prnAggregate.stage, ", ", prnSigma2, ", ", prnMaxit, ", ", prnEpsilon, ", ", prnForce, ")", sep="")
                            }
                            else {
                                outEcalibrate[["call"]] <- paste("e.calibrate(", prnDesign, ", ", prnDf.population, ", ", prnCalmodel,
                                ", ", prnPartition, ", ",    prnCalfun, ", ", prnBounds,
                                ", ", prnAggregate.stage, ", ", prnSigma2, ", ", prnMaxit, ", ", prnEpsilon, ", ", prnForce, ")", sep="")
                            }
                            # assign(OR_EcalibrateObjectName, outEcalibrate, envir = .GlobalEnv)
                            assign2GE(OR_EcalibrateObjectName, outEcalibrate)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            cat(paste("# ",OR_EcalibrateObjectName,"\n",sep=""))
                            print(outEcalibrate)
                            cat("\n")

                            prnEcalibrate<- paste(" <- e.calibrate(", prnDesign, ", ", prnDf.population, ", ",
                                                prnCalmodel,    ", ", prnPartition, ", ",    prnCalfun, ", ",
                                                prnBounds,    ", ", prnAggregate.stage, ", ", prnSigma2, ", ",
                                                prnMaxit, ", ", prnEpsilon, ", ", prnForce, ")", sep="")

                            commands <- paste(OR_EcalibrateObjectName, prnEcalibrate, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                  tkmessageBox(title ="e.calibrate",
                                               message = "Operation executed\nNote: Warnings have been generated!",
                                               icon = "info", parent = ttEcalibrate)
                                 }
                            else {
                                  tkmessageBox(title ="e.calibrate",message = "Operation executed", icon = "info", parent = ttEcalibrate)
                                 }
                            tkgrab.release(ttEcalibrate)
                            }
                    # get back the standard arrow cursor
                    tkconfigure(ttEcalibrate, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# ------------------------------------
# < END building e.calibrate window. <
# ------------------------------------

# ---------------------------------------
# > START building pop.template window. >
# ---------------------------------------

            fPop.template <- function(){
                ttPoptemplate <- tktoplevel()
                tcl("wm", "protocol", ttPoptemplate, "WM_DELETE_WINDOW", function() fOnCancel(ttPoptemplate))
                frameGlobal<- tkframe(ttPoptemplate, borderwidth= 2)
                tkwm.deiconify(ttPoptemplate)
                tkgrab.set(ttPoptemplate)
                tkfocus(ttPoptemplate)
                tkwm.title(ttPoptemplate,label_Poptemplate)
                tkwm.resizable(ttPoptemplate, 0, 0)

                listAnalytic <-  mk.class.list("analytic")
                listSurveydataObj <- c(listAnalytic, listDataSets())

                frameTop     <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                frameOutput  <- tkframe(frameGlobal, borderwidth=0)
                frameButtons <- tkframe(frameGlobal, borderwidth=0)

                lblfSurveyData<- tk2labelframe(frameTop)

                lblSurveyDataObj <- ttklabel(frameTop, text="Select survey data", font=fontTextLabel)

                lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")

                lblPopSurveyDataObj <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarPopSurveyDataObj)),foreground="red")

                lblMandatory <- tk2label(frameGlobal,text="Formula Fields", font=fontTextTitle, foreground= "blue")

                scrSurveydataObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstSurveydataObj,...))
                lista <- tclVar()
                tclObj(lista) <- listSurveydataObj
                lstSurveydataObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="single", yscrollcommand = function (...)tkset(scrSurveydataObj,...), background = "white")

                scrVariables<- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstVariables,...))
                listaVariables <- tclVar()
                lstVariables <- tklistbox(frameTop,height = 4, listvariable= listaVariables, selectmode="extended", yscrollcommand = function (...)tkset(scrVariables,...), background = "white")


                count <<- FALSE

                tkbind(lstSurveydataObj, "<ButtonRelease-1>", function() fElencoCampiPoptemplate(lista, lstSurveydataObj,
                        lblVariables, listaVariables, lstVariables, scrVariables, lblPopSurveyDataObj, VarPopSurveyDataObj,
                        entry.CalmodelForm, Plus.but, Times.but, Colon.but, Minus.but, LeftParen.but, RightParen.but,
                        Collapse.but, OkCalmodelForm.but, entry.PartitionForm, OkPartitionForm.but,
                        rbCalmodel, rbPartition, lblPartitionForm, lblCalmodelForm, Ok.but, textCalmodel, textPartition,
                        entry.ObjectName))

                tkbind(lstVariables, "<Double-Button-1>", function() fPoptemplateInsertVariable(lstVariables, listaVariables,
                               entry.CalmodelForm, entry.PartitionForm, ObjectPartitionF, VarPopSurveyDataObj, lblPopSurveyDataObj,
                               ObjectCalmodelF))

                labellblfCalmodelPartition <- ttklabel(frameCentral,text="Formulae", font=fontTextLabel)
                lblfCalmodelPartition<- ttklabelframe(frameCentral, labelwidget=labellblfCalmodelPartition)

                labellblfCalmodel <- ttklabel(frameCentral,text="  calmodel  ", font=fontTextLabel, image=image_qm, compound="right")
                descfunz <- descArgs("pop.template", args = "calmodel")
                tk2tip(labellblfCalmodel,descfunz)
                lblfCalmodel<- ttklabelframe(lblfCalmodelPartition, labelwidget=labellblfCalmodel)

                # Here layout in pixels: should translate in cm (which is our standard)...
                frameCalmodel<- tkframe(lblfCalmodel, borderwidth= 2, padx=5)
                scrtextCalmodel <- tkscrollbar(frameCalmodel, orient = "vertical", command=function(...) tkyview(textCalmodel, ...))

                textCalmodel <- tktext(frameCalmodel, foreground="red", background= "white", height=4, width=40, yscrollcommand=function(...) tkset(scrtextCalmodel, ...),
                state="disabled", wrap="word", font=fontTextExp)

                labellblfPartition <- ttklabel(frameCentral,text="   partition   ", font=fontTextLabel, image=image_qm, compound="right")
                descfunz <- descArgs("pop.template", args = "partition")
                tk2tip(labellblfPartition,descfunz)
                lblfPartition<- ttklabelframe(lblfCalmodelPartition, labelwidget=labellblfPartition)

                # Here layout in pixels: should translate in cm (which is our standard)...
                framePartition<- tkframe(lblfPartition, borderwidth= 2, padx=5)
                scrtextPartition <- tkscrollbar(framePartition, orient = "vertical", command=function(...) tkyview(textPartition, ...))

                textPartition <- tktext(framePartition, foreground="red", background= "white", height=3, width=40, yscrollcommand=function(...) tkset(scrtextPartition, ...),
                state="disabled", wrap="word", font=fontTextExp)

                tkinsert(textPartition, "end", "FALSE")
                tkconfigure(textPartition, state="disabled")

                labellblfFormulaComp <- ttklabel(frameCentral,text="Formula composer", font=fontTextLabel)
                lblfFormulaComp<- ttklabelframe(frameCentral, labelwidget=labellblfFormulaComp)

                outerOperatorsFrame <- tkframe(lblfFormulaComp)
                FormulaFrame <- tkframe(lblfFormulaComp)

                ObjectCalmodelF <- tclVar("")
                entry.CalmodelForm <- tkentry(FormulaFrame,textvariable=ObjectCalmodelF, width=34, state="disabled", background="white")


                OkCalmodelForm.but <- tk2button(FormulaFrame, image=image_sx, state="disabled", command=function() fPoptemplateOnCalmodelForm(ObjectCalmodelF,
                                            entry.CalmodelForm, Ok.but, textCalmodel, entry.ObjectName, parent = ttPoptemplate))

                ObjectPartitionF <- tclVar("")
                entry.PartitionForm <- tkentry(FormulaFrame,text=ObjectPartitionF, state="disabled", width=34, background="white")

                OkPartitionForm.but <- tk2button(FormulaFrame, image=image_sx, state="disabled",
                command=function() fPoptemplateOnPartitionForm(ObjectPartitionF, entry.PartitionForm, textPartition))

                rbPartition <- ttkradiobutton(FormulaFrame, state="disabled")
                rbCalmodel <- ttkradiobutton(FormulaFrame, state="disabled")

                rbValueCalPart <<- tclVar("CalChoice")

                tkconfigure(rbCalmodel,variable=rbValueCalPart,value="CalChoice")
                tkconfigure(rbPartition,variable=rbValueCalPart,value="ParChoice")

                lblCalmodelForm<- ttklabel(FormulaFrame,text="Calmodel = ~", state="disabled")
                lblPartitionForm <- ttklabel(FormulaFrame,text="Partition = ~", state="disabled")

                tkbind(rbCalmodel, "<ButtonPress>", function(){
                                                                tkconfigure(lblPartitionForm, state = "disabled")
                                                                tkconfigure(entry.PartitionForm, state = "disabled")
                                                                tkconfigure(OkPartitionForm.but, state = "disabled")

                                                                tkconfigure(lblCalmodelForm, state = "normal")
                                                                tkconfigure(entry.CalmodelForm, state = "normal")
                                                                tkconfigure(OkCalmodelForm.but, state = "normal")
                                                                rbValueCalPart <<- tclVar("CalChoice")
                                                                })

                tkbind(rbPartition, "<ButtonPress>", function(){
                                                                 tkconfigure(lblCalmodelForm, state = "disabled")
                                                                 tkconfigure(entry.CalmodelForm, state = "disabled")
                                                                 tkconfigure(OkCalmodelForm.but, state = "disabled")
                                                                 tkconfigure(lblPartitionForm, state = "normal")
                                                                 tkconfigure(entry.PartitionForm, state = "normal")
                                                                 tkconfigure(OkPartitionForm.but, state = "normal")
                                                                 rbValueCalPart <<- tclVar("ParChoice")
                                                                })

                    Plus.but <- tk2button(outerOperatorsFrame, text="+", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, " + "))
                    Times.but <- tk2button(outerOperatorsFrame, text="*", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, "*"))
                    Colon.but <- tk2button(outerOperatorsFrame, text=":", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, ":"))
                    Minus.but <- tk2button(outerOperatorsFrame, text="-", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, " - "))
                    LeftParen.but <- tk2button(outerOperatorsFrame, text="(", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, "("))
                    RightParen.but <- tk2button(outerOperatorsFrame, text=")", width="3", state="disabled", command=function() fOnOperator (ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, ")"))
                    Collapse.but <- tk2button(outerOperatorsFrame, text="+...+", width="5", state="disabled", command=function() fOnCollapseOperator(lstVariables, entry.CalmodelForm, entry.PartitionForm, parent = ttPoptemplate))


                    labellblfPoptemplateObj <- ttklabel(frameOutput,text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfPoptemplateObj<- ttklabelframe(frameOutput, labelwidget = labellblfPoptemplateObj)

                    ObjectName <- tclVar("")
                    entry.ObjectName <-ttkentry(lblfPoptemplateObj,width="20",textvariable=ObjectName, state= "disabled", font="TkDefaultFont")

                    Ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_Poptemplate(textCalmodel, textPartition, ObjectName, ttPoptemplate))

                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttPoptemplate))


                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_Poptemplate), command=function() fOnFunctionHelp(label_Poptemplate))

                    tkgrid(tk2label(frameGlobal, text="Sample Data", font=fontTextTitle, foreground= "blue"),  pady=c("0.2c",0))

                    tkgrid(lblfSurveyData)
                    tkgrid(lblSurveyDataObj, lblVariables)

                    tkgrid(lblPopSurveyDataObj)

                    tkgrid(lstSurveydataObj, scrSurveydataObj)

                    tkgrid.configure(lblSurveyDataObj, column=1, sticky ="e")
                    tkgrid.configure(lblVariables, column=4)
                    tkgrid.configure(lblPopSurveyDataObj, row=2, column=1, sticky ="e")

                    tkgrid.configure(lstSurveydataObj, column=1, row=3, sticky ="e", padx=c("1.7c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrSurveydataObj, column=2, row=3, sticky ="nsw", padx=c(0,"1.9c"), pady=c(0,"0.2c"))

                    tkgrid(Plus.but, Times.but, Colon.but, Minus.but, LeftParen.but, RightParen.but, Collapse.but, sticky ="e")
                    tkgrid(rbCalmodel,lblCalmodelForm, entry.CalmodelForm, OkCalmodelForm.but)

                    tkgrid.configure(rbCalmodel, padx=c("0.2c",0), pady=c(0,"1.9c"))
                    tkgrid.configure(lblCalmodelForm, pady=c(0,"1.9c"))
                    tkgrid.configure(entry.CalmodelForm, pady=c(0,"1.9c"))
                    # Here layout in pixels: should translate in cm (which is our standard)...
                    tkgrid.configure(OkCalmodelForm.but, padx=c(5,5), pady=c(0,"1.9c"))

                    tkgrid(rbPartition,lblPartitionForm, entry.PartitionForm, OkPartitionForm.but)

                    tkgrid.configure(rbPartition, padx=c("0.2c",0), pady=c(0,"1.1c"))
                    tkgrid.configure(lblPartitionForm, pady=c(0,"1.1c"))
                    tkgrid.configure(entry.PartitionForm, pady=c(0,"1.1c"))
                    # Here layout in pixels: should translate in cm (which is our standard)...
                    tkgrid.configure(OkPartitionForm.but, padx=c(5,5), pady=c(0,"1.1c"))

                    tkgrid(frameTop)
                    tkgrid(lblMandatory, pady=c("0.2c",0))

                    tkgrid(frameCentral)
                    tkgrid.configure(frameCentral, padx="0.5c")

                    tkgrid(lblfFormulaComp, lblfCalmodelPartition, sticky="ns")

                    tkgrid.configure(lblfCalmodelPartition,padx=c("0.25","0.3c"))

                    tkgrid(lblfCalmodel)
                    tkgrid(lblfPartition)

                    tkgrid.configure(lblfCalmodel,pady=c(20,10))
                    tkgrid.configure(lblfPartition,padx=c(15,15))

                    tkgrid(frameCalmodel)
                    tkgrid(textCalmodel,scrtextCalmodel)
                    # Enable ctrl-c on textCalmodel
                    ctrl.c(textCalmodel)
                    tkgrid.configure(scrtextCalmodel, sticky ="nsw")

                    tkgrid(framePartition)
                    tkgrid(textPartition,scrtextPartition)
                    # Enable ctrl-c on textPartition
                    ctrl.c(textPartition)
                    tkgrid.configure(scrtextPartition, sticky ="nsw")

                    tkgrid.configure(lblfFormulaComp,padx=c("0.3c","0.25c"))

                    tkgrid.configure(outerOperatorsFrame,  pady=c("0.3c","0.2c"))
                    tkgrid(outerOperatorsFrame)
                    tkgrid(FormulaFrame)

                    tkgrid(lblfPoptemplateObj)
                    tkgrid.configure(entry.ObjectName, padx=c("0.5c","6.5c"),pady=c(0,"0.3c"))

                    tkgrid(Ok.but, Cancel.but, FunctionHelp.but)
                    tkgrid.configure(Cancel.but, padx=("0.5c"))
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))
                    tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")
                    tkgrid.configure(frameButtons, sticky="ne")
                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttPoptemplate))) # To asses the size of the window...
                    # tkfocus(ttPoptemplate)

            }

            fElencoCampiPoptemplate <- function(EC_lista, EC_lstSurveydataObj, EC_lblVariables, x,
                    lstEC_SurveydataObj, scrEC, EC_lblPopSurveyDataObj, VarPopSurveyDataObj, EC_entry.CalmodelForm,
                    EC_Plus.but, EC_Times.but, EC_Colon.but, EC_Minus.but, EC_LeftParen.but, EC_RightParen.but, EC_Collapse.but,
                    EC_OkCalmodelForm.but, EC_entry.PartitionForm, EC_OkPartitionForm.but, EC_rbCalmodel, EC_rbPartition,
                    EC_lblPartitionForm, EC_lblCalmodelForm, EC_Ok.but, EC_textCalmodel, EC_textPartition,
                    EC_entry.ObjectName){
                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstSurveydataObj)))

                    assignTemp("Scelta_SurveydataObj", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstSurveydataObj))+1])

                    VarPopSurveyDataObj <<- tclVar(Scelta_SurveydataObj)

                    tkconfigure(EC_lblPopSurveyDataObj, textvariable= VarPopSurveyDataObj)

                    # DIRE A RAFFAELLA 16/07/10
                    # NO!!! Copying may become a serious concern when data get big
                    # (can tolerate it for names, because they are small: see above)
                    # assignTemp("SurveydataObj", get(Scelta_SurveydataObj, envir=.GlobalEnv))
                    SurveydataObj <- get(Scelta_SurveydataObj, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    if (Index_dataframe_old !=EC_indicesel){
                        count <<- FALSE
                    }

                    if (count == FALSE){
                            count <<- TRUE
                            Index_dataframe_old <<- EC_indicesel

                            tkconfigure(EC_textCalmodel, state="normal")
                            tkdelete(EC_textCalmodel, "1.0", "end")
                            tkconfigure(EC_textCalmodel, state="disabled")

                            tkconfigure(EC_textPartition, state="normal")
                            tkdelete(EC_textPartition, "1.0", "end")
                            tkinsert(EC_textPartition, "end", "FALSE")
                            tkconfigure(EC_textPartition, state="disabled")

                            tkconfigure(EC_rbPartition, state = "normal")
                            tkconfigure(EC_rbCalmodel, state = "normal")
                            tkconfigure(EC_lblCalmodelForm, state = "normal")
                            tkconfigure(EC_OkCalmodelForm.but, state = "normal")
                            tkconfigure(EC_entry.CalmodelForm, state = "normal")
                            tkconfigure(EC_Plus.but, state = "normal")
                            tkconfigure(EC_Times.but, state = "normal")
                            tkconfigure(EC_Colon.but, state = "normal")
                            tkconfigure(EC_Minus.but, state = "normal")
                            tkconfigure(EC_LeftParen.but, state = "normal")
                            tkconfigure(EC_RightParen.but, state = "normal")
                            tkconfigure(EC_Collapse.but, state = "normal")

                            if (tclvalue(rbValueCalPart)=="ParChoice"){
                                rbValueCalPart <<- tclVar("CalChoice")
                                tkconfigure(EC_rbPartition,variable=rbValueCalPart,value="ParChoice")
                                tkconfigure(EC_rbCalmodel,variable=rbValueCalPart, value="CalChoice")

                                tkconfigure(EC_lblPartitionForm, state = "disabled")
                                tkdelete(EC_entry.PartitionForm, 0, "end")
                                tkconfigure(EC_entry.PartitionForm, state = "disabled")
                                tkconfigure(EC_OkPartitionForm.but, state = "disabled")
                                tkconfigure(EC_lblCalmodelForm, state = "normal")

                                tkconfigure(EC_entry.CalmodelForm, state = "normal")
                                tkdelete(EC_entry.CalmodelForm, 0, "end")
                                tkconfigure(EC_OkCalmodelForm.but, state = "normal")
                                tkfocus(EC_entry.CalmodelForm)
                            }
                            else{
                                tkdelete(EC_entry.CalmodelForm, 0, "end")
                                tkconfigure(EC_entry.PartitionForm, state = "normal")
                                tkdelete(EC_entry.PartitionForm, 0, "end")
                                tkconfigure(EC_entry.PartitionForm, state = "disabled")

                            }

                            tkdelete(EC_entry.ObjectName, 0, "end")
                            tkconfigure(EC_Ok.but, state = "disabled")

                            if (is.data.frame(SurveydataObj)){
# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                                for (n in names(SurveydataObj)){
#                                tclObj(x) <- c(names(SurveydataObj))
#                                }
                                tclObj(x) <- names(SurveydataObj)

                            }
                            else{
# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                                for (n in names(SurveydataObj$variables)){
#                                            tclObj(x) <- c(names(SurveydataObj$variables))
#                                }
                                tclObj(x) <- names(SurveydataObj$variables)
                            }
                    }

                    tkgrid(lstEC_SurveydataObj, scrEC)

                    tkgrid.configure(lstEC_SurveydataObj, column=4, row=3, sticky ="e", pady=c(0,"0.2c"))
                    tkgrid.configure(scrEC, column=5, row=3, padx=c("0c","1.7c"), pady=c(0,"0.2c"), sticky ="nsw")
                    tkconfigure(EC_lblVariables, text="Variables", state = "normal")

            }


            fPoptemplateInsertVariable <- function(EC_lstVariables, EC_listaVariables, EC_entry.CalmodelForm,
                    EC_entry.PartitionForm, EC_ObjectPartitionF, VarPopSurveyDataObj, EC_lblPopSurveyDataObj,
                    EC_ObjectCalmodelF){

                    VarPopSurveyDataObj <<- tclVar(Scelta_SurveydataObj)
                    tkconfigure(EC_lblPopSurveyDataObj, textvariable= VarPopSurveyDataObj)

                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstVariables)))

                    assignTemp("Poptemplate_Variables", as.character(tclObj(EC_listaVariables))[as.integer(tkcurselection(EC_lstVariables))+1])
                    if (tclvalue(rbValueCalPart)=="CalChoice"){
                        tkfocus(EC_entry.CalmodelForm)
                        entry <- tclvalue(tkget(EC_entry.CalmodelForm))
                        lenghtentry <- nchar(entry)

                        if (lenghtentry > 0){
                            tclvalue(EC_ObjectCalmodelF) <- paste(entry, Poptemplate_Variables, sep="")
                        }
                        else{
                            tclvalue(EC_ObjectCalmodelF) <- paste(Poptemplate_Variables, sep="")
                        }
                        tkconfigure(EC_entry.CalmodelForm, textvariable=tclVar(tclvalue(EC_ObjectCalmodelF)))

                        lenghtEC_ObjectCalmodelF <- nchar(EC_ObjectCalmodelF)
                        tkicursor(EC_entry.CalmodelForm , "end")
                        tkxview.moveto(EC_entry.CalmodelForm , 1.0)
                    }
                    else{
                        tkfocus(EC_entry.PartitionForm)
                        entry <- tclvalue(tkget(EC_entry.PartitionForm))
                        lenghtentry <- nchar(entry)

                        if (lenghtentry > 0){
                            tclvalue(EC_ObjectPartitionF) <- paste(entry, Poptemplate_Variables, sep="")
                        }
                        else{
                            tclvalue(EC_ObjectPartitionF) <- paste(Poptemplate_Variables, sep="")
                        }
                        tkconfigure(EC_entry.PartitionForm, textvariable=tclVar(tclvalue(EC_ObjectPartitionF)))
                        lenghtEC_ObjectPartitionF <- nchar(EC_ObjectPartitionF)
                        tkicursor(EC_entry.PartitionForm , "end")
                        tkxview.moveto(EC_entry.PartitionForm , 1.0)
                    }
            }

            fPoptemplateOnCalmodelForm <- function(EC_ObjectCalmodelF, EC_entry.CalmodelForm, EC_Ok.but, EC_textCalmodel, EC_entry.ObjectName, parent = "."){

                    EC_ObjectCalmodelF <- tkget(EC_entry.CalmodelForm)

                    if (tclvalue(EC_ObjectCalmodelF) == ""){
                        tkmessageBox(title="Calmodel formula",message="Calmodel formula is empty!",icon="error",type="ok", parent = parent)
                    }
                    else{
                        CalmodelFormula <- tclvalue(EC_ObjectCalmodelF)
                        CalmodelFormula <- Lancia(as.formula(paste("~", CalmodelFormula), env = .GlobalEnv), textWarnings, parent = parent)
                        if (!inherits(CalmodelFormula, "try-error")){
                            CalmodelFormula <- form.to.char(CalmodelFormula)
                        }
                        else return()

                        tkconfigure(EC_textCalmodel, state="normal")
                        tkdelete(EC_textCalmodel, "1.0", "end")
                        tkinsert(EC_textCalmodel, "end", CalmodelFormula)
                        tkconfigure(EC_textCalmodel, state="disabled")

                        tkconfigure(EC_entry.CalmodelForm, textvariable=tclVar(""))

                        if (count)
                            tkconfigure(EC_Ok.but, state = "normal")
                            tkconfigure(EC_entry.ObjectName, state = "normal")

                    }
            }

            fPoptemplateOnPartitionForm <- function(EC_ObjectPartitionF, EC_entry.PartitionForm,
                    EC_textPartition){

                    EC_ObjectPartitionF <- tkget(EC_entry.PartitionForm)

                    if (tclvalue(EC_ObjectPartitionF) == ""){
                        PartitionFormula <- "FALSE"
                    }
                    else{
                        PartitionFormula <- tclvalue(EC_ObjectPartitionF)
                        PartitionFormula <- paste("~", PartitionFormula)
                        }
                        tkconfigure(EC_textPartition, state="normal")
                        tkdelete(EC_textPartition, "1.0", "end")
                        tkinsert(EC_textPartition, "end", PartitionFormula)
                        tkconfigure(EC_textPartition, state="disabled")
                        tkconfigure(EC_entry.PartitionForm, textvariable=tclVar(""))
            }

            fOnRun_Poptemplate <- function(OR_textCalmodel, OR_textPartition, OR_PoptemplateObjectName, OR_ttPoptemplate){
                    campobb <- TRUE
                    calmodel <- tclvalue(tkget(OR_textCalmodel, "1.0", "end -1 chars"))
                    calmodel  <- Lancia(as.formula(calmodel, env = .GlobalEnv), textWarnings, parent = OR_ttPoptemplate)

                    if (!inherits(calmodel,"try-error")){
                        prnCalmodel <- tclvalue(tkget(OR_textCalmodel, "1.1", "end -1 chars"))
                        prnCalmodel <- paste("calmodel=", prnCalmodel, sep =" ~")
                    }
                    else{
                        campobb <- FALSE
                    }

                    partition <- as.character(tclvalue(tkget(OR_textPartition, "1.0", "end -1 chars")))
                    if (partition!= "FALSE"){
                        partition  <- Lancia(as.formula(partition, env = .GlobalEnv), textWarnings, parent = OR_ttPoptemplate)

                        if (!inherits(partition,"try-error")){
                            prnPartition <- tclvalue(tkget(OR_textPartition, "1.1", "end -1 chars"))
                            prnPartition <- paste("partition=", prnPartition, sep =" ~")
                        }
                        else{
                            campobb <- FALSE
                            }
                        }
                        else{
                            partition <- FALSE
                            prnPartition <- paste("partition=", "FALSE")
                        }

                        if (tclvalue(OR_PoptemplateObjectName)==""){
                            tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = OR_ttPoptemplate)
                            campobb <- FALSE
                        }
                        else{
                            if   ( !is.ok.name(tclvalue(OR_PoptemplateObjectName), go.on=campobb,
                                               parent=OR_ttPoptemplate) ){
                                 campobb <- FALSE
                                 }
                            else {
                                 OR_PoptemplateObjectName <- tclvalue(OR_PoptemplateObjectName)
                                 }
                            }

                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(OR_ttPoptemplate, cursor="watch")

                        # DIRE A RAFFAELLA 16/07/10
                        # SurveydataObj no more exists into TempEnv: we left inside its name only
                        SurveydataObj <- get(Scelta_SurveydataObj, envir=.GlobalEnv)
                        # DIRE A RAFFAELLA 16/07/10

                        outPop <- Lancia(pop.template(SurveydataObj, calmodel, partition), textWarnings, parent = OR_ttPoptemplate)

                        if (!inherits(outPop,"try-error")) {
                            # Code below may seem weird, but it handles metadata correctly
                            outPop.fix <- outPop
                            fixRG(outPop.fix)
                            unchanged <- identical(data.frame(outPop), data.frame(outPop.fix))
                            if (!unchanged) outPop[,] <- outPop.fix[,]
                            # assign(OR_PoptemplateObjectName, outPop, envir = .GlobalEnv)
                            assign2GE(OR_PoptemplateObjectName, outPop)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            if (!is.big(outPop)) {
                                cat(paste("# ",OR_PoptemplateObjectName,"\n",sep=""))
                            }
                            else {
                                if (is.large(outPop)) {
                                     cat(paste("# head(",OR_PoptemplateObjectName,"[, 1:10])\n",sep=""))
                                    }
                                else {
                                     cat(paste("# head(",OR_PoptemplateObjectName,")\n",sep=""))
                                    }
                            }
                            PopTemp.onscreen(outPop, OR_PoptemplateObjectName, show = !unchanged)
                            cat("\n")

                            prnData <- paste("data=",Scelta_SurveydataObj)
                            prnPoptemplate<- paste(" <- pop.template(", prnData, ", ", prnCalmodel, ", ", prnPartition, ")", sep="")

                            commands <- paste(OR_PoptemplateObjectName, prnPoptemplate, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (!unchanged){
                                commands <- paste(OR_PoptemplateObjectName, " <- fix(",
                                                  OR_PoptemplateObjectName, ")", "\n", sep="")

                                # Print on the Commands Window
                                tkinsert(textHistory, "end", commands)
                                tkinsert(textHistory, "end", "\n")
                                tkyview.moveto(textHistory, 1.0)
                                # End

                                # Flush commands to Rhistory file
                                upd.Rhistory(commands, RG.stamp = TRUE)
                                # End
                                }

                            tkconfigure(textHistory, state="disabled")
                            tkgrab.release(OR_ttPoptemplate)
                        }
                    # get back the standard arrow cursor
                    tkconfigure(OR_ttPoptemplate, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# -------------------------------------
# < END building pop.template window. <
# -------------------------------------

# -----------------------------------
# > START building pop.desc window. >
# -----------------------------------

fPop.desc <- function() {
#######################################################
# Build a List Box (with scrollbar) storing the names #
# of the objects inheriting from class pop.totals     #
# which are found inside the current Workspace.       #
# One of them is eventually selected, and a natural   #
# language description of its structure is printed.   #
#######################################################

# Seek object in Workspace...
obj.names <- ls(.GlobalEnv)
if (length(obj.names)==0) {
    tkmessageBox(title="Known Totals Description",
                 message = "No objects found in the current Workspace!",
                 icon = "warning")
    return(invisible(NULL))
    }
# Check for objects inheriting from pop.totals...
obj.mainclass <- sapply(obj.names, function(x) data.class(eval(parse(text=x), envir = .GlobalEnv)))
pop.obj <- obj.names[obj.mainclass == "pop.totals"]
if ( length(pop.obj)==0 ) {
    tkmessageBox(title="Known Totals Description",
                 message = "No known totals data frames found in the current Workspace!",
                 icon = "warning")
    return(invisible(NULL))
    }
pop.obj <- sort(pop.obj)

################################################################################
# Build the modal List Box (with scrollbar): select one element and return it. #
################################################################################

  tt <- tktoplevel()
  tkgrab.set(tt)
  tkwm.title(tt, "Known Totals Description")
  box <- tkframe(tt)
  butts <- tkframe(tt)
  scr <- tkscrollbar(box, repeatinterval=5,
                     command=function(...)tkyview(tl,...))
  tl <- tklistbox(box, height=10, selectmode="single",
                  yscrollcommand=function(...)tkset(scr,...), background="white")

  for (el in pop.obj){
      tkinsert(tl,"end", el)
      }
  # Default Dataset is the first of the list
  tkselection.set(tl, 0)

  tkgrid(tklabel(tt, text = " "))
  tkgrid(tklabel(tt, text = "Please select a known totals data frame"))
  tkgrid(tklabel(tt, text = " "))
  tkgrid(box)
  tkgrid(tl, scr)
  tkgrid.configure(tl, sticky="ew", padx=c("1c",0))
  tkgrid.configure(scr, sticky="nsw", padx=c(0,"1c"))

  Choice <- ""
  OnOK <- function(tt) {
      Choice <<- pop.obj[as.numeric(tkcurselection(tl))+1]
      tkgrab.release(tt)
      tkdestroy(tt)
      }

  OK.but <- tk2button(butts,text="   OK   ", image=image_ok,
                      compound="left", command =function() OnOK(tt))
  Canc.but <- tk2button(butts,text="Cancel", image=image_cancel,
                        compound="left", command = function() fOnCancel(tt))
  FunctionHelp.but <- tk2button(butts, text="Function Help",
                                image=image_qm, compound="left", tip=descFun(label_PopDesc),
                                command=function() fOnFunctionHelp(label_PopDesc))

  tkgrid(butts)
  tkgrid.configure(butts, pady=c("0.4c", "0.2c"))
  tkgrid(OK.but, Canc.but, FunctionHelp.but)
  tkgrid.configure(OK.but, padx=c("0.2c",0))
  tkgrid.configure(FunctionHelp.but, padx=c(0, "0.2c"))
  tkfocus(tt)
  tkwm.resizable(tt, 0, 0)
  tkwait.window(tt)
  # If no choice, exit
  if (Choice == ""){
     return()
    }
# End list box

# Now print on screen the description
    PopObj <- get(Choice, envir=.GlobalEnv)
    cat("\n")
    cat(paste("# Natural language description of object: ", Choice,"\n",sep=""))
    cat("\n")
    # Recall that pop.desc prints by its own...
    valPopDesc <- Lancia(pop.desc(PopObj), textWarnings)

    if  (!inherits(valPopDesc,"try-error")) {

        commands <- paste("pop.desc(", Choice, ")\n", sep="")

        # Print on the Commands Window
        tkconfigure(textHistory, state="normal")
        tkinsert(textHistory, "end", commands)
        tkinsert(textHistory, "end", "\n")
        tkyview.moveto(textHistory, 1.0)
        tkconfigure(textHistory, state="disabled")
        # End

        # Flush commands to Rhistory file
        upd.Rhistory(commands, RG.stamp = TRUE)
        # End

        if (getTemp("there.are.warnings", default = FALSE)){
            tkmessageBox(title ="Known Totals Description",
                         message = "Operation executed\nNote: Warnings have been generated!",
                         icon = "info")
            }
        else {
              tkmessageBox(title ="Known Totals Description",message = "Operation executed", icon = "info")
             }
    }
    else{
        cat(valPopDesc)
        cat("\n")
    }
}

# ---------------------------------
# < END building pop.desc window. <
# ---------------------------------

# ----------------------------------------
# > START building fill.template window. >
# ----------------------------------------

#            fFill.template <- function() {
#                tkmessageBox(title ="Fill Template", message = "Sorry, this item is still under construction",
#                    icon = "info")
#            }

            fFill.template <- function(){
                    listDfpopulation <- mk.class.list("pop.totals")
                    listUnivObj <- listDataSets()
                    ttFillTemplate <- tktoplevel()
                    tcl("wm", "protocol", ttFillTemplate, "WM_DELETE_WINDOW", function() fOnCancel(ttFillTemplate))
                    frameGlobal<- tkframe(ttFillTemplate, borderwidth= 2)
                    tkwm.deiconify(ttFillTemplate)
                    tkgrab.set(ttFillTemplate)
                    tkfocus(ttFillTemplate)
                    tkwm.title(ttFillTemplate,label_FillTemplate)
                    tkwm.resizable(ttFillTemplate, 0, 0)

                    frameTop <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameDown <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameOutput <- tkframe(frameGlobal, borderwidth=0)
                    frameButtons <- tkframe(frameGlobal, borderwidth=0)

                    lblDfpopulation <- ttklabel(frameTop, text="Select a template",
                                                font=fontTextLabel)
                    lblUniv <- ttklabel(frameTop, text="Select a universe",
                                        font=fontTextLabel)

                    lblFillDfpopulation <- ttklabel(frameTop,
                                                    textvariable=as.character(tclvalue(VarFillDfpopulation)),
                                                    foreground="red")
                    lblFillUniv <- ttklabel(frameTop,
                                            textvariable=as.character(tclvalue(VarFillUniv)),
                                            foreground="red")

                    lblMandatory <- tk2label(frameGlobal,text="Formula Fields",
                                            font=fontTextTitle, foreground= "blue")
                    lblOptional <-tk2label(frameGlobal,text="Optional Fields",
                                            font=fontTextTitle, foreground= "blue")

                    scrDfpopulation <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstDfpopulation,...))
                    listaDfp <- tclVar()
                    tclObj(listaDfp) <- listDfpopulation
                    lstDfpopulation <- tklistbox(frameTop,height = 4, listvariable= listaDfp, selectmode="single", yscrollcommand = function (...)tkset(scrDfpopulation,...), background = "white")

                    scrUnivObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstUnivObj,...))
                    lista <- tclVar()
                    tclObj(lista) <- listUnivObj
                    lstUnivObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="single", yscrollcommand = function (...)tkset(scrUnivObj,...), background = "white")


                    tkbind(lstDfpopulation, "<ButtonRelease-1>", function() fClassControlFillTemp(lstDfpopulation, listaDfp, textCalmodel, textPartition, VarFillDfpopulation, lblFillDfpopulation,
                            ok.but, entry.ObjectName))
                    count_scelta <<- FALSE

                    tkbind(lstUnivObj, "<ButtonRelease-1>", function() fElencoCampiFillTemplate(lista, lstUnivObj, lblFillUniv, VarFillUniv, ok.but, entry.ObjectName))
                    count_FillTemplate <<- FALSE

                    labellblfCalmodel <- ttklabel(frameCentral,text="  calmodel  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("pop.template", args = "calmodel")
                    tk2tip(labellblfCalmodel,descfunz)
                    lblfCalmodel<- ttklabelframe(frameCentral, labelwidget=labellblfCalmodel)

                    # Here layout in pixels: should translate in cm (which is our standard)...
                    frameCalmodel<- tkframe(lblfCalmodel, borderwidth= 2, padx=5)
                    scrtextCalmodel <- tkscrollbar(frameCalmodel, orient = "vertical", command=function(...) tkyview(textCalmodel, ...))
                    textCalmodel <- tktext(frameCalmodel, foreground="red", background= "white", height=4, width=40, yscrollcommand=function(...) tkset(scrtextCalmodel, ...),
                    state="disabled", wrap="word", font=fontTextExp)

                    labellblfPartition <- ttklabel(frameCentral,text="   partition   ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("pop.template", args = "partition")
                    tk2tip(labellblfPartition,descfunz)
                    lblfPartition<- ttklabelframe(frameCentral, labelwidget=labellblfPartition)

                    # Here layout in pixels: should translate in cm (which is our standard)...
                    framePartition<- tkframe(lblfPartition, borderwidth= 2, padx=5)
                    scrtextPartition <- tkscrollbar(framePartition, orient = "vertical", command=function(...) tkyview(textPartition, ...))
                    textPartition <- tktext(framePartition, foreground="red", background= "white", height=3, width=40, yscrollcommand=function(...) tkset(scrtextPartition, ...),
                    state="disabled", wrap="word", font=fontTextExp)

                    labellblfMemFrac <- ttklabel(frameDown,text="  mem.frac  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("fill.template", args = "mem.frac")
                    tk2tip(labellblfMemFrac,descfunz)
                    lblfMemFrac<- ttklabelframe(frameDown, labelwidget=labellblfMemFrac)

                    # Slider for parameter mem.frac
                    # Meaningful integer values for mem.frac (10, to date)
                    allMemFrac <- c(0,2,5,10,20,50,100,200,500,1000)
                    # Slider variable (identifies the actual element of allMemFrac...)
                      # Initialization:
                      SliderValue <- tclVar("4")
                      MemFrac <- tclVar(as.character(allMemFrac[as.integer(tclvalue(SliderValue))]))
                      # DEBUG 08/05/2020: due to R 4.0.0 had to explicitly use as.character(tclvalue())
                      entry.MemFrac <- tklabel(lblfMemFrac, text=as.character(tclvalue(MemFrac)), foreground="red")
                    # Update MemFrac when using the slider...
                    upd <- function(...) {
                         # NOTE: global tcl variable MemFrac will be then used by fOnRun_FillTemplate
                         MemFrac <<- tclVar(as.character(allMemFrac[as.integer(tclvalue(SliderValue))]))
                         tkconfigure(entry.MemFrac, textvariable=MemFrac)
                        }
                    # Generate the slider widget
                    MemSlider <- tkscale(lblfMemFrac, from=1, to=length(allMemFrac), command = upd,
                                         showvalue=F, variable=SliderValue,
                                         resolution=1, orient="horizontal")
                    # Slider ends.

                    labellblfFillTemplateObj <- ttklabel(frameOutput,text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfFillTemplateObj<- ttklabelframe(frameOutput, labelwidget = labellblfFillTemplateObj)

                    ObjectName <- tclVar("")
                    entry.ObjectName <-ttkentry(lblfFillTemplateObj,width="20",text=ObjectName, state= "disabled", font="TkDefaultFont")

                    ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_FillTemplate(MemFrac, ObjectName,    ttFillTemplate))

                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttFillTemplate))

                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_FillTemplate), command=function() fOnFunctionHelp(label_FillTemplate))


                    tkgrid(tk2label(frameGlobal, text="Mandatory Fields", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))

                    tkgrid(lblUniv, lblDfpopulation)
                    tkgrid(lblFillUniv, lblFillDfpopulation)

                    tkgrid(lstUnivObj, scrUnivObj, lstDfpopulation, scrDfpopulation)

                    tkgrid.configure(lblDfpopulation, column=1, sticky ="e")

                    tkgrid.configure(lblUniv, column=3, sticky ="e")

                    tkgrid.configure(lblFillDfpopulation, column=1, sticky ="e")
                    tkgrid.configure(lblFillUniv, column= 3, sticky ="e")

                    tkgrid.configure(lstDfpopulation, column=1, row=3, sticky ="e", padx=c("1.5c",0), pady=c(0,"0.2c"))
                    tkgrid.configure(scrDfpopulation, column=2, row=3, sticky ="nsw", padx=c(0,"1c"), pady=c(0,"0.2c"))

                    tkgrid.configure(lstUnivObj, column=3, row=3, sticky ="e", padx=c("1c",0), pady=c(0,"0.2c"))
                    tkgrid.configure(scrUnivObj, column=4, row=3, sticky ="nsw", padx=c(0,"1.5c"), pady=c(0,"0.2c"))

                    tkgrid(lblfMemFrac)
                    tkgrid.configure(lblfMemFrac, padx="0.3c", pady="0.2c")
                    tkgrid(entry.MemFrac)
                    tkgrid.configure(entry.MemFrac, sticky="we")
                    tkgrid(MemSlider)
                    tkgrid.configure(MemSlider, sticky="we", padx="0.2c", pady=c(0,"0.2c"))

                    tkgrid(frameTop, padx="0.2c")
                    tkgrid(lblMandatory, pady=c("0.2c",0))

                    tkgrid(frameCentral)

                    tkgrid(lblfCalmodel)
                    tkgrid(lblfPartition)

                    tkgrid.configure(lblfCalmodel,pady=c("0.3c","0.2c"), padx="0.4c")
                    tkgrid.configure(lblfPartition, pady=c("0.2c","0.3c"), padx="0.4c")


                    tkgrid(frameCalmodel)
                    tkgrid(textCalmodel,scrtextCalmodel)
                    tkgrid.configure(textCalmodel, pady="0.2c")
                    # Enable ctrl-c on textCalmodel
                    ctrl.c(textCalmodel)
                    tkgrid.configure(scrtextCalmodel, sticky ="nsw")

                    tkgrid(framePartition)
                    tkgrid(textPartition,scrtextPartition)
                    tkgrid.configure(textPartition, pady="0.2c")
                    # Enable ctrl-c on textPartition
                    ctrl.c(textPartition)
                    tkgrid.configure(scrtextPartition, sticky ="nsw")

                    tkgrid(lblOptional, pady=c("0.2c",0))
                    tkgrid(frameDown)
                    tkgrid.configure(frameDown, padx="0.5c")

                    tkgrid(lblfFillTemplateObj)

                    tkgrid.configure(entry.ObjectName, padx="0.5c",pady=c(0,"0.3c"))
                    tkgrid.configure(frameOutput, padx=c("0.2c",0), pady=c("0.2c","0.2c"), sticky="w")

                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)
                    tkgrid.configure(Cancel.but, padx="0.2c")
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.2c"))
                    tkgrid.configure(frameButtons, sticky="ne")

                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttFillTemplate))) # To asses the size of the window...
                    # tkfocus(ttFillTemplate)
            }


            fElencoCampiFillTemplate <- function(EC_lista, EC_lstUnivObj,
                    EC_lblFillUniv, VarFillUniv,
                    EC_ok.but, EC_entry.ObjectName){

                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstUnivObj)))

                    assignTemp("Scelta_FillTemplateObj", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstUnivObj))+1])

                    VarFillUniv <<- tclVar(Scelta_FillTemplateObj)
                    tkconfigure(EC_lblFillUniv, textvariable= VarFillUniv)

                    if (Index_dataframe_FillTemplate !=EC_indicesel){
                        count_FillTemplate <<- FALSE
                    }
                    if (count_FillTemplate == FALSE){
                            count_FillTemplate <<- TRUE
                            Index_dataframe_FillTemplate <<- EC_indicesel
                            tkdelete(EC_entry.ObjectName, 0, "end")
                    }

                    if (count_scelta==TRUE && count_FillTemplate==TRUE){
                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")
                    }
                    else{
                        tkconfigure(EC_ok.but, state = "disabled")
                        tkdelete(EC_entry.ObjectName, 0, "end")
                        tkconfigure(EC_entry.ObjectName, state = "disabled")
                    }
            }

            fClassControlFillTemp <- function(EC_lstDfpopulation, xDf,
                    EC_textCalmodel, EC_textPartition, VarFillDfpopulation, EC_lblFillDfpopulation,
                    EC_ok.but, EC_entry.ObjectName ){

                    EC_indicescelta <- as.character(tclvalue(tkcurselection(EC_lstDfpopulation)))

                    assignTemp("choiceDf", as.character(tclObj(xDf))[as.integer(tkcurselection(EC_lstDfpopulation))+1])

                    scelta <- get(choiceDf, envir=.GlobalEnv)

                    VarFillDfpopulation <<- tclVar(choiceDf)
                    tkconfigure(EC_lblFillDfpopulation, textvariable= VarFillDfpopulation)

                    if (Index_choice_FillTemplate!=EC_indicescelta){
                        count_scelta <<- FALSE
                    }
                    if (count_scelta == FALSE){
                        count_scelta <<- TRUE
                        Index_choice_FillTemplate <<- EC_indicescelta
                        tkdelete(EC_entry.ObjectName, 0, "end")
                    }

                    if (count_scelta==TRUE && count_FillTemplate==TRUE){
                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")
                    }
                    else{
                        tkconfigure(EC_ok.but, state = "disabled")
                        tkdelete(EC_entry.ObjectName, 0, "end")
                        tkconfigure(EC_entry.ObjectName, state = "disabled")
                    }

                    tkconfigure(EC_textCalmodel, state="normal")
                    tkdelete(EC_textCalmodel, "1.0", "end")
                    tkconfigure(EC_textPartition, state="normal")
                    tkdelete(EC_textPartition, "1.0", "end")

                    VarCalmodel <<- attr(scelta,"calmodel")
                    VarCalmodel <<- tclvalue(tclVar( form.to.char(VarCalmodel) ))

                    tkinsert(EC_textCalmodel, "end", VarCalmodel)
                    tkconfigure(EC_textCalmodel, state="disabled")

                    VarPartition <<- attr(scelta,"partition")

                    if (VarPartition=="FALSE"){
                        VarPartition <<- tclvalue(tclVar(as.character(VarPartition)))

                        tkinsert(EC_textPartition, "end", VarPartition)
                        tkconfigure(EC_textPartition, state="disabled")
                    }
                    else{
                        VarPartition <<- tclvalue(tclVar( form.to.char(VarPartition) ))
                        tkconfigure(EC_textPartition, state="normal")
                        tkinsert(EC_textPartition, "end", VarPartition)
                        tkconfigure(EC_textPartition, state="disabled")
                    }
            }


            fOnRun_FillTemplate <- function(OR_MemFrac, OR_FillTemplateObjectName,ttFillTemplate){

                    campobb <- TRUE

                    scelta <- get(choiceDf, envir=.GlobalEnv)

                        calmodel <- attr(scelta,"calmodel")

                        prnCalmodel <- form.to.char(calmodel)
                        prnCalmodel <- paste("calmodel=", prnCalmodel)

                        partition <- attr(scelta,"partition")
                        if (partition=="FALSE"){
                            prnPartition <- paste("partition=", "FALSE")
                        }
                        else{
                            prnPartition <- form.to.char(partition)
                            prnPartition <- paste("partition=", prnPartition)
                        }

                        mem.frac <- as.numeric(tclvalue(OR_MemFrac))
                        prnMemFrac <- paste("mem.frac=", mem.frac)


                    if (tclvalue(OR_FillTemplateObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = ttFillTemplate)
                        campobb <- FALSE
                    }
                    else{
                        if   ( !is.ok.name(tclvalue(OR_FillTemplateObjectName), go.on=campobb,
                                           parent = ttFillTemplate) ){
                             campobb <- FALSE
                             }
                        else {
                             OR_FillTemplateObjectName <- tclvalue(OR_FillTemplateObjectName)
                             }
                        }

                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(ttFillTemplate, cursor="watch")

                        FillTemplateObj <- get(Scelta_FillTemplateObj, envir=.GlobalEnv)

                        outFillTemplate <- Lancia(fill.template(FillTemplateObj, scelta, mem.frac), textWarnings, parent = ttFillTemplate)

                        if (!inherits(outFillTemplate,"try-error")) {
                            prnUniverse <- paste("universe=",Scelta_FillTemplateObj)
                            prnTemplate <- paste("template=",choiceDf)

                            # assign(OR_FillTemplateObjectName, outFillTemplate, envir = .GlobalEnv)
                            assign2GE(OR_FillTemplateObjectName, outFillTemplate)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            if (!is.big(outFillTemplate)) {
                                cat(paste("# ",OR_FillTemplateObjectName,"\n",sep=""))
                            }
                            else {
                                if (is.large(outFillTemplate)) {
                                     cat(paste("# head(",OR_FillTemplateObjectName,"[, 1:10])\n",sep=""))
                                    }
                                else {
                                     cat(paste("# head(",OR_FillTemplateObjectName,")\n",sep=""))
                                    }
                            }
                            PopTemp.onscreen(outFillTemplate, OR_FillTemplateObjectName)
                            cat("\n")

                            prnFillTemplate<- paste(" <- fill.template(", prnUniverse, ", ", prnTemplate, ", ",
                                                prnMemFrac, ")", sep="")

                            commands <- paste(OR_FillTemplateObjectName, prnFillTemplate, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                  tkmessageBox(title ="fill.template",
                                               message = "Operation executed\nNote: Warnings have been generated!",
                                               icon = "info", parent = ttFillTemplate)
                                 }
                            else {
                                  tkmessageBox(title ="fill.template",message = "Operation executed", icon = "info", parent = ttFillTemplate)
                                 }
                            tkgrab.release(ttFillTemplate)
                            }
                    # get back the standard arrow cursor
                    tkconfigure(ttFillTemplate, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# --------------------------------------
# < END building fill.template window. <
# --------------------------------------

# ------------------------------------
# > START building check.cal window. >
# ------------------------------------

fCheck.cal <- function() {
#######################################################
# Build a List Box (with scrollbar) storing the names #
# of the objects inheriting from class cal.analytic   #
# which are found inside the current Workspace.       #
# One of them is eventually selected, and a           #
# Calibration Convergence check is performed on it.   #
#######################################################

# Seek object in Workspace...
obj.names <- ls(.GlobalEnv)
if (length(obj.names)==0) {
    tkmessageBox(title="Calibration Convergence Check",
                 message = "No objects found in the current Workspace!",
                 icon = "warning")
    return(invisible(NULL))
    }
# Check for objects inheriting from cal.analytic...
cal.obj <- obj.names[inherits.obj("cal.analytic")]
if ( length(cal.obj)==0 ) {
    tkmessageBox(title="Calibration Convergence Check",
                 message = "No calibrated design objects found in the current Workspace!",
                 icon = "warning")
    return(invisible(NULL))
    }
cal.obj <- sort(cal.obj)

################################################################################
# Build the modal List Box (with scrollbar): select one element and return it. #
################################################################################

  tt <- tktoplevel()
  tkgrab.set(tt)
  tkwm.title(tt, "Calibration Convergence Check")
  box <- tkframe(tt)
  butts <- tkframe(tt)
  scr <- tkscrollbar(box, repeatinterval=5,
                     command=function(...)tkyview(tl,...))
  tl <- tklistbox(box, height=10, selectmode="single",
                  yscrollcommand=function(...)tkset(scr,...), background="white")

  for (el in cal.obj){
      tkinsert(tl,"end", el)
      }
  # Default Dataset is the first of the list
  tkselection.set(tl, 0)

  tkgrid(tklabel(tt, text = " "))
  tkgrid(tklabel(tt, text = "Please select a calibrated design object"))
  tkgrid(tklabel(tt, text = " "))
  tkgrid(box)
  tkgrid(tl, scr)
  tkgrid.configure(tl, sticky="ew", padx=c("1c",0))
  tkgrid.configure(scr, sticky="nsw", padx=c(0,"1c"))

  Choice <- ""
  OnOK <- function(tt) {
      Choice <<- cal.obj[as.numeric(tkcurselection(tl))+1]
      tkgrab.release(tt)
      tkdestroy(tt)
      }

  OK.but <- tk2button(butts,text="   OK   ", image=image_ok,
                      compound="left", command =function() OnOK(tt))
  Canc.but <- tk2button(butts,text="Cancel", image=image_cancel,
                        compound="left", command = function() fOnCancel(tt))
  FunctionHelp.but <- tk2button(butts, text="Function Help",
                                image=image_qm, compound="left", tip=descFun(label_CheckCal),
                                command=function() fOnFunctionHelp(label_CheckCal))

  tkgrid(butts)
  tkgrid.configure(butts, pady=c("0.4c", "0.2c"))
  tkgrid(OK.but, Canc.but, FunctionHelp.but)
  tkgrid.configure(OK.but, padx=c("0.2c",0))
  tkgrid.configure(FunctionHelp.but, padx=c(0, "0.2c"))
  tkfocus(tt)
  tkwm.resizable(tt, 0, 0)
  tkwait.window(tt)
  # If no choice, exit
  if (Choice == ""){
     return()
    }
# End list box

# Now perform the check and print on screen
    CalObj <- get(Choice, envir=.GlobalEnv)
    cat("\n")
    cat(paste("# Calibration Convergence Check on object: ", Choice,"\n",sep=""))
    print(CalObj)
    cat("\n")
    cat("# Results of the Calibration Convergence Check:\n")
    # Recall that check.cal prints by its own...
    valCheckCal <- Lancia(check.cal(CalObj), textWarnings)

    if  (!inherits(valCheckCal,"try-error")) {

        # assign("last.check.cal", valCheckCal, envir=.GlobalEnv)
        assign2GE("last.check.cal", valCheckCal)

        commands <- paste("check.cal(", Choice, ")\n", sep="")

        # Print on the Commands Window
        tkconfigure(textHistory, state="normal")
        tkinsert(textHistory, "end", commands)
        tkinsert(textHistory, "end", "\n")
        tkyview.moveto(textHistory, 1.0)
        tkconfigure(textHistory, state="disabled")
        # End

        # Flush commands to Rhistory file
        upd.Rhistory(commands, RG.stamp = TRUE)
        # End

        if (getTemp("there.are.warnings", default = FALSE)){
            tkmessageBox(title ="Calibration Convergence Check",
                         message = "Operation executed\nNote: Warnings have been generated!",
                         icon = "info")
            }
        else {
              tkmessageBox(title ="Calibration Convergence Check",message = "Operation executed", icon = "info")
             }
    }
    else{
        cat(valCheckCal)
        cat("\n")
    }
}

# ----------------------------------
# < END building check.cal window. <
# ----------------------------------

# ----------------------------------
# < START building trimcal window. <
# ----------------------------------

# fTrimcal <- function(){
  # tkmessageBox(title ="Trim Calibration Weights", message = "Coming soon!", icon = "info")
# }

			fTrimcal <- function(){
                    listDesign <- mk.class.list("cal.analytic")
                    ttTrimcal <- tktoplevel()
                    tcl("wm", "protocol", ttTrimcal, "WM_DELETE_WINDOW", function() fOnCancel(ttTrimcal))
                    frameGlobal<- tkframe(ttTrimcal, borderwidth= 2)
                    tkwm.deiconify(ttTrimcal)
                    tkgrab.set(ttTrimcal)
                    tkfocus(ttTrimcal)
                    tkwm.title(ttTrimcal,label_Trimcal)
                    tkwm.resizable(ttTrimcal, 0, 0)

                    frameTop <- tkframe(frameGlobal, relief="groove", borderwidth=2, padx="0.8c",
                                        pady=c("0.2c","0.4c"))
                    frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2,
                                        pady=c("0.2c","0.4c"))
                    frameDown <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                    frameOutput <- tkframe(frameGlobal, borderwidth=0)
                    frameButtons <- tkframe(frameGlobal, borderwidth=0)

                    lblDesign <- ttklabel(frameTop, text="Select a calibrated design object",
                                                font=fontTextLabel)

                    lbltrimDesign <- ttklabel(frameTop,
                                                    textvariable=as.character(tclvalue(trim_VarDesign)),
                                                    foreground="red")

                    lblMandatory <- tk2label(frameGlobal,text="Trimming Bounds",
                                            font=fontTextTitle, foreground= "blue")

                    lblOptional <-tk2label(frameGlobal,text="Optional Fields", font=fontTextTitle, foreground= "blue")

                    scrDesign <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstDesign,...))
                    listaDes <- tclVar()
                    tclObj(listaDes) <- listDesign
                    lstDesign <- tklistbox(frameTop,height = 4, listvariable= listaDes, selectmode="single", yscrollcommand = function (...)tkset(scrDesign,...), background = "white")

                    tkbind(lstDesign, "<ButtonRelease-1>", function() ftrim_VarDesign(lstDesign, listaDes, trim_VarDesign, lbltrimDesign, ok.but,
                           entry.ObjectName, entry.BoundsHigh, entry.BoundsLow, textCurrW))
                    count_trimDes <<- FALSE

#------------------------------------------------

                labellblfCurrW <- ttklabel(frameCentral,text="  summary(w)  ", font=fontTextLabel, image=image_qm, compound="right")
                tk2tip(labellblfCurrW, "Summary of the current calibration weights distribution.")
                lblfCurrW<- ttklabelframe(frameCentral, labelwidget=labellblfCurrW)

                # Here layout in pixels: should translate in cm (which is our standard)...
                frameCurrW<- tkframe(lblfCurrW, borderwidth= 2, padx=5)
                scrtextCurrW <- tkscrollbar(frameCurrW, orient = "vertical", command=function(...) tkyview(textCurrW, ...))

                textCurrW <- tktext(frameCurrW, foreground="red", background= "white", height=6, width=28, yscrollcommand=function(...) tkset(scrtextCurrW,
                                    ...), state="disabled", wrap="word", font=fontTextExp)

#------------------------------------------------
                    # labellblfKey <- ttklabel(frameCentral,text="  w.range  ", font=fontTextLabel, image=image_qm, compound="right")
                    # descfunz <- descArgs("trimcal", args = "w.range")
                    # tk2tip(labellblfKey,descfunz)
                    # lblfKey<- ttklabelframe(frameCentral, labelwidget=labellblfKey)

                    # listaKey <- tclVar()
                    # Key <- c("NULL")
                    # tclObj(listaKey) <- Key
                    # cbKey <- tk2combobox(lblfKey, values = Key, state="readonly")
                    # KeyDef <- tclVar("NULL")
#-----------------------------------------------

#-----------------------------------------------

                    labellblfBounds <- ttklabel(frameCentral,text="  w.range  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("trimcal", args = "w.range")
                    tk2tip(labellblfBounds,descfunz)
                    lblfBounds<- ttklabelframe(frameCentral, labelwidget=labellblfBounds)

                    LowHighFrame <- tkframe(lblfBounds)

                    labelBoundsHigh <- ttklabel(LowHighFrame,text="high")
                    labelBoundsLow <- ttklabel(LowHighFrame,text="low")

                    BoundsHigh <- tclVar(Inf)
                    BoundsLow <- tclVar(-Inf)

                    entry.BoundsHigh <- tkentry(LowHighFrame,width="8", background="white") # Here switching to 'ttkentry' would give the desired white bg
                    entry.BoundsLow <- tkentry(LowHighFrame,width="8", background="white")  # but would cause bounds values to be lost upon execution!
                                                                                            # WHY????
#-----------------------------------------------

                    labellblfMaxit <- ttklabel(frameDown,text="  maxit  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("trimcal", args = "maxit")
                    tk2tip(labellblfMaxit,descfunz)
                    lblfMaxit<- ttklabelframe(frameDown, labelwidget=labellblfMaxit)

                    Maxit <- tclVar(50)
                    entry.Maxit <-tkentry(lblfMaxit,width="4",textvariable=Maxit, background="white")

                    labellblfEpsilon <- ttklabel(frameDown,text="  epsilon  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("trimcal", args = "epsilon")
                    tk2tip(labellblfEpsilon,descfunz)
                    lblfEpsilon<- ttklabelframe(frameDown, labelwidget=labellblfEpsilon)

                    Epsilon <- tclVar(1e-07)
                    entry.Epsilon <-tkentry(lblfEpsilon,width="5",textvariable=Epsilon, background="white")

                    labellblfForce <- ttklabel(frameDown,text="  force  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("trimcal", args = "force")
                    tk2tip(labellblfForce,descfunz)
                    lblfForce<- ttklabelframe(frameDown, labelwidget=labellblfForce)

                    rbForceF <- ttkradiobutton(lblfForce)
                    rbForceT <- ttkradiobutton(lblfForce)
                    rbValueForce <- tclVar("TRUE")

                    tkconfigure(rbForceF,variable=rbValueForce,value="FALSE")
                    tkconfigure(rbForceT,variable=rbValueForce,value="TRUE")

                    labelForceF <- ttklabel(lblfForce,text="False ")
                    labelForceT <- ttklabel(lblfForce,text="True ")
                                                                                            # WHY????
#-----------------------------------------------

                    labellblfTrimcalObj <- ttklabel(frameOutput,text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfTrimcalObj<- ttklabelframe(frameOutput, labelwidget = labellblfTrimcalObj)

                    entry.ObjectName <-ttkentry(lblfTrimcalObj,width="20",text=as.character(tclvalue(TrimcalObjectName)), state= "disabled", font="TkDefaultFont")

                    ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_Trimcal(entry.BoundsHigh, entry.BoundsLow, entry.Maxit, entry.Epsilon, rbValueForce, ttTrimcal))

                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttTrimcal))

                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_Trimcal), command=function() fOnFunctionHelp(label_Trimcal))


                    tkgrid(tk2label(frameGlobal, text="Sample Data", font=fontTextTitle, foreground= "blue"), pady=c("0.2c",0))

                    tkgrid(lblDesign)
                    tkgrid(lbltrimDesign)

                    tkgrid(lstDesign, scrDesign)

                    tkgrid.configure(lblDesign, column=1, sticky ="e")
                    tkgrid.configure(lbltrimDesign, column=1, sticky ="e")

                    tkgrid.configure(lstDesign, column=1, row=3, sticky ="e", padx=c("1.7c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrDesign, column=2, row=3, sticky ="nsw", padx=c(0,"1.9c"), pady=c(0,"0.2c"))

#--------------------
                    # tkconfigure(cbKey, textvariable = KeyDef)
                    # tkgrid(cbKey)

                    # tkgrid.configure(cbKey,padx="0.2c", pady=c(0,"0.2c"))
#--------------------

                    tkgrid(lblfCurrW, lblfBounds)
                    tkgrid.configure(lblfCurrW, padx="0.3c", pady="0.2c")

                    tkgrid(frameCurrW)
                    tkgrid(textCurrW,scrtextCurrW)
                    tkgrid.configure(scrtextCurrW, sticky ="nsw")

                    tkgrid(LowHighFrame)
                    tkgrid.configure(LowHighFrame, sticky="w", pady=c("0.2c"))

                    tkgrid(labelBoundsLow, entry.BoundsLow)
                    tkgrid.configure(labelBoundsLow, sticky="w", padx=c("0.3c",0))
                    tkconfigure(entry.BoundsLow, textvariable=BoundsLow)

                    tkgrid.configure(entry.BoundsLow, column=1, padx=c(8,4))

                    tkgrid(labelBoundsHigh,entry.BoundsHigh)
                    tkgrid.configure(labelBoundsHigh, sticky="w", padx=c("0.3c",0))
                    tkconfigure(entry.BoundsHigh, textvariable=BoundsHigh)

                    tkgrid.configure(entry.BoundsHigh, column=1, padx=c(8,4))

#-------------------------------------------------------------------------------

                    tkgrid(lblfMaxit, lblfEpsilon, lblfForce)

                    tkgrid(entry.Maxit)
                    tkconfigure(entry.Maxit, textvariable=Maxit)
                    tkgrid.configure(entry.Maxit, padx=c("0.6c", 0), pady=c(0,"0.2c"))
                    tkgrid.configure(lblfMaxit, padx="0.8c")

                    tkgrid(entry.Epsilon)
                    tkconfigure(entry.Epsilon, textvariable=Epsilon)
                    tkgrid.configure(entry.Epsilon,  padx=c("0.7c", 0), pady=c(0,"0.2c"))
                    tkgrid.configure(lblfEpsilon, padx="0.1c")

                    tkgrid(labelForceF,rbForceF)
                    tkgrid(labelForceT,rbForceT)
                    tkgrid.configure(lblfForce, padx="0.8c")

                    tkgrid.configure(labelForceF, sticky="w", padx=c("0.4c",0))
                    tkgrid.configure(labelForceT, sticky="w", padx=c("0.4c",0))

#-------------------------------------------------------------------------------

                    tkgrid(frameTop, padx="0.2c")
                    tkgrid(lblMandatory, pady=c("0.2c",0))

                    tkgrid(frameCentral)
                    tkgrid.configure(frameCentral, padx="0.5c")
                    # tkgrid(lblfBounds)
                    tkgrid.configure(lblfBounds, padx="0.3c", pady="0.2c")

                    tkgrid(lblOptional, pady=c("0.2c",0))
                    tkgrid(frameDown)
                    tkgrid.configure(frameDown, padx="0.5c", pady=c(0, "0.2c"))

                    tkgrid(lblfTrimcalObj)

                    tkgrid.configure(entry.ObjectName, padx="0.5c",pady=c(0,"0.3c"))
                    tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")

                    tkgrid(ok.but, Cancel.but, FunctionHelp.but)
                    tkgrid.configure(Cancel.but, padx="0.2c")
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.2c"))
                    tkgrid.configure(frameButtons, sticky="ne")

                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttTrimcal))) # To asses the size of the window...
                    # tkfocus(ttTrimcal)
            }

            ftrim_VarDesign <- function(EC_lstDesign, xDes, trim_VarDesign, EC_lbltrimDesign,
                    EC_ok.but, EC_entry.ObjectName, EC_entry.BoundsHigh, EC_entry.BoundsLow, EC_textCurrW){

                    EC_indicescelta <- as.character(tclvalue(tkcurselection(EC_lstDesign)))

                    assignTemp("choiceDes", as.character(tclObj(xDes))[as.integer(tkcurselection(EC_lstDesign))+1])

                    # Move it below to obtain a small delay...
                    # sceltaDes <- get(choiceDes, envir=.GlobalEnv)

                    trim_VarDesign <<- tclVar(choiceDes)
                    tkconfigure(EC_lbltrimDesign, textvariable= trim_VarDesign)

                    sceltaDes <- get(choiceDes, envir=.GlobalEnv)

                    TrimcalObjectName <<- tclVar("")

                    if (Index_choice_Trimcal!=EC_indicescelta){
                        count_trimDes <<- FALSE
                    }
                    if (count_trimDes == FALSE){
                        count_trimDes <<- TRUE
                        Index_choice_Trimcal <<- EC_indicescelta
                    }


                    tkconfigure(EC_entry.ObjectName, textvariable=TrimcalObjectName, state="normal")

                    w.summary <- as.character(summary(weights(sceltaDes)))
                    tkconfigure(EC_textCurrW, state="normal")
                    tkdelete(EC_textCurrW, "1.0", "end")
                    tkinsert(EC_textCurrW, "end", paste("    Min.  ", w.summary[1], "\n", sep = ""))
                    tkinsert(EC_textCurrW, "end", paste(" 1st Qu.  ", w.summary[2], "\n", sep = ""))
                    tkinsert(EC_textCurrW, "end", paste(" Median   ", w.summary[3], "\n", sep = ""))
                    tkinsert(EC_textCurrW, "end", paste("   Mean   ", w.summary[4], "\n", sep = ""))
                    tkinsert(EC_textCurrW, "end", paste(" 3rd Qu.  ", w.summary[5], "\n", sep = ""))
                    tkinsert(EC_textCurrW, "end", paste("    Max.  ", w.summary[6], sep = ""))
                    tkconfigure(EC_textCurrW, state="disabled")

                    # Code below should restore initial values when changing
                    # design object: works well
                    BoundsHigh <- tclVar("Inf")
                    BoundsLow <- tclVar("-Inf")
                    tkconfigure(EC_entry.BoundsHigh, textvariable=BoundsHigh, state="normal")
                    tkconfigure(EC_entry.BoundsLow, textvariable=BoundsLow, state="normal")

                    if (count_trimDes==TRUE){
                        tkconfigure(EC_ok.but, state = "normal")
                        tkconfigure(EC_entry.ObjectName, state = "normal")
                    }
                    else{
                        tkconfigure(EC_ok.but, state = "disabled")
                        tkconfigure(EC_entry.ObjectName, state = "disabled")
                    }
            }


            fOnRun_Trimcal <- function(OR_entry.BoundsHigh, OR_entry.BoundsLow, OR_entry.Maxit, OR_entry.Epsilon, OR_rbValueForce, ttTrimcal){
                    campobb <- TRUE
                    sceltaDes  <- get(choiceDes, envir=.GlobalEnv)

                    boundsH <- tclvalue(tkget(OR_entry.BoundsHigh))
                    boundsL <- tclvalue(tkget(OR_entry.BoundsLow))

                        if (boundsH!=Inf){
                            if (boundsH ==""){
                                boundsH <- Inf
                            }
                            else{
                                boundsH <- as.numeric(tkget(OR_entry.BoundsHigh))
                                if (is.na(boundsH)){
                                    tkmessageBox(title="high bound", message="Please insert a numeric value",icon="error", parent = ttTrimcal)
                                    campobb <- FALSE
                                }
                            }
                        }
                        else{
                            boundsH <- as.numeric(tkget(OR_entry.BoundsHigh))
                        }

                        if (boundsL!=-Inf){
                            if (boundsL ==""){
                                boundsL <- -Inf
                            }
                            else{
                                boundsL <- as.numeric(tkget(OR_entry.BoundsLow))
                                if (is.na(boundsL)){
                                    tkmessageBox(title="low bound", message="Please insert a numeric value",icon="error", parent = ttTrimcal)
                                    campobb <- FALSE
                                }
                            }
                        }
                        else{
                            boundsL <- as.numeric(tkget(OR_entry.BoundsLow))
                        }

                    if (campobb == TRUE){
                        w.range <- c(boundsL,boundsH)
                        prnW.range <- paste("w.range= c(",boundsL,", ",boundsH,")",sep="")
                    }

#-------------------------------------------------------------------------------

                    maxit <- tclvalue(tkget(OR_entry.Maxit))
                    if (maxit ==""){
                        prnMaxit <- paste("maxit=", 50)
                        maxit<- tclVar(50)
                    }
                    else{
                        maxit <- as.numeric(tkget(OR_entry.Maxit))

                        if (is.na(maxit)){
                            tkmessageBox(title="maxit", message="Please insert a numeric value",icon="error", parent = ttTrimcal)
                            campobb <- FALSE
                        }
                        else{
                            prnMaxit <- paste("maxit=", maxit)
                        }
                    }


                    epsilon <- tclvalue(tkget(OR_entry.Epsilon))
                    if (epsilon ==""){
                        prnEpsilon <- paste("epsilon=", 1e-7)
                        epsilon<- tclVar(1e-7)
                    }
                    else{
                        epsilon <- as.numeric(tkget(OR_entry.Epsilon))

                        if (is.na(epsilon)){
                                tkmessageBox(title="epsilon", message="Please insert a numeric value",icon="error", parent = ttTrimcal)
                                campobb <- FALSE
                        }
                        else{
                            prnEpsilon <- paste("epsilon=", epsilon)
                        }
                    }

                    force <- as.logical(tclvalue(OR_rbValueForce))
                    if (!force) {
                            prnForce <- "force= FALSE"
                    }
                    else{
                        prnForce <- "force= TRUE"
                    }

#-------------------------------------------------------------------------------

                    if (tclvalue(TrimcalObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = ttTrimcal)
                        campobb <- FALSE
                    }
                    else{
                        if   ( !is.ok.name(tclvalue(TrimcalObjectName), go.on=campobb,
                                           parent = ttTrimcal) ){
                             campobb <- FALSE
                             }
                        else {
                             OR_TrimcalObjectName <- tclvalue(TrimcalObjectName)
                             }
                        }

                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(ttTrimcal, cursor="watch")

                        outTrimcal <- Lancia(trimcal(sceltaDes, w.range, maxit, epsilon, force), textWarnings, parent = ttTrimcal)

                        if (!inherits(outTrimcal, "try-error")) {
                            prnDesign <- paste("cal.design=",choiceDes)

                            if (is.list(outTrimcal[["call"]])){
                                outTrimcal[["call"]][[1]] <- paste("trimcal(", prnDesign, ", ", prnW.range,
                                ", ", prnMaxit, ", ", prnEpsilon, ", ", prnForce, ")", sep="")
                            }
                            else {
                                outTrimcal[["call"]] <- paste("trimcal(", prnDesign, ", ", prnW.range,
                                ", ", prnMaxit, ", ", prnEpsilon, ", ", prnForce, ")", sep="")
                            }

                            assign2GE(OR_TrimcalObjectName, outTrimcal)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            cat(paste("# ", OR_TrimcalObjectName, "\n", sep=""))
                            print(outTrimcal)
                            cat("\n")

                            prnTrimcal<- paste(" <- trimcal(", prnDesign, ", ", prnW.range, ", ", prnMaxit, ", ", prnEpsilon, ", ", prnForce, ")", sep="")
                            commands <- paste(OR_TrimcalObjectName, prnTrimcal, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                  tkmessageBox(title ="trimcal",
                                               message = "Operation executed\nNote: Warnings have been generated!",
                                               icon = "info", parent = ttTrimcal)
                                 }
                            else {
                                  tkmessageBox(title ="trimcal",message = "Operation executed", icon = "info", parent = ttTrimcal)
                                 }
                            tkgrab.release(ttTrimcal)
                            }
                    # get back the standard arrow cursor
                    tkconfigure(ttTrimcal, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# --------------------------------
# < END building trimcal window. <
# --------------------------------

# ----------------------------------------
# > START building aux.estimates window. >
# ----------------------------------------

#            fAuxEst <- function() {
#                tkmessageBox(title ="Aux. Estimates", message = "Sorry, this item is still under construction",
#                    icon = "info")
#            }

            fAuxEst <- function(){
                ttAuxEstimates <- tktoplevel()
                tcl("wm", "protocol", ttAuxEstimates, "WM_DELETE_WINDOW", function() fOnCancel(ttAuxEstimates))
                frameGlobal<- tkframe(ttAuxEstimates, borderwidth= 2)
                tkwm.deiconify(ttAuxEstimates)
                tkgrab.set(ttAuxEstimates)
                tkfocus(ttAuxEstimates)
                tkwm.title(ttAuxEstimates,label_AuxEstimates)
                tkwm.resizable(ttAuxEstimates, 0, 0)

                listSurveydataObj <-  mk.class.list("analytic")

                frameTop     <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                frameCentral <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                frameDown <- tkframe(frameGlobal, relief="groove", borderwidth=2, pady=c("0.2c","0.4c"))
                frameOutput  <- tkframe(frameGlobal, borderwidth=0)
                frameButtons <- tkframe(frameGlobal, borderwidth=0)

                lblfSurveyData<- tk2labelframe(frameTop)

                lblSurveyDataObj <- ttklabel(frameTop, text="Select a survey design object", font=fontTextLabel)

                lblVariables <- tklabel(frameTop, font=fontTextLabel, state= "disabled")

                lblAuxSurveyDataObj <- ttklabel(frameTop,textvariable=as.character(tclvalue(VarAuxSurveyDataObj)),foreground="red")

                lblMandatory <- tk2label(frameGlobal,text="Formula Fields", font=fontTextTitle, foreground= "blue")

                lblOptional <-tk2label(frameGlobal,text="Optional Fields", font=fontTextTitle, foreground= "blue")

                scrSurveydataObj <- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstSurveydataObj,...))
                lista <- tclVar()
                tclObj(lista) <- listSurveydataObj
                lstSurveydataObj <- tklistbox(frameTop,height = 4, listvariable= lista, selectmode="single", yscrollcommand = function (...)tkset(scrSurveydataObj,...), background = "white")

                scrVariables<- tkscrollbar(frameTop, orient = "vertical", command = function(...) tkyview(lstVariables,...))
                listaVariables <- tclVar()
                lstVariables <- tklistbox(frameTop,height = 4, listvariable= listaVariables, selectmode="extended", yscrollcommand = function (...)tkset(scrVariables,...), background = "white")


                count <<- FALSE

                tkbind(lstSurveydataObj, "<ButtonRelease-1>",
                function() fElencoCampiAuxEstimates(lista, lstSurveydataObj, lblVariables, listaVariables,
                           lstVariables, scrVariables, lblAuxSurveyDataObj, VarAuxSurveyDataObj,
                           entry.CalmodelForm, Plus.but, Times.but, Colon.but, Minus.but, LeftParen.but,
                           RightParen.but, Collapse.but, OkCalmodelForm.but, entry.PartitionForm,
                           OkPartitionForm.but, rbCalmodel, rbPartition, lblPartitionForm,
                           lblCalmodelForm, Ok.but, textCalmodel, textPartition, entry.ObjectName,
                           Template, listaTemplate, cbTemplate, TemplateDef))

                tkbind(lstVariables, "<Double-Button-1>",
                function() fAuxEstimatesInsertVariable(lstVariables, listaVariables, entry.CalmodelForm,
                           entry.PartitionForm, ObjectPartitionF, VarAuxSurveyDataObj, lblAuxSurveyDataObj,
                           ObjectCalmodelF))

                labellblfCalmodelPartition <- ttklabel(frameCentral,text="Formulae", font=fontTextLabel)
                lblfCalmodelPartition<- ttklabelframe(frameCentral, labelwidget=labellblfCalmodelPartition)

                labellblfCalmodel <- ttklabel(frameCentral,text="  calmodel  ", font=fontTextLabel, image=image_qm, compound="right")
                descfunz <- descArgs("aux.estimates", args = "calmodel")
                tk2tip(labellblfCalmodel,descfunz)
                lblfCalmodel<- ttklabelframe(lblfCalmodelPartition, labelwidget=labellblfCalmodel)

                # Here layout in pixels: should translate in cm (which is our standard)...
                frameCalmodel<- tkframe(lblfCalmodel, borderwidth= 2, padx=5)
                scrtextCalmodel <- tkscrollbar(frameCalmodel, orient = "vertical", command=function(...) tkyview(textCalmodel, ...))

                textCalmodel <- tktext(frameCalmodel, foreground="red", background= "white", height=4, width=40, yscrollcommand=function(...) tkset(scrtextCalmodel, ...),
                state="disabled", wrap="word", font=fontTextExp)

                labellblfPartition <- ttklabel(frameCentral,text="   partition   ", font=fontTextLabel, image=image_qm, compound="right")
                descfunz <- descArgs("aux.estimates", args = "partition")
                tk2tip(labellblfPartition,descfunz)
                lblfPartition<- ttklabelframe(lblfCalmodelPartition, labelwidget=labellblfPartition)

                # Here layout in pixels: should translate in cm (which is our standard)...
                framePartition<- tkframe(lblfPartition, borderwidth= 2, padx=5)
                scrtextPartition <- tkscrollbar(framePartition, orient = "vertical", command=function(...) tkyview(textPartition, ...))

                textPartition <- tktext(framePartition, foreground="red", background= "white", height=3, width=40, yscrollcommand=function(...) tkset(scrtextPartition, ...),
                state="disabled", wrap="word", font=fontTextExp)

                tkinsert(textPartition, "end", "FALSE")
                tkconfigure(textPartition, state="disabled")

                labellblfFormulaComp <- ttklabel(frameCentral,text="Formula composer", font=fontTextLabel)
                lblfFormulaComp<- ttklabelframe(frameCentral, labelwidget=labellblfFormulaComp)

                outerOperatorsFrame <- tkframe(lblfFormulaComp)
                FormulaFrame <- tkframe(lblfFormulaComp)

                ObjectCalmodelF <- tclVar("")
                entry.CalmodelForm <- tkentry(FormulaFrame,textvariable=ObjectCalmodelF, width=34, state="disabled", background="white")


                OkCalmodelForm.but <- tk2button(FormulaFrame, image=image_sx, state="disabled", command=function() fAuxEstimatesOnCalmodelForm(ObjectCalmodelF,
                                            entry.CalmodelForm, Ok.but, textCalmodel, entry.ObjectName, parent = ttAuxEstimates))

                ObjectPartitionF <- tclVar("")
                entry.PartitionForm <- tkentry(FormulaFrame,text=ObjectPartitionF, state="disabled", width=34, background="white")

                OkPartitionForm.but <- tk2button(FormulaFrame, image=image_sx, state="disabled",
                command=function() fAuxEstimatesOnPartitionForm(ObjectPartitionF, entry.PartitionForm, textPartition))

                rbPartition <- ttkradiobutton(FormulaFrame, state="disabled")
                rbCalmodel <- ttkradiobutton(FormulaFrame, state="disabled")

                rbValueCalPart <<- tclVar("CalChoice")

                tkconfigure(rbCalmodel,variable=rbValueCalPart,value="CalChoice")
                tkconfigure(rbPartition,variable=rbValueCalPart,value="ParChoice")

                lblCalmodelForm<- ttklabel(FormulaFrame,text="Calmodel = ~", state="disabled")
                lblPartitionForm <- ttklabel(FormulaFrame,text="Partition = ~", state="disabled")

                tkbind(rbCalmodel, "<ButtonPress>", function(){
                                                                tkconfigure(lblPartitionForm, state = "disabled")
                                                                tkconfigure(entry.PartitionForm, state = "disabled")
                                                                tkconfigure(OkPartitionForm.but, state = "disabled")

                                                                tkconfigure(lblCalmodelForm, state = "normal")
                                                                tkconfigure(entry.CalmodelForm, state = "normal")
                                                                tkconfigure(OkCalmodelForm.but, state = "normal")
                                                                rbValueCalPart <<- tclVar("CalChoice")
                                                                })

                tkbind(rbPartition, "<ButtonPress>", function(){
                                                                 tkconfigure(lblCalmodelForm, state = "disabled")
                                                                 tkconfigure(entry.CalmodelForm, state = "disabled")
                                                                 tkconfigure(OkCalmodelForm.but, state = "disabled")
                                                                 tkconfigure(lblPartitionForm, state = "normal")
                                                                 tkconfigure(entry.PartitionForm, state = "normal")
                                                                 tkconfigure(OkPartitionForm.but, state = "normal")
                                                                 rbValueCalPart <<- tclVar("ParChoice")
                                                                })

                    Plus.but <- tk2button(outerOperatorsFrame, text="+", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, " + "))
                    Times.but <- tk2button(outerOperatorsFrame, text="*", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, "*"))
                    Colon.but <- tk2button(outerOperatorsFrame, text=":", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, ":"))
                    Minus.but <- tk2button(outerOperatorsFrame, text="-", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, " - "))
                    LeftParen.but <- tk2button(outerOperatorsFrame, text="(", width="3", state="disabled", command=function() fOnOperator(ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, "("))
                    RightParen.but <- tk2button(outerOperatorsFrame, text=")", width="3", state="disabled", command=function() fOnOperator (ObjectCalmodelF,entry.CalmodelForm,ObjectPartitionF,entry.PartitionForm, ")"))
                    Collapse.but <- tk2button(outerOperatorsFrame, text="+...+", width="5", state="disabled", command=function() fOnCollapseOperator (lstVariables, entry.CalmodelForm, entry.PartitionForm,parent = ttAuxEstimates))

                    labellblfTemplate <- ttklabel(frameDown,text="  template  ", font=fontTextLabel, image=image_qm, compound="right")
                    descfunz <- descArgs("aux.estimates", args = "template")
                    tk2tip(labellblfTemplate,descfunz)
                    lblfTemplate<- ttklabelframe(frameDown, labelwidget=labellblfTemplate)

                    listaTemplate <- tclVar()
                    Template <- c("NULL")
                    tclObj(listaTemplate) <- Template
                    cbTemplate <- tk2combobox(lblfTemplate, values = Template, state="readonly")
                    TemplateDef <- tclVar("NULL")
                    tkconfigure(cbTemplate, textvariable = TemplateDef)

                    # Initialize variable whose update must trigger the disabling of formula composer
                    oldchooseTempl <<- tclvalue(TemplateDef)
                    # Bind actions to the item selection from the combobox
                    tkbind(cbTemplate, "<<ComboboxSelected>>",
                           function() PopTemplchange(TemplateDef, rbCalmodel, rbPartition,
                           lblPartitionForm, entry.PartitionForm, OkPartitionForm.but,
                           lblCalmodelForm, entry.CalmodelForm, OkCalmodelForm.but, Plus.but,
                           Times.but, Colon.but, Minus.but, LeftParen.but, RightParen.but,
                           Collapse.but, textCalmodel, textPartition, Ok.but, entry.ObjectName))

                    labellblfAuxEstimatesObj <- ttklabel(frameOutput,text="  Output Object Name  ", font=fontTextTitle, foreground= "blue")
                    lblfAuxEstimatesObj<- ttklabelframe(frameOutput, labelwidget = labellblfAuxEstimatesObj)

                    ObjectName <- tclVar("")
                    entry.ObjectName <-ttkentry(lblfAuxEstimatesObj,width="20",textvariable=ObjectName, state= "disabled", font="TkDefaultFont")

                    Ok.but <- tk2button(frameButtons, text="OK", image=image_ok, compound="left", state= "disabled",
                              command=function() fOnRun_AuxEstimates(textCalmodel, textPartition, ObjectName, ttAuxEstimates, cbTemplate))

                    Cancel.but <- tk2button(frameButtons,text="Cancel", image=image_cancel, compound="left", command=function() fOnCancel(ttAuxEstimates))


                    FunctionHelp.but <- tk2button(frameButtons,text="Function Help", image=image_qm, compound="left", tip=descFun(label_AuxEstimates), command=function() fOnFunctionHelp(label_AuxEstimates))

                    tkgrid(tk2label(frameGlobal, text="Sample Data", font=fontTextTitle, foreground= "blue"),  pady=c("0.2c",0))

                    tkgrid(lblfSurveyData)
                    tkgrid(lblSurveyDataObj, lblVariables)

                    tkgrid(lblAuxSurveyDataObj)

                    tkgrid(lstSurveydataObj, scrSurveydataObj)

                    tkgrid.configure(lblSurveyDataObj, column=1, sticky ="e")
                    tkgrid.configure(lblVariables, column=4)
                    tkgrid.configure(lblAuxSurveyDataObj, row=2, column=1, sticky ="e")

                    tkgrid.configure(lstSurveydataObj, column=1, row=3, sticky ="e", padx=c("1.7c","0c"), pady=c(0,"0.2c"))
                    tkgrid.configure(scrSurveydataObj, column=2, row=3, sticky ="nsw", padx=c(0,"1.9c"), pady=c(0,"0.2c"))

                    tkgrid(Plus.but, Times.but, Colon.but, Minus.but, LeftParen.but, RightParen.but, Collapse.but, sticky ="e")
                    tkgrid(rbCalmodel,lblCalmodelForm, entry.CalmodelForm, OkCalmodelForm.but)

                    tkgrid.configure(rbCalmodel, padx=c("0.2c",0), pady=c(0,"1.9c"))
                    tkgrid.configure(lblCalmodelForm, pady=c(0,"1.9c"))
                    tkgrid.configure(entry.CalmodelForm, pady=c(0,"1.9c"))
                    # Here layout in pixels: should translate in cm (which is our standard)...
                    tkgrid.configure(OkCalmodelForm.but, padx=c(5,5), pady=c(0,"1.9c"))

                    tkgrid(rbPartition,lblPartitionForm, entry.PartitionForm, OkPartitionForm.but)

                    tkgrid.configure(rbPartition, padx=c("0.2c",0), pady=c(0,"1.1c"))
                    tkgrid.configure(lblPartitionForm, pady=c(0,"1.1c"))
                    tkgrid.configure(entry.PartitionForm, pady=c(0,"1.1c"))
                    # Here layout in pixels: should translate in cm (which is our standard)...
                    tkgrid.configure(OkPartitionForm.but, padx=c(5,5), pady=c(0,"1.1c"))

                    tkgrid(lblfTemplate)
                    tkgrid(cbTemplate)

                    tkgrid.configure(cbTemplate,padx="0.2c", pady=c(0,"0.2c"))
                    tkgrid.configure(lblfTemplate,padx="0.5c")

                    tkgrid(frameTop)
                    tkgrid(lblMandatory, pady=c("0.2c",0))

                    tkgrid(frameCentral)
                    tkgrid.configure(frameCentral, padx="0.5c")

                    tkgrid(lblfFormulaComp, lblfCalmodelPartition, sticky="ns")

                    tkgrid.configure(lblfCalmodelPartition,padx=c("0.25","0.3c"))

                    tkgrid(lblfCalmodel)
                    tkgrid(lblfPartition)

                    tkgrid.configure(lblfCalmodel,pady=c(20,10))
                    tkgrid.configure(lblfPartition,padx=c(15,15))

                    tkgrid(frameCalmodel)
                    tkgrid(textCalmodel,scrtextCalmodel)
                    # Enable ctrl-c on textCalmodel
                    ctrl.c(textCalmodel)
                    tkgrid.configure(scrtextCalmodel, sticky ="nsw")

                    tkgrid(framePartition)
                    tkgrid(textPartition,scrtextPartition)
                    # Enable ctrl-c on textPartition
                    ctrl.c(textPartition)
                    tkgrid.configure(scrtextPartition, sticky ="nsw")

                    tkgrid.configure(lblfFormulaComp,padx=c("0.3c","0.25c"))

                    tkgrid.configure(outerOperatorsFrame,  pady=c("0.3c","0.2c"))
                    tkgrid(outerOperatorsFrame)
                    tkgrid(FormulaFrame)

                    tkgrid(lblOptional, pady=c("0.2c",0))
                    tkgrid(frameDown)
                    tkgrid.configure(frameDown, padx="0.5c")

                    tkgrid(lblfAuxEstimatesObj)
                    tkgrid.configure(entry.ObjectName, padx=c("0.5c","6.5c"),pady=c(0,"0.3c"))

                    tkgrid(Ok.but, Cancel.but, FunctionHelp.but)
                    tkgrid.configure(Cancel.but, padx=("0.5c"))
                    tkgrid.configure(FunctionHelp.but, padx=c(0,"0.5c"))
                    tkgrid.configure(frameOutput, padx=c("0.5c",0), pady=c("0.2c","0.2c"), sticky="w")
                    tkgrid.configure(frameButtons, sticky="ne")
                    tkgrid(frameGlobal)
                    #PROVA!#
                    # print(tclvalue(tkwm.geometry(ttAuxEstimates))) # To asses the size of the window...
                    # tkfocus(ttAuxEstimates)

            }

            fElencoCampiAuxEstimates <- function(EC_lista, EC_lstSurveydataObj, EC_lblVariables, x,
                    lstEC_SurveydataObj, scrEC, EC_lblAuxSurveyDataObj, VarAuxSurveyDataObj, EC_entry.CalmodelForm,
                    EC_Plus.but, EC_Times.but, EC_Colon.but, EC_Minus.but, EC_LeftParen.but, EC_RightParen.but,
                    EC_Collapse.but, EC_OkCalmodelForm.but, EC_entry.PartitionForm, EC_OkPartitionForm.but,
                    EC_rbCalmodel, EC_rbPartition, EC_lblPartitionForm, EC_lblCalmodelForm, EC_Ok.but,
                    EC_textCalmodel, EC_textPartition, EC_entry.ObjectName, EC_Template, EC_listaTemplate,
                    EC_cbTemplate,EC_TemplateDef){

                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstSurveydataObj)))

                    assignTemp("Scelta_SurveydataObj", as.character(tclObj(EC_lista))[as.integer(tkcurselection(EC_lstSurveydataObj))+1])

                    VarAuxSurveyDataObj <<- tclVar(Scelta_SurveydataObj)

                    tkconfigure(EC_lblAuxSurveyDataObj, textvariable= VarAuxSurveyDataObj)

                    # DIRE A RAFFAELLA 16/07/10
                    # NO!!! Copying may become a serious concern when data get big
                    # (can tolerate it for names, because they are small: see above)
                    # assignTemp("SurveydataObj", get(Scelta_SurveydataObj, envir=.GlobalEnv))
                    SurveydataObj <- get(Scelta_SurveydataObj, envir=.GlobalEnv)
                    # DIRE A RAFFAELLA 16/07/10

                    if (Index_dataframe_old !=EC_indicesel){
                        count <<- FALSE
                    }

                    if (count == FALSE){
                            count <<- TRUE
                            Index_dataframe_old <<- EC_indicesel

                            tkconfigure(EC_textCalmodel, state="normal")
                            tkdelete(EC_textCalmodel, "1.0", "end")
                            tkconfigure(EC_textCalmodel, state="disabled")

                            tkconfigure(EC_textPartition, state="normal")
                            tkdelete(EC_textPartition, "1.0", "end")
                            tkinsert(EC_textPartition, "end", "FALSE")
                            tkconfigure(EC_textPartition, state="disabled")

                            tkconfigure(EC_rbPartition, state = "normal")
                            tkconfigure(EC_rbCalmodel, state = "normal")
                            tkconfigure(EC_lblCalmodelForm, state = "normal")
                            tkconfigure(EC_OkCalmodelForm.but, state = "normal")
                            tkconfigure(EC_entry.CalmodelForm, state = "normal")
                            tkconfigure(EC_Plus.but, state = "normal")
                            tkconfigure(EC_Times.but, state = "normal")
                            tkconfigure(EC_Colon.but, state = "normal")
                            tkconfigure(EC_Minus.but, state = "normal")
                            tkconfigure(EC_LeftParen.but, state = "normal")
                            tkconfigure(EC_RightParen.but, state = "normal")
                            tkconfigure(EC_Collapse.but, state = "normal")

                            if (tclvalue(rbValueCalPart)=="ParChoice"){
                                rbValueCalPart <<- tclVar("CalChoice")
                                tkconfigure(EC_rbPartition,variable=rbValueCalPart,value="ParChoice")
                                tkconfigure(EC_rbCalmodel,variable=rbValueCalPart, value="CalChoice")

                                tkconfigure(EC_lblPartitionForm, state = "disabled")
                                tkdelete(EC_entry.PartitionForm, 0, "end")
                                tkconfigure(EC_entry.PartitionForm, state = "disabled")
                                tkconfigure(EC_OkPartitionForm.but, state = "disabled")
                                tkconfigure(EC_lblCalmodelForm, state = "normal")

                                tkconfigure(EC_entry.CalmodelForm, state = "normal")
                                tkdelete(EC_entry.CalmodelForm, 0, "end")
                                tkconfigure(EC_OkCalmodelForm.but, state = "normal")
                                tkfocus(EC_entry.CalmodelForm)
                            }
                            else{
                                tkdelete(EC_entry.CalmodelForm, 0, "end")
                                tkconfigure(EC_entry.PartitionForm, state = "normal")
                                tkdelete(EC_entry.PartitionForm, 0, "end")
                                tkconfigure(EC_entry.PartitionForm, state = "disabled")

                            }

                            tkdelete(EC_entry.ObjectName, 0, "end")
                            tkconfigure(EC_Ok.but, state = "disabled")

                            if (is.data.frame(SurveydataObj)){
# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                                for (n in names(SurveydataObj)){
#                                tclObj(x) <- c(names(SurveydataObj))
#                                }
                                tclObj(x) <- names(SurveydataObj)

                            }
                            else{
# LOOP NON NECESSARIO: eliminato, vedi sotto. DIRE A RAFFAELLA 15/10/2010
#                                for (n in names(SurveydataObj$variables)){
#                                            tclObj(x) <- c(names(SurveydataObj$variables))
#                                }
                                tclObj(x) <- names(SurveydataObj$variables)
                            }
                    }

                    templateobjs <- mk.class.list("pop.totals")
                    EC_Template <- c("NULL", templateobjs)
                    tclObj(EC_listaTemplate) <- EC_Template
                    tkconfigure(EC_cbTemplate, values = EC_Template)
                    tclvalue(EC_TemplateDef) <- "NULL"
                    oldchooseTempl <<- tclvalue(EC_TemplateDef)

                    tkgrid(lstEC_SurveydataObj, scrEC)

                    tkgrid.configure(lstEC_SurveydataObj, column=4, row=3, sticky ="e", pady=c(0,"0.2c"))
                    tkgrid.configure(scrEC, column=5, row=3, padx=c("0c","1.7c"), pady=c(0,"0.2c"), sticky ="nsw")
                    tkconfigure(EC_lblVariables, text="Variables", state = "normal")

            }

################################################################################

            PopTemplchange <- function(EC_TemplateDef, EC_rbCalmodel, EC_rbPartition,
                    EC_lblPartitionForm, EC_entry.PartitionForm, EC_OkPartitionForm.but, EC_lblCalmodelForm,
                    EC_entry.CalmodelForm, EC_OkCalmodelForm.but, EC_Plus.but, EC_Times.but,
                    EC_Colon.but, EC_Minus.but, EC_LeftParen.but, EC_RightParen.but, EC_Collapse.but,
                    EC_textCalmodel, EC_textPartition, EC_ok.but, EC_entry.ObjectName){

                    if (tclvalue(EC_TemplateDef)=="NULL"){

                        oldchooseTempl <<- tclvalue(EC_TemplateDef)

                        tkconfigure(EC_rbPartition, state = "normal")
                        tkconfigure(EC_rbCalmodel, state = "normal")

                        rbValueCalPart <<- tclVar("CalChoice")

                        tkconfigure(EC_rbCalmodel,variable=rbValueCalPart,value="CalChoice")
                        tkconfigure(EC_rbPartition,variable=rbValueCalPart,value="ParChoice")

                        tkconfigure(EC_lblCalmodelForm, state = "normal")
                        tkconfigure(EC_entry.CalmodelForm, state = "normal")
                        tkconfigure(EC_Plus.but, state = "normal")
                        tkconfigure(EC_Times.but, state = "normal")
                        tkconfigure(EC_Colon.but, state = "normal")
                        tkconfigure(EC_Minus.but, state = "normal")
                        tkconfigure(EC_LeftParen.but, state = "normal")
                        tkconfigure(EC_RightParen.but, state = "normal")
                        tkconfigure(EC_Collapse.but, state = "normal")

                        tkconfigure(EC_OkCalmodelForm.but, state = "normal")

                        tkconfigure(EC_textCalmodel, state="disabled")
                        tkconfigure(EC_textCalmodel, state="normal")
                        tkdelete(EC_textCalmodel, "1.0", "end")
                        tkconfigure(EC_textCalmodel, state="disabled")

                        tkconfigure(EC_textPartition, state="disabled")
                        tkconfigure(EC_textPartition, state="normal")
                        tkdelete(EC_textPartition, "1.0", "end")
                        tkinsert(EC_textPartition, "end", "FALSE")
                        tkconfigure(EC_textPartition, state="disabled")
                    }
                    else if (tclvalue(EC_TemplateDef)!=oldchooseTempl) {

                        oldchooseTempl <<- tclvalue(EC_TemplateDef)

                        newchooseTemplObj <- get(oldchooseTempl, envir = .GlobalEnv)

                        if (count){
                            tkconfigure(EC_ok.but, state = "normal")
                            tkdelete(EC_entry.ObjectName, 0, "end")
                            tkconfigure(EC_entry.ObjectName, state = "normal")
                        }

                        tkconfigure(EC_rbCalmodel, state = "disabled")
                        tkconfigure(EC_rbPartition, state = "disabled")
                        tkconfigure(EC_lblPartitionForm, state = "disabled")
                        tkconfigure(EC_OkPartitionForm.but, state = "disabled")


                        tkconfigure(EC_entry.CalmodelForm, textvariable=tclVar(""), state = "disabled")
                        tkconfigure(EC_entry.PartitionForm, textvariable=tclVar(""), state = "disabled")

                        tkconfigure(EC_entry.PartitionForm, state = "disabled")
                        tkconfigure(EC_lblCalmodelForm, state = "disabled")
                        tkconfigure(EC_OkCalmodelForm.but, state = "disabled")


                        tkconfigure(EC_Plus.but, state = "disabled")
                        tkconfigure(EC_Times.but, state = "disabled")
                        tkconfigure(EC_Colon.but, state = "disabled")
                        tkconfigure(EC_Minus.but, state = "disabled")
                        tkconfigure(EC_LeftParen.but, state = "disabled")
                        tkconfigure(EC_RightParen.but, state = "disabled")
                        tkconfigure(EC_Collapse.but, state = "disabled")

                        tkconfigure(EC_textCalmodel, state="normal")
                        tkdelete(EC_textCalmodel, "1.0", "end")
                        tkconfigure(EC_textPartition, state="normal")
                        tkdelete(EC_textPartition, "1.0", "end")

                        VarCalmodel <<- attr(newchooseTemplObj,"calmodel")
                        VarCalmodel <<- tclvalue(tclVar( form.to.char(VarCalmodel) ))

                        tkinsert(EC_textCalmodel, "end", VarCalmodel)
                        tkconfigure(EC_textCalmodel, state="disabled")

                        VarPartition <<- attr(newchooseTemplObj,"partition")

                        if (VarPartition=="FALSE"){
                            VarPartition <<- tclvalue(tclVar(as.character(VarPartition)))

                            tkinsert(EC_textPartition, "end", VarPartition)
                            tkconfigure(EC_textPartition, state="disabled")
                        }
                        else{
                            VarPartition <<- tclvalue(tclVar( form.to.char(VarPartition) ))

                            tkconfigure(EC_textPartition, state="normal")
                            tkinsert(EC_textPartition, "end", VarPartition)
                            tkconfigure(EC_textPartition, state="disabled")
                        }
                    }
                }


################################################################################

            fAuxEstimatesInsertVariable <- function(EC_lstVariables, EC_listaVariables, EC_entry.CalmodelForm,
                    EC_entry.PartitionForm, EC_ObjectPartitionF, VarAuxSurveyDataObj, EC_lblAuxSurveyDataObj,
                    EC_ObjectCalmodelF){

                    VarAuxSurveyDataObj <<- tclVar(Scelta_SurveydataObj)
                    tkconfigure(EC_lblAuxSurveyDataObj, textvariable= VarAuxSurveyDataObj)

                    EC_indicesel <- as.character(tclvalue(tkcurselection(EC_lstVariables)))

                    assignTemp("AuxEstimates_Variables", as.character(tclObj(EC_listaVariables))[as.integer(tkcurselection(EC_lstVariables))+1])
                    if (tclvalue(rbValueCalPart)=="CalChoice"){
                        tkfocus(EC_entry.CalmodelForm)
                        entry <- tclvalue(tkget(EC_entry.CalmodelForm))
                        lenghtentry <- nchar(entry)

                        if (lenghtentry > 0){
                            tclvalue(EC_ObjectCalmodelF) <- paste(entry, AuxEstimates_Variables, sep="")
                        }
                        else{
                            tclvalue(EC_ObjectCalmodelF) <- paste(AuxEstimates_Variables, sep="")
                        }
                        tkconfigure(EC_entry.CalmodelForm, textvariable=tclVar(tclvalue(EC_ObjectCalmodelF)))

                        lenghtEC_ObjectCalmodelF <- nchar(EC_ObjectCalmodelF)
                        tkicursor(EC_entry.CalmodelForm , "end")
                        tkxview.moveto(EC_entry.CalmodelForm , 1.0)
                    }
                    else{
                        tkfocus(EC_entry.PartitionForm)
                        entry <- tclvalue(tkget(EC_entry.PartitionForm))
                        lenghtentry <- nchar(entry)

                        if (lenghtentry > 0){
                            tclvalue(EC_ObjectPartitionF) <- paste(entry, AuxEstimates_Variables, sep="")
                        }
                        else{
                            tclvalue(EC_ObjectPartitionF) <- paste(AuxEstimates_Variables, sep="")
                        }
                        tkconfigure(EC_entry.PartitionForm, textvariable=tclVar(tclvalue(EC_ObjectPartitionF)))
                        lenghtEC_ObjectPartitionF <- nchar(EC_ObjectPartitionF)
                        tkicursor(EC_entry.PartitionForm , "end")
                        tkxview.moveto(EC_entry.PartitionForm , 1.0)
                    }
            }

            fAuxEstimatesOnCalmodelForm <- function(EC_ObjectCalmodelF, EC_entry.CalmodelForm, EC_Ok.but,
                                                    EC_textCalmodel, EC_entry.ObjectName, parent = "."){

                    EC_ObjectCalmodelF <- tkget(EC_entry.CalmodelForm)

                    if (tclvalue(EC_ObjectCalmodelF) == ""){
                        tkmessageBox(title="Calmodel formula",message="Calmodel formula is empty!",icon="error",type="ok", parent = parent)
                    }
                    else{
                        CalmodelFormula <- tclvalue(EC_ObjectCalmodelF)
                        CalmodelFormula <- Lancia(as.formula(paste("~", CalmodelFormula), env = .GlobalEnv), textWarnings, parent = parent)
                        if (!inherits(CalmodelFormula, "try-error")){
                            CalmodelFormula <- form.to.char(CalmodelFormula)
                        }
                        else return()

                        tkconfigure(EC_textCalmodel, state="normal")
                        tkdelete(EC_textCalmodel, "1.0", "end")
                        tkinsert(EC_textCalmodel, "end", CalmodelFormula)
                        tkconfigure(EC_textCalmodel, state="disabled")

                        tkconfigure(EC_entry.CalmodelForm, textvariable=tclVar(""))

                        if (count)
                            tkconfigure(EC_Ok.but, state = "normal")
                            tkconfigure(EC_entry.ObjectName, state = "normal")

                    }
            }

            fAuxEstimatesOnPartitionForm <- function(EC_ObjectPartitionF, EC_entry.PartitionForm,
                    EC_textPartition){

                    EC_ObjectPartitionF <- tkget(EC_entry.PartitionForm)

                    if (tclvalue(EC_ObjectPartitionF) == ""){
                        PartitionFormula <- "FALSE"
                    }
                    else{
                        PartitionFormula <- tclvalue(EC_ObjectPartitionF)
                        PartitionFormula <- paste("~", PartitionFormula)
                        }
                        tkconfigure(EC_textPartition, state="normal")
                        tkdelete(EC_textPartition, "1.0", "end")
                        tkinsert(EC_textPartition, "end", PartitionFormula)
                        tkconfigure(EC_textPartition, state="disabled")
                        tkconfigure(EC_entry.PartitionForm, textvariable=tclVar(""))
            }

            fOnRun_AuxEstimates <- function(OR_textCalmodel, OR_textPartition, OR_AuxEstimatesObjectName,
                                            OR_ttAuxEstimates, OR_cbTemplate){
                    campobb <- TRUE
                    calmodel <- tclvalue(tkget(OR_textCalmodel, "1.0", "end -1 chars"))
                    calmodel  <- Lancia(as.formula(calmodel, env = .GlobalEnv), textWarnings, parent = OR_ttAuxEstimates)

                    if (!inherits(calmodel,"try-error")){
                        prnCalmodel <- tclvalue(tkget(OR_textCalmodel, "1.1", "end -1 chars"))
                        prnCalmodel <- paste("calmodel=", prnCalmodel, sep =" ~")
                    }
                    else{
                        campobb <- FALSE
                    }

                    partition <- as.character(tclvalue(tkget(OR_textPartition, "1.0", "end -1 chars")))
                    if (partition!= "FALSE"){
                        partition  <- Lancia(as.formula(partition, env = .GlobalEnv), textWarnings, parent = OR_ttAuxEstimates)

                        if (!inherits(partition,"try-error")){
                            prnPartition <- tclvalue(tkget(OR_textPartition, "1.1", "end -1 chars"))
                            prnPartition <- paste("partition=", prnPartition, sep =" ~")
                            }
                        else{
                            campobb <- FALSE
                            }
                        }
                    else{
                            partition <- FALSE
                            prnPartition <- paste("partition=", "FALSE")
                        }

                    if (tclvalue(OR_AuxEstimatesObjectName)==""){
                        tkmessageBox(title="Output object name ", message="Please give a name to the output",icon="error", parent = OR_ttAuxEstimates)
                        campobb <- FALSE
                        }
                    else{
                        if   ( !is.ok.name(tclvalue(OR_AuxEstimatesObjectName), go.on=campobb,
                                           parent = OR_ttAuxEstimates) ){
                             campobb <- FALSE
                             }
                        else {
                             OR_AuxEstimatesObjectName <- tclvalue(OR_AuxEstimatesObjectName)
                             }
                        }

                    templatechar <- tclvalue(tclVar(tkget(OR_cbTemplate)))
                    if (templatechar != "NULL"){
                        template <- get(templatechar, envir = .GlobalEnv)
                        prnTemplate <- paste("template=", templatechar)
                    }
                    else{
                            template <- NULL
                            prnTemplate <- paste("template=", "NULL")
                    }

                    if (campobb == TRUE){
                        # change the cursor to the hourglass to tell work is in progress...
                        tkconfigure(OR_ttAuxEstimates, cursor="watch")

                        # DIRE A RAFFAELLA 16/07/10
                        # SurveydataObj no more exists into TempEnv: we left inside its name only
                        SurveydataObj <- get(Scelta_SurveydataObj, envir=.GlobalEnv)
                        # DIRE A RAFFAELLA 16/07/10

                        outAux <- Lancia(aux.estimates(SurveydataObj, calmodel, partition, template), textWarnings, parent = OR_ttAuxEstimates)

                        if (!inherits(outAux,"try-error")) {
                            attr(outAux,"design") <- as.symbol(Scelta_SurveydataObj)
                            # assign(OR_AuxEstimatesObjectName, outAux, envir = .GlobalEnv)
                            assign2GE(OR_AuxEstimatesObjectName, outAux)
                            Upd.act.funs(functionsMenu, surveydesignMenu, calibrationMenu)

                            cat("\n")
                            if (!is.big(outAux)) {
                                cat(paste("# ",OR_AuxEstimatesObjectName,"\n",sep=""))
                            }
                            else {
                                if (is.large(outAux)) {
                                     cat(paste("# head(",OR_AuxEstimatesObjectName,"[, 1:10])\n",sep=""))
                                    }
                                else {
                                     cat(paste("# head(",OR_AuxEstimatesObjectName,")\n",sep=""))
                                    }
                            }
                            PopTemp.onscreen(outAux, OR_AuxEstimatesObjectName)
                            cat("\n")

                            prnData <- paste("design=",Scelta_SurveydataObj)
                            prnAuxEstimates<- paste(" <- aux.estimates(", prnData, ", ", prnCalmodel, ", ", prnPartition, ", ", prnTemplate, ")", sep="")

                            commands <- paste(OR_AuxEstimatesObjectName, prnAuxEstimates, "\n", sep="")

                            # Print on the Commands Window
                            tkconfigure(textHistory, state="normal")
                            tkinsert(textHistory, "end", commands)
                            tkinsert(textHistory, "end", "\n")
                            tkyview.moveto(textHistory, 1.0)
                            tkconfigure(textHistory, state="disabled")
                            # End

                            # Flush commands to Rhistory file
                            upd.Rhistory(commands, RG.stamp = TRUE)
                            # End

                            if (getTemp("there.are.warnings", default = FALSE)){
                                  tkmessageBox(title ="aux.estimates",
                                               message = "Operation executed\nNote: Warnings have been generated!",
                                               icon = "info", parent = OR_ttAuxEstimates)
                                 }
                            else {
                                  tkmessageBox(title ="aux.estimates",message = "Operation executed", icon = "info", parent = OR_ttAuxEstimates)
                                 }

                            tkgrab.release(OR_ttAuxEstimates)
                        }
                    # get back the standard arrow cursor
                    tkconfigure(OR_ttAuxEstimates, cursor="arrow")
                    # Collect garbage to free memory
                    gc()
                    }
            }

# --------------------------------------
# > END building aux.estimates window. >
# --------------------------------------


    }
