Intro.RG <- function(){
##########################################
# Welcome window for ReGenesees.         #
# Note: Gets called when ReGenesees.GUI  #
#       is invoked.                      #
##########################################

# Check that the screen resolution is enough, otherwise stop
sr <- ScreenResolution()
if ( (sr["width"] < 1000) || (sr["height"] < 700) ) {
    stop("Sorry, ReGenesees GUI needs a screen resolution of 1024x768 or higher (your seems to be about ", paste(sr, collapse="x"), ").\nAnyway, you can use the ReGenesees package via R's command line interface!")
    }

# Under Windows OS, reset the original (i.e. tcltk) TkDefaultFont
# because currently it gets mistakenly changed by tcltk2.
# Do it by copying TkTextFont: this should be a good idea (hopefully...)
  # TkDefaultFont at startup
  TDF.list <- list(family = get.fontfamily("TkDefaultFont"), size = get.fontsize("TkDefaultFont"))

  # TkTextFont at startup
  TTF.list <- list(family = get.fontfamily("TkTextFont"), size = get.fontsize("TkTextFont"))

if (.Platform$OS.type == "windows") {
     if (!identical(TDF.list, TTF.list)) {
         # Reset TDF by copying TTF values. Save startup and current
         # (i.e. changed) TDF.list in a temporary environment
         assignTemp("TDF.startup", TDF.list, replace.existing = TRUE)
         tkfont.configure("TkDefaultFont", family = TTF.list$family, size = TTF.list$size)
         TDF.list <- list(family = get.fontfamily("TkDefaultFont"), size = get.fontsize("TkDefaultFont"))
         assignTemp("TDF.intro", TDF.list, replace.existing = TRUE)
        }
    }
# Under OS other than Windows, all should be ok (hopefully...)

# Get versions of packages ReGenesees and ReGenesees.GUI (for title)
PKG.version <- packageDescription("ReGenesees")$Version
GUI.version <- packageDescription("ReGenesees.GUI")$Version

# Find path to the images folder...
img.path <- system.file("images", package = "ReGenesees.GUI")
# ... get the LOGO image...
image_logo <- tclVar()
tkimage.create("photo", image_logo, file= paste(img.path, "//RG.logo10.gif", sep=""))
# ...and the ICON image...
image_icon <- tclVar()
tkimage.create("photo", image_icon, file= paste(img.path, "//RG_icon32.gif", sep=""))

# Create top-level window
tt <- tktoplevel(bg="white")
# Change window icon from TK to RG
tcl("wm", "iconphoto", tt, "-default", image_icon)
# if (.Platform$OS.type == "windows") tkwm.iconbitmap(tt, "something.ico")
tktitle(tt) <- paste("ReGenesees ",PKG.version, " [pkg] - ", GUI.version, " [gui]", sep="")

# Create authors label (Now already contained into logo.gif)
# font <- tkfont.create(family="courier", size=12, weight="bold")
# Notice: this font should be exported to the user
# font <- tkfont.create(family = "Orbitron", size = 10)
# msg <- tklabel(tt, bg = "white",
#                font =font,
#                wraplength = "8i",
#                justify = "left",
#                text = "Authors: Diego Zardetto, Raffaella Cianchetta")
# tkpack(msg, side="bottom")

# Create canvas to embed the LOGO image
canvas <- tkcanvas(tt, relief="raised", width=705, height=665, bg="white")
tkpack(canvas, side="top", fill="x")
tkcreate(canvas, "image", 350, 334, image=image_logo)

# Create a variable to track the state of the dialog window:
# active -> done = 0
# closed (by pressing start) or destroyed (by x) -> done = 1
done <- tclVar(0)
tkbind(tt, "<Destroy>", function() tclvalue(done) <- 1)

# Create buttons (Start, ...)
buttons <- tkframe(tt, bg="white")
start <- tk2button(buttons, text="START", compound="left",
                   width=15, default="active",
                   command = function() {tclvalue(done) <- 1
                                         tkdestroy(tt)}
                   )

# Pack widgets
tkpack(buttons, side="bottom", fill="x", pady="2m")
tkpack(start, side="left", expand=TRUE)
tkwm.resizable(tt, 0, 0)

# Place canvas with bottom-right corner at screen's bottom-right corner
# tkwm.geometry(tt, "-20-40")
# Place canvas with top-right corner at screen's top-right corner #PROVA!#
# tkwm.geometry(tt, "-20+40") #PROVA!#
tkwm.geometry(tt, "-0+0") #PROVA!#
tkfocus(tt)

# Wait till the window get closed
tkwait.variable(done)
}
