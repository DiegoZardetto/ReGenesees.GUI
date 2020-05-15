.onAttach <- function(libname, pkgname){
# Load required packages (actually NOT NECESSARY due to 'Depends' in Description file)
  #require(tcltk)
  #require(tcltk2)
  #require(svMisc)
  #require(RODBC)
  #require(ReGenesees)
# Hint on how to start the session
packageStartupMessage("\n", appendLF = FALSE)
packageStartupMessage("------------------------------------------------------------------\n", appendLF = FALSE)
packageStartupMessage("> Please type 'ReGenesees.GUI()' to start the ReGenesees system. <\n", appendLF = FALSE)
packageStartupMessage("------------------------------------------------------------------\n", appendLF = FALSE)
packageStartupMessage("\n", appendLF = FALSE)
}
