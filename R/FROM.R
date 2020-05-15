`FROM` <- function(fun, pkg)
#########################################################
# Retrieve function 'fun' from package 'pkg' namespace, #
# even if it is not exported.                           #
#########################################################
{
    get(as.character(fun), envir = asNamespace(as.character(pkg)), inherits = FALSE)
}




################################
# ReGenesees.GUI FROM register #
################################

#### pkg: utils
# in: EditMisc.RG.R
`utils_edit.data.frame` <- FROM(fun = "edit.data.frame", pkg = "utils")

# in: RGhelp.R
`utils_index.search` <- FROM(fun = "index.search", pkg = "utils")
