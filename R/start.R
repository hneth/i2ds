## start.R | i2ds
## hn | uni.kn | 2020 10 01
## ---------------------------

## Open pkg guide: ----------

#' Opens user guide of the i2ds package. 
#'
#' @import utils
#'

i2ds.guide <- function() {

  # utils::vignette(topic = "User Guide", package = "i2ds")
  
  # utils::browseVignettes(package = "i2ds")

  print("ToDo")
  
}

## Initialize pkg: ---------- 

.onAttach <- function(libname, pkgname) {
  
  ## Welcome message: ------
  
  pkg_version <- utils::packageVersion("i2ds", lib.loc = NULL)
  
  # welcome_message <- paste0("Welcome to i2ds!")
  welcome_message <- paste0("Welcome to i2ds (v", pkg_version, ")!")
  
  packageStartupMessage(welcome_message)
  
  ## User guidance: ------
  
  ## Roll dice: ------
  dice <- sample(1:6, 1)
  
  if (dice == -77) {
    pkg_version <- utils::packageVersion("i2ds", lib.loc = NULL)
    pkg_message <- paste0("Running i2ds (v", pkg_version, ")...")
    
    packageStartupMessage(" ")
    packageStartupMessage(pkg_message)
    packageStartupMessage(" ")
  }
  
  if (dice == -99) {
    packageStartupMessage(" ")
    packageStartupMessage("citation('i2ds') provides citation info.")
    packageStartupMessage(" ")
  }
  
  ## all cases:
  # packageStartupMessage("i2ds() opens user guides.")
  
}

## ToDo: ------
## - ...

## eof. ----------
