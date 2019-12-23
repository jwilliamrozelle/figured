#' @name odkbc_CheckAndDL
#' @rdname odkbc_CheckAndDL
#' @title  odkbc_CheckAndDL
#'
#' @description  This function is embedded within other ODK functions to make sure that ODK is downloaded in the correct directory
#' 
#' @note This function is not necessary to by itself. Anytime ODK is needed in this package, the function is run.
#' 
#'
#' @author J.W. Rozelle
#'
#'
#' @export odkbc_CheckAndDL
#' @examples
#'
#' # Run function
#' odkbc_CheckAndDL()
#' 
#'



odkbc_CheckAndDL <- function() {
  require(utils)
  # Check whether inst exists, if not, then create it.
  if (!dir.exists("inst")) {
    dir.create("inst")
  }
  
  # create java subdirectory
  if (!dir.exists("inst/java/")) {
    dir.create("inst/java/")
  }
  
  # Set destination
  destination <-
    paste0(system.file("inst/java/", package = "figured"),
           "/ODK-Briefcase.jar")
  
  # Set url for version of ODK that this will use
  odkDLSource <-
    "https://github.com/opendatakit/briefcase/releases/download/v1.17.1/ODK-Briefcase-v1.17.1.jar"
  
  message("Checking whether ODK Briefcase exists in the proper directory...")
  
  # Check to see whether ODK briefcase exists in the proper destination, return messages throughout the function
  if (!file.exists(destination)) {
    message("It appears that ODK has not yet been downloaded and installed in the package.")
    writeLines("\nAttempting to download ODK-Briefcase.jar.../n")
    # if the ODK Briefcase file does not exist, download it.
    download.file(odkDLSource, destination, method = "auto", mode = "wb")
    
    # After completing the download function, check to see whether the file exists in the proper directory
    if (file.exists(destination)) {
      writeLines("\nODK appears to have been successfully downloaded.")
    } else { # If file does not exist in the proper directory, return a message stating this.
      
      writeLines("\nODK download appears to have failed. Please try again")
    }
  } else {
    writeLines("\nODK jar file exists and is located in the proper directory")
  }
  
}