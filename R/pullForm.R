#' @name pullForm
#' @rdname pullForm
#' @title  pullForm
#'
#' @description  This function pulls forms from an odk server using ODK Briefcase. It's likely useful to set some of the parameters in variables, as they will be necessary to export the forms.
#'
#'
#' @param url The URL to the ODK Aggregate server. This may be something like `https://kc.humanitarianresponse.info/<USERID>/`. Be sure to include your userid here if the server requires it.
#' @param userid The user id for your odk aggregate server account. This must have permission to view submissions.
#' @param passwd The Password for your account. Be careful about keeping this in your R code. Best practice is likely to put this in your .Renviron file.
#' @param formid The unique formid string for your server.
#' @param storage Optional parameter, and defaults to your current working directory.
#'
#' @author J.W. Rozelle
#'
#'
#' @export pullForm
#' @examples
#'
#' # Set parameters
#' odkUserID <- "<USERNAME_GOES_HERE>"
#' odkURL <- paste0("https://kc.humanitarianresponse.info/", odkUserID, "/")
#' odkPW <- "<SECRET_PASSWORD>"
#' odkFormid <- "my_odk_form_id"
#' odkStorage <- paste0(my_directory, "/", "ODK")
#' 
#' 
#' # Run command
#' pullForm(odkURL, odkUserID, odkPW, odkFormid, odkStorage)
#' 
#'


pullForm <- function(url, userid, passwd, formid, storage = getwd()) {
  require(rJava)
  
  tryCatch({
  # show the filepath to odkbriefcase
  odkbc <- system.file("java/ODK-Briefcase-v1.17.1.jar", package = "figured")
  #url_user <- paste0(url, "/", userid, "/")
  # construct the odk briefcase system command
  pullODK_cmd <- paste0(
    'java -jar "',
    odkbc,
    '" --pull_aggregate --storage_directory "',
    storage,
    '" --odk_url "',
    url,
    '" --odk_username ',
    userid,
    ' --odk_password "',
    passwd,
    '" -id "',
    formid,
    '"'
  )
  
  # Give some output about what's happening behind the scenes
  message("Next, running... ")
  cat(pullODK_cmd)
  message(" ...system command.")
  
  # run system command constructed above
  system(pullODK_cmd)},
  # return an error if there's an issue
  error = function(err) {
    print("procFreq_ERROR")
    return(structure(err, class = "try-error"))
    print("You've likely produced this error either because your data is not in 1 / 0 form, or because you did not put quotation marks around variable names")
  })
}


