#' @name pullForm
#' @rdname pullForm
#' @title  pullForm
#'
#' @description  This function pulls forms from an odk server using ODK Briefcase. It's likely useful to set some of the parameters in variables, as they will be necessary to export the forms.
#' 
#' @note This function will check for and download ODK Briefcase. Make sure your firewalls allow for this the first time you run an odk function.
#'
#' @param url The URL to the ODK Aggregate server. This may be something like `https://kc.humanitarianresponse.info/<USERID>/`. Be sure to include your userid here if the server requires it.
#' @param userid The user id for your odk aggregate server account. This must have permission to view submissions.
#' @param passwd The Password for your account. Be careful about keeping this in your R code. Best practice is likely to put this in your .Renviron file.
#' @param formid The unique formid string for your server.
#' @param storage Optional parameter, and defaults to your current working directory.
#' @param central Specify as TRUE if pulling from an ODK Central server. It is FALSE by default, which means that the command will assume an aggregate type server.
#' @param project_id When pulling from a central server, it is essential to specify a project_id. 
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


pullForm <- function(url = NULL, 
                     userid = NULL, 
                     passwd = NULL, 
                     formid = NULL, 
                     storage = getwd(), 
                     central = FALSE, 
                     project_id = NULL
                     ) {
  figured::odkbc_CheckAndDL()
  
  # produce errors if required arguments are not specified.
  #   Check if URL is specified
  if(is.null(url)) {
    stop("You must specify a URL.", call. = TRUE)
  }
  #   Check if userid is specified
  if (is.null(userid)) {
    stop("You must specify a user id. It must be an email if using ODK Central", call. = TRUE)
  }
  #   Check if password is specified
  if (is.null(passwd)) {
    stop("You must specify a password.", call. = TRUE)
  }
  #   Check if formid is specified
  if (is.null(passwd)) {
    stop("You must specify a form id.", call. = TRUE)
  }
  # if central was specified, check for project_id
  if (central && is.null(project_id)) {
    stop("If you are using a central server type, you must specify a project ID.", call. = TRUE)
  }
  
  tryCatch({
  # show the filepath to odkbriefcase
  odkbc <- system.file("inst/java/ODK-Briefcase.jar", package = "figured")
  #url_user <- paste0(url, "/", userid, "/")
  # construct the odk briefcase system command
  pullODK_cmd <- paste0(
    'java -jar "',
    odkbc)
  
  if (!central) {
    paste0(pullodk_cmd,
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
  } else if (central) {
    paste0(pullodk_cmd,
           '" --pull_central --storage_directory "',
           storage,
           '" --odk_url "',
           url,
           '" --odk_email ',
           userid,
           ' --odk_password "',
           passwd,
           '" -id "',
           formid,
           '" --project_id "',
           project_id,
           '"'
    )
  } else {
    message("Error with specifying central, must be TRUE, FALSE or left missing")
  }
  
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
    print("It appears that the infromation you have entered is not correct. Check that all inputs are valid, and try again.")
  })
}


