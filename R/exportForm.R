#'
#' @name exportForm
#' @rdname exportForm
#' @title  exportForm
#'
#' @description  This function pulls forms from an odk server using ODK Briefcase. It's likely useful to set some of the parameters in variables, as they will be necessary to export the forms.
#' 
#' @note This function will check for and download ODK Briefcase. Make sure your firewalls allow for this the first time you run an odk function.
#'
#' @param formid The unique formid string for your server.
#' @param exportDir Export Directory. If left NULL, the function will use odk_export in the working directory and create odk_export in the working directory if it doesn't exist.
#' @param fileName Name of csv, must end in '.csv'
#' @param storage Optional parameter, and defaults to your current working directory.
#' @param pemKey Optional parameter, if the data is encrypted - this should be the filepath to the PEM key
#' @param geojson Optional, logical parameter, defaults to TRUE. Mark false if you do not wish to include a geojson with geodata
#' @param mediaInclude Optional, logical parameter, defauts to TRUE. If you wish to exclude media, mark FALSE
#' @param splitMultiple Optional, logical parameter, defaults to TRUE. IF you do not wish to split 'select_multiple' type questions into separate columns, mark false.
#' @param pullBefore Logical parameter, defaults to False. If you want to pull before export. Regardless, at least one manual pull is required fefore .
#' @param rm_group_names Optional, defualts to TRUE. If you wish to include group names, mark FALSE
#'
#' @author J.W. Rozelle
#' 
#'
#'
#' @export exportForm
#' @examples
#'
#' # specify arguments
#' 
#' odkbc_storage   <- "<your/odk/storage/path>"
#' odkbc_export    <- "<your/export/file/path>"
#' random_id       <- "random_form_id"
#' random_filename <- "/exportdataname.csv"
#' odk_username    <- "MY_USERNAME"
#' odk_url         <- paste0("https://kc.humanitarianresponse.info/", odk_username)
#' 
#' pullForm(odk_url, odk_username, "PASSWORD", random_id, storage = odkbc_storage)
#' 
#' # Export ODK
#' exportForm(random_id, odkbc_export, random_filename, odkbc_storage)
#' 
#' 
#' 


exportForm <-
  function(formid = NULL,
           exportDir = NULL,
           fileName = NULL,
           storage = getwd(),
           pemKey = NULL,
           geojson = FALSE,
           mediaInclude = TRUE,
           splitMultiple = TRUE,
           pullBefore = FALSE,
           rm_group_names = TRUE) {
    
    # make sure ODK Briefcase is downloaded and in the correct directory
    figured::odkbc_CheckAndDL()
    
    if (is.null(formid)) {
      message("The form ID has not been specified. You must specify the formID")
    } else {
      tryCatch({
        # show the filepath to odkbriefcase
        odkbc <-
          system.file("java/ODK-Briefcase.jar", package = "figured")
        
        # if export directory is not specified - use odk_export in the working directory. Create it if it doesn't exist
        if (is.null(exportDir)) {
          # checking for whether exportDir was specified, if not then proceed specify it.
          exportDir <- paste0(getwd(), "\\odk_export")
          if (!base::dir.exists(exportDir)) {
            # check for whether odk_export folder exists in the working directory, if not then create it.
            base::dir.create(exportDir)
          }
        }
        
        # If no export filename is specified, make the formid the export filename.
        if (is.null(fileName)) {
          fileName <- paste0(formid, ".csv")
        }
        
        #url_user <- paste0(url, "/", userid, "/")
        # construct the odk briefcase system command
        exportODK_cmd <- paste0(
          'java -jar "',
          odkbc,
          '" --export --form_id "',
          formid,
          '" --storage_directory "',
          storage,
          '" --export_directory "',
          exportDir,
          '" --export_filename "',
          fileName,
          '"'
        )
        
        # if a PEM key is specified, add it to the command
        if (!is.null(pemKey)) {
          exportODK_cmd <- paste0(exportODK_cmd,
                                  " --pem_file ",
                                  '"',
                                  pemKey,
                                  '"')
        }
        
        # if the geojson argument is true, add the geojson flag
        if (!mediaInclude) {
          exportODK_cmd <- paste0(exportODK_cmd,
                                  " --exclude_media_export")
        }
        
        # if the mediaInclude argument is FALSE, add the flag to exclude media
        if (geojson) {
          exportODK_cmd <- paste0(exportODK_cmd,
                                  " --include_geojson")
        }
        
        # if the splitMultiple argument is TRUE, add the flag to split select multiple into separate columns
        if (splitMultiple) {
          exportODK_cmd <- paste0(exportODK_cmd,
                                  " --split_select_multiples")
        }
        
        # if the splitMultiple argument is TRUE, add the flag to split select multiple into separate columns
        if (pullBefore) {
          exportODK_cmd <- paste0(exportODK_cmd,
                                  " --pull_before")
        }
        
        # if the rm_group_names argument is TRUE, add the flag to Remove group names from column names
        if (rm_group_names) {
          exportODK_cmd <- paste0(exportODK_cmd,
                                  " --remove_group_names")
        }
        
        # Give some output about what's happening behind the scenes
        message("Next, running... ")
        cat(exportODK_cmd)
        message(" ...system command.")
        
        # run system command constructed above
        system(exportODK_cmd)
      },
      # return an error if there's an issue
      error = function(err) {
        message("exportODK_cmd_ERROR")
        return(structure(err, class = "try-error"))
        message("ERROR ERROR ERROR! OH NO!!!")
      })
    }
  }