#' @name stata_svyprop
#' @rdname stata_svyprop
#' @title  stata_svyprop
#'
#' @description  This function was intended to simplify coding for survey data. This uses the 'survey' package, and inputs necessary for a similar calculation for proportion CI's as STATA. Additionally, it outputs the data in a dataframe format for viewing
#'
#'
#' @param design survey design object
#' @param indicator The indicator for used to calculate. Should be in binary format (i.e. 0 and 1)
#' @param level Optional parameter the confidence level, defaults to 0..95
#'
#' @author J.W. Rozelle
#'
#'
#' @export stata_svyprop
#' @examples
#'
#'
#'
#'
#'


stata_svyprop = function(design, indicator, level=0.95) {
  tryCatch({
    # required packages
    require(survey)
    require(scales)
    
    # create the svyciprop object using parameters that reproduce Stata svy: prop
    indicator.svypropci <- svyciprop(~I(indicator==1), design, method="lo", df=degf(design), level = level)
    
    # Extract the proportion, lower confidence limit, upper confidence limit, and var from the object
    proportion <- indicator.svypropci[1]
    lower <- attr(indicator.svypropci, "ci")[1]
    upper <- attr(indicator.svypropci, "ci")[2]
    var <- attr(indicator.svypropci, "var")[1]
    
    # Return as dataframe
    return(cbind(proportion, lower, upper, ind.var))
    

  },
  error = function(err) {
    print("stata_svyprop_ERROR")
    return(structure(err, class = "try-error"))
    print("You've likely produced this error either because your data is not in 1 / 0 form, or because you did not put quotation marks around indicator names")
  })
}




# no_medcare_avail_inlastyr <- svyciprop(~I(no_medcare_avail_inlastyr==1), general_2015.svy, method="lo", df=degf(general_2015.svy), level = 0.95)
# ind.prop <- no_medcare_avail_inlastyr[1]
# ind.lower <- attr(no_medcare_avail_inlastyr, "ci")[1]
# ind.upper <- attr(no_medcare_avail_inlastyr, "ci")[2]
# ind.var <- attr(no_medcare_avail_inlastyr, "var")[1]
# (no_medcare_avail_inlastyr.df <- cbind(ind.prop, ind.lower, ind.upper, ind.var))
# rm(no_medcare_avail_inlastyr, ind.prop, ind.lower, ind.upper, ind.var, no_medcare_avail_inlastyr.df)