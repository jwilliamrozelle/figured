#' @name dup_check
#' @rdname dup_check
#' @title  dup_check
#'
#' @description  This function finds duplicates and returns a boolean vector. Each instance of a duplicate is marked as true, rather only the second instance.
#'
#'
#' @param xset The dataframe
#' @param variable The variable that will be examined for duplicates. This should be enclosed in quotation marks.
#'
#' @author J.W. Rozelle
#'
#'
#' @export dup_check
#' @examples
#'
#' # Unfortunately - I haven't created a dataset yet to demo this. However, this is the loop in practice.
#' # Check for duplicate female ID's
#' female.df$dup_femID <- NA
#' female.df$dup_femID <- dup_check(female.df, "clus_FemaleID")
#' 

dup_check <- function(xset, variable) {
  duplicates.df <- xset #create set for duplication
  xset[, "placeholder"] <- NA
  
  # Remove duplicates based on Sepal.Width columns
  duplicates.df <- duplicates.df[duplicated(duplicates.df[, paste(variable)]), ]
  xset[, "placeholder"] <- inloop(xset, duplicates.df, paste(variable), paste(variable))
  return(xset[, "placeholder"])
}