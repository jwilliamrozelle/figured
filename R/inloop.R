#' @name inloop
#' @rdname inloop
#' @title  inloop
#'
#' @description  Loop that checks to see if a value of a variable in one set is in another set. Returns a vector of the same length as the xset, 
#' which in practice can be added to the xset as an additional column.
#'
#'
#' @param xset The first dataframe
#' @param yset The reference dataframe
#' @param xvar Variable holding values to check for in the second dataframe. This should be enclosed in quotation marks.
#' @param yvar Reference dataframe variable that is is checked against. This should be enclosed in quotation marks
#'
#' @author J.W. Rozelle
#'
#'
#' @export inloop
#' @examples
#'
#' # Unfortunately - I haven't created a dataset yet to demo this. However, this is the loop in practice.
#' female.df$in_hhmem <- NA
#' female.df$in_hhmem <- inloop(female.df, hh_mem.df, "clus_FemaleID", "clus_FemaleID")
#' female.df$in_hhmem <- ifelse(female.df$in_hhmem == 1, 0, 1)
#'


# loop that checks to see if a value of a variable in one set is in another set
inloop <- function (xset, yset, xvar, yvar) {
  loop_counter <- 1
  xset[, "placeholder"] <- NA # 
  for (row in 1:nrow(xset)){
    if(xset[row, paste(xvar)] %in% yset[, paste(yvar)]){
      xset[loop_counter, "placeholder"] <- 1
    } else {
      xset[loop_counter, "placeholder"] <- 0
    }
    loop_counter <- loop_counter + 1
  }
  rm(loop_counter)
  return(xset[, "placeholder"])
}



