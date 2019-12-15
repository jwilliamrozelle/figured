#' @name procMeans
#' @rdname procMeans
#' @title  procMeans
#'
#' @description  This function was created because the available options in R for simple means tables indicators didn't produce the output I wanted. This function will return a dataframe.
#'
#'
#' @param data The dataframe that contains the information you want
#' @param vari_col The the variable column. This should be enclosed in quotations marks. For example, data$variable would be noted here as "variable"
#' @param indic Optional parameter for the name of the indicator
#' @param group_col Optional parameter.This would be the column used to stratify results.
#'
#' @author J.W. Rozelle
#'
#'
#' @export procMeans
#' @examples
#'
#' # Get the mtcars data
#' data(mtcars)
#'
#' # Run the simplest version of the variable
#' procMeans(mtcars, "mpg")
#'
#'
#' # Name the indicator and disaggregate by the cyl column
#' procMeans(data = mtcars, vari_col = "mpg", indic = "Average MPG by cyl", group_col = "cyl")
#'
#'


procMeans =
  function(data, vari_col, indic, group_col) {
    tryCatch({
      require(dplyr)
      require(lazyeval)
      the.mean <-
        interp( ~ mean(var, na.rm = TRUE), var = as.name(vari_col))
      the.median <-
        interp( ~ median(var, na.rm = TRUE), var = as.name(vari_col))
      the.min <-
        interp( ~ min(var, na.rm = TRUE), var = as.name(vari_col))
      the.max <-
        interp( ~ max(var, na.rm = TRUE), var = as.name(vari_col))
      the.total <-
        interp( ~ sum(!is.na(var)), var = as.name(vari_col))
      the.missing <-
        interp( ~ sum(is.na(var)), var = as.name(vari_col))
      the.name  <- interp( ~ var, var = as.name("indic"))
      if (!missing(indic)) {
        if (!missing(group_col)) {
          data %>%
            group_by_(lazyeval::interp( ~ grp, grp = as.name(group_col))) %>%
            summarise_(
              indicator = the.name,
              mean = the.mean,
              median = the.median,
              min = the.min,
              max = the.max,
              Total = the.total,
              Missing = the.missing
            ) -> result
        } else {
          data %>%
            summarise_(
              indicator = the.name,
              mean = the.mean,
              median = the.median,
              min = the.min,
              max = the.max,
              Total = the.total,
              Missing = the.missing
            ) -> result
        }
      } else {
        if (!missing(group_col)) {
          data %>%
            group_by_(lazyeval::interp( ~ grp, grp = as.name(group_col))) %>%
            summarise_(
              mean = the.mean,
              median = the.median,
              min = the.min,
              max = the.max,
              Total = the.total,
              Missing = the.missing
            ) -> result
        } else {
          data %>%
            summarise_(
              mean = the.mean,
              median = the.median,
              min = the.min,
              max = the.max,
              Total = the.total,
              Missing = the.missing
            ) -> result
        }
      }

      return(result)
    },
    error = function(err) {
      print("procMeans_ERROR")
      return(structure(err, class = "try-error"))
      print(
        "You've likely produced this error either because you did not put quotation marks around variable names"
      )
    })
  }
