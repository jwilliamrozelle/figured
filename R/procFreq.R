#' @name procFreq
#' @rdname procFreq
#' @title  procFreq
#'
#' @description  This function was created because the available options in R for frequency tables of 1 / 0 indicators was insufficient.
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
#' @export procFreq
#' @examples
#'
#' # Get the mtcars data
#' data(mtcars)
#'
#' # Run the simplest version of the variable
#' procFreq(mtcars, "am")
#'
#'
#' # Name the indicator and disaggregate by the cyl column
#' procFreq(data = mtcars, vari_col = "am", indic = "Indicator Description by cyl", group_col = "cyl")
#'
#'


procFreq = function(data, vari_col, indic, group_col) {
  tryCatch({
    require(dplyr)
    require(lazyeval)
    require(scales)
    the.percent <-
      interp(~ percent(mean(var, na.rm = TRUE)), var = as.name(vari_col))
    the.mean <-
      interp(~ mean(var, na.rm = TRUE), var = as.name(vari_col))
    the.sum <-
      interp(~ sum(var, na.rm = TRUE), var = as.name(vari_col))
    the.total <-
      interp(~ sum(!is.na(var)), var = as.name(vari_col))
    the.missing <-
      interp(~ sum(is.na(var)), var = as.name(vari_col))
    the.name  <- interp(~ var, var = as.name("indic"))
    if (!missing(indic)) {
      if (!missing(group_col)) {
        data %>%
          group_by_(lazyeval::interp(~ grp, grp = as.name(group_col))) %>%
          summarise_(
            indicator = the.name,
            percent = the.percent,
            proportion = the.mean,
            n = the.sum,
            Total = the.total,
            Missing = the.missing
          ) -> result
      } else {
        data %>%
          summarise_(
            indicator = the.name,
            percent = the.percent,
            Proportion = the.mean,
            n = the.sum,
            Total = the.total,
            Missing = the.missing
          ) -> result
      }
    } else {
      if (!missing(group_col)) {
        data %>%
          group_by_(lazyeval::interp(~ grp, grp = as.name(group_col))) %>%
          summarise_(
            percent = the.percent,
            Proportion = the.mean,
            n = the.sum,
            Total = the.total,
            Missing = the.missing
          ) -> result
      } else {
        data %>%
          summarise_(
            percent = the.percent,
            Proportion = the.mean,
            n = the.sum,
            Total = the.total,
            Missing = the.missing
          ) -> result
      }
    }

    return(result)
  },
  error = function(err) {
    print("procFreq_ERROR")
    return(structure(err, class = "try-error"))
    print("You've likely produced this error either because your data is not in 1 / 0 form, or because you did not put quotation marks around variable names")
  })
}
