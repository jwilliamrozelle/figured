#' @name killDbConnections
#' @rdname killDbConnections
#' @title  killDbConnections
#'
#' @description  Kills database connections
#'
#' @author Unknown
#'
#'
#' @export killDbConnections
#' @examples
#'
#' # Quite simple - just use without parameters
#' killDbConnections()
#' 
#'


killDbConnections <- function () {
  require(RMySQL)
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for(con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}