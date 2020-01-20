#' Coerce object to a airdas_dfr object
#'
#' Check if an object is of class \code{\link{airdas_dfr}}, or coerce it if possible.
#'
#' @param x A object to be coerced to class \code{airdas_dfr}
#'
#' @details Currently only data frames can be coerced to an object of class \code{\link{airdas_dfr}}.
#'   If \code{x} does not have column names and classes as specified in \code{\link{airdas_dfr}},
#'   then the function returns an error message detailing the first column that does not
#'   meet the \code{\link{airdas_dfr}} requirements.
#'
#' @return An object of class `airdas_dfr`
#'
#' @seealso \code{\link{airdas_dfr-class}}
#'
#' @export
as_airdas_dfr <- function(x) UseMethod("as_airdas_dfr")

#' @name as_airdas_dfr
#' @export
as_airdas_dfr.airdas_dfr <- function(x) x

#' @name as_airdas_dfr
#' @export
as_airdas_dfr.data.frame <- function(x) {
  exp.class <- list(
    Event = "character",
    EffortDot = "logical",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    EventNum = "integer",
    file_das = "character",
    line_num = "integer"
  )
  
  x.class <- lapply(x, class)
  if (!identical(exp.class, x.class)) {
    for (i in seq_along(x)) {
      if (!identical(exp.class[i], x.class[i])) {
        stop("The provided object (x) cannot be coerced to an object of class airdas_dfr ",
             "because it does not contain the correct columns. ",
             "Specifically, column ", i, " must be named '", names(exp.class)[i], "' ",
             "and be of class '", exp.class[[i]], "'\n",
             "Was x created using airdas_read()? ", 
             "See `?airdas_dfr-class` or `?as_airdas_dfr` for more details.")
      }
    }
  }
  
  class(x) <- c("airdas_dfr", setdiff(class(x), "airdas_dfr"))
  
  x
}
