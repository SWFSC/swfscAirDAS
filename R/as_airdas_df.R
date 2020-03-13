#' Coerce object to a airdas_df object
#'
#' Check if an object is of class \code{\link{airdas_df}}, or coerce it if possible.
#'
#' @param x An object to be coerced to class \code{airdas_df}
#'
#' @details Currently only data frames can be coerced to an object 
#'   of class \code{\link{airdas_df}}.
#'   If \code{x} does not have column names, classes, and contents 
#'   as specified in \code{\link{airdas_df}},
#'   then the function returns an error message detailing the first column that does not
#'   meet the \code{\link{airdas_df}} requirements. 
#'
#' @return An object of class \code{\link{airdas_df}}
#'
#' @seealso \code{\link{airdas_df-class}}
#'
#' @export
as_airdas_df <- function(x) UseMethod("as_airdas_df")

#' @name as_airdas_df
#' @export
as_airdas_df.airdas_df <- function(x) x

#' @name as_airdas_df
#' @export
as_airdas_df.data.frame <- function(x) {
  # Check that columns have correct names and classes
  exp.class <- list(
    Event = "character",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    OnEffort = "logical",
    Trans = "character",
    Bft = "numeric",
    CCover = "numeric",
    Jelly = "numeric",
    HorizSun = "numeric",
    HKR = "character",
    Haze = "logical",
    Kelp = "logical",
    RedTide = "logical",
    AltFt = "numeric",
    SpKnot = "numeric",
    ObsL = "character",
    ObsB = "character",
    ObsR = "character",
    Rec = "character",
    VLI = "character",
    VLO = "character",
    VB = "character",
    VRI = "character",
    VRO = "character",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    EffortDot = "logical", 
    EventNum = "character",
    file_das = "character",
    line_num = "integer",
    file_type = "character"
  )
  exp.class.names <- names(exp.class)
  
  x.class <- lapply(x, class)
  
  for (i in seq_along(exp.class)) {
    name.curr <- exp.class.names[i]
    x.curr <- x.class[[name.curr]]
    
    if (!identical(x.curr, exp.class[[i]])) {
      stop("The provided object (x) cannot be coerced to an object of class airdas_df ",
           "because it does not contain the correct columns. ",
           "Specifically, it must contain a column with the name '", names(exp.class)[i], "' ",
           "and class '", exp.class[[i]], "'\n",
           "Was x created using airdas_process()? ", 
           "See `?as_airdas_df` or `?airdas_df-class` for more details.")
    }
  }
  
  # Check that all of OnEffort is either TRUE/FALSE; no NAs
  if (any(is.na(x$OnEffort))) 
    stop("The following rows have OnEffort values of NA, ", 
         "and thus this object cannot be coerced to an airdas_df object: ", 
         paste(x$line_num[is.na(x$OnEffort)], collapse = ", "))
  
  # Check for no datetime/lat/lon NAs in on-effort events
  x.oneff <- x[x$OnEffort, ]
  if (any(is.na(x.oneff$Lat) | is.na(x.oneff$Lon) | is.na(x.oneff$DateTime)))
    stop("The following rows have NA values in the Lat, Lon, and/or DateTime columns, ", 
         "and thus this object cannot be coerced to an airdas_df object: ", 
         paste(sort(unique(c(x.oneff$line_num[is.na(x.oneff$Lat)], 
                             x.oneff$line_num[is.na(x.oneff$Lon)], 
                             x.oneff$line_num[is.na(x.oneff$DateTime)]))), 
               collapse = ", "))
  
  # Check for no deleted events
  if (any(x$Event == "#"))
    warning("This airdas_df object has some deleted events, meaning ", 
            "some \"#\" events. Should these be removed?")
  
  # Check that file_type column has an expected value
  file.type.acc <- c("turtle", "caretta", "survey", "phocoena")
  if (!(length(unique(x$file_type)) & all(x$file_type %in% file.type.acc)))
    stop("The file_type column values must be 1) all the same and 2) one of: ", 
         paste(file.type.acc, collapse = ", "))
  
  # Add class and return
  class(x) <- c("airdas_df", setdiff(class(x), "airdas_df"))
  
  x
}
