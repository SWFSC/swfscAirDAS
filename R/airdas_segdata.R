#' Summarize AirDAS data for a continuous effort section
#' 
#' Summarize AirDAS effort data by effort segment, while averaging conditions
#' 
#' @param x \code{airdas_df} object, 
#'   or a data frame that can be coerced to a \code{airdas_df} object.
#'   Must must be filtered for 'OnEffort' events and 
#'   contain a single continuous effort section of AirDAS data; 
#'   see the Details section below
#' @param conditions see \code{\link{airdas_effort}}, or
#'   see Details section for more information
#' @param segdata.method character; either \code{"avg"} or \code{"maxdist"}.
#'   \code{"avg"} means the condition values will be
#'   calculated as a weighted average by distance, while
#'   \code{"maxdist"} means the condition values will be those recorded
#'   for the longest distance during that segment
#' @param seg.lengths numeric; length of the modeling segments 
#'   into which \code{x} will be chopped
#' @param section.id numeric; the ID of \code{x} (the current continuous effort section)
#' @param ... ignored
#' 
#' @details This function should be called by one of the airdas_chop functions, 
#'   e.g. \code{\link{airdas_chop_equal}}, meaning
#'   users should work to avoid calling it themselves.
#'   It loops through the events in \code{x}, calculating and storing relevant
#'   information for each modeling segment as it goes. 
#'   Because \code{x} is a continuous effort section, it must begin with 
#'   a "T" or "R" event and end with the corresponding "E" or "O" event.
#' 
#'   For each segment, this function reports the 
#'   segment ID, transect code, the start/end/midpoints (lat/lon), segment length, 
#'   year, month, day, time, observers, and average conditions.
#'   
#'   The segment ID is designated as \code{eff_id} _ index of the modeling segment.
#'   Thus, if \code{section.id} is \code{1}, then the segment ID for 
#'   the second segment from \code{x} is \code{"1_2"}.
#'   
#'   The average condition values are calculated as a weighted average by distance, 
#'   and reported for the following:
#'   Beaufort, cloud cover, altitude in feet, aircraft speed in knots, jellyfish code, 
#'   and the percentage of the segment with each of haze, kelp, and red tide. 
#'   For logical columns such as Haze, the reported value is the percentage 
#'   (in decimals) of the segment in which that condition was \code{TRUE}.   
#'   Transect number and file name are also also incldued in the segdata output;
#'   these values are (should be) all consistent across the whole effort section,
#'   and thus across all segments in \code{x}.
#'   
#'   \code{\link[swfscMisc]{bearing}} and \code{\link[swfscMisc]{destination}}, 
#'   method = "vincenty",
#'   are used to help calculate the segment start, mid, and end points.
#'   
#' @return Data frame with the segdata information described above
#'   and in \code{\link{airdas_effort}}
#' 
#' @keywords internal
#' 
#' @seealso airdas_segdata_max
#' 
#' @export
airdas_segdata <- function(x, ...) UseMethod("airdas_segdata")


#' @name airdas_segdata
#' @export
airdas_segdata.data.frame <- function(x, ...) {
  airdas_segdata(as_airdas_df(x), ...)
}


#' @name airdas_segdata
#' @export
airdas_segdata.airdas_df <- function(x, conditions, segdata.method, seg.lengths, section.id, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  conditions.acc <- c(
    "Bft", "CCover", "Jelly", "HorizSun", "VertSun", 
    "Haze", "Kelp", "RedTide", "AltFt", "SpKnot", 
    "ObsL", "ObsB", "ObsR", "Rec", "VLI", "VLO", "VB", "VRI", "VRO"
  )
  if (!all(conditions %in% conditions.acc))
    stop("Was this function called by onr of the swfscAirDAS chop functions? ",
         "Please ensure all components of the conditions argument are ",
         "one of the following accepted values:\n",
         paste(conditions.acc, collapse  = ", "))
  
  
  segdata.method.acc <- c("avg", "maxdist")
  if (!(segdata.method %in% segdata.method.acc))
    stop("Was this function called by a _chop_ function? ",
         "segdata.method must be one of the following:\n",
         paste(segdata.method.acc, collapse = ", "))
  
  
  if (!("dist_from_prev" %in% names(x))) 
    stop("x must contain a 'dist_from_prev' column; ", 
         "was this function called by the top-level effort function?")
  
  stopifnot(
    inherits(seg.lengths, c("numeric", "integer")), 
    inherits(section.id, c("numeric", "integer"))
  )
  
  if (!.equal(sum(seg.lengths), sum(x$dist_from_prev)))
    stop("The sum of the seg.lengths values does not equal the sum of the ", 
         "x$dist_from_prev' values; ", 
         "was this function called by the top-level effort function?")
  
  
  #----------------------------------------------------------------------------
  # Prep stuff
  # das.df <- x
  
  ### Prep - get the info that is consistent for the entire effort length
  # ymd determined below to be safe
  df.out1 <- data.frame(
    file = unique(x$file_das), transect = unique(na.omit(x$Trans)), 
    event = x$Event[1], 
    stringsAsFactors = FALSE
  )
  
  if (nrow(df.out1) != 1) {
    browser()
    warning("Error in airdas_segdata(): ", 
         "There are unexpected inconsistencies in continuous effort section. ", 
         "Please report this as an issue")
    }
  df.out1 <- df.out1[1, ]
  
  
  #----------------------------------------------------------------------------
  segdata.all <- .segdata_proc(
    das.df = x, conditions = conditions, segdata.method = segdata.method,
    seg.lengths = seg.lengths, section.id = section.id, df.out1 = df.out1
  )
  
  
  #----------------------------------------------------------------------------
  segdata.all %>% 
    select(.data$seg_idx, .data$event, .data$transect, 
           .data$file, .data$stlin, .data$endlin, 
           .data$lat1, .data$lon1, .data$lat2, .data$lon2, 
           .data$mlat, .data$mlon, .data$dist, 
           .data$mDateTime, .data$year, .data$month, .data$day, .data$mtime, 
           everything())
}