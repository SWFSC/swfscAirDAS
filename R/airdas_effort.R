#' Summarize AirDAS effort
#'
#' Chop AirDAS data into effort segments
#' 
#' @param x \code{airdas_df} object; output from \code{\link{airdas_process}}, 
#'  or a data frame that can be coerced to a \code{airdas_df} object
#' @param method character; method to use to chop AirDAS data into effort segments
#'   Can be "equallength" or "condition" (case-sensitive) to use 
#'   \code{\link{airdas_chop_equal}} or \code{\link{airdas_chop_condition}}, respectively
#' @param sp.codes character; species code(s) to include in segdata. 
#'   These code(s) will be converted to lower case to match \code{\link{airdas_sight}} 
#' @param dist.method character;
#'   method to use to calculate distance between lat/lon coordinates.
#'   Can be "greatcircle" to use the great circle distance method (TODO - add ref),
#'   or one of "lawofcosines", "haversine", or "vincenty" to use
#'   \code{\link[swfscMisc]{distance}}. Default is "greatcircle"
#' @param ... arguments passed to the chopping function specified using \code{method}
#' 
#' @details This is the top-level function for chopping processed AirDAS data 
#'   into modeling segments (henceforth 'segments'), and assigning sightings 
#'   and related information (e.g., weather conditions) to each segment. 
#'   This function returns data frames with all relevant information for the 
#'   effort segments and associated sightings ('segdata' and 'siteinfo', respectively). 
#'   Before chopping, the AirDAS data is filtered for events (rows) where either
#'   the 'OnEffort' column is \code{TRUE} or the 'Event' column is "E" or "O". 
#'   In other words, the data is filtered for continuous effort sections (henceforth 'effort sections'), 
#'   where effort sections run from "T"/"R" to "E"/"O" events (inclusive), 
#'   and then passed to the chopping function specified using \code{method}. 
#' 
#'   Currently, two chopping methods are available: "equallength" and "condition". 
#'   The "equallength" method consists of
#'   chopping effort sections into equal-length segments of length \code{seg.km}, 
#'   and doing a weighted average of the conditions for the length of that segment. 
#'   See \code{\link{airdas_chop_equal}} for more details about this method, 
#'   including arguments that must be passed to it via \code{...}.
#'   
#'   When using the "condition" method, effort sections are chopped into segments
#'   every time a condition changes, 
#'   therby ensuring that the conditions are consistent across the entire segment.
#'   See \code{\link{airdas_chop_condition}} for more details about this method, 
#'   including arguments that must be passed to it via \code{...}.
#'   
#'   The sightings included in the segdata counts are filtered as follows: 
#'   Beaufort is less than or equal to five, the declination angle is less than 78, 
#'   it is a standard sighting, and the sighting was made while on effort. 
#'   Siteinfo, which contains all sightings in \code{x} (see below), 
#'   contains a column ('included') that indicates whether or not the sighting was
#'   included in the segdata counts.
#'   
#'   All on effort events must not have \code{NA} Lat or Lon values; 
#'   remember that in \code{\link{airdas_process}}, S Lat/Lon values
#'   were passed to 1 events. 
#'   TODO: Should this function verbosely remove these events?
#' 
#'   The distance between the lat/lon points of subsequent events
#'   is calculated using the method specified in \code{dist.method}
#' 
#' @return List of three data frames: 
#'   \itemize{
#'     \item segdata: one row for every segment, and columns for information including
#'       unique segment number, start/end/midpoint coordinates, conditions (e.g. Beaufort), 
#'       and number of sightings and number of animals on that segment for every species 
#'       indicated in \code{sp.codes}. 
#'     \item siteinfo: details for all sightings in \code{x}, including: 
#'       the unique segment number it is associated with, segment mid points (lat/lon), 
#'       and whether the sighting was included in the segdata counts (column \code{included}), 
#'       in addition to the other output information described in \code{\link{airdas_sight}}.
#'     \item randpicks: see \code{\link{airdas_chop_equal}}
#'   }
#' 
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.proc <- airdas_process(y)
#' 
#' # Using "equallength" method
#' y.rand <- system.file("airdas_sample_randpicks.csv", package = "swfscAirDAS")
#' airdas_effort(
#'   y.proc, method = "equallength", sp.codes = c("mn", "bm"), 
#'   seg.km = 3, randpicks.load = y.rand, num.cores = 1
#' )
#' 
#' # Using "condition" method
#' airdas_effort(
#'   y.proc, method = "condition", sp.codes = c("mn", "bm"), 
#'   seg.km.min = 0.05, num.cores = 1
#' )
#' 
#' @export
airdas_effort <- function(x, ...) UseMethod("airdas_effort")


#' @name airdas_effort
#' @export
airdas_effort.data.frame <- function(x, ...) {
  airdas_effort(as_airdas_df(x), ...)
}


#' @name airdas_effort
#' @export
airdas_effort.airdas_df <- function(x, method, sp.codes, 
                                    dist.method = "greatcircle", ...) {
  #----------------------------------------------------------------------------
  # Input checks
  methods.acc <- c("equallength", "condition")
  if (!(length(method) == 1 & (method %in% methods.acc))) 
    stop("method must be a string, and must be one of: ", 
         paste0("\"", paste(methods.acc, collapse = "\", \""), "\""))
  
  #Check for dist.method happens in .dist_from_prev()
  
  
  #----------------------------------------------------------------------------
  # Prep
  # Convert species codes to lower case
  sp.codes <- tolower(sp.codes)
  
  # Filter for and number continuous effort sections
  #   'on effort + 1' is to capture O/E event
  x.oneff.which <- sort(unique(c(which(x$OnEffort), which(x$OnEffort) + 1)))
  stopifnot(all(between(x.oneff.which, 1, nrow(x))))
  
  x.oneff <- x[x.oneff.which, ]
  rownames(x.oneff) <- NULL
  
  x.oneff$dist_from_prev <- swfscDAS::.dist_from_prev(x.oneff, dist.method)

  
  #----------------------------------------------------------------------------
  # Chop and summarize effort using specified method
  if (method == "equallength") {
    eff.list <- airdas_chop_equal(as_airdas_df(x.oneff), ...)
    x.eff <- eff.list[[1]]
    segdata <- eff.list[[2]]
    randpicks <- eff.list[[3]]
    
  } else if (method == "condition") {
    eff.list <- airdas_chop_condition(as_airdas_df(x.oneff), ...)
    x.eff <- eff.list[[1]]
    segdata <- eff.list[[2]]
    randpicks <- NULL
  }
  
  x.eff.names <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort", "Trans", "Bft", 
    "CCover", "Jelly", "HorizSun", "VertSun", 
    "HKR", "Haze", "Kelp", "RedTide", 
    "AltFt", "SpKnot", "ObsL", "ObsB", "ObsR", "Rec", 
    "VLI", "VLO", "VB", "VRI", "VRO", 
    "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", 
    "EffortDot", "EventNum", "file_das", "line_num", "file_type", 
    "dist_from_prev", "cont_eff_section", "effort_seg", "seg_idx", "segnum"
  )
  if (!identical(names(x.eff), x.eff.names))
    stop("Error in airdas_effort: names of x.eff. ", 
         "Please report this as an issue")
  
  if (!all(x.eff$segnum %in% segdata$segnum))
    stop("Error in airdas_effort(): Error creating and processing ", 
         "segement numbers. Please report this as an issue")
  
  
  #----------------------------------------------------------------------------
  # Summarize sightings (based on siteinfo) and add applicable data to segdata
  siteinfo <- x.eff %>% 
    left_join(select(segdata, .data$segnum, .data$mlat, .data$mlon), 
              by = "segnum") %>% 
    airdas_sight() %>% 
    mutate(included = (.data$Bft <= 5 & abs(.data$Angle) <= 78 & 
                         .data$SightStd & .data$OnEffort), 
           included = ifelse(is.na(.data$included), FALSE, .data$included)) %>% 
    select(-.data$dist_from_prev, -.data$cont_eff_section, -.data$effort_seg)
  
  
  # Make data frame with nSI and ANI columns, and join it with segdata
  # TODO: Throw warning if element(s) of sp.codes are not in data?
  sp.codes <- sort(sp.codes)
  
  segdata.col1 <- select(segdata, .data$seg_idx)
  siteinfo.forsegdata.list <- lapply(sp.codes, function(i, siteinfo, d1) {
    d0 <- siteinfo %>% 
      filter(.data$included, .data$Sp == i) %>% 
      group_by(.data$seg_idx) %>% 
      summarise(nSI = length(.data$Sp), 
                ANI = sum(.data$GsSp))
    
    names(d0) <- c("seg_idx", paste0(i, "_", names(d0)[-1]))
    
    z <- full_join(d1, d0, by = "seg_idx") %>% select(-.data$seg_idx)
    z[is.na(z)] <- 0
    
    z
  }, siteinfo = siteinfo, d1 = segdata.col1)
  
  siteinfo.forsegdata.df <- segdata.col1 %>% 
    bind_cols(siteinfo.forsegdata.list)
  
  segdata <- segdata %>% 
    left_join(siteinfo.forsegdata.df, by = "seg_idx")
  
  
  #----------------------------------------------------------------------------
  # Return list
  list(segdata = segdata, siteinfo = siteinfo, randpicks = randpicks)
} 
