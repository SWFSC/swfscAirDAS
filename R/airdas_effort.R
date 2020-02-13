#' Summarize AirDAS effort
#'
#' Chop AirDAS data into effort segments
#' 
#' @param x \code{airdas_df} object; output from \code{\link{airdas_process}}, 
#'  or a data frame that can be coerced to a \code{airdas_df} object
#' @param method character; method to use to chop AirDAS data into effort segments.
#'   Currently the only option is "equallength".
#' @param sp.codes character; species code(s) to include in segdata
#' @param ... arguments passed to the chopping function specified using \code{method}
#' 
#' @importFrom dplyr %>% between bind_cols filter full_join group_by summarise
#' @importFrom swfscMisc distance
#' @importFrom utils head
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
#'   Currently, the only available chopping method is "equallength": 
#'   chopping effort sections into equal-length segments of length \code{seg.km}, 
#'   and doing a weighted average of the conditions for the length of that segment. 
#'   See \code{\link{airdas_chop_equal}} for more details about this method, 
#'   including arguments that must be passed to it via \code{...}.
#'   
#'   In progress: chop by conditions
#'   
#'   The sightings included in the segdata counts are filtered as follows: 
#'   Beaufort is less than or equal to five, the declination angle is less than 78, 
#'   it is a standard sighting, and the sighting was made while on effort. 
#'   Siteinfo, which contains all sightings in \code{x} (see below), 
#'   contains a column ('included') that indicates whether or not the sighting was
#'   included in the segdata counts.
#' 
#'   The function \code{\link[swfscMisc]{distance}}, \code{method = "vincenty"}, is used to
#'   calculate the distance (in km) between the lat/lon points of subsequent events.
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
#' 
#' y.proc <- airdas_process(y)
#' airdas_effort(
#'   y.proc, method = "equallength", sp.codes = c("mn", "bm"), 
#'   seg.km = 3
#' )
#' 
#' y.rand <- system.file("airdas_sample_randpicks.csv", package = "swfscAirDAS")
#' airdas_effort(
#'   y.proc, method = "equallength", sp.codes = c("mn", "bm"), 
#'   seg.km = 3, randpicks.load = y.rand
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
airdas_effort.airdas_df <- function(x, method, sp.codes, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  methods.acc <- c("equallength", "condition")
  if (!(length(method) == 1 & (method %in% methods.acc))) 
    stop("method must be a string, and must be one of: ", 
         paste0("\"", paste(methods.acc, collapse = "\", \""), "\""))
  
  
  #----------------------------------------------------------------------------
  # Prep
  # Filter for and number continuous effort sections
  #   'on effort + 1' is to capture O/E event
  x.oneff.which <- sort(unique(c(which(x$OnEffort), which(x$OnEffort) + 1)))
  stopifnot(all(between(x.oneff.which, 1, nrow(x))))
  
  x.oneff <- x[x.oneff.which, ]
  rownames(x.oneff) <- NULL
  
  # For each event, calculate distance to previous event
  dist.from.prev <- mapply(function(x1, y1, x2, y2) {
    distance(y1, x1, y2, x2, units = "km", method = "vincenty")
  },
  x1 = head(x.oneff$Lon, -1), y1 = head(x.oneff$Lat, -1),
  x2 = x.oneff$Lon[-1], y2 = x.oneff$Lat[-1], 
  SIMPLIFY = TRUE)
  
  x.oneff$dist_from_prev <- c(NA, dist.from.prev)
  
  
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
    "CCover", "Jelly", "HorizSun", "HKR", "Haze", "Kelp", "RedTide", 
    "ObsL", "ObsB", "ObsR", "Rec", "AltFt", "SpKnot", 
    "VLI", "VLO", "VB", "VRI", "VRO", 
    "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", 
    "EffortDot", "EventNum", "file_das", "line_num", 
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
