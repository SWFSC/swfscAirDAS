#' Summarize AirDAS effort
#'
#' Chop AirDAS data into effort segments
#' 
#' @param x \code{airdas_df} object; output from \code{\link{airdas_process}}, 
#'  or a data frame that can be coerced to a \code{airdas_df} object
#' @param method character; method to use to chop AirDAS data into effort segments
#'   Can be \code{"condition"}, \code{"equallength"}, 
#'   or \code{"section"} (case-sensitive) to use 
#'   \code{\link{airdas_chop_condition}}, \code{\link{airdas_chop_equal}}, 
#'   or \code{\link{airdas_chop_section}}, respectively
#' @param sp.codes character; species code(s) to include in segdata. 
#'   These code(s) will be converted to lower case to match \code{\link{airdas_sight}} 
#' @param conditions character vector of names of conditions to include in segdata output.
#'   These values must be column names from the output of \code{\link{airdas_process}},
#'   e.g. 'Bft', 'CCover', etc.
#'   If \code{method == "condition"}, then these also are the conditions which
#'   trigger segment chopping when they change.
#' @param dist.method character;
#'   method to use to calculate distance between lat/lon coordinates.
#'   Can be "greatcircle" to use the great circle distance method (TODO - add ref),
#'   or one of "lawofcosines", "haversine", or "vincenty" to use
#'   \code{\link[swfscMisc]{distance}}. Default is "greatcircle"
#' @param num.cores Number of CPUs to over which to distribute computations.
#'   Defaults to \code{NULL}, which uses one fewer than the number of cores
#'   reported by \code{\link[parallel]{detectCores}}
#'   Using 1 core likely will be faster for smaller datasets
#' @param ... arguments passed to the chopping function specified using \code{method},
#'   such as \code{seg.km} or \code{seg.min.km}
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
#'   All on effort events must not have \code{NA} Lat or Lon values; 
#'   note Lat/Lon values for 1 events were 'filled in' in \code{\link{airdas_process}}.
#' 
#'   The following chopping methods are currently available: 
#'   \code{"condition"}, \code{"equallength"}, and \code{"section"}. 
#'   When using the \code{"condition"} method, effort sections are chopped 
#'   into segments every time a condition changes, 
#'   thereby ensuring that the conditions are consistent across the entire segment.
#'   See \code{\link{airdas_chop_condition}} for more details about this method, 
#'   including arguments that must be passed to it via \code{...}.
#'   
#'   The \code{"equallength"} method consists of
#'   chopping effort sections into equal-length segments of length \code{seg.km}, 
#'   and doing a weighted average of the conditions for the length of that segment. 
#'   See \code{\link{airdas_chop_equal}} for more details about this method, 
#'   including arguments that must be passed to it via \code{...}.
#'   
#'   The \code{"section"} method involves 'chopping' the effort into continuous effort sections,
#'   i.e. each continuous effort section is a single effort segment.
#'   See \code{\link{airdas_chop_section}} for more details about this method.
#'   
#'   The sightings included in the segdata counts are filtered as follows: 
#'   Beaufort is less than or equal to five, the declination angle is less than 78, 
#'   it is a standard sighting, and the sighting was made while on effort. 
#'   Included sightings are those with a \code{TRUE} value in the 'included'
#'   column in siteinfo (described below).
#'   TODO: Allow user to specify this.
#' 
#'   The distance between the lat/lon points of subsequent events
#'   is calculated using the method specified in \code{dist.method}
#'   See \code{\link{airdas_sight}} for how the sightings are processed.
#' 
#' @return List of three data frames: 
#'   \itemize{
#'     \item segdata: one row for every segment, and columns for information including
#'       unique segment number, event code that started the associated continuous effort section, 
#'       start/end/midpoint coordinates, conditions (e.g. Beaufort), 
#'       and number of sightings and number of animals on that segment for every species 
#'       indicated in \code{sp.codes}. 
#'     \item siteinfo: details for all sightings in \code{x}, including: 
#'       the unique segment number it is associated with, segment mid points (lat/lon), 
#'       and whether the sighting was included in the segdata counts (column \code{included}), 
#'       in addition to the other output information described in \code{\link{airdas_sight}}.
#'     \item randpicks: see \code{\link{airdas_chop_equal}}. 
#'       \code{NULL} if using "condition" method.
#'   }
#' 
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.proc <- airdas_process(y)
#' 
#' # Using "condition" method
#' airdas_effort(
#'   y.proc, method = "condition", sp.codes = c("mn", "bm"), 
#'   conditions = "Bft", seg.min.km = 0.05, num.cores = 1
#' )
#' 
#' # Using "equallength" method
#' y.rand <- system.file("airdas_sample_randpicks.csv", package = "swfscAirDAS")
#' airdas_effort(
#'   y.proc, method = "equallength", sp.codes = c("mn", "bm"), 
#'   conditions = c("Bft", "CCover"), 
#'   seg.km = 3, randpicks.load = y.rand, num.cores = 1
#' )
#' 
#' # Using "section" method
#' airdas_effort(
#'   y.proc, method = "section", sp.codes = c("mn", "bm"), 
#'   num.cores = 1
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
airdas_effort.airdas_df <- function(x, method, sp.codes, conditions = NULL, 
                                    dist.method = "greatcircle", 
                                    num.cores = NULL, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  
  # Method
  methods.acc <- c("condition", "equallength", "section")
  if (!(length(method) == 1 & (method %in% methods.acc))) 
    stop("method must be a string, and must be one of: ", 
         paste0("\"", paste(methods.acc, collapse = "\", \""), "\""))
  
  #Check for dist.method happens in .dist_from_prev()
  
  # Conditions
  conditions.acc <- c(
    "Bft", "CCover", "Jelly", "HorizSun", "VertSun", 
    "Haze", "Kelp", "RedTide", "AltFt", "SpKnot", 
    "ObsL", "ObsB", "ObsR", "Rec", "VLI", "VLO", "VB", "VRI", "VRO"
  )
  
  if (is.null(conditions)) {
    conditions <- c(
      "Bft", "CCover", "Jelly", "HorizSun", "VertSun", 
      "Haze", "Kelp", "RedTide", "AltFt", "SpKnot"
    )
    
  } else {
    if (!all(conditions %in% conditions.acc))
      stop("Please ensure all components of the conditions argument are ",
           "one of the following accepted values:\n",
           paste(conditions.acc, collapse  = ", "))
  }
  
  
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
  
  x.oneff$dist_from_prev <- .dist_from_prev(x.oneff, dist.method)
  
  
  #----------------------------------------------------------------------------
  # Chop and summarize effort using specified method
  eff.list <- if (method == "condition") {
    airdas_chop_condition(as_airdas_df(x.oneff), conditions = conditions, 
                          num.cores = num.cores, ...)
  } else if (method == "equallength") {
    airdas_chop_equal(as_airdas_df(x.oneff), conditions = conditions, 
                      num.cores = num.cores, ...)
  } else if (method == "section") {
    airdas_chop_section(as_airdas_df(x.oneff), conditions = conditions, 
                        num.cores = num.cores, ...)
  } else {
    stop("method is not an accepted value")
  }
  
  x.eff <- eff.list[[1]]
  segdata <- eff.list[[2]]
  randpicks <- eff.list[[3]]
  
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
