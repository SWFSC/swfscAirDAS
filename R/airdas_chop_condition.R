#' Chop AirDAS data - condition
#' 
#' Chop AirDAS data into a new effort segment every time a condition changes
#' 
#' @param x \code{airdas_df} object, 
#'   or a data frame that can be coerced to a \code{airdas_df} object. 
#'   This data must be filtered for 'OnEffort' events; 
#'   see the Details section below
#' @param ... ignored
#' @param seg.km.min numeric; minimum allowable segment length (in kilometers).
#'   Default is 0.1. See the Details section below for more information
#' @param dist.method character; see \code{\link{airdas_effort}}.
#'   Default is \code{NULL} since these distances should have already been
#'   calculated in \code{\link{airdas_effort}}
#' @param num.cores Number of CPUs to over which to distribute computations.
#'   Defaults to \code{NULL} which uses one fewer than the number of cores
#'   reported by \code{\link[parallel]{detectCores}}
#'   
#' @details This function is intended to only be called by \code{\link{airdas_effort}} 
#'   when the "condition" method is specified. 
#'   Thus, \code{x} must be filtered for events (rows) where either
#'   the 'OnEffort' column is \code{TRUE} or the 'Event' column is either "E" or "O"; 
#'   see \code{\link{airdas_effort}} for more details. 
#'   This function chops each continuous effort section (henceforth 'effort sections') 
#'   in \code{x} into modeling segments (henceforth 'segments') by 
#'   creating a new segment every time a condition changes. 
#'   Each effort section runs from a T/R event to its corresponding E/O event. 
#'   After chopping, \code{\link{airdas_segdata_avg}} is called to get relevant  
#'   segdata information for each segment.
#'   
#'   Changes in the following conditions trigger a new segment:
#'   Beaufort, percent overcast (cloud cover), jellyfish code, horizontal sun
#'   altitude, speed, haze, kelp, red tide, and observer viewing conditions.
#'   The main exception is when multiple condition changes happen at 
#'   the same location, such as a 'TVPAW' series of events. 
#'   When this happens, no segments of length zero are created; 
#'   rather, a single segment is created that includes all of the condition changes 
#'   (i.e. all of the events in the event series) that happened during 
#'   the series of events (i.e. at the same location). 
#'   
#'   In addition, (almost) all segments whose length is less than \code{seg.km.min}
#'   are combined with the segment immediately following them to ensure that the length
#'   of (almost) all segments is at least \code{seg.km.min}. 
#'   This allows users to account for situations where multiple conditions, 
#'   such as Beaufort and the viewing conditions, change in rapid succession, say 0.05 km apart.
#'   When segments are combined, a warning is thrown and the conditions are averaged together 
#'   across the now-larger segment (question for Karin - is this right?). 
#'   The only exception to this rule is if the short segment ends in an "E" or O" event, 
#'   meaning it is the last segment of the effort section. 
#'   Since in this case there is no 'next' segment, this segment is left as-is.
#'   
#'   Note that the above rule for 'combining' condition changes that have the same location 
#'   into a single segment (such as a 'TVPAW' series of events) 
#'   is followed even if \code{seg.km.min = 0}.
#'
#'   If the column \code{dist_from_prev} does not exist, the distance between
#'   subsequent events is calculated as described in \code{\link{airdas_effort}}
#'   
#'   Outstanding question: should das_segdata_avg be used, i.e. do we need a different function 
#'   that doesn't average conditions?
#'   
#' @return List of two data frames:
#' \itemize{
#'   \item \code{x}, with columns added for the corresponding unique segment code and number
#'   \item segdata: data frame with one row for each segment, and columns with
#'     relevant data (see \code{\link{airdas_effort}} for specifics)
#' }
#' 
#' @keywords internal
#' 
#' @seealso airdas_chop_equal, airdas_chop_section
#' 
#' @export
airdas_chop_condition <- function(x, ...) UseMethod("airdas_chop_condition")


#' @name airdas_chop_condition
#' @export
airdas_chop_condition.data.frame <- function(x, ...) {
  airdas_chop_condition(as_airdas_df(x), ...)
}


#' @name airdas_chop_condition
#' @export
airdas_chop_condition.airdas_df <- function(x, seg.km.min = 0.1, 
                                            dist.method = NULL, 
                                            num.cores = NULL, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  if (!all(x$OnEffort | x$Event %in% c("O", "E"))) 
    stop("x must be filtered for on effort events; see `?airdas_chop_condition")
  
  if (!inherits(seg.km.min, c("integer", "numeric")))
    stop("When using the \"condition\" method, seg.km.min must be a numeric. ",
         "See `?airdas_chop_condition` for more details")
  
  if (!.greater_equal(seg.km.min, 0))
    stop("seg.km.min must be greater than or equal to 0; ", 
         "see `?airdas_chop_condition")
  
  
  #----------------------------------------------------------------------------
  # Calculate distance between points if necessary
  if (!("dist_from_prev" %in% names(x))) {
    if (is.null(dist.method))
      stop("If the distance between consectutive points (events) ",
           "has not already been calculated, ",
           "then you must provide a valid argument for dist.method")
    
    x$dist_from_prev <- .dist_from_prev(x, dist.method)
  }
  
  # Get distance to next point
  x$dist_to_next <- c(x$dist_from_prev[-1], NA)
  
  
  #----------------------------------------------------------------------------
  # ID continuous effort sections, then for each modeling segment: 
  #   1) chop by condition change
  #   2) aggregate 0-length segments (e.g. tvpaw),
  #   3) aggregate small segments as specified by user
  x$cont_eff_section <- cumsum(x$Event %in% c("T", "R"))
  eff.uniq <- unique(x$cont_eff_section)
  
  cond.names <- c(
    "Bft", "CCover", "Jelly", "HorizSun", "Haze", "Kelp", "RedTide", 
    "AltFt", "SpKnot", "VLI", "VLO", "VB", "VRI", "VRO"
  )
  
  # Prep for parallel
  call.x <- x
  call.cond.names <- cond.names
  call.seg.km.min <- seg.km.min
  call.func1 <- airdas_segdata_avg
  call.func2 <- as_airdas_df
  
  # Setup number of cores
  if(is.null(num.cores)) num.cores <- parallel::detectCores() - 1
  if(is.na(num.cores)) num.cores <- 1
  num.cores <- max(1, num.cores)
  num.cores <- min(parallel::detectCores() - 1, num.cores)
  
  # Use parallel to lapply through - modeled after rfPermute
  cl <- swfscMisc::setupClusters(num.cores)
  eff.list <- tryCatch({
    if(is.null(cl)) { # Don't parallelize if num.cores == 1
      lapply(
        eff.uniq, swfscDAS::.chop_condition_eff, call.x = call.x,
        call.cond.names = call.cond.names, call.seg.km.min = call.seg.km.min,
        call.func1 = call.func1, call.func2 = call.func2
      )
      
    } else { # Run lapply using parLapplyLB
      parallel::clusterExport(
        cl = cl,
        varlist = c("call.x", "call.cond.names", "call.seg.km.min", 
                    "call.func1", "call.func2"),
        envir = environment()
      )
      parallel::parLapplyLB(
        cl, eff.uniq, swfscDAS::.chop_condition_eff, call.x = call.x,
        call.cond.names = call.cond.names, call.seg.km.min = call.seg.km.min,
        call.func1 = call.func1, call.func2 = call.func2
      )
    }
  }, finally = if(!is.null(cl)) parallel::stopCluster(cl) else NULL)
  
  
  #----------------------------------------------------------------------------
  # Extract information from eff.list, and return
  
  ### Segdata
  segdata <- data.frame(
    do.call(rbind, lapply(eff.list, function(i) i[["das.df.segdata"]])), 
    stringsAsFactors = FALSE
  ) %>%
    mutate(segnum = seq_along(.data$seg_idx), 
           dist = round(.data$dist, 4)) %>%
    select(.data$segnum, .data$seg_idx, everything())
  
  ###
  x.len <- lapply(eff.list, function(i) i[["seg.lengths"]])
  
  ### Each das data point, along with segnum
  x.eff <- data.frame(
    do.call(rbind, lapply(eff.list, function(i) i[["das.df"]])), 
    stringsAsFactors = FALSE
  ) %>% 
    left_join(segdata[, c("seg_idx", "segnum")], by = "seg_idx") %>% 
    select(-.data$dist_to_next)
  
  
  #----------------------------------------------------------------------------
  # Return
  list(as_airdas_df(x.eff), segdata)
}
