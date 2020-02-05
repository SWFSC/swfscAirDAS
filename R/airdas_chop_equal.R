#' Chop AirDAS effort segment length
#' 
#' Determine lengths of segments into which to chop aerial DAS effort data
#' 
#' @param x data frame; processed aerial DAS data from \code{\link{airdas_effort}}.
#'   This data must contain exactly one continuous effort section, 
#'   i.e. it must begin with a "T"/"R" event and end with a "E"/"O" event 
#'   (with no "T"/"R"/"E"/"O" events in between)
#' @param seg.km numeric; length of effort segments. Passed from \code{\link{airdas_effort}}
#' @param randpicks numeric; segment positions that get leftover segment bits as described in Details
#' 
#' @importFrom dplyr between
#' @importFrom stats runif
#' @importFrom swfscMisc distance
#' @importFrom utils head tail
#' 
#' @details This function chops continuous effort sections (henceforth 'effort sections') 
#'   from the processed AirDAS data into modeling segments (henceforth 'segments') of equal length. 
#'   Each effort sections runs from a T/R event to its corresponding E/O event. 
#' 
#'   If the extra length remaining after chopping is greater than or equal to half of the 
#'   target segment length (i.e. \code{>= 0.5*seg.km}), 
#'   it is randomly assigned to a portion of the effort section and 
#'   the associated distance provided as an offset value (Fig. 1a). 
#'   If the extra length is less than half of the target segment length (i.e. \code{< 0.5*seg.km}), 
#'   it is randomly added to one of the equal-length segments and the associated distance provided 
#'   as an offset value (Fig. 1b). 
#'   Therefore, the length of each segment is constrained to be between one half and one and one half of 
#'   \code{seg.km} (i.e. \code{0.5*seg.km to 1.5*seg.km}), 
#'   and the central tendency is approximately equal to the target segment length. 
#'   The only exception is when a continuous effort section is less than one half of the 
#'   target segment length (i.e. \code{< 0.5*seg.km}), 
#'   in which case the entire continuous effort section is a single segment (Fig. 1c).
#'   
#'   If the length of the effort section is zero and there are more than two events 
#'   (i.e., events between the "T"/R" and "E"/"O" events),
#'   the function throws a warning (but still returns a length of zero for that segment).
#'   
#'   If the column \code{dist_from_prev} does not exist 
#'   (it should be calculated in  \code{\link{airdas_effort}}), 
#'   then the distance between the lat/lon points of subsequent events 
#'   is calculated using \code{\link[swfscMisc]{distance}}, \code{method = "vincenty"}.
#'   
#'   randpicks description
#'   
#'   If segment length is 0, nothing..? 
#'   
#'   If segment length is less than target length, only make one segment
#'   
#'   If segment length is greater than the target length and remainder is greater than or equal to half of the target length, 
#'   the remainder is its own (randomly placed) segment
#'   
#'   If segment length is greater than the target length and remainder is less than half of the target length, 
#'   the remainder added to a random segment
#'   
#' @return List of:
#' \itemize{
#'   \item The AirDAS data, with a \code{effort_seg} column added..
#'   \item
#'   \item The index of 
#' }
#' 
#' @keywords internal
#' 
#' @export
airdas_chop_equal <- function(x, seg.km, randpicks = NULL) {
  #----------------------------------------------------------------------------
  das.df <- x #TODO: quick fix for function argument
  
  # Input checks
  # TODO: what to do if multiple continuous effort sections are in das.df?
  stopifnot(
    all.equal(unique(cumsum(das.df$Event %in% c("R", "T"))), 1), 
    all(das.df$OnEffort | das.df$Event %in% c("O", "E"))
  )
  
  if (!("dist_from_prev" %in% names(das.df))) {
    stopifnot(
      sum(is.na(das.df$Lat)) == 0,
      sum(is.na(das.df$Lon)) == 0, 
      sum(das.df$Event == "#") == 0
    )
    
    dist.from.prev <- mapply(function(x1, y1, x2, y2) {
      distance(y1, x1, y2, x2, units = "km", method = "vincenty")
    },
    x1 = head(das.df$Lon, -1), y1 = head(das.df$Lat, -1),
    x2 = tail(das.df$Lon, -1), y2 = tail(das.df$Lat, -1), 
    SIMPLIFY = TRUE)
    
    das.df$dist_from_prev <- c(NA, dist.from.prev)
  }
  
  #----------------------------------------------------------------------------
  # Prep
  if (!is.null(randpicks)) pos <- randpicks
  
  das.df$dist_from_prev[1] <- 0 #Ignore distance from last effort
  
  seg.dist <- sum(das.df$dist_from_prev)
  seg.dist.mod <- seg.dist %% seg.km
  
  #----------------------------------------------------------------------------
  ### Get lengths of effort segments
  if (seg.dist == 0) {
    # If current segment length is 0 and there are other events, throw warning
    if (nrow(das.df) > 2) 
      warning("A segment distance was zero, ", 
              "and there were events between start and end points")
    
    # EAB makes a 0.1km segment if it includes a sighting
    # if (any(curr.df$Event == "S")) print("Effort of length 0") #browser()
    subseg.lengths <- 0
    pos <- NA
    
  } else {
    if (seg.dist <= seg.km) {
      # If current segment length is less than target length,
      #   only make one segment
      n.subseg <- 1
      if (is.null(randpicks)) pos <- NA
      subseg.lengths <- seg.dist
      
    } else if (seg.dist.mod >= (seg.km / 2)) {
      # If current segment length is greater than the target length and
      #   remainder is greater than or equal to half of the target length,
      #   the remainder is its own (randomly placed) segment
      n.subseg <- ceiling(seg.dist/seg.km)
      if (is.null(randpicks)) pos <- ceiling(runif(1, 0, 1) * n.subseg)
      if (is.na(pos) | !between(pos, 1, n.subseg)) 
        stop("Randpicks value is not in proper range")
      subseg.lengths <- rep(seg.km, n.subseg)
      subseg.lengths[pos] <- seg.dist.mod
      
    } else if (seg.dist.mod < (seg.km / 2)) {
      # If current segment length is greater than the target length and
      #   remainder is less than half of the target length,
      #   the remainder added to a random segment
      n.subseg <- floor(seg.dist/seg.km)
      if (is.null(randpicks)) pos <- ceiling(runif(1, 0, 1) * n.subseg)
      if (is.na(pos) | !between(pos, 1, n.subseg)) 
        stop("Randpicks value is not in proper range")
      subseg.lengths <- rep(seg.km, n.subseg)
      subseg.lengths[pos] <- seg.km + seg.dist.mod
      
    } else {
      stop("Error while chopping effort - unrecognized effort situation")
    }
  }
  
  
  #----------------------------------------------------------------------------
  ### Assign each point to a segment, and return list
  subseg.cumsum <- cumsum(subseg.lengths)
  das.cumsum <- cumsum(das.df$dist_from_prev)
  # das.df$dist_from_prev_cumsum <- cumsum(das.df$dist_from_prev)
  
  das.df$effort_seg <- findInterval(
    round(das.cumsum, 4), round(c(-1, subseg.cumsum), 4),
    left.open = TRUE, rightmost.closed = TRUE
  )
  
  list(das.df, subseg.lengths, pos)
}
