#' Chop AirDAS effort segment length
#' 
#' Determine lengths of segments into which to chop aerial DAS effort data
#' 
#' @param das.df data frame; processed aerial DAS data output from \link{airdas_process} (or subsequent airdas function)
#' @param seg.km numeric; length of effort segments 
#' @param randpicks numeric; if not NULL, segment positions that get leftover segment bits todo
#' 
#' @importFrom dplyr between
#' @importFrom stats runif
#' @importFrom swfscMisc distance
#' @importFrom utils head tail
#' 
#' @details Chop continuous effort section into effort segments (modeling segments)
#'   
#'   Calculates dist_from_prev using swfscMisc::distance, if necessary
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
#' @return List of...
#' 
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.proc <- airdas_process(y)
#' 
#' airdas_effort_chop(y.proc[1:33, ], 1)
#' 
#' @export
airdas_effort_chop <- function(das.df, seg.km, randpicks = NULL) {
  #----------------------------------------------------------------------------
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
    # If current segment length is 0, nothing..?
    stop("Segment distance was zero")
    # EAB makes a 0.1km segment if it includes a sighting
    # if (any(curr.df$Event == "S")) print("Effort of length 0") #browser()
    NA
    
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
