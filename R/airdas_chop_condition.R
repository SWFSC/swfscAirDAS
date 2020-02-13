#' Chop AirDAS data - condition
#' 
#' Chop AirDAS data into a new effort segment every time any condition changes
#' 
#' @param x \code{airdas_df} object, 
#'   or a data frame that can be coerced to a \code{airdas_df} object. 
#'   This data must be filtered for 'OnEffort' events; 
#'   see the Details section below
#' @param ... ignored
#' @param seg.km.min numeric; default is 0.1. 
#'   All segments shorter than this distance, which is in kilometers, 
#'   will be smushed with the segment immediately following them. 
#'   If \code{seg.km.min} is \code{0}, then only event series (e.g. TVPAW) will be smushed
#'   
#' @importFrom dplyr %>% filter left_join mutate select
#' @importFrom swfscMisc distance
#' @importFrom utils head
#' 
#' @details This function is intended to be called by \code{\link{airdas_effort}} 
#'   when the "condition" method is specified. 
#'   Thus, \code{x} must be filtered for events (rows) where either
#'   the 'OnEffort' column is \code{TRUE} or the 'Event' column is "E" or "O"; 
#'   see \code{\link{airdas_effort}} for more details. 
#'   Continuous effort sections (henceforth 'effort sections') from \code{x}
#'   are chopped into modeling segments (henceforth 'segments') every time a condition changes 
#'   Each effort section runs from a T/R event to its corresponding E/O event. 
#'   Then \code{\link{airdas_segdata_avg}} is called to get relevant segdata information.
#'   
#'   The following conditions trigger the creation of a new segment:
#'   viewing conditions, altitude, speed, HKR, percent overcast (cloud cover), 
#'   Beaufort, Jellyfish code, and horizontal sun
#'   
#'   All segments with length zero will be smushed with the segment immediately following them.
#'   This will take care of event series that all have the same location, such as TVPAW.
#'   Exceptions to \code{seg.km.min} are segments at the end of a continuous effort section
#'   
#'   TODO
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
#' @export
airdas_chop_condition <- function(x, ...) UseMethod("airdas_chop_condition")


#' @name airdas_chop_condition
#' @export
airdas_chop_condition.data.frame <- function(x, ...) {
  airdas_chop_condition(as_airdas_df(x), ...)
}


#' @name airdas_chop_condition
#' @export
airdas_chop_condition.airdas_df <- function(x, seg.km.min = 0.1, ...){
  #----------------------------------------------------------------------------
  # Input checks
  if (!all(x$OnEffort | x$Event %in% c("O", "E"))) 
    stop("x must be filtered for on effort events; see `?airdas_chop_equal")
  
  if (!(seg.km.min >= 0))
    stop("seg.km.min must be greater than or equal to 0; ", 
         "see `?airdas_chop_equal")
  
  
  #----------------------------------------------------------------------------
  # Calculate distance between points if necessary
  if (!("dist_from_prev" %in% names(x))) {
    dist.from.prev <- mapply(function(x1, y1, x2, y2) {
      distance(y1, x1, y2, x2, units = "km", method = "vincenty")
    },
    x1 = head(x$Lon, -1), y1 = head(x$Lat, -1),
    x2 = x$Lon[-1], y2 = x$Lat[-1], 
    SIMPLIFY = TRUE)
    
    x$dist_from_prev <- c(NA, dist.from.prev)
  }
  
  # Get distance to next point
  x$dist_to_next <- c(x$dist_from_prev[-1], 0)
  
  
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
  
  eff.list <- lapply(eff.uniq, function(i, x) {
    #------------------------------------------------------
    # Prep
    das.df <- filter(x, .data$cont_eff_section == i)
    
    # Ignore distance from last effort
    das.df$dist_from_prev[1] <- 0
    # Ignore distance past this continuous effort section
    das.df$dist_to_next[nrow(das.df)] <- 0
    
    
    #------------------------------------------------------
    ### Determine indices of condition changes, smushing as needed
    cond.list <- lapply(cond.names, function(j) {
      which(c(NA, head(das.df[[j]], -1) != das.df[[j]][-1]))
    })
    cond.idx.pre <- sort(unique(c(1, unlist(cond.list))))
    
    effort.seg.pre <- rep(FALSE, nrow(das.df))
    effort.seg.pre[cond.idx.pre] <- TRUE
    
    das.df$effort_seg_pre <- cumsum(effort.seg.pre)
    das.df$idx <- seq_len(nrow(das.df))
    
    # Get distances of current effort sections
    d.pre <- das.df %>% 
      group_by(.data$effort_seg_pre) %>% 
      summarise(idx_start = min(.data$idx), 
                idx_end = max(.data$idx), 
                dist_length = sum(.data$dist_to_next))
    
    # == 0 check is here in case seg.km.min is 0
    seg.len0 <- d.pre$idx_end[d.pre$dist_length == 0] + 1
    seg.len1 <- d.pre$idx_end[d.pre$dist_length < seg.km.min] + 1
    seg.diff <- setdiff(seg.len1, seg.len0)
    if (length(seg.diff) > 0 & all(seg.diff <= nrow(das.df)))
      warning("Because of smushing, not all conditions are the same ", 
              "for each segment in continuous effort section ", i, 
              immediate. = TRUE)
    # TODO: throw warning if any seg.len1? aka conditions are not consistent
    
    
    idx.torm <- sort(unique(c(seg.len0, seg.len1)))
    
    # Remove segment breaks that create too-small segments
    #   Ignores idx.torm values > nrow(das.df)
    cond.idx <- cond.idx.pre[!(cond.idx.pre %in% idx.torm)]
    effort.seg <- rep(FALSE, nrow(das.df))
    effort.seg[cond.idx] <- TRUE
    
    das.df <- das.df %>% 
      select(-.data$effort_seg_pre, -.data$idx) %>% 
      mutate(effort_seg = cumsum(effort.seg), 
             seg_idx = paste(i, .data$effort_seg, sep = "_"))
    
    
    #------------------------------------------------------
    ### Calculate lengths of effort segments
    d <- das.df %>% 
      group_by(.data$effort_seg) %>% 
      summarise(sum_dist = sum(.data$dist_to_next))
    
    seg.lengths <- d$sum_dist
    
    #------------------------------------------------------
    ### Get segdata and return
    das.df.segdata <- airdas_segdata_avg(
      as_airdas_df(das.df), seg.lengths, i
    )
    # TODO: rename avg?
    if (!all(das.df$seg_idx %in% das.df.segdata$seg_idx)) browser()
    
    list(
      das.df = das.df, seg.lengths = seg.lengths, 
      das.df.segdata = das.df.segdata
    )
  }, x = x)
  
  
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
