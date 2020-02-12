#' Chop AirDAS data - condition
#' 
#' Chop AirDAS data into a new effort segment every time any condition changes
#' 
#' @param x \code{airdas_df} object, 
#'   or a data frame that can be coerced to a \code{airdas_df} object. 
#'   This data must be filtered for 'OnEffort' events; 
#'   see the Details section below
#' @param ... ignored
#' @param seg.km.min numeric; all segments shorter than this distance, 
#'   which is in kilometers,  will be smushed with the segment 
#'   immediately following them. 
#'   Default is 0.1
#'   
#' @importFrom dplyr %>% filter left_join mutate select
#' @importFrom stringr str_locate_all
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
#'   
#'   TODO
#'   
#' @return List?
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
  # ID continuous effort sections and segments
  x$cont_eff_section <- cumsum(x$Event %in% c("T", "R"))
  
  # # Get the idx of all 
  # x.event.chr <- paste(x$Event, collapse = "")
  # idx.vpaw <- unlist(
  #   lapply(str_locate_all(x.event.chr, "TVPAW")[[1]][, "start"], function(i, j) {
  #     (i+1):(i+j)
  #   }, j = 4))
  # if (any(duplicated(idx.tvpaw))) browser()
  
  cond.names <- c(
    "Bft", "CCover", "Jelly", "HorizSun", "Haze", "Kelp", "RedTide", 
    "AltFt", "SpKnot", "VLI", "VLO", "VB", "VRI", "VRO", 
    "cont_eff_section"
  )
  
  cond.list <- lapply(cond.names, function(i) {
    which(c(NA, head(x[[i]], -1) != x[[i]][-1]))
  })
  cond.idx.pre <- sort(unique(c(1, unlist(cond.list))))
  
  effort_seg_pre <- rep(FALSE, nrow(x))
  effort_seg_pre[cond.idx.pre] <- TRUE
  
  x$effort_seg_pre <- cumsum(effort_seg_pre)
  x$idx <- seq_len(nrow(x))
  
  
  x.summ <- x %>% 
    group_by(.data$effort_seg_pre) %>% 
    summarise(cont_eff_section = unique(.data$cont_eff_section), 
              idx_start = min(.data$idx), 
              idx_end = max(.data$idx), 
              dist_length = sum(dist_to_next))
  
  # == 0 check is here in case seg.km.min is 0
  seg.len0 <- x.summ$idx_end[x.summ$dist_length == 0] + 1
  seg.len1 <- x.summ$idx_end[x.summ$dist_length < seg.km.min] + 1
  idx.torm <- unique(c(seg.len0, seg.len1))
  
  if (any(idx.torm > nrow(x))) browser()
  
  ### Remove 'small' seg lengths
  cond.idx <- cond.idx.pre[!(cond.idx.pre %in% idx.torm)]
  effort_seg <- rep(FALSE, nrow(x))
  effort_seg[cond.idx] <- TRUE
  
  x$effort_seg <- cumsum(effort_seg)
  x$effort_seg_pre <- NULL
  
  # Sanity check
  x.summ2 <- x %>% 
    group_by(.data$effort_seg) %>% 
    summarise(cont_eff_section = unique(.data$cont_eff_section), 
              idx_start = min(.data$idx), 
              idx_end = max(.data$idx), 
              dist_length = sum(dist_to_next))
  if (any(x.summ2$dist_length < seg.km.min)) browser()
  
  #----------------------------------------------------------------------------
  # For each modeling segment: chop by condition change, and aggregate 
  #  small segments as necessary (e.g. tvpaw) or as specified by user
  eff.uniq <- unique(x$cont_eff_section)
  eff.list <- lapply(eff.uniq, function(i, x) {
    #------------------------------------------------------
    ### Get lengths of effort segments
    # Prep
    das.df <- x %>% 
      filter(.data$cont_eff_section == i) %>% 
      mutate(seg_idx = paste(i, .data$effort_seg - min(.data$effort_seg) + 1, 
                             sep = "_"))
    
    # #-----
    # d <- vapply(cond.names, function(j) {
    #   length(unique(stats::na.omit(das.df[[j]]))) == 1
    # }, as.logical(1))
    # if (!all(d)) {
    #   print("Not all conditions are consistent!")
    #   browser()
    # }
    # #-----
    
    # Calculate segment lengths
    das.df$dist_from_prev[1] <- 0 #Ignore distance from last effort (not used)
    das.df$dist_to_next[nrow(das.df)] <- 0 #Ignore distance from last effort
    
    d <- das.df %>% 
      group_by(.data$effort_seg) %>% 
      summarise(sum_dist = sum(.data$dist_to_next))
    
    seg.lengths <- d$sum_dist
    
    #------------------------------------------------------
    ### Get segdata and return
    das.df.segdata <- airdas_segdata_avg(as_airdas_df(das.df), seg.lengths, i)
    
    list(das.df = das.df, seg.lengths = seg.lengths, das.df.segdata = das.df.segdata)
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
    select(-.data$effort_seg)
  
  
  #----------------------------------------------------------------------------
  # Return
  list(as_airdas_df(x.eff), segdata, NULL)
}
