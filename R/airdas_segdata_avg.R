#' Summarize data for a continuous effort section
#' 
#' Summarize effort data by effort segment, while averaging conditions
#' 
#' @param x data frame; a single continuous effort section of AirDAS data
#' @param subseg.lengths numeric; length of the modeling segments 
#'   into which \code{x} will be chopped
#' @param eff.id numeric; the ID of \code{x} (the current continuous effort section)
#' 
#' @importFrom dplyr %>% .data bind_cols everything mutate select
#' @importFrom lubridate year month day tz
#' @importFrom stats na.omit
#' @importFrom swfscMisc bearing destination
#' @importFrom utils head
#' 
#' @details This function was designed to be called \code{\link{airdas_chop_equal}}.
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
#'   Thus, if \code{eff.id} is \code{1}, then the segment ID for 
#'   the second segment from \code{x} is \code{"1_2"}.
#'   
#'   The average condition values are calculated as a weighted average by distance, 
#'   and reported for the following:
#'   Beaufort, cloud cover, altitude in feet, aircraft speed in knots, jellyfish code, 
#'   and the percentage of the segment with each of haze, kelp, and red tide. 
#'   
#'   \code{\link[swfscMisc]{bearing}} and \code{\link[swfscMisc]{destination}} 
#'   are used to calculate the segment midpoints.
#'   
#' @return Data frame with the segdata information described above
#' 
#' @keywords internal
#' 
#' @export
airdas_segdata_avg <- function(x, subseg.lengths, eff.id) {
  #----------------------------------------------------------------------------
  # Prep stuff
  das.df <- x
  
  ### Prep - get the info that is consistent for the entire effort length
  # ymd determined below to be safe
  df.out1 <- data.frame(
    file = das.df$file_das[1], transect = das.df$Trans[1], 
    stringsAsFactors = FALSE
  )
  
  stopifnot(nrow(df.out1) == 1)
  
  
  ### Prep - objects for for loop
  n.subseg <- length(subseg.lengths)
  subseg.cumsum <- cumsum(subseg.lengths)
  subseg.mid.cumsum <- (c(0, head(subseg.cumsum, -1)) + subseg.cumsum) / 2
  
  if (!("dist_from_prev_cumsum" %in% names(das.df))) {
    stopifnot("dist_from_prev" %in% names(das.df))
    das.df$dist_from_prev_cumsum <- cumsum(das.df$dist_from_prev)
  }
  
  
  # Store condition data in list for organization and readability
  conditions.list.init <- list(
    Bft = 0, CCover = 0, AltFt = 0, SpKnot = 0, Jelly = 0, #HKR = "",
    Haze = 0, Kelp = 0, RedTide = 0
  )
  
  # 'Initialize' necessary objects
  subseg.curr <- 1
  stlin.curr <- das.df$line_num[1]
  startpt.curr <- c(das.df$Lat[1], das.df$Lon[2])
  midpt.curr <- NULL
  segdata.all <- NULL
  
  conditions.list <- conditions.list.init
  
  stopifnot(nrow(das.df) >= 2)
  
  #----------------------------------------------------------------------------
  ### Step through each point in effort length, 
  #     calculating segment midpoints, endpoints, and avg conditions as you go
  for (j in 2:nrow(das.df)) {
    # t1 and t2: Is point j past the segment midpt or endpt, respectively,
    #   i.e. do we need to calculate the midpt or endpt?
    t1 <- das.df$dist_from_prev_cumsum[j] >= subseg.mid.cumsum[subseg.curr]
    t2 <- (das.df$dist_from_prev_cumsum[j] >= subseg.cumsum[subseg.curr])
    
    if (!t2) {
      # If we didn't cross a segment endpoint, get 
      #   1) the percentage of the segment between j-1 and j, and 
      #   2) the condition and sight info
      seg.percentage <- das.df$dist_from_prev[j] / subseg.lengths[subseg.curr]
      conditions.list <- fn_aggr_conditions(
        conditions.list, das.df, j-1, seg.percentage
      )
      rm(seg.percentage)
    }
    
    # While the current subsegment midpoint or endpoint 
    #   comes before the next event (which is indexed by j) 
    while((t1 & is.null(midpt.curr)) | t2) {
      ### Make objects for values used multiple times
      dist.subseg.curr <- subseg.cumsum[subseg.curr]
      dist.subseg.prev <- subseg.cumsum[subseg.curr - 1]
      dist.pt.prev     <- das.df$dist_from_prev_cumsum[j-1]
      dist.pt.curr     <- das.df$dist_from_prev_cumsum[j]
      
      
      ### Get data
      # Calculate midpoint (if not already done for this segment)
      if (t1 & is.null(midpt.curr)) {
        midpt.curr <- destination(
          das.df$Lat[j-1], das.df$Lon[j-1], 
          bearing(das.df$Lat[j-1], das.df$Lon[j-1], das.df$Lat[j], das.df$Lon[j])[1], 
          units = "km", 
          distance = subseg.mid.cumsum[subseg.curr] - dist.pt.prev
        )
      }
      
      # Calculate endpoint
      if (t2) {
        ### Destination calculated from das.df[j-1, ], so d calc is ok
        ###   (destination calculates the endpoint, not the seg length)
        d <- dist.subseg.curr - dist.pt.prev
        endpt.curr <- destination(
          das.df$Lat[j-1], das.df$Lon[j-1], 
          bearing(das.df$Lat[j-1], das.df$Lon[j-1], das.df$Lat[j], das.df$Lon[j])[1], 
          units = "km", type = "vincenty", distance = d
        )
        
        ### Conditions and sightings
        #     d.tmp handles multiple segments between pts, aka when current
        #     segment start point is closer than [j-1]
        d.tmp <- max(dist.pt.prev, dist.subseg.prev) 
        d.rat <- (dist.subseg.curr - d.tmp) / subseg.lengths[subseg.curr]
        # if (is.nan(d.rat)) d.rat <- NA
        conditions.list <- fn_aggr_conditions(conditions.list, das.df, j-1, d.rat)
        rm(d, d.tmp, d.rat)
        
        ## If next point is at the same location, don't end the segment yet
        if (j < nrow(das.df)) {
          if (das.df$dist_from_prev[j+1] == 0) {
            # tmp1 is dist from current point to end of last segment
            tmp1a <- ifelse(subseg.curr>1, subseg.cumsum[subseg.curr-1], 0) 
            tmp1 <- dist.pt.curr - tmp1a
            tmp2 <- subseg.lengths[subseg.curr]
            
            if (tmp1 <= tmp2) {break}
            rm(tmp1a, tmp1, tmp2)
          }
        }
        
        ### Store data from this segment
        # Get average condition information
        conditions.list.df <- data.frame(
          lapply(conditions.list, round, 2), stringsAsFactors = FALSE
        )
        names(conditions.list.df) <- paste0("ave", names(conditions.list.df))
        
        # Get start line
        j.stlin.curr <- which(das.df$line_num == stlin.curr)
        
        # Get observer information
        #   If beginning is TVPAW, then ignore Observers pre-P event
        if (nrow(das.df) > 5) {
          if (identical(das.df$Event[1:5], c("T", "V", "P", "A", "W"))) {
            das.df$ObsL[1:2] <- NA
            das.df$ObsB[1:2] <- NA
            das.df$ObsR[1:2] <- NA
            das.df$Rec[1:2] <- NA
          }
        }
        obs.vals <- vapply(c("ObsL", "ObsB", "ObsR", "Rec"), function(k) {
          k.uniq <- unique(das.df[[k]][j.stlin.curr:j])
          if (length(na.omit(k.uniq)) >= 1) k.uniq <- na.omit(k.uniq)
          paste(k.uniq, collapse = ",")
        }, as.character(1), USE.NAMES = TRUE)
        obs.vals[obs.vals == "NA"] <- NA
        
        # Put segdata in data frame
        segdata <- data.frame(
          seg_idx = paste0(eff.id, "_", subseg.curr), 
          stlin = stlin.curr, endlin = das.df$line_num[j],
          lat1 = startpt.curr[1], lon1 = startpt.curr[2], 
          lat2 = endpt.curr[1], lon2 = endpt.curr[2], 
          mlat = midpt.curr[1], mlon = midpt.curr[2],
          mDateTime = mean(c(das.df$DateTime[j.stlin.curr], das.df$DateTime[j])),
          dist = subseg.lengths[subseg.curr], 
          ObsL = obs.vals["ObsL"], ObsB = obs.vals["ObsB"], 
          ObsR = obs.vals["ObsR"], Rec = obs.vals["Rec"],
          stringsAsFactors = FALSE
        ) %>% 
          mutate(mtime = strftime(.data$mDateTime, format = "%H:%M:%S", 
                                  tz = tz(.data$mDateTime)), 
                 year = year(.data$mDateTime), month = month(.data$mDateTime), 
                 day = day(.data$mDateTime)) %>%
          bind_cols(df.out1, conditions.list.df)
        
        segdata.all <- rbind(segdata.all, segdata)
        rm(conditions.list.df, j.stlin.curr, obs.vals, segdata)
        
        
        ### Prep for next segment
        if (j == nrow(das.df) & subseg.curr == n.subseg) {
          # If at the end of das.df and all segs have been processed, break
          break
          
        } else {
          # Else, prep for next segment: 
          # Increment
          subseg.curr <- subseg.curr + 1
          
          # Reset/set points as appropriate
          startpt.curr <- endpt.curr
          midpt.curr <- NULL
          endpt.curr <- NULL
          stlin.curr <- das.df$line_num[j]
          
          t1 <- das.df$dist_from_prev_cumsum[j] >= subseg.mid.cumsum[subseg.curr]
          t2 <- das.df$dist_from_prev_cumsum[j] >= subseg.cumsum[subseg.curr]
          
          # If pt j is before the next seg endpoint, get data from endpt to j
          #   Else, this info is calculated in t2 section above
          tmp1 <- das.df$dist_from_prev_cumsum[j] - subseg.cumsum[subseg.curr - 1]
          tmp2 <- subseg.lengths[subseg.curr]
          conditions.list <- conditions.list.init
          
          if (tmp1 < tmp2) {
            conditions.list <- fn_aggr_conditions(
              conditions.list.init, das.df, j-1, tmp1 / tmp2
            )
          }
          rm(tmp1, tmp2)
        }
      }
    }
  }
  
  
  #----------------------------------------------------------------------------
  segdata.all %>% 
    select(.data$seg_idx, .data$transect, .data$file, .data$stlin, .data$endlin, 
           .data$lat1, .data$lon1, .data$lat2, .data$lon2, 
           .data$mlat, .data$mlon, .data$dist, 
           .data$mDateTime, .data$year, .data$month, .data$day, .data$mtime, 
           everything())
  
}