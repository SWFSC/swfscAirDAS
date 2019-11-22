#' Summarize data within effort segment
#' 
#' Summarize data for continuous effort section by effort segment
#' 
#' @param das.df data frame; todo
#' @param subseg.lengths todo
#' @param eff.id numeric; todo
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr .data
#' @importFrom dplyr bind_cols
#' @importFrom dplyr everything
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom stats na.omit
#' @importFrom swfscMisc bearing
#' @importFrom swfscMisc destination
#' @importFrom utils head
#' 
#' @details Get summary data for effort segment
#'   
#' @return Data frame...
#' 
#' @examples
#' #TODO
#' 
#' @export
airdas_effort_segdata <- function(das.df, subseg.lengths, eff.id) {
  #----------------------------------------------------------------------------
  # Prep stuff
  
  ### Check that observers stay consistent throughout effort segment
  # obs.vals1 <- vapply(c("ObsL", "ObsB", "ObsR", "Rec"), function(k) {
  #   k.uniq <- unique(das.df[[k]])
  #   if (length(na.omit(k.uniq)) >= 1) {k.uniq <- na.omit(k.uniq)}
  #   paste(k.uniq, collapse = ",")
  # }, as.character(1))
  
  ### Prep - get the info that is consistent for the entire effort length
  df.out1 <- data.frame(
    Date = as.Date(das.df$DateTime[1]), file = das.df$file_das[1], 
    transect = das.df$Trans[1], 
    stringsAsFactors = FALSE
  ) %>% 
    mutate(year = year(.data$Date), month = month(.data$Date), 
           day = day(.data$Date)) %>% 
    select(.data$file, .data$transect, .data$year, .data$month, .data$day)
  
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
  cond.list.init <- list(
    Bft = 0, CCover = 0, AltFt = 0, SpKnot = 0, Jelly = 0, HKR = ""
  )
  
  # 'Initialize' necessary objects
  subseg.curr <- 1
  stlin.curr <- das.df$line_num[1]
  startpt.curr <- c(das.df$Lat[1], das.df$Lon[2])
  midpt.curr <- NULL
  segdata.all <- NULL
  
  cond.list <- cond.list.init
  
  stopifnot(nrow(das.df) >= 2)
  
  ### Step through each point in effort length, 
  #     calculating segment midpoints, endpoints, and avg conditions as you go
  # if (unique(das.df$effort_seg) == 11) browser()
  
  
  for (j in 2:nrow(das.df)) {
    # if (j == 44) browser()
    # t1 and t2: Is point j past the segment midpt or endpt, respectively,
    #   i.e. do we need to calculate the midpt or endpt
    t1 <- das.df$dist_from_prev_cumsum[j] >= subseg.mid.cumsum[subseg.curr]
    t2 <- (das.df$dist_from_prev_cumsum[j] >= subseg.cumsum[subseg.curr])
    
    if (!t2) {
      # If we didn't cross a segment endpoint, get condition and sight info
      d.rat <- das.df$dist_from_prev[j] / subseg.lengths[subseg.curr]
      cond.list <- fn_aggr_conditions(cond.list, das.df, j-1, d.rat)
      rm(d.rat)
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
        cond.list <- fn_aggr_conditions(cond.list, das.df, j-1, d.rat)
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
        cond.list.df <- data.frame(cond.list, stringsAsFactors = FALSE)
        names(cond.list.df) <- paste0("ave", names(cond.list.df))
        
        j.stlin.curr <- which(das.df$line_num == stlin.curr)
        
        obs.vals <- vapply(c("ObsL", "ObsB", "ObsR", "Rec"), function(k) {
          k.uniq <- unique(das.df[[k]][j.stlin.curr:j])
          if (length(na.omit(k.uniq)) >= 1) {k.uniq <- na.omit(k.uniq)}
          paste(k.uniq, collapse = ",")
        }, as.character(1), USE.NAMES = TRUE)
        obs.vals[obs.vals == "NA"] <- NA
        
        segdata <- data.frame(
          seg_idx = paste0(eff.id, "_", subseg.curr), 
          stlin = stlin.curr, endlin = das.df$line_num[j],
          lat1 = startpt.curr[1], lon1 = startpt.curr[2], 
          lat2 = endpt.curr[1], lon2 = endpt.curr[2], 
          mlat = midpt.curr[1], mlon = midpt.curr[2],
          mtime = mean(c(das.df$DateTime[j.stlin.curr], das.df$DateTime[j])),
          dist = subseg.lengths[subseg.curr], 
          ObsL = obs.vals["ObsL"], ObsB = obs.vals["ObsB"], 
          ObsR = obs.vals["ObsR"], Rec = obs.vals["Rec"],
          stringsAsFactors = FALSE
        ) %>% 
          bind_cols(df.out1, cond.list.df)
        
        segdata.all <- rbind(segdata.all, segdata)
        rm(cond.list.df, j.stlin.curr, obs.vals, segdata)
        
        
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
          cond.list <- cond.list.init
          
          if (tmp1 < tmp2) {
            cond.list <- fn_aggr_conditions(
              cond.list.init, das.df, j-1, tmp1 / tmp2
            )
          }
          rm(tmp1, tmp2)
        }
      }
    }
  }
  
  segdata.all %>% 
    select(.data$seg_idx, .data$transect, .data$file, .data$stlin, .data$endlin, 
           everything())
}