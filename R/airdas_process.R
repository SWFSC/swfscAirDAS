#' Process aerial survey DAS data
#'
#' Process output of \link{airdas_read}
#'
#' @param x Either a data frame (the output of \link{airdas_read}) or
#'   a character which is then passed to \link{airdas_read}
#' @param ... Ignore
#' @export
airdas_process <- function(x, ...) UseMethod("airdas_process")


#' @name airdas_process
#' @export
airdas_process.character <- function(x, ...) {
  airdas_process(airdas_read(x), ...)
}


#' @name airdas_process
#' 
#' @param days.gap numeric of length 1; time gap (in days) used to identify 
#'   when propogated info (weather/env, observers, etc) is reset. 
#'   The default is 30 minutes 
#'   
#' @importFrom dplyr %>%
#' @importFrom dplyr .data
#' @importFrom dplyr select
#'
#' @details Read...
#'   Adapted from \code{\link[swfscMisc]{das.read}}
#'  TODO: describe columns created?
#'  TODO: any error checking?
#'
#' @return Processed aerial survey DAS data...
#'
#' @examples
#' # d <- do.call(rbind, lapply(list.files("../airdas/airDAS_files", full.names = TRUE), airdas_read))
#' # x <- airdas_process(d)
#' airdas_process(system.file("airdas_sample.das", package = "swfscAirDAS"))
#' 
#' das.sample <- airdas_read(system.file("airdas_sample.das", package = "swfscAirDAS"))
#' airdas_process(das.sample)
#'
#' @export
airdas_process.data.frame <- function(x, days.gap = 0.5/24, ...) { 
  # TODO: #Add flag for printing out lines ID'd by days .gap for KAF error check
  # TODO: rm names.expected
  #----------------------------------------------------------------------------
  das.df <- x
  names.expected <- c(
    "Event", "EffortDot", "DateTime", "Lat", "Lon", 
    "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", 
    "file_das", "event_num", "line_num"
  )
  
  stopifnot(
    identical(names(das.df), names.expected)
  ); rm(names.expected)
  
  nDAS <- nrow(das.df)
  
  
  #----------------------------------------------------------------------------
  ### Determine new days for reset of columns
  dt.na <- is.na(das.df$DateTime)
  time.diff <- rep(NA, nDAS)
  time.diff[!dt.na] <- c(NA, abs(diff(das.df$DateTime[!dt.na]))) / (60*60*24)
  
  idx.reset <- c(1, which(time.diff > days.gap)) #Will not reset transect num
  idx.reset.full <- c(1, which(time.diff > 0.5)) #Time diff > 12 hours, reset all
  
  if (!all(idx.reset.full %in% idx.reset)) {
    warning("Warning: not all full resets were in partial resets; ", 
            "check days.gap?", immediate. = TRUE)
  }
  
  if (!all(which(!duplicated(das.df$file_das)) %in% idx.reset)) {
    warning("Warning: not all new flight row indices were new file indices", 
            immediate. = TRUE)
  }
  
  
  #----------------------------------------------------------------------------
  # Determine effort using events
  
  ### 'Initialize' appropriate objects as NA; will be updated in for loop
  # t1 <- Sys.time()
  init.val <- as.numeric(rep(NA, nDAS))
  
  Bft <- CCover <- Jelly <- HorizSun <- HKR <-
    ObsL <- ObsB <- ObsR <- Rec <- AltFt <- SpKnot <-
    VLI <- VLO <- VB <- VRI <- VRO <- Trans <- Eff <- init.val
  
  LastBft <- LastCCover <- LastJelly <- LastHorizSun <- LastHKR <-
    LastObsL <- LastObsB <- LastObsR <- LastRec <- LastAltFt <- LastSpKnot <-
    LastVLI <- LastVLO <- LastVB <- LastVRI <- LastVRO <- LastTrans <- 
    LastEff <- NA
  
  ### Indices of specific events
  event.A <- das.df$Event == "A"
  event.E <- das.df$Event == "E"
  event.O <- das.df$Event == "O"
  event.P <- das.df$Event == "P"
  event.R <- das.df$Event == "R"
  event.T <- das.df$Event == "T"
  event.V <- das.df$Event == "V"
  event.W <- das.df$Event == "W"
  
  browser()
  
  ### Put data in vectors in all places where data values change
  event.na <- -9999
  
  Bft[event.W]      <- .airdas_process_num(das.df, "Data3", event.W, event.na)
  CCover[event.W]   <- .airdas_process_num(das.df, "Data2", event.W, event.na)
  Jelly[event.W]    <- .airdas_process_num(das.df, "Data4", event.W, event.na)
  HorizSun[event.W] <- .airdas_process_num(das.df, "Data5", event.W, event.na)
  HKR[event.W]      <- .airdas_process_chr(das.df, "Data1", event.W, event.na)
  
  ObsL[event.P] <- .airdas_process_chr(das.df, "Data1", event.P, event.na)
  ObsB[event.P] <- .airdas_process_chr(das.df, "Data2", event.P, event.na)
  ObsR[event.P] <- .airdas_process_chr(das.df, "Data3", event.P, event.na)
  Rec[event.P] <-  .airdas_process_chr(das.df, "Data4", event.P, event.na)
  
  AltFt[event.A]  <- .airdas_process_num(das.df, "Data1", event.A, event.na)
  SpKnot[event.A] <- .airdas_process_num(das.df, "Data2", event.A, event.na)
  
  VLI[event.V] <- .airdas_process_chr(das.df, "Data1", event.V, event.na)
  VLO[event.V] <- .airdas_process_chr(das.df, "Data2", event.V, event.na)
  VB[event.V]  <- .airdas_process_chr(das.df, "Data3", event.V, event.na)
  VRI[event.V] <- .airdas_process_chr(das.df, "Data4", event.V, event.na)
  VRO[event.V] <- .airdas_process_chr(das.df, "Data5", event.V, event.na)
  
  Trans[which(event.O) + 1] <- event.na
  Trans[event.T] <- .airdas_process_chr(das.df, "Data1", event.T, event.na)
  
  Eff[which(event.O) + 1] <- FALSE
  Eff[which(event.E) + 1] <- FALSE
  Eff[idx.reset.full] <- event.na # For resets immediately after O or E events
  Eff[event.T] <- TRUE
  Eff[event.R] <- TRUE
  
  
  
  for (i in seq_len(nDAS)) {
    # Reset data when necessary
    if (i %in% idx.reset) {
      LastBft <- LastCCover <- LastJelly <- LastHorizSun <- LastHKR <-
        LastObsL <- LastObsB <- LastObsR <- LastRec <- LastAltFt <- LastSpKnot <-
        LastVLI <- LastVLO <- LastVB <- LastVRI <- LastVRO <- LastEff <- NA
    }
    
    # Reset transect number only when it is a new day
    if (i %in% idx.reset.full) LastTrans <- NA
    
    # Set/pass along 'carry-over info'
    if (is.na(Bft[i])) Bft[i] <- LastBft else LastBft <- Bft[i]                #Beaufort
    if (is.na(CCover[i])) CCover[i] <- LastCCover else LastCCover <- CCover[i] #Cloud cover
    if (is.na(Jelly[i])) Jelly[i] <- LastJelly else LastJelly <- Jelly[i]      #Jellyfish
    if (is.na(HorizSun[i])) HorizSun[i] <- LastHorizSun else LastHorizSun <- HorizSun[i] # Horizontal sun
    if (is.na(HKR[i])) HKR[i] <- LastHKR else LastHKR <- HKR[i]                #Haze/Kelp/Red tide
    if (is.na(ObsL[i])) ObsL[i] <- LastObsL else LastObsL <- ObsL[i]           #Observer - left
    if (is.na(ObsB[i])) ObsB[i] <- LastObsB else LastObsB <- ObsB[i]           #Observer - belly
    if (is.na(ObsR[i])) ObsR[i] <- LastObsR else LastObsR <- ObsR[i]           #Observer - right
    if (is.na(Rec[i])) Rec[i] <- LastRec else LastRec <- Rec[i]                #Recorder
    if (is.na(AltFt[i])) AltFt[i] <- LastAltFt else LastAltFt <- AltFt[i]      #Altitude (ft)
    if (is.na(SpKnot[i])) SpKnot[i] <- LastSpKnot else LastSpKnot <- SpKnot[i] #Speed (knots)
    if (is.na(VLI[i])) VLI[i] <- LastVLI else LastVLI <- VLI[i]                #Visibility - left inner
    if (is.na(VLO[i])) VLO[i] <- LastVLO else LastVLO <- VLO[i]                #Visibility - left outer
    if (is.na(VB[i])) VB[i] <- LastVB else LastVB <- VB[i]                     #Visibility - belly
    if (is.na(VRI[i])) VRI[i] <- LastVRI else LastVRI <- VRI[i]                #Visibility - right inner
    if (is.na(VRO[i])) VRO[i] <- LastVRO else LastVRO <- VRO[i]                #Visibility - right outer
    if (is.na(Trans[i])) Trans[i] <- LastTrans else LastTrans <- Trans[i]      #Transect number
    if (is.na(Eff[i])) Eff[i] <- LastEff else LastEff <- Eff[i]                #Effort
  }
  
  
  tmp <- list(
    Bft = Bft, CCover = CCover, Jelly = Jelly, HorizSun = HorizSun, HKR = HKR, 
    ObsL = ObsL, ObsB = ObsB, ObsR = ObsR, Rec = Rec, 
    AltFt = AltFt, SpKnot = SpKnot, 
    VLI = VLI, VLO = VLO, VB = VB, VRI = VRI, VRO = VRO, 
    Trans = Trans, OnEffort = Eff
  )
  
  # Replace event.reset values with NAs
  tmp <- lapply(tmp, function(j) {
    j[j == -9999] <- NA
    j
  })
  
  tmp$OnEffort <- as.logical(tmp$OnEffort)
  tmp$OnEffort[is.na(tmp$OnEffort)] <- FALSE
  
  data.frame(das.df, tmp, stringsAsFactors = FALSE) %>%
    select(.data$Event, .data$DateTime, .data$Lat, .data$Lon, .data$OnEffort, .data$Trans, 
           .data$Bft, .data$CCover, .data$Jelly, .data$HorizSun, .data$HKR, 
           .data$ObsL, .data$ObsB, .data$ObsR, .data$Rec, .data$AltFt, .data$SpKnot, 
           .data$VLI, .data$VLO, .data$VB, .data$VRI, .data$VRO, 
           .data$Data1, .data$Data2, .data$Data3, .data$Data4, .data$Data5, .data$Data6, .data$Data7,
           .data$EffortDot, .data$file_das, .data$event_num, .data$line_num)
}