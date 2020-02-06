#' Process aerial survey DAS data
#'
#' Process AirDAS data (the output of \code{\link{airdas_read}}), 
#'   including extracting state and condition information for each AirDAS event
#'
#' @param x either a data frame (the output of \code{\link{airdas_read}}), 
#'   or a character (filepath) which is first passed to \code{\link{airdas_read}}
#' @param ... ignored
#' @export
airdas_process <- function(x, ...) UseMethod("airdas_process")


#' @name airdas_process
#' @export
airdas_process.character <- function(x, ...) {
  airdas_process(airdas_read(x), ...)
}


#' @name airdas_process
#' @export
airdas_process.data.frame <- function(x, ...) {
  airdas_process(as_airdas_dfr(x), ...)
}


#' @name airdas_process
#' 
#' @param days.gap.part numeric of length 1; 
#'   time gap (in days) used to identify when a 'partial reset' is performed, 
#'   i.e. when propogated info (weather, observers, etc) is reset. 
#'   Default is 30 minutes; must be less than or equal to \code{days.gap.full}
#' @param days.gap.full numeric of length 1; 
#'   time gap (in days) used to identify when a 'full reset; is performed, 
#'   i.e. when all info (transect number and propogated info) is reset. 
#'   Default is 12 hours
#' @param gap.message logical; should messages be printed detailing which row(s) of the 
#'   output data frame were partially or fully reset?
#'   
#' @importFrom dplyr %>% select
#' @importFrom rlang !!
#'
#' @details If \code{x} is a character, it is assumed to be a filepath 
#'   and first passed to \code{\link{airdas_read}}.
#'   This output is then passed to \code{airdas_process}.
#'      
#'   AirDAS data is event-based, meaning most events indicate when a state or weather condition changes.
#'   For instance, a 'W' event indicates when the Beaufort sea state changes, and 
#'   the Beaufort is the same for subsequent events until the next 'W' event.
#'   For each state/condition: a new column is created, 
#'   the state/condition information is extracted from relevant events, 
#'   and extracted information is propagated to appropriate subsequent rows (events). 
#'   Thus, each row in the output data frame contains all 
#'   pertinent state/condition information for that row.
#'   
#'   The following assumptions/decisions are made during processing:
#'   \itemize{
#'     \item All '#' events (deleted events) are removed
#'     \item 'Datetime', 'Lat', and 'Lon' information are added to '1' events
#'     \item Effort is determined as follows: T/R events turns effort on, 
#'       and O/E events turn effort off. 
#'       T/R events themselves will be on effort, while O/E events will be off effort.
#'       The 'EffortDot' column is ignored
#'     \item Missing values are \code{NA} rather than \code{-1}
#'   }
#'   
#'   See \code{\link{airdas_format_pdf}} for more information about AirDAS format requirements
#'   
#' @return An \code{airdas_df} object, which is also a data frame.
#'   It consists of the input data frame, i.e. the output of \code{\link{airdas_read}},
#'   with the following columns added:
#'   \tabular{lll}{
#'     \emph{State/condition}            \tab \emph{Column name} \tab \emph{Data source}\cr
#'     On/off effort                     \tab OnEffort \tab T/R and O/E events\cr
#'     Transect code                     \tab Trans    \tab Event: T; Column: Data1\cr
#'     Beaufort sea state                \tab Bft      \tab Event: W; Column: Data3\cr
#'     Percent overcast (cloud cover)    \tab CCover   \tab Event: W; Column: Data2\cr
#'     Jellyfish code                    \tab Trans    \tab Event: W; Column: Data4\cr
#'     Horizontal sun (clock system)     \tab HorizSun \tab Event: W; Column: Data5\cr
#'     Haze/Kelp/Red tide code           \tab HKR      \tab Event: W; Column: Data1\cr
#'     Haze (from HKR code)              \tab Haze     \tab HKR\cr
#'     Kelp (from HKR code)              \tab Kelp     \tab HKR\cr
#'     Red tide (from HKR code)          \tab RedTide  \tab HKR\cr
#'     Left observer                     \tab ObsL     \tab Event: P; Column: Data1\cr
#'     Belly observer                    \tab ObsB     \tab Event: P; Column: Data2\cr
#'     Right observer                    \tab ObsR     \tab Event: P; Column: Data3\cr
#'     Data recorder                     \tab Rec      \tab Event: P; Column: Data4\cr
#'     Altitude (feet)                   \tab AltFt    \tab Event: A; Column: Data1\cr
#'     Speed (knots)                     \tab SpKnot   \tab Event: A; Column: Data2\cr
#'     Viewing condition - left inside   \tab VLI      \tab Event: V; Column: Data1\cr
#'     Viewing condition - left outside  \tab VLO      \tab Event: V; Column: Data2\cr
#'     Viewing condition - belly         \tab VB       \tab Event: V; Column: Data3\cr
#'     Viewing condition - right inside  \tab VRI      \tab Event: V; Column: Data4\cr
#'     Viewing condition - right outside \tab VRO      \tab Event: V; Column: Data5\cr
#'   }
#'
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' airdas_process(y)
#' 
#' y.read <- airdas_read(y)
#' airdas_process(y.read)
#'
#' @export
airdas_process.airdas_dfr <- function(x, days.gap.part = 0.5/24, 
                                      days.gap.full = 12/24, 
                                      gap.message = FALSE, ...) 
{ 
  #----------------------------------------------------------------------------
  # Input checks
  das.df <- x
  stopifnot(
    # .airdas_format_check(das.df, "process"), 
    length(days.gap.part) == 1, 
    length(days.gap.full) == 1, 
    inherits(days.gap.part, c("integer", "numeric")), 
    inherits(days.gap.full, c("integer", "numeric")), 
    inherits(gap.message, "logical")
  )
  
  if (days.gap.part > days.gap.full) {
    stop("days.gap.part must be less than or equal to days.gap.full")
  }
  
  
  #----------------------------------------------------------------------------
  # Remove '#' (deleted) events
  das.del <- das.df$Event == "#"
  das.df <- das.df[!das.del, ]
  
  
  #----------------------------------------------------------------------------
  # Fill in Lat/Lon/DateTime of '1' events
  event.1 <- which(das.df$Event == 1)
  stopifnot(all(event.1 > 1))
  
  das.df$Lat[event.1] <- das.df$Lat[event.1 - 1]
  das.df$Lon[event.1] <- das.df$Lon[event.1 - 1]
  das.df$DateTime[event.1] <- das.df$DateTime[event.1 - 1]
  
  
  #----------------------------------------------------------------------------
  ### Determine new days for reset of columns
  nDAS <- nrow(das.df)
  
  dt.na <- is.na(das.df$DateTime)
  time.diff <- rep(NA, nDAS)
  time.diff[!dt.na] <- c(NA, abs(diff(das.df$DateTime[!dt.na]))) / (60*60*24)
  
  # Soft reset (not transect num) for time gaps > days.gap.part
  idx.reset.part <- c(1, which(time.diff > days.gap.part))
  # Full reset (including transect num) for time gaps > days.gap.full
  idx.reset.full <- c(1, which(time.diff > days.gap.full))
  
  if (gap.message) {
    message("A 'partial reset' was performed at the following row indicies of the output data frame: ", 
            idx.reset.part)
    message("A 'full reset' was performed at the following row indicies of the output data frame: ", 
            idx.reset.full)
  }
  
  if (!all(idx.reset.full %in% idx.reset.part)) {
    warning("Warning: not all full resets were in partial resets; ", 
            "check days.gap?", immediate. = TRUE)
  }
  
  if (!all(which(!duplicated(das.df$file_das)) %in% idx.reset.part)) {
    warning("Warning: not all new flight row indices were new file indices", 
            immediate. = TRUE)
  }
  
  
  #----------------------------------------------------------------------------
  ### Create vectors with data where values change/are reset
  init.val <- as.numeric(rep(NA, nDAS))
  event.na <- -9999
  
  event.A <- das.df$Event == "A"
  event.E <- das.df$Event == "E"
  event.O <- das.df$Event == "O"
  event.P <- das.df$Event == "P"
  event.R <- das.df$Event == "R"
  event.T <- das.df$Event == "T"
  event.V <- das.df$Event == "V"
  event.W <- das.df$Event == "W"
  
  Bft      <- .airdas_process_num(init.val, das.df, "Data3", event.W, event.na)
  CCover   <- .airdas_process_num(init.val, das.df, "Data2", event.W, event.na)
  Jelly    <- .airdas_process_num(init.val, das.df, "Data4", event.W, event.na)
  HorizSun <- .airdas_process_num(init.val, das.df, "Data5", event.W, event.na)
  HKR      <- .airdas_process_chr(init.val, das.df, "Data1", event.W, event.na)
  
  ObsL <- .airdas_process_chr(init.val, das.df, "Data1", event.P, event.na)
  ObsB <- .airdas_process_chr(init.val, das.df, "Data2", event.P, event.na)
  ObsR <- .airdas_process_chr(init.val, das.df, "Data3", event.P, event.na)
  Rec <-  .airdas_process_chr(init.val, das.df, "Data4", event.P, event.na)
  
  AltFt  <- .airdas_process_num(init.val, das.df, "Data1", event.A, event.na)
  SpKnot <- .airdas_process_num(init.val, das.df, "Data2", event.A, event.na)
  
  VLI <- .airdas_process_chr(init.val, das.df, "Data1", event.V, event.na)
  VLO <- .airdas_process_chr(init.val, das.df, "Data2", event.V, event.na)
  VB  <- .airdas_process_chr(init.val, das.df, "Data3", event.V, event.na)
  VRI <- .airdas_process_chr(init.val, das.df, "Data4", event.V, event.na)
  VRO <- .airdas_process_chr(init.val, das.df, "Data5", event.V, event.na)
  
  Trans <- .airdas_process_chr(init.val, das.df, "Data1", event.T, event.na)
  Trans[event.O] <- event.na
  
  Eff <- init.val
  Eff[event.O | event.E] <- FALSE
  Eff[idx.reset.full] <- event.na # For resets immediately after O or E events
  Eff[event.T | event.R] <- TRUE
  
  # Additional processing done after for loop
  
  
  #----------------------------------------------------------------------------
  # Loop through data for 'carry-over info' that applies to subsequent events
  # idx.new.cruise always includes 1, so don't need to pre-set Last.. objects
  for (i in seq_len(nDAS)) {
    # Reset data when necessary
    if (i %in% idx.reset.part) {
      LastBft <- LastCCover <- LastJelly <- LastHorizSun <- LastHKR <-
        LastObsL <- LastObsB <- LastObsR <- LastRec <- LastAltFt <- LastSpKnot <-
        LastVLI <- LastVLO <- LastVB <- LastVRI <- LastVRO <- LastEff <- NA
    }
    
    # Reset transect number only when it is a new day;
    #   all of idx.reset.full are in idx.reset.part
    if (i %in% idx.reset.full) LastTrans <- NA
    
    # Set/pass along 'carry-over info'
    if (is.na(Bft[i]))      Bft[i] <- LastBft           else LastBft <- Bft[i]           #Beaufort
    if (is.na(CCover[i]))   CCover[i] <- LastCCover     else LastCCover <- CCover[i]     #Cloud cover
    if (is.na(Jelly[i]))    Jelly[i] <- LastJelly       else LastJelly <- Jelly[i]       #Jellyfish
    if (is.na(HorizSun[i])) HorizSun[i] <- LastHorizSun else LastHorizSun <- HorizSun[i] # Horizontal sun
    if (is.na(HKR[i]))      HKR[i] <- LastHKR           else LastHKR <- HKR[i]           #Haze/Kelp/Red tide
    if (is.na(ObsL[i]))     ObsL[i] <- LastObsL         else LastObsL <- ObsL[i]         #Observer - left
    if (is.na(ObsB[i]))     ObsB[i] <- LastObsB         else LastObsB <- ObsB[i]         #Observer - belly
    if (is.na(ObsR[i]))     ObsR[i] <- LastObsR         else LastObsR <- ObsR[i]         #Observer - right
    if (is.na(Rec[i]))      Rec[i] <- LastRec           else LastRec <- Rec[i]           #Recorder
    if (is.na(AltFt[i]))    AltFt[i] <- LastAltFt       else LastAltFt <- AltFt[i]       #Altitude (ft)
    if (is.na(SpKnot[i]))   SpKnot[i] <- LastSpKnot     else LastSpKnot <- SpKnot[i]     #Speed (knots)
    if (is.na(VLI[i]))      VLI[i] <- LastVLI           else LastVLI <- VLI[i]           #Visibility - left inner
    if (is.na(VLO[i]))      VLO[i] <- LastVLO           else LastVLO <- VLO[i]           #Visibility - left outer
    if (is.na(VB[i]))       VB[i] <- LastVB             else LastVB <- VB[i]             #Visibility - belly
    if (is.na(VRI[i]))      VRI[i] <- LastVRI           else LastVRI <- VRI[i]           #Visibility - right inner
    if (is.na(VRO[i]))      VRO[i] <- LastVRO           else LastVRO <- VRO[i]           #Visibility - right outer
    if (is.na(Trans[i]))    Trans[i] <- LastTrans       else LastTrans <- Trans[i]       #Transect number
    if (is.na(Eff[i]))      Eff[i] <- LastEff           else LastEff <- Eff[i]           #Effort
  }
  
  # Post-processing
  tmp <- list(
    Bft = Bft, CCover = CCover, Jelly = Jelly, HorizSun = HorizSun, HKR = HKR, 
    Haze = grepl("h", HKR, ignore.case = TRUE), Kelp = grepl("h", HKR, ignore.case = TRUE), 
    RedTide = grepl("r", HKR, ignore.case = TRUE), 
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
  
  
  #----------------------------------------------------------------------------
  cols.toreturn <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort", "Trans", 
    "Bft", "CCover", "Jelly", "HorizSun", "HKR", "Haze", "Kelp", "RedTide", 
    "ObsL", "ObsB", "ObsR", "Rec", "AltFt", "SpKnot", 
    "VLI", "VLO", "VB", "VRI", "VRO", 
    "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7",
    "EffortDot", "EventNum", "file_das", "line_num"
  )
  
  as_airdas_df(
    data.frame(das.df, tmp, stringsAsFactors = FALSE, row.names = 1:nrow(das.df)) %>%
      select(!!cols.toreturn)
  )
}
