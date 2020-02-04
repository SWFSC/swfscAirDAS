#' Summarize aerial DAS effort
#'
#' Summarize aerial DAS data by effort segments
#' 
#' @param x data frame; processed aerial DAS data output from \link{airdas_process} (or subsequent airdas function)
#' @param seg.km numeric; length of effort segments 
#' @param sp.codes character; species codes to include in segdata (and siteinfo?)
#' @param randpicks.load character; file to run read.csv on and pass output to airdas_effort_chop TODO
#' @param randpicks.save character; if not 
#' 
#' @importFrom dplyr %>%  between bind_cols filter full_join group_by summarise
#' @importFrom rlang !!
#' @importFrom swfscMisc distance
#' @importFrom utils read.csv write.csv
#' 
#' @details Todo
#' 
#'   "continuous effort sections" and "modeling segments"
#' 
#'   Filters for events where \code{OnEffort} is \code{TRUE}
#'   
#'   Continuous effort sections ID'd by T and R events
#' 
#'   Uses \link[swfscMisc]{distance}, \code{method = "vincenty"} to calculate distance (in km) 
#'   between events (lat/lon points)
#'   
#'   Describe segdata, siteinfo, and randpicks
#' 
#' @return List of segdata, siteinfo, and randpicks data frames
#' 
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' 
#' y.proc <- airdas_process(y)
#' airdas_effort(y.proc, 3, sp.codes = c("mn", "bm"))
#' 
#' y.rand <- system.file("airdas_sample_randpicks.csv", package = "swfscAirDAS")
#' airdas_effort(
#'   y.proc, 3, sp.codes = c("mn", "bm"), randpicks.load = y.rand
#' )
#' 
#' @export
airdas_effort <- function(x, seg.km, sp.codes, 
                          randpicks.load = NULL, randpicks.save = NULL) {
  #----------------------------------------------------------------------------
  # TODO: format.. checks
  das.df <- x
  das.df.orig <- das.df
  
  stopifnot(
    sum(is.na(das.df$Lat)) == 0,
    sum(is.na(das.df$Lon)) == 0, 
    sum(das.df$Event == "#") == 0
  )
  
  seg.km <- suppressWarnings(as.numeric(seg.km))
  if (!inherits(seg.km, c("numeric", "integer"))) {
    stop("seg.km must be a number")
  }
  
  
  #----------------------------------------------------------------------------
  # Prep
  
  ### Filter for on effort (and on effort + 1 for O/E event), 
  ###   and number continuous effort sections
  das.oneff <- sort(unique(c(which(das.df$OnEffort), which(das.df$OnEffort) + 1)))
  stopifnot(all(between(das.oneff, 1, nrow(das.df))))
  
  das.df <- das.df %>%
    # filter(.data$OnEffort | .data$Event %in% c("O", "E")) %>% #bug if consecutive O/E events
    mutate(cont_eff_section = cumsum(.data$Event %in% c("T", "R")))
  das.df <- das.df[das.oneff, ]
  rm(das.oneff)
  
  ### For each event, calculate distance traveled since previous event
  dist.from.prev <- mapply(function(x1, y1, x2, y2) {
    distance(y1, x1, y2, x2, units = "km", method = "vincenty")
  },
  x1 = head(das.df$Lon, -1), y1 = head(das.df$Lat, -1),
  x2 = tail(das.df$Lon, -1), y2 = tail(das.df$Lat, -1),
  SIMPLIFY = TRUE)
  
  das.df$dist_from_prev <- c(NA, dist.from.prev)
  
  
  #----------------------------------------------------------------------------
  ### Load randpicks file, if specified
  if (is.null(randpicks.load)) {
    r.pos <- NULL
    
  } else {
    randpicks.df <- read.csv(randpicks.load)
    if (all(c("effort_section", "randpicks") %in% names(randpicks.df))) {
      r.eff.sect <- randpicks.df$effort_section
      r.pos <- randpicks.df$randpicks
      
    } else {
      warning("For the provided randpicks CSV file, it is assumed that ", 
              "the first column is the continuous effort section numbers, ", 
              "and the second column is the randpick values for that ", 
              "continuous effort section")
      r.eff.sect <- randpicks.df[[1]]
      r.pos <- randpicks.df[[2]]
    }
  }
  
  
  ### Do stuff
  eff.uniq <- unique(das.df$cont_eff_section)
  # Check against provided randpicks
  if (exists("r.eff.sect")) {
    if (length(eff.uniq) != length(r.eff.sect)) 
      stop("The provided AirDAS data (das.df) does not have the same number of ", 
           "continuous effort sections as the provided randpicks file has rows. ", 
           "Did you load the correct randpicks file, and does it have ", 
           "proper column names? See `?airdas_effort` for more details")
  }
  
  eff.list <- lapply(eff.uniq, function(i, das.df, seg.km, r.pos) {
    das.curr <- filter(das.df, .data$cont_eff_section == i)
    # Get segment lengths
    y <- airdas_effort_chop(das.curr, seg.km, r.pos[i])
    y[[1]]$seg_idx <- paste0(i, "_", y[[1]]$effort_seg)
    
    # Summarize info by segment
    y.segdata <- airdas_effort_segdata(y[[1]], y[[2]], i)
    
    list(y[[1]], y[[2]], y[[3]], y.segdata)
  }, das.df = das.df, seg.km = seg.km, r.pos = r.pos)
  
  
  
  
  
  #----------------------------------------------------------------------------
  # Extract information about each segment
  
  ### Randpicks; including writing to csv if specified
  randpicks <- data.frame(
    effort_section = eff.uniq,
    randpicks = vapply(eff.list, function(j) j[[3]], 1)
  )
  if (!is.null(randpicks.save)) 
    write.csv(randpicks, file = randpicks.save, row.names = FALSE)
  
  ### Segment lengths (todo: remove)
  segdata.len <- lapply(eff.list, function(j) j[[2]])
  
  ### Segdata
  segdata <- data.frame(
    do.call(rbind, lapply(eff.list, function(i) i[[4]])), 
    stringsAsFactors = FALSE
  ) %>%
    mutate(segnum = seq_along(.data$seg_idx)) %>%
    select(.data$segnum, .data$seg_idx, everything())
  # TODO: round decimals? to 4?
  
  ### Each das data point, along with segnum
  das.df.eff <- data.frame(
    do.call(rbind, lapply(eff.list, function(i) i[[1]])), 
    stringsAsFactors = FALSE
  ) %>% 
    left_join(segdata[, c("seg_idx", "segnum")], by = "seg_idx")
  
  ### Sanity check - todo: remove
  # stopifnot(
  #   all(segdata$dist == unlist(segdata.len)), 
  #   identical(das.df %>% select(Event, DateTime, Lat, Lon) %>% tibble::remove_rownames(), 
  #             das.df.eff %>% select(Event, DateTime, Lat, Lon) %>% tibble::remove_rownames()), 
  #   sum(duplicated(segdata$seg_idx)) == 0, 
  #   all(das.df.eff$seg_idx %in% segdata$seg_idx), 
  #   nrow(segdata) == length(unlist(lapply(eff.list, function(i) i[[2]]))), 
  #   all.equal(segdata$dist, unlist(lapply(eff.list, function(i) i[[2]])))
  # )
  
  
  #----------------------------------------------------------------------------
  # Summarize sightings (based on siteinfo) and add applicable data to segdata
  
  ### Columns to be included in siteinfo output
  # siteinfo.include <- c(
  #   "segnum", "seg_idx", "Event", "DateTime", "Lat", "Lon", "OnEffort", 
  #   "Trans", "Bft", "CCover", "Jelly", "HorizSun", "HKR", 
  #   "ObsL", "ObsB", "ObsR", "Rec", "AltFt", "SpKnot", 
  #   #paste0("Data", 1:7), 
  #   "file_das", "line_num", "included"
  # )
  
  ### Filter sightings, then add sightings info to segdata
  # Current sighting filters:
  #   Beaufort <= 5
  #   Truncation angle is between -78 and 78
  #   Standard effort, i.e. observer is one of ObsL, ObsB, or ObsR
  #   On effort
  
  # das.forsiteinfo <- das.df.eff[, siteinfo.include]
  siteinfo <- airdas_sight(das.df.eff) %>% 
    mutate(included = (.data$Bft <= 5 & abs(.data$Angle) <= 78 & 
                         .data$SightStd & .data$OnEffort), 
           included = ifelse(is.na(.data$included), FALSE, .data$included))
  # select(!!siteinfo.include)
  
  
  
  # Make data frame with nSI and ANI columns, and join with segdata
  sp.codes <- sort(sp.codes)
  # TODO: check that sp.codes is an acceptable code?
  
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
  list(segdata = segdata, siteinfo = siteinfo, randpicks = randpicks)
} 
