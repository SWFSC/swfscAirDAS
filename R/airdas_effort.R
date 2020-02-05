#' Summarize aerial DAS effort
#'
#' Summarize aerial DAS data by effort segments
#' 
#' @param x \code{airdas_df} object; output from \code{\link{airdas_process}}, 
#'  or a data frame that can be coerced to a \code{airdas_df} object
#' @param sp.codes character; species code(s) to include in segdata
#' @param ... passed to TODO
#' 
#' @importFrom dplyr %>% between bind_cols filter full_join group_by summarise
#' @importFrom rlang !!
#' @importFrom swfscMisc distance
#' @importFrom utils read.csv write.csv
#' 
#' @details Chops processed AirDAS data into modeling segments (henceforth 'segments'), 
#'   and assigns sightings and related information (e.g., weather conditions) to each segment. 
#'   This function returns all relevant information for the effort segments and 
#'   associated sightings ('segdata' and 'siteinfo', respectively). 
#'   Before chopping, the AirDAS data is filtered for events (rows) where either
#'   the 'OnEffort' column is \code{TRUE} or the 'Event' column is "E" or "O". 
#'   In other words, the data is filtered for continuous effort sections (henceforth 'effort sections'), 
#'   where effort sections run from "T"/"R" to "E"/"O" events (inclusive). 
#' 
#'   Currently, the only available chopping method is chopping effort sections into 
#'   equal-length modeling segments of length \code{seg.km}, 
#'   and doing a weighted average of the conditions for the length of that segment. 
#'   See \code{\link{airdas_chop_equal}} for more details about the segment chopping.
#' 
#'   The function \code{\link[swfscMisc]{distance}}, \code{method = "vincenty"}, is used to
#'   calculate the distance (in km) between the lat/lon points of subsequent events.
#' 
#' @return List of three data frames: 
#'   \itemize{
#'     \item segdata: one row for every segment, and columns for information such as
#'       unique segment number, start/end/midpoint coordinates, conditions (e.g. Beaufort), 
#'       and number of sightings and number of animals on that segment for evey species 
#'       indicated in \code{sp.codes}. 
#'     \item siteinfo: details for each sighting, including the segdata segment number it belongs on, 
#'       segment mid points (lat/lon), and whether 
#'       the sighting was included in the segdata segments (column \code{included}), 
#'       in addition to the other output information described in \code{\link{airdas_sight}}.
#'     \item randpicks: see \code{\link{airdas_chop_equal}}
#'   }
#' 
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' 
#' y.proc <- airdas_process(y)
#' airdas_effort(y.proc, sp.codes = c("mn", "bm"), seg.km = 3)
#' 
#' y.rand <- system.file("airdas_sample_randpicks.csv", package = "swfscAirDAS")
#' airdas_effort(
#'   y.proc, sp.codes = c("mn", "bm"), seg.km = 3, randpicks.load = y.rand
#' )
#' 
#' @export
airdas_effort <- function(x, ...) UseMethod("airdas_effort")


#' @name airdas_effort
#' @export
airdas_effort.data.frame <- function(x, ...) {
  airdas_effort(as_airdas_df(x),...)
}


#' @name airdas_effort
#' @export
airdas_effort.airdas_df <- function(x, sp.codes, ...) {
  #----------------------------------------------------------------------------
  # TODO: format.. checks
  das.df <- x

  stopifnot(
    sum(is.na(das.df$Lat)) == 0,
    sum(is.na(das.df$Lon)) == 0, 
    sum(das.df$Event == "#") == 0
  )
  
  # seg.km <- suppressWarnings(as.numeric(seg.km))
  # if (!inherits(seg.km, c("numeric", "integer")))
  #   stop("seg.km must be a number")
  
  
  #----------------------------------------------------------------------------
  # Prep
  
  ### Filter for on effort and number continuous effort sections
  ###   'on effort + 1' is to capture O/E event
  das.oneff <- sort(unique(c(which(das.df$OnEffort), which(das.df$OnEffort) + 1)))
  stopifnot(all(between(das.oneff, 1, nrow(das.df))))
  
  # das.df <- das.df %>%
  #   mutate(cont_eff_section = cumsum(.data$Event %in% c("T", "R")))
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
  # ### Load randpicks file, if specified
  # if (is.null(randpicks.load)) {
  #   r.pos <- NULL
  #   
  # } else {
  #   randpicks.df <- read.csv(randpicks.load)
  #   if (all(c("effort_section", "randpicks") %in% names(randpicks.df))) {
  #     r.eff.sect <- randpicks.df$effort_section
  #     r.pos <- randpicks.df$randpicks
  #     
  #   } else {
  #     warning("For the provided randpicks CSV file, it is assumed that ", 
  #             "the first column is the continuous effort section numbers, ", 
  #             "and the second column is the randpick values for that ", 
  #             "continuous effort section")
  #     r.eff.sect <- randpicks.df[[1]]
  #     r.pos <- randpicks.df[[2]]
  #   }
  # }
  
  
  # ### Chop effort segments and get segdata info
  # eff.uniq <- unique(das.df$cont_eff_section)
  # # Check against provided randpicks
  # if (exists("r.eff.sect")) {
  #   if (length(eff.uniq) != length(r.eff.sect)) 
  #     stop("The provided AirDAS data (das.df) does not have the same number of ", 
  #          "continuous effort sections as the provided randpicks file has rows. ", 
  #          "Did you load the correct randpicks file, and does it have ", 
  #          "proper column names? See `?airdas_effort` for more details")
  # }
  
  # eff.list <- lapply(eff.uniq, function(i, das.df, seg.km, r.pos) {
  #   das.curr <- filter(das.df, .data$cont_eff_section == i)
  #   # Get segment lengths
  #   y <- airdas_chop_equal(das.curr, seg.km, r.pos[i])
  #   y[[1]]$seg_idx <- paste0(i, "_", y[[1]]$effort_seg)
  #   
  #   # Summarize info by segment
  #   y.segdata <- airdas_effort_segdata(y[[1]], y[[2]], i)
  #   
  #   list(y[[1]], y[[2]], y[[3]], y.segdata)
  # }, das.df = das.df, seg.km = seg.km, r.pos = r.pos)
  
  eff.list <- airdas_chop_equal(das.df, ...)
  das.df.eff <- eff.list[[1]]
  segdata <- eff.list[[2]]
  randpicks <- eff.list[[3]]
  
  
  # #----------------------------------------------------------------------------
  # # Extract information about each segment
  # 
  # ### Randpicks; including writing to csv if specified
  # randpicks <- data.frame(
  #   effort_section = eff.uniq,
  #   randpicks = vapply(eff.list, function(j) j[[3]], 1)
  # )
  # if (!is.null(randpicks.save)) 
  #   write.csv(randpicks, file = randpicks.save, row.names = FALSE)
  # 
  # # ### Segment lengths (todo: remove)
  # # segdata.len <- lapply(eff.list, function(j) j[[2]])
  # # segdata.len <- lapply(segdata.len, round, 4)
  # 
  # ### Segdata
  # segdata <- data.frame(
  #   do.call(rbind, lapply(eff.list, function(i) i[[4]])), 
  #   stringsAsFactors = FALSE
  # ) %>%
  #   mutate(segnum = seq_along(.data$seg_idx), 
  #          dist = round(.data$dist, 4)) %>%
  #   select(.data$segnum, .data$seg_idx, everything())
  # 
  # ### Each das data point, along with segnum
  # das.df.eff <- data.frame(
  #   do.call(rbind, lapply(eff.list, function(i) i[[1]])), 
  #   stringsAsFactors = FALSE
  # ) %>% 
  #   left_join(segdata[, c("seg_idx", "segnum")], by = "seg_idx")
  
  
  #----------------------------------------------------------------------------
  # Summarize sightings (based on siteinfo) and add applicable data to segdata
  siteinfo <- airdas_sight(das.df.eff) %>% 
    mutate(included = (.data$Bft <= 5 & abs(.data$Angle) <= 78 & 
                         .data$SightStd & .data$OnEffort), 
           included = ifelse(is.na(.data$included), FALSE, .data$included))
  
  
  # Make data frame with nSI and ANI columns, and join it with segdata
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
  # Return list
  list(segdata = segdata, siteinfo = siteinfo, randpicks = randpicks)
} 
