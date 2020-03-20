#' Check AirDAS file
#'
#' Check that AirDAS file has accepted formatting and values
#'
#' @param file filename(s) of one or more AirDAS files
#' @param file.type character; indicates the program used to create \code{file}.
#'   Must be one of: "turtle", "caretta", "survey", or "phocoena" (case sensitive).
#'   Default is "turtle". Passed to \code{\link{airdas_read}}
#' @param skip integer: see \code{\link[readr]{read_fwf}}. Default is 0. 
#'   Passed to \code{\link{airdas_read}}
#' @param file.out filename to which to write the error log; 
#'   default is \code{NULL}
#'
#' @details
#' Checks that the following is true:
#' \itemize{
#'   \item Event codes are one of the following: 
#'     #, *, 1, A, C, E, O, P, R, s, S, t, T, V, W
#'   \item The effort dot matches effort determined using T, R, O, and E events
#'   \item A T or R event does not occur while already on effort
#'   \item All Data# columns, for events other than C events, are right-justified
#'   \item All * events have NA (blank) Data# columns
#'   \item Event/column pairs meet the following requirements:
#' }
#'
#' \tabular{ll}{
#'   \emph{Item} \tab \emph{Requirement}\cr
#'   Viewing conditions \tab Must be one of: e, g, p, o, or NA (blank). Not case sensitive\cr
#'   Altitude           \tab Can be converted to a numeric value, and is not NA\cr
#'   Speed              \tab Can be converted to a numeric value, and is not NA\cr
#'   HKR              \tab Characters must all be one of: n, h, k, r, y, or NA (blank). Not case sensitive\cr
#'   Percent overcast \tab Can be converted to a numeric value, and is between 0 and 100\cr
#'   Beaufort         \tab Can be converted to a numeric value, and is between 0 and 9\cr
#'   Jellyfish        \tab Must be one of 0, 1, 2, 3, or NA (blank)\cr
#'   Horizontal sun   \tab Can be converted to a numeric value, and must be one of 0:12, or NA (blank)\cr
#'   Vertical sun     \tab Can be converted to a numeric value, and must be one of 0:4 or NA (blank)\cr
#'   Observers        \tab Each entry must be two characters\cr
#'   Observers        \tab No observer code can be used twice in the same P event\cr
#'   Sighting (mammal) \tab Angle and number of animals can be converted to a numeric value\cr
#'   Sighting (mammal) \tab Species code has exactly one or two characters\cr
#'   Sighting (mammal) \tab Observer code has exactly one or two characters\cr
#'   Sighting info     \tab Species percentages can be converted to a numeric value\cr
#'   Sighting info     \tab Unused columns of a '1' event must be NA (blank)\cr
#'   Resight           \tab Angle can be converted to a numeric value\cr
#'   Resight           \tab Unused resight columns must be NA (blank)\cr
#'   Turtle sighting   \tab Species code has exactly one or two characters\cr
#'   Turtle sighting   \tab Observer code has exactly one or two characters\cr
#'   Turtle sighting   \tab Angle and travel direction can be converted to a numeric value\cr
#'   Turtle sighting   \tab Tail visible must be one of y, n, u, or NA (blank). Case sensitive\cr
#'   Turtle sighting   \tab In TURTLE data, number of turtles can be converted to a numeric value\cr
#'   Turtle sighting   \tab In TURTLE data, turtle size can be converted to a numeric value\cr
#'   Turtle sighting   \tab In TURTLE data, the Data7 column must be NA (blank)\cr
#' }
#'
#' Outstanding questions:
#' \itemize{
#'   \item Are there supposed to be equal numbers of T/O and R/E events?
#'   \item For which conditions should NA not be allowed?
#'   \item For which sighting data bits should NA not be allowed?
#'   \item Check for valid fish ball/mola/jelly/crab pot codes in comments?
#'   \item Do checks for transect number?
#' }
#'
#' @return 
#' A data frame with five columns that list information about errors found 
#' in the AirDAS files: the file name, line number, 
#' index (row number) from the \code{airdas_read(file)} data frame, 
#' 'ID' (pre-Data# columns from the DAS file), and description of the issue
#'
#' If \code{file.out} is not \code{NULL}, then the error log is also
#' written to a text/csv file
#'
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' airdas_check(y)
#'
#' @export
airdas_check <- function(file, file.type = "turtle", skip = 0, file.out = NULL) {
  error.out <- data.frame(
    File = NA, LineNum = NA, Idx = NA, ID = NA, Description = NA,
    stringsAsFactors = FALSE
  )
  
  x <- suppressWarnings(airdas_read(file, file.type = file.type, skip = skip))
  x.proc <- suppressWarnings(airdas_process(x))
  
  id.idx <- switch(file.type, phocoena = 45, turtle = 39, 39)
  x.lines <- do.call(c, lapply(file, function(i) substr(readLines(i), 1, id.idx)))
  if (skip > 0) x.lines <- x.lines[-c(1:skip)]
  
  stopifnot(nrow(x) == length(x.lines))
  
  
  #----------------------------------------------------------------------------
  ### Check event codes
  event.acc <- c("#", "*", 1, "A", "C", "E", "O", 
                 "P", "R", "s", "S", "t", "T", "V", "W")
  ev.which <- which(!(x$Event %in% event.acc))
  error.out <- rbind(
    error.out,
    list(x$file_das[ev.which], x$line_num[ev.which], ev.which, 
         x.lines[ev.which],
         rep("The event code is not recognized", length(ev.which)))
  )
  
  
  #----------------------------------------------------------------------------
  ### Check that effort dot matches effort determined by T/R to E/O events
  x.all <- left_join(
    select(x, .data$Event, .data$file_das, .data$line_num, 
           .data$EffortDot), 
    select(x.proc, .data$Event, .data$file_das, .data$line_num, 
           .data$EffortDot, .data$OnEffort), 
    by = c("Event", "file_das", "line_num", "EffortDot")
  ) %>% 
    mutate(idx = seq_along(.data$Event))
  
  tmp.which <- which(is.na(x.all$OnEffort))
  stopifnot(
    nrow(x) == nrow(x.all), 
    all(x$Event[tmp.which] == "#")
  )
  x.all$OnEffort[tmp.which] <- x.all$OnEffort[tmp.which-1]
  
  # 1 events do not have effort dots
  e.which <- which((x.all$OnEffort != x.all$EffortDot) & (x.all$Event != 1))
  
  error.out <- rbind(
    error.out,
    list(x$file_das[e.which], x$line_num[e.which], e.which, x.lines[e.which], 
         rep("Effort dot does not match T/R to O/E effort", length(e.which)))
  )
  
  
  #----------------------------------------------------------------------------
  ### Check that data is OFF effort before a T or R event
  idx.tr <- which(x$Event %in% c("T", "R"))
  x.all.preTR <- x.all[idx.tr-1, ]
  tr.which <- x.all.preTR$idx[x.all.preTR$OnEffort] + 1

  ### Check that data is ON effort before a E or O event
  idx.eo <- which(x$Event %in% c("E", "O"))
  x.all.preEO <- x.all[idx.eo-1, ]
  eo.which <- x.all.preEO$idx[!x.all.preEO$OnEffort] + 1
  
  rm(x.all.preTR, x.all.preEO)
  
  error.out <- rbind(
    error.out,
    list(x$file_das[tr.which], x$line_num[tr.which], tr.which, x.lines[tr.which], 
         rep("Attempt to resume effort when already on effort", length(tr.which)))#, 
    # list(x$file_das[eo.which], x$line_num[eo.which], eo.which, x.lines[eo.which], 
    #      rep("Attempt to end effort when already off effort", length(eo.which)))
  )
  
  #----------------------------------------------------------------------------
  ### Check that Data# columns are right-justifed for non-C events
  x.noc <- x %>% 
    mutate(idx = seq_along(.data$Event)) %>% 
    filter(!(.data$Event %in% c("C", "*")))
  
  call.read <- switch(
    file.type, phocoena = .airdas_read_phocoena, survey = .airdas_read_survey,
    caretta = .airdas_read_turtle, turtle = .airdas_read_turtle
  )
  
  x.tmp.noc.data <- do.call(
    rbind, 
    lapply(file, function(i) {
      call.read(i, skip = skip, tz = "UTC", file.type = file.type)
    })
  ) %>% 
    select(-.data$DateTime) %>% 
    filter(!(.data$Event %in% c("C", "*"))) %>% 
    select(starts_with("Data"))
  
  x.tmp.which <- lapply(1:7, function(i) {
    x1 <- trimws(x.tmp.noc.data[[i]], which = "left")
    x2 <- trimws(x.tmp.noc.data[[i]], which = "both")
    which(x1 != x2)
  })
  
  r.which <- x.noc$idx[sort(unique(unlist(x.tmp.which)))]
  error.out <- rbind(
    error.out,
    list(x$file_das[r.which], x$line_num[r.which], r.which, x.lines[r.which], 
         rep("Data column(s) are not right-justified", length(r.which)))
  )
  
  rm(x.noc, call.read, x.tmp.noc.data, x.tmp.which)
  
  #----------------------------------------------------------------------------
  ### Check that value type of values in Data# columns are as expected for
  ###   columns that are added in das_process
  
  # Variables are code named as "z".event code'.'data# column' for
  #   line numbers with weird info,
  #   and "txt".event code'.'data# column' for the txt to go in error.out
  
  # "*" events have no data
  idx.star <- .check_character(x, "*", paste0("Data", 1:7), NA)
  txt.star <- "Position (*) events should have no data in the Data# columns"
  
  
  # Viewing conditions
  patt <- c("e", "g", "o", "p")
  idx.V <- .check_character(x, "V", paste0("Data", 1:5), c(patt, toupper(patt), NA))
  txt.V <- "A viewing condition (Data1-5 of V events) is not one of: e, g, p, o, or NA"
  rm(patt)
  
  # Altitude and altitude
  idx.A.1 <- .check_numeric(x, "A", "Data1")
  txt.A.1 <- "Altitude (Data1 of A events) cannot be converted to a numeric"
  
  idx.A.2 <- .check_numeric(x, "A", "Data2")
  txt.A.2 <- "Speed (Data2 of A events) cannot be converted to a numeric"
  
  idx.A.nona <- .check_nona(x, "A", c("Data1", "Data2"))
  txt.A.nona <- "Altitude and speed (Data1-2 of A events) must not be NA"
  
  # HKR
  x.tmp <- x
  patt <- c("h", "k", "r", "n", "y") #y means h in early years
  x.tmp$Data1 <- .gsub_multi(c(patt, toupper(patt)), "", x.tmp$Data1)
  
  idx.W.1 <- .check_character(as_airdas_dfr(x.tmp), "W", "Data1", c("", NA))
  txt.W.1 <- "HKR (Data1 of W events) has a character that is not one of: n, h, k, r, y, or NA"
  rm(x.tmp, patt)
  
  # Percent overcast
  idx.cc1 <- .check_numeric(x, "W", "Data2")
  txt.cc1 <- "Percent overcast (Data2 of W events) cannot be converted to a numeric"
  
  cc.num <- c(as.character(0:100), sprintf("%02d", 0:100))
  idx.cc2 <- .check_character(x, "W", "Data2", c(cc.num, NA))
  txt.cc2 <- "Percent overcast (Data2 of W events) must be a whole number between 0 and 100"
  rm(cc.num)
  
  # Beaufort
  idx.bft1 <- .check_numeric(x, "W", "Data3")
  txt.bft1 <- "Beaufort (Data3 of W events) cannot be converted to a numeric"
  
  idx.bft2 <- .check_character(x, "W", "Data3", c(0:9, NA))
  txt.bft2 <- "Beaufort (Data3 of W events) must be a whole number between"
  
  # Jellyfish
  txt.jelly <- "Jellyfish code (Data4 of W events) is not one of 0, 1, 2, 3, or NA"
  idx.jelly <- if (file.type == "turtle") {
    .check_character(x, "W", "Data4", c(0, 1, 2, 3, NA))
  } else {
    integer(0)
  }
  
  # Horizontal sun
  if (file.type == "phocoena") {
    idx.hsun <- .check_character(x, "W", "Data4", c(0:12, sprintf("0%d", 0:12), NA))
    idx.hsun.b <- .check_numeric(x, "W", "Data4")
    txt.hsun <- paste("Horizontal sun (Data4 of W events) is not one of", 
                      paste(c(0:12, NA), collapse = ", "))
    txt.hsun.b <- "Horizontal sun (Data4 of W events) cannot be converted to a numeric"
    
  } else if (file.type == "turtle") {
    idx.hsun <- .check_character(x, "W", "Data5", c(0:12, sprintf("0%d", 0:12), NA))
    idx.hsun.b <- .check_numeric(x, "W", "Data5")
    txt.hsun <- paste("Horizontal sun (Data5 of W events) is not one of", 
                      paste(c(0:12, NA), collapse = ", "))
    txt.hsun.b <- "Horizontal sun (Data5 of W events) cannot be converted to a numeric"
    
  } else {
    idx.hsun <- idx.hsun.b <- integer(0)
  }
  
  # Vertical sun
  txt.vsun <- paste("Vertical sun (Data5 of W events) is not one of", 
                    paste(c(0:4, NA), collapse = ", "))
  txt.vsun.b <- "Vertical sun (Data5 of W events) cannot be converted to a numeric"
  if (file.type == "phocoena") {
    idx.vsun <- .check_character(x, "W", "Data5", c(0:4, sprintf("0%d", 0:4), NA))
    idx.vsun.b <- .check_numeric(x, "W", "Data5")
  } else {
    idx.vsun <- idx.vsun.b <- integer(0)
  }
  
  # Observers
  idx.P <- .check_character_length(x, "P", c("Data1", "Data2", "Data3", "Data4"), 2)
  txt.P <- "An observer entry (Data1-4 of P events) is not two characters"
  
  x.p <- x %>% mutate(idx = seq_along(.data$Event)) %>% filter(.data$Event == "P")
  x.p.data <- select(x.p, Data1, Data2, Data3, Data4)
  x.p.which <- apply(x.p.data, 1, function(i) any(duplicated(na.omit(i))))
  
  idx.obs.dup <- x.p$idx[x.p.which]
  txt.obs.dup <- "One or more observer entries (Data1-4 of P events) are duplicated"
  rm(x.p, x.p.data, x.p.which)
  
  
  # Add text to error.out as needed and return
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, idx.star, txt.star),
    .check_list(x, x.lines, idx.V, txt.V),
    .check_list(x, x.lines, idx.A.1, txt.A.1),
    .check_list(x, x.lines, idx.A.2, txt.A.2),
    .check_list(x, x.lines, idx.A.nona, txt.A.nona),
    .check_list(x, x.lines, idx.W.1, txt.W.1),
    .check_list(x, x.lines, idx.cc1, txt.cc1),
    .check_list(x, x.lines, idx.cc2, txt.cc2),
    .check_list(x, x.lines, idx.bft1, txt.bft1),
    .check_list(x, x.lines, idx.bft2, txt.bft2),
    .check_list(x, x.lines, idx.jelly, txt.jelly),
    .check_list(x, x.lines, idx.hsun, txt.hsun),
    .check_list(x, x.lines, idx.hsun.b, txt.hsun.b),
    .check_list(x, x.lines, idx.vsun, txt.vsun),
    .check_list(x, x.lines, idx.vsun.b, txt.vsun.b),
    .check_list(x, x.lines, idx.P, txt.P), 
    .check_list(x, x.lines, idx.obs.dup, txt.obs.dup)
  )
  
  
  #----------------------------------------------------------------------------
  ### Check Data# columns for sightings data format
  ### Phocoena data has no 1/s/t event codes, so can just leave those in
  
  # Marine mammal sightings (SKM)
  idx.S.num <- .check_numeric(x, "S", paste0("Data", 3:4))
  txt.S.num <- paste(
    "A Data3-4 column(s) for S events cannot be converted to a numeric"
  )
  
  if (file.type == "phocoena") {
    idx.S.sp <- .check_character_length(x, "S", "Data2", 1:2)
    txt.S.sp <- "A species code (Data2 of S events) is not one or two characters"
  } else {
    idx.S.sp <- .check_character_length(x, "S", paste0("Data", 5:7), 1:2)
    txt.S.sp <- "A species code (Data5, Data6, and Data7 of S events) is not one or two characters"
  }
  
  if (file.type == "phocoena") {
    idx.S.obs <- .check_character_length(x, "S", "Data5", 1:2)
    txt.S.obs <- "The sighting observer code (Data5 of S events) is not one or two characters"
  } else {
    idx.S.obs <- .check_character_length(x, "S", "Data2", 1:2)
    txt.S.obs <- "The sighting observer code (Data2 of S events) is not one or two characters"
  }
  
  # Multi-species info (1)
  idx.1.num <- .check_numeric(x, "1", paste0("Data", 5:7)) #3:9
  txt.1.num <- paste(
    "A Data5-7 column(s) for a 1 event cannot be converted to a numeric"
  )
  
  idx.1.na <- .check_character(x, "1", paste0("Data", 1:4), c(NA))
  txt.1.na <- "A Data1-4 column(s) for a 1 event is not NA (blank)"
  
  # Resights (s)
  idx.s.num <- .check_numeric(x, "s", "Data2")
  txt.s.num <- paste(
    "The Data2 column for s events cannot be converted to a numeric"
  )
  
  idx.s.na <- .check_character(x, "s", paste0("Data", 3:7), c(NA))
  txt.s.na <- "A Data3-7 column(s) for s events is not NA (blank)"
  
  # Turtle
  if (file.type == "turtle") {
    # Turtle - TURTLE
    idx.t.obs <- .check_character_length(x, "t", "Data1", 1:2)
    txt.t.obs <- "The turtle sighting observer code (Data1 of t events) is not one or two characters"
    
    idx.t.sp <- .check_character_length(x, "t", "Data3", 2)
    txt.t.sp <- "The turtle species code (Data3 of t events) is not two characters"
    
    idx.t.num <- .check_numeric(x, "t", paste0("Data", c(2, 4, 5)))
    txt.t.num <- paste(
      "A Data2/4-5 column(s) for t events cannot be converted to a numeric"
    )
    
    idx.t.tail <- .check_character(x, "t", "Data6", c("y", "n", "u", NA))
    txt.t.tail <- paste(
      "Turtle tail visible (Data6 of t events) is not one of y, n, u, or NA"
    )
    
    idx.t.na <- .check_character(x, "t", paste0("Data", 7), c(NA))
    txt.t.na <- "The Data7 column for t events is not NA (blank)"
    
  } else {
    # Turtle - CARETTA
    # Not else if so that all is set to integer(0) for phocoena
    idx.t.obs <- .check_character_length(x, "t", "Data2", 1:2)
    txt.t.obs <- "The turtle sighting observer code (Data2 of t events) is not one or two characters"
    
    idx.t.sp <- .check_character_length(x, "t", "Data5", 2)
    txt.t.sp <- "The turtle species code (Data5 of t events) is not two characters"
    
    idx.t.num <- .check_numeric(x, "t", paste0("Data", c(3, 4)))
    txt.t.num <- paste(
      "A Data3-4 column(s) for t events cannot be converted to a numeric"
    )
    
    idx.t.tail <- .check_character(x, "t", "Data7", c("y", "n", "u", NA))
    txt.t.tail <- paste(
      "Turtle tail visible (Data7 of t events) is not one of y, n, u, or NA"
    )
    
    idx.t.na <- integer(0)
    txt.t.na <- ""
  }
  
  
  # Add to error.out
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, idx.S.num, txt.S.num),
    .check_list(x, x.lines, idx.S.obs, txt.S.obs),
    .check_list(x, x.lines, idx.S.sp, txt.S.sp),
    .check_list(x, x.lines, idx.1.num, txt.1.num),
    .check_list(x, x.lines, idx.1.na, txt.1.na),
    .check_list(x, x.lines, idx.s.num, txt.s.num),
    .check_list(x, x.lines, idx.s.na, txt.s.na),
    .check_list(x, x.lines, idx.t.obs, txt.t.obs),
    .check_list(x, x.lines, idx.t.sp, txt.t.sp),
    .check_list(x, x.lines, idx.t.num, txt.t.num),
    .check_list(x, x.lines, idx.t.tail, txt.t.tail),
    .check_list(x, x.lines, idx.t.na, txt.t.na)
  )
  
  
  #----------------------------------------------------------------------------
  # Remove first line and return
  if (nrow(error.out) == 1) {
    to.return <- data.frame(
      File = NA, LineNum = NA, Idx = NA, ID = NA, 
      Description = "No errors found", 
      stringsAsFactors = FALSE
    )
  } else {
    to.return <- error.out[-1, ]
  }
  row.names(to.return) <- seq_len(nrow(to.return))
  
  if (!is.null(file.out)) write.csv(to.return, file = file.out)
  
  to.return
}



# Function to remove multiple characters from the same string
.gsub_multi <- function(pattern, replacement, x) {
  for (i in pattern) x <- gsub(i, replacement, x)
  x
}

