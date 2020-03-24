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
#'   \item There is an O event between each T event, and vice versa, i.e. 
#'     the data does not sart a new transect while still on a transect and
#'     does not end a transect when already not on a transect
#'   \item A T or R event does not occur while already on effort
#'   \item An E event does not occur while already off effort
#'   \item When the file ends, the data must be off effort and not still on a transect 
#'     (i.e. an O event must occurred more recently than a T event)
#'   \item All Data# columns for non-C events are right-justified
#'   \item The following events have NA (blank) Data# columns: *, R, E, O
#'   \item Event/column pairs meet the following requirements:
#' }
#'
#' \tabular{ll}{
#'   \emph{Item} \tab \emph{Requirement}\cr
#'   Viewing conditions \tab Must be one of: e, g, p, o, or NA (blank). Not case sensitive\cr
#'   Altitude           \tab Can be converted to a numeric value, and is not NA\cr
#'   Speed              \tab Can be converted to a numeric value, and is not NA\cr
#'   HKR              \tab Characters must all be one of: n, h, k, r, or NA (blank). 
#'     Not case sensitive; y is also accepted for PHOCOENA data\cr
#'   Percent overcast \tab Can be converted to a whole number between 0 and 100\cr
#'   Beaufort         \tab Can be converted to a whole number between 0 and 9\cr
#'   Jellyfish        \tab Must be one of 0, 1, 2, 3, or NA (blank)\cr
#'   Horizontal sun   \tab Must be one of 0:12, or NA (blank)\cr
#'   Vertical sun     \tab Must be one of 0:4 or NA (blank)\cr
#'   Observers        \tab Each entry must be two characters, and no observer code can be used twice in the same P event\cr
#'   Sighting (mammal) \tab Angle is a whole number between -90 and 90\cr
#'   Sighting (mammal) \tab Group size is a whole number between 1 and 5000\cr
#'   Sighting (mammal) \tab Species code has exactly two characters\cr
#'   Sighting (mammal) \tab Observer code has exactly two characters\cr
#'   Sighting info     \tab Species percentages can be converted to a numeric value, and sum to 100\cr
#'   Sighting info     \tab Unused columns (DateTime, Lat, Lon, and Data1-4) of a '1' event must be NA (blank)\cr
#'   Resight           \tab Angle can be converted to a numeric value\cr
#'   Resight           \tab Unused resight columns must be NA (blank)\cr
#'   Turtle sighting   \tab Angle is a whole number between -90 and 90\cr
#'   Turtle sighting   \tab Group size is a whole number between 1 and 10 (only included in CARETTA data)\cr
#'   Turtle sighting   \tab Species code has exactly two characters, and is one of dc, cc, lo, cm, uh, ut, or lv\cr
#'   Turtle sighting   \tab Observer code has exactly two characters\cr
#'   Turtle sighting   \tab Turtle size is a whole number between 1 and 9; one of s, m, l is also accepted for CARETTA data\cr
#'   Turtle sighting   \tab Travel direction is a whole number between 0 and 360 (only included in TURTLE data)\cr
#'   Turtle sighting   \tab Tail visible must be one of y, n, u, or NA (blank). Case sensitive\cr
#'   Turtle sighting   \tab In TURTLE data, the Data7 column must be NA (blank)\cr
#' }
#'
#' Outstanding questions/todo:
#' \itemize{
#'   \item For which conditions should NA not be allowed?
#'   \item For which sighting data bits should NAs not be allowed (e.g. Obs, angle, sp, Gs)?
#'   \item Check for valid fish ball/mola/jelly/crab pot codes in comments?
#'   \item Do checks for transect number?
#'   \item todo: Check that sighting observer is one of currently listed observers
#'   \item todo: Check that sighting observer side matches angle +/-
#'   \item todo: some way to load spcodes.dat to check species codes?
#'   \item todo: Check for equal number of non-NA species codes and percentages (including 1 sp code for no 1 event)
#' }
#'
#' @return 
#' A data frame with five columns that list information about errors found 
#' in the AirDAS files: the file name, line number, 
#' index (row number) from the \code{airdas_read(file)} data frame, 
#' 'ID' (pre-Data# columns from the DAS file), and description of the issue. 
#' This data frame is sorted by the 'Description' column. 
#' If there are multiple issues with the same line, the issue descriptions
#' are concatenated together using \code{paste(..., collapse = "; ")}
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
  x$idx <- seq_along(x$Event)
  x <- as_airdas_dfr(x)
  
  x.proc <- suppressWarnings(airdas_process(x)) %>% 
    left_join(select(x, line_num, idx), by = "line_num")
  x.proc <- as_airdas_df(x.proc)
  
  id.lines.idx <- switch(file.type, caretta = 39, phocoena = 45, turtle = 39)
  x.lines <- do.call(
    c, lapply(file, function(i) substr(readLines(i), 1, id.lines.idx))
  )
  if (skip > 0) x.lines <- x.lines[-c(1:skip)]
  
  stopifnot(nrow(x) == length(x.lines))
  
  
  #----------------------------------------------------------------------------
  ### Check event codes
  event.acc <- c("#", "*", 1, "A", "C", "E", "O", 
                 "P", "R", "s", "S", "t", "T", "V", "W")
  ev.which <- which(!(x$Event %in% event.acc))
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, ev.which, "The event code is not recognized")
  )
  
  
  #----------------------------------------------------------------------------
  ### Check that effort dot matches effort determined by T/R to E/O events
  ###   PHOCOENA data has no effort dots
  if (file.type != "phocoena") {
    x.proc.no1 <- x.proc[x.proc$Event != 1, ] #1 events do not have effort dots
    edot.which <- x.proc.no1$idx[(x.proc.no1$OnEffort != x.proc.no1$EffortDot)]
    
    error.out <- rbind(
      error.out,
      .check_list(x, x.lines, edot.which, 
                  "Effort dot does not match T/R to O/E effort")
    )
    rm(x.proc.no1)
  }
  
  
  #----------------------------------------------------------------------------
  ### Check that: 
  ###   1) there are not consecutive T or O events
  x.trans <- x[x$Event %in% c("T", "O"), ]
  x.trans.str <- paste(x.trans$Event, collapse = "")
  d.t <- gregexpr("TT", x.trans.str)[[1]]
  d.o <- gregexpr("OO", x.trans.str)[[1]]
  
  t.which <- x.trans$idx[d.t[d.t != -1] + 1]
  o.which <- x.trans$idx[d.o[d.o != -1] + 1]
  
  ### 2) data is OFF effort before a T or R event
  # idx.tr <- which(x$Event %in% c("T", "R"))
  # x.all.preTR <- x.all[idx.tr-1, ]
  # tr.which <- x.all.preTR$idx[x.all.preTR$OnEffort] + 1
  idx.proc.tr <- which(x.proc$Event %in% c("T", "R"))
  x.proc.preTR <- x.proc[idx.proc.tr - 1, ]
  tr.which <- x.proc.preTR$idx[x.proc.preTR$OnEffort] + 1
  
  ### 3) data is ON effort before an E event (O event after E is ok)
  idx.proc.e <- which(x.proc$Event == "E")
  x.proc.preE <- x.proc[idx.proc.e - 1, ]
  e.which <- x.proc.preE$idx[!x.proc.preE$OnEffort] + 1
  
  rm(x.proc.preTR, x.proc.preE)
  
  
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, t.which, 
                paste("Duplicate T event: there is no O event between", 
                      "this T event and the previous T event")), 
    .check_list(x, x.lines, o.which, 
                paste("Duplicate O event: there is no T event between", 
                      "this O event and the previous O event")), 
    .check_list(x, x.lines, tr.which, 
                "Attempt to resume effort (T/R event) when already on effort"),
    .check_list(x, x.lines, e.which, 
                "Attempt to end effort (E event) when already off effort")
  )
  
  
  #----------------------------------------------------------------------------
  ### Check that file ends off effort, and after an "O" rather than "E" event
  idx.proc.o <- which(x.proc$Event == "O")
  if (length(idx.proc.e) == 0) idx.proc.e <- 0 # In case file has no E events
  if (tail(x.proc, 1)$OnEffort | (tail(idx.proc.o, 1) < tail(idx.proc.e, 1))) {
    end.which <- nrow(x)
    
    error.out <- rbind(
      error.out,
      .check_list(x, x.lines, end.which, 
                  paste("The file ends on effort, and/or the", 
                        "last off effort event is an E rather than O"))
    )
  }
  
  
  #----------------------------------------------------------------------------
  ### Check that Data# columns are right-justifed for selected events
  event.tofilt <- c("C", "*", "R", "E", "O", "#")
  x.filt <- x %>% 
    filter(!(.data$Event %in% event.tofilt))
  
  call.read <- switch(
    file.type, phocoena = .airdas_read_phocoena, survey = .airdas_read_survey,
    caretta = .airdas_read_turtle, turtle = .airdas_read_turtle
  )
  
  x.tmp.filt.data <- do.call(
    rbind, 
    lapply(file, function(i) {
      call.read(i, skip = skip, tz = "UTC", file.type = file.type)
    })
  ) %>% 
    select(-.data$DateTime) %>% 
    filter(!(.data$Event %in% event.tofilt)) %>% 
    select(starts_with("Data")) %>% 
    mutate(Data7 = substr(.data$Data7, 1, 5))
  
  x.tmp.which <- lapply(1:7, function(i) {
    x1 <- trimws(x.tmp.filt.data[[i]], which = "left")
    x2 <- trimws(x.tmp.filt.data[[i]], which = "both")
    which(x1 != x2)
  })
  
  r.which <- x.filt$idx[sort(unique(unlist(x.tmp.which)))]
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, r.which, "Data column(s) are not right-justified")
  )
  
  rm(event.tofilt, x.filt, call.read, x.tmp.filt.data, x.tmp.which)
  
  
  #----------------------------------------------------------------------------
  ### Check that value type of values in Data# columns are as expected for
  ###   columns that are added in das_process
  ###   Note that for fields with specific number requirements, 
  ###   a separate check_numeric() is not necessary
  
  # "*", R, E, and O events have no data
  idx.na <- .check_character(x, c("*", "R", "E", "O"), paste0("Data", 1:7), NA)
  txt.na <- "R, E, O, and * events should have no data in the Data# columns"
  
  
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
  patt <- c("h", "k", "r", "n")
  if (file.type == "phocoena") patt <- c(patt, "y") #y means h in early years
  x.tmp <- x
  x.tmp$Data1 <- .gsub_multi(c(patt, toupper(patt)), "", x.tmp$Data1)
  
  idx.W.1 <- .check_character(as_airdas_dfr(x.tmp), "W", "Data1", c("", NA))
  txt.W.1 <- paste(
    "HKR (Data1 of W events) has a character that is not one of:", 
    "n, h, k, r, or NA (or y for PHOCOENA data)"
  )
  rm(x.tmp, patt)
  
  # Percent overcast; no check_numeric() needed
  cc.num <- c(as.character(0:100), sprintf("%02d", 0:100))
  idx.cc <- .check_character(x, "W", "Data2", c(cc.num, NA))
  txt.cc <- "Percent overcast (Data2 of W events) must be a whole number between 0 and 100"
  rm(cc.num)
  
  # Beaufort; no check_numeric() needed
  idx.bft <- .check_character(x, "W", "Data3", c(0:9, NA))
  txt.bft <- "Beaufort (Data3 of W events) must be a whole number between 0 and 9"
  
  # Jellyfish
  txt.jelly <- "Jellyfish code (Data4 of W events) is not one of 0, 1, 2, 3, or NA"
  idx.jelly <- if (file.type == "phocoena") {
    integer(0)
  } else {
    .check_character(x, "W", "Data4", c(0, 1, 2, 3, NA))
  }
  
  # Horizontal sun
  data.hsun <- switch(file.type, caretta = 5, phocoena = 4, turtle = 5)
  acc.hsun <- c(0:12, sprintf("0%d", 0:12), NA)
  
  idx.hsun <- .check_character(x, "W", paste0("Data", data.hsun), acc.hsun)
  txt.hsun <- paste("Horizontal sun is not one of", 
                    paste(c(0:12, NA), collapse = ", "))
  
  # Vertical sun
  txt.vsun <- paste("Vertical sun (Data5 of W events) is not one of", 
                    paste(c(0:4, NA), collapse = ", "))
  idx.vsun <- if (file.type == "phocoena") {
    .check_character(x, "W", "Data5", c(0:4, sprintf("0%d", 0:4), NA))
  } else {
    integer(0)
  }
  
  # Observers
  idx.P <- .check_character_length(x, "P", c("Data1", "Data2", "Data3", "Data4"), 2)
  txt.P <- "An observer entry (Data1-4 of P events) is not two characters"
  
  x.p <- x %>% mutate(idx = seq_along(.data$Event)) %>% filter(.data$Event == "P")
  x.p.data <- select(x.p, .data$Data1, .data$Data2, .data$Data3, .data$Data4)
  x.p.which <- apply(x.p.data, 1, function(i) any(duplicated(na.omit(i))))
  
  idx.obs.dup <- x.p$idx[x.p.which]
  txt.obs.dup <- "One or more observer entries (Data1-4 of P events) are duplicated"
  rm(x.p, x.p.data, x.p.which)
  
  
  # Add text to error.out as needed and return
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, idx.na, txt.na),
    .check_list(x, x.lines, idx.V, txt.V),
    .check_list(x, x.lines, idx.A.1, txt.A.1),
    .check_list(x, x.lines, idx.A.2, txt.A.2),
    .check_list(x, x.lines, idx.A.nona, txt.A.nona),
    .check_list(x, x.lines, idx.W.1, txt.W.1),
    .check_list(x, x.lines, idx.cc, txt.cc),
    .check_list(x, x.lines, idx.bft, txt.bft),
    .check_list(x, x.lines, idx.jelly, txt.jelly),
    .check_list(x, x.lines, idx.hsun, txt.hsun),
    .check_list(x, x.lines, idx.vsun, txt.vsun),
    .check_list(x, x.lines, idx.P, txt.P), 
    .check_list(x, x.lines, idx.obs.dup, txt.obs.dup)
  )
  
  
  #----------------------------------------------------------------------------
  ### Check Data# columns for sightings data format
  ### Phocoena data has no 1/s/t event codes, so can just leave those in
  
  ### Marine mammal sightings (SKM)
  # Angle
  data.skm.ang <- switch(file.type, caretta = 3, phocoena = 4, turtle = 3)
  ang.acc <- c(-90:90, sprintf("%03d", -9:-1), sprintf("%02d", 0:9), NA)
  idx.S.ang <- .check_character(x, "S", paste0("Data", data.skm.ang), ang.acc)
  txt.S.ang <- "Angle must be a whole number between -90 and 90"
  
  # Group size
  data.skm.gs <- switch(file.type, caretta = 4, phocoena = 3, turtle = 4)
  gs.acc <- c(1:5000, sprintf("%02d", 1:5000), NA)
  idx.S.gs <- .check_character(x, "S", paste0("Data", data.skm.gs), gs.acc)
  txt.S.gs <- "Group size must be a whole number between 1 and 5000"
  
  # Species code(s)
  data.skm.sp <- switch(file.type, caretta = 5:7, phocoena = 2, turtle = 5:7)
  idx.S.sp <- .check_character_length(x, "S", paste0("Data", data.skm.sp), 2)
  txt.S.sp <- "A species code is not exactly two characters"
  
  # Observer
  data.skm.obs <- switch(file.type, caretta = 2, phocoena = 5, turtle = 2)
  idx.S.obs <- .check_character_length(x, "S", paste0("Data", data.skm.obs), 2)
  txt.S.obs <- "The sighting observer code is not exactly two characters"
  
  # # Observer - check that is one of entered observers
  # idx.obs.pos <- .check_sight_obs(x.proc, "S", paste0("Data", data.skm.obs))

  ### Multi-species info (1)  
  # TODO: equal number of percentages and species codes?
  # Percentages are numeric
  idx.1.num <- .check_numeric(x, "1", paste0("Data", 5:7))
  txt.1.num <- "A species percentage cannot be converted to a numeric"
  
  # If percentages are numeric, do they sum to 100?
  txt.1.sum <- "The species percentages do not sum to 100"
  if (length(idx.1.num) == 0) {
    x.1 <- x %>% mutate(idx = seq_along(.data$Event)) %>% filter(.data$Event == "1")
    x.1.data <- select(x.1, .data$Data5, .data$Data6, .data$Data7)
    x.1.which <- apply(x.1.data, 1, function(i) {
      !isTRUE(all.equal(100, sum(as.numeric(i), na.rm = TRUE)))
    })
    
    idx.1.sum <- x.1$idx[x.1.which]
    rm(x.1, x.1.data, x.1.which)
    
  } else {
    idx.1.sum <- integer(0)
  }
  
  # Are other 1 event columns NA
  idx.1.na <- .check_character(x, "1", paste0("Data", 1:4), c(NA))
  txt.1.na <- "A Data1-4 column(s) for a 1 event is not NA (blank)"
  
  idx.1.na2 <- .check_character(x, "1", c("DateTime", "Lat", "Lon"), c(NA))
  txt.1.na2 <- "One of DateTime, Lat, or Lon for a 1 event is not NA (blank)"
  
  ### Resights (s)
  idx.s.ang <- .check_character(x, "s", "Data2", ang.acc)
  txt.s.ang <- "Angle must be a whole number between -90 and 90"
  
  idx.s.na <- .check_character(x, "s", paste0("Data", 3:7), c(NA))
  txt.s.na <- "A Data3-7 column(s) for s events is not NA (blank)"
  
  
  # Add to error.out
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, idx.S.ang, txt.S.ang),
    .check_list(x, x.lines, idx.S.gs, txt.S.gs),
    .check_list(x, x.lines, idx.S.obs, txt.S.obs),
    .check_list(x, x.lines, idx.S.sp, txt.S.sp),
    .check_list(x, x.lines, idx.1.num, txt.1.num),
    .check_list(x, x.lines, idx.1.sum, txt.1.sum),
    .check_list(x, x.lines, idx.1.na, txt.1.na),
    .check_list(x, x.lines, idx.1.na2, txt.1.na2),
    .check_list(x, x.lines, idx.s.ang, txt.s.ang),
    .check_list(x, x.lines, idx.s.na, txt.s.na)
  )
  
  
  #----------------------------------------------------------------------------
  ### Turtle
  
  if (file.type != "phocoena") {
    # Angle
    data.t.ang <- switch(file.type, caretta = 3, turtle = 2)
    idx.t.ang <- .check_character(x, "t", paste0("Data", data.t.ang), ang.acc)
    txt.t.ang <- "Turtle angle must be a whole number between -90 and 90"
    
    # Group size
    idx.t.gs <- integer(0)
    txt.t.gs <- "Turtle group size must be a whole number between 1 and 10"
    
    if (file.type == "caretta") { #TURTLE doesn't have group size field
      data.t.gs <- 4
      idx.t.gs <- .check_character(x, "t", paste0("Data", data.t.gs), 1:10)
    }
    
    # Species code(s)
    data.t.sp <- switch(file.type, caretta = 5, turtle = 3)
    idx.t.sp <- .check_character_length(x, "t", paste0("Data", data.t.sp), 2)
    txt.t.sp <- "A turtle species code is not two characters"
    
    t.codes <- tolower(c("DC","CC","LO","CM","UH","UT", "LV"))
    idx.t.sp2 <- .check_character(x, "t", paste0("Data", data.t.sp), t.codes)
    txt.t.sp2 <- paste("Turtle species codes must be one of:", 
                       paste(t.codes, collapse = ", "))
    
    # Observer
    data.t.obs <- switch(file.type, caretta = 2, turtle = 1)
    idx.t.obs <- .check_character_length(x, "t", paste0("Data", data.t.obs), 2)
    txt.t.obs <- "The turtle sighting observer code is not two characters"
    
    # Turtle size
    idx.t.size <- idx.t.size.b <- integer(0)
    txt.t.size <- txt.t.size.b <- ""
    data.t.size <- switch(file.type, caretta = 6, turtle = 4)
    acc.size <- 1:9
    
    if (file.type == "caretta") {
      acc.size <- c(acc.size, "s", "m", "l")
      idx.t.size <- .check_character(x, "t", paste0("Data", data.t.size), acc.size)
      txt.t.size <- paste("Turtle size must be either a whole number between 1 and 9,", 
                          "or one of s, m, or l")
    } else if (file.type == "turtle") {
      idx.t.size <- .check_character(x, "t", paste0("Data", data.t.size), acc.size)
      txt.t.size <- "Turtle size must be a whole number between 1 and 9"
    }
    
    
    # Travel direction
    idx.t.dir <- integer(0)
    txt.t.dir <- "Turtle travel direction must be between 0 and 360"
    
    if (file.type == "turtle") { #CARETTA doesn't have travel direction field
      dir.acc <- 0:360
      idx.t.dir <- .check_character(x, "t", paste0("Data", 5), dir.acc)
    }
    
    # Tail visible
    data.t.tail <- switch(file.type, caretta = 7, turtle = 6)
    idx.t.tail <- .check_character(x, "t", paste0("Data", data.t.tail), c("y", "n", "u", NA))
    txt.t.tail <- "Turtle tail visible is not one of y, n, u, or NA"
    
    # NA check 
    idx.t.na <- integer(0)
    txt.t.na <- "The Data7 column for TURTLE t events is not NA (blank)"
    if (file.type == "turtle") {
      idx.t.na <- .check_character(x, "t", paste0("Data", 7), c(NA))
    }
    
    
    # Add to error.out
    error.out <- rbind(
      error.out,
      .check_list(x, x.lines, idx.t.ang, txt.t.ang),
      .check_list(x, x.lines, idx.t.gs, txt.t.gs),
      .check_list(x, x.lines, idx.t.sp, txt.t.sp),
      .check_list(x, x.lines, idx.t.sp2, txt.t.sp2),
      .check_list(x, x.lines, idx.t.obs, txt.t.obs),
      .check_list(x, x.lines, idx.t.size, txt.t.size),
      .check_list(x, x.lines, idx.t.dir, txt.t.dir),
      .check_list(x, x.lines, idx.t.tail, txt.t.tail),
      .check_list(x, x.lines, idx.t.na, txt.t.na)
    )
  }
  
  
  #----------------------------------------------------------------------------
  #----------------------------------------------------------------------------
  ### Remove first line and return
  if (nrow(error.out) == 1) {
    to.return <- data.frame(
      File = NA, LineNum = NA, Idx = NA, ID = NA, 
      Description = "No errors found", 
      stringsAsFactors = FALSE
    )
  } else {
    to.return <- error.out %>% 
      slice(-1) %>% 
      # group_by(File, LineNum, Idx, ID) %>% 
      # summarise(Description = paste(Description, collapse = "; ")) %>% 
      # ungroup() %>% 
      arrange(Description)
  }
  row.names(to.return) <- NULL
  
  if (!is.null(file.out)) write.csv(to.return, file = file.out)
  
  to.return
}



# Function to remove multiple characters from the same string
.gsub_multi <- function(pattern, replacement, x) {
  for (i in pattern) x <- gsub(i, replacement, x)
  x
}

