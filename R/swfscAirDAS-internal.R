# Internal helper functions for swfscAirDAS

###############################################################################
# Helper functions for airdas_process
airdas_process_help_num <- function(das.df, col.name, event.curr, event.na) {
  ifelse(
    is.na(as.numeric(das.df[event.curr, col.name])),
    event.na, as.numeric(das.df[event.curr, col.name])
  )
}

airdas_process_help_chr <- function(das.df, col.name, event.curr, event.na) {
  ifelse(
    is.na(das.df[event.curr, col.name]),
    event.na, das.df[event.curr, col.name]
  )
}

###############################################################################


#############################################################################
# ### Read and briefly process data
# airdas_read <- function(file) {
#   stopifnot(inherits(file, "character"))
#   
#   DAS <- readLines(file)
#   Event <- substr(DAS, 4, 4)
#   Event.num <- substr(DAS, 1, 3)
#   nDAS <- length(DAS)
#   EffortDot <- (substr(DAS, 5, 5) == ".")
#   
#   ### Format times and dates
#   time.txt <- gsub(" ", "", substr(DAS, 6, 11))
#   date.txt <- gsub(" ", "", substr(DAS, 13, 18))
#   
#   DateTime <- strptime(paste(date.txt, time.txt), "%m%d%y %H%M%S")
#   
#   if (!all(names(table(Event[is.na(DateTime)])) %in% c("*", "#", "?", "C", 1:8))) {
#     warning("There are DateTime NAs for unexpected events, meaning for ",
#             "events that are not one of ",
#             paste(c("*", "#", "?", "C", 1), collapse = ", "), "\n",
#             "Are all dates 'mmddyy' and times 'hhmmss'?")
#   }
#   
#   ### Get lat/long info
#   LatD <- as.numeric(substr(DAS, 21, 22))
#   LatM <- as.numeric(substr(DAS, 24, 28))
#   LonD <- as.numeric(substr(DAS, 31, 33))
#   LonM <- as.numeric(substr(DAS, 35, 39))
#   Lat <- (LatD + LatM/60) * ifelse(substr(DAS, 20, 20) == "S", -1, 1)
#   Lon <- (LonD + LonM/60) * ifelse(substr(DAS, 30, 30) == "W", -1, 1)
#   rm(LatD, LatM, LonD, LonM)
#   
#   if (!(all.equal(which(is.na(Lon)), which(is.na(Lat))) == TRUE)) {
#     warning("The Lon and Lat columns have NA values for different rows")
#   }
#   
#   ### Get data (i.e. field) info
#   Data1 <- gsub(" ", "", substr(DAS, 40, 44))
#   Data1[Data1 == ""] <- NA
#   Data2 <- gsub(" ", "", substr(DAS, 45, 49))
#   Data2[Data2 == ""] <- NA
#   Data3 <- gsub(" ", "", substr(DAS, 50, 54))
#   Data3[Data3 == ""] <- NA
#   Data4 <- gsub(" ", "", substr(DAS, 55, 59))
#   Data4[Data4 == ""] <- NA
#   Data5 <- gsub(" ", "", substr(DAS, 60, 64))
#   Data5[Data5 == ""] <- NA
#   Data6 <- gsub(" ", "", substr(DAS, 65, 69))
#   Data6[Data6 == ""] <- NA
#   Data7 <- gsub(" ", "", substr(DAS, 70, max(vapply(DAS, nchar, 1))))
#   Data7[Data7 == ""] <- NA
#   
#   das.data <- data.frame(
#     Event, EffortDot, DateTime, Lat, Lon,
#     Data1, Data2, Data3, Data4, Data5, Data6, Data7,
#     file_das = tail(strsplit(file, "/")[[1]], 1), event_num = Event.num,
#     line_num = 1:nDAS,
#     stringsAsFactors = FALSE
#   )
#   
#   ### Data check that no unexpected events have NA data
#   event.num <- is.na(suppressWarnings(as.numeric(das.data$Event)))
#   if (any((is.na(das.data$Lat) | is.na(das.data$Lon)) & event.num))
#     warning("Some of the lat/lon data for ", file, " is NA")
#   
#   if (any(is.na(das.data$DateTime) & event.num))
#     warning("The date/time data is unexpectedly NA for ", file,
#             " for the following rows: ", which(is.na(das.data$DateTime)))
#   
#   ### Return data
#   das.data
# }
#
#############################################################################
