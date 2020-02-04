# code to prepare sample AirDAS .das file to use for examples

library(dplyr)
library(gdata)
library(lubridate)
library(stringr)
library(swfscAirDAS)
library(tibble)

source("data-raw/airdas_sample_funcs.R")


x.orig <- airdas_read("../airdas/airDAS_files/DASDC2019_JUN.das")

###############################################################################
# Extract, process, and jitter actual data

# Pre-processing
x <- x.orig %>% 
  select(-file_das, -line_num) %>% 
  slice(c(90:200)) %>% 
  mutate(DateTime = DateTime - years(4) - months(2) + hours(4) - minutes(15), 
         Lat = Lat + 2.3, Lon = Lon - 1.27)

# Choose some subset of sightings and comments to keep
l.sight.keep <- sum(x$Event == "S")
n.sight.keep <- 25
set.seed(42)
sight.rm <- sort(sample(which(x$Event == "S"), l.sight.keep - n.sight.keep))

comment.rm <- which(x$Event == "C")[-c(5, 6, 7, 11, 21)]

x <- x %>% slice(-c(sight.rm, comment.rm))
rm(sight.rm, comment.rm)

sight.curr <- which(x$Event == "S")
x$EffortDot[x$Event == 1]

# Jitter sighting data
x$Data1[sight.curr] <- seq_len(n.sight.keep)
x$Data2[sight.curr] <- unname(
  vapply(x$Data2[sight.curr], function(i) switch(i, sb = "aa", kw = "bb", sh = "cc", kf = "dd"), 
         character(1))
)
set.seed(42)
x$Data3[sight.curr] <- round(as.numeric(x$Data3[sight.curr]) + runif(n.sight.keep, -5, 5), 0)
set.seed(42)
x$Data4[sight.curr] <- round(runif(n.sight.keep, 1, 6), 0)
set.seed(42)
x$Data5[sight.curr] <- sample(c("mn", "bm", "pp", "gg"), size = n.sight.keep, replace = TRUE)
rm(sight.curr)

# Jitter P event data
p.which <- which(x$Event == "P")
stopifnot(length(p.which) == 2)
x$Data1[p.which] <- c("aa", "cc")
x$Data2[p.which] <- "bb"
x$Data3[p.which] <- c("cc", "aa")
x$Data4[p.which] <- "dd"
rm(p.which)

# Jitter transect number
x$Data1[x$Event == "T"] <- c("T1", "T2")

# Jitter jelly code
x$Data4[which(x$Event == "W")[2]] <- 2


# Manual addition of a '1', 't', "R", and 'E' events. And ending "O"
x$Event[c(32, 34)] <- c("E", "R")

c.txt <- " off effort to circle on unidentifed object"
c.data <- str_sub(c.txt, seq(1, 31, by = 5), c(seq(5, 30, by = 5), -1))
c.idx.after <- 32

t.idx.after <- 24
s.idx.after <- nrow(x)

x$Data4[13] <- 10
x$Data6[13] <- "er"

x.out <- x %>% 
  add_row(Event = "C", EffortDot = TRUE, DateTime = x$DateTime[c.idx.after],
          Lat = x$Lat[c.idx.after], Lon = x$Lon[c.idx.after],
          Data1 = c.data[1], Data2 = c.data[2], Data3 = c.data[3], Data4 = c.data[4], 
          Data5 = c.data[5], Data6 = c.data[6], Data7 = c.data[7],
          .after = c.idx.after) %>% 
  add_row(Event = "t", EffortDot = TRUE, DateTime = x$DateTime[t.idx.after],
          Lat = x$Lat[t.idx.after], Lon = x$Lon[t.idx.after], Data1 = "aa", 
          Data2 = -20, Data3 = "dc", Data4 = 5, Data5 = 90, Data6 = "N", 
          .after = t.idx.after) %>% 
  add_row(Event = 1, EffortDot = FALSE, Data5 = 80, Data6 = 20, 
          .after = 13) %>% #2nd for indices
  add_row(Event = "s", EffortDot = TRUE, DateTime = x$DateTime[s.idx.after],
          Lat = x$Lat[s.idx.after], Lon = x$Lon[s.idx.after], Data1 = "25", 
          Data2 = -70) %>% 
  add_row(Event = "O", EffortDot = FALSE, DateTime = tail(x$DateTime, 1) + seconds(30), 
          Lat = tail(x$Lat, 1), Lon = tail(x$Lon, 1) + 0.02) %>% 
  mutate(EventNum = c(1:13, NA, 14:83))



# Checks
identical(order(na.omit(x.out$DateTime)), sort(order(na.omit(x.out$DateTime))))

### Write to das file
# raw_airdas_fwf(x, "data-raw/airdas_strawman_test.das", data7len = 5)
raw_airdas_fwf(x.out, "inst/airdas_sample.das", data7len = 15)

###############################################################################
