# Internal, helper functions for swfscAirDAS

###############################################################################
# Helper functions for airdas_process
.airdas_process_num <- function(das.df, col.name, event.curr, event.na) {
  ifelse(
    is.na(as.numeric(das.df[event.curr, col.name])),
    event.na, as.numeric(das.df[event.curr, col.name])
  )
}

.airdas_process_chr <- function(das.df, col.name, event.curr, event.na) {
  ifelse(
    is.na(das.df[event.curr, col.name]),
    event.na, das.df[event.curr, col.name]
  )
}

###############################################################################
