# Internal, helper functions for swfscAirDAS

###############################################################################
# Helper functions for airdas_process
.airdas_process_num <- function(init.val, das.df, col.name, event.curr, event.na) {
  toreturn <- init.val
  toreturn[event.curr] <- ifelse(
    is.na(as.numeric(das.df[event.curr, col.name])),
    event.na, as.numeric(das.df[event.curr, col.name])
  )
  
  toreturn
}

.airdas_process_chr <- function(init.val, das.df, col.name, event.curr, event.na) {
  toreturn <- init.val
  toreturn[event.curr] <- ifelse(
    is.na(das.df[event.curr, col.name]),
    event.na, das.df[event.curr, col.name]
  )
  
  toreturn
}

###############################################################################
# Helper functions for airdas_effort_segdata

### Extract unique (and sorted) characters from a string
# stackoverflow.com/questions/31814548
fn_uniqchars <- function(x) sort(unique(strsplit(x, "")[[1]]))


### Keep running sum of data (conditions) multiplied by distance ratio
# Requires that names of cond.list elements are the same as
#   the column names in curr.df
fn_aggr_conditions <- function(data.list, curr.df, idx, dist.perc) {
  stopifnot(
    all(names(data.list) %in% names(curr.df)), 
    idx <= nrow(curr.df)#, 
    # dist.perc >= 0
  )
  
  if (is.na(dist.perc)) {
    lapply(data.list, function(i) NA)
    
  } else if (dist.perc != 0) {
    tmp <- lapply(names(data.list), function(k, dist.perc) {
      z <- data.list[[k]]
      if (inherits(z, c("numeric", "integer"))) {
        z + (dist.perc * curr.df[[k]][idx])
      } else if (inherits(z, "character")) {
        paste(fn_uniqchars(paste0(z, curr.df[[k]][idx])), collapse = "")
      } else if (inherits(z, "logical")) {
        browser()
      } else {
        print("class not recognized")
        browser()
      }
    }, dist.perc = dist.perc)
    
    names(tmp) <- names(data.list)
    tmp
    
  } else {
    data.list
  }
}


###############################################################################
