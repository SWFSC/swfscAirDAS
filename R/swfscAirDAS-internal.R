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
# Function for checking format of function inputs
.airdas_format_check <- function(x, func.name) {
  #------------------------------------------------------------------
  if (func.name == "process") {
    names.expected <- c(
      "Event", "EffortDot", "DateTime", "Lat", "Lon", 
      "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", 
      "file_das", "event_num", "line_num"
    )
    classes.expected <- list(
      "character", "logical", c("POSIXct", "POSIXt"), "numeric", "numeric", 
      "character", "character", "character", "character", "character", "character", 
      "character", "character", "integer", "integer"
    )
    
    if (!identical(names(x), names.expected) | 
        !identical(unname(lapply(x, class)), classes.expected)) {
      stop("This function expects a data frame consisting of columns with ", 
           "the following names and classes, respectively:\n", 
           "Names: ", paste(names.expected, collapse  = ", "), "\n", 
           "Classes: ", paste(classes.expected, collapse  = ", "))
    }
    
    stopifnot(.airdas_format_check_event(x$Event))
    
    #----------------------------------------------------------------
  } else if (func.name == "sight") {
    FALSE
    
    #----------------------------------------------------------------
  } else {
    func.name.all <- c(
      "process", "sight"
    )
    stop("func.name is not one of: ", paste(func.name.all, collapse = ", "))
  }
  
  #------------------------------------------------------------------
  TRUE
}

### Format helper - Check event codes
.airdas_format_check_event <- function(i) {
  event.exp <- sort(c(
    "#", "*", "1", "A", "C", "E", "O", "P", 
    "R", "S", "t", "T", "V", "W", "s", "c"
  ))
  if (!all(i %in% event.exp)) {
    warning("Some of the data in the 'Event' column are not one of:\n", 
            paste(event.exp, collapse = ", "))
  }
  
  TRUE
}


###############################################################################
# Helper functions for airdas_effort_segdata

### Extract unique (and sorted) characters from a string
# stackoverflow.com/questions/31814548
fn_uniqchars <- function(x) sort(unique(strsplit(x, "")[[1]]))


### Keep running sum of data (conditions) multiplied by distance ratio
# Requires that names of cond.list elements are the same as
#   the column names in curr.df
# TODO?: For HKR, should n be removed if there are other (i.e. hkr) entries
fn_aggr_conditions <- function(data.list, curr.df, idx, dist.rat) {
  stopifnot(
    all(names(data.list) %in% names(curr.df)), 
    idx <= nrow(curr.df), dist.rat >= 0
  )
  
  if (dist.rat != 0) {
    tmp <- lapply(names(data.list), function(k, dist.rat) {
      z <- data.list[[k]]
      if (inherits(z, c("numeric", "integer"))) {
        z + (dist.rat * curr.df[[k]][idx])
      } else if (inherits(z, "character")) {
        paste(fn_uniqchars(paste0(z, curr.df[[k]][idx])), collapse = "")
      } else {
        print("class not recognized")
        browser()
      }
    }, dist.rat = dist.rat)
    
    names(tmp) <- names(data.list)
    tmp
    
  } else {
    data.list
  }
}


###############################################################################
