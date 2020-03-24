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
# Helper functions for airdas_segdata_

### Extract unique (and sorted) characters from a string
# stackoverflow.com/questions/31814548
.fn_uniqchars <- function(x) sort(unique(strsplit(x, "")[[1]]))


### Keep running sum of data (conditions) multiplied by distance ratio
# Requires that names of cond.list elements are the same as
#   the column names in curr.df
.fn_aggr_conditions <- function(data.list, curr.df, idx, dist.perc) {
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
        paste(.fn_uniqchars(paste0(z, curr.df[[k]][idx])), collapse = "")
      } else if (inherits(z, "logical")) {
        stop(".fn_aggr_conditions error - _segdata_ cannot average logical data. ", 
             "Please report this as an issue")
      } else {
        stop(".fn_aggr_conditions error - unrecognized data class. ", 
             "Please report this as an issue")
      }
    }, dist.perc = dist.perc)
    
    names(tmp) <- names(data.list)
    tmp
    
  } else {
    data.list
  }
}


###############################################################################
# Functions for doing < / > / <= / >= comparisons with floating points
.less <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) {(i < y) & !isTRUE(all.equal(i, y))}, as.logical(1))
  # (x < y) & !isTRUE(all.equal(x, y))
}

.greater <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) {(i > y) & !isTRUE(all.equal(i, y))}, as.logical(1))
  # (x > y) & !isTRUE(all.equal(x, y))
}

.less_equal <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) {(i < y) | isTRUE(all.equal(i, y))}, as.logical(1))
  # (x < y) | isTRUE(all.equal(x, y))
}

.greater_equal <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) {(i > y) | isTRUE(all.equal(i, y))}, as.logical(1))
  # (x > y) | isTRUE(all.equal(x, y))
}

.equal <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) isTRUE(all.equal(i, y)), as.logical(1))
  # isTRUE(all.equal(x, y))
}


###############################################################################
# Helper functions for airdas_check

# Check that specified values can be convereted to a numeric
.check_numeric <- function(z, event.code, z.col) {
  # z: airdas_dfr object
  # event.code: character; event code by which to filter z
  # z.col: Column which to check; must be one of the Data# columns
  ### Output: indices of z that cannot be converted to a numeric
  
  stopifnot(
    inherits(z, "airdas_dfr"),
    z.col %in% paste0("Data", 1:7),
    "idx" %in% names(z)
  )
  
  z.out <- c()
  
  for (i in event.code) {
    for (j in z.col) {
      z.curr <- z[z$Event == i, ]
      z.vec <- z.curr[[j]]
      
      z1.na <- is.na(z.vec)
      z2.na <- is.na(suppressWarnings(as.numeric(z.vec)))
      stopifnot(all(which(z1.na) %in% which(z2.na)))
      
      z.out <- c(z.out, z.curr$idx[z2.na != z1.na])
    }
  }
  
  sort(unique(z.out))
}


# Check that specified values are part of a set of accepted values
.check_character <- function(z, event.code, z.col, vals.accepted) {
  # z: airdas_dfr object
  # event.code: character; event code(s) by which to filter z
  # z.col: Column(s) which to check
  # vals.accepted: character; accepted (expected) values
  ### Output: indices of z where z.col is not one of vals.accepted
  
  stopifnot(
    inherits(z, "airdas_dfr"),
    z.col %in% c(paste0("Data", 1:7), "DateTime", "Lat", "Lon"),
    "idx" %in% names(z)
  )
  
  z.out <- c()
  
  for (i in event.code) {
    for (j in z.col) {
      z.curr <- z[z$Event == i, ]
      z.vec <- z.curr[[j]]
      
      z.out <- c(z.out, z.curr$idx[!(z.vec %in% vals.accepted)])
    }
  }
  
  sort(unique(z.out))
}


# Check that specified values are a certain length
.check_character_length <- function(z, event.code, z.col, len.accepted) {
  # z: airdas_dfr object
  # event.code: character; event code(s) by which to filter z
  # z.col: Column(s) which to check
  # len.accepted: numeric; number of characters allowed
  ### Output: indices of z where z.col is not one of vals.accepted
  
  stopifnot(
    inherits(z, "airdas_dfr"),
    z.col %in% paste0("Data", 1:7),
    "idx" %in% names(z)
  )
  
  z.out <- c()
  
  for (i in event.code) {
    for (j in z.col) {
      z.curr <- z[z$Event == i, ]
      z.vec <- z.curr[[j]]
      
      z.out <- c(z.out, z.curr$idx[!(nchar(z.vec) %in% len.accepted) & !is.na(z.vec)])
    }
  }
  
  sort(unique(z.out))
}


# Check that specified values are NA
.check_nona <- function(z, event.code, z.col) {
  # z: airdas_dfr object
  # event.code: character; event code by which to filter z
  # z.col: Column which to check; must be one of the Data# columns
  ### Output: indices of z that is NA
  
  stopifnot(
    inherits(z, "airdas_dfr"),
    z.col %in% paste0("Data", 1:7),
    "idx" %in% names(z)
  )
  
  z.out <- c()
  
  for (i in event.code) {
    for (j in z.col) {
      z.curr <- z[z$Event == i, ]
      z.vec <- z.curr[[j]]
      
      # z1.na <- is.na(z.vec)
      # z2.na <- is.na(suppressWarnings(as.numeric(z.vec)))
      # stopifnot(all(which(z1.na) %in% which(z2.na)))
      
      z.out <- c(z.out, z.curr$idx[is.na(z.vec)])
    }
  }
  
  sort(unique(z.out))
}


# Check that specified...
.check_sight_obs <- function(z, event.code, z.col) {
  # z: airdas_df object - need process observer values
  # event.code: character; event code by which to filter z
  # z.col: Column which to check; must be one of the Data# columns
  ### Output: indices of z that is NA
  
  stopifnot(
    inherits(z, "airdas_df"),
    z.col %in% paste0("Data", 1:7),
    "idx" %in% names(z)
  )
  
  z.out <- c()
  browser()
  
  z2 <- z %>% 
    filter(.data$Event == event.code) %>% 
    select(.data$idx, .data$OnEffort, 
           .data$ObsL, .data$ObsB, .data$ObsR, .data$Rec, Obs = !!z.col)
  
  z2$obs_out <- unlist(apply(z2, 1, function(i) {
    obs <- i["Obs"]
    ifelse(is.na(obs), NA, which(obs == i[c("ObsL", "ObsB", "ObsR", "Rec")]))
  }))
  
  z2
}


# TODO
.check_list <- function(z1, z2, z3, z4) {
  # z1: x
  # z2: x.lines
  # z3: idx.
  # z4: txt.
  ### Output: list formatted to be added to error.out
  
  stopifnot(inherits(z1, "airdas_dfr"))
  list(z1$file_das[z3], z1$line_num[z3], z3, z2[z3], rep(z4, length(z3)))
}

###############################################################################
