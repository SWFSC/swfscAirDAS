#' Aerial DAS sightings
#'
#' Extract sighting information from aerial DAS data
#'
#' @param x \code{airdas_df} object; output from \code{\link{airdas_process}}, 
#'   or a data frame that can be coerced to a \code{airdas_df} object
#' 
#' @importFrom dplyr %>% .data arrange bind_rows case_when filter left_join mutate select
#' @importFrom rlang !!
#'
#' @details AirDAS events contain specific information in the 'Data#' columns,
#'   with the information depending on the event code for that row.
#'   This function extracts relevant data for sighting events, and returns a
#'   data frame with dedicated columns for each piece of sighting information.
#'   This function recognizes the following types of sightings: 
#'   marine mammal sightings (event code "S"), marine mammal resights (code "s"), 
#'   and turtle sightings (code "t"). 
#'   Multi-species (mixed species) marine mammal sightings are also followed by a "1" event.
#'   See \code{\link{airdas_format_pdf}} for more information about events and event formats.
#'   
#'   Abbreviations used in column names: Gs = group size, Sp = species, 
#'   Mixed = mixed species (multi-species) sighting.
#' 
#'   A 'standard sighting' ('SightStd' in output data frame) is a sighting 
#'   made by ObsL, ObsB, or ObsR (not the data recorder or pilot).
#'   
#'   Multi-species group sizes are rounded to nearest integer using \code{round(, 0)}.
#'
#' @return Data frame with 1) the columns from \code{x}, excluding the 'Data#' columns,
#'   and 2) columns with sighting information (observer, species, etc.) 
#'   extracted from 'Data#' columns as specified in Details.
#'   The data frame has one row for each sighting,
#'   or one row for each species of each sighting if 
#'   it is a multi-species sighting.
#'
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.proc <- airdas_process(y)
#' 
#' airdas_sight(y.proc)
#'
#' @export
airdas_sight <- function(x) UseMethod("airdas_sight")


#' @name airdas_sight
#' @export
airdas_sight.data.frame <- function(x) {
  airdas_sight(as_airdas_df(x))
}


#' @name airdas_sight
#' @export
airdas_sight.airdas_df <- function(x) {
  #----------------------------------------------------------------------------
  ### Filter for and extract sighting data
  event.sight <- c("S", "s", "t")
  event.sight.info <- "1"
  
  ### Filter for sighting-related data
  sight.df <- x %>% 
    filter(.data$Event %in% c(event.sight, event.sight.info)) %>% 
    mutate(sight_cumsum = cumsum(.data$Event %in% event.sight))
  
  stopifnot(length(event.sight.info) == 1)
  
  
  #----------------------------------------------------------------------------
  ### For every sighting event paired with a '1' event, split the sighting
  ###   into multiple lines. 
  ### If no '1' events, sight.mult will be list() and thus not change sight.df
  sight.cumsum.mult <- sight.df$sight_cumsum[sight.df$Event %in% event.sight.info]
  
  sight.mult <- lapply(sight.cumsum.mult, function(i, sight.df) {
    curr.df <- sight.df %>% filter(.data$sight_cumsum == i)
    stopifnot(identical(curr.df$Event, c("S", "1")))
    
    # Extract species and group size info
    gs.total <- as.numeric(curr.df$Data4[1])
    sp.all <- c(curr.df$Data5[1], curr.df$Data6[1], curr.df$Data7[1])
    sp.perc.all <- c(
      as.numeric(curr.df$Data5[2]), as.numeric(curr.df$Data6[2]), 
      as.numeric(curr.df$Data7[2])
    )
    sp.num.all <- as.integer(round(sp.perc.all / 100 * gs.total, 0))
    
    # Warning if species percentages do not sum to 100
    if (!all.equal(sum(sp.perc.all, na.rm = TRUE), 100)) {
      file.line <- paste0(
        "file '", curr.df$file_das[1], "', line '", curr.df$line_num[2], "'"
      )
      warning("The multispecies sightings percentages for ", file.line, 
              ", do not sum to 100", immediate. = TRUE)
    }
    
    # Create df with one row for each species
    bind_rows(curr.df[1, ], curr.df[1, ], curr.df[1, ]) %>% 
      mutate(Data4 = as.character(sp.num.all), Data5 = sp.all, 
             Data6 = NA, Data7 = NA, 
             GsTotal = gs.total, Mixed = TRUE) %>% 
      filter(!is.na(.data$Data4))
  }, sight.df = sight.df)
  
  # Add multi-species sightings back into sight.df
  sight.df <- sight.df %>% 
    filter(!(.data$sight_cumsum %in% sight.cumsum.mult)) %>% 
    mutate(GsTotal = ifelse(.data$Event == "S", as.integer(.data$Data4), NA), 
           Mixed = ifelse(.data$Event == "s", NA, FALSE)) %>% 
    bind_rows(sight.mult) %>% 
    arrange(.data$sight_cumsum) %>% 
    mutate(idx = seq_along(.data$sight_cumsum)) %>% 
    select(-.data$sight_cumsum)
  
  
  
  #----------------------------------------------------------------------------
  # Extract relevant information:
  stopifnot(all(sight.df$Event %in% event.sight))
  
  ### 1) Processed AirDAS variables
  sight.info <- sight.df %>% 
    select(-!!paste0("Data", 1:7), -.data$GsTotal, -.data$Mixed)
  
  ### 2) Sighting data shared by all sighting types (hence no)
  sight.info.all <- sight.df %>% 
    mutate(SightNo = ifelse(.data$Event == "t", NA, .data$Data1), 
           Obs = case_when(.data$Event == "S" ~ .data$Data2,
                           .data$Event == "t" ~ .data$Data1), 
           Angle = as.numeric(case_when(.data$Event == "S" ~ .data$Data3,
                                        .data$Event == "s" ~ .data$Data2, 
                                        .data$Event == "t" ~ .data$Data2)), 
           SightStd = ifelse(.data$Event == "s", NA, 
                             .data$Obs %in% c(.data$ObsL, .data$ObsB, .data$ObsR)), 
           Sp = case_when(.data$Event == "S" ~ .data$Data5,
                          .data$Event == "t" ~ .data$Data3), 
           GsSp = case_when(.data$Event == "S" ~ as.integer(.data$Data4),
                            .data$Event == "t" ~ as.integer(1)), 
           GsTotal = case_when(.data$Event == "S" ~ .data$GsTotal, 
                               .data$Event == "t" ~ as.integer(1))) %>% 
    select(.data$idx, .data$SightNo, .data$Obs, .data$Angle, .data$SightStd, 
           .data$Sp, .data$GsSp, .data$GsTotal, .data$Mixed)
  
  ### 3) Turtle sighting data
  sight.info.t <- sight.df %>% 
    filter(.data$Event == "t") %>%
    mutate(TurtleSizeFt = as.numeric(.data$Data4), 
           TurtleDirection = as.numeric(.data$Data5), 
           TurtleTail = .data$Data6) %>% 
    select(.data$idx, .data$TurtleSizeFt, .data$TurtleDirection, 
           .data$TurtleTail)
  
  
  #----------------------------------------------------------------------------
  # Join data frames and return
  sight.info %>% 
    left_join(sight.info.all, by = "idx") %>% 
    left_join(sight.info.t, by = "idx") %>% 
    select(-.data$idx)
}
