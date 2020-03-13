#' Aerial DAS sightings
#'
#' Extract sighting information from aerial DAS data
#'
#' @param x \code{airdas_df} object; output from \code{\link{airdas_process}}, 
#'   or a data frame that can be coerced to a \code{airdas_df} object
#' 
#' @details AirDAS events contain specific information in the 'Data#' columns,
#'   with the information depending on the event code and file type for that row.
#'   This function extracts relevant data for sighting events, and returns a
#'   data frame with dedicated columns for each piece of sighting information. 
#'   It can handle multiple file types in \code{x}; for instance, 
#'   \code{x} could be processed PHOCOENA and TURTLE data
#'   combined using \code{\link[base:cbind]{rbind}}. 
#'   See \code{\link{airdas_format_pdf}} for more information about the expected
#'   events and event formats, depending on the file type.
#'   
#'   Abbreviations used in column names include: Gs = group size, Sp = species, 
#'   Mixed = mixed species (multi-species) sighting.#' 
#'   A 'standard sighting' ('SightStd' in output data frame) is a sighting 
#'   made by ObsL, ObsB, or ObsR (not the data recorder or pilot).#'   
#'   In addition, multi-species group sizes are rounded to 
#'   the nearest whole number using \code{round(, 0)}
#'
#' @return Data frame with 1) the columns from \code{x}, excluding the 'Data#' columns,
#'   and 2) columns with sighting information extracted from 'Data#' columns as described below.
#'   The data frame has one row for each sighting, or one row for each 
#'   species of each sighting if it is a multi-species (mixed) sighting.
#'   
#'   Added sighting information columns:
#'   \tabular{lll}{
#'     \emph{Sighting information}       \tab \emph{Column name} \tab \emph{Notes}\cr
#'     Sighting number                   \tab SightNo\cr
#'     Observer that made the sighting   \tab Obs\cr
#'     Angle of declination              \tab Angle    \tab Left is negative\cr
#'     Standard sighting                 \tab SightStd \tab Logical; described in Details\cr
#'     Mixed species sighting            \tab Mixed    \tab Logical\cr
#'     Total group size                  \tab GsTotal  \tab Only differnt from GsSp if mixed species sighting\cr
#'     Species                           \tab Sp\cr
#'     Species-specific group size       \tab GsSp\cr
#'     Turtle length (feet)              \tab TurtleSizeFt    \tab \code{NA} for non-"t" events\cr
#'     Turtle travel direction (degrees) \tab TurtleDirection \tab \code{NA} for non-"t" events\cr
#'     Turtle tail visible?              \tab TurtleTail      \tab \code{NA} for non-"t" events\cr
#'   }
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
    sp.num.all <- round(sp.perc.all / 100 * gs.total, 0)
    
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
    mutate(GsTotal = ifelse(.data$Event == "S", as.numeric(.data$Data4), NA), 
           Mixed = ifelse(.data$Event == "s", NA, FALSE)) %>% 
    bind_rows(sight.mult) %>% 
    arrange(.data$sight_cumsum) %>% 
    mutate(idx = seq_along(.data$sight_cumsum)) %>% 
    select(-.data$sight_cumsum)
  
  
  #----------------------------------------------------------------------------
  stopifnot(all(sight.df$Event %in% event.sight))
  
  ### 1) Extract processed AirDAS variables
  sight.info <- sight.df %>% 
    select(-!!paste0("Data", 1:7), -.data$GsTotal, -.data$Mixed)
  
  ### 2) Extract sighting information based on file type
  sight.df.all <- bind_rows(
    .airdas_sight_phocoena(filter(sight.df, .data$file_type == "phocoena")), 
    # .airdas_sight_turtle(filter(sight.df, .data$file_type == "caretta")), 
    .airdas_sight_turtle(filter(sight.df, .data$file_type == "turtle"))
  )

  #----------------------------------------------------------------------------
  # Join data frames and return
  sight.info %>% 
    left_join(sight.df.all, by = "idx") %>% 
    select(-.data$idx)
}



###############################################################################
# Extract sighting data from data created using PHOCOENA program
.airdas_sight_phocoena <- function(sight.df) {
  if (!all(sight.df$Event == "S")) 
    stop("Error in processing: not all sighitng rows with file_type ", 
         "PHOCOENA are Event S. ", 
         "Please reprocess data and/or submit an issue")
  
  sight.df %>% 
    mutate(SightNo = .data$Data1, 
           Sp = .data$Data2, 
           GsTotal = as.numeric(.data$Data3), 
           Angle = as.numeric(.data$Data4), 
           Obs = .data$Data5, 
           GsSp = .data$GsTotal, 
           SightStd = .data$Obs %in% c(.data$ObsL, .data$ObsB, .data$ObsR), 
           TurtleSizeFt = NA, 
           TurtleDirection = NA, 
           TurtleTail = NA) %>% 
    select(.data$idx, .data$SightNo, .data$Obs, .data$Angle, .data$SightStd, 
           .data$Mixed, .data$GsTotal, .data$Sp, .data$GsSp, 
           .data$TurtleSizeFt, .data$TurtleDirection, .data$TurtleTail)
}


# Extract sighting data from data created using CARETTA program
.airdas_sight_caretta <- function(sight.df) {
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
           GsSp = case_when(.data$Event == "S" ~ as.numeric(.data$Data4),
                            .data$Event == "t" ~ 1), 
           GsTotal = case_when(.data$Event == "S" ~ .data$GsTotal, 
                               .data$Event == "t" ~ 1)) %>% 
    select(.data$idx, .data$SightNo, .data$Obs, .data$Angle, .data$SightStd, 
           .data$Mixed, .data$GsTotal, .data$Sp, .data$GsSp)
  
  sight.info.t <- sight.df %>% 
    filter(.data$Event == "t") %>%
    mutate(TurtleSizeFt = as.numeric(.data$Data4), 
           TurtleDirection = as.numeric(.data$Data5), 
           TurtleTail = .data$Data6) %>% 
    select(.data$idx, .data$TurtleSizeFt, .data$TurtleDirection, 
           .data$TurtleTail)
  
  left_join(sight.info.all, sight.info.t, by = "idx")
}


# Extract sighting data from data created using TURTLE program
.airdas_sight_turtle <- function(sight.df) {
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
           GsSp = case_when(.data$Event == "S" ~ as.numeric(.data$Data4),
                            .data$Event == "t" ~ 1), 
           GsTotal = case_when(.data$Event == "S" ~ .data$GsTotal, 
                               .data$Event == "t" ~ 1)) %>% 
    select(.data$idx, .data$SightNo, .data$Obs, .data$Angle, .data$SightStd, 
           .data$Mixed, .data$GsTotal, .data$Sp, .data$GsSp)
  
  sight.info.t <- sight.df %>% 
    filter(.data$Event == "t") %>%
    mutate(TurtleSizeFt = as.numeric(.data$Data4), 
           TurtleDirection = as.numeric(.data$Data5), 
           TurtleTail = .data$Data6) %>% 
    select(.data$idx, .data$TurtleSizeFt, .data$TurtleDirection, 
           .data$TurtleTail)
  
  left_join(sight.info.all, sight.info.t, by = "idx")
}
