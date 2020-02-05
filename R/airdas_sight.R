#' Aerial DAS sightings
#'
#' Extract sighting information from aerial DAS data
#'
#' @param x \code{airdas_df} object; output from \code{\link{airdas_process}}, 
#'  or a data frame that can be coerced to a \code{airdas_df} object
#' 
#' @importFrom dplyr %>% .data arrange bind_rows case_when filter left_join mutate select
#' @importFrom rlang !!
#'
#' @details This function expects the following event codes: 
#'   
#'   Marine mammal sighting codes: "S" (sighting) and "s" (resight); 
#'   Turtle sighting code: "t";
#'   Aditional sighting information (multipecies sighting): "1"
#'   
#'   For more information about the event codes, see 
#'   \url{https://github.com/smwoodman/swfscAirDAS/blob/master/inst/AirDAS_Format.pdf}
#'   
#'   A 'standard sighting' (SightStd in output data frame) means it was by 
#'   ObsL, ObsB, or ObsR (not the recorder or pilot)
#'   
#'   Multispecies group size is rounded to nearest integer using round(, 0)
#'   
#'   This function assumes the following 
#'   \tabular{llll}{
#'     \emph{Information} \tab \emph{Mammal sighting} \tab \emph{Turtle sighting} \tab \emph{New column name}\cr
#'     Sighting number   \tab Data1          \tab       \tab SightNo\cr
#'     Observer          \tab Data2          \tab Data1 \tab Obs\cr
#'     Declination angle \tab Data3          \tab Data2 \tab Angle\cr
#'     Group size (best estimate) \tab Data4 \tab NA    \tab Gs \cr
#'     Species code 1    \tab Data5          \tab Data3 \tab Sp \cr
#'     Species code 2    \tab Data6          \tab NA    \tab todo \cr
#'     Species code 3    \tab Data7          \tab NA    \tab todo \cr
#'     Turtle size (ft)  \tab NA             \tab Data4 \tab todo\cr
#'     Travel direction (deg) \tab NA        \tab Data5 \tab todo\cr
#'     Tail visible?     \tab NA             \tab Data6 \tab todo\cr
#'   }
#'
#' @return Data frame with 1) the columns from \code{x}, excluding the 'Data#' columns,
#'   and 2) columns with sighting information
#'   (observer, species, etc.) extracted from 'Data#' columns as specified in Details.
#'   The data frame has one row for each sighting,
#'   or one row for each species of each sighting if it is a multispecies sighting.
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
             GsTotal = gs.total, Multi = TRUE) %>% 
      filter(!is.na(.data$Data4))
  }, sight.df = sight.df)
  
  # Add multispecies sightings back into sight.df
  sight.df <- sight.df %>% 
    filter(!(.data$sight_cumsum %in% sight.cumsum.mult)) %>% 
    mutate(GsTotal = as.integer(.data$Data4), Multi = FALSE) %>% 
    bind_rows(sight.mult) %>% 
    arrange(.data$sight_cumsum) %>% 
    mutate(idx = seq_along(.data$sight_cumsum)) %>% 
    select(-.data$sight_cumsum)
  
  
  
  #----------------------------------------------------------------------------
  # Extract relevant information:
  stopifnot(all(sight.df$Event %in% event.sight))
  
  ### 1) Processed AirDAS variables
  sight.info <- sight.df %>% 
    select(-!!paste0("Data", 1:7), -.data$GsTotal, -.data$Multi)
  
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
                            .data$Event == "t" ~ as.integer(1))) %>% 
    select(.data$idx, .data$SightNo, .data$Obs, .data$Angle, .data$SightStd, 
           .data$Sp, .data$GsSp, .data$GsTotal, .data$Multi)
  
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
