#' Aerial DAS sightings
#'
#' Extract sightings and associated information from aerial DAS data
#'
#' @param das.df data frame; processed aerial DAS data.. TODO
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr .data
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang !!
#'
#' @details This function requires the following event codes: 
#'   
#'   Marine mammal sighting codes: "S" (first sighting) and "s" (resighting); 
#'   turtle sighting code: "t";
#'   additional sighting information: "1"
#'   
#'   See ... todo (link to pdf) more info
#'   
#'   A 'standard sighting' (std_sight in output data frame) means it was by ObsL, ObsB, or ObsR
#'   
#'   This function assumes the following 
#'   \tabular{llll}{
#'     \emph{Information} \tab \emph{Mammal sighting} \tab \emph{Turtle sighting} \tab \emph{New column name}\cr
#'     Sighting number \tab Data1 \tab \tab sight_no\cr
#'     Observer \tab Data2 \tab Data1 \tab obs\cr
#'     Declination angle \tab Data3 \tab Data2 \tab angle_declination\cr
#'     Group size (best estimate) \tab Data4 \tab NA \tab todo \cr
#'     Species code 1 \tab Data5 \tab Data3 \tab todo \cr
#'     Species code 2 \tab Data6 \tab NA \tab todo \cr
#'     Species code 3 \tab Data7 \tab NA \tab todo \cr
#'     Turtle size (ft) \tab NA \tab Data4 \tab todo\cr
#'     Travel direction (deg) \tab NA \tab Data5 \tab todo\cr
#'     Tail visible? \tab NA \tab Data6 \tab todo\cr
#'   }
#'
#' @return Data frame with one row for each species for each sighting.
#'   Associated information (observer, species, etc.) is extracted 
#'   from \code{Data#} columns into as specified in Details.
#'   Other columns from \code{das.df} will be included in the output
#'
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.proc <- airdas_process(y)
#' 
#' airdas_sight(y.proc)
#'
#' @export
airdas_sight <- function(das.df) {
  #----------------------------------------------------------------------------
  ### Filter for and extract sighting data
  event.sight <- c("S", "s", "t")
  event.sight.info <- "1"

  ### Filter for sighting-related data
  sight.df <- das.df %>% 
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
    sp.num.all <- sp.perc.all / 100 * gs.total
    
    # Warning if species percentages do not sum to 100
    if (!all.equal(sum(sp.perc.all, na.rm = TRUE), 100)) {
      file.line <- paste0(
        "file '", curr.df$file_das[1], "', line '", curr.df$line_num[2], "'"
      )
      warning("The multispecies sightings percentages for ", file.line, 
              ", do not sum to 100", immediate. = TRUE)
    }
    
    # Create df with one row for each species
    curr.df.e1 <- curr.df[1, ] %>% mutate(Event = 1)
    rbind(curr.df[1, ], curr.df.e1, curr.df.e1) %>% 
      mutate(Data4 = as.character(sp.num.all), Data5 = sp.all, 
             Data6 = NA, Data7 = NA) %>% 
      filter(!is.na(.data$Data4))
  }, sight.df = sight.df)
  
  # Add 'new' sightings back into sight.df
  sight.df <- sight.df %>% 
    filter(!(.data$sight_cumsum %in% sight.cumsum.mult)) %>% 
    bind_rows(do.call(rbind, sight.mult)) %>% 
    arrange(.data$sight_cumsum)
  
  # Ensure that there is no "S" event data in Data6 or Data7
  stopifnot(
    all(is.na(filter(sight.df, .data$Event %in% c("S", 1))$Data6)), 
    all(is.na(filter(sight.df, .data$Event %in% c("S", 1))$Data7))
  )
  
  
  #----------------------------------------------------------------------------
  # Extract relevant sighting data for...
  ### ...all sighting events
  sight.info.all <- sight.df %>% 
    filter(.data$Event %in% c(event.sight, 1)) %>% 
    mutate(sight_no = ifelse(.data$Event == "t", NA, .data$Data1), 
           obs = ifelse(.data$Event == "t", .data$Data1, .data$Data2), 
           angle_declination = as.numeric(ifelse(.data$Event == "t", .data$Data2, .data$Data3)), 
           species = ifelse(.data$Event == "t", .data$Data3, .data$Data5), 
           groupsize = as.numeric(ifelse(.data$Event == "t", 1, .data$Data4)),
           std_sight = .data$obs %in% c(.data$ObsL, .data$ObsB, .data$ObsR)) %>% 
    select(-!!paste0("Data", 1:7))
  
  ### ...only t events
  # Must be filtered for t so multi-sp lines are not duplicated in final join
  sight.info.t <- sight.df %>% 
    filter(.data$Event == "t") %>% 
    mutate(turtle_sizeft = as.numeric(.data$Data4), 
           turtle_direction = as.numeric(.data$Data5), 
           turtle_tail = .data$Data6) %>% 
    select(.data$sight_cumsum, .data$turtle_sizeft, .data$turtle_direction, 
           .data$turtle_tail)
  
  
  #----------------------------------------------------------------------------
  # Warning checks and return
  
  ### Checks
  if (any(is.na(filter(sight.info.all, .data$Event == "S")[["sight_groupsize"]]))) 
    warning("Some 'S' events had non-numeric group sizes")
  
  ### Join data frames and return
  sight.info.all %>% 
    left_join(sight.info.t, by = "sight_cumsum") %>% 
    select(-.data$sight_cumsum)
}
