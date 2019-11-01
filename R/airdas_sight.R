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
#' @importFrom dplyr slice
#'
#' @details TODO. 
#'   TODO: make parameters for 'event.sight' and 'event.turtle'?
#'   Marine mammal sighting codes: "S" and "s"; turtle sighting code: "t"
#'   This function assumes the following 
#'   \tabular{llll}{
#'     \emph{Information} \tab \emph{Mammal sighting} \tab 
#'       \emph{Turtle sighting} \tab \emph{New column name}\cr
#'     Sighting number \tab Data1 \tab \tab sight_no\cr
#'     Observer \tab Data2 \tab Data1 \tab obs\cr
#'     Declination angle \tab Data3 \tab Data2 \tab angle_declination\cr
#'     Etc\cr
#'   }
#'
#' @return Data frame with one row for each species for each sighting.
#'   Associated information (observer, species, etc.) is extracted 
#'   from \code{Data#} columns into as specified in Details.
#'   Other columns from \code{das.df} will be included in the output
#'
#' @examples
#' # TODO
#' # d <- do.call(rbind, lapply(list.files("../airdas/airDAS_files", full.names = TRUE), airdas_read))
#' # x <- airdas_process(d)
#' # x.sight <- airdas_sight(x)
#'
#' @export
airdas_sight <- function(das.df) {
  #----------------------------------------------------------------------------
  # Filter for and extract sighting data
  das.df.orig <- das.df
  event.sight <- c("S", "s", "t")
  
  ### Filter for sighting-related data
  das.df <- das.df.orig %>% 
    filter(.data$Event %in% c(event.sight, 1)) %>% 
    mutate(sight_cumsum = cumsum(.data$Event %in% event.sight))
  
  
  #----------------------------------------------------------------------------
  ### For every sighting event paired with a '1' event, split the sighting
  ###   into multiple lines
  sight.cumsum.mult <- das.df$sight_cumsum[das.df$Event == 1]
  sight.mult <- lapply(sight.cumsum.mult, function(i, das.df) {
    curr.df <- das.df %>% filter(.data$sight_cumsum == i)
    stopifnot(
      nrow(curr.df) == 2, 
      identical(curr.df$Event, c("S", "1"))
    )
    
    gs.total <- as.numeric(curr.df$Data4[1])
    sp.all <- c(curr.df$Data5[1], curr.df$Data6[1], curr.df$Data7[1])
    sp.num.all <- c(
      as.numeric(curr.df$Data5[2]) / 100 * gs.total, 
      as.numeric(curr.df$Data6[2]) / 100 * gs.total, 
      as.numeric(curr.df$Data7[2]) / 100 * gs.total
    )
    
    # TODO: make warning message more informative
    if (!all.equal(sum(sp.num.all  * 100 / gs.total, na.rm = TRUE), 100))
      warning("Some of the multispecies sightings percentages ", 
              "do not sum to 100")
    
    rbind(curr.df[1, ], curr.df[1, ], curr.df[1, ]) %>% 
      mutate(Data4 = as.character(sp.num.all), Data5 = sp.all, 
             Data6 = NA, Data7 = NA) %>% 
      slice(which(!is.na(sp.all)))
  }, das.df = das.df)
  
  # Add 'new' sightings back into das.df
  das.df <- das.df %>% 
    filter(!(.data$sight_cumsum %in% sight.cumsum.mult)) %>% 
    bind_rows(do.call(rbind, sight.mult)) %>% 
    arrange(.data$sight_cumsum)
  
  rm(sight.mult)
  
  # Ensure that there is no "S" event data in Data6 or Data7
  stopifnot(
    all(is.na(filter(das.df, .data$Event == "S")$Data6)), 
    all(is.na(filter(das.df, .data$Event == "S")$Data7))
  )
  
  
  #----------------------------------------------------------------------------
  ### Extract relevant sighting data for...
  # All sighting events
  sight.info.s <- das.df %>% 
    filter(.data$Event %in% event.sight) %>% 
    mutate(sight_no = ifelse(.data$Event == "t", NA, .data$Data1), 
           obs = ifelse(.data$Event == "t", .data$Data1, .data$Data2), 
           angle_declination = as.numeric(ifelse(.data$Event == "t", .data$Data2, .data$Data3)), 
           species = ifelse(.data$Event == "t", .data$Data3, .data$Data5), 
           sight_groupsize = as.numeric(ifelse(.data$Event == "t", NA, .data$Data4)),
           std_sight = .data$obs %in% c(.data$ObsL, .data$ObsB, .data$ObsR)) %>% 
    select(-!!paste0("Data", 1:7))
  
  # Only t events. Must be filtered for t so that multispecies lines are not
  #   duplicated in final join
  sight.info.t <- das.df %>% 
    filter(.data$Event == "t") %>% 
    mutate(turtle_sizeft = .data$Data4, 
           turtle_direction = .data$Data5, 
           turtle_tail = .data$Data6) %>% 
    select(.data$sight_cumsum, .data$turtle_sizeft, .data$turtle_direction, 
           .data$turtle_tail)
  
  
  #----------------------------------------------------------------------------
  # Warning checks and return
  
  ### Checks
  if (any(is.na(filter(sight.info.s, .data$Event == "S")[["sight_groupsize"]]))) 
    warning("Some 'S' events had non-numeric group sizes")
  
  ### Join data frames and return
  sight.info.s %>% 
    left_join(sight.info.t, by = "sight_cumsum") %>% 
    select(-.data$sight_cumsum)
}
