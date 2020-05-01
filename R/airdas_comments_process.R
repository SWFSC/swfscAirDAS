#' Process comments in AirDAS data
#' 
#' Extract miscellaneous information recorded in AirDAS data comments
#' 
#' @param x \code{airdas_dfr} or \code{airdas_df} object, 
#'  or a data frame that can be coerced to a \code{airdas_dfr} object
#' 
#' @details Historically, project-specific or miscellaneous data have been 
#'   recorded in AirDAS comments using specific formats and character codes. 
#'   This functions identifies and extracts this data from the comment 
#'   text strings. Current supported data types are: 
#'   fish balls, molas, jellyfish, and crab pots. 
#'   See any of the AirDAS format PDFs (\code{\link{airdas_format_pdf}}) 
#'   for information about the specific codes and formats used to
#'   record this data. All comments are converted to lower case for processing 
#'   to avoid missing data.
#'   
#'   These different codes contain (at most) 
#'   a level one descriptor (e.g. fish ball or crab pot), 
#'   a level two descriptor (e.g. size or jellyfish species), 
#'   and a value (a count or percentage). 
#'   Thus, the extracted data are returned together in this structure. 
#'   The output data frame is long data, i.e. it has one piece of information per line.
#'   For instance, if the comment is "fb1s fb1m", then the output data frame
#'   will have one line for the small fish ball and one for the medium fish ball. 
#'   See Value section for more details.
#'   
#'   Currently this function only recognizes mola data recorded using the 
#'   "m1", "m2", and "m3" codes (small, medium, and large mola, respectively). 
#'   Thus, "mola" is not recognized and processed.
#'   
#'   The following codes are used for the level two descriptors: 
#'   \tabular{lr}{
#'     \emph{Description} \tab \emph{Code}\cr
#'     Small  \tab s\cr
#'     Medium \tab m\cr
#'     Large  \tab l\cr
#'     Unknown  \tab u\cr
#'     Chrysaora  \tab c\cr
#'     Moon jelly \tab m\cr
#'     Egg yolk   \tab e\cr
#'     Other      \tab o\cr
#'   }
#'   
#' @return \code{x}, with the following columns added: 
#'   \itemize{
#'     \item comment_str: the full comment string
#'     \item Misc1: level one descriptor, e.g. "Fish ball" or "Jellyfish"
#'     \item Misc2: level two descriptor, e.g. s, m, or c (see Details section for key)
#'     \item Value: Associated count or percentage
#'     \item flag_check: logical indicating if the comment string was longer 
#'       than an expected number of characters, and thus should be manually inspected 
#'   }
#'   
#'    
#'   
#' 
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.proc <- airdas_process(y)
#' 
#' d <- airdas_comments_process(y.proc)
#' 
#' @export
airdas_comments_process <- function(x) UseMethod("airdas_comments_process")


#' @name airdas_comments_process
#' @export
airdas_comments_process.data.frame <- function(x) {
  airdas_comments_process(as_airdas_dfr(x))
}


#' @name airdas_comments_process
#' @export
airdas_comments_process.airdas_dfr <- function(x) {
  as_airdas_dfr(.airdas_comments_process(x))
}


#' @name airdas_comments_process
#' @export
airdas_comments_process.airdas_df <- function(x) {
  as_airdas_df(.airdas_comments_process(x))
}



.airdas_comments_process <- function(x) {
  #----------------------------------------------------------------------------
  stopifnot(inherits(x, "airdas_df") | inherits(x, "airdas_dfr"))
  
  x.c <- airdas_comments(x) %>% mutate(str_lower = tolower(.data$comment_str))
  df.na <- x.c %>% 
    mutate(Misc1 = NA, Misc2 = NA, Value = NA, flag_check = FALSE) %>% 
    slice(0)
  
  
  #----------------------------------------------------------------------------
  ### Fish balls
  x.c.fb <- x.c %>% filter(str_detect(.data$str_lower, "fb"))
  
  if (nrow(x.c.fb) > 0) {
    # Get data for 0-9 fbs
    fb.4 <- x.c.fb %>% 
      mutate(var_extract = str_match_all(.data$str_lower, "fb(..)")) %>% 
      unnest(cols = c(.data$var_extract), keep_empty = FALSE) %>% 
      mutate(var_extract = .data$var_extract[, 1], 
             Misc1 = "fish ball", 
             Misc2 = substr(.data$var_extract, 4, 4), 
             Value = suppressWarnings(as.numeric(substr(.data$var_extract, 3, 3))))

    # Get data for 10+ fbs
    fb.5 <- x.c.fb %>% 
      mutate(var_extract = str_match_all(.data$str_lower, "fb(...)")) %>% 
      unnest(cols = c(.data$var_extract), keep_empty = FALSE) %>% 
      mutate(var_extract = .data$var_extract[, 1], 
             Misc1 = "fish ball", 
             Misc2 = substr(.data$var_extract, 5, 5), 
             Value = suppressWarnings(as.numeric(substr(.data$var_extract, 3, 4))))
    
    fb.df <- fb.4 %>% 
      bind_rows(fb.5) %>% 
      mutate(flag_check = nchar(.data$str_lower) > 12) %>% 
      filter(!is.na(.data$Value), .data$Misc2 %in% c("s", "m", "l", "u")) %>% 
      arrange(.data$line_num)
    
  } else {
    fb.df <- df.na
  }
  
  
  #----------------------------------------------------------------------------
  ### Molas
  x.c.mola <- x.c %>% 
    filter(str_detect(.data$str_lower, "m1") | str_detect(.data$str_lower, "m2") | 
             str_detect(.data$str_lower, "m3"))  #str_detect(.data$str_lower, "mola")
  
  mola.df <- if (nrow(x.c.mola) > 0) {
    x.c.mola %>% 
      mutate(str_lower = paste0(" ", .data$str_lower), #in case comment is e.g. "1 m1"
             var_extract = str_match_all(.data$str_lower, "(..) m(.)")) %>% 
      unnest(cols = c(.data$var_extract), keep_empty = FALSE) %>% 
      mutate(var_extract = .data$var_extract[, 1], 
             Misc1 = "mola", 
             Misc2 = substr(.data$var_extract, 5, 5), 
             Value = suppressWarnings(as.numeric(substr(.data$var_extract, 1, 2))), 
             flag_check = nchar(.data$str_lower) > 12) %>% 
      filter(!is.na(.data$Value), .data$Misc2 %in% c(1, 2, 3))
  } else {
    df.na
  }
  
  
  #----------------------------------------------------------------------------
  ### Jellyfish
  x.c.jelly <- x.c %>% filter(str_detect(.data$str_lower, "jf"))
  
  jelly.df <- if (nrow(x.c.jelly) > 0) {
    x.c.jelly %>% 
      mutate(var_extract = str_match_all(.data$str_lower, "jf(....)")) %>% 
      unnest(cols = c(.data$var_extract), keep_empty = FALSE) %>% 
      mutate(var_extract = .data$var_extract[, 1], 
             Misc1 = "jellyfish", 
             Misc2 = substr(.data$var_extract, 3, 3), 
             Value = suppressWarnings(as.numeric(substr(.data$var_extract, 4, 6))), 
             flag_check = nchar(.data$str_lower) > 15) %>% 
      filter(!is.na(.data$Value), 
             .data$Misc2 %in% c("c", "m", "e", "o", "u"))
  } else {
    df.na
  }
  
  
  #----------------------------------------------------------------------------
  ### Crab pots
  x.c.cp <- x.c %>% filter(str_detect(.data$str_lower, "cp"))
  
  cp.df <- if (nrow(x.c.cp) > 0) {
    x.c.cp %>% 
      mutate(str_lower = paste0(" ", .data$str_lower), #in case comment is e.g. "1 cp"
             var_extract = str_match_all(.data$str_lower, "(..) cp")) %>% 
      unnest(cols = c(.data$var_extract), keep_empty = FALSE) %>% 
      mutate(var_extract = .data$var_extract[, 1], 
             Misc1 = "crab pot", 
             Misc2 = NA, 
             Value = suppressWarnings(as.numeric(substr(.data$var_extract, 1, 2))), 
             flag_check = nchar(.data$str_lower) > 10) %>% 
      filter(!is.na(.data$Value))
  } else {
    df.na
  }
  
  
  #----------------------------------------------------------------------------
  ### Return
  df.out <- bind_rows(fb.df, mola.df, jelly.df, cp.df) %>% 
    select(-.data$var_extract, -.data$DateTime, -.data$str_lower) %>% 
    arrange(.data$line_num) 
  
  right_join(x, df.out, by = c("file_das", "line_num"))
}
