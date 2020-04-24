#' Process comments in AirDAS data
#' 
#' Extract miscellaneous information recorded in AirDAS data comments
#' 
#' @param x \code{airdas_dfr} or \code{airdas_df} object, 
#'  or a data frame that can be coerced to a \code{airdas_dfr} object
#' 
#' @details Extract information about fish balls, molas, jellyfish, 
#'   and crab pots.
#'   See any of the AirDAS format PDFs, \code{\link{airdas_format_pdf}}, 
#'   for information about format requirements.
#'   
#'   These different codes contain (at most)
#'   a level one descriptor (e.g. fish ball or crab pot), 
#'   a level 2 descriptor (e.g. small, medium, or a jellyfish species), 
#'   and a value (count or percentage). 
#'   
#'   The following codes are used in the output for the level one and two descriptors:
#'   \tabular{lr}{
#'     \emph{Description} \tab \emph{Code}\cr
#'     Fish ball \tab FB\cr
#'     Mola mola \tab Mola\cr
#'     Jellyfish \tab JF\cr
#'     Crab pot  \tab CP\cr
#'     Small  \tab s\cr
#'     Medium \tab m\cr
#'     Large  \tab l\cr
#'     Chrysaora  \tab C\cr
#'     Moon jelly \tab M\cr
#'     Egg yolk   \tab E\cr
#'     Other      \tab O\cr
#'   }
#'   
#' @return \code{x} with the following columns added: 
#'   Misc1 (one of FB/Mola/JF/CP), Misc2 (s/m/l/C/M/E/O), and Value 
#' 
#' @examples
#' #TODO
#' 
#' @export
airdas_process_comments <- function(x) UseMethod("airdas_process_comments")


#' @name airdas_process_comments
#' @export
airdas_process_comments.data.frame <- function(x) {
  airdas_process_comments(as_airdas_dfr(x))
}


#' @name airdas_process_comments
#' @export
airdas_process_comments.airdas_dfr <- function(x) {
  as_airdas_dfr(.airdas_process_comments(x))
}


#' @name airdas_process_comments
#' @export
airdas_process_comments.airdas_df <- function(x) {
  as_airdas_df(.airdas_process_comments(x))
}



.airdas_process_comments <- function(x) {
  stopifnot(inherits(x, "airdas_df") | inherits(x, "airdas_dfr"))
  
  # x.c <- airdas_comments(x)
  
  NULL
}
