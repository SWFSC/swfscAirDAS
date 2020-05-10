#' Summarize AirDAS sightings by effort segment
#' 
#' Summarize number of sightings and animals for selected species by segment
#' 
#' @param x.list list; output of \code{\link{airdas_effort}}
#' @param sp.codes character; species code(s) to include in segdata. 
#'   These code(s) will be converted to lower case to match \code{\link{airdas_sight}} 
#' 
#' @details This function takes the output of \code{\link{airdas_effort}} and
#'   adds columns for the number of sightings (nSI) and number of animals (ANI)
#'   for selected species (selected via \code{sp.codes}) for each segment
#'   to the segdata element of \code{x.list}.
#'   However, only sightings with an included value of \code{TRUE}
#'   (included is a column in siteinfo) are included in the summaries.
#'   Having this step separate from \code{\link{airdas_effort}} allows users to
#'   personalize the included values as desired for their analysis.
#' 
#' @return A list, identical to \code{x.list} except for
#'   1) the nSI and ANI columns added to \code{x.list$segdata},
#'   one each for each element of \code{sp.codes}, and
#'   2) the included column of \code{x.list$siteinfo}, which has been set as
#'   \code{FALSE} for sightings of species not listed in \code{sp.codes}
#' 
#' @examples 
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.proc <- airdas_process(y)
#' y.cond <- airdas_effort(
#'   y.proc, method = "condition", 
#'   conditions = "Bft", seg.min.km = 0.05, num.cores = 1
#' )
#' 
#' airdas_effort_sight(y.cond, sp.codes = c("mn", "bm"))
#' 
#' @export
airdas_effort_sight <- function(x.list, sp.codes) {
  swfscDAS::das_effort_sight(x.list, sp.codes)
}
