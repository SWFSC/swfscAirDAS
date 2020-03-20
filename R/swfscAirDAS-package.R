#' Process and summarize aerial survey DAS data
#'
#' This package contains functions designed for processing and analyzing 
#' aerial survey DAS data (AirDAS) generated using the TURTLEP program (or associated programs) 
#' by the Southwest Fisheries Science Center. 
#' Functionality includes reading AirDAS data into a data frame, 
#' processing this data (extracting state and condition information for each AirDAS event), 
#' and summarizing sighting and effort information.
#'
#' @name swfscAirDAS-package
#' @aliases swfscAirDAS
#' @docType package
#' @title Southwest Fisheries Science Center Aerial Survey DAS
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#' @seealso \url{https://smwoodman.github.io/swfscAirDAS/}
#'
#' @importFrom dplyr arrange between bind_cols bind_rows case_when everything
#' @importFrom dplyr filter full_join group_by left_join mutate select starts_with summarise
#' @importFrom lubridate year month day tz
#' @importFrom magrittr %>%
#' @importFrom parallel clusterExport detectCores parLapplyLB stopCluster
#' @importFrom readr cols col_character read_fwf fwf_positions
#' @importFrom rlang !! .data
#' @importFrom stats na.omit runif
#' @importFrom swfscDAS .chop_condition_eff .chop_equal_eff .dist_from_prev .fn.grcirclkm
#' @importFrom swfscMisc bearing destination distance setupClusters
#' @importFrom utils head read.csv write.csv 
#' 
#' @keywords package
NULL

setOldClass("airdas_dfr")
setOldClass("airdas_df")
