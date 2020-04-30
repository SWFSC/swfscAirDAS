#' Process and summarize aerial survey DAS data
#'
#' This package contains functions designed for processing and analyzing 
#' aerial survey DAS data (AirDAS) collected using one of the following 
#' Southwest Fisheries Science Center (SWFSC) programs: 
#' PHOCOENA, SURVEY, CARETTA, or TURTLE (such as TURTLEP or TURTLE 4D).
#' Functionality includes checking AirDAS data for data entry errors, 
#' reading AirDAS data into a data frame, processing this data 
#' (extracting state and condition information for each AirDAS event), 
#' and summarizing sighting and effort information.
#'
#' @name swfscAirDAS-package
#' @aliases swfscAirDAS
#' @docType package
#' @title Southwest Fisheries Science Center Aerial Survey DAS
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#' @seealso \url{https://smwoodman.github.io/swfscAirDAS/}
#'
#' @importFrom dplyr arrange between bind_cols bind_rows case_when everything filter full_join group_by 
#'   left_join mutate right_join select slice starts_with summarise ungroup
#' @importFrom lubridate year month day tz
#' @importFrom magrittr %>%
#' @importFrom parallel clusterExport detectCores parLapplyLB stopCluster
#' @importFrom readr cols col_character read_fwf fwf_positions
#' @importFrom rlang !! .data
#' @importFrom stats na.omit runif
#' @importFrom stringr str_detect str_match_all
#' @importFrom swfscDAS .chop_condition_eff .chop_equal_eff .dist_from_prev .fn.grcirclkm 
#'   .process_chr .process_num .segdata_aggr .segdata_proc
#' @importFrom swfscMisc bearing destination distance setupClusters
#' @importFrom tidyr unnest
#' @importFrom utils head tail read.csv write.csv 
#' 
#' @keywords package
NULL

setOldClass("airdas_dfr")
setOldClass("airdas_df")
