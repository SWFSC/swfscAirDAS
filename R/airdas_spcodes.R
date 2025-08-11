#' Read AirDAS SpCodes file
#' 
#' Read AirDAS SpCodes file
#' 
#' @name airdas_spcodes
#' 
#' @param file character; filename of .dat file from which to read accepted
#'   species codes
#' @param skip integer; default is 3. 
#'   Number of lines to skip when reading SpCodes file. 
#'   See [readr::read_fwf()] for more details
#'   
#' @details
#' Provide a standardized function to read an aerial survey DAS SpCodes file.
#' Methods described in 'returns'
#' 
#' @returns Data frame with three columns: 
#' * code: species code, columns 1 to 6
#' * common_name: species common name, columns 10 to 42
#' * sci_name: species scientific name, columns 43 until the end of the line
#' 
#' @examples
#' sp.codes.file <- system.file(
#'   "extdata", "SpCodesAirDAS.dat", package = "swfscAirDAS"
#' )
#' if (interactive()) airdas_spcodes_read(sp.codes.file)
#' 
#' @export
airdas_spcodes_read <- function(file, skip = 3) {
  read_fwf(
    file, 
    # col_positions = fwf_positions(start = c(1, 10, 43), end = c(6, 42, NA)),
    col_positions = fwf_cols(
      code = c(1, 6), 
      common_name = c(10, 42), 
      sci_name = c(43, NA_integer_)
    ), 
    col_types = cols(.default = col_character()),
    trim_ws = TRUE, 
    skip = skip, 
    skip_empty_rows = FALSE
  )
}
