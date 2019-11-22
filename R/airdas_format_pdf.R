#' Aerial DAS format requirements
#'
#' Save the pdf document describing the aerial DAS format required by \code{swfscAirDAS} to a specified file
#'
#' @param file character vector, the name of the file where the pdf will be saved
#' @param ... passed on to \code{\link[base:files]{file.copy}}, might included named argument \code{overwrite}
#'
#' @details A wrapper function for \code{\link[base:files]{file.copy}}. This function
#'   saves the pdf document describing the aerial DAS data format requirements by 
#'   copying the pdf document located at \code{system.file("AirDAS_Format.pdf", package = "swfscAirDAS")}
#'   to \code{file}
#'
#' @return \code{TRUE} if writing of file was successful, and \code{FALSE} otherwise
#'
#' @examples
#' \dontrun{
#' airdas_format_pdf("AirDAS_Format.pdf", overwrite = FALSE)
#' }
#'
#' @export
airdas_format_pdf <- function(file, ...) {
  stopifnot(
    inherits(file, "character"), 
    length(file) == 1
  )
  
  file.copy(
    system.file("AirDAS_Format.pdf", package = "swfscAirDAS"), to = file, ...
  )
}
