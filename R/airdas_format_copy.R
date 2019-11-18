#' Aerial DAS format requirements
#'
#' Save pdf document describing the required format of aerial DAS data
#'
#' @param file character vector, the name of the file where the format pdf will be saved.
#'   This argument is passed top the \code{to} argument of \link[base]{file.copy}
#' @param ... arguments passed to \link[base]{file.copy}, such as \code{overwrite}
#'
#' @details A wrapper function for \link[base]{file.copy}; 
#'   saves the pdf document describing the aerial DAS data format requirements.
#'   This function copies the pdf document located at \code{system.file("AirDAS_Format.pdf", package = "swfscAirDAS")}
#'   to \code{file}
#'
#' @return \code{TRUE} is writing of file was successful, and \code{FALSE} otherwise
#'
#' @examples
#' \dontrun{
#' airdas.format.pdf <- system.file("AirDAS_Format.pdf", package = "swfscAirDAS")
#' file.copy(from = airdas.format.pdf, to ="AirDAS_Format.pdf", overwrite = FALSE)
#' }
#'
#' @export
airdas_format_copy <- function(file, ...) {
  stopifnot(
    inherits(file, "character"), 
    length(file) == 1
  )
  
  file.copy(
    system.file("AirDAS_Format.pdf", package = "swfscAirDAS"), to = file, ...
  )
}
