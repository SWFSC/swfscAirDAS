#' Extract comments from AirDAS data
#' 
#' Extract comments from \code{airdas_dfr} or \code{airdas_df} object
#' 
#' @param x \code{airdas_dfr} or \code{airdas_df} object, 
#'  or a data frame that can be coerced to a \code{airdas_dfr} object
#'  
#' @details This function recreates the comment strings by 
#'   pasting the Data# columns back together for the C events (comments)
#'   
#'   See the examples section for how to search for comments with the
#'   phrase "record" to determine what extra information (e.g. molas)
#'   was being recorded vs ignored.
#' 
#' @return A data frame with the comment strings in column comment_str, 
#'   as well as the following columns from \code{x}: 
#'   file_das, line_num, and DateTime
#' 
#' @examples 
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.read <- airdas_read(y, file.type = "turtle")
#' 
#' airdas_comments(y.read)
#' 
#' # Extract all comments containing "record"
#' y.comm <- airdas_comments(y.read)
#' y.comm[grepl("record", y.comm$comment_str, ignore.case = TRUE), ]
#' 
#' # Extract all comments containing "record", but not "recorder"
#' y.comm <- airdas_comments(y.read)
#' y.comm[grepl("record", y.comm$comment_str, ignore.case = TRUE) & 
#'          !grepl("recorder", y.comm$comment_str, ignore.case = TRUE), ]
#' 
#' @export
airdas_comments <- function(x) UseMethod("airdas_comments")


#' @name airdas_comments
#' @export
airdas_comments.data.frame <- function(x) {
  airdas_comments(as_airdas_dfr(x))
}


#' @name airdas_comments
#' @export
airdas_comments.airdas_df <- function(x) {
  airdas_comments(as_airdas_dfr(x))
}


#' @name airdas_comments
#' @export
airdas_comments.airdas_dfr <- function(x) {
  x.c <- x[x$Event == "C", ]
  x.c.c <- apply(x.c[, paste0("Data", 1:7)], 1, function(i) {
    paste(na.omit(i), collapse = "")
  })
  
  data.frame(
    file_das = x.c$file_das, line_num = x.c$line_num, 
    DateTime = x.c$DateTime, comment_str = x.c.c, 
    stringsAsFactors = FALSE
  )  
}
