#' Get the correct system path to transparent picture
#'
#' Transparent picture is used as a placeholder for quiz answers and
#' leaderboard. It is supplied with the package, but needs to be accessed with
#' [base::system.file()].
#'
#' @return System path to the transparent picture.
get_transparent_pic <- function() {
  system.file("pics", "transparent.png", package = "peRson")
}

#' HTML code for header div
#'
#' @param text Text to include in the header.
#' @param bgcolor Background color of the header (CSS compatible string).
#'
#' @return HTML div element
header_div <- function(text, bgcolor) {
  glue::glue('<div class="header" style="background-color: {bgcolor};">{text}</div>')
}
