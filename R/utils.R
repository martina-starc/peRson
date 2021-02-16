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

#' Get the color for the question based on its sequence number
#'
#' Helper function for assembling the navigation bars.
#'
#' @param n The sequence number of the question.
#' @param question_colors List of question colors as created by [quiz_setup()].
#' @param named_colors Named list of colors as created by [quiz_setup()].
#'
#' @return The color of the question for sequence numbers within questions and
#'   average color otherwise.
get_question_color <- function(n, question_colors, named_colors) {
  color <- question_colors[n]
  if (length(color) == 0 || is.na(color)) {
    color <- named_colors["avg"]
  }
  color
}

#' Select a random color
#'
#' Excludes gray and white colors and colors from exclude_colors list and
#' returns one random color.
#'
#' @param exclude_colors Vector of color names (from [base::colors()]) to
#'   exclude from random selection.
#'
#' @return A random color name
random_color <- function(exclude_colors = NA) {
  colors()[!stringr::str_detect(colors(), "(gra|ey)|white") & !(colors() %in% exclude_colors)] %>%
    sample(1)
}

#' Calculate distances between colors in RGB space
#'
#' @param r Vector of values for red.
#' @param g Vector of values for green.
#' @param b Vector of values for blue.
#'
#' @return Vector containing the average distances of colors from other colors
#' in the vector.
get_color_dist <- function(r, g, b) {
  data.frame(r, g, b) %>%
    stats::dist(upper = TRUE) %>%
    as.matrix() %>%
    colMeans()
}

#' Get the URL of the first image search result on Bing
#'
#' Google makes it hard to scrape images, so Bing it is. :D
#'
#' @param search_term The search term to use in image search. Only
#'   alphanumerical characters and spaces will be included.
#'
#' @return Character string with the URL of the first image result or path to
#'   transparent image if there are no search results.
#'
#' @export
get_first_bing_image <- function(search_term) {
  purrr::map_chr(search_term, function(search_term) {
    search_term <- search_term %>%
      stringr::str_replace_all("[^[:alnum:][:space:]]", "") %>%
      stringr::str_replace_all(" ", "+")
    page <- xml2::read_html(glue::glue("https://www.bing.com/images/search?q={search_term}"))
    node <- rvest::html_nodes(page, css = "a.thumb")
    if(length(node) == 0) {
      get_transparent_pic()
    } else {
      rvest::html_attr(node[[1]], "href")
    }
  })
}
