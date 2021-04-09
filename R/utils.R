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
  colors() %>%
    purrr::set_names() %>%
    grDevices::col2rgb() %>%
    grDevices::rgb2hsv() %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("color_name") %>%
    filter(v > 0.5, s > 0.25) %>%
    filter(
      !stringr::str_detect(color_name, "(gra|ey)|white|brown"),
      !(color_name %in% exclude_colors)
    ) %>%
    pull(color_name) %>%
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

#' Get the average hex color from a list of colors
#'
#' First the average hue is calculated by averaging r, g and b components of the
#' colors and converted to HSL. Then, this hue is combined with average s and l
#' in order to avoid muddy colors. Finally, the combined HSV is converted to
#' hex.
#'
#' @param color_names Vector of color names (from [grDevices::colors()]).
#'
#' @return Average color in hex form.
average_color <- function(color_names) {
  color_table <- tibble(color = color_names) %>%
    rowwise() %>%
    mutate(rgb = list(purrr::set_names(grDevices::col2rgb(color), c("r", "g", "b"))),
           hsv = list(purrr::set_names(grDevices::rgb2hsv(rgb), c("h", "s", "v")))) %>%
    ungroup() %>%
    tidyr::unnest_wider(rgb) %>%
    tidyr::unnest_wider(hsv)

  average_color <- color_table %>%
    select(-color) %>%
    summarise_all(mean)
  average_hue <- with(average_color, grDevices::rgb2hsv(r, g, b, maxColorValue = 255) %>% as.vector())[1]
  #average_rgb =
  with(average_color, grDevices::rgb(r, g, b, maxColorValue = 255))
  #average_hsv = with(average_color, colorspace::HSV(h * 360, s, v) %>% colorspace::hex())
  #average_hue =with(average_color, colorspace::HSV(average_hue * 360, s, v) %>% colorspace::hex())
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
    hrefs <- search_term %>%
      stringr::str_replace_all("[^[:alnum:][:space:]]", "") %>%
      stringr::str_replace_all(" ", "+") %>%
      utils::URLencode() %>%
      glue::glue("https://www.bing.com/images/search?q={search_term}&qft=+filterui%3aaspect-wide", search_term = .) %>%
      xml2::read_html() %>%
      rvest::html_nodes(css = "a.iusc") %>%
      rvest::html_attr("m") %>%
      stringr::str_match(".*murl\":\"(.*?)\"") %>%
      `[`(, 2) %>%
      purrr::keep(!(. %in% suppressMessages(tools::showNonASCII(.))))
    if (length(hrefs) == 0) {
      get_transparent_pic()
    } else {
      hrefs[1]
    }
  })
}


#' Add Bing image search images to questions with missing images
#'
#' @param questions A data frame with questions (see [demo_questions] for
#'   format).
#'
#' @return A data frame in which missing values in image variables have been
#'   replaces with URLs generated by [get_first_bing_image()].
#' @export
add_bing_images <- function(questions) {
  fun_environment <- environment()
  purrr::walk(LETTERS[1:4], function(letter) {
    answer_column <-  paste0("answer_", letter)
    image_column <- paste0("image_", letter)
    missing_rows <- which(is.na(questions[, image_column]))
    search_terms <- unname(unlist(questions[missing_rows, answer_column]))
    search_results <- get_first_bing_image(search_terms)
    questions[missing_rows, image_column] <- search_results
    assign("questions", value = questions, envir = fun_environment)
  })
  questions
}
