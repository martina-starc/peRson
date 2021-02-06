random_colour <- function(exclude_colors = NA) {
  colors()[!stringr::str_detect(colors(), "(gra|ey)|white") & !(colors() %in% exclude_colors)] %>%
    sample(1)
}
random_color <- random_colour

process_colors <- function(data) {
  data %>%
    mutate(color = tidyr::replace_na(color, random_color())) %>%
    rowwise() %>%
    mutate(rgb = list(purrr::set_names(grDevices::col2rgb(color), c("r", "g", "b")))) %>%
    ungroup() %>%
    tidyr::unnest_wider(rgb) %>%
    mutate(dist =
      as.matrix(r, g, b) %>%
      stats::dist(upper = TRUE) %>%
      as.matrix() %>%
      colMeans()
    ) %>%
    mutate(avg = grDevices::rgb(mean(r), mean(g), mean(b), maxColorValue = 255))
}

quiz_setup <- function(questions, participants, presence = participants$person) {
  quiz <- list()
  quiz$all_questions <- questions %>%
    shuffle_answers()
  quiz$questions <- quiz$all_questions %>%
    filter(person %in% presence) %>%
    shuffle_questions()
  quiz$participants <- participants %>%
    process_colors()
  quiz$asked <- list()
  quiz$answers <- list()
}

