quiz_setup <- function(questions, participants, presence = participants$name) {
  quiz <- list()
  quiz$css_file <- system.file("css", "styles.css", package = "peRson")
  quiz$all_questions <- questions %>%
    shuffle_answers()
  quiz$questions <- quiz$all_questions %>%
    filter(person %in% presence) %>%
    shuffle_questions()
  quiz$participants <- participants %>%
    process_colors()
  quiz$named_colors <- with(quiz$participants, c(purrr::set_names(hex, name),
                                                 "avg" = grDevices::rgb(mean(r), mean(g), mean(b), maxColorValue = 255)))
  quiz$question_colors <- quiz$named_colors[quiz$questions$person]
  quiz$presence <- presence
  quiz$answers <- list()

  quiz <<- quiz
}

process_colors <- function(data) {
  data %>%
    rowwise() %>%
    mutate(color = if_else(is.na(color), random_color(data$color), color)) %>%
    mutate(rgb = list(purrr::set_names(grDevices::col2rgb(color), c("r", "g", "b"))),
           hex = gplots::col2hex(color)) %>%
    ungroup() %>%
    tidyr::unnest_wider(rgb) %>%
    mutate(dist =
             as.matrix(r, g, b) %>%
             stats::dist(upper = TRUE) %>%
             as.matrix() %>%
             colMeans()
    )
}

random_color <- function(exclude_colors = NA) {
  colors()[!stringr::str_detect(colors(), "(gra|ey)|white") & !(colors() %in% exclude_colors)] %>%
    sample(1)
}

#' Randomly shuffle questions in groups
#'
#' Divides the questions into groups containing one question per participant, then randomly shuffles the order of the questions within the group. This way the order of the questions will be random, but a question from the same participant won't appear again until a question from every participant is used.
#'
#' @param questions data frame containing quiz questions and a rn variable that represents the random sequence number of the questions that were provided by the same participant
#'
#' @return data frame with shuffled questions
shuffle_questions <- function(questions, shuffle_by = rn) {
  questions %>%
    group_by({{shuffle_by}}) %>%
    arrange({{shuffle_by}}, sample(1:length({{shuffle_by}}))) %>%
    ungroup() %>%
    mutate(n = row_number()) %>%
    select(n, everything())
}

shuffle_answers <- function(questions) {
  questions %>%
    tidyr::pivot_longer(cols = -c(person, text), names_to = c(".value", "type"),
                        names_pattern = "(image|answer)_([A-D])"
    ) %>%
    group_by(person, text) %>%
    mutate(type = sample(type, 4)) %>%
    arrange(person, text, type) %>%
    tidyr::pivot_wider(id_cols = c(person, text), values_from = c(answer, image), names_from = type) %>%
    group_by(person) %>%
    mutate(rn = sample(1:length(person)))
}
