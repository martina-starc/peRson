quiz.env <- new.env(parent = emptyenv())
quiz_setup <- function(questions, participants, presence = NULL, shuffle = TRUE, create_sheets = TRUE) {
  dir.create("quiz")
  quiz <- list()
  if (is.null(presence)) {
     quiz$presence <- participants$name
  } else {
     quiz$presence <- presence
  }
  quiz$css_file <- system.file("css", "styles.css", package = "peRson")
  quiz$all_questions <- questions %>% {
    if (shuffle) shuffle_answers(.) else .
  }
  quiz$questions <- quiz$all_questions %>%
    filter(person %in% quiz$presence) %>%
    {
      if (shuffle) shuffle_questions(.) else .
    }
  quiz$participants <- participants %>%
    mutate(chose_color = !is.na(color)) %>%
    split(.$chose_color) %>%
    purrr::map(process_colors) %>%
    bind_rows() %>%
    mutate(dist = if_else(chose_color, dist, NA_real_)) %>%
    mutate(present = name %in% quiz$presence)
  if (create_sheets) {
    quiz$participants$answer_sheet <- purrr::pmap_chr(quiz$participants, function(name, email, present, ...) {
      if (present) {
        create_answer_sheet(name, email, length(quiz$questions))
      } else {
        NA_character_
      }
    })
    quiz$summary_sheet_id <- create_summary_sheet(quiz$participants, length(quiz$questions))
  }
  quiz$named_colors <- with(quiz$participants, c(purrr::set_names(hex, name),
    "avg" = grDevices::rgb(mean(r[chose_color], na.rm = TRUE),
      mean(g[chose_color], na.rm = TRUE),
      mean(b[chose_color], na.rm = TRUE),
      maxColorValue = 255
    )
  ))
  quiz$question_colors <- quiz$named_colors[quiz$questions$person]

  quiz$answers <- list()
  purrr::walk(names(quiz), ~ assign(., quiz[[.]], quiz.env))
}

get_color_dist <- function(r, g, b) {
  data.frame(r, g, b) %>%
    stats::dist(upper = TRUE) %>%
    as.matrix() %>%
    colMeans()
}

process_colors <- function(participants) {
  participants %>%
    rowwise() %>%
    mutate(color = if_else(is.na(color), random_color(exclude_colors = participants$color), color)) %>%
    mutate(
      rgb = list(purrr::set_names(grDevices::col2rgb(color), c("r", "g", "b"))),
      hex = gplots::col2hex(color)
    ) %>%
    ungroup() %>%
    tidyr::unnest_wider(rgb) %>%
    mutate(dist = get_color_dist(r, g, b))
}

random_color <- function(exclude_colors = NA) {
  colors()[!stringr::str_detect(colors(), "(gra|ey)|white") & !(colors() %in% exclude_colors)] %>%
    sample(1)
}

get_question_color <- function(n, question_colors, named_colors) {
  color <- question_colors[n]
  if (length(color) == 0 || is.na(color)) {
    color <- named_colors["avg"]
  }
  color
}

#' Randomly shuffle questions in groups
#'
#' Divides the questions into groups containing one question per participant,
#' then randomly shuffles the order of the questions within the group. This way
#' the order of the questions will be random, but a question from the same
#' participant won't appear again until a question from every participant is
#' used.
#'
#' @param questions Data frame containing quiz questions and a rn variable that
#'   represents the random sequence number of the questions that were provided
#'   by the same participant
#' @param shuffle_by Unquoted name of the variable used to create shuffling
#'   groups.
#'
#' @return data frame with shuffled questions
shuffle_questions <- function(questions, shuffle_by = rn) {
  questions %>%
    group_by({{ shuffle_by }}) %>%
    arrange({{ shuffle_by }}, sample(1:length({{ shuffle_by }}))) %>%
    ungroup() %>%
    mutate(n = row_number()) %>%
    select(n, everything())
}

shuffle_answers <- function(questions) {
  questions %>%
    tidyr::pivot_longer(
      cols = -c(person, text), names_to = c(".value", "type"),
      names_pattern = "(image|answer)_([A-D])"
    ) %>%
    group_by(person, text) %>%
    mutate(type = sample(type, 4)) %>%
    arrange(person, text, type) %>%
    tidyr::pivot_wider(id_cols = c(person, text), values_from = c(answer, image), names_from = type) %>%
    group_by(person) %>%
    mutate(rn = sample(1:length(person)))
}
