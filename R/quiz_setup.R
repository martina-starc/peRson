#' Set up the quiz environment
#'
#' @param questions A data frame with questions (see [demo_questions] for
#'   format).
#' @param participants A data frame with participants (see [demo_participants]
#'   for format).
#' @param presence Vector of present participants' names. If NULL, all
#'   participants from participants data frame (participants$name) will be
#'   deemed present.
#' @param shuffle If TRUE, the order of answers and questions is randomly
#'   shuffled by [shuffle_answers()] and [shuffle_questions()] respectively.
#' @param create_sheets If TRUE, a Google Sheet for answers is created for and
#'   shared with each participant, plus one summary sheet containing imported
#'   answer sheets (though the sheets need to be linked manually in the sheet
#'   before answers can be read from it).
#'
#' @return A quiz environment with the following variables. \describe{
#'   \item{presence}{vector or present participants' names} \item{css_file}{path
#'   to css file used to style the HTML quiz files} \item{all_questions}{data
#'   frame with all the provided questions from the questions data frame
#'   (shuffled if shuffle is TRUE)} \item{questions}{data frame with questions
#'   that will be used in the quiz (only those whose askers are present)}
#'   \item{participants}{data frame with participants from the participants data
#'   frame (with added variables chose_color, dist, present and answer_sheet if
#'   create_sheets is TRUE)} \item{summary_sheet_id}{if create_sheets is TRUE
#'   character id of the created summary sheet} \item{named_colors}{vector of
#'   hex colors chosen by participants, named with their names, plus average
#'   color of chosen colors with "avg" name} \item{question_colors}{vector of
#'   hex colors for each question in questions based on the person who asked it}
#'   \item{answers}{empty list that will be filled with answers by
#'   [evaluate_answers()]} }
#' @export
#'
#' @examples
quiz_setup <- function(questions, participants, presence = NULL, shuffle = TRUE, create_sheets = TRUE) {
  dir.create("quiz")
  quiz <- list()
  quiz$presence <- if (is.null(presence)) participants$name else presence
  quiz$css_file <- system.file("css", "styles.css", package = "peRson")
  quiz$all_questions <- questions %>% {
    if (shuffle) shuffle_answers(.) else .
  }
  quiz$questions <- quiz$all_questions %>%
    filter(person %in% quiz$presence) %>%
    {
      if (shuffle) shuffle_questions(.) else mutate(., n = row_number())
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
        create_answer_sheet(name, email, nrow(quiz$questions))
      } else {
        NA_character_
      }
    })
    quiz$summary_sheet_id <- create_summary_sheet(quiz$participants, nrow(quiz$questions))
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

  quiz.env <- new.env(parent = emptyenv())
  list2env(quiz, envir = quiz.env)
}



#' Process colors chosen by participants
#'
#' For participants who didn't choose colors a random color is picked. Colors
#' are converted to RGB and hex to use in HTML files. Distance from other
#' participants' colors is calculated.
#'
#' @inheritParams quiz_setup
#'
#' @return A data frame with random colors chosen for participants who didn't
#'   choose colors, r, g, b columns with RGB values, hex with hex color value
#'   and dist with the average distance from other participants' colors.
#'
#' @examples
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


#' Randomly shuffle questions in groups
#'
#' Divides the questions into groups containing one question per participant,
#' then randomly shuffles the order of the questions within the group. This way
#' the order of the questions will be random, but a question from the same
#' participant won't appear again until a question from every participant is
#' used.
#'
#' @param questions A data frame with quiz questions (see [demo_questions] for
#'   format) and a rn variable that represents the random sequence number of the
#'   questions that were provided by the same participant.
#' @param shuffle_by Unquoted name of the variable used to create shuffling
#'   groups.
#'
#' @return A data frame with shuffled questions.
shuffle_questions <- function(questions, shuffle_by = rn) {
  questions %>%
    group_by({{ shuffle_by }}) %>%
    arrange({{ shuffle_by }}, sample(1:length({{ shuffle_by }}))) %>%
    ungroup() %>%
    mutate(n = row_number()) %>%
    select(n, everything())
}

#' Randomly assign answers to A-D
#'
#' The answers to questions are randomly assigned to letters A-D.
#'
#' @inheritParams quiz_setup
#'
#' @return A data frame with shuffled answers.
#'
#' @examples
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
