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
