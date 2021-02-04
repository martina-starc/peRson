#' Randomly shuffle questions in groups
#'
#' Divides the questions into groups containing one question per participant, then randomly shuffles the order of the questions within the group. This way the order of the questions will be random, but a question from the same participant won't appear again until a question from every participant is used.
#'
#' @param questions data frame containing quiz questions and a rn variable that represents the random sequence number of the questions that were provided by the same participant
#'
#' @return data frame with shuffled questions
shuffle_questions <- function(questions) {
  questions %>%
    group_by(rn) %>%
    arrange(rn, sample(1:length(rn))) %>%
    ungroup() %>%
    mutate(n = row_number()) %>%
    select(n, everything())
}
