quiz_setup <- function(questions, participants, presence = participants$name) {
  quiz <- list()
  quiz$all_questions <- questions %>%
    shuffle_answers()
  quiz$questions <- quiz$all_questions %>%
    filter(person %in% presence) %>%
    shuffle_questions()
  quiz$participants <- participants %>%
    process_colors()
  quiz$named_colors <- with(quiz$participants, c(purrr::set_names(hex, name),
                                                 "avg" = grDevices::rgb(mean(r), mean(g), mean(b), maxColorValue = 255)))
  quiz$presence <- presence
  quiz$answers <- list()
  quiz <<- quiz
}
