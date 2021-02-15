run_demo_quiz <- function(create_sheets = FALSE) {
  quiz_setup(demo_questions, get_demo_participants(), create_sheets = create_sheets)
  purrr::walk(1:nrow(quiz.env$questions), create_question)
  show_contestants(quiz.env$participants$name)
  purrr::walk(1:nrow(quiz.env$questions), ~ evaluate_answers(demo_answers))
  favourite_question()
  favourite_result(demo_favourite)
  final_results()
  if (create_sheets) quiz_clean_drive()
}

get_demo_participants <- function() {
  demo_participants %>%
    mutate(image = system.file("pics", image, package = "peRson"))
}
