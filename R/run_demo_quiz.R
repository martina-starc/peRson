run_demo_quiz <- function() {
  quiz_setup(demo_questions, demo_participants)
  purrr::walk(1:nrow(quiz$questions), create_question)
  show_contestants(quiz$participants$name)
  purrr::walk(1:nrow(quiz$questions), ~evaluate_answers(demo_answers))
  favourite_question()
  favourite_result(demo_favourite)
  final_results()
}
