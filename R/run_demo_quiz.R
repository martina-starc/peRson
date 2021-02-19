#' Run demo quiz
#'
#' Setup the quiz, create questions, evaluate answers, favourite question and
#' final results with demo data.
#'
#' @param questions A data frame with questions (see [demo_questions] for
#'   format).
#' @param participants A data frame with participants (see [demo_participants]
#'   for format).
#' @param answers A data frame with answers (see [demo_answers] for format).
#' @param favourite A data frame with favourite question (see [demo_favourite]
#'   for format).
#' @param create_sheets If TRUE, sheets for answers will be created on Google
#'   Drive as they would be in a real quiz and deleted at the end.
#'
#' @return Creates quiz HTML files and results images.
#' @export
#'
#' @examples
run_demo_quiz <- function(questions = demo_questions,
                          participants = get_demo_participants(),
                          answers = demo_answers,
                          favourite = demo_favourite,
                          create_sheets = FALSE) {
  quiz_setup(questions, participants, create_sheets = create_sheets)
  purrr::walk(1:nrow(quiz.env$questions), create_question)
  show_contestants(quiz.env$participants$name)
  purrr::walk(1:nrow(quiz.env$questions), ~ evaluate_answers(answers))
  favourite_question()
  favourite_result(favourite)
  final_results()
  if (create_sheets) quiz_clean_drive()
}


#' Get demo participants data frame
#'
#' @return A data frame with demo participant and correct paths to their images.
#' @export
get_demo_participants <- function() {
  demo_participants %>%
    mutate(image = system.file("pics", image, package = "peRson"))
}
