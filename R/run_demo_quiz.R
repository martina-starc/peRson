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
#'   Drive as they would be in a real quiz and trashed at the end.
#'
#' @return Creates quiz HTML files and results images.
#' @export
run_demo_quiz <- function(questions = demo_questions,
                          participants = get_demo_participants(),
                          answers = demo_answers,
                          favourite = demo_favourite,
                          create_sheets = FALSE) {
  quiz <- quiz_setup(questions, participants, create_sheets = create_sheets)
  options(peRson.quiz = quiz)
  purrr::walk(1:nrow(quiz$questions), create_question)
  show_contestants(quiz$participants$name)
  purrr::walk(1:nrow(quiz$questions), ~ evaluate_answers(answers))
  favourite_question()
  favourite_result(favourite)
  final_results()
  if (create_sheets) quiz_clean_drive()
}

#' Demo quiz participants
#'
#' A dataset containing data for fictional quiz participants. Images from image
#' variable are included in the package. To get correct paths, use
#' [get_demo_participants()].
#'
#' @format A data frame with 7 rows and 4 variables: \describe{ \item{name}{name
#'   of the participant} \item{color}{chosen color of the participant}
#'   \item{image}{path to the participant's image} \item{email}{participant's
#'   email} }
"demo_participants"

#' Get demo participants data frame
#'
#' @return A data frame with demo participant and correct paths to their images.
#' @export
get_demo_participants <- function() {
  demo_participants %>%
    mutate(image = system.file("pics", image, package = "peRson"))
}

#' Demo quiz questions
#'
#' A dataset containing questions provided by fictional quiz participants.
#'
#' @format A data frame with 21 rows and 10 variables:
#' \describe{
#'   \item{person}{name of the participant who provided the question}
#'   \item{text}{question}
#'   \item{answer_A}{first answer choice}
#'   \item{answer_B}{second answer choice}
#'   \item{answer_C}{third answer choice}
#'   \item{answer_D}{fourth answer choice}
#'   \item{image_A}{path to the image for the first answer choice}
#'   \item{image_B}{path to the image for the first answer choice}
#'   \item{image_C}{path to the image for the first answer choice}
#'   \item{image_D}{path to the image for the first answer choice}
#' }
"demo_questions"


#' Demo quiz answers
#'
#' A dataset containing answers provided by fictional quiz participants.
#'
#' @format A data frame with 22 rows and 7 variables:
#' \describe{
#'   \item{Beverly}{Beverly's answers}
#'   \item{Caty}{Caty's answers}
#'   \item{Doug}{Doug's answers}
#'   \item{Drake}{Drake's answers}
#'   \item{Fawn}{Fawn's answers}
#'   \item{Spike}{Spike's answers}
#'   \item{Teddy}{Teddy's answers}
#' }
"demo_answers"
