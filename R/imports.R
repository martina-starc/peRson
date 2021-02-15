#' @import dplyr
NULL


#' Demo quiz participants
#'
#' A dataset containing data for fixional quiz participants.
#'
#' @format A data frame with 7 rows and 4 variables:
#' \describe{
#'   \item{name}{name of the participant}
#'   \item{color}{chosen color of the participant}
#'   \item{image}{path to the participant's image}
#'   \item{email}{participant's email}
#' }
"demo_participants"


#' Demo quiz questions
#'
#' A dataset containing questions provided by fixional quiz participants.
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
#' A dataset containing answers provided by fixional quiz participants.
#'
#' @format A data frame with 21 rows and 7 variables:
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


#' Demo quiz favourite question
#'
#' A dataset containing the favourite question provided by fixional quiz participants.
#'
#' @format A data frame with 1 row and 7 variables:
#' \describe{
#'   \item{Beverly}{Beverly's favourite question}
#'   \item{Caty}{Caty's favourite question}
#'   \item{Doug}{Doug's favourite question}
#'   \item{Drake}{Drake's favourite question}
#'   \item{Fawn}{Fawn's favourite question}
#'   \item{Spike}{Spike's favourite question}
#'   \item{Teddy}{Teddy's favourite question}
#' }
"demo_favourite"
