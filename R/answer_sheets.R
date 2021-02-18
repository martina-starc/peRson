#' Creates and shares an answer sheet for the participant
#'
#' The function creates a Google Sheet on Google Drive containing a table with
#' the question number and an empty Answer column. The sheet has n_questions + 1
#' rows (additional row is for the favourite question). Write access is given to
#' the participant so they may enter their answers during the quiz.
#'
#' @param name Name of the participant (string).
#' @param email Participant's e-mail address (string).
#' @param n_questions Number of questions in the quiz.
#'
#' @return The id of the sheet as a character string.
#' @export
#'
#' @examples
create_answer_sheet <- function(name, email, n_questions) {
  answer_table <- data.frame(N = 1:(n_questions + 1), Answer = NA, stringsAsFactors = FALSE)
  answer_sheet_id <- googlesheets4::gs4_create(paste0("Answers ", name), sheets = list("Answers" = answer_table))
  drive_dribble <- googledrive::drive_share(answer_sheet_id, role = "writer", type = "user", emailAddress = email)
  as.character(answer_sheet_id)
}


#' Create a summary answer sheet
#'
#' The function creates a Google Sheet on Google Drive containing links to the
#' participant answer sheets (created with [create_answer_sheet()]). The sheet
#' contains one column per participant and n_questions + 1 rows (additional row
#' is for the favourite question). Import range functions are inserted into the
#' first row, however, you need to go to the file and manually link the sheets
#' by clicking on cells with REF!.
#'
#' @param participants Character vector with the names of the participants.
#' @param n_questions Number of questions in the quiz.
#'
#' @return The id of the sheet as a character string.
#' @export
#'
#' @examples
create_summary_sheet <- function(participants, n_questions) {
  import_table <- participants %>%
    mutate(import_formula = googlesheets4::gs4_formula(glue::glue('=IMPORTRANGE("{answer_sheet}", "Answers!B2:B")'))) %>%
    select(name, import_formula) %>%
    tidyr::pivot_wider(names_from = name, values_from = import_formula)
  googlesheets4::gs4_create("Answers summary", sheets = list("Answers" = import_table)) %>%
    googlesheets4::sheet_resize(sheet = "Answers", nrow = n_questions + 2) %>%
    as.character()
}


#' Deletes the answer sheets
#'
#' Puts the participants' answer sheets and the summary answer sheet into drive trash.
#'
#' @param sheet_ids Vector of string sheet ids.
#'
#' @return No result, just a drive cleaned of quiz traces.
#' @export
#'
#' @examples
quiz_clean_drive <- function(sheet_ids = c(quiz.env$participants$answer_sheet, quiz.env$summary_sheet_id)) {
  sheet_ids %>%
    purrr::walk(~ googledrive::drive_rm(googledrive::as_id(.)))
}


#' Fill demo answer sheets with answers for testing purposes
#'
#' @param quiz Quiz environment with quiz variables (uses participants).
#' @param answers Answers to write into the sheets (see [demo_answers] for
#'   format).
#'
#' @return Fills the answer sheets created by [quiz_setup()] with answers, so
#'   you can test [evaluate_answers()].
#' @export
fill_demo_answer_sheets <- function(quiz = quiz.env, answers = demo_answers) {
  quiz.env$participants %>%
    filter(present) %>%
    purrr::pwalk(function(name, answer_sheet, ...) {
      googlesheets4::range_write(answer_sheet,
        data = answers[name],
        sheet = "Answers", range = "B2",
        col_names = FALSE
      )
    })
}
