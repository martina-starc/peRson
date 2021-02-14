#quiz$questions wont work without globl variable in quiz setup
create_answer_sheet <- function(name, email, n_questions = nrow(quiz$questions)) {
  answer_table <- data.frame(N = 1:n_questions, Answer = NA, stringsAsFactors = FALSE)
  answer_sheet_id <- googlesheets4::gs4_create(paste0("Answers ", name), sheets = list("Answers" = answer_table))
  as.character(answer_sheet_id)
}

share_answer_sheet <- function(sheet_id, email) {
  googledrive::drive_share(file = googledrive::as_id(sheet_id), role = "writer", type = "user", emailAddress = email)
}

create_summary_sheet <- function(participants, n_questions = nrow(quiz$questions)) {
  import_table <- participants %>%
    mutate(import_formula = googlesheets4::gs4_formula(glue::glue('=IMPORTRANGE("{answer_sheet}", "Answers!B2:B")'))) %>%
    select(name, import_formula) %>%
    tidyr::pivot_wider(names_from = name, values_from = import_formula)
  googlesheets4::gs4_create("Answers summary", sheets = list("Answers" = import_table)) %>%
  googlesheets4::sheet_resize(sheet = "Answers", nrow = n_questions + 1) %>%
    as.character()
}

quiz_clean_drive <- function(sheet_ids = c(quiz$participants$answer_sheet_id, quiz$summary_sheet_id)) {
  sheet_list <- googledrive::as_id(sheet_ids)
  purrr::walk(sheet_list, googledrive::drive_trash)
}

