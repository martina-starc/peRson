create_answer_sheet <- function(name, email) {
  answer_table <- data.frame(N = 1:nrow(quiz$questions), Answer = NA, stringsAsFactors = FALSE)
  answer_sheet_id <- googlesheets4::gs4_create(paste0("Answers ", name), sheets = list("Answers" = answer_table))
  googledrive::drive_share(file = answer_sheet_id, role = "writer", type = "user", emailAddress = email)
  as.character(answer_sheet_id)
}

create_summary_sheet <- function(participants) {
  import_table <- participants %>%
    mutate(import_formula = googlesheets4::gs4_formula(glue::glue('=IMPORTRANGE("{answer_sheet}", "Answers!B2:B")'))) %>%
    select(name, import_formula) %>%
    tidyr::pivot_wider(names_from = name, values_from = import_formula)
  googlesheets4::gs4_create("Answers summary", sheets = list("Answers" = import_table)) %>%
  googlesheets4::sheet_resize(sheet = "Answers", nrow = nrow(quiz$questions) + 1) %>%
    as.character()
}

quiz_clean_drive <- function() {
  sheet_list <- googledrive::as_id(c(quiz$participants$answer_sheet_id, quiz$summary_sheet_id))
  purrr::walk(sheet_list, googledrive::drive_trash)
}






