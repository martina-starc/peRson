shuffle_questions <- function(data) {
  data %>%
    group_by(rn) %>%
    arrange(rn, sample(1:length(rn))) %>%
    ungroup() %>%
    mutate(n = row_number()) %>%
    select(n, everything())
}
