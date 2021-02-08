header_div <- function(text, bgcolor) {
  glue::glue('<div class="header" style="background-color: {bgcolor};">{text}</div>')
}


final_results <- function() {

  who_asked <- quiz$questions %>%
    head(length(quiz$answers)) %>%
    select(n, person)

  results <- quiz$answers %>%
    bind_rows(.id = "n") %>%
    mutate(n = as.numeric(n))

  totals <- results %>%
    filter(name %in% quiz$presence) %>%
    group_by(name) %>%
    summarise(total = sum(correct, na.rm = T), .groups = "drop") %>%
    ungroup() %>%
    mutate(place = dense_rank(-total)) %>%
    arrange(place) %>%
    mutate(prob = 1 - pbinom(total - 1, size = length(quiz$answers), prob = 0.25),
           prob2 = 1 - ((1 - prob) ** length(name)))

  totals_question <- results %>%
    group_by(n) %>%
    summarise(total = sum(correct, na.rm = T), .groups = "drop") %>%
    ungroup() %>%
    left_join(who_asked, by = "n") %>%
    arrange(desc(total))

  badge_winners <- list()

  badge_winners$participate <- quiz$presence
  badge_winners$send <- unique(quiz$all_questions$person)

  badge_winners$random <- totals %>% filter(total > nrow(who_asked) / 4) %>% pull(name)
  badge_winners$cointoss <- totals %>% filter(prob < 0.05) %>% pull(name)
  badge_winners$monkey <- totals %>% filter(prob2 < 0.05) %>% pull(name)

  badge_winners$top3 <- totals %>% filter(place <= 3) %>% pull(name)
  badge_winners$winner <- totals %>% filter(place == 1) %>% pull(name)

  badge_winners$difficult <- totals_question %>% filter(total == min(total)) %>% distinct(person) %>% pull(person)
  badge_winners$easy <- totals_question %>% filter(total == max(total)) %>% distinct(person) %>% pull(person)
  badge_winners$favourite <- quiz$favourite_results %>% filter(count == max(count)) %>% distinct(person) %>% pull(person)

  badge_winners$mean <- quiz$participants %>% filter(dist == min(dist)) %>% distinct(name) %>% pull(name)
  badge_winners$unique <- quiz$participants %>% filter(dist == max(dist)) %>% distinct(name) %>% pull(name)

  badge_winners <- badge_winners %>%
    purrr::map(~data.frame(name = .)) %>%
    bind_rows(`.id` = "badge") %>%
    mutate(importance = factor(badge, levels = c("winner", "top3", "monkey", "cointoss", "random", "favourite", "difficult", "easy", "unique", "mean", "send", "participate"))) %>%
    arrange(importance) %>%
    split(.$name) %>%
    purrr::map(~pull(., badge))

  badge_labels <- purrr::set_names(c("Quiz winner", "Top 3", "Above monkeys", "p < 0.05", "Above random", "Favourite question", "Difficult question", "Easy question", "Unique colour", "Average color", "Sent questions", "Participated"),
                            c("winner", "top3", "monkey", "cointoss", "random", "favourite", "difficult", "easy", "unique", "mean", "send", "participate"))

  sorted_names <- badge_winners %>%
    purrr::map(length) %>%
    bind_rows() %>%
    tidyr::pivot_longer(everything()) %>%
    group_by(value) %>%
    arrange(desc(value), sample(1:length(value))) %>%
    pull(name)



  person_tables <- sorted_names %>%
    purrr::map(function(name) {
      n_badges <- length(badge_winners[[name]])
      purrr::map(badge_winners[[name]], function(badge) {
        default_badge <- system.file("pics", glue::glue("black_{badge}.png"), package = "peRson")
        glue::glue('<img src="{name}_{badge}.png" alt="{badge_labels[[badge]]}" style="display:inline-block" onerror="this.onerror=null; this.src=\'{default_badge}\'"></img>', name = name, badge = badge)
      }) %>%
      paste(collapse = "") %>%
      glue::glue('
<table style="width: {n_badges * 66 + 160}px; margin:0px; padding:0px">
  <tr style="margin: 0px; padding: 0px">
    <td style = "width: 130px; margin: 0px; padding: 2px"><img src = ""></img></td>
    <td style = "width: {n_badges * 66 + 30}px; text-align: left"><p style = "font-size: 20px; vertical-align: top; padding-bottom: 10px; margin: 0px">{name}<br></p>{pics}</td>
  </tr>
</table>', pics = .)
    })

  middle_user <- ceiling(length(person_tables) / 2)
  width_col1 <- length(badge_winners[[sorted_names[1]]]) * 66 + 160
  width_col2 <- length(badge_winners[[sorted_names[middle_user]]]) * 66 + 160

  html_doc <- purrr::map2(person_tables[1:middle_user], person_tables[(middle_user + 1):(length(person_tables) + length(person_tables) %% 2)],
       ~glue::glue('
<tr style="background-color: white; margin: 0px; padding: 0px">
  <td style = "width: {width_col1}px; margin: 0px; padding: 0px">{.x}</td>
  <td style = "width: {width_col2}px; margin: 0px; padding: 0px">{ifelse(is.null(.y), "", .y)}</td>
</tr>')) %>%
    unlist() %>%
    paste(collapse = "") %>%
    glue::glue('
<table style = "margin: auto;">
{tables}
</table>', tables = .)

  html_file <- file(paste0("q", length(quiz$answers) + 2, ".html"))
  writeLines(html_doc, html_file)
  close(html_file)

}
