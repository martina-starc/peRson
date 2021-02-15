evaluate_answers <- function(answers = NULL, n = NULL, correct_value = NULL, quiz = quiz.env) {
  if (is.null(answers)) {
    answers <- suppressMessages(read_sheet(quiz$summary_sheet_id, sheet = "Answers"))
  }
  n <- if_else(is.null(n), length(quiz$answers) + 1, n)

  answer <- answers[n, ]
  correct_value <- if_else(is.null(correct_value), answer[[quiz$questions[[n, "person"]]]], correct_value)

  answer <- answer %>%
    tidyr::pivot_longer(everything()) %>%
    mutate(correct = value == correct_value)

  quiz_answsers <- quiz$answers
  quiz_answsers[[n]] <- answer
  assign("answers", quiz_answsers, envir = quiz)
  # suppressMessages(save_tmp(answers))

  plot_height <- answer %>%
    group_by(value) %>%
    summarise(n = n()) %>%
    pull(n) %>%
    max() * 0.25 + 0.5

  results_plot <- answer %>%
    group_by(value) %>%
    mutate(rn = row_number()) %>%
    ungroup() %>%
    filter(!is.na(value)) %>%
    ggplot2::ggplot(ggplot2::aes(value)) +
    ggplot2::geom_bar(ggplot2::aes(fill = correct), width = 0.75) +
    ggplot2::geom_text(stat = "count", ggplot2::aes(label = ..count..), size = 3, vjust = -0.6) +
    ggplot2::geom_text(ggplot2::aes(y = rn - 0.5, label = name), size = 3) +
    ggplot2::scale_x_discrete(limits = LETTERS[1:4], expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.15)), breaks = 0:100, minor_breaks = NULL) +
    ggplot2::scale_fill_manual(values = c("FALSE" = "tomato2", "TRUE" = "green3"), guide = "none") +
    ggplot2::theme(
      plot.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 10)
    )

  ggplot2::ggsave(filename = paste0("quiz/A", n, ".png"), plot = results_plot, height = plot_height)
  if (n %% 4 == 0) show_leaderboard(n = n)
}

show_leaderboard <- function(answers = quiz$answers, n = length(quiz$answers), quiz = quiz.env) {
  leaderboard_plot <- answers %>%
    bind_rows() %>%
    filter(n <= n) %>%
    group_by(name) %>%
    filter(name %in% quiz$presence) %>%
    summarise(total = sum(correct, na.rm = T), `.groups` = "drop") %>%
    mutate(name = forcats::fct_reorder(name, total, `.desc` = T)) %>%
    ggplot2::ggplot(ggplot2::aes(name, total)) +
    ggplot2::geom_col(ggplot2::aes(fill = name)) +
    ggplot2::geom_text(ggplot2::aes(label = ..y..), size = 5, vjust = -0.6) +
    ggplot2::geom_hline(yintercept = length(answers) / 4) +
    ggplot2::scale_fill_manual(values = quiz$named_colors, guide = "none") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.15)), breaks = 0:100, minor_breaks = NULL) +
    ggplot2::labs(title = "Leaderboard") +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 15)
    )
  print(leaderboard_plot)
  ggplot2::ggsave(filename = paste0("quiz/L", n, ".png"), plot = leaderboard_plot, height = 5)
}
