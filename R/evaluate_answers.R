evaluate_answers <- function(answers, n = length(quiz$answers) + 1, correct_value = NULL) {

  #answer <- suppressMessages(read_sheet(source_sheet, sheet =  "Answers"))[n, ] # needs to be Answers for real
  answer <- answers[n, ]
  correct_value <- ifelse(is.null(correct_value), answer[[quiz$questions[[n, "person"]]]], correct_value)

  answer <- answer %>%
    tidyr::pivot_longer(everything()) %>%
    mutate(correct = value == correct_value)

  quiz$answers[[n]] <<- answer
  #suppressMessages(save_tmp(answers))

  plot_height <- answer %>%
    group_by(value) %>%
    summarise(n = n()) %>%
    pull(n) %>%
    max() %>%
    c(. / 2 + 0.5, 5) %>%
    min()

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
    #labs(title = glue('{n}. {questions[n, "text"]}')) +
    #ggplot2::labs(title = n) +
    ggplot2::theme(
      plot.background = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = 10),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(size = 10))

  ggplot2::ggsave(filename = paste0("a", n, ".png"), plot = results_plot, height = plot_height)
  print(results_plot)
  #if(n %% 4 == 0) show_leaderboard()
}

evaluate_answers(demo_answers, 1)
