show_leaderboard <- function(answers = quiz$answers, n = length(quiz$answers)) {
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
    ggplot2::labs(title = 'Leaderboard') +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = 10),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(size = 15))
  print(leaderboard_plot)
  ggplot2::ggsave(filename = paste0("l", n, ".png"), plot = leaderboard_plot, height = 5)

}
