#' Evaluate the correctness of question answers
#'
#' @param answers A data frame with the answers (see [demo_answers] for format).
#'   If NULL, answers are read from the quiz summary sheet (summary_sheet_id)
#'   created by [quiz_setup()].
#' @param n Sequence number of the question to evaluate. If NULL, the next
#'   question based on the length of saved answers in [quiz.env] is evaluated.
#' @param correct_answer Correct answer to the question. If NULL, the answer of
#'   the person who asked the question is taken as the correct answer.
#' @param quiz Quiz environment with quiz variables (uses summary_sheet_id,
#'   answers, questions).
#'
#' @return Evaluated answers are saved to quiz.env answers list. A plot with
#'   results is printed and saved to "quiz/A{n}.png".
#' @export
#'
#' @examples
evaluate_answers <- function(answers = NULL, n = NULL, correct_answer = NULL, quiz = quiz.env) {
  if (is.null(answers)) {
    answers <- suppressMessages(read_sheet(quiz$summary_sheet_id, sheet = "Answers"))
  }
  n <- if_else(is.null(n), length(quiz$answers) + 1, n)

  answer <- answers[n, ]
  correct_answer <- if_else(is.null(correct_answer), answer[[quiz$questions[[n, "person"]]]], correct_answer)

  answer <- answer %>%
    tidyr::pivot_longer(everything()) %>%
    mutate(correct = value == correct_answer)

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
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0, add = c(0, 1)), breaks = 0:100, minor_breaks = NULL) +
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
  if (n %% 4 == 0) show_leaderboard(n = n, quiz = quiz)
}


#' Create the current leaderboard
#'
#' @param answers Current list of evaluated answers. If NULL, taken from quiz
#'   environment.
#' @param n Current question. If NULL, estimated based on the length of saved
#'   answers in quiz environment.
#' @param quiz Quiz environment with quiz variables (uses answers, named_colors,
#'   presence)
#'
#' @return A plot with leaderboard is printed and saved to "quiz/L{n}.png".
#' @export
#'
#' @examples
show_leaderboard <- function(answers = NULL, n = NULL, quiz = quiz.env) {
  if (is.null(answers)) {
    answers <- quiz$answers
  }
  if (is.null(n)) {
    n <- length(quiz$answers)
  }

  current_results <- answers %>%
    bind_rows() %>%
    filter(n <= n) %>%
    group_by(name) %>%
    filter(name %in% quiz$presence) %>%
    summarise(total = sum(correct, na.rm = T), `.groups` = "drop") %>%
    mutate(name = forcats::fct_reorder(name, total, `.desc` = T))

  leaderboard_plot <- current_results %>%
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
      plot.title = ggplot2::element_text(size = 15, hjust = 0.5)
    )
  plot_width <- length(quiz$presence) * 0.6 + 0.3
  ggplot2::ggsave(filename = paste0("quiz/L", n, ".png"), plot = leaderboard_plot, height = 4, width = plot_width)
}
