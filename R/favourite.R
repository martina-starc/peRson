#' Create an HTML file with the favourite question question
#'
#' Creates an HTML file with the table of asked questions and their sequence
#' numbers, so participants can vote about their favourite question. Also
#' contains navigation to the previous and next question.
#'
#' @param n Number of questions to include in the table.
#' @param quiz Quiz environment with quiz variables (uses answers (length),
#'   questions (n, person, text)).
#'
#' @return
#' @export
#'
#' @examples
favourite_question <- function(n = NULL, quiz = quiz.env) {
  if (is.null(n)) {
    n <- length(quiz$answers)
  }
  quiz$questions %>%
    head(n) %>%
    select(n, person, text) %>%
    favourite_table(n = n + 1, quiz = quiz)
}


#' Create an HTML file with the favourite question answer
#'
#' Creates an HTML file with the table of questions that were chosen as
#' favourites, ordered by number of votes. Also contains navigation to the
#' previous and next question.
#'
#' As the results of the favourite question are processed in this function, it
#' also saves favourite_result table to the quiz environment.
#'
#' @param answer Data frame with the answer. Must contain one column for each
#'   participant named with their name and one row with their answers (see
#'   [demo_favourite]).
#' @param n Number of questions in the quiz (excluding the favourite questions
#'   question).
#' @param quiz Quiz environment with quiz variables (uses answers (length),
#'   questions (n, person, text)).
#'
#' @return
#' @export
#'
#' @examples
favourite_result <- function(answer, n = NULL, quiz = quiz.env) {
  if (is.null(n)) {
    n <- length(quiz$answers)
  }
  fresult <- answer %>%
    tidyr::pivot_longer(everything()) %>%
    mutate(n = as.numeric(value)) %>%
    select(n, name) %>%
    count(n, name = "count") %>%
    left_join(
      quiz$questions %>%
        head(n) %>%
        select(n, person, text),
      by = "n"
    ) %>%
    arrange(desc(count)) %>%
    na.omit()
  assign("favourite_results", fresult, envir = quiz)
  fresult %>%
    select(n = count, person, text) %>%
    favourite_table(n = n + 2, quiz = quiz)
}


#' Create an HTML favourite question table
#'
#' @param questions Data frame with questions to include in the table (uses n,
#'   person, text)
#' @param n Page/question number
#' @param quiz Quiz environment with quiz variables (uses css_file,
#'   question_colors, named_colors).
#'
#' @return
#'
#' @examples
favourite_table <- function(questions, n, quiz = quiz.env) {
  args <- list(
    n = n,
    css_file = quiz$css_file,
    bg_previous = get_question_color(n - 1, question_colors = quiz$question_colors, named_colors = quiz$named_colors),
    bg_current = get_question_color(n, question_colors = quiz$question_colors, named_colors = quiz$named_colors),
    bg_next = get_question_color(n + 1, question_colors = quiz$question_colors, named_colors = quiz$named_colors)
  )
  args$questions <- questions %>%
    mutate(bgcolor = quiz$named_colors[person]) %>%
    purrr::pmap(function(bgcolor, text, n, ...) {
      glue::glue('
  <tr style="background-color:{plotly::toRGB(bgcolor, alpha = 0.5)}">
    <td class="fav-color" style="background-color: {bgcolor}"></td>
    <td class="fav-n">{n}</td>
    <td class="fav-text">{text}</td>
  </tr>')
    }) %>%
    paste(collapse = "\n")

  html_doc <- with(args, glue::glue('
<!DOCTYPE html>
<html>
<head>
<link rel="stylesheet" href="{css_file}">
<style>
a.previous:hover {{
  background-color: {bg_previous};
}}

a.current:hover {{
  background-color: {bg_current};
}}

a.next:hover {{
  background-color: {bg_next};
}}
</style>
</head>
<body>
<table class="navigation">
  <tr>
    <td><a class="current" href="Q{n}.html"></a></td>
  </tr>
</table>
<table id="question">
  <tr>
    <td colspan=4 style="background-color: {bg_current}"></td>
  </tr>
  <tr>
    <td class="q" colspan=4>Favourite question</td>
  </tr>
</table>
<table class="favourite">
{questions}
</table>
<table class="navigation">
  <tr>
    <td class="previous"><a class="previous" href="Q{n-1}.html"></a></td>
    <td class="next"><a class="next" href="Q{n+1}.html"></a></td>
  </tr>
</table>
</body>
</html>'))

  html_file <- file(paste0("quiz/Q", n, ".html"))
  writeLines(html_doc, html_file)
  close(html_file)
}
