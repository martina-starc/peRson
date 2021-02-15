#' Create an HTML file with the quiz question
#'
#' Creates an HTML file with the question, answers, answer images, image holders
#' for results and leaderboard and navigation to the previous and next question.
#'
#' @param n Sequence number of the quiz question.
#' @param quiz Quiz environment with quiz variables (uses questions,
#'   question_colors, named_colors, css_file).
#'
#' @return
#' @export
#'
#' @examples
create_question <- function(n, quiz = quiz.env) {
  question <- quiz$questions[n, ] %>% as.list()
  question$n <- n
  question$bg_previous <- get_question_color(n - 1, question_colors = quiz$question_colors, named_colors = quiz$named_colors)
  question$bg_current <- get_question_color(n, question_colors = quiz$question_colors, named_colors = quiz$named_colors)
  question$bg_next <- get_question_color(n + 1, question_colors = quiz$question_colors, named_colors = quiz$named_colors)
  question$css_file <- quiz$css_file

  html_doc <- with(question, glue::glue('
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
    <td class="q" colspan=4>{n}. {text}</td>
  </tr>
  <tr>
    <td>A: {answer_A}<img src="{image_A}"></img></td>
    <td>B: {answer_B}<img src="{image_B}"></img></td>
    <td>C: {answer_C}<img src="{image_C}"></img></td>
    <td>D: {answer_D}<img src="{image_D}"></img></td>
  </tr>
  <tr>
    <td colspan=4><img src="A{n}.png" onerror="this.onerror=null; this.src=\'{get_transparent_pic()}\'"></img></td>
  </tr>
  <tr>
    <td colspan=4><img src="L{n}.png" onerror="this.onerror=null; this.src=\'{get_transparent_pic()}\'"></img></td>
  </tr>
</table>
<table class="navigation">
  <tr>
    <td class="previous"><a class="previous" href="Q{n-1}.html"></a></td>
    <td class="next"><a class="next" href="Q{n+1}.html"></a></td>
  </tr>
</table>
</body>
</html>
'))

  html_file <- file(paste0("quiz/Q", n, ".html"))
  writeLines(html_doc, html_file)
  close(html_file)
}
