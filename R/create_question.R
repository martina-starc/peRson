create_question <- function(n) {
  question <- quiz$questions[n, ] %>% as.list
  bg_previous <- ifelse(n == 1, quiz$named_colors[["avg"]], quiz$named_colors[unlist(quiz$questions[n - 1, "person"])])
  bg_current <- quiz$named_colors[question$person]
  bg_next <- ifelse(n == nrow(quiz$questions), quiz$named_colors[["avg"]], quiz$named_colors[unlist(quiz$questions[n + 1, "person"])])
  html_doc <- with(question, glue::glue('
<!DOCTYPE html>
<html>
<head>
<link rel="stylesheet" href="{quiz$css_file}">
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
    <td><a class="current" href="q{n}.html"></a></td>
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
    <td colspan=4><img src="a{n}.png" onerror="this.onerror=null; this.src=\'{get_transparent_pic()}\'"></img></td>
  </tr>
  <tr>
    <td colspan=4><img src="l{n}.png" onerror="this.onerror=null; this.src=\'{get_transparent_pic()}\'"></img></td>
  </tr>
</table>
<table class="navigation">
  <tr>
    <td class="previous"><a class="previous" href="q{n-1}.html"></a></td>
    <td class="next"><a class="next" href="q{n+1}.html"></a></td>
  </tr>
</table>
</body>
</html>
'))

  html_file <- file(paste0("quiz/q", n, ".html"))
  writeLines(html_doc, html_file)
  close(html_file)

}
