create_question <- function(n) {
  question <- quiz$questions[n, ] %>% as.list
  bg_previous <- ifelse(n == 1, "black", quiz$named_colors[unlist(quiz$questions[n - 1, "person"])])
  bg_current <- quiz$named_colors[question$person]
  bg_next <- ifelse(n == nrow(quiz$questions), "black", quiz$named_colors[unlist(quiz$questions[n + 1, "person"])])

  html_doc <- with(question, glue::glue('
<!DOCTYPE html>
<html>
<head>
<link rel="stylesheet" href="./inst/styles.css">
<style>
a.previous:hover {{
  background-color: {bg_previous};
}}

a.next:hover {{
  background-color: {bg_next};
}}

</style>
</head>
<body>

<table id="question">
  <tr>
    <td colspan = 4, style = "background-color: {bg_current}"></td>
  </tr>
  <tr>
    <td class = "q", colspan = 4>{n}. {text}</td>
  </tr>
  <tr>
    <td>A: {answer_A}<img src = "{image_A}"></img></td>
    <td>B: {answer_B}<img src = "{image_B}"></img></td>
    <td>C: {answer_C}<img src = "{image_C}"></img></td>
    <td>D: {answer_D}<img src = "{image_D}"></img></td>
  </tr>
  <tr>
    <td colspan = 4><img src = "a{n}.png", onerror = "this.onerror=null; this.src=\'./inst/pics/transparent.png\'"></img></td>
  </tr>
</table>
<table style = "width: 100%">
  <tr>
    <td style = "width: 50%"><a class = "previous", href = "q{n-1}.html"></a></td>
    <td style = "width: 50%; text-align: right"><a class = "next", href = "q{n+1}.html"></a></td>
  </tr>
</table>
</body>
</html>
'))

  html_file <- file(paste0("q", n, ".html"))
  writeLines(html_doc, html_file)
  close(html_file)

}
