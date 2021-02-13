favourite_question <- function(n = length(quiz$answers)) {
  quiz$questions %>%
    head(n) %>%
    select(n, person, text) %>%
    favourite_table(n = n + 1)
}

favourite_result <- function(answer, n = length(quiz$answers)) {
  fresult <- answer %>%
    tidyr::pivot_longer(everything()) %>%
    mutate(n = as.numeric(value)) %>%
    select(n, name) %>%
    count(n, name = "count") %>%
    left_join(
      quiz$questions %>%
        head(n) %>%
        select(n, person, text), by = "n"
    ) %>%
    arrange(desc(count)) %>%
    na.omit()
  quiz$favourite_results <<- fresult
  fresult %>%
    select(n = count, person, text) %>%
    favourite_table(n = n + 2)
}

favourite_table <- function(questions, n) {
  html_doc <- questions %>%
    mutate(bgcolor = quiz$named_colors[person]) %>%
    purrr::pmap(function(bgcolor, text, n, ...) {
      glue::glue('
  <tr style="background-color:{plotly::toRGB(bgcolor, alpha = 0.5)}">
    <td class="fav-color" style="background-color: {bgcolor}"></td>
    <td class="fav-n">{n}</td>
    <td class="fav-text">{text}</td>
  </tr>')
    }) %>%
    paste(collapse = "\n") %>%
    glue::glue('
<!DOCTYPE html>
<html>
<head>
<link rel="stylesheet" href="{quiz$css_file}">
</head>
<body>
<table class="navigation">
  <tr>
    <td><a class="current" href="Q{n}.html"></a></td>
  </tr>
</table>
<table id="question">
  <tr>
    <td colspan=4 style="background-color: {quiz$named_colors[[\"avg\"]]}"></td>
  </tr>
  <tr>
    <td class="q" colspan=4>Favourite question</td>
  </tr>
</table>
<table class="favourite">
{rows}
</table>
<table class="navigation">
  <tr>
    <td class="previous"><a class="previous" href="Q{n-1}.html"></a></td>
    <td class="next"><a class="next" href="Q{n+1}.html"></a></td>
  </tr>
</table>
</body>
</html>', rows = .)

  html_file <- file(paste0("Q", n, ".html"))
  writeLines(html_doc, html_file)
  close(html_file)

}



