favourite_question <- function(n) {
  quiz$questions %>%
    head(n) %>%
    select(n, person, text)
}

favourite_result <- function(answer, n) {
  fresult <- answer %>%
    tidyr::pivot_longer(everything()) %>%
    mutate(n = as.numeric(value)) %>%
    select(n, name) %>%
    count(n, name = "count") %>%
    left_join(
      favourite_question(n), by = "n"
    ) %>%
    arrange(desc(count)) %>%
    filter(!is.na(count))
  quiz$favourite_results <<- fresult
  fresult %>%
    select(n = count, person, text)
}

favourite_table <- function(questions, n) {
  css_file <- system.file("css", "styles.css", package = "peRson")
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
<link rel="stylesheet" href="{css_file}">
</head>
<body>
<table class="favourite">
{rows}
</table>
</body>
</html>', rows = .)

  html_file <- file(paste0("q", n, ".html"))
  writeLines(html_doc, html_file)
  close(html_file)

}



