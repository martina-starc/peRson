favourite_question <- function(n) {
  css_file <- system.file("css", "styles.css", package = "peRson")
  html_doc <- quiz$questions %>%
    head(n) %>%
    select(n, person, text) %>%
    mutate(bgcolor = quiz$named_colors[person]) %>%
    purrr::pmap(function(bgcolor, n, text, ...) {
      glue::glue('
  <tr>
    <td class="fav-color" style="background-color: {bgcolor}"></td>
    <td class="fav-n">{n}.</td>
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

  html_file <- file(paste0("q", n + 1, ".html"))
  writeLines(html_doc, html_file)
  close(html_file)

}



