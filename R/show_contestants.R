show_contestants <- function(presence, n_per_row = 5) {

  person_tables <- quiz$participants %>%
    filter(name %in% presence) %>%
    arrange(sample(row_number())) %>%
    purrr::pmap(function(name, image, hex, ...) {
      glue::glue('
<td id = "contestant-wrapper", style = "width: {100 / n_per_row}%">
<table id = "contestant">
  <tr>
    <td style = "background-color: {hex}; height: 10px"></td>
  </tr>
  <tr>
    <td><img src = "{image}"></img></td>
  </tr>
  <tr>
    <td>{name}</td>
  </tr>
</table>
</td>
')
    })

  html_doc <- purrr::map(1:(length(person_tables) %/% n_per_row + ifelse(length(person_tables) %% n_per_row > 0, 1, 0)), ~person_tables[(. * n_per_row - n_per_row + 1):(. * n_per_row)]) %>%
    purrr::map(~paste(unlist(.), collapse = "")) %>%
    purrr::map(~paste0('<tr style = "background-color: white">', ., "</tr>\n")) %>%
    unlist() %>%
    paste(collapse = "") %>%
    glue::glue('
<!DOCTYPE html>
<html>
<head>
<link rel="stylesheet" href="./inst/styles.css">
<style>
table {{}
  width: {200 * n_per_row + 20 * (n_per_row - 1)}px;
  height: {ceiling(length(person_tables) / n_per_row) * 250}px
}}
</style>
</head>
<body>
<table>
{tables}
</table>
</body>
</html>', tables = .)

  html_file <- file(paste0("q0.html"))
  writeLines(html_doc, html_file)
  close(html_file)

}
