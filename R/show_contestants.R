show_contestants <- function(presence, n_per_row = 5) {

  bg_next <- quiz$named_colors[unlist(quiz$questions[1, "person"])]
  pic_file <- system.file("pics", "caty_pexels-kelvin-valerio-617278.jpg", package = "peRson")
  css_file <- system.file("css", "styles.css", package = "peRson")

  person_tables <- quiz$participants %>%
    filter(name %in% presence) %>%
    mutate(image_file = system.file("pics", image, package = "peRson")) %>%
    arrange(sample(row_number())) %>%
    purrr::pmap(function(name, image_file, hex, ...) {
      glue::glue('
<td style="width: {100 / n_per_row}%">
<table class="contestant">
  <tr>
    <td style="background-color: {hex}; height: 10px"></td>
  </tr>
  <tr>
    <td><img src="{image_file}"></img></td>
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
    purrr::map(~paste0('<tr>', ., "</tr>\n")) %>%
    unlist() %>%
    paste(collapse = "") %>%
    glue::glue('
<!DOCTYPE html>
<html>
<head>
<link rel="stylesheet" href="{css_file}">
<style>
.contestant-wrapper {{
  width: {200 * n_per_row + 20 * (n_per_row - 1)}px;
  height: {ceiling(length(person_tables) / n_per_row) * 250}px
}}

a.next:hover {{
  background-color: {bg_next};
}}
</style>
</head>
<body>
<table class="contestant-wrapper">
  {rows}
  <tr>
    <td colspan={n_per_row} style = "width: 100%"><a class="next" href="q1.html"></a></td>
  </tr>
</table>
</body>
</html>', rows = .)

  html_file <- file(paste0("q0.html"))
  writeLines(html_doc, html_file)
  close(html_file)

}
