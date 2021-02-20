#' Create the quiz intro HTML page with participants' images and colors
#'
#' @param presence Vector of present participants. If NULL, taken from quiz
#'   environment (presence).
#' @param n_per_row Number of pictures per row.
#' @param quiz Quiz environment with quiz variables (uses presence,
#'   named_colors, question_colors, css_file, participants)
#'
#' @return Saves Q0.html file.
#' @export
#'
#' @examples
show_contestants <- function(presence = NULL, n_per_row = 5, quiz = getOption("peRson.quiz")) {
  args <- as.list(quiz)[c("presence", "named_colors", "participants", "css_file")]
  args$bg_next <- quiz$question_colors[1]
  if (!is.null(presence)) {
    args$presence <- presence
  }

  person_tables <- args$participants %>%
    filter(name %in% args$presence) %>%
    arrange(sample(row_number())) %>%
    purrr::pmap(function(name, image, hex, ...) {
      glue::glue('
<td style="width: {100 / n_per_row}%">
<table class="contestant">
  <tr>
    <td style="background-color: {hex}; height: 10px"></td>
  </tr>
  <tr>
    <td><img src="{image}"></img></td>
  </tr>
  <tr>
    <td>{name}</td>
  </tr>
</table>
</td>
')
    })

  html_doc <- purrr::map(1:(length(person_tables) %/% n_per_row + ifelse(length(person_tables) %% n_per_row > 0, 1, 0)), ~ person_tables[(. * n_per_row - n_per_row + 1):(. * n_per_row)]) %>%
    purrr::map(~ paste(unlist(.), collapse = "")) %>%
    purrr::map(~ paste0("<tr>", ., "</tr>\n")) %>%
    unlist() %>%
    paste(collapse = "") %>%
    glue::glue('
<!DOCTYPE html>
<html>
<head>
<link rel="stylesheet" href="{args$css_file}">
<style>
.contestant-wrapper {{
  width: {200 * n_per_row + 20 * (n_per_row - 1)}px;
  height: {ceiling(length(person_tables) / n_per_row) * 250}px
}}

a.next:hover {{
  background-color: {args$bg_next};
}}
</style>
</head>
<body>
{header_div("Do you know me? QUIZ", args$named_colors[\"avg\"])}
<table class="contestant-wrapper">
               {rows}
</table>
<table class="navigation">
  <tr>
    <td><a class="next" href="Q1.html"></a></td>
  </tr>
</table>
</body>
</html>', rows = .)

  html_file <- file(paste0("quiz/Q0.html"))
  writeLines(html_doc, html_file)
  close(html_file)
}
