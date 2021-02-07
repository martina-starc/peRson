random_colour <- function(exclude_colors = NA) {
  colors()[!stringr::str_detect(colors(), "(gra|ey)|white") & !(colors() %in% exclude_colors)] %>%
    sample(1)
}
random_color <- random_colour

process_colors <- function(data) {
  data %>%
    mutate(color = tidyr::replace_na(color, random_color())) %>%
    rowwise() %>%
    mutate(rgb = list(purrr::set_names(grDevices::col2rgb(color), c("r", "g", "b"))),
           hex = gplots::col2hex(color)) %>%
    ungroup() %>%
    tidyr::unnest_wider(rgb) %>%
    mutate(dist =
      as.matrix(r, g, b) %>%
      stats::dist(upper = TRUE) %>%
      as.matrix() %>%
      colMeans()
    ) %>%
    mutate(avg = grDevices::rgb(mean(r), mean(g), mean(b), maxColorValue = 255))
}

quiz_setup <- function(questions, participants, presence = participants$name) {
  quiz <- list()
  quiz$all_questions <- questions %>%
    shuffle_answers()
  quiz$questions <- quiz$all_questions %>%
    filter(person %in% presence) %>%
    shuffle_questions()
  quiz$participants <- participants %>%
    process_colors()
  quiz$answers <- list()
  quiz <<- quiz
}


create_question <- function(n) {
  question <- quiz$questions[n, ] %>% as.list
  bg_colors <- with(quiz$participants, purrr::set_names(gplots::col2hex(color), name))
  bg_previous <- ifelse(n == 1, "black", bg_colors[unlist(quiz$questions[n - 1, "person"])])
  bg_current <- bg_colors[question$person]
  bg_next <- ifelse(n == nrow(quiz$questions), "black", bg_colors[unlist(quiz$questions[n + 1, "person"])])

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
purrr::walk(1:21, create_question)

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
show_contestants(quiz$participants$name)
quiz_setup(demo_questions, demo_participants)


