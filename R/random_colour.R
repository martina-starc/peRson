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
  bgcolor <- gplots::col2hex(with(quiz$participants, color[which(name == question$person)]))
  html_doc <- with(question, glue::glue('
<!DOCTYPE html>
<html>
<head>
<style>
html, body {{
  height: 100%;
  margin: 0;
  padding: 20px;
}}

#question img {{
  padding: 0;
  padding-top: 10px;
  display: block;
  margin: 0 auto;
  max-height: 100%;
  max-width: 100%;
}}

#question {{
  font-family: Arial, Helvetica, sans-serif;
  border-collapse: collapse;
  width: 100%;
}}

#question a {{
  font-size: 50px;
}}

#question td {{
  vertical-align: top;
  text-align: left;
  width: 25%;
  padding: 8px;
  font-size: 20px;
}}

#question td.q {{
  font-size: 25px;
  background-color: #f3f3f3;
  font-style: normal;
  padding-bottom: 30px;
}}

#question tr:nth-child(even){{background-color: white;}}

#question tr:hover {{background-color: transparent;}}
</style>
</head>
<body>

<table id="question">
  <tr>
    <td colspan = 4, style = "background-color: {bgcolor}"></td>
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
  <tr>
    <td colspan = 2><a href = "q{n-1}.html"><</a></td>
    <td colspan = 2, style = "text-align: right"><a href = "q{n+1}.html">></a></td>
  </tr>

</table>

</body>
</html>
'))

  html_file <- file(paste0("q", n, ".html"))
  writeLines(html_doc, html_file)
  close(html_file)

}

show_contestants <- function(presence, n_per_row = 5) {

  person_tables <- quiz$participants %>%
    filter(name %in% presence) %>%
    arrange(sample(row_number())) %>%
    purrr::pmap(function(name, image, hex, ...) {
      glue::glue('
<td style = "width: {100 / n_per_row}%"><table style = "width: 200px">
  <tr><td style = "background-color: {hex}; width: 200px; height: 10px; padding: 0px; margin: 0px"></td></tr>
  <tr><td style = "vertical-align: top; padding: 0px; margin: 0px; width: 200 px"><img src = "{image}"></img></td></tr>
  <tr><td style = "text-align: center; font-size: 20px; vertical-align: middle; padding: 0px; margin: 0px; width: 200 px">{name}</td></tr>
</table></td>
')
    })

  html_doc <- purrr::map(1:(length(person_tables) %/% n_per_row + ifelse(length(person_tables) %% n_per_row > 0, 1, 0)), ~person_tables[(. * n_per_row - n_per_row + 1):(. * n_per_row)]) %>%
    purrr::map(~paste(unlist(.), collapse = "")) %>%
    purrr::map(~paste0('<tr style = "background-color: white">', ., "</tr>\n")) %>%
    unlist() %>%
    paste(collapse = "") %>%
    paste0('<table style = "margin-left: auto; margin-right: auto;">', ., "</table>")

  html_file <- file(paste0("q0.html"))
  writeLines(html_doc, html_file)
  close(html_file)

}
quiz_setup(demo_questions, demo_participants)
show_contestants(quiz$participants$name)
