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

random_colour <- function(exclude_colors = NA) {
  colors()[!stringr::str_detect(colors(), "(gra|ey)|white") & !(colors() %in% exclude_colors)] %>%
    sample(1)
}

random_color <- random_colour

