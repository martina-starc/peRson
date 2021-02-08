get_transparent_pic <- function() {
  system.file("pics", "transparent.png", package = "peRson")
}

header_div <- function(text, bgcolor) {
  glue::glue('<div class="header" style="background-color: {bgcolor};">{text}</div>')
}
