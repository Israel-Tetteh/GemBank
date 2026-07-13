files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
tags <- c("div", "span", "hr", "br", "p", "h2", "h4", "h5", "strong")
for (f in files) {
  content <- readLines(f)
  for (t in tags) {
    # Match tag( not preceded by shiny:: or tags$
    pattern <- paste0("(?<!shiny::)(?<!tags\\$)\\b", t, "\\(")
    replacement <- paste0("shiny::", t, "(")
    content <- gsub(pattern, replacement, content, perl = TRUE)
  }
  writeLines(content, f)
}
