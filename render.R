file_path <- commandArgs()[4]
file_path <- strsplit(file_path, "=")[[1]][2]
file_path <- dirname(file_path)

rmarkdown::render(
    paste0(file_path, "/report.Rmd"), 
    output_format = rmarkdown::html_document(
        theme = NULL,
        mathjax = NULL,
        highlight = NULL,
        css = "style.css"
    ),
    output_file = "index.html"
)
