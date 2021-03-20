rmarkdown::render(
    "report.Rmd", 
    output_format = rmarkdown::html_document(
        theme = NULL,
        mathjax = NULL,
        highlight = NULL,
        css = "style.css"
    ),
    output_file = "index.html"
)