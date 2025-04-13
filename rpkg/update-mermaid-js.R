# fix to render tar_mermaid
# https://github.com/rich-iannone/DiagrammeR/issues/475#issue-1412818156
#'  Update the mermaid library in the package
#' @param version A [character] of the desired version (the latest by default)
#' @return See value returned by [download.file]
update_mermaid_js <- function(
  version = "",
  destfile = "/usr/local/lib/R/site-library/DiagrammeR/htmlwidgets/lib/mermaid/dist/mermaid.slim.min.js"
) {
  url <- "https://cdn.jsdelivr.net/npm/mermaid@version/dist/mermaid.min.js"
  if (version != "") {
    stopifnot(grepl("^[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+$", version))
    version <- paste0("@", version)
  }
  url <- gsub("@version", version, url)

  try(
    download.file(
      url = url,
      destfile = destfile
    )
  )
  return(TRUE)
}
