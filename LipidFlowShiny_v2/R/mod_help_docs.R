# "Help Documents" tab - renders www/help.md as an in-app page (replacing
# the previous static-HTML-in-a-new-tab link), styled to read like an
# academic reference doc (see .lfs-help-doc/.lfs-help-toc in custom.css).
#
# shiny::markdown() (bundled with shiny >= 1.8, backed by commonmark with
# GFM extensions - tables, fenced code blocks - enabled by default) is used
# instead of the `markdown` package, which isn't a project dependency.

# ---------------------------------------------------------------------------
# Render help.md to HTML and inject an id= attribute into every <h2> so a
# sidebar table-of-contents can jump to it - shiny::markdown()'s commonmark
# backend doesn't generate heading ids on its own. Returns list(html, toc)
# where toc is a data.frame(title, slug) in document order.
# ---------------------------------------------------------------------------
.lfs_render_help_doc <- function(md_path) {
  if (!file.exists(md_path)) {
    return(list(
      html = "<p>Help document not found.</p>",
      toc = data.frame(title = character(), slug = character(), stringsAsFactors = FALSE)
    ))
  }

  md_text <- paste(readLines(md_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- tryCatch(
    as.character(shiny::markdown(md_text)),
    error = function(e) {
      message("[mod_help_docs] Failed to render help.md: ", conditionMessage(e))
      paste0("<p>Could not render the help document: ", conditionMessage(e), "</p>")
    }
  )

  h2_matches <- regmatches(html, gregexpr("<h2>.*?</h2>", html))[[1]]
  if (length(h2_matches) == 0) {
    return(list(html = html, toc = data.frame(title = character(), slug = character(), stringsAsFactors = FALSE)))
  }

  titles <- gsub("^<h2>|</h2>$", "", h2_matches)
  slugs <- gsub("(^-|-$)", "", gsub("[^a-z0-9]+", "-", tolower(titles)))
  slugs <- make.unique(slugs, sep = "-")

  for (i in seq_along(h2_matches)) {
    html <- sub(h2_matches[i], paste0('<h2 id="', slugs[i], '">', titles[i], '</h2>'), html, fixed = TRUE)
  }

  list(html = html, toc = data.frame(title = titles, slug = slugs, stringsAsFactors = FALSE))
}

mod_help_docs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "lfs-help-page", shiny::uiOutput(ns("doc")))
}

mod_help_docs_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$doc <- shiny::renderUI({
      rendered <- .lfs_render_help_doc(file.path("www", "help.md"))

      toc_ui <- if (nrow(rendered$toc) > 0) {
        shiny::tags$nav(
          class = "lfs-help-toc",
          shiny::h6("On this page"),
          lapply(seq_len(nrow(rendered$toc)), function(i) {
            shiny::tags$a(href = paste0("#", rendered$toc$slug[i]), rendered$toc$title[i])
          })
        )
      } else {
        NULL
      }

      shiny::div(
        class = "lfs-help-shell",
        toc_ui,
        shiny::div(class = "lfs-help-doc", shiny::HTML(rendered$html))
      )
    })
  })
}
