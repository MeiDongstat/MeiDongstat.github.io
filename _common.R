library(glue)
library(htmltools)
library(markdown)
library(readr)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

read_publications <- function(path = "content/pubs.csv") {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"),
    show_col_types = FALSE
  )
}

make_citation <- function(pub) {
  title <- if (grepl("[.!?]$", pub$title)) {
    pub$title
  } else {
    paste0(pub$title, ".")
  }

  year <- if (!is.na(pub$year) && nzchar(pub$year)) {
    glue::glue(" ({pub$year}).")
  } else {
    "."
  }

  venue <- if (!is.na(pub$venue) && nzchar(pub$venue)) {
    glue::glue(" *{pub$venue}*.")
  } else {
    ""
  }

  details <- if (!is.na(pub$details) && nzchar(pub$details)) {
    glue::glue(" {pub$details}")
  } else {
    ""
  }

  glue::glue('{pub$authors}{year} "{title}"{venue}{details}')
}

publication_link <- function(label, url, icon) {
  if (is.na(url) || !nzchar(url)) {
    return("")
  }

  as.character(
    htmltools::a(
      href = url,
      class = "icon-link",
      target = "_blank",
      rel = "noopener",
      HTML(glue::glue('<i class="{icon}"></i> {label}'))
    )
  )
}

make_publication_item <- function(pub) {
  citation <- markdown::markdownToHTML(
    text = make_citation(pub),
    fragment.only = TRUE
  )

  award_text <- if ("awards" %in% names(pub)) pub$awards else NA_character_
  awards <- if (!is.na(award_text) && nzchar(award_text)) {
    award_content <- markdown::markdownToHTML(
      text = award_text,
      fragment.only = TRUE
    )
    glue::glue('<div class="publication-awards">{award_content}</div>')
  } else {
    ""
  }

  doi_url <- if (!is.na(pub$doi) && nzchar(pub$doi)) {
    paste0("https://doi.org/", pub$doi)
  } else {
    NA_character_
  }

  links <- paste0(
    publication_link("DOI", doi_url, "fas fa-external-link-alt"),
    publication_link("Journal", pub$url_pub, "fas fa-book-open"),
    publication_link("arXiv", pub$url_arxiv, "far fa-file-pdf"),
    publication_link("Code", pub$url_code, "fab fa-github")
  )

  glue::glue(
    '<li><div class="publication-citation">{citation}</div>',
    '{awards}',
    '<div class="publication-links">{links}</div></li>'
  )
}

make_pub_list <- function(pubs, category) {
  selected <- pubs[pubs$category == category, , drop = FALSE]

  if (nrow(selected) == 0) {
    return(htmltools::HTML(""))
  }

  items <- vapply(
    seq_len(nrow(selected)),
    function(i) make_publication_item(selected[i, ]),
    character(1)
  )

  htmltools::HTML(
    paste0('<ol class="publication-list">', paste(items, collapse = ""), "</ol>")
  )
}

icon_link <- function(text, url) {
  htmltools::a(
    href = url,
    text,
    class = "icon-link",
    target = "_self",
    rel = "noopener"
  )
}
