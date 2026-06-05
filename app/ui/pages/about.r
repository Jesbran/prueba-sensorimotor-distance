# ui/pages/about.r

# --- Tab 1: Project Overview ---
tab_about <- tabPanel(
  title = "About",
  includeMarkdown("ui/page_text/about.md"),
  tags$hr(),
  tags$p(tags$small(paste("Software version", meta_version, "| UNAM PAPIIT Project.")))
)

# --- Tab 2: Technical Information ---
tab_information <- tabPanel(
  title = "Information",
  value = "tab_info",
  # withMathJax() for math formulas renderization
  withMathJax(
    includeMarkdown("ui/page_text/information.md")
  )
)

# --- Tab 3: News ---
tab_news <- tabPanel(
  title = "News and updates",
  includeMarkdown("ui/page_text/news.md")
)

# --- Main About Menu ---
page_about <- navbarMenu(
  "About",
  tab_about,
  tab_information, 
  tab_news
)