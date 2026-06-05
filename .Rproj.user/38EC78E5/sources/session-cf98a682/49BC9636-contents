source("ui/shared_elements.r")

page_explore <- tabPanel(
  title = "Sensorimotor space",
  sidebarLayout(
    sidebarPanel(
      h3("Sensorimotor space"),
      aboutText(includeMarkdown("ui/page_text/explore.md")),
      
      # Select dataset
      selectInput(
        inputId = "explore_dataset",
        label = "Dataset",
        choices = setNames(names(DATASETS), sapply(DATASETS, function(x) x$label)),
        selected = names(DATASETS)[1] # Selects first dataset from default
      ),
      
      distance_select_with_id("explore"),
      
      radioButtons(
        inputId = "explore_dominance",
        label = "Colour concepts by their dominance in",
        choices = list(
          "Sensorimotor dimension" = "sensorimotor",
          "Perceptual modality" = "perceptual",
          "Action effector" = "action"
        ),
        selected = "sensorimotor"
      ),
      helpText(includeMarkdown("ui/help_text/dominance_colouring.md")),
    ),
    mainPanel(
      helpText(includeMarkdown("ui/help_text/t-sne.md")),
      withSpinner(
        plotlyOutput(
          outputId = "explore_tsne_plot",
          height='60vh'
        )
      )
    )
  )
)
