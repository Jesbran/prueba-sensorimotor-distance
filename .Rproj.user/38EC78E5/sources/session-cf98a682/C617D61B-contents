# ui/pages/profiles.r

source("ui/shared_elements.r")

# Lista de opciones:

dataset_choices <- setNames(names(DATASETS), sapply(DATASETS, function(x) x$label))

page_profiles <- tabPanel(
  title = "Sensorimotor profiles",
  sidebarLayout(
    sidebarPanel(
      h3("Concept Profiles"),
      
      # Datasets
      tags$div(style="display: flex; gap: 10px;",
               selectInput("profile_ds1", "Dataset 1 (Colors)", 
                           choices = dataset_choices, 
                           selected = names(DATASETS)[1]),
               selectInput("profile_ds2", "Dataset 2 (Grey Overlay)", 
                           choices = dataset_choices, 
                           selected = names(DATASETS)[2])
      ),
      
      hr(),
      # Espacio para las palabras
      textAreaInput("profile_words", "Concepts (one per line or comma separated)", 
                    rows = 5, value = ""),
      
      # Botón Random Concept
      actionButton("profile_button_random", "Random Concept"),
      br(), 
      # Selección de estadísticas para la tabla
      checkboxGroupInput("profile_stats", "Show Data Table statistics:",
                         choices = c("Mean" = ".mean", "SD" = ".SD", "Minkowski" = "Minkowski3", 
                                     "Exclusivity" = "Exclusivity", "Dominant" = "Dominant", 
                                     "Max Strength" = "Max_strength"),
                         selected = ".mean"),
      downloadButton("download_profile_data", "Download Table") 
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Polar Comparison", plotOutput("profile_plot", height = "600px")),
        tabPanel("Data Table", DT::dataTableOutput("profile_table_dt"))
      )
    )
  )
)


