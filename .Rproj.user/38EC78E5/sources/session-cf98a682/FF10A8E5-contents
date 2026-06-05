# ui/pages/explore_full.r
source("ui/shared_elements.r")

# Preparamos las opciones con nombres bonitos
dataset_choices <- setNames(names(DATASETS), sapply(DATASETS, function(x) x$label))

page_explore_full <- tabPanel(
  title = "Full Datasets",
  sidebarLayout(
    sidebarPanel(
      h3("Browse Full Datasets"),
      aboutText("Select a dataset to view all its concepts and sensorimotor norms."),
      
      # Selector de un solo dataset
      selectInput("full_ds_select", "Choose Dataset:", 
                  choices = dataset_choices),
      
      hr(),
      # Botón de descarga para el dataset completo
      downloadButton("download_full_dataset", "Download Entire Dataset (.csv)", 
                     style="width: 100%;"),
      br(), br(),
      helpText("Note: Large datasets may take a few seconds to load.")
    ),
    
    mainPanel(
      # Tabla que ocupa el resto de la pantalla
      DT::dataTableOutput("full_dataset_table")
    )
  )
)
