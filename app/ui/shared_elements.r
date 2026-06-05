# shared_elements.r

# Lista de opciones de distancia con nombres canónicos
# IMPORTANTE: "minkowski-3" debe coincidir con el nombre usado en precompute_tsne_all.R
distance_choices <- list(
  "Euclidean distance"   = "euclidean",
  "Minkowski-3 distance" = "minkowski-3", 
  "Cosine distance"      = "cosine",
  "Correlation distance" = "correlation",
  "Mahalanobis distance" = "mahalanobis"
)

distance_default <- "cosine"

# Selector de distancia con un ID dado
distance_select_with_id <- function(inputId) {
  return(
    tags$div(
      selectInput(
        inputId = paste0(inputId, "_distance"),
        label = "Distance measure",
        choices = distance_choices,
        selected = distance_default
      ),
      helpText(includeMarkdown("ui/help_text/distance_select.md"))
    )
  )
}

# Wrapper para bloques de texto informativos
aboutText <- function(content) {
  return(
    tags$div(content, class = "about-block")
  )
}

# Wrapper para mensajes de resumen (errores de palabras no encontradas, etc.)
summaryText <- function(id) {
  return(
    textOutput(id) %>% 
      tagAppendAttributes(class = 'summary') %>% 
      tagAppendAttributes(class = "inline")
  )
}
