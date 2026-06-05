library(R.utils)
library(MASS)
library(dplyr)
library(magrittr)
library(markdown)
library(purrr)
library(readr)
library(rdist)
library(ICSNP)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(stringr)
library(tidyr)
library(plotly)
library(readxl) 
library(tools)  

# Core logic and data
source("meta.r")
source("calculate/norms.r")
source("calculate/distance.r")
source("calculate/distance_tables.r")
source("calculate/neighbours.r")

# UI elements and helpers
source("ui/text.r")
source("ui/summarise.r")
source("ui/plotting.r")
source("ui/shared_elements.r")
source("ui/parse_input.r")

# Pages UI
source("ui/pages/about.r")
source("ui/pages/explore_full.r")
source("ui/pages/profiles.r")
source("ui/pages/distances.r")
source("ui/pages/neighbours.r")  
source("ui/pages/visualise.r")
source("ui/pages/explore.r")

options(spinner.color="#e95420", spinner.type=7)

ui <- navbarPage(
  title = "Human & Synthetic Sensorimotor Norms Explorer",
  id = "main_nav",
  page_about,
  page_explore_full,
  page_profiles,
  page_distances,
  page_neighbours,
  page_visualise,
  page_explore,
  header=list(tags$head(includeCSS("www/styles.css"))),
  theme=shinytheme("flatly")
)

server <- function(input, output, session) {
  
  # Lógica para saltar a la pestaña de información desde el link del About
  observeEvent(input$go_to_info, {
    # Esta función busca el menú "main_nav" y selecciona "tab_info"
    updateNavbarPage(session, "main_nav", selected = "tab_info")
  })
  
  # --- Lógica para Concepto Aleatorio ---
  
  # 1. Al iniciar la aplicación: Cargar un concepto al azar del Dataset 1
  # Usamos observe() con priority para que se ejecute una sola vez al arranque
  observe({
    req(input$profile_ds1) # Esperar a que el selector esté listo
    new_word <- random_norm(ds = input$profile_ds1)
    updateTextAreaInput(session, "profile_words", value = new_word)
  }, priority = 10) # Prioridad alta para que sea lo primero que ocurra
  
  # 2. Al presionar el botón "Random Concept":
  observeEvent(input$profile_button_random, {
    # Obtenemos un concepto del dataset que el usuario tenga seleccionado en "Dataset 1"
    new_word <- random_norm(ds = input$profile_ds1)
    
    # Actualizamos el cuadro de texto
    updateTextAreaInput(session, "profile_words", value = new_word)
  })
  
  # Default dataset ID for initializations
  default_ds <- names(DATASETS)[1]
  second_ds  <- if(length(DATASETS) > 1) names(DATASETS)[2] else names(DATASETS)[1]
  
  # -------- Full Data Set logic --------
  # 1. Renderizar la tabla del dataset completo seleccionado
  output$full_dataset_table <- DT::renderDataTable({
    req(input$full_ds_select)
    
    # Obtenemos el dataset completo de la lista global
    df_full <- get_norms(input$full_ds_select)
    
    # Identificar numéricas para redondear
    cols_numeric <- names(df_full)[sapply(df_full, is.numeric)]
    
    DT::datatable(
      df_full,
      options = list(
        pageLength = 20, 
        scrollX = TRUE, 
        rownames = FALSE,
        dom = 'fltip' # 'f' añade buscador, 'l' cantidad de filas
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = cols_numeric, digits = 3)
  })
  
  # 2. Lógica para descargar el dataset completo
  output$download_full_dataset <- downloadHandler(
    filename = function() {
      paste0(input$full_ds_select, "_full_dataset.csv")
    },
    content = function(file) {
      # Escribir el dataset seleccionado tal cual está
      write.csv(get_norms(input$full_ds_select), file, row.names = FALSE)
    }
  )
  
  # -------- Profiles Logic --------
  
  profile_data_individual <- reactive({
    req(input$profile_words)
    
    # 1. Limpiamos la lista de palabras ingresadas
    w_list <- get_words(input$profile_words)$words
    if(length(w_list) == 0) return(NULL)
    
    ds1 <- input$profile_ds1
    ds2 <- input$profile_ds2
    
    # 2. Extraer datos individuales del Dataset 1
    df1 <- get_norms(ds1) %>% 
      filter(Word %in% w_list) %>%
      # Aquí asignamos el nombre amigable: "English (Human)", etc.
      mutate(dataset = dataset_label(ds1)) 
    
    # 3. Extraer datos individuales del Dataset 2
    df2 <- get_norms(ds2) %>% 
      filter(Word %in% w_list) %>%
      # Aquí asignamos el nombre amigable: "English (GPT)", etc.
      mutate(dataset = dataset_label(ds2))
    
    # 4. Unir ambas tablas
    # Ahora tendrás una fila por cada palabra de cada dataset
    bind_rows(df1, df2) %>%
      # Reorganizar para que Word y dataset sean las primeras columnas
      select(Word, dataset, everything())
  })
  
  # 2. Render de la Tabla con columnas dinámicas
  output$profile_table_dt <- DT::renderDataTable({
    # 1. Obtener los datos individuales (sin promediar)
    data <- profile_data_individual()
    req(data)
    
    # 2. Identificar qué estadísticas mostrar (Mean, SD, Minkowski)
    stats_to_show <- input$profile_stats
    
    # 3. Construir la lista de columnas a mostrar
    # Siempre incluimos 'Word', 'Word_' y 'dataset' al principio
    # Buscamos cualquier columna que empiece con "Word_" 
    # Esto atrapará Word_Croatian, Word_Italian, etc.
    word_extra_cols <- names(data)[grepl("^Word_", names(data))]
    
    # Luego buscamos las columnas que coincidan con los sufijos seleccionados
    cols_to_keep <- c("Word", word_extra_cols, "dataset", 
                      names(data)[grepl(paste(stats_to_show, collapse="|"), names(data))])
    
    # Aseguramos que no haya duplicados y que las columnas existan
    cols_to_keep <- intersect(cols_to_keep, names(data))
    
    final_df <- data[, cols_to_keep, drop=FALSE]
    
    # 4. Limpieza de nombres para que se vea mejor (opcional)
    # Por ejemplo, mover 'dataset' al principio después de Word
    final_df <- final_df %>% select(Word, dataset, everything())
    
    # 5. Identificar columnas numéricas para redondear (evitar el error 'closure')
    cols_numeric <- names(final_df)[sapply(final_df, is.numeric)]
    
    # 6. Crear el DT
    dt_obj <- DT::datatable(
      final_df, 
      options = list(
        scrollX = TRUE, 
        pageLength = 10, 
        order = list(list(0, 'asc')), # Ordenar por la columna 'Word'
        dom = 'ltip'
      ),
      rownames = FALSE
    )
    
    # 7. Formatear decimales
    if (length(cols_numeric) > 0) {
      dt_obj <- dt_obj %>% DT::formatRound(columns = cols_numeric, digits = 3)
    }
    
    dt_obj
  })
  
  # 3. Render del Plot
  profile_data_individual <- reactive({
    req(input$profile_words)
    # Extraer lista de palabras (limpia comas y saltos de línea)
    w_list <- get_words(input$profile_words)$words
    if(length(w_list) == 0) return(NULL)
    
    ds1 <- input$profile_ds1
    ds2 <- input$profile_ds2
    
    # Obtener filas individuales de cada dataset
    df1 <- get_norms(ds1) %>% filter(Word %in% w_list) %>% mutate(dataset = dataset_label(ds1))
    df2 <- get_norms(ds2) %>% filter(Word %in% w_list) %>% mutate(dataset = dataset_label(ds2))
    
    bind_rows(df1, df2)
  })
  
  # 2. Render del Plot (Mapeo de nombres y Facetting)
  output$profile_plot <- renderPlot({
    data <- profile_data_individual()
    req(data)
    
    # Transformar a formato largo manteniendo la columna 'Word'
    df_long <- data %>%
      select(Word, dataset, all_of(all_columns)) %>%
      pivot_longer(cols = all_of(all_columns), names_to = "dimension", values_to = "rating") %>%
      mutate(dimension = gsub(".mean", "", dimension))
    
    # Mapeo de nombres Lancaster (Mouth -> Mouth/throat, etc.)
    df_long$dimension <- plyr::mapvalues(df_long$dimension,
                                         from = c("Head", "Mouth", "Hand_arm", "Foot_leg", "Torso", "Auditory", "Gustatory", "Haptic", "Interoceptive", "Olfactory", "Visual"),
                                         to = c("Head", "Mouth/throat", "Hand/arm", "Foot/leg", "Torso", "Auditory", "Gustatory", "Haptic", "Interoceptive", "Olfactory", "Visual")
    )
    
    # Ordenar dimensiones para que coincidan con la paleta de colores
    df_long$dimension <- factor(df_long$dimension, levels = c(
      "Auditory", "Gustatory", "Haptic", "Interoceptive", "Olfactory", "Visual",
      "Foot/leg", "Hand/arm", "Head", "Mouth/throat", "Torso"
    ))
    
    # Llamar a la función (ahora con soporte para múltiples palabras)
    profile_polar_plot(df_long, dataset_label(input$profile_ds1), dataset_label(input$profile_ds2))
    
  }, height = function() {
    # Altura dinámica: si hay muchas palabras, el plot crece hacia abajo
    n_words <- length(unique(profile_data_individual()$Word))
    return(max(500, ceiling(n_words/2) * 350))
  }, width = 700)
  
  # 3. Descarga de datos (individuales, no promedios)
  output$download_profile_data <- downloadHandler(
    filename = function() { paste0("profile_data_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(profile_data_individual(), file, row.names = FALSE) }
  )
  
  # -------- One-to-one --------
  one_one_ds1 <- reactive({ input$one_one_ds1 %||% default_ds })
  one_one_ds2 <- reactive({ input$one_one_ds2 %||% second_ds  })
  one_one_distance_type <- reactive({ input$one_one_distance })
  
  one_one_pairs <- reactive({ get_word_pairs(input$one_one_word_pairs) })
  one_one_left_words  <- reactive({ one_one_pairs()$left_words })
  one_one_right_words <- reactive({ one_one_pairs()$right_words })
  
  # Initialization
  updateTextInput(session, "one_one_word_pairs", 
                  value=render_pairs(random_norm_pairs(10, ds=default_ds)))
  
  observeEvent(input$one_one_button_clear, { updateTextInput(session,"one_one_word_pairs", value="") })
  observeEvent(input$one_one_button_random_pairs, { 
    updateTextInput(session,"one_one_word_pairs", value=render_pairs(random_norm_pairs(10, ds=one_one_ds1()))) 
  })
  
  output$one_one_summary_pairs <- renderText({
    summarise_pairs(one_one_left_words(), one_one_right_words(),
                    one_one_pairs()$words_not_in_norms, one_one_pairs()$malformed_lines)
  })
  
  observe({
    updateCheckboxInput(session,"will_show_results_one_one",
                        value=(length(one_one_left_words())>0 && length(one_one_right_words())>0))
  })
  
  one_one_table_data <- reactive({
    distance_table_for_word_pairs(one_one_left_words(), one_one_right_words(), 
                                  one_one_distance_type(), one_one_ds1(), one_one_ds2())
  })
  output$one_one_distances_table <- renderTable({ one_one_table_data() }, digits=precision)
  output$one_one_table_download <- downloadHandler(
    filename=function() "distance_pairs.csv",
    content=function(file) write.csv(one_one_table_data(), file, row.names=FALSE)
  )
  
  # -------- One-to-many --------
  one_many_ds1 <- reactive({ input$one_many_ds1 %||% default_ds })
  one_many_ds2 <- reactive({ input$one_many_ds2 %||% second_ds  })
  one_many_distance_type <- reactive({ input$one_many_distance })
  
  one_many_left_word <- reactive({ canonise_word(input$one_many_word_one) })
  one_many_words_many <- reactive({ get_words(input$one_many_words_many) })
  one_many_right_words <- reactive({ one_many_words_many()$words })
  one_many_right_missing <- reactive({ one_many_words_many()$missing })
  
  # Initialization
  updateTextInput(session,"one_many_word_one", value=random_norm(ds=default_ds))
  updateTextInput(session,"one_many_words_many", value=render_list(random_norms(10, ds=default_ds)))
  
  observeEvent(input$one_many_button_clear_one,  { updateTextInput(session,"one_many_word_one", value="") })
  observeEvent(input$one_many_button_clear_many, { updateTextInput(session,"one_many_words_many", value="") })
  observeEvent(input$one_many_button_random_one, { 
    updateTextInput(session,"one_many_word_one", value=random_norm(ds=one_many_ds1())) 
  })
  observeEvent(input$one_many_button_random_many,{ 
    updateTextInput(session,"one_many_words_many", value=render_list(random_norms(10, ds=one_many_ds1()))) 
  })
  
  output$one_many_summary_one <- renderText({
    w <- one_many_left_word()
    if (nchar(w)==0) "" else if (!(w %in% all_words_union())) "Word not found in any dataset." else ""
  })
  output$one_many_summary_many <- renderText({ summarise_words(one_many_right_words(), one_many_right_missing()) })
  
  observe({
    updateCheckboxInput(session,"will_show_results_one_many",
                        value=((one_many_left_word() %in% all_words_union()) && length(one_many_right_words())>0))
  })
  
  one_many_table_data <- reactive({
    distance_table_for_one_many(one_many_left_word(), one_many_right_words(), 
                                one_many_distance_type(), one_many_ds1(), one_many_ds2())
  })
  output$one_many_distances_table <- renderTable({ one_many_table_data() }, digits=precision)
  output$one_many_table_download <- downloadHandler(
    filename=function() "distance_one_many.csv",
    content=function(file) write.csv(one_many_table_data(), file, row.names=FALSE)
  )
  
  # -------- Many-to-many --------
  many_many_ds1 <- reactive({ input$many_many_ds1 %||% default_ds })
  many_many_ds2 <- reactive({ input$many_many_ds2 %||% second_ds  })
  many_many_distance_type <- reactive({ input$many_many_distance })
  
  many_many_left_in <- reactive({ get_words(input$many_many_words_left) })
  many_many_left_words <- reactive({ many_many_left_in()$words })
  many_many_right_in <- reactive({
    if (isTRUE(input$many_many_symmetric)) many_many_left_in() else get_words(input$many_many_words_right)
  })
  many_many_right_words <- reactive({ many_many_right_in()$words })
  
  # Initialization
  updateTextInput(session,"many_many_words_left", value=render_list(random_norms(10, ds=default_ds)))
  updateTextInput(session,"many_many_words_right", value=render_list(random_norms(10, ds=default_ds)))
  
  observeEvent(input$many_many_button_clear_left, { updateTextInput(session,"many_many_words_left", value="") })
  observeEvent(input$many_many_button_clear_right,{ updateTextInput(session,"many_many_words_right", value="") })
  observeEvent(input$many_many_button_random_left,{ 
    updateTextInput(session,"many_many_words_left", value=render_list(random_norms(10, ds=many_many_ds1()))) 
  })
  observeEvent(input$many_many_button_random_right,{ 
    updateTextInput(session,"many_many_words_right", value=render_list(random_norms(10, ds=many_many_ds1()))) 
  })
  observeEvent(input$many_many_button_copy_right,{ updateTextInput(session,"many_many_words_right", value=input$many_many_words_left) })
  
  output$many_many_summary_left <- renderText({
    summarise_words_count_limit(many_many_left_words(), many_many_left_in()$missing,
                                max=max_words_distance_matrtix, clip_max=TRUE)
  })
  output$many_many_summary_right <- renderText({
    summarise_words_count_limit(many_many_right_words(), many_many_right_in()$missing,
                                max=max_words_distance_matrtix, clip_max=TRUE)
  })
  
  observe({
    updateCheckboxInput(session,"will_show_results_many_many",
                        value=(length(many_many_left_words())>0 && length(many_many_right_words())>0))
  })
  
  many_many_mats <- reactive({
    distance_matrices_for_word_pairs(
      many_many_left_words(), many_many_right_words(),
      many_many_distance_type(), many_many_ds1(), many_many_ds2(),
      max_words=max_words_distance_matrtix
    )
  })
  
  output$many_many_distances_table_AA <- renderTable({ many_many_mats()$AA }, digits=precision, rownames=TRUE)
  output$many_many_distances_table_BB <- renderTable({ many_many_mats()$BB }, digits=precision, rownames=TRUE)
  output$many_many_distances_table_AB <- renderTable({ many_many_mats()$AB }, digits=precision, rownames=TRUE)
  output$many_many_distances_table_BA <- renderTable({ many_many_mats()$BA }, digits=precision, rownames=TRUE)
  
  # -------- Neighbours --------
  neighbours_distance_type <- reactive({ input$neighbours_distance })
  neighbours_source_word <- reactive({ canonise_word(input$neighbour_word) })
  neighbours_dataset <- reactive({ input$neighbours_dataset %||% default_ds })
  
  neighbour_count <- reactive({ as.numeric(input$neighbours_count) })
  neighbour_distance_input <- reactive({ try_parse_float(input$neighbour_radius, default_value=Inf, empty_to_default=TRUE) })
  
  # Random word based on current selected dataset
  observeEvent(input$neighbour_word_button_random, {
    updateTextInput(session,"neighbour_word", value=random_norm(ds = neighbours_dataset()))
  })
  
  observeEvent(input$neighbour_word_button_clear, { updateTextInput(session,"neighbour_word", value="") })
  observeEvent(input$neighbour_button_any_distance,{ updateTextInput(session,"neighbour_radius", value="") })
  
  output$neighbour_word_summary <- renderText({
    w <- neighbours_source_word()
    if (nchar(w)==0) "" else if (!(w %in% all_words(neighbours_dataset()))) paste0("Word not found in ", dataset_label(neighbours_dataset())) else ""
  })
  output$neighbour_radius_summary <- renderText({
    summarise_positive_float(neighbour_distance_input()$value,
                             neighbour_distance_input()$original,
                             neighbour_distance_input()$success)
  })
  
  observe({
    ds <- neighbours_dataset()
    updateCheckboxInput(session,"will_show_results_neighbours",
                        value=(neighbours_source_word() %in% all_words(ds)))
  })
  
  output$neighbour_title <- renderText({
    paste0("Nearest neighbours of “", neighbours_source_word(), "” (", dataset_label(neighbours_dataset()), ")")
  })
  
  neighbours_table_data <- reactive({
    neighbours_table(
      word = neighbours_source_word(),
      distance_type = neighbours_distance_type(),
      count = neighbour_count(),
      radius = neighbour_distance_input()$value,
      ds = neighbours_dataset()
    )
  })
  
  output$neighbours_table <- renderTable({ neighbours_table_data() }, digits=precision)
  output$neighbour_table_download <- downloadHandler(
    filename=function() paste0("neighbours_", neighbours_source_word(), "_", neighbours_dataset(), ".csv"),
    content=function(file) write.csv(neighbours_table_data(), file, row.names=FALSE)
  )
  
  # -------- Visualise (MDS) --------
  visualise_ds1 <- reactive({ input$visualise_ds1 %||% default_ds })
  visualise_ds2 <- reactive({ input$visualise_ds2 %||% second_ds  })
  visualise_distance_type <- reactive({ input$visualise_distance })
  visualise_words_block <- reactive({ get_words(input$visualise_words) })
  visualise_words <- reactive({ visualise_words_block()$words })
  visualise_missing <- reactive({ visualise_words_block()$missing })
  visualise_show_lines <- reactive({ input$visualise_show_lines })
  
  # Initialization
  updateTextInput(session,"visualise_words", value=render_list(random_norms(10, ds=default_ds)))
  
  observeEvent(input$visualise_button_clear, { updateTextInput(session,"visualise_words", value="") })
  observeEvent(input$visualise_button_random,{ 
    updateTextInput(session,"visualise_words", value=render_list(random_norms(10, ds=visualise_ds1()))) 
  })
  
  output$visualise_words_summary <- renderText({
    summarise_words_count_limit(visualise_words(), visualise_missing(), min=3, max=max_words_mds, clip_max=TRUE)
  })
  
  mds_positions <- reactive({
    get_mds_positions_for_words_two_datasets(visualise_words(), visualise_distance_type(), 
                                             visualise_ds1(), visualise_ds2(), 
                                             max_words=max_words_mds)
  })
  output$visualise_mds_plot <- renderPlotly({
    mds_plot_two_datasets(mds_positions(), visualise_ds1(), visualise_ds2(), with_lines=visualise_show_lines())
  })
  
  # -------- Explore (t-SNE) --------
  explore_distance_type <- reactive({ input$explore_distance })
  explore_dominance <- reactive({ input$explore_dominance })
  explore_dataset <- reactive({ input$explore_dataset %||% default_ds })
  
  tsne_positions <- reactive({ get_tsne_positions(explore_distance_type(), dims=3, ds=explore_dataset()) })
  output$explore_tsne_plot <- renderPlotly({
    tsne_plot(tsne_positions(), explore_dominance(), dims=3, ds=explore_dataset())
  })
}

shinyApp(ui=ui, server=server)

