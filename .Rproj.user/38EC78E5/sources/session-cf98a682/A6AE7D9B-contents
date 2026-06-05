source("meta.r")
source("calculate/norms.r")
source("calculate/distance.r")

library(MASS)
library(plotly)

# ---- MDS combinado (Cualquier par de datasets) ----
get_mds_positions_for_words_two_datasets <- function(words, distance_type, ds1, ds2, max_words=Inf) {
  
  words <- unique(words)
  if (length(words) < 2) return(NULL)
  if (length(words) > max_words) words <- words[1:max_words]
  
  # Cargar normas dinámicamente
  norms1 <- get_norms(ds1)
  norms2 <- get_norms(ds2)
  
  words1 <- words[words %in% norms1$Word]
  words2 <- words[words %in% norms2$Word]
  
  # Necesitamos al menos 3 puntos en total para un MDS con sentido
  if (length(words1) + length(words2) < 3) return(NULL)
  
  X1 <- if (length(words1) > 0) matrix_for_words(words1, ds = ds1) else NULL
  X2 <- if (length(words2) > 0) matrix_for_words(words2, ds = ds2) else NULL
  
  # Cálculo de sub-bloques de la matriz de distancia
  D11 <- if (!is.null(X1)) {
    if (distance_type == "mahalanobis") distance_matrix(X1, X1, distance_type, covariance_matrix = get_covariance_matrix(ds1))
    else distance_matrix(X1, X1, distance_type)
  } else NULL
  
  D22 <- if (!is.null(X2)) {
    if (distance_type == "mahalanobis") distance_matrix(X2, X2, distance_type, covariance_matrix = get_covariance_matrix(ds2))
    else distance_matrix(X2, X2, distance_type)
  } else NULL
  
  D12 <- if (!is.null(X1) && !is.null(X2)) {
    if (distance_type == "mahalanobis") distance_matrix(X1, X2, distance_type, covariance_matrix = get_covariance_matrix(ds1))
    else distance_matrix(X1, X2, distance_type)
  } else NULL
  
  D21 <- if (!is.null(X2) && !is.null(X1)) {
    if (distance_type == "mahalanobis") distance_matrix(X2, X1, distance_type, covariance_matrix = get_covariance_matrix(ds2))
    else distance_matrix(X2, X1, distance_type)
  } else NULL
  
  # Ensamblar matriz global D
  label1 <- paste0(words1, "@", ds1)
  label2 <- paste0(words2, "@", ds2)
  labels <- c(label1, label2)
  
  n1 <- length(words1)
  n2 <- length(words2)
  n <- n1 + n2
  D <- matrix(0, n, n)
  
  if (n1 > 0) D[1:n1, 1:n1] <- D11
  if (n2 > 0) D[(n1+1):n, (n1+1):n] <- D22
  if (n1 > 0 && n2 > 0) {
    D[1:n1, (n1+1):n] <- D12
    D[(n1+1):n, 1:n1] <- D21
  }
  
  # Ejecutar MDS
  if (distance_type %in% c("euclidean", "minkowski-3", "mahalanobis")) {
    pts <- cmdscale(D, k = 2)
  } else {
    set.seed(0)
    init <- matrix(runif(n * 2), nrow = n)
    fit <- sammon(D, y = init, k = 2)
    pts <- fit$points
  }
  
  df <- data.frame(label = labels, x = pts[, 1], y = pts[, 2], stringsAsFactors = FALSE)
  df$Dataset <- ifelse(grepl(paste0("@", ds1, "$"), df$label), dataset_label(ds1), dataset_label(ds2))
  df$Word <- sub("@[^@]+$", "", df$label)
  df$ds_id <- ifelse(grepl(paste0("@", ds1, "$"), df$label), ds1, ds2)
  df
}

# Líneas conectoras entre Dataset 1 y Dataset 2
get_mds_line_segments_two_datasets <- function(mds_positions, ds1, ds2) {
  segs <- list()
  if (is.null(mds_positions) || nrow(mds_positions) < 2) return(segs)
  
  label_ds1 <- dataset_label(ds1)
  label_ds2 <- dataset_label(ds2)
  
  by_word <- split(mds_positions, mds_positions$Word)
  for (w in names(by_word)) {
    block <- by_word[[w]]
    if (nrow(block) >= 2) {
      a <- block[block$Dataset == label_ds1, , drop = FALSE]
      b <- block[block$Dataset == label_ds2, , drop = FALSE]
      if (nrow(a) == 1 && nrow(b) == 1) {
        segs[[length(segs) + 1]] <- list(
          type = "line",
          line = list(width = 1, color = "#999", dash = "dot"),
          x0 = a$x, y0 = a$y, x1 = b$x, y1 = b$y
        )
      }
    }
  }
  segs
}

mds_plot_two_datasets <- function(mds_positions, ds1, ds2, with_lines = FALSE) {
  if (is.null(mds_positions)) return(NULL)
  
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, fixedrange = TRUE)
  
  fig <- plot_ly(mds_positions) %>%
    config(toImageButtonOptions = list(format = "svg", filename = "mds-plot", width = 1000, height = 1000)) %>%
    add_trace(
      x = ~x, y = ~y,
      type = "scatter", mode = "markers+text",
      text = ~Word, textposition = "bottom center",
      color = ~Dataset,
      marker = list(size = 10)
    ) %>%
    layout(xaxis = axis, yaxis = axis)
  
  if (isTRUE(with_lines)) {
    fig <- fig %>% layout(shapes = get_mds_line_segments_two_datasets(mds_positions, ds1, ds2))
  }
  
  fig
}

# ---- t-SNE dinámico ----
get_tsne_positions <- function(distance_type, dims, ds) {
  # ds ahora es el ID (ej: "en_deepseek")
  
  infile <- file.path(data_dir, sprintf("t-SNE_%d_%s_cache_%s.bin", dims, distance_name(distance_type), ds))
  
  if (!file.exists(infile)) {
    stop(paste("No se encontró el archivo de cache:", infile))
  }
  
  con <- file(infile, "rb")
  dimv <- readBin(con, "integer", 2)
  Mat <- matrix(readBin(con, "numeric", prod(dimv)), dimv[1], dimv[2])
  close(con)
  
  ret <- data.frame(Mat)
  # all_words(ds) ahora obtiene las palabras correctas según el ID del dataset
  ret <- cbind(Word = all_words(ds), ret)
  
  if (dims == 3) names(ret) <- c("Word", "x", "y", "z")
  else if (dims == 2) names(ret) <- c("Word", "x", "y")
  
  ret
}

tsne_plot <- function(tsne_positions, dominance, dims, ds) {
  df_norms <- get_norms(ds)
  
  # Mapeo de columnas de dominancia
  col_name <- switch(dominance,
                     "sensorimotor" = dominance_column_sensorimotor,
                     "perceptual"   = dominance_column_perceptual,
                     "action"       = dominance_column_action,
                     stop("Dominancia inválida")
  )
  
  tsne_positions$Dominance <- df_norms[[col_name]]
  
  if (dims == 3) {
    fig <- plot_ly(tsne_positions) %>%
      config(toImageButtonOptions = list(format = "svg", filename = "tsne-plot")) %>%
      add_trace(type = "scatter3d", mode = "markers",
                marker = list(size = 3, opacity = 0.7),
                x = ~x, y = ~y, z = ~z,
                hoverinfo = "text", text = ~Word,
                color = ~Dominance, colors = "Set3")
  } else {
    fig <- plot_ly(tsne_positions) %>%
      config(toImageButtonOptions = list(format = "svg", filename = "tsne-plot")) %>%
      add_trace(type = "scatter", mode = "markers",
                marker = list(size = 3, opacity = 0.7),
                x = ~x, y = ~y,
                hoverinfo = "text", text = ~Word,
                color = ~Dominance, colors = "Set3")
  }
  
  fig %>% layout(
    scene = list(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      zaxis = list(visible = FALSE)
    ),
    legend = list(
      title = list(text = "Dominance"),
      font = list(size = 12),
      itemsizing = "constant")
  )
}

# Profiles

profile_polar_plot <- function(df_long, label1, label2) {
  if (is.null(df_long) || nrow(df_long) == 0) return(NULL)
  
  # Capas de datos
  df1 <- df_long %>% filter(dataset == label1)
  df2 <- df_long %>% filter(dataset == label2)
  
  ggplot() +
    # Dataset 1 (Colores Lancaster)
    geom_bar(data = df1, aes(x = dimension, y = rating, fill = dimension), 
             stat = "identity", width = 1, color = "white", show.legend = FALSE) +
    
    # Dataset 2 (Gris Overlay)
    geom_bar(data = df2, aes(x = dimension, y = rating), 
             stat = "identity", width = 1, fill = "darkgrey", alpha = 0.5, color = "white") +
    
    # Facet: Crea un plot por cada concepto
    facet_wrap(~Word, ncol = 2) + 
    
    scale_y_continuous(breaks = seq(0, 5, 1), limits = c(0, 5)) +
    scale_fill_manual(values = c("#6e40aa","#963db3","#bf3caf","#e4419d","#fe4b83","#ff5e63",
                                 "#ff7847","#fb9633","#e2b72f","#c6d63c","#aff05b")) +
    xlab("") + ylab("Strength") +
    theme_bw() +
    
    # Rejillas Lancaster
    geom_hline(yintercept = seq(0, 5, by = 1), color = "darkgrey", size = .2) +
    geom_vline(xintercept = seq(.5, 11.5, by = 1), color = "darkgrey", size = .2) +
    
    coord_polar(start = 0, direction = 1) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 8, face = "bold"),
      strip.text = element_text(size = 12, face = "bold.italic"), # Estilo del nombre del concepto
      panel.grid = element_blank(),
      strip.background = element_blank()
    )
}