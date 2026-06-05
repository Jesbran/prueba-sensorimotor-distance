source("calculate/distance.r")
source("calculate/norms.r")

# Calculate nearest neighbours for a concept within a specific dataset
neighbours_table <- function(word, distance_type, count, radius, ds) {
  # 'ds' is now the dataset ID (e.g., "en_deepseek", "it_human")
  
  if (is.null(radius) || radius <= 0) radius <- Inf
  
  # Fetch norms for the selected dataset
  df <- get_norms(ds)
  
  # Return NULL if the word is not in the dataset vocabulary
  if (!(word %in% df$Word)) return(NULL)
  
  # Calculate distances between the target word and all words in the dataset
  if (distance_type == "mahalanobis") {
    mx <- distance_matrix(
      vector_for_word(word, ds = ds), 
      matrix_for_words(all_words(ds), ds = ds),
      distance_type, 
      covariance_matrix = get_covariance_matrix(ds)
    )
  } else {
    mx <- distance_matrix(
      vector_for_word(word, ds = ds), 
      matrix_for_words(all_words(ds), ds = ds), 
      distance_type
    )
  }
  
  # Sorting indices by distance
  # Index 1 is the word itself (distance 0), so we start from index 2
  argsort <- order(mx)
  nearest_idxs <- argsort[2:(count + 1)]
  
  # Ensure we don't go out of bounds if dataset is small
  nearest_idxs <- nearest_idxs[!is.na(nearest_idxs)]
  
  nearest_words <- all_words(ds)[nearest_idxs]
  nearest_distances <- mx[nearest_idxs]
  
  # Filter by radius if a limit was set
  if (!is.infinite(radius)) {
    keep <- nearest_distances <= radius
    nearest_distances <- nearest_distances[keep]
    nearest_words <- nearest_words[keep]
  }
  
  # Construct result table
  table <- data.frame(
    Order = seq_along(nearest_words),
    Concept = nearest_words,
    distance = nearest_distances,
    stringsAsFactors = FALSE
  )
  
  # Set the specific distance name as the column header (e.g., "Cosine distance")
  names(table)[3] <- distance_col_name(distance_type)
  
  return(table)
}
