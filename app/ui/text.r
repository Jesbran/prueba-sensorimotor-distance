# ui/text.r

library(purrr)
library(magrittr)

# Renderiza una lista de pares de palabras para mostrar (ej: en One-to-one)
render_pairs <- function(pairs, item_sep = " : ", pair_sep = "\n") {
  if (is.null(pairs) || length(pairs) == 0) return("")
  
  text_block <- pairs %>%
    map(paste, collapse = item_sep) %>%
    unlist() %>%
    paste(collapse = pair_sep) %>%
    tolower()
  
  return(text_block)
}

# Renderiza una lista simple de palabras para mostrar (ej: en Many-to-many)
render_list <- function(word_list, item_sep = "\n") {
  if (is.null(word_list) || length(word_list) == 0) return("")
  
  text_block <- word_list %>%
    unlist() %>%
    paste(collapse = item_sep) %>%
    tolower()
  
  return(text_block)
}
