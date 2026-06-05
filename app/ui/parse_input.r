source("calculate/norms.r")
library(stringr)
library(dplyr)

# Aseguramos que la canonización sea consistente en toda la app
canonise_word <- function(word) {
  word %>% 
    as.character() %>% 
    str_trim() %>% 
    tolower() %>% 
    str_squish()
}

get_word_pairs <- function(word_pairs_block) {
  
  left_words <- list(); right_words <- list()
  words_not_found <- list(); malformed_lines <- list()
  
  if (is.null(word_pairs_block) || nchar(word_pairs_block) == 0) {
    return(list(left_words=left_words, right_words=right_words,
                words_not_in_norms=words_not_found, malformed_lines=malformed_lines))
  }
  
  # all_words_union() ahora viene de meta.r y contiene palabras de los 15 datasets
  valid_vocab <- all_words_union()
  lines <- word_pairs_block %>% strsplit("\n") %>% unlist
  
  for (line in lines) {
    bare <- str_trim(line)
    if (nchar(bare) == 0) next
    
    # Separadores aceptados: dos puntos, punto y coma, coma, tabulación
    pair <- bare %>% strsplit("[:;,\t]") %>% unlist
    pair <- Filter(function(s) s != "", str_trim(pair))
    
    if (length(pair) != 2) { 
      malformed_lines[length(malformed_lines)+1] = bare
      next 
    }
    
    w1 <- canonise_word(pair[1]); w2 <- canonise_word(pair[2])
    
    # Verificar si existen en el universo total de normas
    if (!(w1 %in% valid_vocab)) words_not_found[length(words_not_found)+1] <- w1
    if (!(w2 %in% valid_vocab)) words_not_found[length(words_not_found)+1] <- w2
    
    # Aceptamos el par si ambos existen en al menos UN dataset del sistema
    if ((w1 %in% valid_vocab) && (w2 %in% valid_vocab)) {
      left_words[length(left_words)+1]  <- w1
      right_words[length(right_words)+1] <- w2
    }
  }
  
  list(
    left_words = unlist(left_words),
    right_words = unlist(right_words),
    words_not_in_norms = unique(unlist(words_not_found)),
    malformed_lines = unlist(malformed_lines)
  )
}

get_words <- function(words_block) {
  
  words <- list(); missing <- list()
  
  if (is.null(words_block) || nchar(words_block) == 0) {
    return(list(words=words, missing=missing))
  }
  
  valid_vocab <- all_words_union()
  # Separar por comas, puntos y coma, tabs o saltos de línea
  raw_list <- words_block %>% strsplit("[:;,\t\n]") %>% unlist
  
  for (item in raw_list) {
    bare <- str_trim(item)
    if (nchar(bare) == 0) next
    
    w <- canonise_word(bare)
    if (w %in% valid_vocab) {
      words[length(words)+1] <- w 
    } else {
      missing[length(missing)+1] <- w
    }
  }
  
  list(
    words = unique(unlist(words)), 
    missing = unique(unlist(missing))
  )
}

try_parse_float <- function(input, default_value, empty_to_default=FALSE) {
  if (is.null(input) || (empty_to_default && (nchar(str_trim(input)) == 0))) {
    return(list(value=default_value, success=TRUE, original=input))
  }
  
  val <- suppressWarnings(as.numeric(input))
  if (is.na(val)) {
    return(list(value=default_value, success=FALSE, original=input))
  } else {
    return(list(value=val, success=TRUE, original=input))
  }
}
