source("calculate/norms.r")
source("calculate/distance.r")

# Pairwise distance between two lists of words (point-to-point)
pairwise_dist_vector <- function(left_words, right_words, distance_type, ds_left, ds_right) {
  dfL <- get_norms(ds_left); dfR <- get_norms(ds_right)
  
  ok <- (left_words %in% dfL$Word) & (right_words %in% dfR$Word)
  out <- rep(NA_real_, length(left_words))
  if (!any(ok)) return(out)
  
  ML <- matrix_for_words(left_words[ok], ds=ds_left)
  MR <- matrix_for_words(right_words[ok], ds=ds_right)
  
  if (distance_type == "mahalanobis") {
    # Convention: in cross comparisons we use the covariance of the left dataset
    covL <- get_covariance_matrix(ds_left)
    dmx <- distance_matrix(ML, MR, distance_type, covariance_matrix=covL)
  } else {
    dmx <- distance_matrix(ML, MR, distance_type)
  }
  
  # Since dmx is a full matrix, we extract only the diagonal (the exact pairs)
  out[ok] <- diag(dmx)
  out
}

# One-to-one (Creates dynamic columns based on selected datasets ds1 and ds2)
distance_table_for_word_pairs <- function(left_words, right_words, distance_type, ds1, ds2) {
  if (length(left_words) != length(right_words)) stop("Word pairs do not match in length")
  if (length(left_words) == 0) return(NULL)
  
  tab <- data.frame(`Word 1` = left_words, `Word 2` = right_words, check.names = FALSE)
  
  lbl1 <- dataset_label(ds1)
  lbl2 <- dataset_label(ds2)
  dist_lbl <- distance_col_name(distance_type)
  
  # If datasets are the same, we only compute one intra-dataset column
  if (ds1 == ds2) {
    col11 <- paste0(dist_lbl, " (", lbl1, " → ", lbl1, ")")
    tab[[col11]] <- pairwise_dist_vector(left_words, right_words, distance_type, ds1, ds1)
  } else {
    # If they are different, we compute the 4 cross combinations
    col11 <- paste0(dist_lbl, " (", lbl1, " → ", lbl1, ")")
    col22 <- paste0(dist_lbl, " (", lbl2, " → ", lbl2, ")")
    col12 <- paste0(dist_lbl, " (", lbl1, " → ", lbl2, ")")
    col21 <- paste0(dist_lbl, " (", lbl2, " → ", lbl1, ")")
    
    tab[[col11]] <- pairwise_dist_vector(left_words, right_words, distance_type, ds1, ds1)
    tab[[col22]] <- pairwise_dist_vector(left_words, right_words, distance_type, ds2, ds2)
    tab[[col12]] <- pairwise_dist_vector(left_words, right_words, distance_type, ds1, ds2)
    tab[[col21]] <- pairwise_dist_vector(left_words, right_words, distance_type, ds2, ds1)
  }
  
  tab
}

# One-to-many 
distance_table_for_one_many <- function(left_word, right_words, distance_type, ds1, ds2) {
  distance_table_for_word_pairs(rep(left_word, length(right_words)), right_words, distance_type, ds1, ds2)
}

# Many-to-many: Returns a list with 4 matrices based on ds1 and ds2
distance_matrices_for_word_pairs <- function(left_words, right_words, distance_type, ds1, ds2, max_words=Inf) {
  if (length(left_words) == 0 || length(right_words) == 0) return(NULL)
  if (length(left_words) > max_words) left_words <- left_words[1:max_words]
  if (length(right_words) > max_words) right_words <- right_words[1:max_words]
  
  build <- function(dsL, dsR) {
    dfL <- get_norms(dsL); dfR <- get_norms(dsR)
    L <- left_words[left_words %in% dfL$Word]
    R <- right_words[right_words %in% dfR$Word]
    if (length(L) == 0 || length(R) == 0) return(NULL)
    
    ML <- matrix_for_words(L, ds = dsL)
    MR <- matrix_for_words(R, ds = dsR)
    
    if (distance_type == "mahalanobis") {
      covL <- get_covariance_matrix(dsL)
      dmx <- distance_matrix(ML, MR, distance_type, covariance_matrix = covL)
    } else {
      dmx <- distance_matrix(ML, MR, distance_type)
    }
    
    out <- data.frame(dmx, row.names = L)
    names(out) <- R
    out
  }
  
  # Keep keys AA, BB, AB, BA so server.r does not need major changes
  if (ds1 == ds2) {
    list(
      AA = build(ds1, ds1),
      BB = NULL,
      AB = NULL,
      BA = NULL
    )
  } else {
    list(
      AA = build(ds1, ds1),
      BB = build(ds2, ds2),
      AB = build(ds1, ds2),
      BA = build(ds2, ds1)
    )
  }
}

# Helper 
distance_list_from_matrix <- function(distance_mx, distance_type) {
  left_words <- rownames(distance_mx)
  rownames(distance_mx) <- NULL
  distance_mx <- cbind(left_words, distance_mx)
  list_table <- distance_mx %>% tidyr::pivot_longer(!left_words, names_to = "Word 2", values_to = "distance")
  names(list_table) <- c("Word 1", "Word 2", distance_col_name(distance_type))
  list_table
}