# ==============================================================================
# SENSORIMOTOR T-SNE PRECOMPUTATION SCRIPT
# ==============================================================================
# This script automates the generation of 3D t-SNE coordinates for multiple 
# sensorimotor datasets (Human norms and LLM-generated data) across different 
# languages (English, Chinese, Italian, Croatian, and Spanish).
#
# KEY FEATURES AND OPTIMIZATIONS:
#
# 1. DYNAMIC DATA LOADING:
#    The script automatically detects and reads both .csv and .xlsx files 
#    based on the dataset_config list, ensuring a unified processing pipeline.
#
# 2. ADAPTIVE PERPLEXITY (for small datasets):
#    The t-SNE algorithm has a mathematical requirement: the number of samples 
#    must be significantly larger than the 'perplexity' parameter. 
#    For small datasets (such as the Spanish norms), a standard perplexity 
#    of 30 causes a crash. This script dynamically calculates the maximum 
#    allowed perplexity using the formula floor((N - 1) / 3), allowing the 
#    analysis to run even on very small word lists without errors.
#
# 3. ZERO-VARIANCE HANDLING & JITTERING (for correlation distance):
#    Correlation distance is mathematically impossible to calculate if a word 
#    has the exact same value across all 11 sensorimotor dimensions (zero 
#    variance), resulting in a division by zero (NaN). 
#    To resolve this without removing words, the script applies "Jittering": 
#    it adds microscopic random noise (between 1e-8 and 1e-7) to problematic 
#    rows. This is invisible to the user but stabilizes the math, ensuring 
#    the t-SNE can be computed for every word in the files.
#
# 4. ROBUST DATA CLEANING:
#    The script handles missing values (NAs) by automatically filling them 
#    with zeros. This prevents calculation failures and ensures that the 
#    resulting binary cache files contain coordinates for every single word 
#    found in the original source files.
#
# 5. BINARY CACHING SYSTEM:
#    The final 3D coordinates are saved in a custom little-endian binary format 
#    (.bin). This allows the Shiny App to load thousands of coordinates 
#    instantly for any selected language/model/distance combination.
# ==============================================================================

library(Rtsne)
library(proxy)
library(readxl)
library(tools)
library(dplyr)

# ------------------ CONFIG ------------------
base_dir <- path.expand("~/Desktop/UNAM/Experimentos/SENSORIMOTOR/PAPIIT/Archivos_Lancaster_para_LLMs/Shiny/sensorimotor-distance-LLMs_2")
data_dir <- file.path(base_dir, "app", "data")

dataset_config <- list(
  en_human     = list(label = "English (Human)", file = "sensorimotor_norms_Lancaster_matched.csv"),
  en_GPT       = list(label = "English (GPT)", file = "GPT_sensorimotor_norms_T0_English.csv"),
  en_deepseek  = list(label = "English (DeepSeek)", file = "DeepSeek_sensorimotor_norms_T0_English.csv"),
  ch_human     = list(label = "Chinese (Human)", file = "Chinese_sensorimotor_norms.csv"),
  ch_GPT       = list(label = "Chinese (GPT)", file = "GPT_sensorimotor_norms_T0_Chinese.csv"),
  ch_deepseek  = list(label = "Chinese (DeepSeek)", file = "DeepSeek_sensorimotor_norms_T0_Chinese.csv"),
  cr_human     = list(label = "Croatian (Human)", file = "Croatian_sensorimotor_norms.csv"),
  cr_GPT       = list(label = "Croatian (GPT)", file = "GPT_sensorimotor_norms_T0_Croatian.csv"),
  cr_deepseek  = list(label = "Croatian (DeepSeek)", file = "DeepSeek_sensorimotor_norms_T0_Croatian.csv"),
  it_human     = list(label = "Italian (Human)", file = "Italian_sensorimotor_norms.csv"),
  it_GPT       = list(label = "Italian (GPT)", file = "GPT_sensorimotor_norms_T0_Italian.csv"),
  it_deepseek  = list(label = "Italian (DeepSeek)", file = "DeepSeek_sensorimotor_norms_T0_Italian.csv"),
  es_human     = list(label = "Spanish (Human)", file = "spanish_mex_sensorimotor_norms.csv"),
  es_GPT       = list(label = "Spanish (GPT)", file = "GPT_sensorimotor_norms_T0_Spanish.csv"),
  es_deepseek  = list(label = "Spanish (DeepSeek)", file = "DeepSeek_sensorimotor_norms_T0_Spanish.csv")
)

dims <- 3
distances <- c("cosine", "euclidean", "minkowski-3", "correlation", "mahalanobis")

default_perplexity <- 30
max_iter <- 1000
eta <- 200
seed <- 1

feature_cols <- c(
  "Auditory.mean","Gustatory.mean","Haptic.mean","Interoceptive.mean","Olfactory.mean","Visual.mean",
  "Foot_leg.mean","Hand_arm.mean","Head.mean","Mouth.mean","Torso.mean"
)

# --- Helpers ---

canonise_word <- function(x) tolower(trimws(gsub("\\s+", " ", x)))

standardise_norms <- function(df) {
  if (!("Word" %in% names(df))) stop("Column 'Word' is missing")
  df$Word <- canonise_word(df$Word)
  df
}

read_any_data <- function(path) {
  ext <- tolower(file_ext(path))
  if (ext == "csv") {
    return(read.csv(path, stringsAsFactors = FALSE))
  } else if (ext %in% c("xlsx", "xls")) {
    return(as.data.frame(read_excel(path)))
  } else {
    stop(paste("Format not supported:", ext))
  }
}

write_cache_bin <- function(mat, bin_path) {
  con <- file(bin_path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.integer(nrow(mat)), con, size = 4, endian = "little")
  writeBin(as.integer(ncol(mat)), con, size = 4, endian = "little")
  for (j in seq_len(ncol(mat))) {
    writeBin(as.double(mat[, j]), con, size = 8, endian = "little")
  }
}

distance_matrix <- function(X, distance_name) {
  dn <- tolower(distance_name)
  if (dn == "euclidean") return(as.matrix(dist(X, method = "euclidean")))
  if (dn == "minkowski-3") return(as.matrix(dist(X, method = "minkowski", p = 3)))
  if (dn %in% c("cosine", "correlation")) return(as.matrix(proxy::dist(X, method = dn)))
  if (dn == "mahalanobis") {
    S  <- cov(X) + diag(1e-10, ncol(X))
    VI <- solve(S)
    R  <- chol(VI)
    X2 <- X %*% R
    return(as.matrix(dist(X2, method = "euclidean")))
  }
  stop("Distance not supported")
}

compute_tsne <- function(X, dims, distance_name) {
  
  # 1. Adaptive Perplexity
  n_samples <- nrow(X)
  max_perp <- floor((n_samples - 1) / 3)
  current_perp <- min(default_perplexity, max_perp)
  if (current_perp < 1) current_perp <- 1
  
  if (current_perp != default_perplexity) {
    message(sprintf("    ! Using perplexity: %d", current_perp))
  }
  
  # 2. Correlation Jittering
  # If any row has zero variance (all 11 dimensions are the same), 
  # correlation becomes NaN. We add a tiny bit of noise to fix the math.
  row_vars <- apply(X, 1, var)
  problematic_rows <- which(row_vars == 0 | is.na(row_vars))
  
  if (length(problematic_rows) > 0) {
    message(sprintf("    ! Adding jitter to %d zero-variance words", length(problematic_rows)))
    X[problematic_rows, ] <- X[problematic_rows, ] + 
      matrix(runif(length(problematic_rows) * ncol(X), 1e-8, 1e-7), 
             nrow = length(problematic_rows))
  }
  
  D <- distance_matrix(X, distance_name)
  
  # Ensure no NaNs exist in the distance matrix before starting Rtsne
  if (any(is.na(D))) stop("Distance matrix contains NaNs. Jittering failed.")
  
  set.seed(seed)
  fit <- Rtsne(D, is_distance = TRUE, dims = dims,
               perplexity = current_perp, max_iter = max_iter, eta = eta,
               check_duplicates = FALSE, verbose = FALSE)
  fit$Y
}

# --- Main Loop ---

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

for (ds_id in names(dataset_config)) {
  cfg <- dataset_config[[ds_id]]
  file_path <- file.path(data_dir, cfg$file)
  
  message("\n>>> Processing: ", cfg$label, " [ID: ", ds_id, "]")
  
  if (!file.exists(file_path)) {
    message("  - File not found. Skipping.")
    next
  }
  
  # Read and handle missing values
  df <- read_any_data(file_path) %>% standardise_norms()
  
  # We keep all rows, but we ensure there are no NA in the dimensions
  # If a row has NAs, we replace them with 0 to avoid crashing
  df[is.na(df)] <- 0
  
  X <- as.matrix(df[, feature_cols])
  storage.mode(X) <- "double"
  
  if (nrow(X) < 2) {
    message("  - Too few samples. Skipping.")
    next
  }
  
  for (d in distances) {
    message("  - Calculating t-SNE (", d, ")...")
    tryCatch({
      Y <- compute_tsne(X, dims = dims, distance_name = d)
      out_name <- sprintf("t-SNE_%d_%s_cache_%s.bin", dims, tolower(d), ds_id)
      write_cache_bin(Y, file.path(data_dir, out_name))
    }, error = function(e) {
      message("    - Error in ", d, ": ", e$message)
    })
  }
}



