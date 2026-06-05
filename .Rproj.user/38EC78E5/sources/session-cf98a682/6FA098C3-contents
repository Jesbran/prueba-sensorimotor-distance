# calculate/distance.r
library(rdist)
library(R.utils)

# Gets the canonical display name for a distance_type
distance_name <- function(distance_type) {
  # Handle minkowski-3/minkowski3 for consistency with .bin cache files
  if (distance_type == "minkowski-3" || distance_type == "minkowski3") {
    return("Minkowski-3")
  }
  else if(distance_type == "euclidean") {
    return("Euclidean")
  }
  else if(distance_type == "cosine") {
    return("Cosine")
  }
  else if(distance_type == "correlation") {
    return("Correlation")
  }
  else if(distance_type == "mahalanobis") {
    return("Mahalanobis")
  }
  else {
    stop(paste("Unsupported distance type:", distance_type))
  }
}

# Gets the display column name for a distance type
distance_col_name <- function(distance_type) {
  name <- paste0(distance_name(distance_type), " distance")
  # capitalize comes from R.utils package
  name <- capitalize(tolower(name))  
  return(name)
}

# Computes a distance matrix from two data matrices
distance_matrix <- function(matrix_left, matrix_right, distance_type, covariance_matrix = NULL) {
  
  if (distance_type == "minkowski-3" || distance_type == "minkowski3") {
    distances <- cdist(matrix_left, matrix_right, metric="minkowski", p=3)
  }
  else if(distance_type == "euclidean") {
    distances <- cdist(matrix_left, matrix_right, metric="euclidean", p=2)
  }
  else if(distance_type == "cosine") {
    # Manual cosine calculation for higher precision
    distances <- cdist(matrix_left, matrix_right, metric=function(x, y) {
      1 - (sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2))))
    })
  }
  else if(distance_type == "correlation") {
    # For some reason, when you tell rdist to do "correlation distance", it
    # actually does square root of half the correlation distance
    # this is probably to make it a metric distance.
    # either way, this is not what we want, and we correct it here
    distances <- cdist(matrix_left, matrix_right, metric="correlation") ^ 2 * 2
  }
  else if(distance_type == "mahalanobis") {
    if (is.null(covariance_matrix)) stop("Mahalanobis requires a covariance matrix.")
    
    # solve() with a tiny regularization term to prevent errors with singular matrices
    invCov <- solve(covariance_matrix + diag(1e-10, ncol(covariance_matrix)))
    
    distances <- cdist(matrix_left, matrix_right, metric=function(x, y) {
      diff <- x - y
      sqrt(t(diff) %*% invCov %*% diff)
    })
  }
  else {
    stop(paste("Unsupported distance type:", distance_type))
  }
  
  return(distances)
}
  
  
