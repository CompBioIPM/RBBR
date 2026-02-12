#' @title Scale features to [0,1] range
#'
#' @description Scales input features to the [0,1] interval using the 97.5th percentile of each feature.
#' The last column (target) is not scaled.
#'
#' @param data A numeric dataset. Each row is a sample and each column a feature. The target variable is expected in the last column.
#'
#' @return A dataset with scaled features (all columns except the last), capped at 0.9999.
#' @examples
#' # Load dataset
#' data(example_data)
#'
#' # Inspect loaded data
#' head(MAGIC_data)
#'
#' # Scale features
#' data_scaled <- rbbr_scaling(MAGIC_data)
#' head(data_scaled)
#'
#' @export
#' @importFrom stats quantile
rbbr_scaling <- function(data){
  for(i in 1:(ncol(data)-1)){
    if(!is.numeric(data[, i])) next
    min_val <- min(data[, i], na.rm = TRUE)
    data[, i] <- data[, i] - min_val
    q975 <- quantile(data[, i], probs = 0.975, na.rm = TRUE)
    data[, i] <- data[, i] / q975
    data[, i] <- pmin(data[, i], 0.9999)
  }
  return(data)
}
