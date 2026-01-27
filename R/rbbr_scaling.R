#' @title This function adjusts the scale of input features to fit within the range of [0,1].
#'
#' @description This function adjusts the scale of input features to fit within the range of [0,1].
#'
#' @param data The original dataset. Each row corresponds to a sample, and each column represents a feature. The target variable (binary label or continuous value) is expected to be in the last column.
#'
#' @return The function returns the scaled features, ensuring they fall within the range of [0,1].
#' @examples
#' # Load dataset
#' data(example_data)
#'
#' # Inspect loaded data
#' head(MAGIC_data)
#'
#' data_scaled   <- rbbr_scaling(MAGIC_data)
#' head(data_scaled)
#' @export
#'
#' @importFrom stats cor predict quantile sd var
rbbr_scaling <- function(data){
  for(i in 1:(ncol(data)-1)){
    data[ ,i]   <- ( data[ ,i] - min(data[ ,i]) )
    q975        <- quantile(as.numeric(unlist(data[ ,i])), probs = 0.975 , na.rm = TRUE)
    data[ ,i]   <- data[ ,i]/q975
    data[ ,i]   <- replace(data[ ,i], data[ ,i]>=1, 0.9999)
  }
  return(data)
}

