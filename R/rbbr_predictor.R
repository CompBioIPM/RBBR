#' @title This function is designed to make predictions on a new dataset by utilizing a trained model.
#'
#' @description This function is designed to make predictions on a new dataset by utilizing a trained model.
#'
#' @param trained_model The model that has been trained using the rbbr_train() function on a training dataset.
#' @param data_test The new dataset for which we want to predict the target value or label probability. Each sample is represented as a row, and the input features are in columns.
#' @param num_top_rules Specifies the number of Boolean rules with the best Bayesian Information Criterion (BIC) scores to be used for prediction. The default value is 1.
#' @param slope The slope parameter used in the Sigmoid activation function. The default value is 10.
#' @param num_cores Specify the number of parallel workers (adjust according to your system).
#'
#' @return This function returns the predicted target value or label probability for the input dataset. For each sample in the input dataset (represented as rows), rbbr_predictor() provides the corresponding predicted value or label probability.
#' @export
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom stats cor predict quantile sd var
#' @importFrom utils combn txtProgressBar
rbbr_predictor <- function(trained_model, data_test, num_top_rules = NA, slope = NA, num_cores = NA){

  if(is.na(num_top_rules)){
    num_top_rules   <- 1
  }
  if(is.na(slope)){
    slope           <- 10
  }
  if(is.na(num_cores)){
    num_cores       <- parallel::detectCores()
  }
  ##############################################################################
  rbbr_ynew <- function(trained_model, k, i, x_new, slope){

    transformation <- function(x){
      multiply_vector <- function(vec) {
        result <- 1
        for (i in 1:length(vec)) {
          result <- result * vec[i]
        }
        return(result)
      }
      x_transformed <- c(multiply_vector(x))
      for(k in 1:length(x)){
        C   <- combn(length(x), k)
        for(j in 1:ncol(C)){
          x_complement  <- x
          not_elements  <- C[ ,j]
          for(i in not_elements){
            x_complement[i] <- 1 - x_complement[i]
          }
          x_transformed <- c(x_transformed, multiply_vector(x_complement))
        }
      }
      return(x_transformed)
    }

    sigmoid <- function(x) {
      1 / (1 + exp(-slope*(x-0.5)) )
    }
    split_string  <- strsplit(as.character(k), ".", fixed = TRUE)[[1]]

    if(length(split_string)==1){
      k             <- as.numeric(k)

      if(!is.vector(trained_model$LOGIC_VALUES[[as.character(k)]])){
        nodes_index1<- trained_model$LOGIC_VALUES[[as.character(k)]][[i,1]][1:k]                # regulators
        coef_index1 <- trained_model$LOGIC_VALUES[[as.character(k)]][[i,1]][(k+1):(k+2+2^k-1)]  # coefficient
      }else{
        nodes_index1<- trained_model$LOGIC_VALUES[[as.character(k)]][[1]][1:k]                  # regulators
        coef_index1 <- trained_model$LOGIC_VALUES[[as.character(k)]][[1]][(k+1):(k+2+2^k-1)]    # coefficient
      }

      x1            <- unlist( transformation(x_new[nodes_index1]) )

      # Add intercept term
      x1_with_intercept <- c(1, x1)

      # Apply coefficients for prediction
      y1          <- sum(coef_index1 * x1_with_intercept)
      y           <- sigmoid(y1)
    }else if(length(split_string)==2){
      if(!is.vector(trained_model$LOGIC_VALUES[[as.character(k)]])){
        i_index1  <- trained_model$LOGIC_VALUES[[as.character(k)]][[i,1]][1]
        i_index2  <- trained_model$LOGIC_VALUES[[as.character(k)]][[i,1]][2]
      }else{
        i_index1  <- trained_model$LOGIC_VALUES[[as.character(k)]][[1]][1]
        i_index2  <- trained_model$LOGIC_VALUES[[as.character(k)]][[1]][2]
      }

      k1          <- as.numeric(split_string[1])
      k2          <- as.numeric(split_string[2])

      if(!is.vector(trained_model$LOGIC_VALUES[[as.character(k1)]])){
        nodes_index1<- trained_model$LOGIC_VALUES[[as.character(k1)]][[i_index1,1]][1:k1]
        coef_index1 <- trained_model$LOGIC_VALUES[[as.character(k1)]][[i_index1,1]][(k1+1):(k1+2+2^k1-1)]
      }else{
        nodes_index1<- trained_model$LOGIC_VALUES[[as.character(k1)]][[1]][1:k1]
        coef_index1 <- trained_model$LOGIC_VALUES[[as.character(k1)]][[1]][(k1+1):(k1+2+2^k1-1)]
      }

      if(!is.vector(trained_model$LOGIC_VALUES[[as.character(k2)]])){
        nodes_index2<- trained_model$LOGIC_VALUES[[as.character(k2)]][[i_index2,1]][1:k2]
        coef_index2 <- trained_model$LOGIC_VALUES[[as.character(k2)]][[i_index2,1]][(k2+1):(k2+2+2^k2-1)]
      }else{
        nodes_index2<- trained_model$LOGIC_VALUES[[as.character(k2)]][[1]][1:k2]
        coef_index2 <- trained_model$LOGIC_VALUES[[as.character(k2)]][[1]][(k2+1):(k2+2+2^k2-1)]
      }

      sub_logics  <- 2
      sub_elements<- k1 + k2

      if(!is.vector(trained_model$LOGIC_VALUES[[as.character(k)]])){
        coef_added_layer <- trained_model$LOGIC_VALUES[[as.character(k)]][[i,1]][(sub_logics + sub_elements + 1):(sub_logics + sub_elements + 2 + 2^(sub_logics)-1)]
      }else{
        coef_added_layer <- trained_model$LOGIC_VALUES[[as.character(k)]][[1]][(sub_logics + sub_elements + 1):(sub_logics + sub_elements + 2 + 2^(sub_logics)-1)]
      }

      x1          <- unlist( transformation(x_new[nodes_index1]) )
      x2          <- unlist( transformation(x_new[nodes_index2]) )

      # Add intercept term
      x1_with_intercept <- c(1, x1)
      x2_with_intercept <- c(1, x2)

      # Apply coefficients for prediction
      y1          <- sum(coef_index1 * x1_with_intercept)
      y2          <- sum(coef_index2 * x2_with_intercept)

      y1          <- sigmoid(y1)
      y2          <- sigmoid(y2)

      y           <- c(1, y1*y2, (1-y1)*y2, y1*(1-y2), (1-y1)*(1-y2) )
      y           <- sum(y * coef_added_layer)
      y           <- sigmoid(y)
    }

    prob          <- exp( -10*min(y, abs(1-y)) )
    output        <- c(y, prob)
    return(output)
  }
  ##############################################################################
  cl                <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)

  boolean_rules_sorted <- trained_model$gate_info
  num_top_rules        <- min(num_top_rules, nrow(boolean_rules_sorted))
  boolean_rules_sorted <- boolean_rules_sorted[1:num_top_rules, ]

  results <- foreach::foreach(row = 1:nrow(boolean_rules_sorted), .combine = rbind) %dopar% {
    print(row)
    k               <- boolean_rules_sorted[row, 4]
    i               <- boolean_rules_sorted[row, 5]

    prediction_tmp  <- c()
    prob_tmp        <- c()
    for(sample_index in 1:nrow(data_test)){
      x_new         <- data_test[sample_index, ]
      y_pred        <- rbbr_ynew(trained_model, k, i, x_new, slope)
      prediction_tmp<- c(prediction_tmp, y_pred[1])
      prob_tmp      <- c(prob_tmp, y_pred[2])
    }
    list(prediction = prediction_tmp,
         prob_matrix= prob_tmp,
         row        = row)
  } # end for row
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()

  prediction        <- c()
  prob_matrix       <- c()
  if(num_top_rules>1){

    # Sort the data frame by the descending order of the last column
    results         <- results[order(unlist(results[, ncol(results)]), decreasing = FALSE), ]
    for(row in 1:nrow(results)){
      prediction    <- rbind(prediction , results[row,1][[1]])
      prob_matrix   <- rbind(prob_matrix, results[row,2][[1]])
    }
  }else{
    prediction      <- rbind(prediction , results[1][[1]])
    prob_matrix     <- rbind(prob_matrix, results[2][[1]])
  }
  ##############################################################################
  lambda            <- 10
  BIC               <- boolean_rules_sorted$BIC
  distances         <- abs((BIC - BIC[1])/BIC[1])
  RW                <- exp(-lambda*distances)/sum(exp(-lambda*distances))
  for(num_top_rules in 1:nrow(prediction)){
    if(num_top_rules>1){
      L_DATA_MO     <- prob_matrix[1:num_top_rules, ]
      for (i in 1:nrow(L_DATA_MO)) {
        L_DATA_MO[i, ]<- L_DATA_MO[i, ] * RW[i]
      }
      sorted_indexes<- apply( L_DATA_MO, 2, function(x) order(-x) )
      column_max    <- c()
      for(sample_index in 1:ncol(sorted_indexes)){
        ind         <- sorted_indexes[ , sample_index]
        column_max  <- c(column_max,  prediction[ind[1], sample_index])
      }
    }else{
      column_max    <- prediction[1, ]
    }
  } # end for num_top_rules
  return(column_max)
}
