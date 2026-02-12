#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom utils combn txtProgressBar setTxtProgressBar

# helper ---------------------------------------------------------------

rbbr_multiply_vector <- function(vec) {
  prod(vec)
}

rbbr_transformation <- function(x){
  x_transformed <- rbbr_multiply_vector(x)

  for(k in seq_along(x)){
    C <- utils::combn(length(x), k)
    for(j in seq_len(ncol(C))){
      x_complement <- x
      x_complement[C[, j]] <- 1 - x_complement[C[, j]]
      x_transformed <- c(x_transformed, rbbr_multiply_vector(x_complement))
    }
  }
  x_transformed
}

rbbr_sigmoid <- function(x, slope){
  1 / (1 + exp(-slope * (x - 0.5)))
}

# core prediction for one sample --------------------------------------

rbbr_ynew <- function(trained_model, k, i, x_new, slope){

  split_string  <- strsplit(as.character(k), ".", fixed = TRUE)[[1]]

  get_logic <- function(k, idx){
    obj <- trained_model$LOGIC_VALUES[[as.character(k)]]
    if(is.vector(obj)) obj[[1]] else obj[[idx,1]]
  }

  # ---- single rule ----
  if(length(split_string)==1){

    k <- as.numeric(k)
    logic_vec <- get_logic(k, i)

    nodes_index <- logic_vec[1:k]
    coef_index  <- logic_vec[(k+1):(k+2+2^k-1)]

    x1 <- rbbr_transformation(x_new[nodes_index])
    y1 <- sum(coef_index * c(1, x1))
    y  <- rbbr_sigmoid(y1, slope)

    # ---- two-layer rule ----
  } else {

    logic_vec <- get_logic(k, i)
    i1 <- logic_vec[1]; i2 <- logic_vec[2]
    k1 <- as.numeric(split_string[1]); k2 <- as.numeric(split_string[2])

    L1 <- get_logic(k1, i1)
    L2 <- get_logic(k2, i2)

    nodes1 <- L1[1:k1]; coef1 <- L1[(k1+1):(k1+2+2^k1-1)]
    nodes2 <- L2[1:k2]; coef2 <- L2[(k2+1):(k2+2+2^k2-1)]

    x1 <- rbbr_transformation(x_new[nodes1])
    x2 <- rbbr_transformation(x_new[nodes2])

    y1 <- rbbr_sigmoid(sum(coef1 * c(1, x1)), slope)
    y2 <- rbbr_sigmoid(sum(coef2 * c(1, x2)), slope)

    coef_added_layer <- logic_vec[(k1+k2+3):(k1+k2+4+2^2-1)]

    y <- c(1, y1*y2, (1-y1)*y2, y1*(1-y2), (1-y1)*(1-y2))
    y <- rbbr_sigmoid(sum(y * coef_added_layer), slope)
  }

  prob <- exp(-10 * min(y, abs(1-y)))
  c(y, prob)
}

# main exported function -----------------------------------------------

#' @title Predict Using a Trained RBBR Model
#'
#' @description Make predictions for new datapoints by utilizing a trained RBBR model.
#'
#' @param trained_model Model returned by `rbbr_train()`
#' @param data_test The new dataset for which we want to predict the target class or label probability. Each sample is represented as a row, and features are in columns.
#' @param num_top_rules Number of Boolean rules with the best Bayesian Information Criterion (BIC) scores to be used for prediction. The default value is 1.
#' @param slope The slope parameter for the Sigmoid activation function. Default is 10.
#' @param num_cores Number of parallel workers to use for computation. Adjust according to your system. Default is NA (automatic selection).
#' @param verbose Logical. If TRUE, progress messages are shown. Default is FALSE.
#'
#' @return Numeric vector of predicted probabilities (length = nrow(data_test))
#' @examples
#' # Load dataset
#' data(example_data)
#'
#' # Inspect loaded data
#' head(XOR_data)
#'
#' # For fast run, use the first three input features to predict target class in column 11
#' data_train <- XOR_data[1:800, c(1,2,3,11)]
#' data_test <- XOR_data[801:1000, c(1,2,3,11)]
#'
#' # training model
#' trained_model <- rbbr_train(data_train,
#'                             max_feature = 2,
#'                             num_cores = 1, verbose = TRUE)
#'
#' head(trained_model$boolean_rules)
#'
#' # testing model
#' data_test_x <- data_test[ ,1:(ncol(data_test)-1)]
#' labels <- data_test[ ,ncol(data_test)]
#'
#' predicted_label_probabilities <- rbbr_predictor(trained_model,
#'                                    data_test_x,
#'                                    num_top_rules = 1,
#'                                    num_cores = 1, verbose = TRUE)
#'
#' head(predicted_label_probabilities)
#' head(labels) # true labels
#'
#' @export
rbbr_predictor <- function(trained_model,
                           data_test,
                           num_top_rules = 1,
                           slope = 10,
                           num_cores = 1,
                           verbose = FALSE){

  # ---- input validation ----
  if(!is.list(trained_model))
    stop("trained_model must be a list")

  if(is.null(trained_model$LOGIC_VALUES) || is.null(trained_model$gate_info))
    stop("trained_model is not valid")

  if(!is.data.frame(data_test) && !is.matrix(data_test))
    stop("data_test must be a data.frame or matrix")

  if(!is.numeric(num_top_rules) || num_top_rules < 1)
    stop("num_top_rules must be >= 1")

  if(!is.numeric(slope) || slope <= 0)
    stop("slope must be > 0")

  data_test <- as.matrix(data_test)

  if (is.na(num_cores)) {
    num_cores <- parallel::detectCores()
  }

  if (verbose) message("predicting started with ", num_cores," computing cores")

  use_parallel <- num_cores > 1
  if(use_parallel){
    cl <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(cl)
    on.exit({
      parallel::stopCluster(cl)
      foreach::registerDoSEQ()
    }, add = TRUE)
  }

  boolean_rules_sorted <- trained_model$gate_info
  num_top_rules <- min(num_top_rules, nrow(boolean_rules_sorted))
  boolean_rules_sorted <- boolean_rules_sorted[1:num_top_rules, ]

  n_samples <- nrow(data_test)
  n_rules   <- nrow(boolean_rules_sorted)

  prediction <- matrix(NA_real_, n_rules, n_samples)
  prob_matrix<- matrix(NA_real_, n_rules, n_samples)

  `%op%` <- if(use_parallel) foreach::`%dopar%` else foreach::`%do%`

  results <- foreach::foreach(row = seq_len(n_rules),
                              .export=c("rbbr_ynew","rbbr_transformation",
                                        "rbbr_sigmoid","rbbr_multiply_vector")) %op% {

                                          k <- boolean_rules_sorted[row,4]
                                          i <- boolean_rules_sorted[row,5]

                                          pred <- numeric(n_samples)
                                          prob <- numeric(n_samples)

                                          for(s in seq_len(n_samples)){
                                            out <- rbbr_ynew(trained_model, k, i, data_test[s,], slope)
                                            pred[s] <- out[1]
                                            prob[s] <- out[2]
                                          }

                                          list(pred=pred, prob=prob, row=row)
                                        }

  for(res in results){
    prediction[res$row,]  <- res$pred
    prob_matrix[res$row,] <- res$prob
  }

  # ---- BIC weighting ----
  BIC <- boolean_rules_sorted$BIC
  lambda <- 10
  distances <- abs((BIC - BIC[1]) / BIC[1])
  RW <- exp(-lambda * distances)
  RW <- RW / sum(RW)

  weighted_prob <- sweep(prob_matrix, 1, RW, "*")
  best_rule <- apply(weighted_prob, 2, which.max)

  prediction[cbind(best_rule, seq_len(n_samples))]
}
