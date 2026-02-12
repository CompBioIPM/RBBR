#' @title Trains RBBR to learn Boolean rules
#'
#' @description Regression-Based Boolean Rule (RBBR) inference is performed on datasets where the input and target features are either binarized or continuous within the [0,1] range.
#'
#' @param data The dataset with scaled features within the [0,1] interval. Each row represents a sample and each column represents a feature. The target variable must be in the last column.
#' @param max_feature The maximum number of input features allowed in a Boolean rule. The default value is 3.
#' @param mode Choose between "1L" for fitting 1-layered models or "2L" for fitting 2-layered models. The default value is "1L".
#' @param slope The slope parameter used in the Sigmoid activation function. The default value is 10.
#' @param weight_threshold Conjunctions with weights above this threshold in the fitted ridge regression models will be printed as active conjunctions in the output. The default value is 0.
#' @param balancing Logical. This is for adjusting the distribution of target classes or categories within a dataset to ensure that each class is adequately represented. The default value is TRUE. Set it to FALSE, if you don't need to perform the data balancing.
#' @param num_cores Number of parallel workers to use for computation. Adjust according to your system. Default is NA (automatic selection).
#' @param verbose Logical. If TRUE, progress messages and a progress bar are shown. Default is FALSE.
#'
#' @return This function outputs the predicted Boolean rules with the best Bayesian Information Criterion (BIC).
#' @examples
#' # Load dataset
#' data(example_data)
#'
#' # Example for training a two-layer model
#' head(OR_data)
#'
#' # For fast run, use the first three input features to predict target class in column five
#' data_train   <- OR_data[1:800, c(1,2,3,5)]
#' data_test    <- OR_data[801:1000, c(1,2,3,5)]
#'
#' # training model
#' trained_model <- rbbr_train(data_train,
#'                            max_feature = 2,
#'                            mode = "2L",
#'                            balancing = FALSE,
#'                            num_cores = 1, verbose = TRUE)
#'
#' head(trained_model$boolean_rules)
#'
#' # testing model
#' data_test_x  <- data_test[ ,1:(ncol(data_test)-1)]
#' labels       <- data_test[ ,ncol(data_test)]
#'
#' predicted_label_probabilities <- rbbr_predictor(trained_model,
#'                                    data_test_x,
#'                                    num_top_rules = 10,
#'                                    num_cores = 1, verbose = TRUE)
#'
#' head(predicted_label_probabilities)
#'
#' @export
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom stats cor predict quantile sd var
#' @importFrom utils combn txtProgressBar
rbbr_train <- function(data, max_feature = 3, mode = "1L", slope = 10, weight_threshold = 0, balancing = TRUE, num_cores = NA, verbose = FALSE){
  if(is.na(num_cores)){
    num_cores <- parallel::detectCores()
  }

  if (verbose) message("training process started with ", num_cores," computing cores")

  # Create a progress bar
  progress_percent <- 0
  if (verbose) {
    pb <- txtProgressBar(min = 0, max = 100, style = 3, width = 20)
  }
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  sigmoid <- function(x) {
    1 / (1 + exp(-slope*(x-0.5)) )
  }

  ToComputeLogic <- function(X, ORD) {
    if (is.vector(X)) {
      SAMP <- X
    }else {
      SAMP <- X[, c(ORD)]
    }
    ncol_SAMP <- length(c(ORD))
    if (length(c(ORD)) > 1) {
      SAMP_PARTITIONED <- matrix(rep(1, nrow(SAMP) * (2^ncol_SAMP)),
                                 nrow(SAMP), 2^ncol_SAMP)
    }else {
      SAMP <- matrix(SAMP, ncol = ncol_SAMP)
      SAMP_PARTITIONED <- matrix(rep(1, length(SAMP) *
                                       (2^ncol_SAMP)), length(SAMP), 2^ncol_SAMP)
    }
    new_logic_index <- 1
    for (gene_id in 1:ncol_SAMP) {
      SAMP_PARTITIONED[, new_logic_index] <- SAMP_PARTITIONED[,
                                                              new_logic_index] * SAMP[, gene_id]
    }
    for (k in 1:ncol_SAMP) {
      D = combn(1:ncol_SAMP, k)
      for (i in 1:ncol(D)) {
        index_of_not <- t(D[, i])
        new_logic_index <- new_logic_index + 1
        for (j in 1:ncol(index_of_not)) {
          SAMP[, index_of_not[j]] <- (1 - SAMP[, index_of_not[j]])
        }
        for (gene_id in 1:ncol_SAMP) {
          SAMP_PARTITIONED[, new_logic_index] <- SAMP_PARTITIONED[,
                                                                  new_logic_index] * SAMP[, gene_id]
        }
        if (is.vector(X)) {
          SAMP <- X
        }
        else {
          SAMP <- X[, c(ORD)]
        }
      }
    }
    return(SAMP_PARTITIONED)
  }

  balancing_data  <- function(data){
    # Last column
    response     <- data[, ncol(data)]

    # Count of 0s and 1s
    count_0      <- sum(response == 0)
    count_1      <- sum(response == 1)

    # Percentage calculation
    percentage_0 <- count_0 / length(response) * 100
    percentage_1 <- count_1 / length(response) * 100

    additional_samples <- abs(count_1-count_0)
    if (count_1 < count_0) {
      # Sampling from the class with fewer records (0s)
      sampled_data <- rbind(
        data[response == 0, ],
        data[response == 1, ],
        data[sample(which(response == 1), additional_samples, replace = TRUE), ]
      )
    } else if (count_0 < count_1) {
      # Sampling from the class with fewer records (1s)
      sampled_data <- rbind(
        data[response == 0, ],
        data[response == 1, ],
        data[sample(which(response == 0), additional_samples, replace = TRUE), ]
      )
    } else{
      sampled_data <- data
    }
    return(sampled_data)
  }
  ################################################################################ Fit single complex
  if( (all(data[  ,ncol(data)] %in% c(0, 1))) & balancing ){
    data         <- balancing_data(data)
  }

  predictive_data<- data[  ,-ncol(data)]
  n              <- ncol(data) -1

  total_iterations     <- 0
  for(i in 1:max_feature){
    total_iterations   <- total_iterations + ncol(combn(n,i))
  }

  if( mode == "2L"){
    for(i in 1:(max_feature-1)){
      for(j in (i+1):max_feature){
        total_iterations<- total_iterations + ncol(combn(n,i))*ncol(combn(n,j))
      } # end for i
    } # end for j

    for(i in 2:max_feature){
      total_iterations  <- total_iterations + ncol(combn(n,i))*(ncol(combn(n,i))-1)/2
    } # end for i

  } # end for mode
  # print(total_iterations)

  # Set up parallel backend
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)

  current_it     <- 0
  LOGIC_VALUES   <- list("1" = list(), "2" = list())
  for (k in 1:max_feature) {
    C             <- combn(1:n, k)  # choose causal gene index
    current_it    <- current_it + ncol(C)

    results  <- foreach::foreach(j = 1:ncol(C), .combine = rbind) %dopar% {
      ORD         <- t(C[, j])
      x           <- ToComputeLogic(predictive_data, ORD)
      y           <- data[, ncol(data)]
      model       <- glmnet::glmnet(x, y, alpha = 0)
      cv_model    <- glmnet::cv.glmnet(x, y, alpha = 0)
      best_lambda <- cv_model$lambda.min
      best_model  <- glmnet::glmnet(x, y, alpha = 0, lambda = best_lambda)
      y_predicted <- predict(model, s = best_lambda, newx = x)
      y_predicted <- sigmoid(y_predicted)
      sst         <- sum((y - mean(y))^2)
      sse         <- sum((y_predicted - y)^2)
      rsq         <- 1 - sse / sst

      p           <- (2^k) + 1
      rsq_adj     <- 1 - ((1 - rsq) * (nrow(data) - 1) / (nrow(data) - p - 1))
      BIC         <- nrow(data) * log(sse / nrow(data)) + p * log(nrow(data))
      #+++++++++++++++++++++++
      list(AGRE_OUT      = c(ORD, coef(best_model)[1:length(coef(best_model))], rsq, rsq_adj, BIC),
           AGRE_OUT_pred = t(y_predicted),
           R2            = rsq_adj)
    }

    progress_percent <- round(100*current_it/total_iterations,2)
    if (verbose) utils::setTxtProgressBar(pb, progress_percent)

    LOGIC_VALUES[[as.character(k)]] <- results
  }
  ################################################################################ Fit double complex
  if(mode == "2L"){
    for(k1 in 1:max_feature){
      for(k2 in k1:max_feature){
        if(is.vector(LOGIC_VALUES[[as.character(k1)]])){
          row_k1<- 1
        }else{
          row_k1<- nrow(LOGIC_VALUES[[as.character(k1)]])
        }
        end_i   <-  row_k1 - (k1 >= k2)
        if( (!((k1==k2)&(k1==1))) & (end_i >0) ){
          ######################################################################
          results_list <- list()

          for(i in 1:end_i){
            if(k1 < k2){
              start_j <- 1
            }else{
              start_j <- i + 1
            }

            if(is.vector(LOGIC_VALUES[[as.character(k2)]])){
              end_j   <- 1
            }else{
              end_j   <- nrow(LOGIC_VALUES[[as.character(k2)]])
            }
            current_it    <- current_it + (end_j-start_j+1)
            results   <- foreach::foreach(j = start_j:end_j, .combine = rbind) %dopar% {
              # library(glmnet)
              y_predicted_1 <- as.vector(LOGIC_VALUES[[as.character(k1)]][[i,2]])

              if(is.vector(LOGIC_VALUES[[as.character(k2)]])){
                y_predicted_2 <- as.vector(LOGIC_VALUES[[as.character(k2)]][[2]])
              }else{
                y_predicted_2 <- as.vector(LOGIC_VALUES[[as.character(k2)]][[j,2]])
              }

              if(var(y_predicted_1)==0){
                y_predicted_1[seq(1, length(y_predicted_1), 2)] <- y_predicted_1[seq(1, length(y_predicted_1), 2)] + 0.00001
              }
              if(var(y_predicted_2)==0){
                y_predicted_2[seq(1, length(y_predicted_2), 2)] <- y_predicted_2[seq(1, length(y_predicted_2), 2)] + 0.00001
              }

              x           <- cbind(y_predicted_1, y_predicted_2)
              x           <- ToComputeLogic(x,c(1,2))
              y           <- data[ ,ncol(data)]
              model       <- glmnet::glmnet(x, y, alpha = 0)
              cv_model    <- glmnet::cv.glmnet(x, y, alpha = 0)
              best_lambda <- cv_model$lambda.min
              best_model  <- glmnet::glmnet(x, y, alpha = 0, lambda = best_lambda)
              y_predicted <- predict(model, s = best_lambda, newx = x)
              y_predicted <- sigmoid(y_predicted)
              sst         <- sum((y - mean(y))^2)
              sse         <- sum((y_predicted - y)^2)
              rsq         <- 1 - sse / sst
              p           <- (2^k1) + (2^k2) + (2^2) + 3 # 2^2 for the combination of SL1 and SL2
              rsq_adj     <- 1 - ( (1-rsq)*(nrow(data)-1)/(nrow(data)-p-1) )
              BIC         <- nrow(data) * log(sse / nrow(data)) + p * log(nrow(data))

              Reg1        <- LOGIC_VALUES[[as.character(k1)]][[i,1]][1:k1]

              if(is.vector(LOGIC_VALUES[[as.character(k2)]])){
                Reg2      <- LOGIC_VALUES[[as.character(k2)]][[1]][1:k2]
              }else{
                Reg2      <- LOGIC_VALUES[[as.character(k2)]][[j,1]][1:k2]
              }
              #+++++++++++++++++++++++
              list(AGRE_OUT      = c(i, j, Reg1, Reg2, coef(best_model)[1:length(coef(best_model))], rsq, rsq_adj, BIC),
                   AGRE_OUT_pred = c(),
                   R2            = rsq_adj)
            } # end for j
            results_list[[i]] <- results
          } # end for i
          progress_percent <- round(100*current_it/total_iterations,2)
          if (verbose) utils::setTxtProgressBar(pb, progress_percent)

          LOGIC_VALUES[[paste0(as.character(k1),".", as.character(k2))]] <- do.call(rbind, results_list)
          ######################################################################
        } # if condition ok for combination
      } # end for k2
    } # end for k1
  } # complex or not
  ################################################################################
  W2SYMBOL_ORIGINAL <- function(logic_significance, predicted_impact_set){
    k                  <- length(predicted_impact_set)
    reg_char           <- predicted_impact_set
    x                  <- list()
    logical_sets       <- array(x,c(2^k,2))

    logical_sets_index <- 1
    logical_sets[[1,1]]<- 0
    logical_sets[[1,2]]<- paste0( "AND(", paste(reg_char, collapse = ','), ")" )

    for(i in 1:k){
      not_comb <- combn(1:k,i)  # not of elements are choosen
      for(j in 1:ncol(not_comb)){
        reg_char_tmp                        <- reg_char
        logical_sets_index                  <- logical_sets_index + 1
        logical_sets[[logical_sets_index,1]]<- not_comb[ ,j]

        for(l in 1:(length(not_comb[ ,j]))){
          reg_char_tmp[not_comb[ ,j][l]]    <- paste0("\u00AC", reg_char[not_comb[ ,j][l]])
        }

        logical_sets[[logical_sets_index,2]]<- paste0("AND(", paste0(reg_char_tmp, collapse = ','), ")")
      }
    }

    LOGIC_VECTOR       <- c()
    for(i in 1:(2^k)){
      if(logic_significance[i]==1){
        LOGIC_VECTOR   <- c(LOGIC_VECTOR, logical_sets[[i,2]])
      }
    }
    LOGIC_VECTOR       <- paste0("[OR(", paste0(LOGIC_VECTOR, collapse = ','), ")]" )
    return(LOGIC_VECTOR)
  }

  W2SYMBOL <- function(logic_significance, predicted_impact_set, R){
    if((R==1)||(R>2)){
      predicted_impact_set <- colnames(data)[c(predicted_impact_set)]
    }
    if(length(predicted_impact_set)>2){
      LOGIC_VECTOR   <- W2SYMBOL_ORIGINAL(logic_significance, predicted_impact_set)
    }else if(length(predicted_impact_set)==2){
      if(all(logic_significance==c(0,0,0,0))){
        LOGIC_VECTOR <- 0
      }else if(all(logic_significance==c(0,0,0,1))){
        LOGIC_VECTOR <- paste0( "[NOR(" , predicted_impact_set[1], "," , predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(0,1,0,0))){
        LOGIC_VECTOR <- paste0( "[AND(", "\u00AC", predicted_impact_set[1], "," , predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(0,1,0,1))){
        LOGIC_VECTOR <- paste0( "[R3("  , predicted_impact_set[1], "," , predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(0,0,1,0))){
        LOGIC_VECTOR <- paste0( "[AND(" , predicted_impact_set[1], ",", "\u00AC", predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(0,0,1,1))){
        LOGIC_VECTOR <- paste0( "[R5("  , predicted_impact_set[1], "," , predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(0,1,1,0))){
        LOGIC_VECTOR <- paste0( "[XOR(" , predicted_impact_set[1], "," , predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(0,1,1,1))){
        LOGIC_VECTOR <- paste0("[NAND(" , predicted_impact_set[1], "," , predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(1,0,0,0))){
        LOGIC_VECTOR <- paste0("[AND("  , predicted_impact_set[1], "," , predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(1,0,0,1))){
        LOGIC_VECTOR <- paste0("[XNOR(" , predicted_impact_set[1], "," , predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(1,1,0,0))){
        LOGIC_VECTOR <- paste0("[R10("  , predicted_impact_set[1], "," , predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(1,1,0,1))){
        LOGIC_VECTOR <- paste0("[OR(", "\u00AC" , predicted_impact_set[1], "," , predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(1,0,1,0))){
        LOGIC_VECTOR <- paste0("[R12("  , predicted_impact_set[1], "," , predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(1,0,1,1))){
        LOGIC_VECTOR <- paste0("[OR("   , predicted_impact_set[1], ",", "\u00AC", predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(1,1,1,0))){
        LOGIC_VECTOR <- paste0("[OR("   , predicted_impact_set[1], "," , predicted_impact_set[2], ")]")
      }else if(all(logic_significance==c(1,1,1,1))){
        LOGIC_VECTOR <- 1
      }
    }else if(length(predicted_impact_set)==1){
      if(all(logic_significance==c(0,0))){
        LOGIC_VECTOR <- 0
      }else if(all(logic_significance==c(1,0))){
        LOGIC_VECTOR <- paste0("[" , predicted_impact_set, "]")
      }else if(all(logic_significance==c(0,1))){
        LOGIC_VECTOR <- paste0("[", "\u00AC", predicted_impact_set, "]")
      }else if(all(logic_significance==c(1,1))){
        LOGIC_VECTOR <- 1
      }
    }
    return(LOGIC_VECTOR)
  }

  #  sort gates
  gate_info <- c()
  for(k in 1:max_feature){
    if(is.vector(LOGIC_VALUES[[as.character(k)]])){
      in_nodes      <- LOGIC_VALUES[[as.character(k)]][[1]][1:k]
      coef          <- LOGIC_VALUES[[as.character(k)]][[1]][(k+2):(k+2+2^k-1)]
      logic_significance <- as.integer(coef > weight_threshold)
      LOGIC_VECTOR  <- W2SYMBOL(logic_significance, in_nodes, R=1)
      bic_val       <- LOGIC_VALUES[[as.character(k)]][[1]][length(LOGIC_VALUES[[as.character(k)]][[1]])]
      r2_adj        <- LOGIC_VALUES[[as.character(k)]][[1]][length(LOGIC_VALUES[[as.character(k)]][[1]])-1]

      Rule_num      <- sum(logic_significance)
      Rule_coef     <- c(paste(round(coef,2), collapse = ":"), " ", " ")
      gate_info     <- rbind(gate_info, c(LOGIC_VECTOR, r2_adj, bic_val, k, i, paste(colnames(data)[in_nodes], collapse = "."), Rule_num, Rule_coef) )
    }else{
      results <- foreach::foreach(i = 1:nrow(LOGIC_VALUES[[as.character(k)]]), .combine = rbind) %dopar% {
        in_nodes    <- LOGIC_VALUES[[as.character(k)]][[i,1]][1:k]
        coef        <- LOGIC_VALUES[[as.character(k)]][[i,1]][(k+2):(k+2+2^k-1)]
        logic_significance <- as.integer(coef > weight_threshold)
        LOGIC_VECTOR<- W2SYMBOL(logic_significance, in_nodes, R=1)
        bic_val     <- LOGIC_VALUES[[as.character(k)]][[i,1]][length(LOGIC_VALUES[[as.character(k)]][[i,1]])]
        r2_adj      <- LOGIC_VALUES[[as.character(k)]][[i,1]][length(LOGIC_VALUES[[as.character(k)]][[i,1]])-1]

        Rule_num    <- sum(logic_significance)
        Rule_coef   <- c(paste(round(coef,2), collapse = ":"), " ", " ")
        # gate_info   <- rbind(gate_info, c(LOGIC_VECTOR, r2_adj, bic_val, k, i, paste(colnames(data)[in_nodes], collapse = "."), Rule_num, Rule_coef) )
        #+++++++++++++++++++++++
        c(LOGIC_VECTOR, r2_adj, bic_val, k, i, paste(colnames(data)[in_nodes], collapse = "."), Rule_num, Rule_coef)
      }
      gate_info     <- rbind(gate_info, results)
    } # end for if else
  } # end for k
  ## joint
  if(mode == "2L"){
    for(k1 in 1:max_feature){
      for(k2 in k1:max_feature){
        k <- paste0(as.character(k1),".", as.character(k2))
        if( (!((k1==k2)&(k1==1))) & (!is.null(LOGIC_VALUES[[as.character(k)]])) ){
          ##########################################################################
          if( is.vector(LOGIC_VALUES[[as.character(k)]]) ){
            output        <- "this is not possible"
          }else{
            results <- foreach::foreach(i = 1:nrow(LOGIC_VALUES[[as.character(k)]]), .combine = rbind) %dopar% {
              i_index1    <- LOGIC_VALUES[[as.character(k)]][[i,1]][1]
              i_index2    <- LOGIC_VALUES[[as.character(k)]][[i,1]][2]
              ################################################################# inner node info
              nodes_index1<- LOGIC_VALUES[[as.character(k1)]][[i_index1,1]][1:k1]
              coef_index1 <- LOGIC_VALUES[[as.character(k1)]][[i_index1,1]][(k1+2):(k1+2+2^k1-1)]

              if(is.vector(LOGIC_VALUES[[as.character(k2)]])){
                nodes_index2<- LOGIC_VALUES[[as.character(k2)]][[1]][1:k2]
                coef_index2 <- LOGIC_VALUES[[as.character(k2)]][[1]][(k2+2):(k2+2+2^k2-1)]
              }else{
                nodes_index2<- LOGIC_VALUES[[as.character(k2)]][[i_index2,1]][1:k2]
                coef_index2 <- LOGIC_VALUES[[as.character(k2)]][[i_index2,1]][(k2+2):(k2+2+2^k2-1)]
              }

              logic_significance_index1 <- as.integer(coef_index1 > weight_threshold)
              logic_significance_index2 <- as.integer(coef_index2 > weight_threshold)

              LOGIC_VECTOR_index1 <- W2SYMBOL(logic_significance_index1, nodes_index1, R=1)
              LOGIC_VECTOR_index2 <- W2SYMBOL(logic_significance_index2, nodes_index2, R=1)
              #################################################################
              sub_logics  <- 2
              sub_elements<- k1 + k2

              coef        <- LOGIC_VALUES[[as.character(k)]][[i,1]][(sub_logics + sub_elements + 2):(sub_logics + sub_elements + 2 + 2^(sub_logics)-1)]
              logic_significance <- as.integer(coef > weight_threshold)

              in_nodes    <- c(LOGIC_VECTOR_index1, LOGIC_VECTOR_index2)
              LOGIC_VECTOR<- W2SYMBOL(logic_significance, in_nodes, R=2)
              bic_val     <- LOGIC_VALUES[[as.character(k)]][[i,1]][length(LOGIC_VALUES[[as.character(k)]][[i,1]])]
              r2_adj      <- LOGIC_VALUES[[as.character(k)]][[i,1]][length(LOGIC_VALUES[[as.character(k)]][[i,1]]) -1]

              in_features <- paste(unique(c(colnames(data)[nodes_index1], colnames(data)[nodes_index2])), collapse = ".")

              Rule_num    <- sum(logic_significance_index1) + sum(logic_significance_index2) + sum(logic_significance)
              Rule_coef   <- c( paste(round(coef_index1,2), collapse = ":"), paste(round(coef_index2,2), collapse = ":"), paste(round(coef,2), collapse = ":") )
              c(LOGIC_VECTOR, r2_adj, bic_val, k, i, in_features, Rule_num, Rule_coef)
            }
            gate_info     <- rbind(gate_info, results)
          } # end for if vector
        } # end for if k1
        ########################################################################
      } # end for k2
    } # end for k1
  }
  ##############################################################################
  trained_model      <- list()
  trained_model$LOGIC_VALUES   <- LOGIC_VALUES

  gate_info          <- as.data.frame(gate_info)
  colnames(gate_info)<- c("Boolean_Rule", "R2", "BIC", "Input_Size", "Index",
                          "Features", "Active_Conjunctions",
                          "Weights Layer1, Sub-Rule1",
                          "Weights Layer1, Sub-Rule2", "Weights Layer2")
  if( mode == "1L"){
    gate_info        <- gate_info[ ,1:(ncol(gate_info)-2)]
  }

  gate_info$BIC      <- round(as.numeric(gate_info$BIC), 2)
  gate_info$R2       <- round(as.numeric(gate_info$R2), 2)
  gate_info$Input_Size<- as.numeric(gate_info$Input_Size)
  gate_info$Index    <- as.numeric(gate_info$Index)
  gate_info          <- gate_info[order(gate_info$BIC), ]
  rownames(gate_info) <- NULL

  trained_model$gate_info <- gate_info

  if( mode == "1L"){
    gate_info <- gate_info[, c("Boolean_Rule", "R2", "BIC",
                                       "Weights Layer1, Sub-Rule1")]
  }else{
    gate_info <- gate_info[, c("Boolean_Rule", "R2", "BIC",
                                       "Weights Layer1, Sub-Rule1",
                                       "Weights Layer1, Sub-Rule2",
                                       "Weights Layer2")]
  }
  trained_model$boolean_rules <- gate_info
  ##############################################################################
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  if (verbose) close(pb)
  return(trained_model)
}


