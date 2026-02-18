# RBBR: Regression-Based Boolean Rule inference in artificial intelligence studies.
- This repository contains code and tutorials for executing RBBR.
- The RBBR package supports parallel execution on multi-CPU platforms, enhancing accessibility for real-world Boolean rule inference applications.

<br>

Malekpour, S.A., Pezeshk, H., Explainable artificial intelligence with Boolean rule-aware predictions in ridge regression models, Neurocomputing, 132991 (2026).

<br>

## RBBR installation from CRAN
   
```R
install.packages("RBBR")
```
<br>

## Prepare input files
### rbbr_scaling()
To preprocess raw data, including steps such as rescaling to bring each input feature within the [0,1] range, you can use the rbbr_scaling() function from the RBBR package.

```R
rbbr_scaling(data)

# Required input arguments
# data              Each row is a sample and each column a feature. The target variable is expected in the last column.

############################# Example usage #############################
# Load dataset
data(example_data)

# Inspect loaded data
head(MAGIC_data)

# Scale features
data_scaled <- rbbr_scaling(MAGIC_data)
head(data_scaled)
```
<br>

## Train RBBR 
### rbbr_train()
For training the RBBR model on a dataset to extract Boolean rules, you can use the `rbbr_train()` function.

```R
# For training the RBBR model
rbbr_train(
  data,
  max_feature = 3,
  mode = "1L",
  slope = 10,
  weight_threshold = 0,
  balancing = TRUE,
  num_cores = NA,
  verbose = FALSE
)

# Required input arguments
# data	           The dataset with scaled features within the [0,1] interval.
#                   Each row represents a sample and each column represents a feature. The target variable must be in the last column.
#
# Optional input arguments  
# max_feature       The maximum number of input features allowed in a Boolean rule.
#                   The default value is 3.
# mode	           Choose between "1L" for fitting 1-layered models or "2L" for fitting 2-layered models.
#                   The default value is "1L".
# slope	           The slope parameter used in the Sigmoid activation function.
#                   The default value is 10.
# weight_threshold  Conjunctions with weights above this threshold in the fitted ridge regression models will be printed as active conjunctions in the output.
#                   The default value is 0.
# balancing	        Logical. This is for adjusting the distribution of target classes or categories within a dataset to ensure that each class is adequately represented.
#                   The default value is TRUE. Set it to FALSE, if you don't need to perform the data balancing.
# num_cores	        Number of parallel workers to use for computation.
#                   Adjust according to your system. Default is NA (automatic selection).
# verbose	        Logical. If TRUE, progress messages and a progress bar are shown.
#                   Default is FALSE.

############################# Example usage #############################
library(RBBR)

# Load dataset
data(example_data)

# Example for training a two-layer model
head(OR_data) # Y = OR([AND(C,D)],[NOR(A,B)]) dataset

          C          D         A         B Y
1 0.9503997 0.81028102 0.6249662 0.1931601 1
2 0.1481077 0.50052685 0.1527322 0.8670829 0
3 0.8151770 0.65770170 0.3031337 0.6836050 1
4 0.8049381 0.07865007 0.6084361 0.5317733 0
5 0.8960193 0.27124144 0.5090235 0.4332590 0
6 0.1509037 0.79736172 0.1772568 0.5100828 0

data_train   <- OR_data[1:800, ]
data_test    <- OR_data[801:1000, ]

# training model
trained_model <- rbbr_train(data_train,
                           max_feature = 2,
                           mode = "2L",
                           balancing = FALSE,
                           num_cores = 1, verbose = TRUE)

training process started with 1 computing cores
  |====================| 100%

head(trained_model$boolean_rules)
                  Boolean_Rule   R2      BIC Weights Layer1, Sub-Rule1 Weights Layer1, Sub-Rule2         Weights Layer2
1  [OR([AND(C,D)],[NOR(A,B)])] 0.71 -2039.64     0.92:-0.37:-0.35:-0.2     -0.2:-0.37:-0.36:0.91   0.48:0.41:0.37:-0.73
2 [AND([OR(C,¬B)],[OR(D,¬A)])] 0.44 -1504.10      0.19:-0.68:0.34:0.18      0.09:-0.67:0.52:0.08 0.97:-0.15:-0.13:-0.43
3 [AND([OR(C,¬A)],[OR(D,¬B)])] 0.43 -1480.53      0.08:-0.66:0.49:0.12      0.12:-0.62:0.46:0.05   0.9:-0.1:-0.08:-0.46
4         [OR([C],[NOR(A,B)])] 0.41 -1468.38                0.29:-0.27     -0.2:-0.37:-0.36:0.91   0.45:0.47:0.18:-0.63
5         [OR([D],[NOR(A,B)])] 0.41 -1467.83                0.31:-0.29     -0.2:-0.37:-0.36:0.91    0.5:0.43:0.15:-0.64
6        [OR([¬A],[AND(C,D)])] 0.40 -1458.98                 -0.3:0.29     0.92:-0.37:-0.35:-0.2   0.54:0.37:0.15:-0.64

# testing model
data_test_x  <- data_test[ ,1:(ncol(data_test)-1)]
labels       <- data_test[ ,ncol(data_test)]

predicted_label_probabilities <- rbbr_predictor(trained_model,
                                   data_test_x,
                                   num_top_rules = 10,
                                   num_cores = 1, verbose = TRUE)

head(predicted_label_probabilities)
[1] 0.005587339 0.110986479 0.826692566 0.036342787 0.731197972 0.011742257
```

<br>

## Making predictions with RBBR on new dataset
### rbbr_predictor()
For utilizing the trained model to predict target values or labels on a new dataset, you can use the `rbbr_predictor()` function. In datasets with binary (0/1) target features, the rbbr_predictor() function produces predicted probabilities for target labels. However, when dealing with a continuous target variable, the rbbr_predictor() output can be regarded as the predicted target value.

```R
# For making predictions
rbbr_predictor(
  trained_model,
  data_test,
  num_top_rules = 1,
  slope = 10,
  num_cores = 1,
  verbose = FALSE
)

# Required input arguments
# trained_model	  Model returned by 'rbbr_train()'
# data_test	        The new dataset for which we want to predict the target class or label probability. Each sample is represented as a row, and features are in columns.
#
# Optional input arguments  
# num_top_rules     Number of Boolean rules with the best Bayesian Information Criterion (BIC) scores to be used for prediction.
#                   The default value is 1.
# slope	           The slope parameter for the sigmoid activation function.
#                   Default is 10.
# num_cores	        Number of parallel workers to use for computation. Adjust according to your system.
#                   Default is NA (automatic selection).
# verbose	        Logical. If TRUE, progress messages are shown. Default is FALSE.


############################# Example usage #############################
library(RBBR)

# Load dataset
data(example_data)

# Inspect loaded data
head(XOR_data) # Y = XOR(feat_0, feat_1) dataset
       feat_0     feat_1     feat_2     feat_3      feat_4    feat_5    feat_6      feat_7    feat_8     feat_9 xor
1 0.625853352 0.48361474 0.10529046 0.57628866 0.878811699 0.7895082 0.7824021 0.019096636 0.9185168 0.34709053   1
2 0.903319041 0.02643791 0.37173989 0.05941228 0.204367513 0.1348234 0.3502440 0.928994618 0.3147315 0.95440266   1
3 0.880183794 0.64759100 0.09914387 0.26202347 0.000217744 0.4028034 0.1407160 0.767496957 0.5380130 0.22020956   0
4 0.000478779 0.09698873 0.79021914 0.21921378 0.964783778 0.8605876 0.7625196 0.869578955 0.9855441 0.03724109   0
5 0.442783273 0.22467666 0.61272149 0.11801844 0.380510666 0.8145049 0.3535930 0.007680483 0.3920220 0.62243004   0
6 0.088976773 0.68076354 0.92153500 0.04653772 0.178060560 0.1599707 0.2744070 0.414334818 0.7652831 0.26153153   1

data_train <- XOR_data[1:800, ]
data_test <- XOR_data[801:1000, ]

# training model
trained_model <- rbbr_train(data_train,
                            max_feature = 2,
                            num_cores = 1, verbose = TRUE)
training process started with 1 computing cores
  |====================| 100%


head(trained_model$boolean_rules)
          Boolean_Rule   R2      BIC Weights Layer1, Sub-Rule1
1 [XOR(feat_0,feat_1)] 0.77 -2363.32      -1.1:0.99:1.08:-1.06
2            [¬feat_5] 0.00 -1149.03                -0.02:0.02
3             [feat_6] 0.00 -1148.64                0.01:-0.01
4             [feat_0] 0.00 -1147.63                       0:0
5            [¬feat_7] 0.00 -1147.37                       0:0
6            [¬feat_1] 0.00 -1147.05                       0:0

# testing model
data_test_x <- data_test[ ,1:(ncol(data_test)-1)]
labels <- data_test[ ,ncol(data_test)]

predicted_label_probabilities <- rbbr_predictor(trained_model,
                                   data_test_x,
                                   num_top_rules = 1,
                                   num_cores = 1, verbose = TRUE)

head(predicted_label_probabilities)
[1] 0.0327335844 0.9884560168 0.0003526281 0.3229442409 0.0789854545 0.0043184231

head(labels) # true labels
[1] 0 1 0 0 0 0
```

<br>

### MAGIC dataset example

```R
library(RBBR)

# Load dataset
data(example_data)

# Inspect loaded data
head(MAGIC_data)
   fLength   fWidth  fSize  fConc fConc1    fAsym  fM3Long fM3Trans  fAlpha    fDist class
1  28.7967  16.0021 2.6449 0.3918 0.1982  27.7004  22.0110  -8.2027 40.0920  81.8828     1
2  31.6036  11.7235 2.5185 0.5303 0.3773  26.2722  23.8238  -9.9574  6.3609 205.2610     1
3 162.0520 136.0310 4.0612 0.0374 0.0187 116.7410 -64.8580 -45.2160 76.9600 256.7880     1
4  23.8172   9.5728 2.3385 0.6147 0.3922  27.2107  -6.4633  -7.1513 10.4490 116.7370     1
5  75.1362  30.9205 3.1611 0.3168 0.1832  -5.5277  28.5525  21.8393  4.6480 356.4620     1
6  51.6240  21.1502 2.9085 0.2420 0.1340  50.8761  43.1887   9.8145  3.6130 238.0980     1

# Scaling to bring each input feature within the (0,1) range
data_scaled <- rbbr_scaling(MAGIC_data)
head(data_scaled)
    fLength    fWidth     fSize      fConc     fConc1     fAsym   fM3Long  fM3Trans     fAlpha     fDist class
1 0.1427532 0.2050390 0.3490045 0.49884575 0.42928416 0.8943205 0.8223308 0.7967131 0.46874459 0.2292175     1
2 0.1590993 0.1502162 0.2863067 0.68128604 0.81778742 0.8916903 0.8265443 0.7896415 0.07436989 0.5800906     1
3 0.9187688 0.9999000 0.9999000 0.03200938 0.03991323 0.9999000 0.6204176 0.6475468 0.89979506 0.7266274     1
4 0.1137550 0.1226587 0.1970219 0.79246265 0.85010846 0.8934186 0.7561468 0.8009503 0.12216682 0.3283388     1
5 0.4126125 0.3961922 0.6050535 0.40005137 0.39674620 0.8331270 0.8375355 0.9177845 0.05434313 0.9999000     1
6 0.2756886 0.2710029 0.4797571 0.30152045 0.29002169 0.9370013 0.8715550 0.8693237 0.04224220 0.6734752     1

# Randomly select indices for the training dataset
train_indices <- sample(nrow(data_scaled), floor(0.8 * nrow(data_scaled)))

# create train and test sets
data_train <- data_scaled[train_indices, ]
data_test  <- data_scaled[-train_indices, ]

# training model
trained_model <- rbbr_train(data_train,
                            max_feature = 6,
                            num_cores = 8, verbose = TRUE)

training process started with  8  computing cores
  |====================| 100%

# use input features from test data for making predictions
data_test_x  <- data_test[ ,1:(ncol(data_test)-1)]
labels <- data_test[ ,ncol(data_test)]

predicted_label_probabilities <- rbbr_predictor(trained_model,
                                   data_test_x,
                                   num_top_rules = 1,
                                   slope = 10,
                                   num_cores = 8, verbose = TRUE)

head(predicted_label_probabilities) # the output from rbbr_predictor() as shown above is the predicted probabilities for target labels (0/1). 
[1] 0.97106361 0.67250382 0.03986523 0.98782233 0.82680708 0.82235614

head(labels) # true labels
[1] 1 1 1 1 1 1
```


<br>
