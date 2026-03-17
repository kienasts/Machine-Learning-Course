# =============================================================================
# DATA CLEANING & FEATURE ENGINEERING PIPELINE
# Wage Prediction Assignment — HR Analytics
# =============================================================================
# Order: Load → Inspect → Split → Impute → Outliers → Encode → Scale → Construct
# All data-dependent statistics are fit on TRAIN only, then applied to both sets.
# =============================================================================

library(tidyverse)
library(caret)
library(glmnet)
library(randomForest)
library(rpart)

# =============================================================================
# STEP 0: LOAD DATA
# =============================================================================

train_raw <- read.csv("wage_training.csv", stringsAsFactors = FALSE)
test_raw  <- read.csv("new_hires.csv",     stringsAsFactors = FALSE)

# =============================================================================
# STEP 1: EXPLORATORY CHECKS
# =============================================================================

# str(train_raw)          # inspect variable types
# colMeans(is.na(...))    # check missingness proportions
# summary(train_raw)      # review distributions

miss <- colMeans(is.na(train_raw))   # proportion missing per column; inspect before proceeding

# =============================================================================
# STEP 2: VARIABLE CLASSIFICATION & ENCODING VERIFICATION
# =============================================================================
# Based on the data dictionary, we classify each variable and check its
# current R class against the intended type.
# =============================================================================

# --- Expected types from the data dictionary ---
intended_types <- list(
  # Continuous / numeric
  numeric = c("yos", "exp", "exp2", "age", "famsize", "nchlt5", "hoursworked", "lwwage"),

  # Binary indicators (should be integer 0/1, NOT factor)
  binary  = c("sex", "hispanic", "dchlt5", "dchlt19", "msa", "metro", "vetstat"),

  # Nominal categorical — no inherent order; encoded as unordered factor
  factor  = c("marst", "race", "schltype", "degfield",
              "ind1990c", "occ2010c", "region"),

  # Ordinal categorical — meaningful rank order; encoded as ordered factor
  # model.matrix will use polynomial contrasts (L/Q/C) instead of dummies
  ordered = c("speakeng", "education", "hw"),

  # ID — keep as-is, exclude from modelling
  id      = c("id")
)

check_and_fix_types <- function(df, intended, label) {
  df <- df

  for (v in intended$numeric) {
    if (!v %in% names(df)) next
    actual <- class(df[[v]])
    if (!actual %in% c("numeric", "integer")) {
      # coerce to numeric if stored as character or factor
      df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
    }
  }

  for (v in intended$binary) {
    if (!v %in% names(df)) next
    actual <- class(df[[v]])
    # binary indicators must be integer 0/1, not factor or character
    if (actual == "character") {
      df[[v]] <- suppressWarnings(as.integer(df[[v]]))
    } else if (actual == "factor") {
      df[[v]] <- as.integer(as.character(df[[v]]))
    }
  }

  for (v in intended$factor) {
    if (!v %in% names(df)) next
    actual <- class(df[[v]])
    if (!"factor" %in% actual) {
      # coerce to unordered factor so model.matrix handles dummies correctly
      df[[v]] <- as.factor(df[[v]])
    }
  }

  for (v in intended$ordered) {
    if (!v %in% names(df)) next
    actual <- class(df[[v]])
    if (!"ordered" %in% actual) {
      # coerce to unordered factor here; correct level order is applied in Step 6
      df[[v]] <- as.factor(df[[v]])
    }
  }

  return(df)
}

train_raw <- check_and_fix_types(train_raw, intended_types, "TRAIN")
test_raw  <- check_and_fix_types(test_raw,  intended_types, "TEST")

# =============================================================================
# STEP 3: TRAIN / VALIDATION SPLIT (80 / 20, stratified on lwwage quintile)
# =============================================================================

set.seed(42)

# Stratify by quintile of the outcome to preserve distribution
train_raw$quintile_strat <- cut(train_raw$lwwage,
                                 breaks = quantile(train_raw$lwwage, probs = 0:5/5),
                                 include.lowest = TRUE, labels = FALSE)

train_idx <- createDataPartition(train_raw$quintile_strat, p = 0.8, list = FALSE)
train_raw$quintile_strat <- NULL   # drop helper column

train <- train_raw[ train_idx, ]
val   <- train_raw[-train_idx, ]   # internal validation set

# Separate outcome variable
y_train <- train$lwwage
y_val   <- val$lwwage

# =============================================================================
# STEP 4: HANDLE MISSING DATA
# =============================================================================
# Fit imputation statistics on train ONLY; apply to train, val, and test.
# =============================================================================

cont_vars <- c("yos", "exp", "exp2", "age", "famsize", "nchlt5", "hoursworked")
bin_vars  <- c("sex", "hispanic", "dchlt5", "dchlt19", "msa", "metro", "vetstat")
cat_vars_nom <- c("marst", "race", "schltype", "degfield",
                  "ind1990c", "occ2010c", "region")
cat_vars_ord <- c("speakeng", "education", "hw")
# Note: educd is highly correlated with education — exclude to avoid redundancy

# Explicit level orderings for ordinal variables.
# Verify these strings match the actual values in your data before running.
ord_levels <- list(
  speakeng  = c("Does not speak English", "Yes, but not well",
                "Yes, speaks well", "Yes, speaks very well", "Only speaks English"),
  education = c("Less than high school", "High school diploma",
                "Some college", "Bachelor's degree", "Graduate degree"),
  hw        = c("1-14 hours", "15-29 hours", "30-34 hours",
                "35-39 hours", "40 hours", "41-48 hours", "49-59 hours", "60+ hours")
)

cat_vars <- c(cat_vars_nom, cat_vars_ord)   # combined list used for imputation

impute_datasets <- function(train, val, test_raw) {

  # --- Continuous: median imputation ---
  medians <- sapply(train[cont_vars], median, na.rm = TRUE)

  for (v in cont_vars) {
    train[[v]]    <- ifelse(is.na(train[[v]]),    medians[v], train[[v]])
    val[[v]]      <- ifelse(is.na(val[[v]]),      medians[v], val[[v]])
    test_raw[[v]] <- ifelse(is.na(test_raw[[v]]), medians[v], test_raw[[v]])
  }

  # --- Binary: mode imputation ---
  bin_modes <- sapply(train[bin_vars], function(x) {
    tbl <- table(x); as.integer(names(tbl)[which.max(tbl)])
  })

  for (v in bin_vars) {
    train[[v]]    <- ifelse(is.na(train[[v]]),    bin_modes[v], train[[v]])
    val[[v]]      <- ifelse(is.na(val[[v]]),      bin_modes[v], val[[v]])
    test_raw[[v]] <- ifelse(is.na(test_raw[[v]]), bin_modes[v], test_raw[[v]])
  }

  # --- Categorical: mode imputation ---
  cat_modes <- sapply(train[cat_vars], function(x) {
    tbl <- table(x); names(tbl)[which.max(tbl)]
  })

  for (v in cat_vars) {
    train[[v]]    <- ifelse(is.na(train[[v]]),    cat_modes[v], train[[v]])
    val[[v]]      <- ifelse(is.na(val[[v]]),      cat_modes[v], val[[v]])
    test_raw[[v]] <- ifelse(is.na(test_raw[[v]]), cat_modes[v], test_raw[[v]])
  }

  list(train = train, val = val, test = test_raw,
       medians = medians, bin_modes = bin_modes, cat_modes = cat_modes)
}

imputed <- impute_datasets(train, val, test_raw)
train    <- imputed$train
val      <- imputed$val
test_raw <- imputed$test

# na_check: sapply over imputed columns — should return all zeros if imputation succeeded
na_check <- sapply(train[c(cont_vars, bin_vars, cat_vars)], function(x) sum(is.na(x)))

# =============================================================================
# STEP 5: HANDLE OUTLIERS — Winsorization at 1st / 99th percentile
# =============================================================================

winsorize <- function(x, lo, hi) pmax(pmin(x, hi), lo)

caps <- lapply(train[cont_vars], function(x) {
  list(lo = quantile(x, 0.01, na.rm = TRUE),
       hi = quantile(x, 0.99, na.rm = TRUE))
})

for (v in cont_vars) {
  train[[v]]    <- winsorize(train[[v]],    caps[[v]]$lo, caps[[v]]$hi)
  val[[v]]      <- winsorize(val[[v]],      caps[[v]]$lo, caps[[v]]$hi)
  test_raw[[v]] <- winsorize(test_raw[[v]], caps[[v]]$lo, caps[[v]]$hi)
}

# =============================================================================
# STEP 6: ENCODE CATEGORICAL VARIABLES
# =============================================================================

# --- Rare category pooling: nominal variables only ---
# Ordinal variables are excluded — their levels are fixed by the rank ordering
# and collapsing them would destroy the distance information.
rare_levels <- lapply(train[cat_vars_nom], function(x) {
  freq <- prop.table(table(x))
  names(freq[freq < 0.02])
})

for (v in cat_vars_nom) {
  train[[v]]    <- ifelse(train[[v]]    %in% rare_levels[[v]], "Other", train[[v]])
  val[[v]]      <- ifelse(val[[v]]      %in% rare_levels[[v]], "Other", val[[v]])
  test_raw[[v]] <- ifelse(test_raw[[v]] %in% rare_levels[[v]], "Other", test_raw[[v]])
}

# --- Nominal: unordered factor; levels derived from training data ---
for (v in cat_vars_nom) {
  train[[v]]    <- factor(train[[v]])
  val[[v]]      <- factor(val[[v]],      levels = levels(train[[v]]))
  test_raw[[v]] <- factor(test_raw[[v]], levels = levels(train[[v]]))

  # Any unseen level in val/test becomes NA → impute with training mode
  val[[v]]      <- fct_explicit_na(val[[v]],      na_level = imputed$cat_modes[[v]])
  test_raw[[v]] <- fct_explicit_na(test_raw[[v]], na_level = imputed$cat_modes[[v]])
}

# --- Ordinal: ordered factor with explicit level sequence, then integer-encoded ---
# Integer encoding (rank 1, 2, ..., K) is preferred over model.matrix on ordered
# factors, which produces polynomial contrasts (L/Q/C) that are harder to interpret
# and behave poorly when the spacing between levels is unequal.
for (v in cat_vars_ord) {
  train[[v]]    <- ordered(train[[v]],    levels = ord_levels[[v]])
  val[[v]]      <- ordered(val[[v]],      levels = ord_levels[[v]])
  test_raw[[v]] <- ordered(test_raw[[v]], levels = ord_levels[[v]])

  # Values not in ord_levels become NA → replace with training mode rank
  mode_rank <- as.integer(ordered(imputed$cat_modes[[v]], levels = ord_levels[[v]]))
  train[[v]]    <- ifelse(is.na(as.integer(train[[v]])),    mode_rank, as.integer(train[[v]]))
  val[[v]]      <- ifelse(is.na(as.integer(val[[v]])),      mode_rank, as.integer(val[[v]]))
  test_raw[[v]] <- ifelse(is.na(as.integer(test_raw[[v]])), mode_rank, as.integer(test_raw[[v]]))
}

# =============================================================================
# STEP 7: SCALE CONTINUOUS VARIABLES
# =============================================================================
# z-score standardisation — fit on train only
# =============================================================================

scale_params <- lapply(train[cont_vars], function(x) {
  list(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
})

for (v in cont_vars) {
  m <- scale_params[[v]]$mean
  s <- scale_params[[v]]$sd
  if (s == 0) s <- 1   # guard against zero-variance columns
  train[[v]]    <- (train[[v]]    - m) / s
  val[[v]]      <- (val[[v]]      - m) / s
  test_raw[[v]] <- (test_raw[[v]] - m) / s
}

# =============================================================================
# STEP 8: FEATURE CONSTRUCTION
# =============================================================================

# --- (a) exp2 is already in the data; log-transform hoursworked for skew ---
for (df_name in c("train", "val", "test_raw")) {
  df <- get(df_name)
  df$log_hours <- log1p(df$hoursworked * scale_params$hoursworked$sd +
                          scale_params$hoursworked$mean)   # back-transform then log
  assign(df_name, df)
}

# --- (b) Interaction: experience × education category (human capital theory) ---
for (df_name in c("train", "val", "test_raw")) {
  df <- get(df_name)
  df$exp_yos   <- df$exp * df$yos         # experience × years of schooling
  df$exp2_yos  <- df$exp2 * df$yos        # convex interaction
  assign(df_name, df)
}

# --- (c) Family structure composite ---
for (df_name in c("train", "val", "test_raw")) {
  df <- get(df_name)
  df$family_burden <- df$nchlt5 / (df$famsize + 1)  # share of very young children
  assign(df_name, df)
}

# =============================================================================
# STEP 9: BUILD DESIGN MATRICES  (model.matrix for Lasso / Ridge / Trees)
# =============================================================================

feature_cols <- c(cont_vars,
                  "log_hours", "exp_yos", "exp2_yos", "family_burden",
                  bin_vars, cat_vars)

# Formula: all features, no intercept (glmnet adds its own)
build_matrix <- function(df, ref_df = NULL) {
  # Align nominal factor levels to training data; ordinal vars are already integers
  if (!is.null(ref_df)) {
    for (v in cat_vars_nom) {
      df[[v]] <- factor(df[[v]], levels = levels(ref_df[[v]]))
    }
  }
  X <- model.matrix(~ . - 1,
                    data = df[, feature_cols, drop = FALSE])
  return(X)
}

X_train <- build_matrix(train)
X_val   <- build_matrix(val,      ref_df = train)
X_test  <- build_matrix(test_raw, ref_df = train)

# Align columns: val / test must match train exactly
X_val  <- X_val[,  colnames(X_train), drop = FALSE]
X_test <- X_test[, colnames(X_train), drop = FALSE]

# =============================================================================
# STEP 10: FEATURE SELECTION — remove near-zero variance & high correlations
# =============================================================================

nzv_idx <- nearZeroVar(X_train)
if (length(nzv_idx) > 0) {
  # drops columns with near-zero variance — uninformative for any model
  X_train <- X_train[, -nzv_idx]
  X_val   <- X_val[,  -nzv_idx]
  X_test  <- X_test[, -nzv_idx]
}

cor_mat     <- cor(X_train, use = "pairwise.complete.obs")
high_cor    <- findCorrelation(cor_mat, cutoff = 0.95)
if (length(high_cor) > 0) {
  # drops one column from each pair with correlation > 0.95
  X_train <- X_train[, -high_cor]
  X_val   <- X_val[,  -high_cor]
  X_test  <- X_test[, -high_cor]
}

# =============================================================================
# STEP 11: FINAL INTEGRITY CHECKS
# =============================================================================

stopifnot("NA in X_train"  = sum(is.na(X_train)) == 0)
stopifnot("NA in X_val"    = sum(is.na(X_val))   == 0)
stopifnot("NA in X_test"   = sum(is.na(X_test))  == 0)
stopifnot("Column mismatch train/val"  = identical(colnames(X_train), colnames(X_val)))
stopifnot("Column mismatch train/test" = identical(colnames(X_train), colnames(X_test)))

# =============================================================================
# STEP 12: FIT MODELS
# =============================================================================

rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae  <- function(actual, pred) mean(abs(actual - pred))

results <- list()

# --- (a) LASSO (alpha = 1) ---
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 10)
pred_lasso_val <- predict(cv_lasso, X_val, s = "lambda.min")
results$lasso <- list(
  model   = cv_lasso,
  rmse    = rmse(y_val, pred_lasso_val),
  mae     = mae(y_val,  pred_lasso_val),
  lambda  = cv_lasso$lambda.min
)

# --- (b) RIDGE (alpha = 0) ---
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 10)
pred_ridge_val <- predict(cv_ridge, X_val, s = "lambda.min")
results$ridge <- list(
  model   = cv_ridge,
  rmse    = rmse(y_val, pred_ridge_val),
  mae     = mae(y_val,  pred_ridge_val),
  lambda  = cv_ridge$lambda.min
)

# --- (c) DECISION TREE (cp tuned via caret) ---
ctrl_tree <- trainControl(method = "cv", number = 10)
grid_tree <- expand.grid(cp = seq(0.0001, 0.05, length.out = 20))
tree_fit  <- train(x = as.data.frame(X_train), y = y_train,
                   method    = "rpart",
                   trControl = ctrl_tree,
                   tuneGrid  = grid_tree)
pred_tree_val <- predict(tree_fit, as.data.frame(X_val))
results$tree <- list(
  model  = tree_fit,
  rmse   = rmse(y_val, pred_tree_val),
  mae    = mae(y_val,  pred_tree_val),
  best_cp = tree_fit$bestTune$cp
)

# --- (d) RANDOM FOREST (mtry tuned via caret; 5-fold CV for speed) ---
ctrl_rf  <- trainControl(method = "cv", number = 5)
grid_rf  <- expand.grid(mtry = c(floor(sqrt(ncol(X_train))),
                                  floor(ncol(X_train) / 3),
                                  floor(ncol(X_train) / 2)))
rf_fit   <- train(x = as.data.frame(X_train), y = y_train,
                  method    = "rf",
                  ntree     = 500,
                  trControl = ctrl_rf,
                  tuneGrid  = grid_rf)
pred_rf_val <- predict(rf_fit, as.data.frame(X_val))
results$rf <- list(
  model    = rf_fit,
  rmse     = rmse(y_val, pred_rf_val),
  mae      = mae(y_val,  pred_rf_val),
  best_mtry = rf_fit$bestTune$mtry
)

# =============================================================================
# STEP 13: MODEL COMPARISON & BEST MODEL SELECTION
# =============================================================================

comparison <- data.frame(
  Model  = c("Lasso", "Ridge", "Tree", "RandomForest"),
  RMSE   = c(results$lasso$rmse, results$ridge$rmse,
             results$tree$rmse,  results$rf$rmse),
  MAE    = c(results$lasso$mae,  results$ridge$mae,
             results$tree$mae,   results$rf$mae)
)
comparison <- comparison[order(comparison$RMSE), ]   # inspect comparison to pick best model

best_model_name <- comparison$Model[1]   # model with lowest validation RMSE

# =============================================================================
# STEP 14: PREDICTIONS ON NEW HIRES
# =============================================================================

best_preds <- switch(best_model_name,
  "Lasso"        = as.vector(predict(results$lasso$model, X_test, s = "lambda.min")),
  "Ridge"        = as.vector(predict(results$ridge$model, X_test, s = "lambda.min")),
  "Tree"         = predict(results$tree$model, as.data.frame(X_test)),
  "RandomForest" = predict(results$rf$model,   as.data.frame(X_test))
)

submission <- data.frame(
  id     = test_raw$id,
  lwwage = best_preds
)

write.csv(submission, "wage_predictions.csv", row.names = FALSE)
