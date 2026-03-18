# Preliminaries -----------------------------------------------------------

packages <- c(
  "tidyverse",    # data science
  "janitor",       # data cleaning
  "glmnet",
  "randomForest",
  "rpart",
  "corrplot"
)
install.packages(setdiff(packages, rownames(installed.packages())), type = "binary")
lapply(packages, library, character.only = TRUE)

wage_training_raw <- read_csv("data/wage_training.csv")
new_hires_raw <- read_csv("data/new_hires.csv")



# Exploratory Data Analysis -----------------------------------------------

# Overview
summary(wage_training_raw)
glimpse(wage_training_raw)

# Missings
miss_pct <- colMeans(is.na(wage_training_raw))
print(miss_pct[miss_pct > 0])

# Distributions
hist(wage_training_raw$lwwage, breaks = 50)
wage_training_raw %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ name, scales = "free_x")

# Correlations
num_cols <- names(wage_training_raw)[sapply(wage_training_raw, is.numeric)]
num_cols <- setdiff(num_cols, c("lwwage", "id"))
cor <- round(cor(wage_training_raw[, num_cols]), 2)
corrplot(cor)



# Variable Types ----------------------------------------------------------

glimpse(wage_training_raw)
sapply(wage_training_raw, class)

sapply(wage_training_raw[sapply(wage_training_raw, is.numeric)], function(x) length(unique(x)))

intended_types <- list(
  numeric = c("yos", "exp", "exp2", "age", "famsize", "nchlt5", "hoursworked", "lwwage"),
  binary = c("sex", "hispanic", "schltype", "vetstat", "dchlt5", "dchlt19", "msa"),
  factor = c("marst", "race", "degfield", "educd", "education", "ind1990c", "occ2010c", 
             "region", "metro"),
  ordered = c("speakeng", "hw"),
  id = c("id")
)

check_fix_var_types <- function(df, intended, label){
  df <- df
  
  for (var in intended_types$numeric) {
    if (!var %in% names(df)) next
    actual <- class(df[[var]])
    if (!actual %in% c("numeric", "integer")) {
      df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
    }
  }  
  
  for (var in intended_types$binary) {
    if (!var %in% names(df)) next
    actual <- class(df[[var]])
    if (actual == "character") {
      parsed <- suppressWarnings(as.integer(df[[var]]))
      if (all(is.na(parsed) == is.na(df[[var]]))) {
        df[[var]] <- parsed
      } else {
        lvls <- sort(unique(df[[var]][!is.na(df[[var]])]))
        if (length(lvls) != 2) warning(paste0("Variable ", var, ": length differs from 2."))
        df[[var]] <- as.integer(df[[var]] == lvls[2])
      }
    }
    
    else if (actual == "factor") {
      df[[var]] <- as.integer(as.character(df[[var]]))
    }
  } 
  
  for (var in intended_types$factor) {
    if (!var %in% names(df)) next
    actual <- class(df[[var]])
    if (actual != "factor") {
      df[[var]] <- as.factor(df[[var]])
    }
  }
  
  for (var in intended_types$ordered) {
    if (!var %in% names(df)) next
    actual <- class(df[[var]])
    if (actual != "ordered") {
      df[[var]] <- as.factor(df[[var]])
    }
  }
  
  return(df)  
}

wage_training <- check_fix_var_types(wage_training_raw, intended_types)
new_hires <- check_fix_var_types(new_hires_raw, intended_types)

sapply(wage_training, class)
lapply(wage_training[sapply(wage_training, is.factor)], levels)
names(wage_training)[sapply(wage_training, is.character)]
sapply(new_hires, class)
lapply(new_hires[sapply(new_hires, is.factor)], levels)
names(new_hires)[sapply(new_hires, is.character)]



# Train / Test Split ------------------------------------------------------

set.seed(1001)
training_set_idx <- sample(seq_len(nrow(wage_training)), size = floor(0.5 * nrow(wage_training)))
train <- wage_training[training_set_idx, ]
test <- wage_training[-training_set_idx, ]

y_train <- train$lwwage
y_test <- test$lwwage



# Missing Data ------------------------------------------------------------

cont_vars <- c("yos", "exp", "exp2", "age", "famsize", "nchlt5", "hoursworked")
bin_vars <- c("sex", "hispanic", "vetstat", "dchlt5", "dchlt19", "msa")
cat_vars_nom <- c("marst", "race", "degfield", "educd", "education", "ind1990c", "occ2010c", "region", "metro")
cat_vars_ord <- c("speakeng", "hw")

colMeans(is.na(train))

zero_var <- sapply(train, function(x) length(unique(x[!is.na(x)])) < 2)
cat("Zero-variance variables:", names(which(zero_var)), "\n")
train <- train[, !zero_var]
test  <- test[,  names(train)]

colSums(is.na(train))
colSums(is.na(test))



# Outliers ----------------------------------------------------------------



# Categorical Variables ---------------------------------------------------

# Rare category pooling
rare_levels <- lapply(train[cat_vars_nom], function(x) {
  freq <- prop.table(table(x))
  names(freq[freq < 0.02])
})

for (var in cat_vars_nom) {
  
}





