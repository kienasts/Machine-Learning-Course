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
      df[[var]] <- suppressWarnings(as.integer(df[[var]]))
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

training_idx <- sample(seq_len(nrow(wage_training)), size = floor(0.5 * nrow(wage_training)))

wage_training$quintile_strat <- cut(wage_training$lwwage,
                                   breaks = quantile(wage_training$lwwage, probs = seq(0, 1, by = 0.2)),
                                   include.lowest = TRUE, labels = FALSE)

training_set_stratq <- createDataPartition(wage_training$quintile_strat, p = 0.5, list = FALSE)
wage_training$quintile_strat <- NULL















  