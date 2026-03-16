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

str(wage_training_raw)
sapply(wage_training_raw, class)

intended_types <- list(
  numeric = c("yos", "exp", "exp2", "age", "famsize", "nchlt5", "hoursworked", "lwwage"),
  binary = c("sex", "hispanic", "schltype", "vetstat", "dchlt5", "dchlt19", "msa"),
  factor = c("marst", "race", "speakeng", "degfield", "educd", "education", "ind1990c", "occ2010c", 
             "region", "metro", "hw"),
  id = c("id")
)

check <- function(df, intended, label){
  df <- df
  
  for (var in intended$numeric) {
    if (!var %in% names(df)) next
    actual <- class(df[[var]])
    if (!actual %in% c("numeric", "integer")) {
      df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
    }
  }  
  
  for (var in intended$binary) {
    if (!var %in% names(df)) next
    actual <- class(df[[var]])
    if (actual == "character") {
      df[[var]] <- suppressWarnings(as.integer(df[[var]]))
    }
    else if (actual == "factor") {
      df[[var]] <- as.integer(as.character(df[[var]]))
    }
  } 
  
  for (var in intended$factor) {
    if (!var %in% names(df)) next
    actual <- class(df[[var]])
    if (actual != "factor") {
      df[[var]] <- as.factor(df[[var]])
    }
  }
  
  return(df)  
}

wage_training <- check(wage_training_raw, intended_types)
new_hires <- check(new_hires_raw, intended_types)


sapply(wage_training_raw, class)
sapply(wage_training, class)








wage_training %>% 
  select(all_of(var_numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ name, scales = "free_x")
  