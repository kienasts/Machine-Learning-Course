# Preliminaries -----------------------------------------------------------

packages <- c(
  "tidyverse",    # data science
  "janitor"       # data cleaning
)
install.packages(setdiff(packages, rownames(installed.packages())), type = "binary")
lapply(packages, library, character.only = TRUE)

wage_training <- read_csv("data/wage_training.csv")
new_hires <- read_csv("data/new_hires.csv")




# EDA ---------------------------------------------------------------------
summary(wage_training)
colMeans(is.na(wage_training))
glimpse(wage_training)

var_numeric <- wage_training %>%
  select_if(is.numeric) %>%
  names()

var_categorical <- wage_training %>%
  select_if(is.character) %>%
  names()

wage_training %>% 
  select(all_of(var_numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ name, scales = "free_x")
  