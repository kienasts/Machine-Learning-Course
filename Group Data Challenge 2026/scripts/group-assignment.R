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

var_num <- c("yos", "exp", "exp2", "age", "famsize", "nchlt5", "hoursworked", "lwwage")
var_cat <- c("sex", "marst", "race", "speakeng", "hispanic", "schltype", "degfield", "vetstat", "educd",
             "education", "ind1990c", "occ2010c", "region", "dchlt5", "dchlt19", "msa", "metro", "hw")

check <- tibble(
  variable = names(wage_training),
  actual_class = sapply(wage_training, class)
) %>%
  mutate(
    expected = case_when(
      variable %in% var_num ~ "numeric",
      variable %in% var_cat ~ "factor/character",
      TRUE ~ "unlisted / check me"
    ),
    mismatch = case_when(
      variable %in% var_num & !actual_class %in% c("numeric", "integer") ~ TRUE,
      variable %in% var_cat & !actual_class %in% c("factor", "character") ~ TRUE,
      TRUE ~ FALSE
    )
  )

check %>% 
  print(n = Inf)







wage_training %>% 
  select(all_of(var_numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ name, scales = "free_x")
  