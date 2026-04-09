clean<- FITFR_final_data_v3

clean <- clean[1:(nrow(clean) - 1), ]

# 1. Set the column names to the values in the first row
names(clean) <- clean[1, ]

# 2. Remove the first row from the data
clean <- clean[-1, ]

# Optional: Reset the row numbers so they start at 1 again
rownames(clean) <- NULL

clean$PR <- ifelse(clean$PR == "yes", 1, 0)

print(clean[,58])

table(clean[,58])


clean







# Load required library for data manipulation
library(tidyverse)

# 1. DATA IMPORT AND CLEANING
df <- FITFR_final_data_v3
names(df) <- df[1, ]

# 2. Remove the first row from the data
df <- df[-1, ]

# Optional: Reset the row numbers so they start at 1 again
rownames(df) <- NULL


df_clean <- df %>%
  filter(!is.na(ID)) %>%
  mutate(
    sex = str_to_lower(sex),
    sex = case_when(
      sex == "m" ~ "male",
      sex == "f" ~ "female",
      TRUE ~ sex
    ),
    treatment = str_to_upper(treatment),
    PR = as.factor(ifelse(PR == "yes", 1, 0)) # glm needs a factor or 0/1 numeric
  ) %>%
  mutate(
    age = ifelse(age == -99, NA, age),
    age = ifelse(is.na(age), median(age, na.rm = TRUE), age)
  )

# Prepare the final dataframe for the models (dropping unneeded columns)
df_model <- df_clean %>%
  select(-ID, -smoker) # Drop ID and the zero-variance smoker column

# Convert categorical covariates to factors
df_model$sex <- as.factor(df_model$sex)
df_model$treatment <- as.factor(df_model$treatment)

# 2. MODEL 1: STANDARD LINEAR REGRESSION FOR AF (Continuous Outcome)
df_model$rs123456 <- NULL
df_model$age <- as.numeric(as.character(df_model$age))

# Re-run the model
model_af_lm <- lm(AF ~ ., data = df_model)

print("--- Activating Factor (AF) Linear Model Summary ---")
summary(model_af_lm)

# 3. MODEL 2: STANDARD LOGISTIC REGRESSION FOR PR (Binary Outcome)
# family = "binomial" tells glm to use logistic regression
model_pr_glm <- glm(PR ~ ., data = df_model %>% select(-AF), family = "binomial")

print("--- Positive Response (PR) Logistic Model Summary ---")
summary(model_pr_glm)




data<-FITFR_final_data_v3

pacman::p_load(tidyverse, tidymodels, skimr, readxl, here, dplyr, janitor)

data <- readxl::read_excel(here("raw-data", "FITFR-final-data-v3.xlsx"))

# AF and PR = outcome variables
# age, sex, treatment and genetics = predictors

# CLEANING #

# correct row names
data <- data |>
  row_to_names(row_number = 1)

# standardise sex column
data <- data |>
  mutate(
    sex = case_when(
      str_detect(sex, "M") ~ "male",
      str_detect(sex, "Male") ~ "male",
      str_detect(sex, "Female") ~ "female",
      TRUE ~ sex)
  )

# remove last 4 useless columns
data <- data |>
  slice(1:(n() - 4))

# remove illogical ages
data <- data |>
  mutate(age = na_if(age, "-99"))

# standardise treatment column
data <- data |>
  mutate(
    treatment = case_when(
      str_detect(treatment, "a") ~ "A",
      str_detect(treatment, "b") ~ "B",
      str_detect(treatment, "c") ~ "C",
      TRUE ~ treatment)
  )

# make yes/no response binary
data <- data |>
  mutate(PR = case_when(
    PR == "yes" ~ 1,
    PR == "no" ~ 0,
    TRUE ~ NA_real_)
  )

# convert to numeric variables
data <- data |>
  mutate(across(c(age, ID, AF), as.numeric))

# convert to factors
data <- data %>%
  mutate(across(where(is.character), as.factor))



fit_bivariate <- function(x, y, df) {
  df <- df |>
    select(all_of(c(x, y))) |>
    na.omit()
  model_form <- reformulate(x, y)
  null_form <- formula(str_glue("{y} ~ 1"))
  full <- glm(model_form, data = df, family = "binomial")
  null <- glm(null_form, data = df, family = "binomial")
  
  # The ANOVA for GLM objects naturally supports the Likelihood Ratio Test
  pv <- anova(null, full, test = "Chisq") |> # Often referred to as Analysis of Deviance
    broom::tidy() |>
    slice(2) |>
    pull(p.value)
  tibble(term = x, pv = pv)
}
fit_bivariates <- function(y, preds, df) {
  bivariates <-
    preds |>
    map(safely(\(x) fit_bivariate(x, y, df))) |>
    transpose()
  print(bivariates)
  bivariates$result |>
    list_rbind() |>
    mutate(adj_p = p.adjust(pv, method = "fdr")) |>
    filter(!is.na(adj_p)) |>
    mutate(outcome = y)
}


preds<-colnames(data)

preds<-preds[1:55]

hello<-fit_bivariates("PR",preds,data)







































