library(dplyr)
library(stringr)
library(pROC)
library(caret)
library(glmnet)
library(writexl)
library(tidyverse)

# Step 1: Load and prepare the data
pilot <- read.csv("Data/SUDDENPilot-VeteranStatusProject_DATA_2021-07-22_1508 updated 11Aug21.csv")
control <- read.csv("Data/SimpsonSUDDENControl-VeteranStatusProject_DATA_2021-07-22_1508 updated 02Aug21 csv.csv")
colnames(pilot)
colnames(control)

# Function to summarize PTSD variables
summarize_ptsd_vars <- function(data, vars) {
  data %>%
    select(all_of(vars)) %>%
    summarise(across(everything(), 
                     list(
                       size = ~length(.),
                       missing = ~sum(is.na(.))
                     ))) %>%
    pivot_longer(everything(), 
                 names_to = c("variable", ".value"), 
                 names_pattern = "(.*)_(.*)") %>%
    arrange(variable)
}

pilot_ptsd_vars <- c("ptsd_phys_diag", "ptsd_milt", "ptsd_exposure", "ptsd_reliv", 
                     "ptsd_sleep", "ptsd_irrit", "ptsd_concen", "ptsd_meds", "ptsd_summary")
control_ptsd_vars <- c("ptsd_controlsd")

# Summarize PTSD variables in pilot data
cat("Summary of PTSD variables in pilot data:\n")
print(summarize_ptsd_vars(pilot_clean, pilot_ptsd_vars))
print(summarize_ptsd_vars(pilot, pilot_ptsd_vars))

# Summarize PTSD variable in control data
cat("Summary of PTSD variable in control data:\n")
print(summarize_ptsd_vars(control, control_ptsd_vars))

# Function to calculate Fisher's Exact Test p-value
calculate_fisher_p <- function(var, group) {
  tbl <- table(var, group)
  fisher_test <- fisher.test(tbl)
  return(fisher_test$p.value)
}

# Calculate p-values
ptsd_p_values <- sapply(pilot_ptsd_vars, function(var) {
  calculate_fisher_p(pilot_clean[[var]], pilot_clean$vet)
})

# Create a summary dataframe
ptsd_summary <- data.frame(
  missing_count = sapply(pilot_clean[pilot_ptsd_vars], function(x) sum(is.na(x))),
  p_value = ptsd_p_values
)

# Sort by missing count
ptsd_summary <- ptsd_summary %>%
  arrange(p_value)

# Print the summary
print(ptsd_summary)









##### Pre-process ##### 
# Data cleaning for pilot dataset
pilot_clean <- pilot %>%
  mutate(
    vet = case_when( # Veteran Status
      death_veteran_2021 == 1 ~ "Veteran",
      death_veteran_2021 %in% c(2, 3) ~ "Non-Veteran",
      TRUE ~ NA_character_ # Default case for any other values
    ),
    marry = case_when( # Marital Status
      str_detect(marital_status, "^Never") ~ "Never Married",
      str_detect(marital_status, "^Widowed|^Divorced|but") ~ "Married",
      marital_status %in% c("", "Unknown") ~ NA_character_,
      TRUE ~ "Married"
    ),
    race = case_when(
      race %in% c("Black", "Black or African American") ~ "Black",
      race == "White" ~ "White",
      TRUE ~ "Other"
    ), 
    mental_abuse_alc_hx = ifelse(mental_abuse_alc_hx == 2, 1, mental_abuse_alc_hx),
    mental_abuse_sub_hx = ifelse(mental_abuse_sub_hx == 2, 1, mental_abuse_sub_hx),
    ptsd_phys_diag = ifelse(ptsd_phys_diag == 3, NA, ptsd_phys_diag),
    ptsd_exposure = ifelse(ptsd_exposure == 3, NA, ptsd_exposure),
    ptsd_reliv = ifelse(ptsd_reliv == 3, NA, ptsd_reliv),
    ptsd_sleep = ifelse(ptsd_sleep == 3, NA, ptsd_sleep),
    ptsd_irrit = ifelse(ptsd_irrit == 3, NA, ptsd_irrit),
    ptsd_concen = ifelse(ptsd_concen == 3, NA, ptsd_concen),
    ptsd_meds = ifelse(ptsd_meds == 3, NA, ptsd_meds),
    comorb_htn = ifelse(comorb_htn == 2, NA, comorb_htn),
    comorb_dyslip = ifelse(comorb_dyslip == 2, NA, comorb_dyslip),
    comorb_dm = ifelse(comorb_dm == 2, NA, comorb_dm),
    comorb_cad = ifelse(comorb_cad == 2, NA, comorb_cad),
    comorb_cmypthy = ifelse(comorb_cmypthy == 2, NA, comorb_cmypthy),
    comorb_lvh = ifelse(comorb_lvh == 2, NA, comorb_lvh),
    
    # Alcohol
    alcohol = ifelse(mental_abuse_alc_ems == 1 | mental_abuse_alc_hx == 1, 1,
                     ifelse(mental_abuse_alc_ems == 0 & mental_abuse_alc_hx == 0, 0, NA)),
    # Substance
    sub = ifelse(mental_abuse_sub_ems == 1 | mental_abuse_sub_hx == 1, 1,
                 ifelse(mental_abuse_sub_ems == 0 & mental_abuse_sub_hx == 0, 0, NA)),
    # Anxiety or Depression
    anx_dep = ifelse(mental_anx_diagnosis == 1 | mental_depress_diagnosis == 1, 1,
                     ifelse(mental_anx_diagnosis == 0 & mental_depress_diagnosis == 0, 0, NA)),
    substance = ifelse(alcohol == 1 | sub == 1, 1, ifelse(alcohol == 0 & sub == 0, 0, NA)),
    sudden = 1
  ) %>%
  mutate(age_num = str_sub(age, 1, 2))

# Data cleaning for control dataset
control_clean <- control %>%
  mutate( # Veteran Status
    vet = case_when(
      final_vet_sts == 1 ~ "Veteran",
      final_vet_sts %in% c(2, 3) ~ "Non-Veteran",
      TRUE ~ NA_character_  # Default case for any other values
    ),
    marry = case_when( # Marital Status
      marital_sts == 0 ~ "Never Married",
      !marital_sts %in% c(0, 6) ~ "Married",
      marital_sts == 6 ~ NA_character_,
      TRUE ~ NA_character_  # Default case for any other values
    ),
    race = case_when( # Race
      race_controlsd %in% c(3, 4, 5) ~ "Other",
      race_controlsd == 1 ~ "White",
      race_controlsd == 2 ~ "Black",
      race_controlsd == 6 ~ "",
      TRUE ~ NA_character_  # Default case for any other values
    ),
    mental = ifelse(sevmh_controlsd == 1 | anymh_controlsd == 1, 1, 
                    ifelse(sevmh_controlsd == 0 & anymh_controlsd == 0, 0, NA)),
    anx_dep = ifelse(anx_controlsd == 1 | dep_controlsd == 1, 1, 
                     ifelse(anx_controlsd == 0 & dep_controlsd == 0, 0, NA)),
    substance = ifelse(alc_controlsd == 1 | sub_controlsd == 1, 1, 
                       ifelse(alc_controlsd == 0 & sub_controlsd == 0, 0, NA)),
    mental_all = ifelse(mental == 1 | anx_dep == 1, 1, 
                        ifelse(mental == 0 & anx_dep == 0, 0, NA)),
    gender = ifelse(sex_controlsd == 1, "Male", 
                    ifelse(sex_controlsd == 0, "Female", NA)),
    sudden = 0
  )
print(colnames(pilot))
print(colnames(control))
print(colnames(pilot_clean))
print(colnames(control_clean))

# Selecting columns from the pilot_clean dataframe
pilot_clean <- pilot_clean[c("study_id", "age_num", "gender", "race", "marry",
                             "comorb2_crd", "mental_depress_diagnosis", "mental_anx_diagnosis",
                             "alcohol", "substance", "ptsd_phys_diag", 
                             "comorb_htn", "comorb_dyslip", "comorb_dm", 
                             "comorb_cad", "comorb2_stroke", "comorb2_heartfailure", 
                             "comorb_cmypthy", "comorb_lvh", "vet", 
                             "anx_dep", "sudden")]

# Selecting columns from the control_clean dataframe
control_clean <- control_clean[c("id_controlsd", "age_controlsd", "gender", "race",
                                 "marry", "resp_controlsd", "dep_controlsd", "anx_controlsd",
                                 "alc_controlsd", "sub_controlsd", "ptsd_controlsd",
                                 "htn_controlsd", "dyslip_controlsd", "dm_controlsd", 
                                 "cad_controlsd", "stroke_controlsd", "chf_controlsd", 
                                 "cm_controlsd", "lvh_controlsd",  "vet", 
                                 "anx_dep", "sudden")]

names(pilot_clean) <- c("study_id", "age_num", "gender", "race", "marry", 
                        "crd", "depress", "anxiety", "alcohol", "substance", "ptsd",
                        "htn", "dyslip", "dm", "cad", "stroke", "heartfailure", 
                        "cm", "lvh",  "vet", "anx_dep", "sudden")
names(control_clean) <- c("study_id", "age_num", "gender", "race", "marry", 
                          "crd", "depress", "anxiety", "alcohol", "substance", "ptsd",
                          "htn", "dyslip", "dm", "cad", "stroke", "heartfailure", 
                          "cm", "lvh",  "vet", "anx_dep", "sudden")

data <- rbind(pilot_clean, control_clean)
data$age_num <- as.numeric(data$age_num)
data$numVet <- ifelse(data$vet == "Veteran", 1, 0)

# Filter out observations with NA in the vet variable
table(data$numVet, useNA = "always")
table(pilot_clean$numVet, useNA = "always")
table(control_clean$numVet, useNA = "always")
data <- data[!is.na(data$numVet), ]

# Convert categorical variables to factors
# data$vet <- factor(data$vet, levels = c("Non-Veteran", "Veteran"))
data$gender <- factor(data$gender, levels = c("Female", "Male"))
data$race <- factor(data$race, levels = c("White", "Black", "Other"))
data$marry <- factor(data$marry, levels = c("Married", "Never Married", "Unknown"))
data$substance <- factor(data$substance, levels = c("0", "1"))
data$anx_dep <- factor(data$anx_dep, levels = c("0", "1"))

vars <- c("sudden", "numVet", "age_num", "gender", "race", "marry", "substance", "anx_dep")

saveRDS(data, file="./Data/processed_data.rds")
##############################################################
# Step 2: Models
# Model 1: Veteran Status with Sudden Death
model1 <- glm(sudden ~ numVet, data = data, family = binomial(link = "logit"))
summary(model1)
or_table1 <- as.data.frame(exp(cbind(OR = coef(model1), confint(model1))))
or_table1 <- cbind(Variable = rownames(or_table1), or_table1)
rownames(or_table1) <- NULL
write_xlsx(or_table1, "./2022/Veteran_SuddenDeath/or_table1.xlsx")

# Model 2: Veteran Status with Sudden Death, adjusted by age, gender, race, and marital status
model2 <- glm(sudden ~ numVet + age_num + gender + race + marry,
              data = data_complete, family = binomial(link = "logit"))
summary(model2)
or_table2 <- as.data.frame(exp(cbind(OR = coef(model2), confint(model2))))
or_table2 <- cbind(Variable = rownames(or_table2), or_table2)
rownames(or_table2) <- NULL
write_xlsx(or_table2, "./2022/Veteran_SuddenDeath/or_table2.xlsx")

# Model 3: Veteran Status with Sudden Death, adjusted by age, gender, race, marital status, substance abuse, and mental health
model3 <- glm(sudden ~ numVet + age_num + gender + race + marry + substance + anx_dep,
              data = data_complete, family = binomial(link = "logit"))
summary(model3)
or_table3 <- exp(cbind(OR = coef(model3), confint(model3)))
or_table3 <- as.data.frame(exp(cbind(OR = coef(model3), confint(model3))))
or_table3 <- cbind(Variable = rownames(or_table3), or_table3)
rownames(or_table3) <- NULL
write_xlsx(or_table3, "./2022/Veteran_SuddenDeath/or_table3.xlsx")

# Mediation Analysis
library(mediation)
missing_counts <- colSums(is.na(data_complete))
print(missing_counts)

data_complete <- data[complete.cases(data[, c("sudden", "numVet", "age_num", "gender", "race", "marry", "substance", "anx_dep")]), ]
model3 <- glm(sudden ~ numVet + age_num + gender + race + marry + substance + anx_dep,
              data = data_complete, family = binomial(link = "logit"))
or_table <- as.data.frame(exp(cbind(OR = coef(model3), confint(model3))))

# Step 2: Establish the relationships between veteran status and the potential mediators
# Substance abuse
model_substance <- glm(substance ~ numVet + age_num + gender + race + marry, 
                       data = data_complete, family = binomial(link = "logit"), na.action = na.exclude)
summary(model_substance)
or_table_substance <- as.data.frame(exp(cbind(OR = coef(model_substance), confint(model_substance))))
# or_table_substance

# Anxiety/depression
model_anx_dep <- glm(anx_dep ~ numVet + age_num + gender + race + marry, 
                     data = data_complete, family = binomial(link = "logit"), na.action = na.exclude)
summary(model_anx_dep)
or_table_anx_dep <- as.data.frame(exp(cbind(OR = coef(model_anx_dep), confint(model_anx_dep))))
# or_table_anx_dep

#######################################################################
# Conduct a mediation analysis using bootstrapping 
library(boot)

# Define a function to perform the mediation analysis
mediation_analysis <- function(data, indices) {
  # Subset the data based on the bootstrap indices
  # data_boot <- data[sample(nrow(data), replace = TRUE), ]
  data_boot <- data[indices, ]
  
  # Fit the models using the bootstrapped data
  model3_boot <- glm(sudden ~ numVet + age_num + gender + race + marry + substance + anx_dep,
                     data = data_boot, family = binomial(link = "logit"))
  
  model_substance_boot <- glm(substance ~ numVet + age_num + gender + race + marry,
                              data = data_boot, family = binomial(link = "logit"))
  
  model_anx_boot <- glm(anx_dep ~ numVet + age_num + gender + race + marry,
                              data = data_boot, family = binomial(link = "logit"))
  
  # Extract the coefficients
  a1 <- coef(model_substance_boot)["numVet"]
  b1 <- coef(model3_boot)["substance1"]
  
  a2 <- coef(model_anx_boot)["numVet"]
  b2 <- coef(model3_boot)["anx_dep1"]
  
  indirect_effect1 <- a1 * b1
  indirect_effect2 <- a2 * b2
  
  return(c(indirect_effect1, indirect_effect2))
}

# Perform bootstrapping
set.seed(123)
boot_results <- boot(data_complete, mediation_analysis, R = 100)

# Calculate confidence intervals for the indirect effects
# Substance abuse
lower_substance <- quantile(boot_results$t[, 1], prob = 0.05/2)
upper_substance <- quantile(boot_results$t[, 1], prob = 1 - 0.05/2)
ci_substance <- c(lower_substance, upper_substance)

# Anxiety/depression
lower_anx_dep <- quantile(boot_results$t[, 2], prob = alpha/2)
upper_anx_dep <- quantile(boot_results$t[, 2], prob = 1 - alpha/2)
ci_anx_dep <- c(lower_anx_dep, upper_anx_dep)

# Print the results
cat("Indirect effect of substance abuse:", boot_results$t0[1], "\n")
cat("95% CI:", ci_substance[1], ci_substance[2], "\n\n")

cat("Indirect effect of anxiety/depression:", boot_results$t0[2], "\n")
cat("95% CI:", ci_anx_dep[1], ci_anx_dep[2], "\n")


# # Step 6: Assess model performance using ROC curves
# roc1 <- roc(data$sudden, predict(model1, type = "response"))
# roc2 <- roc(data$sudden, predict(model2, type = "response"))
# roc3 <- roc(data$sudden, predict(model3, type = "response"))
# 
# # Plot ROC curves
# plot(roc1, main = "ROC Curves for Logistic Regression Models")
# plot(roc2, add = TRUE, col = "blue")
# plot(roc3, add = TRUE, col = "red")
# legend("bottomright", legend = c("Model 1", "Model 2", "Model 3"), 
#        col = c("black", "blue", "red"), lty = 1)

mediation_analysis <- function(data, num_bootstrap = 1000) {
  indirect_effects1 <- numeric(num_bootstrap)
  indirect_effects2 <- numeric(num_bootstrap)
  
  for (i in 1:num_bootstrap) {
    data_boot <- data[sample(nrow(data), replace = TRUE), ]
    
    model3_boot <- try(glm(sudden ~ numVet + age_num + gender + race + marry + substance + anx_dep,
                           data = data_boot, family = binomial(link = "logit")), silent = TRUE)
    
    model_substance_boot <- try(glm(substance ~ numVet + age_num + gender + race + marry,
                                    data = data_boot, family = binomial(link = "logit")), silent = TRUE)
    
    model_anx_boot <- try(glm(anx_dep ~ numVet + age_num + gender + race + marry,
                              data = data_boot, family = binomial(link = "logit")), silent = TRUE)
    
    if (!inherits(model3_boot, "try-error") && !inherits(model_substance_boot, "try-error") && !inherits(model_anx_boot, "try-error")) {
      a1 <- coef(model_substance_boot)["numVet"]
      b1 <- coef(model3_boot)["substance"]
      
      a2 <- coef(model_anx_boot)["numVet"]
      b2 <- coef(model3_boot)["anx_dep"]
      
      if (!is.na(a1) && !is.na(b1)) {
        indirect_effects1[i] <- a1 * b1
      } else {
        indirect_effects1[i] <- NA
      }
      
      if (!is.na(a2) && !is.na(b2)) {
        indirect_effects2[i] <- a2 * b2
      } else {
        indirect_effects2[i] <- NA
      }
    } else {
      indirect_effects1[i] <- NA
      indirect_effects2[i] <- NA
    }
  }
  
  # Remove NAs before calculating confidence intervals
  indirect_effects1 <- na.omit(indirect_effects1)
  indirect_effects2 <- na.omit(indirect_effects2)
  
  if (length(indirect_effects1) > 0 && length(indirect_effects2) > 0) {
    ci1 <- quantile(indirect_effects1, probs = c(0.025, 0.975), na.rm = TRUE)
    ci2 <- quantile(indirect_effects2, probs = c(0.025, 0.975), na.rm = TRUE)
    
    list(indirect_effect1 = mean(indirect_effects1), ci1 = ci1,
         indirect_effect2 = mean(indirect_effects2), ci2 = ci2)
  } else {
    list(indirect_effect1 = NA, ci1 = NA,
         indirect_effect2 = NA, ci2 = NA)
  }
}

results <- mediation_analysis(data_complete)
print(results)


#######################################################################
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Assuming your data is stored in a data frame called 'data'
# with columns for each variable and a 'numVet' column indicating the veteran status

# Function to calculate p-values for categorical variables using chi-square test
calc_cat_pvalue <- function(data, var, group) {
  tbl <- table(data[[var]], data[[group]])
  p_value <- chisq.test(tbl)$p.value
  return(p_value)
}

# Function to calculate p-values for continuous variables using ANOVA or Kruskal-Wallis test
calc_cont_pvalue <- function(data, var, group, normality = TRUE) {
  if (normality) {
    model <- aov(data[[var]] ~ data[[group]])
    p_value <- summary(model)[[1]][["Pr(>F)"]][1]
  } else {
    p_value <- kruskal.test(data[[var]] ~ data[[group]])$p.value
  }
  return(p_value)
}

# Specify categorical and continuous variables
cat_vars <- c("gender", "race", "marry", "substance", "anx_dep")
cont_vars <- c("age_num")
cont_norm_vars <- c("age_num")

# Calculate p-values for each variable
p_values <- c()
for (var in cat_vars) {
  p_values[var] <- calc_cat_pvalue(data, var, "numVet")
}
for (var in cont_norm_vars) {
  p_values[var] <- calc_cont_pvalue(data, var, "numVet", normality = TRUE)
}
for (var in setdiff(cont_vars, cont_norm_vars)) {
  p_values[var] <- calc_cont_pvalue(data, var, "numVet", normality = FALSE)
}

# Create a summary table
summary_table <- data %>%
  group_by(numVet) %>%
  summarize(
    across(all_of(cat_vars), list(count = ~n(), prop = ~prop.table(table(.)))),
    across(all_of(cont_norm_vars), list(mean = mean, sd = sd)),
    across(setdiff(cont_vars, cont_norm_vars), list(median = median, q1 = ~quantile(., 0.25), q3 = ~quantile(., 0.75)))
  ) %>%
  pivot_longer(cols = -numVet, names_to = c("variable", ".value"), names_sep = "_") %>%
  mutate(
    p_value = map_dbl(variable, ~p_values[.x]),
    variable = factor(variable, levels = c(cat_vars, cont_vars))
  ) %>%
  arrange(variable)

# Display the table
kable(summary_table, caption = "Table 1. Comparison of Baseline Characteristics by Veteran Status") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Veteran Status" = 2)) %>%
  footnote(general = "Note: Values expressed as n (%), mean ± SD, or median (Q1, Q3). P-values calculated using chi-square test for categorical variables, ANOVA for normally distributed continuous variables, and Kruskal-Wallis test for non-normally distributed continuous variables.")