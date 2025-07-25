################################################################################
# HtM PREDICTION
# desc: estimating how personal characteristics affect the probability of being HtM
################################################################################

# Load packages
library(dplyr)
library(glmnet) # LASSO
library(BAS) # BMA
library(pscl) # pR2 
library(pROC)
library(ResourceSelection)
library(loo)

################################################################################
# Load P1 to P5 occupational pension data and append them
p_files <- paste0("P", 1:5, ".csv")
df1 <- bind_rows(lapply(p_files, function(f) read.csv(file.path(HFCSDATA, f))))
df <- df1

################################################################################
# Create variables and prepare for merging
# Country selection
df <- df %>%
  filter(sa0100 == COUNTRY)

# Age either midpoint (standard) or ordered index (group) method
df <- df %>%
  mutate(
    age = if (ageGrpMethod == 1) {
      ra0300
    } else if (ageGrpMethod == 2) {
      case_when(
        is.na(ra0300) ~ NA_integer_,
        ra0300 >= 100 ~ 102,
        TRUE ~ floor(ra0300 / 5) * 5 + 2
      )
    } else if (ageGrpMethod == 3) {
      case_when(
        is.na(ra0300) ~ NA_integer_,
        ra0300 >= 100 ~ 21,
        TRUE ~ floor(ra0300 / 5) + 1
      )
    } else {
      stop("Error: invalid ageGrpMethod selected, please choose 1, 2 or 3.")
    }
  )
#barplot(table(df$age), main = "Distribution of variable",
#        las = 2, xlab = "variable", ylab = "Count")

# Variables age2, selfempl, employee, married, female, perm, hwusual, empl
df <- df %>%
  mutate(
    age2 = age^2,
    selfempl = as.integer(pe0200 %in% c(2, 3)),
    employee = as.integer(pe0200 == 1),
    married = as.integer(pa0100 %in% c(2, 3)),
    female = as.integer(ra0200 == 2),
    perm = as.integer(pe0500 == 1),
    hwusual = pe0600,
    empl = if_else((pe0100a %in% c(1,2)) | (pe0100b %in% c(1,2)) | 
                     (pe0100c %in% c(1,2)) | (pe0100d %in% c(1,2)), 1, 0)
  )

# Education levels # few obs. in prim_edu, most pop. sec_edu
df <- df %>%
  mutate(
    prim_edu = as.integer(pa0200 == 1),
    sec_edu = as.integer(pa0200 %in% c(2, 3)),
    #primsec_edu = as.integer(pa0200 %in% c(1, 2, 3)),
    tert_edu = as.integer(pa0200 == 5)
  )
#df_edu <- df %>% filter(!is.na(pa0200))
#edu_counts <- table(df_edu$pa0200)
#print(edu_counts)
#barplot(edu_counts, main = "Distribution of education levels (pa0200)",
#        col = "lightgreen", las = 2, xlab = "Education Level", ylab = "Count")

# Job dummy from ISCO
job_categories <- sort(unique(df$pe0300 %/% 10), na.last = TRUE)
for (job in job_categories[!is.na(job_categories)]) {
  dummy_name <- paste0("job", job)
  df[[dummy_name]] <- ifelse(df$pe0300 %/% 10 == job, 1, ifelse(is.na(df$pe0300), NA, 0))
}
#df_job <- df %>% filter(!is.na(pe0300)) %>%
#  mutate(jobgroup = pe0300 %/% 10)
#job_counts <- table(df_job$jobgroup)
#print(job_counts)
#barplot(job_counts, main = "Distribution of ISCO (pe0300)",
#        col = "salmon", las = 2, xlab = "ISCO", ylab = "Count")

# Sector dummy from NACE2
if (imputationModelChoice == 1) {
  # Default sectors by the first letter of NACE2 code
  df$pe0400 <- ifelse(df$pe0400 == "", NA, df$pe0400)
  
  nace2_sectors <- LETTERS[1:21]
  for (i in seq_along(nace2_sectors)) {
    sector_letter <- nace2_sectors[i]
    dummy_name <- paste0("sec", i)
    
    df[[dummy_name]] <- case_when(
      is.na(df$pe0400) ~ NA_real_,
      df$pe0400 == sector_letter ~ 1,
      TRUE ~ 0
    )
  }
  #df_sec <- df %>%
  #  filter(pe0400 %in% nace2_sectors) %>%
  #  mutate(secgroup = pe0400)
  #sec_counts <- table(factor(df_sec$secgroup, levels = nace2_sectors))
  #print(sec_counts)
  #barplot(sec_counts, main = "Distribution of NACE (pe0400)",
  #        col = "lightblue", las = 2, xlab = "pe0400", ylab = "Count")
} else if (imputationModelChoice %in% c(2, 3, 4)) {
  # NACE2 "high-level aggregation" grouping (Slacalek i. transforms NACE2 to NACE, ii. groups some codes, e.g. A,B; C,D,E; O,P,Q)
  df <- df %>%
    mutate(pe0400_grouped = case_when(
      pe0400 %in% c("A") ~ "A", # Agriculture, forestry, fishing
      pe0400 %in% c("B", "D", "E") ~ "B,D,E", # Mining and quarrying and other industry
      pe0400 %in% c("C") ~ "C", # Manufacturing
      pe0400 %in% c("F") ~ "F", # Construction
      pe0400 %in% c("G", "H", "I") ~ "G,H,I", # Wholesale and retail trade, transportation and storage, accommodation and food service activities
      pe0400 %in% c("J") ~ "J", # Information and communication
      pe0400 %in% c("K") ~ "K", # Financial and insurance activities
      pe0400 %in% c("L")~ "L", # Real estate activities
      pe0400 %in% c("M", "N")~ "M,N", # Professional, scientific, technical, administration and support service activities
      pe0400 %in% c("O", "P", "Q")~ "O,P,Q", # Public administration, defence, education, human health and social work activities
      pe0400 %in% c("R", "S", "T", "U") ~ "R,S,T,U", # Other services
      TRUE ~ NA_character_
    ))
  
  nace2_sectors_grouped <- c("A", "B,D,E", "C", "F", "G,H,I", "J", "K", "L", "M,N", "O,P,Q", "R,S,T,U")
  for (i in seq_along(nace2_sectors_grouped)) {
    sector_letter <- nace2_sectors_grouped[i]
    dummy_name <- paste0("sec", i)
    
    df[[dummy_name]] <- case_when(
      is.na(df$pe0400) ~ NA_real_,
      df$pe0400 == sector_letter ~ 1,
      TRUE ~ 0
    )
  }
  
  # CHECK
  #df_sec <- df %>%
  #  filter(pe0400_grouped %in% nace2_sectors_grouped) %>%
  #  mutate(secgroup = pe0400_grouped)
  #sec_counts <- table(factor(df_sec$secgroup, levels = nace2_sectors_grouped))
  #print(sec_counts)
  #barplot(sec_counts, main = "Distribution of grouped NACE (pe0400_grouped)",
  #        col = "lightblue", las = 2, xlab = "pe0400_grouped", ylab = "Count")
  
  df <- df %>% select(-pe0400_grouped)
} else {
  stop("Error: invalid imputationModelChoice selected, please choose 1, 2, 3 or 4.")
}

# Check if 'sa0100' is numeric
if (is.numeric(df$sa0100)) {
  # Convert 'sa0100' to a character string if it's numeric
  df <- df %>%
    mutate(sa0100 = as.character(sa0100)) %>%
    arrange(sa0100, sa0010, im0100)
} else {
  # If 'sa0100' is already a string, ensure sorting and encoding
  df <- df %>% arrange(sa0100)
  # Convert 'sa0100' to a factor to simulate encoding, if needed
  df <- df %>% mutate(sa0100 = factor(sa0100))
}

# Keep only relevant variables (no age filtering applied)
df <- df %>%
  filter(ra0100 == 1) %>%  # Only keep observations where 'ra0100' equals 1
  dplyr::select(hid, age, age2, starts_with("sec"), starts_with("job"), ends_with("edu"), married, female, empl, employee, selfempl, perm, hwusual)

# Rename 'hid' to 'id'
df <- df %>% rename(id = hid)

################################################################################
# Merge dataframes
# Perform a 1:1 merge with the external dataset
df_orig <- read.csv(file.path(OUTPUT, paste0(DBNAME, "_orig", ".csv")))

# Merge the datasets on 'id'
df <- df %>%
  left_join(df_orig, by = "id") %>% # Perform a 1:1 merge based on 'id'
  filter(!is.na(hw0010)) # Drop rows without weights

write.csv(df, file.path(OUTPUT, paste0(DBNAME, "_rp.csv")), row.names = FALSE)
#df <- read.csv(file.path(OUTPUT, paste0(DBNAME, "_rp", ".csv")))

################################################################################
# NA handling
# Characteristics not related to employment plus employment status
cols_not_employment <- c("sa0100", "age", "age2", "prim_edu", "sec_edu", "tert_edu", 
                         "married", "female", "empl")

# Characteristics related to employment
if (imputationModelChoice == 1) {
  cols_empldep <- c("employee", "selfempl", "hwusual",
                    "job0", "job1", "job2", "job3", "job4", "job5", "job6", "job7", "job8", "job9")
  cols_employeedep <- c("perm", "sec1", "sec2", "sec3", "sec4", "sec5", "sec6", "sec7", "sec8", "sec9", "sec10",
                        "sec11", "sec12", "sec13", "sec14", "sec15", 
                        "sec16", "sec17", "sec18", "sec19", "sec20", "sec21")
} else if (imputationModelChoice == 2) {
  cols_empldep <- c("employee", "selfempl", "hwusual",
                    "job0", "job1", "job2", "job3", "job4", "job5", "job6", "job7", "job8", "job9")
  cols_employeedep <- c("perm", "sec1", "sec2", "sec3", "sec4", "sec5", "sec6", "sec7", "sec8", "sec9", "sec10",
                        "sec11")
} else if (imputationModelChoice == 3) {
  cols_empldep <- NULL
  cols_employeedep <- NULL
} else {
  stop("Error: invalid imputationModelChoice selected, please choose 1, 2 or 3.")
}

# Drop rows with NAs in non-employment variables and in employment status
df_nafree <- df[complete.cases(df[, cols_not_employment]), ]

if (imputationModelChoice %in% c(1, 2)) {
  # For rows where employment is 0, replace NAs in employment-dependent columns with 0
  unemployed_rows <- df_nafree$empl == 0
  df_nafree[unemployed_rows, cols_empldep] <- lapply(df_nafree[unemployed_rows, cols_empldep], function(x) {
    x[is.na(x)] <- 0
    x
  })
  
  # For rows where employment is 1, drop rows that have any NA in employment-dependent columns
  employed_rows <- df_nafree$empl == 1
  df_nafree <- df_nafree[!(employed_rows & !complete.cases(df_nafree[, cols_empldep])), ]
} else if (imputationModelChoice == 3) {
  {}
} else {
  stop("Error: invalid imputationModelChoice selected, please choose 1, 2 or 3.")
}

if (imputationModelChoice %in% c(1, 2)) {
  # For rows where employee is 0, replace NAs in employee-dependent columns with 0
  selfemployed_rows <- df_nafree$employee == 0
  df_nafree[selfemployed_rows, cols_employeedep] <- lapply(df_nafree[selfemployed_rows, cols_employeedep], function(x) {
    x[is.na(x)] <- 0
    x
  })
  
  # For rows where employee is 1, drop rows that have any NA in employee-dependent columns
  notselfemployed_rows <- df_nafree$employee == 1
  df_nafree <- df_nafree[!(notselfemployed_rows & !complete.cases(df_nafree[, cols_employeedep])), ]
} else if (imputationModelChoice == 3) {
  {}
} else {
  stop("Error: invalid imputationModelChoice selected, please choose 1, 2 or 3.")
}

# Re-normalize weights
# Compute the original total weights by date_q from df (before NA omission)
original_hw0010 <- df %>%
  summarise(orig_sum = sum(hw0010, na.rm = TRUE)) %>%
  mutate(sa0100 = "CZ")
# Join totals into the nafree dataset and re-normalize
df_nafree <- df_nafree %>%
  left_join(original_hw0010, by = "sa0100") %>%
  mutate(current_sum = sum(hw0010, na.rm = TRUE),
         hw0010 = hw0010 * (orig_sum / current_sum)) %>%
  ungroup() %>%
  dplyr::select(-orig_sum, -current_sum)

# Make dataframe without NAs the main dataframe
df <- df_nafree

################################################################################
# Define HtM status with two groups only
df_htm <- df %>%
  mutate(statushtm = case_when(
    statushtm %in% c(1, 2) ~ 1,  # HtM
    statushtm %in% c(3, 4) ~ 0,  # Non-HtM
    TRUE ~ NA_integer_
  ))

# Recode to exclude non-HtM, create dummy of two HtM groups only
df_phtm <- df %>%
  filter(!statushtm %in% c(3, 4)) %>%
  mutate(statushtm = if_else(statushtm == 2, 0, statushtm))  

# Recode to exclude HtM, create dummy of two non-HtM groups only
if (top10Index == 1) {
  df_top10 <- df %>%
    filter(!statushtm %in% c(1, 2)) %>%
    mutate(statushtm = if_else(statushtm == 4, 1, 0))  # Recode for top 10% income
}

# Define the list of implicates
implicates <- 1:5

################################################################################
# Define the dummy variable names
#dummyvars <- c("tert_edu", "married", "female", 
#               "selfempl", "perm", 
#               "job0", "job1", "job2", "job3", "job4", "job5", "job6", "job7", "job8", "job9",
#               "sec1", "sec2", "sec3", "sec4", "sec5", "sec6", "sec7", "sec8", "sec9", "sec10",
#               "sec11", "sec12", "sec13", "sec14", "sec15", "sec16", "sec17", "sec18", "sec19", "sec20", "sec21")
#
## Initialize an empty list to store per-implicate summaries
#countcheck_list <- list()
#
#for (imp in implicates) {
#  # Filter each dataset for the current imputation
#  df_htm_imp <- df_htm %>% filter(im0100 == imp)
#  df_phtm_imp <- df_phtm %>% filter(im0100 == imp)
#  df_top10_imp <- df_top10 %>% filter(im0100 == imp)
#  
#  # For each dummy variable, sum the ones in each dataset
#  htm_counts <- sapply(dummyvars, function(var) sum(df_htm_imp[[var]], na.rm = TRUE))
#  phtm_counts <- sapply(dummyvars, function(var) sum(df_phtm_imp[[var]], na.rm = TRUE))
#  top10_counts <- sapply(dummyvars, function(var) sum(df_top10_imp[[var]], na.rm = TRUE))
#  
#  # Create a data frame for this implicate with the dummy variable names and counts
#  imp_df <- data.frame(
#    Variable = dummyvars,
#    htmi = htm_counts,
#    phtmi = phtm_counts,
#    top10i = top10_counts,
#    stringsAsFactors = FALSE
#  )
#  
#  # Rename the columns to reflect the implicate number
#  names(imp_df)[2:4] <- paste0("imp", imp, c("_htm", "_phtm", "_top10"))
#  countcheck_list[[imp]] <- imp_df
#}
#
## Merge the per-implicate results into one summary table by 'Variable'
#dummy_summary <- Reduce(function(x, y) merge(x, y, by = "Variable", all = TRUE), countcheck_list)
#print(dummy_summary)

################################################################################
# Prepare the model formulas, predictors and colnames for coefficient storage dataframe
if (imputationModelChoice == 1) {
  model_formula_htm <- statushtm ~ age + age2 + tert_edu +
                        married + female + selfempl + perm + hwusual +
                        job0 + job1 + job2 + job3 + job4 + job5 + job6 + job8 + job9 +
                        sec1 + sec2 + sec4 + sec5 + sec6 + sec7 + sec8 + sec9 + sec10 +
                        sec11 + sec12 + sec13 + sec14 + 
                        sec15 + sec16 + sec17 + sec18 + sec19 + sec20 + sec21
  model_formula_phtm <- model_formula_htm
  model_formula_top10 <- model_formula_htm
} else if (imputationModelChoice == 2) {
  model_formula_htm <- statushtm ~ age + age2 + tert_edu +
                        married + female + selfempl + perm + hwusual +
                        job0 + job1 + job3 + job4 + job5 + job6 + job7 + job8 + job9 +
                        sec1 + sec2 + sec4 + sec5 + sec6 + sec7 + sec8 + sec9 + sec10 + 
                        sec11
  model_formula_phtm <- model_formula_htm
  model_formula_top10 <- model_formula_htm
} else if (imputationModelChoice == 3) {
  model_formula_htm <- statushtm ~ age + age2 + tert_edu + married + female
  model_formula_phtm <- model_formula_htm
  model_formula_top10 <- model_formula_htm
} else {
  stop("Error: invalid imputationModelChoice selected, please choose 1, 2 or 3.")
}

if (imputationModelChoice == 1) {
  predictors_htm <- c("(Intercept)", "age", "age2", "tert_edu", "married", "female", 
                  "selfempl", "perm", "hwusual",
                  "job0", "job1", "job2", "job3", "job4", "job5", "job6", "job8", "job9",
                  "sec1", "sec2", "sec4", "sec5", "sec6", "sec7", "sec8", "sec9", "sec10",
                  "sec11", "sec12", "sec13", "sec14", "sec15", 
                  "sec16", "sec17", "sec18", "sec19", "sec20", "sec21")
  predictors_phtm <- predictors_htm
  predictors_top10 <- predictors_htm
} else if (imputationModelChoice == 2) {
  predictors_htm <- c("(Intercept)", "age", "age2", "tert_edu", "married", "female",
                  "selfempl", "perm", "hwusual",
                  "job0", "job1", "job2", "job3", "job4", "job5", "job6", "job8", "job9",
                  "sec1", "sec2", "sec4", "sec5", "sec6", "sec7", "sec8", "sec9", "sec10",
                  "sec11")
  predictors_phtm <- predictors_htm
  predictors_top10 <- predictors_htm
} else if (imputationModelChoice == 3) {
  predictors_htm <- c("(Intercept)", "age", "age2", "tert_edu", "married", "female")
  predictors_phtm <- predictors_htm
  predictors_top10 <- predictors_htm
} else {
  stop("Error: invalid imputationModelChoice selected, please choose 1, 2 or 3.")
}
  
if (imputationModelChoice == 1) {
  bcolnames_htm <- c("b_intercept", "b_age", "b_age2", "b_tert_edu", "b_married", 
                 "b_female", "b_selfempl", "b_perm", "b_hwusual",
                 "b_job0", "b_job1", "b_job2", "b_job3", "b_job4", "b_job5", "b_job6", "b_job8", "b_job9",
                 "b_sec1", "b_sec2", "b_sec4", "b_sec5", "b_sec6", "b_sec7", "b_sec8", "b_sec9", "b_sec10",
                 "b_sec11", "b_sec12", "b_sec13", "b_sec14", "b_sec15",
                 "b_sec16", "b_sec17", "b_sec18", "b_sec19", "b_sec20", "b_sec21")
  bcolnames_phtm <- bcolnames_htm
  bcolnames_top10 <- bcolnames_htm
} else if (imputationModelChoice == 2) {
  bcolnames_htm <- c("b_intercept", "b_age", "b_age2", "b_tert_edu", "b_married", 
                 "b_female", "b_selfempl", "b_perm", "b_hwusual",
                 "b_job0", "b_job1", "b_job2", "b_job3", "b_job4", "b_job5", "b_job6", "b_job8", "b_job9",
                 "b_sec1", "b_sec2", "b_sec4", "b_sec5", "b_sec6", "b_sec7", "b_sec8", "b_sec9", "b_sec10",
                 "b_sec11")
  bcolnames_phtm <- bcolnames_htm
  bcolnames_top10 <- bcolnames_htm
} else if (imputationModelChoice == 3) {
  bcolnames_htm <- c("b_intercept", "b_age", "b_age2", "b_tert_edu", "b_married", "b_female")
  bcolnames_phtm <- bcolnames_htm
  bcolnames_top10 <- bcolnames_htm
} else {
  stop("Error: invalid imputationModelChoice selected, please choose 1, 2 or 3.")
}

################################################################################
# LASSO
if (runLASSO == 1) {
  ################################################################################
  # HtM estimation
  results_htm_lasso <- list()
  model_formula <- model_formula_htm
  predictors <- predictors_htm
  bcolnames <- bcolnames_htm
  
  # Loop over each imputation
  for (imp in implicates) {
    df_imp <- df_htm %>%
      filter(sa0100 == COUNTRY, im0100 == imp)
    
    if (nrow(df_imp) > 0) {
      x <- model.matrix(model_formula, data = df_imp)
      y <- df_imp$statushtm
      
      if (imputationWeighted == 1) {
        # Fit logistic LASSO with cross-validation to select lambda
        cvfit <- cv.glmnet(x, y, weights = df_imp$hw0010, family = "binomial", alpha = 1)
        best_lambda <- cvfit$lambda.min
        
        # Refit final LASSO model w the best lambda, get, convert and save coeffs
        lasso_model_htm <- glmnet(x, y, weights = df_imp$hw0010, family = "binomial", alpha = 1, lambda = best_lambda)
      } else {
        cvfit <- cv.glmnet(x, y, family = "binomial", alpha = 1)
        best_lambda <- cvfit$lambda.min
        
        lasso_model_htm <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)
      }
      
      #pred_probs <- predict(lasso_model_htm, newx = x, type = "response")
      #logL_model <- sum(y * log(pred_probs) + (1 - y) * log(1 - pred_probs))
      #null_prob <- mean(y)
      #logL_null <- sum(y * log(null_prob) + (1 - y) * log(1 - null_prob))
      #pseudo_R2 <- 1 - (logL_model / logL_null)
      #cat("Imputation", imp, "- McFadden's pseudo-R²:", pseudo_R2, "\n")
      
      lasso_coeffs <- as.numeric(coef(lasso_model_htm))
      names(lasso_coeffs) <- rownames(coef(lasso_model_htm))
      lasso_coeffs <- lasso_coeffs[predictors]
      results_htm_lasso[[paste0("imp", imp)]] <- lasso_coeffs
    }
  }
  
  # Results to a matrix w row for each imputation, average coeffs across imputations
  coeff_matrix_htm_lasso <- do.call(rbind, lapply(implicates, function(imp) {
    results_htm_lasso[[paste0("imp", imp)]]
  }))
  average_coeffs_htm_lasso <- colMeans(coeff_matrix_htm_lasso, na.rm = TRUE)
  
  # Save averaged coefficients, rename columns with a "b_" prefix
  coeff_df_htm_lasso <- data.frame(t(average_coeffs_htm_lasso))
  colnames(coeff_df_htm_lasso) <- bcolnames
  coeff_df_htm_lasso$country <- COUNTRY
  #View(coeff_df_htm_lasso)
  
  # Save the final coefficients to a CSV file
  write.csv(coeff_df_htm_lasso, file.path(OUTPUT, paste0("lfs/beta_", DBNAME, "_prob_htm_lasso_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")), row.names = FALSE)
  
  ################################################################################
  # Poor HtM estimation conditional on HtM
  results_phtm_lasso <- list()
  model_formula <- model_formula_phtm
  predictors <- predictors_phtm
  bcolnames <- bcolnames_phtm
  
  # Loop over each imputation
  for (imp in implicates) {
    df_imp <- df_phtm %>%
      filter(sa0100 == COUNTRY, im0100 == imp)
    
    if (nrow(df_imp) > 0) {
      x <- model.matrix(model_formula, data = df_imp)
      y <- df_imp$statushtm
      
      if (imputationWeighted == 1) {
        cvfit <- cv.glmnet(x, y, weights = df_imp$hw0010, family = "binomial", alpha = 1)
        best_lambda <- cvfit$lambda.min
        
        lasso_model_phtm <- glmnet(x, y, weights = df_imp$hw0010, family = "binomial", alpha = 1, lambda = best_lambda)
      } else {
        vfit <- cv.glmnet(x, y, family = "binomial", alpha = 1)
        best_lambda <- cvfit$lambda.min
        
        lasso_model_phtm <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)
      }
      
      #pred_probs <- predict(lasso_model_phtm, newx = x, type = "response")
      #logL_model <- sum(y * log(pred_probs) + (1 - y) * log(1 - pred_probs))
      #null_prob <- mean(y)
      #logL_null <- sum(y * log(null_prob) + (1 - y) * log(1 - null_prob))
      #pseudo_R2 <- 1 - (logL_model / logL_null)
      #cat("Imputation", imp, "- McFadden's pseudo-R²:", pseudo_R2, "\n")
      
      lasso_coeffs <- as.numeric(coef(lasso_model_phtm))
      names(lasso_coeffs) <- rownames(coef(lasso_model_phtm))
      lasso_coeffs <- lasso_coeffs[predictors]
      results_phtm_lasso[[paste0("imp", imp)]] <- lasso_coeffs
    }
  }
  
  # Results to a matrix w row for each imputation, average coeffs across imputations
  coeff_matrix_phtm_lasso <- do.call(rbind, lapply(implicates, function(imp) {
    results_phtm_lasso[[paste0("imp", imp)]]
  }))
  average_coeffs_phtm_lasso <- colMeans(coeff_matrix_phtm_lasso, na.rm = TRUE)
  
  # Save averaged coefficients, rename columns with a "b_" prefix
  coeff_df_phtm_lasso <- data.frame(t(average_coeffs_phtm_lasso))
  colnames(coeff_df_phtm_lasso) <- bcolnames
  coeff_df_phtm_lasso$country <- COUNTRY
  #View(coeff_df_phtm_lasso)
  
  # Save the final coefficients to a CSV file
  write.csv(coeff_df_phtm_lasso, file.path(OUTPUT, paste0("lfs/beta_", DBNAME, "_prob_phtm_lasso_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")), row.names = FALSE)
  
  ################################################################################
  # Top 10% estimation conditional on Non-HtM
  if (top10Index == 1) {
    results_top10_lasso <- list()
    model_formula <- model_formula_top10
    predictors <- predictors_top10
    bcolnames <- bcolnames_top10
    
    # Loop over each imputation
    for (imp in implicates) {
      df_imp <- df_top10 %>%
        filter(sa0100 == COUNTRY, im0100 == imp)
      
      if (nrow(df_imp) > 0) {
        x <- model.matrix(model_formula, data = df_imp)
        y <- df_imp$statushtm
        
        if (imputationWeighted == 1) {
          cvfit <- cv.glmnet(x, y, weights = df_imp$hw0010, family = "binomial", alpha = 1)
          best_lambda <- cvfit$lambda.min
          
          lasso_model_top10 <- glmnet(x, y, weights = df_imp$hw0010, family = "binomial", alpha = 1, lambda = best_lambda)
        } else {
          cvfit <- cv.glmnet(x, y, family = "binomial", alpha = 1)
          best_lambda <- cvfit$lambda.min
          
          lasso_model_top10 <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)
        }
        
        #pred_probs <- predict(lasso_model_top10, newx = x, type = "response")
        #logL_model <- sum(y * log(pred_probs) + (1 - y) * log(1 - pred_probs))
        #null_prob <- mean(y)
        #logL_null <- sum(y * log(null_prob) + (1 - y) * log(1 - null_prob))
        #pseudo_R2 <- 1 - (logL_model / logL_null)
        #cat("Imputation", imp, "- McFadden's pseudo-R²:", pseudo_R2, "\n")
        
        lasso_coeffs <- as.numeric(coef(lasso_model_top10))
        names(lasso_coeffs) <- rownames(coef(lasso_model_top10))
        lasso_coeffs <- lasso_coeffs[predictors]
        results_top10_lasso[[paste0("imp", imp)]] <- lasso_coeffs
      }
    }
    
    # Results to a matrix w row for each imputation, average coeffs across imputations
    coeff_matrix_top10_lasso <- do.call(rbind, lapply(implicates, function(imp) {
      results_top10_lasso[[paste0("imp", imp)]]
    }))
    average_coeffs_top10_lasso <- colMeans(coeff_matrix_top10_lasso, na.rm = TRUE)
    
    # Save averaged coefficients, rename columns with a "b_" prefix
    coeff_df_top10_lasso <- data.frame(t(average_coeffs_top10_lasso))
    colnames(coeff_df_top10_lasso) <- bcolnames
    coeff_df_top10_lasso$country <- COUNTRY
    #View(coeff_df_top10_lasso)
    
    # Save the final coefficients to a CSV file
    write.csv(coeff_df_top10_lasso, file.path(OUTPUT, paste0("lfs/beta_", DBNAME, "_prob_top10_lasso_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")), row.names = FALSE)
  }
}

################################################################################
# BMA
if (runBMA == 1) {
  ################################################################################
  # HtM estimation
  results_htm_bma <- list()
  model_formula <- model_formula_htm
  predictors <- predictors_htm
  bcolnames <- bcolnames_htm
  
  # Loop over each imputation
  for (imp in implicates) {
    df_imp <- df_htm %>%
      filter(sa0100 == COUNTRY, im0100 == imp)
    
    if (nrow(df_imp) > 0) {
      if (imputationWeighted == 1) {
        bma_model_htm <- bas.glm(
          formula = model_formula, 
          data = df_imp,
          weights = df_imp$hw0010,
          family = binomial(link = "logit"), # no probit link available
          method = "BAS",
          MCMC.iterations = 1000
        )
      } else {
        bma_model_htm <- bas.glm(
          formula = model_formula, 
          data = df_imp,
          family = binomial(link = "logit"), # no probit link available
          method = "BAS",
          MCMC.iterations = 1000
        )
      }
      
      bma_coef_obj <- coef(bma_model_htm, order.by.pip = TRUE)
      bma_coef_vec <- bma_coef_obj$postmean
      names(bma_coef_vec) <- bma_coef_obj$namesx
      matched_coefs <- sapply(predictors, function(pred) {
        if (pred %in% names(bma_coef_vec)) {
          bma_coef_vec[pred]
        } else {
          0
        }
      })
      names(matched_coefs) <- predictors
      results_htm_bma[[paste0("imp", imp)]] <- matched_coefs
    }
  }
  
  # Results to a matrix w row for each imputation, average coeffs across imputations
  coeff_matrix_htm_bma <- do.call(rbind, lapply(implicates, function(imp) {
    results_htm_bma[[paste0("imp", imp)]]
  }))
  average_coeffs_htm_bma <- colMeans(coeff_matrix_htm_bma, na.rm = TRUE)
  
  # Save averaged coefficients, rename columns with a "b_" prefix
  coeff_df_htm_bma <- data.frame(t(average_coeffs_htm_bma))
  colnames(coeff_df_htm_bma) <- bcolnames
  coeff_df_htm_bma$country <- COUNTRY
  #View(coeff_df_htm_bma)
  
  # Save the final coefficients to a CSV file
  write.csv(coeff_df_htm_bma, file.path(OUTPUT, paste0("lfs/beta_", DBNAME, "_prob_htm_bma_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")), row.names = FALSE)

  ################################################################################
  # Poor HtM estimation conditional on HtM
  results_phtm_bma <- list()
  model_formula <- model_formula_phtm
  predictors <- predictors_phtm
  bcolnames <- bcolnames_phtm
  
  # Loop over each imputation
  for (imp in implicates) {
    df_imp <- df_phtm %>%
      filter(sa0100 == COUNTRY, im0100 == imp)
    
    if (nrow(df_imp) > 0) {
      if (imputationWeighted == 1) {
        bma_model_phtm <- bas.glm(
          formula = model_formula, 
          data = df_imp,
          weights = df_imp$hw0010,
          family = binomial(link = "logit"), # no probit link available
          method = "BAS",
          MCMC.iterations = 1000
        )
      } else {
        bma_model_phtm <- bas.glm(
          formula = model_formula, 
          data = df_imp,
          family = binomial(link = "logit"), # no probit link available
          method = "BAS",
          MCMC.iterations = 1000
        )
      }
      
      bma_coef_obj <- coef(bma_model_phtm, order.by.pip = TRUE)
      bma_coef_vec <- bma_coef_obj$postmean
      names(bma_coef_vec) <- bma_coef_obj$namesx
      matched_coefs <- sapply(predictors, function(pred) {
        if (pred %in% names(bma_coef_vec)) {
          bma_coef_vec[pred]
        } else {
          0
        }
      })
      names(matched_coefs) <- predictors
      results_phtm_bma[[paste0("imp", imp)]] <- matched_coefs
    }
  }
  
  # Results to a matrix w row for each imputation, average coeffs across imputations
  coeff_matrix_phtm_bma <- do.call(rbind, lapply(implicates, function(imp) {
    results_phtm_bma[[paste0("imp", imp)]]
  }))
  average_coeffs_phtm_bma <- colMeans(coeff_matrix_phtm_bma, na.rm = TRUE)
  
  # Save averaged coefficients, rename columns with a "b_" prefix
  coeff_df_phtm_bma <- data.frame(t(average_coeffs_phtm_bma))
  colnames(coeff_df_phtm_bma) <- bcolnames
  coeff_df_phtm_bma$country <- COUNTRY
  #View(coeff_df_phtm_bma)
  
  # Save the final coefficients to a CSV file
  write.csv(coeff_df_phtm_bma, file.path(OUTPUT, paste0("lfs/beta_", DBNAME, "_prob_phtm_bma_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")), row.names = FALSE)

  ################################################################################
  # Top 10% estimation conditional on Non-HtM
  if (top10Index == 1) {
    results_top10_bma <- list()
    model_formula <- model_formula_top10
    predictors <- predictors_top10
    bcolnames <- bcolnames_top10
    
    # Loop over each imputation
    for (imp in implicates) {
      df_imp <- df_top10 %>%
        filter(sa0100 == COUNTRY, im0100 == imp)
      
      if (nrow(df_imp) > 0) {
        if (imputationWeighted == 1) {
          bma_model_top10 <- bas.glm(
            formula = model_formula, 
            data = df_imp,
            weights = df_imp$hw0010,
            family = binomial(link = "logit"), # no probit link available
            method = "BAS",
            MCMC.iterations = 1000
          )
        } else {
          bma_model_top10 <- bas.glm(
            formula = model_formula, 
            data = df_imp,
            family = binomial(link = "logit"), # no probit link available
            method = "BAS",
            MCMC.iterations = 1000
          )
        }
        
        bma_coef_obj <- coef(bma_model_top10, order.by.pip = TRUE)
        bma_coef_vec <- bma_coef_obj$postmean
        names(bma_coef_vec) <- bma_coef_obj$namesx
        matched_coefs <- sapply(predictors, function(pred) {
          if (pred %in% names(bma_coef_vec)) {
            bma_coef_vec[pred]
          } else {
            0
          }
        })
        names(matched_coefs) <- predictors
        results_top10_bma[[paste0("imp", imp)]] <- matched_coefs
      }
    }
    
    # Results to a matrix w row for each imputation, average coeffs across imputations
    coeff_matrix_top10_bma <- do.call(rbind, lapply(implicates, function(imp) {
      results_top10_bma[[paste0("imp", imp)]]
    }))
    average_coeffs_top10_bma <- colMeans(coeff_matrix_top10_bma, na.rm = TRUE)
    
    # Save averaged coefficients, rename columns with a "b_" prefix
    coeff_df_top10_bma <- data.frame(t(average_coeffs_top10_bma))
    colnames(coeff_df_top10_bma) <- bcolnames
    coeff_df_top10_bma$country <- COUNTRY
    #View(coeff_df_top10_bma)
    
    # Save the final coefficients to a CSV file
    write.csv(coeff_df_top10_bma, file.path(OUTPUT, paste0("lfs/beta_", DBNAME, "_prob_top10_bma_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")), row.names = FALSE)
  }
}

################################################################################
# BMAhg
if (runBMAhg == 1) {
  ################################################################################
  # HtM estimation
  results_htm_bmahg <- list()
  model_formula <- model_formula_htm
  predictors <- predictors_htm
  bcolnames <- bcolnames_htm
  
  # Loop over each imputation
  for (imp in implicates) {
    df_imp <- df_htm %>%
      filter(sa0100 == COUNTRY, im0100 == imp)
    
    if (nrow(df_imp) > 0) {
      if (imputationWeighted == 1) {
        bmahg_model_htm <- bas.glm(
          formula = model_formula, 
          data = df_imp,
          weights = df_imp$hw0010,
          family = binomial(link = "logit"), # no probit link available
          betaprior = hyper.g(alpha = 3),
          MCMC.iterations = 1000
        )
      } else {
        bmahg_model_htm <- bas.glm(
          formula = model_formula, 
          data = df_imp,
          family = binomial(link = "logit"), # no probit link available
          betaprior = hyper.g(alpha = 3),
          MCMC.iterations = 1000
        )
      }
      
      bmahg_coef_obj <- coef(bmahg_model_htm, order.by.pip = TRUE)
      bmahg_coef_vec <- bmahg_coef_obj$postmean
      names(bmahg_coef_vec) <- bmahg_coef_obj$namesx
      matched_coefs <- sapply(predictors, function(pred) {
        if (pred %in% names(bmahg_coef_vec)) {
          bmahg_coef_vec[pred]
        } else {
          0
        }
      })
      names(matched_coefs) <- predictors
      results_htm_bmahg[[paste0("imp", imp)]] <- matched_coefs
    }
  }
  
  # Results to a matrix w row for each imputation, average coeffs across imputations
  coeff_matrix_htm_bmahg <- do.call(rbind, lapply(implicates, function(imp) {
    results_htm_bmahg[[paste0("imp", imp)]]
  }))
  average_coeffs_htm_bmahg <- colMeans(coeff_matrix_htm_bmahg, na.rm = TRUE)
  
  # Save averaged coefficients, rename columns with a "b_" prefix
  coeff_df_htm_bmahg <- data.frame(t(average_coeffs_htm_bmahg))
  colnames(coeff_df_htm_bmahg) <- bcolnames
  coeff_df_htm_bmahg$country <- COUNTRY
  #View(coeff_df_htm_bmahg)
  
  # Save the final coefficients to a CSV file
  write.csv(coeff_df_htm_bmahg, file.path(OUTPUT, paste0("lfs/beta_", DBNAME, "_prob_htm_bmahg_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")), row.names = FALSE)
  
  ################################################################################
  # Poor HtM estimation conditional on HtM
  results_phtm_bmahg <- list()
  model_formula <- model_formula_phtm
  predictors <- predictors_phtm
  bcolnames <- bcolnames_phtm
  
  # Loop over each imputation
  for (imp in implicates) {
    df_imp <- df_phtm %>%
      filter(sa0100 == COUNTRY, im0100 == imp)
    
    if (nrow(df_imp) > 0) {
      if (imputationWeighted == 1) {
        bmahg_model_phtm <- bas.glm(
          formula = model_formula, 
          data = df_imp,
          weights = df_imp$hw0010,
          family = binomial(link = "logit"), # no probit link available
          betaprior = hyper.g(alpha = 3),
          MCMC.iterations = 1000
        )
      } else {
        bmahg_model_phtm <- bas.glm(
          formula = model_formula, 
          data = df_imp,
          family = binomial(link = "logit"), # no probit link available
          betaprior = hyper.g(alpha = 3),
          MCMC.iterations = 1000
        )
      }
      
      bmahg_coef_obj <- coef(bmahg_model_phtm, order.by.pip = TRUE)
      bmahg_coef_vec <- bmahg_coef_obj$postmean
      names(bmahg_coef_vec) <- bmahg_coef_obj$namesx
      matched_coefs <- sapply(predictors, function(pred) {
        if (pred %in% names(bmahg_coef_vec)) {
          bmahg_coef_vec[pred]
        } else {
          0
        }
      })
      names(matched_coefs) <- predictors
      results_phtm_bmahg[[paste0("imp", imp)]] <- matched_coefs
    }
  }
  
  # Results to a matrix w row for each imputation, average coeffs across imputations
  coeff_matrix_phtm_bmahg <- do.call(rbind, lapply(implicates, function(imp) {
    results_phtm_bmahg[[paste0("imp", imp)]]
  }))
  average_coeffs_phtm_bmahg <- colMeans(coeff_matrix_phtm_bmahg, na.rm = TRUE)
  
  # Save averaged coefficients, rename columns with a "b_" prefix
  coeff_df_phtm_bmahg <- data.frame(t(average_coeffs_phtm_bmahg))
  colnames(coeff_df_phtm_bmahg) <- bcolnames
  coeff_df_phtm_bmahg$country <- COUNTRY
  #View(coeff_df_phtm_bmahg)
  
  # Save the final coefficients to a CSV file
  write.csv(coeff_df_phtm_bmahg, file.path(OUTPUT, paste0("lfs/beta_", DBNAME, "_prob_phtm_bmahg_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")), row.names = FALSE)
  
  ################################################################################
  # Top 10% estimation conditional on Non-HtM
  if (top10Index == 1) {
    results_top10_bmahg <- list()
    model_formula <- model_formula_top10
    predictors <- predictors_top10
    bcolnames <- bcolnames_top10
    
    # Loop over each imputation
    for (imp in implicates) {
      df_imp <- df_top10 %>%
        filter(sa0100 == COUNTRY, im0100 == imp)
      
      if (nrow(df_imp) > 0) {
        if (imputationWeighted == 1) {
          bmahg_model_top10 <- bas.glm(
            formula = model_formula, 
            data = df_imp,
            weights = df_imp$hw0010,
            family = binomial(link = "logit"), # no probit link available
            betaprior = hyper.g(alpha = 3),
            MCMC.iterations = 1000
          )
        } else {
          bmahg_model_top10 <- bas.glm(
            formula = model_formula, 
            data = df_imp,
            family = binomial(link = "logit"), # no probit link available
            betaprior = hyper.g(alpha = 3),
            MCMC.iterations = 1000
          )
        }
        
        bmahg_coef_obj <- coef(bmahg_model_top10, order.by.pip = TRUE)
        bmahg_coef_vec <- bmahg_coef_obj$postmean
        names(bmahg_coef_vec) <- bmahg_coef_obj$namesx
        matched_coefs <- sapply(predictors, function(pred) {
          if (pred %in% names(bmahg_coef_vec)) {
            bmahg_coef_vec[pred]
          } else {
            0
          }
        })
        names(matched_coefs) <- predictors
        results_top10_bmahg[[paste0("imp", imp)]] <- matched_coefs
      }
    }
    
    # Results to a matrix w row for each imputation, average coeffs across imputations
    coeff_matrix_top10_bmahg <- do.call(rbind, lapply(implicates, function(imp) {
      results_top10_bmahg[[paste0("imp", imp)]]
    }))
    average_coeffs_top10_bmahg <- colMeans(coeff_matrix_top10_bmahg, na.rm = TRUE)
    
    # Save averaged coefficients, rename columns with a "b_" prefix
    coeff_df_top10_bmahg <- data.frame(t(average_coeffs_top10_bmahg))
    colnames(coeff_df_top10_bmahg) <- bcolnames
    coeff_df_top10_bmahg$country <- COUNTRY
    #View(coeff_df_top10_bmahg)
    
    # Save the final coefficients to a CSV file
    write.csv(coeff_df_top10_bmahg, file.path(OUTPUT, paste0("lfs/beta_", DBNAME, "_prob_top10_bmahg_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")), row.names = FALSE)
  }
}

################################################################################
# PROBIT
if (runProbit == 1) {
  ################################################################################
  # HtM estimation
  results_htm <- list()
  model_formula <- model_formula_htm
  predictors <- predictors_htm
  bcolnames <- bcolnames_htm
  
  for (imp in implicates) {
    df_imp <- df_htm %>%
      filter(sa0100 == COUNTRY, im0100 == imp)
    
    if (nrow(df_imp) > 0) {
      # Probit regression
      if (imputationWeighted == 1) {
        probit_model_htm <- glm(model_formula, data = df_imp, weights = df_imp$hw0010, family = binomial(link = "probit"))
      } else {
        probit_model_htm <- glm(model_formula, data = df_imp, family = binomial(link = "probit"))
      }
      #print(summary(probit_model_htm))
      #pR2(probit_model_htm)
      
      # Extract coefficients and p-values
      coeffs <- coef(probit_model_htm)
      
      # Additional step to control for statistical significance of the estimates
      #p_values <- summary(probit_model_htm)$coefficients[, "Pr(>|z|)"]
      #coeffs <- coeffs[predictors]
      #p_values <- p_values[predictors]
      #coeffs <- ifelse(!is.na(p_values) & p_values <= 0.10, coeffs, NA)
      
      # Save coefficients
      results_htm[[paste0("imp", imp)]] <- coeffs
    }
  }
  
  # Average coefficients across implicates
  coeff_matrix_htm <- do.call(rbind, lapply(implicates, function(imp) {
    as.numeric(results_htm[[paste0("imp", imp)]])
  }))
  average_coeffs_htm <- colMeans(coeff_matrix_htm, na.rm = TRUE)
  
  # Save coefficients in a data frame
  coeff_df_htm <- data.frame(t(average_coeffs_htm))
  colnames(coeff_df_htm) <- bcolnames
  coeff_df_htm$country <- COUNTRY
  
  # Save to CSV
  write.csv(coeff_df_htm, file.path(OUTPUT, paste0("lfs/beta_", DBNAME, "_prob_htm_probit_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")), row.names = FALSE)
  
  ################################################################################
  # Poor HtM estimation conditional on HtM
  results_phtm <- list()
  model_formula <- model_formula_phtm
  predictors <- predictors_phtm
  bcolnames <- bcolnames_phtm
  
  for (imp in implicates) {
    df_imp <- df_phtm %>%
      filter(sa0100 == COUNTRY, im0100 == imp)
    
    if (nrow(df_imp) > 0) {
      if (imputationWeighted == 1) {
        probit_model_phtm <- glm(model_formula, data = df_imp, weights = df_imp$hw0010, family = binomial(link = "probit"))
      } else {
        probit_model_phtm <- glm(model_formula, data = df_imp, family = binomial(link = "probit"))
      }
      #print(summary(probit_model_phtm))
      #pR2(probit_model_phtm)
      
      coeffs <- coef(probit_model_phtm)
      
      #p_values <- summary(probit_model_phtm)$coefficients[, "Pr(>|z|)"]
      #coeffs <- coeffs[predictors]
      #p_values <- p_values[predictors]
      #coeffs <- ifelse(!is.na(p_values) & p_values <= 0.10, coeffs, NA)
      
      results_phtm[[paste0("imp", imp)]] <- coeffs
    }
  }
  
  coeff_matrix_phtm <- do.call(rbind, lapply(implicates, function(imp) {
    as.numeric(results_phtm[[paste0("imp", imp)]])
  }))
  average_coeffs_phtm <- colMeans(coeff_matrix_phtm, na.rm = TRUE)
  
  coeff_df_phtm <- data.frame(t(average_coeffs_phtm))
  colnames(coeff_df_phtm) <- bcolnames
  coeff_df_phtm$country <- COUNTRY
  
  # Save to CSV
  write.csv(coeff_df_phtm, file.path(OUTPUT, paste0("lfs/beta_", DBNAME, "_prob_phtm_probit_", imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")), row.names = FALSE)
  
  ################################################################################
  # Top 10% estimation conditional on Non-HtM
  if (top10Index == 1) {
    results_top10 <- list()
    model_formula <- model_formula_top10
    predictors <- predictors_top10
    bcolnames <- bcolnames_top10
    
    for (imp in implicates) {
      df_imp <- df_top10 %>%
        filter(sa0100 == COUNTRY, im0100 == imp)
      
      if (nrow(df_imp) > 0) {
        if (imputationWeighted == 1) {
          probit_model_top10 <- glm(model_formula, data = df_imp, weights = df_imp$hw0010, family = binomial(link = "probit"))
        } else {
          probit_model_top10 <- glm(model_formula, data = df_imp, family = binomial(link = "probit"))
        }
        #print(summary(probit_model_top10))
        #pR2(probit_model_top10)
        
        coeffs <- coef(probit_model_top10)
        
        #p_values <- summary(probit_model_top10)$coefficients[, "Pr(>|z|)"]
        #coeffs <- coeffs[predictors]
        #p_values <- p_values[predictors]
        #coeffs <- ifelse(!is.na(p_values) & p_values <= 0.10, coeffs, NA)
        
        results_top10[[paste0("imp", imp)]] <- coeffs
      }
    }
    
    coeff_matrix_top10 <- do.call(rbind, lapply(implicates, function(imp) {
      as.numeric(results_top10[[paste0("imp", imp)]])
    }))
    average_coeffs_top10 <- colMeans(coeff_matrix_top10, na.rm = TRUE)
    
    coeff_df_top10 <- data.frame(t(average_coeffs_top10))
    colnames(coeff_df_top10) <- bcolnames
    coeff_df_top10$country <- COUNTRY
    
    # Save to CSV
    write.csv(coeff_df_top10, file.path(OUTPUT, paste0("lfs/beta_", DBNAME, "_prob_top10_probit_",  imputationModelChoice, ifelse(imputationWeighted == 1, "wt", "uwt"), "_ageGrp", ageGrpMethod, ".csv")), row.names = FALSE)
  }
}

################################################################################
# Create table for model comparison
# Define the output file name for the model comparison table
comparison_file <- file.path(TABLES, paste0(DBNAME, "_modelcomp",  ".csv"))

# If the file exists, load it; otherwise, create an empty data frame.
if (file.exists(comparison_file)) {
  model_comparison <- read.csv(comparison_file, stringsAsFactors = FALSE)
} else {
  model_comparison <- data.frame(
    Outcome = character(),         # e.g., "HtM", "pHtM", "top10"
    VariableSet = integer(),       # imputationModelChoice (1,2,3,4)
    ModelType = character(),       # "BMA", "BMAhg", "LASSO", "Probit"
    Weighted = character(),        # wt (1), uwt (0)
    ageGrouped = character(),      # standard (1), grouped (0)
    LogLikelihood = numeric(),
    AUC = numeric(),
    Brier = numeric(),
    HL_Xsq = numeric(),
    HL_df = numeric(),
    HL_p = numeric(),
    Pseudo_R2 = numeric(),
    WAIC = numeric(),
    LOOIC = numeric(),
    stringsAsFactors = FALSE
  )
}

# Define the outcomes to loop over. If top10Index==1 include "top10"
outcome_list <- c("htm", "phtm")
if (top10Index == 1) outcome_list <- c(outcome_list, "top10")

# Define the model types to compare based on your switches:
model_types <- c()
if (runBMA == 1) { model_types <- c(model_types, "BMA") }
if (runBMAhg == 1) { model_types <- c(model_types, "BMAhg") }
if (runLASSO == 1) { model_types <- c(model_types, "LASSO") }
if (runProbit == 1) { model_types <- c(model_types, "Probit") }

# Loop over each outcome and each model type
for (outcome in outcome_list) {
  
  # Select the appropriate data and model objects
  if (outcome == "htm") {
    data_model <- df_htm
    if ("BMA" %in% model_types)   model_BMA   <- bma_model_htm
    if ("BMAhg" %in% model_types) model_BMAhg <- bmahg_model_htm
    if ("LASSO" %in% model_types) model_LASSO <- lasso_model_htm
    if ("Probit" %in% model_types) model_Probit<- probit_model_htm
  } else if (outcome == "phtm") {
    data_model <- df_phtm
    if ("BMA" %in% model_types)   model_BMA   <- bma_model_phtm
    if ("BMAhg" %in% model_types) model_BMAhg <- bmahg_model_phtm
    if ("LASSO" %in% model_types) model_LASSO <- lasso_model_phtm
    if ("Probit" %in% model_types) model_Probit<- probit_model_phtm
  } else if (outcome == "top10") {
    data_model <- df_top10
    if ("BMA" %in% model_types)   model_BMA   <- bma_model_top10
    if ("BMAhg" %in% model_types) model_BMAhg <- bmahg_model_top10
    if ("LASSO" %in% model_types) model_LASSO <- lasso_model_top10
    if ("Probit" %in% model_types) model_Probit<- probit_model_top10
  }
  
  for (modtype in model_types) {
    # Extract predicted probabilities depending on model type.
    if (modtype == "BMA") {
      # Use HPM estimator for BMA to reduce memory usage.
      pred_obj <- predict(model_BMA, newdata = data_model, estimator = "HPM", type = "response")
      # Predicted probabilities are stored in the "fit" element:
      pred_probs <- as.numeric(pred_obj$fit)
      loglik_matrix <- NULL  # Replace with extraction code if available.
    } else if (modtype == "BMAhg") {
        pred_obj <- predict(model_BMAhg, newdata = data_model, estimator = "HPM", type = "response")
        pred_probs <- as.numeric(pred_obj$fit)
        loglik_matrix <- NULL
    } else if (modtype == "LASSO") {
      x_mat <- model.matrix(get(paste0("model_formula_", outcome)), data = data_model)
      pred_probs <- as.numeric(predict(model_LASSO, newx = x_mat, type = "response"))
      loglik_matrix <- NULL
    } else if (modtype == "Probit") {
      pred_probs <- as.numeric(predict(model_Probit, newdata = data_model, type = "response"))
      loglik_matrix <- NULL
    }
    
    # Ensure the response is numeric 0/1. (Assume statushtm has been recoded accordingly.)
    y_val <- as.numeric(as.character(data_model$statushtm))
    # Remove any NA values
    valid_idx <- !is.na(pred_probs) & !is.na(y_val)
    pred_probs <- pred_probs[valid_idx]
    y_val <- y_val[valid_idx]
    
    # If there are no valid observations, skip this model
    if (length(y_val) == 0) next
    
    # 1. Log Likelihood and Pseudo-R2 (McFadden)
    logL_model <- sum(y_val * log(pred_probs) + (1 - y_val) * log(1 - pred_probs))
    null_prob <- mean(y_val)
    logL_null <- sum(y_val * log(null_prob) + (1 - y_val) * log(1 - null_prob))
    pseudo_R2 <- 1 - (logL_model / logL_null)
    
    # 2. AUC
    # Ensure that y is a factor with levels "0" and "1"
    y_factor <- factor(y_val, levels = c(0, 1))
    roc_obj <- roc(response = y_factor, predictor = pred_probs, levels = c("0", "1"))
    auc_val <- as.numeric(auc(roc_obj))
    
    # 3. Brier Score
    brier_score <- mean((pred_probs - y_val)^2)
    
    # 4. Hosmer–Lemeshow Test with error handling
    hl <- tryCatch({
      hoslem.test(x = y_val, y = pred_probs, g = 10)
    }, error = function(e) {
      message("Hosmer–Lemeshow test failed: ", e$message)
      return(NULL)
    })
    if (is.null(hl)) {
      HL_Xsq <- NA
      HL_df <- NA
      HL_p <- NA
    } else {
      HL_Xsq <- as.numeric(hl$statistic)
      HL_df <- as.numeric(hl$parameter)
      HL_p <- hl$p.value
    }
    
    # 5. WAIC and LOOIC
    # Otherwise, we approximate these metrics using the point predictions only,
    # which does not capture posterior uncertainty. Here we provide an approximation:
    if (is.null(loglik_matrix)) {
      # Compute pointwise log-likelihoods:
      pointwise_loglik <- y_val * log(pred_probs) + (1 - y_val) * log(1 - pred_probs)
      # Here, variance is zero because we have only one estimate per observation.
      # Thus, p_waic = 0 and:
      waic_val <- -2 * sum(pointwise_loglik)
      loo_val <- waic_val
    } else {
      # If loglik_matrix is available, compute WAIC and LOO properly:
      waic_result <- tryCatch({ waic(loglik_matrix) }, error = function(e) NA)
      loo_result <- tryCatch({ loo(loglik_matrix) }, error = function(e) NA)
      waic_val <- if (!is.na(waic_result)) waic_result$waic else NA
      loo_val <- if (!is.na(loo_result)) loo_result$looic else NA
    }
    
    # Create a new row with all the metrics
    new_row <- data.frame(
      Outcome = outcome,
      VariableSet = imputationModelChoice,
      ModelType = modtype,
      Weighted = imputationWeighted,
      ageGrouped = ageGrpMethod,
      LogLikelihood = logL_model,
      AUC = auc_val,
      Brier = brier_score,
      HL_Xsq = HL_Xsq,
      HL_df = HL_df,
      HL_p = HL_p,
      Pseudo_R2 = pseudo_R2,
      WAIC = waic_val,
      LOOIC = loo_val,
      stringsAsFactors = FALSE
    )
    
    # Append new_row to the model comparison table
    model_comparison <- rbind(model_comparison, new_row)
  } # end for modtype
} # end for outcome

# Arrange, save and print the model comparison table
model_comparison <- model_comparison %>%
  arrange(Outcome, VariableSet, ModelType, Weighted)
write.csv(model_comparison, comparison_file, row.names = FALSE)
print(model_comparison)
#View(model_comparison)

################################################################################
# Create plots for fit analysis of BMA models 
analyseBMAfitPlots <- 0
if (analyseBMAfitPlots == 1) {
  
  image(bma_model_htm, rotate = F)
  coefz <- coef(bma_model_htm)
  plot(bma_model_htm, ask = F)[1]
  plot(coefz, subset = c(5:6), ask = F)
  plot(confint(coef(bma_model_htm, estimator = "HPM")))
  plot(confint(coef(bma_model_htm)))
  summary(bma_model_htm)
  
  #pred_probs <- predict(bmahg_model_htm, newdata = df_htm, estimator = "BMA", type = "response")
  pred_probs <- predict(bma_model_htm, newdata = df_htm, estimator = "HPM", type = "response")
  pred_probs_numeric <- as.numeric(pred_probs$fit)
  y <- factor(df_phtm$statushtm, levels = c(0, 1))
  y_num <- as.numeric(as.character(y))  # if y is a factor with levels "0" and "1"
  
  roc_obj <- roc(response = y, predictor = pred_probs_numeric, levels = c("0", "1"))
  auc_val <- auc(roc_obj)
  cat("AUC:", auc_val, "\n") # above 0.5 is better than random, 0.8-0.9 is the excellence threshold
  plot(roc_obj, main = "ROC Curve for BMA Model")
  
  cal_df <- data.frame(y = y_num, p = pred_probs_numeric)
  cal_df <- cal_df %>% filter(!is.na(p))
  cal_df <- cal_df %>%
    mutate(prob_bin = ntile(p, 10)) %>%
    group_by(prob_bin) %>%
    summarise(
      avg_pred = mean(p),
      obs_freq = mean(y)
    )
  plot(cal_df$avg_pred, cal_df$obs_freq,
       xlab = "Average Predicted Probability (Decile)", 
       ylab = "Observed Frequency (Decile)",
       main = "Calibration Plot")
  abline(0, 1, col = "red", lty = 2)  # reference line for perfect calibration
  
  plot(pred_probs_numeric, y_num, main="Predicted Probability vs Actual", xlab="Predicted", ylab="Observed")
  abline(h=0.5, col="red", lty=2)
}