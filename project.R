# load relevant packages
library(funModeling) # useful for counting NAs
library(tidyverse)
library(MASS) # determine AIC
library(mice) # single imputation
library(car) # get VIF values to check for co-linearity
library(gtsummary) # create summary tables
library(cardx) # add p-values to tables automatically

#######################
## Declare Functions ##
#######################

#' having functions at the top of the file is important because they will be
#' available to use in the environment if the file is sourced (runs from top
#' to bottom)

#' function to determine the max number of degrees of freedom in the predictor
#' for a given response variable
max.predictors <- function(response_var, dataframe) {
  if(is.numeric(dataframe[[response_var]])) {
    
    # the variable is numeric
    tmp <- dataframe %>%
      summarise(n_observations = n())
    
    return(floor(tmp$n_observations/15))
      
  } else {
    
    # the variable is logical
    tmp <- dataframe %>% 
      summarise(
        true_count = sum(get(response_var)),
        false_count = sum(!get(response_var))
      )
    
    return(floor(min(tmp$true_count, tmp$false_count) / 15))
  
  }
}

###############
## Data Prep ##
###############

# import data into data frame
d <- read.csv("ds_project_data.csv")

# select relevant columns, keep all rows
d <- d[, c("Pittsburgh.Sleep.Quality.Index.Score",
           "Epworth.Sleepiness.Scale",
           "Berlin.Sleepiness.Scale",
           "Athens.Insomnia.Scale",
           "SF36.PCS",
           "SF36.MCS",
           "Age",
           "Gender",
           "BMI",
           "Time.from.transplant",
           "Liver.Diagnosis",
           "Recurrence.of.disease",
           "Rejection.graft.dysfunction",
           "Any.fibrosis",
           "Renal.Failure",
           "Depression",
           "Corticoid")]

# rename columns with long names for ease of coding
colnames(d)[1:4] <- c("PSQI", "ESS", "BSS", "AIS")

# check for duplicates, there are none
any(duplicated(d))

# check for any variables with high rate of missingness
status(d)

#' PSQI has ~32% missing data! This is above our chosen threshold of 30%
#' We will exclude PSQI from analysis.

# formatting columns as appropriate
d$Gender <- ifelse(d$Gender == 1, "Male", "Female")
d$Gender <- as.factor(d$Gender)
levels(d$Gender)

d$Liver.Diagnosis <- as.factor(d$Liver.Diagnosis)
levels(d$Liver.Diagnosis)

logical_columns <- c("BSS","Recurrence.of.disease", "Rejection.graft.dysfunction", "Any.fibrosis", "Renal.Failure", "Corticoid", "Depression")
d[, logical_columns] <- as.logical(c(d$BSS, d$Recurrence.of.disease, d$Rejection.graft.dysfunction, d$Any.fibrosis, d$Renal.Failure, d$Corticoid, d$Depression))

# remove invalid data and replace with NA
# ESS is score on a scale from 0-24, cannot have values higher than 24
sum(d$ESS > 24, na.rm = T)
d$ESS[d$ESS > 24] <- NA

# data for other scales is within expected ranges

###############################
## Exploratory Data Analysis ##
###############################

# create table with summary of baseline characteristics
d %>% 
  select(-PSQI, -ESS, -AIS, -SF36.MCS, -SF36.PCS, -BSS) %>%
  mutate(Liver.Diagnosis = ifelse(Liver.Diagnosis == 1, "Hep C",
                             ifelse(Liver.Diagnosis == 2, "Hep B",
                                ifelse(Liver.Diagnosis == 3, "PSC/PBC/AHA",
                                  ifelse(Liver.Diagnosis == 4, "Alcohol", "Other"))))) %>% 
  tbl_summary(
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"),
    label = list(
      Age ~ "Age (years)",
      Gender ~ "Gender",
      BMI ~ "BMI",
      Time.from.transplant ~ "Time Since Transplant (years)",
      Liver.Diagnosis ~ "Liver Diagnosis",
      Recurrence.of.disease ~ "Disease Recurrence",
      Rejection.graft.dysfunction ~ "Rejection Graft Dysfunction",
      Any.fibrosis ~ "Fibrosis",
      Renal.Failure ~ "Renal Failure",
      Depression ~ "Depression",
      Corticoid ~ "Using Corticosteroid")) %>%
  italicize_levels() %>% 
  add_n() %>% 
  bold_labels() %>% 
  modify_header(
    update = list(label ~ "Variable", stat_0 ~ "Summary Statistics", n ~ "Available Observations"))

#' create data frame with additional columns to group by scoring category to help
#' colour code the histograms that will be created next
score_buckets <- d %>% 
  mutate(ESS_group = ifelse(ESS < 11, "Normal Range",
                            ifelse(ESS < 16, "Mild to Moderate Daytime Sleepiness", "High Daytime Sleepiness"))) %>% 
  mutate(AIS_group = ifelse(AIS < 6, "Normal Sleep",
                            ifelse(AIS < 11, "Mild Insomnia", ifelse(AIS < 16, "Moderate Insomnia", "Severe Insomnia"))))

# set order for factors so that they are in order for figure legends
score_buckets$ESS_group <- factor(score_buckets$ESS_group, ordered = TRUE, levels = c(
  "Normal Range",
  "Mild to Moderate Daytime Sleepiness",
  "High Daytime Sleepiness"))

score_buckets$AIS_group <- factor(score_buckets$AIS_group, ordered = TRUE, levels = c(
  "Normal Sleep",
  "Mild Insomnia",
  "Moderate Insomnia",
  "Severe Insomnia"))

# create histogram for ESS score
ggplot(data = score_buckets , mapping = aes(x = ESS, fill = ESS_group)) +
  geom_histogram(binwidth = 1, width = 0.8, col = "black") +
  scale_x_continuous(limits = c(0,24)) +
  scale_fill_manual(values = c(
    "Normal Range" = "darkgreen",
    "Mild to Moderate Daytime Sleepiness" = "darkorange",
    "High Daytime Sleepiness" = "darkred")) +
  labs(
    x = "ESS Score",
    y = "Frequency",
    title = "Distribution of ESS Scores",
    fill = "ESS Interpretation") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  annotate(
    "label",
    x = Inf, y = Inf,
    label = paste("Mean ± SD\n", round(mean(score_buckets$ESS, na.rm = T), 2), "±", round(sd(score_buckets$ESS, na.rm = T), 2)),
    hjust = 1.1, vjust = 1.5,
    color = "black", size = 3.5,
    fill = "white", label.size = 0.3)

# create histogram for AIS score
ggplot(data = score_buckets, mapping = aes(x = AIS, fill = AIS_group)) +
  geom_histogram(binwidth = 1, width = 0.8, col = "black") +
  scale_x_continuous(limits = c(0,24)) +
  scale_fill_manual(values = c(
    "Normal Sleep" = "darkgreen",
    "Mild Insomnia" = "yellow",
    "Moderate Insomnia" = "darkorange",
    "Severe Insomnia" = "darkred")) +
  labs(
    x = "AIS Score",
    y = "Frequency",
    title = "Distribution of AIS Scores",
    fill = "AIS Interpretation") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  annotate(
    "label",
    x = Inf, y = Inf,
    label = paste("Mean ± SD\n", round(mean(score_buckets$AIS, na.rm = T), 2), "±", round(sd(score_buckets$AIS, na.rm = T), 2)),
    hjust = 1.1, vjust = 1.5,
    color = "black", size = 3.5,
    fill = "white", label.size = 0.3)

# create histogram for SF36.MCS scores
ggplot(data = score_buckets, mapping = aes(x = SF36.MCS)) +
  geom_histogram(binwidth = 1.5, width = 0.8, col = "black", fill = "lightgrey") +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    x = "SF36-MCS Score",
    y = "Frequency",
    title = "Distribution of SF36-MCS Scores") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_label(
    aes(x = 85, y = Inf, label = paste("Mean ± SD\n", round(mean(SF36.MCS, na.rm = T), 2), "±", round(sd(SF36.MCS, na.rm = T), 2))),
    vjust = 2, color = "black", size = 3.5)

# create histogram for SF36.PCS scores
ggplot(data = score_buckets, mapping = aes(x = SF36.PCS)) +
  geom_histogram(binwidth = 1.5, width = 0.8, col = "black", fill = "lightgrey") +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    x = "SF36-PCS Score",
    y = "Frequency",
    title = "Distribution of SF36-PCS Scores") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_label(
    aes(x = 85, y = Inf, label = paste("Mean ± SD\n", round(mean(SF36.PCS, na.rm = T), 2), "±", round(sd(SF36.PCS, na.rm = T), 2))),
    vjust = 2, color = "black", size = 3.5)

# create a bar graph to show the distribution of BSS scores
ggplot(data = d %>% filter(!is.na(BSS)), mapping = aes(x = BSS)) +
  geom_bar(fill = "lightgrey", col = "black", width = 0.6) +
  scale_y_continuous(limits = c(0, 175)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  scale_x_discrete(labels = c("TRUE" = "Sleep Disordered Breathing", "FALSE" = "No Sleep Disordered Breathing")) +
  labs(
    x = "BSS",
    y = "Frequency",
    title = "Distribution of BSS Score") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

###########################
## Imputing Missing Data ##
###########################

# set appropriate imputation method for each variable in the dataset 

#' "norm.nob" used for numerical variables, "logreg" used for binary variables.
#' Polyreg and polr methods are only mentioned because the vector must have a
#' length of 4. Additionally, no method will be used for variables
#' that have no missing values (no imputation necessary).
methods <- c("norm.nob", "logreg", "polyreg", "polr")

# create an imputed dataset with the methods specified above
d_imputed <- mice(d, defaultMethod = methods, print = F, seed = 7, m=1)

# check that correct method was used for each column
d_imputed$method

# extract the imputed dataset
d_complete <- complete(d_imputed, action = 1)

# check that no NAs remain
status(d_complete)

# correct imputed values that are outside of possible range for clinical scores
summary(d_complete)
d_complete$ESS[d_complete$ESS < 0] <- 0
d_complete$AIS[d_complete$AIS < 0] <- 0

#' it seems mice() converted BSS column from logical to numerical, so it should be
#' converted back to logical
d_complete$BSS <- as.logical(d_complete$BSS)

##############################
## Sleep Disturbance Models ##
##############################

library(broom.helpers)

############ ESS Model ################

# determine the max number of predictors for a model with ESS as a response variable
max.predictors("ESS", d_complete)

# create based on clinical literature
ess.mod <- lm(ESS ~ Gender + Depression + Age + BMI + Time.from.transplant, data = d_complete)
summary(ess.mod)

# test if adding graft dysfunction improves the model
ess.mod2 <- lm(ESS ~ Gender + Depression + Age + BMI + Time.from.transplant + Rejection.graft.dysfunction, data = d_complete)
summary(ess.mod2)

anova(ess.mod, ess.mod2)
AIC(ess.mod, ess.mod2)

deviance(ess.mod)
deviance(ess.mod2)

# complex model is better

# test for co-linearity
vif(ess.mod)
vif(ess.mod2)

# no values above 5, so there is no concern for co-linearity

# create summary tables for both models
ess.rt <- tbl_regression(ess.mod, exponentiate = F, intercept = TRUE,
  label = list(
    Time.from.transplant ~ "Time Since Transplant (years)")) %>% 
  italicize_levels() %>%
  bold_labels()

ess.rt2 <- tbl_regression(ess.mod2, exponentiate = F, intercept = TRUE,
  label = list(
    Rejection.graft.dysfunction ~ "Rejection Graft Dysfunction",
    Time.from.transplant ~ "Time Since Transplant (years)")) %>% 
  italicize_levels() %>%
  bold_labels()

# merge ESS tables
ess.rt.merged <- tbl_merge(
  tbls = list(ess.rt, ess.rt2),
  tab_spanner = c("**Model 1**", "**Model 2**"))

# view the merged table
ess.rt.merged

# check normality of residuals for both models
qqnorm(ess.mod$residuals, main = "Q-Q Plot of Residuals (ESS Model 1)")
qqline(ess.mod$residuals, col = "red")

qqnorm(ess.mod2$residuals, main = "Q-Q Plot of Residuals (ESS Model 2)")
qqline(ess.mod2$residuals, col = "red")

# check homoscedasticity
plot(ess.mod$fitted.values, ess.mod$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values (ESS Model 1)")
abline(h = 0, col = "red")

plot(ess.mod2$fitted.values, ess.mod2$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values (ESS Model 2)")
abline(h = 0, col = "red")

################ AIS Model ##################

# check max df in predictors for model with AIS as response variable
max.predictors("AIS", d_complete)

# use same predictors as the ESS model since daytime sleepiness and insomnia are correlated
ais.mod <- lm(AIS ~ Gender + Depression + Age + BMI + Time.from.transplant + Rejection.graft.dysfunction, data = d_complete)
summary(ais.mod)

# create a simpler model based on limited literature
ais.mod.simple <- lm(AIS ~ Gender + Depression, data = d_complete)
summary(ais.mod.simple)

# compare the two models with ANOVA
anova(ais.mod, ais.mod.simple)

# see AIC of the models
AIC(ais.mod, ais.mod.simple)

# see deviance of the models
deviance(ais.mod)
deviance(ais.mod.simple)

# check for co-linearity
vif(ais.mod)
vif(ais.mod.simple)

# no values above 5, so there is no concern for co-linearity

# create summary tables for both models
ais.rt <- tbl_regression(ais.mod, exponentiate = T, intercept = FALSE,
  label = list(
    Time.from.transplant ~ "Time Since Transplant (years)",
    Rejection.graft.dysfunction ~ "Rejection Graft Dysfunction")) %>% 
  italicize_levels() %>%
  bold_labels()

ais.rt.simple <- tbl_regression(ais.mod.simple, exponentiate = F, intercept = TRUE) %>% 
  italicize_levels() %>%
  bold_labels()

# merge AIS tables
ais.rt.merged <- tbl_merge(
  tbls = list(ais.rt.simple, ais.rt),
  tab_spanner = c("**Model 1**", "**Model 2**"))

# view the merged table
ais.rt.merged

# check normality of residuals for both models
qqnorm(ais.mod$residuals, main = "Q-Q Plot of Residuals (AIS Model 1)")
qqline(ais.mod$residuals, col = "red")

qqnorm(ais.mod.simple$residuals, main = "Q-Q Plot of Residuals (AIS Model 2)")
qqline(ais.mod.simple$residuals, col = "red")

# check homoscedasticity
plot(ais.mod$fitted.values, ais.mod$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values (AIS Model 1)")
abline(h = 0, col = "red")

plot(ais.mod.simple$fitted.values, ais.mod.simple$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values (AIS Model 2)")
abline(h = 0, col = "red")

################ BSS Model ##################

# check max df in predictors for model with BSS as response variable
max.predictors("BSS", d_complete)

# start with the predictors from literature
bss.mod <- glm(BSS ~ Gender + Depression + Age + BMI + Time.from.transplant + Rejection.graft.dysfunction, data = d_complete, family = "binomial")
summary(bss.mod)

# see if renal failure predictor improves the model
bss.mod2 <- glm(BSS ~ Gender + Depression + Age + BMI + Time.from.transplant + Rejection.graft.dysfunction + Renal.Failure, data = d_complete, family = "binomial")
summary(bss.mod2)

# compare the AIC of the two models
AIC(bss.mod, bss.mod2)

# compare the deviance
deviance(bss.mod)
deviance(bss.mod2)

# use anova LRT to compare the two models
anova(bss.mod, bss.mod2, test = "Chisq")

# check for co-linearity
vif(bss.mod)
vif(bss.mod2)

# no values above 5, so there is no concern for co-linearity

# create summary tables for both models
bss.rt <- tbl_regression(bss.mod, exponentiate = T, intercept = TRUE,
  label = list(
    Time.from.transplant ~ "Time Since Transplant (years)",
    Rejection.graft.dysfunction ~ "Rejection Graft Dysfunction")) %>% 
  italicize_levels() %>%
  bold_labels()

bss.rt2 <- tbl_regression(bss.mod2, exponentiate = T, intercept = TRUE,
  label = list(
    Time.from.transplant  ~ "Time Since Transplant (years)",
    Rejection.graft.dysfunction ~ "Rejection Graft Dysfunction",
    Renal.Failure ~ "Renal Failure")) %>% 
  italicize_levels() %>%
  bold_labels()

# merge BSS tables
bss.rt.merged <- tbl_merge(
  tbls = list(bss.rt, bss.rt2),
  tab_spanner = c("**Model 1**", "**Model 2**"))

# view the merged table
bss.rt.merged

################
## QOL Models ##
################

########### SF36-MCS Model ###########

# check max predictors
max.predictors("SF36.MCS", d_complete)

# create a linear model to predict QOL based on sleep disturbance
mcs.mod <- lm(SF36.MCS ~ ESS + AIS + BSS, data = d_complete)
summary(mcs.mod)

# check of co-linearity
vif(mcs.mod)

# no evidence of co-linearity

# check AIC
AIC(mcs.mod)

# determine the deviance
deviance(mcs.mod)

# create summary table
mcs.rt <- tbl_regression(mcs.mod, exponentiate = F, intercept = TRUE) %>% 
  italicize_levels() %>%
  bold_labels()

# view the table
mcs.rt

# check normality of residuals for both models
qqnorm(mcs.mod$residuals, main = "Q-Q Plot of Residuals (SF36-MCS)")
qqline(mcs.mod$residuals, col = "red")

# check homoscedasticity
plot(mcs.mod$fitted.values, mcs.mod$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values (SF36-MCS)")
abline(h = 0, col = "red")

########### SF36-PCS Model ###########

# check max predictors
max.predictors("SF36.PCS", d_complete)

# create a linear model to predict QOL based on sleep disturbance
pcs.mod <- lm(SF36.PCS ~ ESS + AIS + BSS, data = d_complete)
summary(pcs.mod)

# check of co-linearity
vif(pcs.mod)

# no evidence of co-linearity

# create summary table for the model
pcs.rt <- tbl_regression(pcs.mod, exponentiate = F, intercept = TRUE) %>% 
  italicize_levels() %>%
  bold_labels()

# view the table
pcs.rt

# check normality of residuals in the model
qqnorm(pcs.mod$residuals, main = "Q-Q Plot of Residuals (SF36-PCS)")
qqline(pcs.mod$residuals, col = "red")

# check homoscedasticity
plot(pcs.mod$fitted.values, pcs.mod$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values (SF36-PCS)")
abline(h = 0, col = "red")

# merge the MCS and PCS table for simpler presentation
qol.rt.merged <- tbl_merge(
  tbls = list(mcs.rt, pcs.rt),
  tab_spanner = c("**Mental Health**", "**Physical Health**"))

# view the merged table
qol.rt.merged

# detach the package since it has conflicts with other packages (masking)
detach("package:broom.helpers", unload = T)

################################
## Hypothesis Testing for QOL ##
################################

# create binary columns for each of the sleep scales
qol_comparison <- d_complete %>%
  mutate(ESS_binary = ifelse(ESS > 10, "High", "Low")) %>%
  mutate(AIS_binary = ifelse(AIS > 5, "High", "Low")) %>% 
  mutate(BSS = ifelse(BSS == TRUE, "High", "Low"))
  
# compare the mean QOL for patients with high vs low ESS
t1 <- qol_comparison %>%
  select(ESS_binary, SF36.MCS, SF36.PCS) %>% 
  tbl_summary(
    by = ESS_binary,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)")) %>%
  add_n() %>%
  add_p(
    test = everything() ~ "t.test",
    test.args = all_tests("t.test") ~ list(var.equal = FALSE)) %>% 
  bold_labels()

# compare the mean QOL for patients with high vs low AIS
t2 <- qol_comparison %>%
  select(AIS_binary, SF36.MCS, SF36.PCS) %>% 
  tbl_summary(
    by = AIS_binary,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)")) %>%
  add_n() %>%
  add_p(
    test = everything() ~ "t.test",
    test.args = all_tests("t.test") ~ list(var.equal = FALSE)) %>% 
  bold_labels()

# compare the mean QOL for patients with high vs low BSS
t3 <- qol_comparison %>%
  select(BSS, SF36.MCS, SF36.PCS) %>% 
  tbl_summary(
    by = BSS,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)")) %>%
  add_n() %>%
  add_p(
    test = everything() ~ "t.test",
    test.args = all_tests("t.test") ~ list(var.equal = FALSE)) %>% 
  bold_labels()

# merge tables
tbl_merge <- tbl_merge(
    tbls = list(t1, t2, t3),
    tab_spanner = c("**ESS**", "**AIS**", "**BSS**"))

# view the table
tbl_merge

# make a box plot to represent the mean QOL score for each binary scale

# put data into an easier format for facet_wrap() to create multiple boxplots
boxplot_data <- qol_comparison %>%
  pivot_longer(
    cols = c(ESS_binary, AIS_binary, BSS),
    names_to = "sleep_dist_type",
    values_to = "sleep_dist_value") %>% 
  mutate(sleep_dist_type = ifelse(sleep_dist_type == "ESS_binary", "ESS",
                                  ifelse(sleep_dist_type == "AIS_binary", "AIS", "BSS")))

# create a box plot for each scale
ggplot(data = boxplot_data, mapping = aes(x = sleep_dist_value, y = SF36.MCS)) +
  facet_wrap(~sleep_dist_type, nrow = 1) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "Likelihood of Sleep Disturbance",
    y = "SF36-MCS Score",
    title = "SF36-MCS Scores in Patients with \nDisturbed Sleep vs Patients with Undisturbed Sleep") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5))

ggplot(data = boxplot_data, mapping = aes(x = sleep_dist_value, y = SF36.PCS)) +
  facet_wrap(~sleep_dist_type, nrow = 1) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "Likelihood of Sleep Disturbance",
    y = "SF36-PCS Score",
    title = "SF36-PCS Scored in Patients with \nDisturbed Sleep vs Patients with Undisturbed Sleep") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5))

###########################
## Estimating Prevalence ##
###########################

#' create a table to estimate the prevalence of sleep disturbance according to
#' each of the three scales

prev_table <- qol_comparison %>%
  select(ESS_binary, AIS_binary, BSS) %>% 
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"),
    label = list(
      ESS_binary ~ "Daytime Sleepiness (ESS)",
      AIS_binary ~ "Severity of Insomnia (AIS)",
      BSS ~ "Sleep Disordered Breathing (BSS)")) %>%
  bold_labels() %>% 
  italicize_levels()

# view the table
prev_table
