#===============================================================================
# Exploratory Data Analysis
#===============================================================================

#-------------------------------------------------------------------------------
# Data Wrangling and Feature Engineering
#-------------------------------------------------------------------------------

# Load Packages

library(tidyverse)
library(baseballr)
library(stargazer)
library(formattable)
library(GGally)
library(kableExtra)
library(MASS)
library(glmnet)
library(ggridges)
library(randomForest)
library(leaps)
library(forcats)
library(raster)
library(caret)
library(ModelMetrics)
library(pROC)
library(xgboost)
library(ggrepel)
library(ResourceSelection)
library(performance)
library(arm)
library(patchwork)
library(ggpattern)


# Load Data

data <- read_csv("/Users/rileyleonard99/Documents/Baseball/Thesis (Cornell)/ThesisData.csv")
data2 <- read_csv("/Users/rileyleonard99/Documents/Baseball/Thesis (Cornell)/ThesisData2.csv")

full_data_complete <- read_csv("/Users/rileyleonard99/Documents/Baseball/Thesis (Cornell)/Data/full_data_complete.csv")

data <- rbind(data, data2)

data <- data %>%
  dplyr::select(-1) %>%
  unique()


# In Zone Indicator

data <- data %>%
  mutate(in_zone = ifelse(plate_x >= -0.85 & plate_x <= 0.85 & plate_z >= sz_bot & plate_z <= sz_top, 1, 0))


# Swing Indicator

data <- data %>%
  mutate(swing = ifelse(description %in% c("bunt_foul_tip", "foul", "foul_bunt", "foul_tip", "hit_into_play", 
                                           "missed_bunt", "swinging_strike", "swinging_strike_blocked"), 1, 0))


# Positive RV Indicator

data <- data %>%
  mutate(pos_run_exp = ifelse(delta_run_exp >= 0, 1, 0))


# Count Variable

data <- data %>%
  mutate(count = paste(balls, strikes, sep = "-"))


# Drop Mistagged Data

data <- data %>%
  filter(balls != 4)


# Recode Baserunners

data <- data %>%
  mutate(on_3b = ifelse(!is.na(on_3b), 1, 0),
         on_2b = ifelse(!is.na(on_2b), 1, 0),
         on_1b = ifelse(!is.na(on_1b), 1, 0))


# Previous Pitch Variable

full_data_complete <- full_data_complete %>%
  group_by(game_pk, batter, at_bat_number) %>%
  mutate(previous_pitch_type = if_else(row_number() == 1, "FP", lag(pitch_type))) %>%
  ungroup()


# Modeling Data

modeling_data <- data %>%
  dplyr::select(1, 3:5, 18:19, 25:26, 28:35, 51:52, 56, 58, 69, 92, 99:102)

#write_csv(modeling_data, "run_exp_modeling_data.csv")



# Modeling Subsets

modeling_swings <- modeling_data %>%
  filter(swing == 1)

modeling_takes <- modeling_data %>%
  filter(swing == 0)



#-------------------------------------------------------------------------------
# Random Forest (Run Expectancy)
#-------------------------------------------------------------------------------

# Set Seed

set.seed(28)


# Modeling Subsets

swing_tune <- full_data_complete %>%
  filter(swing == 1) %>%
  dplyr::select(plate_z, plate_x, pfx_z, pfx_x, release_speed, sz_bot, sz_top,
                pitch_type, previous_pitch_type,
                release_extension, balls, strikes, outs_when_up, p_throws,
                stand, on_1b, on_2b, on_3b, pitch_number, delta_run_exp) %>%
  drop_na() %>%
  sample_n(size = 10000)

swing_train <- full_data_complete %>%
  filter(swing == 1) %>%
  dplyr::select(plate_z, plate_x, pfx_z, pfx_x, release_speed, sz_bot, sz_top,
                pitch_type, previous_pitch_type,
                release_extension, balls, strikes, outs_when_up, p_throws,
                stand, on_1b, on_2b, on_3b, pitch_number, delta_run_exp) %>%
  drop_na() %>%
  sample_n(size = 30000)

swing_test <- full_data_complete %>%
  filter(swing == 1) %>%
  dplyr::select(plate_z, plate_x, pfx_z, pfx_x, release_speed, sz_bot, sz_top,
                pitch_type, previous_pitch_type,
                release_extension, balls, strikes, outs_when_up, p_throws,
                stand, on_1b, on_2b, on_3b, pitch_number, delta_run_exp) %>%
  anti_join(swing_train) %>%
  drop_na() %>%
  sample_n(size = 10000)

take_tune <- full_data_complete %>%
  filter(swing == 0) %>%
  dplyr::select(plate_z, plate_x, pfx_z, pfx_x, release_speed, sz_bot, sz_top,
                pitch_type, previous_pitch_type,
                release_extension, balls, strikes, outs_when_up, p_throws,
                stand, on_1b, on_2b, on_3b, pitch_number, delta_run_exp) %>%
  drop_na() %>%
  sample_n(size = 10000)

take_train <- full_data_complete %>%
  filter(swing == 0) %>%
  dplyr::select(plate_z, plate_x, pfx_z, pfx_x, release_speed, sz_bot, sz_top,
                pitch_type, previous_pitch_type,
                release_extension, balls, strikes, outs_when_up, p_throws,
                stand, on_1b, on_2b, on_3b, pitch_number, delta_run_exp) %>%
  drop_na() %>%
  sample_n(size = 30000)

take_test <- full_data_complete %>%
  filter(swing == 0) %>%
  dplyr::select(plate_z, plate_x, pfx_z, pfx_x, release_speed, sz_bot, sz_top,
                pitch_type, previous_pitch_type,
                release_extension, balls, strikes, outs_when_up, p_throws,
                stand, on_1b, on_2b, on_3b, pitch_number, delta_run_exp) %>%
  drop_na() %>%
  sample_n(size = 10000)


# Define Train Control Object

train_control <- trainControl(method = "cv", 
                              number = 10,
                              search = "grid",
                              verboseIter = TRUE)


# Create Tuning Grid

grid <- expand.grid(.mtry = 2:10)


# Perform Cross Validation

tune_swing <- train(delta_run_exp ~ ., 
                    data = swing_sample, 
                    method = "rf", 
                    metric = "RMSE",
                    trControl = train_control, 
                    tuneGrid = grid,
                    ntree = 500)

tune_take <- train(delta_run_exp ~ ., 
                   data = take_sample, 
                   method = "rf", 
                   metric = "RMSE",
                   trControl = train_control, 
                   tuneGrid = grid,
                   ntree = 500)
  

# Cross Validation Results

tune_swing$bestTune$mtry
tune_take$bestTune$mtry


# Graph Results

ggplot(tune_swing$results, aes(x = mtry, y = RMSE)) +
  geom_line() + 
  geom_point() +
  labs(x = "Number of Variables Randomly Sampled (mtry)",
       y = "Root Mean Squared Error (RMSE)") +
  scale_y_continuous(limits = c(0,1)) +
  theme_classic()

ggplot(tune_take$results, aes(x = mtry, y = RMSE)) +
  geom_line() +
  geom_point() + 
  labs(x = "Number of Variables Randomly Sampled (mtry)",
       y = "Root Mean Squared Error (RMSE)") +
  scale_y_continuous(limits = c(0,1)) +
  theme_classic()


# Train Model

swing_rf_model <- randomForest(delta_run_exp ~ ., 
                               data = swing_train, 
                               mtry = 2, 
                               ntree = 1000)

take_rf_model <- randomForest(delta_run_exp ~ ., 
                              data = take_train, 
                              mtry = 10, 
                              ntree = 1000)



# Assess Variable Importance

varImpPlot(swing_rf_model)

varImpPlot(take_rf_model)


# Save Feature Importance

ImportanceSwing <- as.data.frame(importance(swing_rf_model))

ImportanceSwing <- data.frame(names = row.names(ImportanceSwing), ImportanceSwing)

ImportanceSwing <- ImportanceSwing %>%
  rename(Feature = names,
         Importance = IncNodePurity)

#write_csv(ImportanceSwing, "ImportanceSwing.csv")


ImportanceTake <- as.data.frame(importance(take_rf_model))

ImportanceTake <- data.frame(names = row.names(ImportanceTake), ImportanceTake)

ImportanceTake <- ImportanceTake %>%
  rename(Feature = names,
         Importance = IncNodePurity)

#write_csv(ImportanceTake, "ImportanceTake.csv")


# Random Forest Predictions

pred_RF_swing <- predict(swing_rf_model, modeling_data)

pred_RF_take <- predict(take_rf_model, modeling_data)


# Bind Predictions

modeling_results <- cbind(modeling_data, pred_RF_swing, pred_RF_take)


# Calculate Difference

modeling_results <- modeling_results %>%
  mutate(pred_delta_run_exp_swing = pred_RF_swing - pred_RF_take)


# R-Squared

swing_test_results <- predict(swing_rf_model, swing_test)
cor(swing_test$delta_run_exp, swing_test_results) #0.0898

take_test_results <- predict(take_rf_model, take_test)
cor(take_test$delta_run_exp, take_test_results) #0.778


# RMSE for Random Forest

squared_diffs_swing <- (swing_test$delta_run_exp - swing_test_results) ^ 2
mse_swing <- mean(squared_diffs_swing)
sqrt(mse_swing) #0.335

squared_diffs_take <- (take_test$delta_run_exp - take_test_results) ^ 2
mse_take <- mean(squared_diffs_take)
sqrt(mse_take) #0.0632


#-------------------------------------------------------------------------------
# Random Forest (Swing Probability)
#-------------------------------------------------------------------------------

# Modeling Subsets

prob_tune <- full_data_complete %>%
  mutate(swing = as.factor(swing)) %>%
  dplyr::select(plate_z, plate_x, pfx_z, pfx_x, release_speed, sz_bot, sz_top,
                pitch_type, previous_pitch_type,
                release_extension, balls, strikes, outs_when_up, p_throws,
                stand, on_1b, on_2b, on_3b, pitch_number, swing) %>%
  drop_na() %>%
  sample_n(size = 10000)

prob_train <- full_data_complete %>%
  mutate(swing = as.factor(swing)) %>%
  dplyr::select(plate_z, plate_x, pfx_z, pfx_x, release_speed, sz_bot, sz_top,
                pitch_type, previous_pitch_type,
                release_extension, balls, strikes, outs_when_up, p_throws,
                stand, on_1b, on_2b, on_3b, pitch_number, swing) %>%
  drop_na() %>%
  sample_n(size = 30000)

prob_test <- full_data_complete %>%
  mutate(swing = as.factor(swing)) %>%
  dplyr::select(plate_z, plate_x, pfx_z, pfx_x, release_speed, sz_bot, sz_top,
                pitch_type, previous_pitch_type,
                release_extension, balls, strikes, outs_when_up, p_throws,
                stand, on_1b, on_2b, on_3b, pitch_number, swing) %>%
  drop_na() %>%
  anti_join(prob_train) %>%
  sample_n(size = 10000)


# Define Train Control Object

train_control <- trainControl(method = "cv", 
                              number = 10,
                              search = "grid",
                              verboseIter = TRUE)


# Create Tuning Grid

grid <- expand.grid(.mtry = 4:12)


# Perform Cross Validation

tune_prob <- train(swing ~ ., 
                   data = prob_tune, 
                   method = "rf", 
                   metric = "Accuracy",
                   trControl = train_control, 
                   tuneGrid = grid,
                   ntree = 500)


# Cross Validation Results

tune_prob$bestTune$mtrytube


# Graph Results

ggplot(tune_prob$results, aes(x = mtry, y = Accuracy)) +
  geom_line() + 
  geom_point() +
  labs(x = "Number of Variables Randomly Sampled (mtry)",
       y = "Predictive Accuracy (%)") +
  scale_y_continuous(limits = c(0,1)) +
  theme_classic()



# Train Model

prob_model <- randomForest(swing ~ ., 
                           data = prob_train,
                           mtry = 12, 
                           ntree = 3000)


# Assess Variable Importance

varImpPlot(prob_model)


# Save Feature Importance

ImportanceProb <- as.data.frame(importance(prob_model))

ImportanceProb <- data.frame(names = row.names(ImportanceProb), ImportanceProb)

ImportanceProb <- ImportanceProb %>%
  rename(Feature = names,
         Importance = MeanDecreaseGini)

#write_csv(ImportanceProb, "ImportanceProb.csv")



# Random Forest Predictions

prob_RF_swing <- predict(prob_model, full_data_complete, type = "prob")

prob_RF_swing <- prob_RF_swing[, 2]

decision_RF_swing <- if_else(prob_RF_swing >= 0.5, 1, 0)


# Bind Predictions

modeling_results <- cbind(modeling_results, prob_RF_swing, decision_RF_swing)

write_csv(modeling_results, "ModelingResults.csv")


# Test Accuracy

prob_test_results <- predict(prob_model, prob_test, type = "prob")
prob_test_results <- prob_test_results[, 2]
prob_test_decision <- if_else(prob_test_results >= 0.5, 1, 0)

mean(prob_test_decision == prob_test$swing) #0.7778


# ROC Curve for Random Forest

roc_curve_swing_prob <- roc(response = prob_test$swing, predictor = prob_test_results)
pROC::auc(roc_curve_swing_prob) #0.8496

ggroc(roc_curve_swing_prob , legacy.axes = TRUE) + theme_classic() + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color = "grey", linetype = "dashed")



#-------------------------------------------------------------------------------
# Full Data
#-------------------------------------------------------------------------------

full_data <- cbind(data, pred_RF_swing, pred_RF_take, prob_RF_swing, decision_RF_swing)

full_data <- full_data %>%
  mutate(pred_delta_run_exp_swing = pred_RF_swing - pred_RF_take,
         dec_delta_run_exp = ifelse(swing == 1, pred_RF_swing, pred_RF_take),
         exp_delta_run_exp = (prob_RF_swing * pred_RF_swing) + ((1 - prob_RF_swing) * pred_RF_take),
         optimal = if_else((pred_RF_swing >= pred_RF_take & swing == 1) | 
                             (pred_RF_swing < pred_RF_take & swing == 0), 1, 0),
         low_prob = ifelse(prob_RF_swing < 0.5, 1, 0))

#write.csv(full_data, "full_data.csv")
#full_data <- read_csv("/Users/rileyleonard99/Documents/Baseball/Thesis (Cornell)/full_data.csv")

#-------------------------------------------------------------------------------
# Model Data Visualization
#-------------------------------------------------------------------------------

# Feature Importance

ImportanceSwing %>% 
  filter(Feature != "pitch_number") %>%
  mutate(Feature = fct_reorder(Feature, Importance)) %>%
  mutate(Feature = recode(Feature,
                          `plate_z` = "Vert. Location",
                          `plate_x` = "Horz. Location",
                          `pfx_z` = "Vert. Movement",
                          `pfx_x` = "Horz. Movement",
                          `release_speed` = "Velocity",
                          `sz_bot` = "Zone Bottom",
                          `sz_top` = "Zone Top",
                          `release_extension` = "Extension",
                          `pitch_type` = "Pitch Type",
                          `previous_pitch_type` = "Previous Pitch Type",
                          `balls` = "Balls",
                          `strikes` = "Strikes",
                          `outs_when_up` = "Outs",
                          `p_throws` = "Pitcher Hand",
                          `stand` = "Batter Hand",
                          `on_1b` = "On 1B",
                          `on_2b` = "On 2B",
                          `on_3b` = "On 3B")) %>%
  ggplot(aes(x = Feature, y = Importance)) +
  geom_segment(aes(x = Feature, xend = Feature, y = 0, yend = Importance), color = "skyblue") +
  geom_point(color = "dodgerblue", size = 4) +
  theme_light() +
  coord_flip() +
  labs(x = "Model Feature",
       y = "Relative Importance (Increase in Node Purity)") +
  theme(axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())

ImportanceTake %>% 
  filter(Feature != "pitch_number") %>%
  mutate(Feature = fct_reorder(Feature, Importance)) %>%
  mutate(Feature = recode(Feature,
                          `plate_z` = "Vert. Location",
                          `plate_x` = "Horz. Location",
                          `pfx_z` = "Vert. Movement",
                          `pfx_x` = "Horz. Movement",
                          `release_speed` = "Velocity",
                          `sz_bot` = "Zone Bottom",
                          `sz_top` = "Zone Top",
                          `release_extension` = "Extension",
                          `pitch_type` = "Pitch Type",
                          `previous_pitch_type` = "Previous Pitch Type",
                          `balls` = "Balls",
                          `strikes` = "Strikes",
                          `outs_when_up` = "Outs",
                          `p_throws` = "Pitcher Hand",
                          `stand` = "Batter Hand",
                          `on_1b` = "On 1B",
                          `on_2b` = "On 2B",
                          `on_3b` = "On 3B")) %>%
  ggplot(aes(x = Feature, y = Importance)) +
  geom_segment(aes(x = Feature, xend = Feature, y = 0, yend = Importance), color = "skyblue") +
  geom_point(color = "dodgerblue", size = 4) +
  theme_light() +
  coord_flip() +
  labs(x = "Model Feature",
       y = "Relative Importance (Increase in Node Purity)") +
  theme(axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())

ImportanceProb %>% 
  filter(Feature != "pitch_number") %>%
  mutate(Feature = fct_reorder(Feature, Importance)) %>%
  mutate(Feature = recode(Feature,
                          `plate_z` = "Vert. Location",
                          `plate_x` = "Horz. Location",
                          `pfx_z` = "Vert. Movement",
                          `pfx_x` = "Horz. Movement",
                          `release_speed` = "Velocity",
                          `sz_bot` = "Zone Bottom",
                          `sz_top` = "Zone Top",
                          `release_extension` = "Extension",
                          `pitch_type` = "Pitch Type",
                          `previous_pitch_type` = "Previous Pitch Type",
                          `balls` = "Balls",
                          `strikes` = "Strikes",
                          `outs_when_up` = "Outs",
                          `p_throws` = "Pitcher Hand",
                          `stand` = "Batter Hand",
                          `on_1b` = "On 1B",
                          `on_2b` = "On 2B",
                          `on_3b` = "On 3B")) %>%
  ggplot(aes(x = Feature, y = Importance)) +
  geom_segment(aes(x = Feature, xend = Feature, y = 0, yend = Importance), color = "skyblue") +
  geom_point(color = "dodgerblue", size = 4) +
  theme_light() +
  coord_flip() +
  labs(x = "Model Feature",
       y = "Relative Importance (Mean Decrease in GINI)") +
  theme(axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())



# Scatter Plot

modeling_results <- modeling_results %>%
  mutate(pred_RF_decision = ifelse(swing == 1, pred_RF_swing, pred_RF_take))

modeling_results  %>%
  sample_n(size = 10000) %>%
  ggplot(aes(x = pred_RF_decision, y = delta_run_exp, color = as.factor(swing))) +
  geom_point(size = 2, alpha = 0.2) +
  geom_line(stat = "smooth", method = "lm", 
            formula = y ~ x,
            size = 1,
            linetype ="dashed",
            color = "black") +
  labs(x = "Predicted REA (Swing Decision)", 
       y = "Observed REA (Swing Decision)") +
  scale_color_manual(values = c("deepskyblue", "palevioletred"),
                     name = "Swing Decision",
                     labels = c("Take", "Swing")) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_y_continuous(limits = c(-1, 2)) +
  theme_classic() +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        plot.title = element_text(size = 8, face = "bold.italic", hjust = 0.5))


# Strike Zone Run Expectancy Hexbin

modeling_results %>%
  ggplot(aes(x = plate_x, y = plate_z, z = pred_RF_swing)) +
  stat_summary_hex(fun = mean, bins = 30) +
  scale_fill_gradient2(low = "blue", high = "#dd3a3e") +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(0, 5)) +
  annotate("rect", xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.36, fill = NA, color = "black", size = 1) +
  labs(y = "Vertical Location", x = "Horizontal Location", fill = "Exp. REA") +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, 'cm'),
        plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5))


modeling_results %>%
  ggplot(aes(x = plate_x, y = plate_z, z = pred_RF_take)) +
  stat_summary_hex(fun = mean, bins = 30) +
  scale_fill_gradient2(low = "blue", high = "#dd3a3e") +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(0, 5)) +
  annotate("rect", xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.36, fill = NA, color = "black", size = 1) +
  labs(y = "Vertical Location", x = "Horizontal Location", fill = "Exp. REA") +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, 'cm'),
        plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5))


full_data_complete %>%
  ggplot(aes(x = plate_x, y = plate_z, z = pred_delta_run_exp_swing)) +
  stat_summary_hex(fun = mean, bins = 25) +
  scale_fill_gradient2(low = "blue", high = "#dd3a3e") +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(0, 5)) +
  annotate("rect", xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.36, 
           fill = NA, color = "black", size = 1) +
  labs(y = "Vertical Location", 
       x = "Horizontal Location", 
       fill = "Pred. REA") +
  theme_classic() +
  theme(axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.3, 'cm'))



# Strike Zone Swing Probability Hexbin

full_data_complete %>%
  ggplot(aes(x = plate_x, y = plate_z, z = prob_RF_swing)) +
  stat_summary_hex(fun = mean, bins = 25) +
  scale_fill_gradient2(low = "white", high = "#dd3a3e") +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(0, 5)) +
  annotate("rect", xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.36, 
           fill = NA, color = "black", size = 1) +
  labs(y = "Vertical Location", 
       x = "Horizontal Location", 
       fill = "Swing Prob.") +
  theme_classic() +
  theme(axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.3, 'cm'))


# Strike Zone Swing Probability Hexbin (Faceted)

full_data_complete %>%
  ggplot(aes(x = plate_x, y = plate_z, z = prob_RF_swing)) +
  stat_summary_hex(fun = mean, bins = 25) +
  scale_fill_gradient2(low = "white", high = "#dd3a3e") +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(0, 5)) +
  annotate("rect", xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.36, 
           fill = NA, color = "black", size = 0.75) +
  labs(y = "Vertical Location", 
       x = "Horizontal Location", 
       fill = "Swing Prob.") +
  facet_wrap(~count, ncol = 3) +
  theme_minimal() +
  theme(strip.text = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.3, 'cm'))


# Strike Zone Run Expectancy Hexbin (Faceted)

full_data_complete %>%
  filter(pred_delta_run_exp_swing > -0.4) %>%
  ggplot(aes(x = plate_x, y = plate_z, z = pred_delta_run_exp_swing)) +
  stat_summary_hex(fun = mean, bins = 25) +
  scale_fill_gradient2(low = "blue", high = "#dd3a3e") +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(0, 5)) +
  annotate("rect", xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.36, 
           fill = NA, color = "black", size = 0.75) +
  labs(y = "Vertical Location", 
       x = "Horizontal Location", 
       fill = "Pred. REA") +
  facet_wrap(~count, ncol = 3) +
  theme_minimal() +
  theme(strip.text = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.3, 'cm'))


# Overlaid Density Plot (All Counts): Distribution of RV

full_data_complete %>%
  ggplot(aes(x = delta_run_exp, fill = fct_rev(as.factor(swing)), color = fct_rev(as.factor(swing)))) +
  geom_density(alpha = 0.25, bw = 0.15, linewidth = 0.8) +
  labs(y = "Density",
       x = "Change in Run Expectancy",
       fill = "Swing Decision") +
  scale_x_continuous(limits = c(-1, 2)) +
  scale_fill_manual(values = c("palevioletred", "deepskyblue"),
                    name = "Swing Decision",
                    labels = c("Swing", "Take")) +
  scale_color_manual(values = c("palevioletred", "deepskyblue"),
                     name = "Swing Decision",
                     labels = c("Swing", "Take")) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, 'cm'),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5))


# Overlaid Density Plot (Faceted): Distribution of RV

full_data_complete %>%
  ggplot(aes(x = delta_run_exp, fill = as.factor(swing), color = as.factor(swing))) +
  geom_density(alpha = 0.25, bw = 0.1, linewidth = 0.8) +
  labs(y = "Density",
       x = "Change in Run Expectancy",
       fill = "Swing Decision") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_fill_manual(values = c("deepskyblue", "palevioletred"),
                    name = "Swing Decision",
                    labels = c("Take", "Swing")) +
  scale_color_manual(values = c("deepskyblue", "palevioletred"),
                     name = "Swing Decision",
                     labels = c("Take", "Swing")) +
  facet_wrap(~count, ncol = 3) +
  theme_light() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.3, 'cm'))

full_data_complete %>%
  ggplot(aes(x = delta_run_exp, fill = fct_rev(as.factor(swing)), color = fct_rev(as.factor(swing)))) +
  geom_density(alpha = 0.25, bw = 0.1, linewidth = 0.8) +
  labs(y = "Density",
       x = "Change in Run Expectancy",
       fill = "Swing Decision") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_fill_manual(values = c("palevioletred", "deepskyblue"),
                    name = "Swing Decision",
                    labels = c("Swing", "Take")) +
  scale_color_manual(values = c("palevioletred", "deepskyblue"),
                     name = "Swing Decision",
                     labels = c("Swing", "Take")) +
  facet_wrap(~count, ncol = 3) +
  theme_minimal() +
  theme(strip.text = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.5, 'cm'))


# Ridge Plot (By Count): Distribution of RV (Swings) ### See if you can do overlaid ridges

modeling_results %>%
  ggplot(aes(x = pred_delta_run_exp_swing, y = count)) +
  geom_density_ridges(aes(fill = as.factor(swing), color = as.factor(swing)), 
                      quantile_lines = TRUE,
                      quantiles = 2,
                      show.legend = FALSE,
                      size = 1,
                      scale = 1.5,
                      alpha = 0.2,
                      bandwidth = 0.05) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_fill_manual(values = c("deepskyblue", "palevioletred"), labels = c("Take", "Swing")) +
  scale_color_manual(values = c("deepskyblue", "palevioletred"), labels = c("Take", "Swing")) +
  labs(y = "Count",
       x = "Change in Run Expectancy") +
  theme_minimal()


#-------------------------------------------------------------------------------
# Summary Data
#-------------------------------------------------------------------------------

# Aggregate and Summarize

summary <- full_data_complete %>%
  filter(events %in% c(NA, "catcher_interf", "double", "double_play",
                       "field_error", "field_out", "fielders_choice",
                       "fielders_choice_out", "force_out", "grounded_into_double_play",
                       "hit_by_pitch", "home_run", "sac_bunt",
                       "sac_fly", "sac_fly_double_play", "single",
                       "strikeout", "triple", "triple_play", "walk")) %>%
  mutate(game_ab = paste(game_pk, at_bat_number, sep = "")) %>%
  group_by(player_name, batter) %>%
  summarise(PA = n_distinct(game_ab),
            Pitches = n(),
            `Pitches/PA` = n()/n_distinct(game_ab),
            `Swing %` = mean(swing, na.rm = TRUE),
            `O-Swing %` = mean(swing[in_zone == 0], na.rm = TRUE),
            `Z-Swing %` = mean(swing[in_zone == 1], na.rm = TRUE),
            `Aggressive %` = mean(swing[low_prob == 1], na.rm = TRUE),
            `Optimal %` = mean(optimal, na.rm = TRUE),
            `SOE` = sum(swing, na.rm = TRUE) - sum(prob_RF_swing, na.rm = TRUE),
            `sREA` = sum(delta_run_exp[swing == 1], na.rm = TRUE),
            `tREA` = sum(delta_run_exp[swing == 0], na.rm = TRUE),
            `xREA` = sum(exp_delta_run_exp, na.rm = TRUE),
            `dxREA` = sum(dec_delta_run_exp, na.rm = TRUE),
            `REA` = sum(delta_run_exp, na.rm = TRUE)) %>%
  ungroup()

player_team <- full_data %>%
  group_by(player_name, batter) %>%
  summarize(player_team = paste(unique(batting_team), collapse = "-"),
            .groups = 'drop')

summary <- left_join(summary, player_team, by = c("player_name", "batter"))

qualified <- summary %>%
 # relocate(player_team, .after = batter) %>%
  filter(PA >= 400) %>%
  mutate(`sREA/PA` = sREA/PA,
         `tREA/PA` = tREA/PA,
         `xREA/PA` = xREA/PA,
         `dxREA/PA` = dxREA/PA,
         `REA/PA` = REA/PA,) %>%
  mutate(`Std Swing %` = as.numeric(scale(`Swing %`)),
         `Std O-Swing %` = as.numeric(scale(`O-Swing %`)),
         `Std Z-Swing %` = as.numeric(scale(`Z-Swing %`)),
         `Std Aggressive %` = as.numeric(scale(`Aggressive %`)),
         `Std Optimal %` = as.numeric(scale(`Optimal %`)),
         `Std SOE` = as.numeric(scale(`SOE`)),
         `Std sREA` = as.numeric(scale(`sREA`)),
         `Std tREA` = as.numeric(scale(`tREA`)),
         `Std xREA` = as.numeric(scale(`xREA`)),
         `Std dxREA` = as.numeric(scale(`dxREA`)),
         `Std REA` = as.numeric(scale(`REA`)),
         `Std sREA/PA` = as.numeric(scale(`sREA/PA`)),
         `Std tREA/PA` = as.numeric(scale(`tREA/PA`)),
         `Std xREA/PA` = as.numeric(scale(`xREA/PA`)),
         `Std dxREA/PA` = as.numeric(scale(`dxREA/PA`)),
         `Std REA/PA` = as.numeric(scale(`REA/PA`)))


team_summary <- full_data %>%
  filter(events %in% c(NA, "catcher_interf", "double", "double_play",
                       "field_error", "field_out", "fielders_choice",
                       "fielders_choice_out", "force_out", "grounded_into_double_play",
                       "hit_by_pitch", "home_run", "sac_bunt",
                       "sac_fly", "sac_fly_double_play", "single",
                       "strikeout", "triple", "triple_play", "walk")) %>%
  mutate(game_ab = paste(game_pk, at_bat_number, sep = "")) %>%
  group_by(batting_team) %>%
  summarise(PA = n_distinct(game_ab),
            Pitches = n(),
            `Pitches/PA` = n()/n_distinct(game_ab),
            `Swing %` = mean(swing, na.rm = TRUE),
            `O-Swing %` = mean(swing[in_zone == 0], na.rm = TRUE),
            `Z-Swing %` = mean(swing[in_zone == 1], na.rm = TRUE),
            `Aggressive %` = mean(swing[low_prob == 1], na.rm = TRUE),
            `Optimal %` = mean(optimal, na.rm = TRUE),
            `SOE` = sum(swing, na.rm = TRUE) - sum(prob_RF_swing, na.rm = TRUE),
            `sREA` = sum(delta_run_exp[swing == 1], na.rm = TRUE),
            `tREA` = sum(delta_run_exp[swing == 0], na.rm = TRUE),
            `xREA` = sum(exp_delta_run_exp, na.rm = TRUE),
            `dxREA` = sum(dec_delta_run_exp, na.rm = TRUE),
            `REA` = sum(delta_run_exp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`sREA/PA` = sREA/PA,
         `tREA/PA` = tREA/PA,
         `xREA/PA` = xREA/PA,
         `dxREA/PA` = dxREA/PA,
         `REA/PA` = REA/PA) %>%
  mutate(`Std Swing %` = as.numeric(scale(`Swing %`)),
         `Std O-Swing %` = as.numeric(scale(`O-Swing %`)),
         `Std Z-Swing %` = as.numeric(scale(`Z-Swing %`)),
         `Std Aggressive %` = as.numeric(scale(`Aggressive %`)),
         `Std Optimal %` = as.numeric(scale(`Optimal %`)),
         `Std SOE` = as.numeric(scale(`SOE`)),
         `Std sREA` = as.numeric(scale(`sREA`)),
         `Std tREA` = as.numeric(scale(`tREA`)),
         `Std xREA` = as.numeric(scale(`xREA`)),
         `Std dxREA` = as.numeric(scale(`dxREA`)),
         `Std REA` = as.numeric(scale(`REA`)),
         `Std sREA/PA` = as.numeric(scale(`sREA/PA`)),
         `Std tREA/PA` = as.numeric(scale(`tREA/PA`)),
         `Std xREA/PA` = as.numeric(scale(`xREA/PA`)),
         `Std dxREA/PA` = as.numeric(scale(`dxREA/PA`)),
         `Std REA/PA` = as.numeric(scale(`REA/PA`)))

#write.csv(team_summary, "team_summary.csv")
#team_summary <- read_csv("/Users/rileyleonard99/Documents/Baseball/Thesis (Cornell)/Excess Data/team_summary.csv")

#-------------------------------------------------------------------------------
# Summary Data Visualization
#-------------------------------------------------------------------------------

# xREA vs. REA

qualified %>%
ggplot(aes(x = xREA, y = REA)) +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE, color = "black", size = 0.75) +
  geom_point(aes(size = PA), alpha = 0.25, size = 4) +
  geom_text_repel(aes(label = player_name),
                  data = qualified %>% filter(REA >= 50 | xREA >= 30),
                  box.padding = 0.65,
                  size = 2.5) +
  scale_size_continuous(limits = c(0, 800)) +
  labs(y = "Run Expectancy Added (REA)", 
       x = "Expected Run Expectancy Added (xREA)") +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position = "none")
  

# Quadrant Plot

qualified %>%
  ggplot(aes(x = `Std tREA/PA`, y = `Std sREA/PA`)) +
  scale_x_continuous(name = "Standardized Take Run Expectancy Added (tREA/PA)", 
                     breaks = seq(-3, 3, 1),
                     limits = c(-3, 3)) +
  scale_y_continuous(name = "Standardized Swing Run Expectancy Added (sREA/PA)", 
                     breaks = seq(-3, 3, 1), limits = c(-3, 3)) +
  annotate(geom = "rect", xmin = 0, xmax = 3, ymin = 0, ymax = 3, fill = "palevioletred2", alpha = 0.2) +
  annotate(geom = "rect", xmin = -3, xmax = 0, ymin = -3, ymax = 0, fill = "deepskyblue", alpha = 0.2) +
  geom_point(data = qualified, aes(x = `Std tREA/PA`, y = `Std sREA/PA`), 
             shape = 21, colour = "dodgerblue", fill = "grey100", size = 5, alpha = 0.8) +
  geom_text_repel(aes(label = player_name),
                  data = qualified %>% filter(abs(`Std tREA/PA`) >= 1.99 | abs(`Std sREA/PA`) >= 1.99 | (abs(`Std tREA/PA`) >= 1 & abs(`Std sREA/PA`) >= 1)),
                  box.padding = 0.6,
                  size = 2.5) +
  theme_minimal() +
  theme(axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8))


# Time Series (Top and Bottom 5)

top_five_time <- full_data %>%
  mutate(game_date = as.Date(game_date)) %>%
  filter(player_name %in% c("Acuña Jr., Ronald",
                            "Freeman, Freddie",
                            "Olson, Matt",
                            "Betts, Mookie",
                            "Ohtani, Shohei",
                            "Anderson, Tim",
                            "Straw, Myles",
                            "Maldonado, Martín",
                            "DeJong, Paul",
                            "Turang, Brice")) %>%
  group_by(player_name, game_date) %>%
  summarize(daily_REA = sum(delta_run_exp, na.rm = TRUE), .groups = 'drop') %>%
  arrange(player_name, game_date) %>%
  group_by(player_name) %>%
  mutate(cum_REA = cumsum(daily_REA)) %>%
  ungroup()

all_star_start <- as.Date("2023-07-9")
all_star_end <- as.Date("2023-07-14")

last_points <- top_five_time %>%
  group_by(player_name) %>%
  filter(game_date == max(game_date)) %>%
  ungroup()

last_points_ordered <- last_points %>%
  arrange(desc(cum_REA)) %>%
  .$player_name

top_five_time$player_name <- factor(top_five_time$player_name, levels = last_points_ordered)


top_five_time %>%
  ggplot(aes(x = game_date, y = cum_REA)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(aes(color = player_name),
            size  = 1.5,
            alpha = 0.5) +
  geom_point(data = last_points, aes(color = player_name, x = game_date, y = cum_REA),
             size = 3) +
  labs(y = "Cumulative Run Expectancy Added (REA)", 
       x = "Date",
       color = "Player") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  annotate("rect", xmin = all_star_start, xmax = all_star_end, ymin = -Inf, ymax = Inf, 
           alpha = 0.2, fill = "grey50") +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 10))


# Diverging Team Summaries

team_summary %>%
  mutate(`dxREA/PA Type` = ifelse(`Std dxREA/PA` >=0, "above", "below")) %>%
  ggplot(aes(x = reorder(batting_team, `Std dxREA/PA`),
                         y = `Std dxREA/PA`,
                         label = `Std dxREA/PA`)) + 
  geom_bar(stat = 'identity', aes(fill = `dxREA/PA Type`), width = .5, alpha = 0.5)  +
  scale_fill_manual(labels = c("Above Average", "Below Average"), 
                    values = c("below"="deepskyblue", "above"="palevioletred")) + 
  labs(x = "Team",
       y = "Standardized Swing Decision Run Expectancy Added (dxREA/PA)") + 
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.position = "none")

team_summary %>%
  mutate(`REA/PA Type` = ifelse(`Std REA/PA` >=0, "above", "below")) %>%
  ggplot(aes(x = reorder(batting_team, `Std REA/PA`),
             y = `Std REA/PA`,
             label = `Std REA/PA`)) + 
  geom_bar(stat = 'identity', aes(fill = `REA/PA Type`), width = .5, alpha = 0.5)  +
  scale_fill_manual(labels = c("Above Average", "Below Average"), 
                    values = c("below"="deepskyblue", "above"="palevioletred")) + 
  labs(x = "Team",
       y = "Standardized Run Expectancy Added (REA/PA)") + 
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.position = "none")


# Optimal % vs REA (team-level)

team_summary %>%
  ggplot(aes(x = `Optimal %`, y = REA)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE, color = "black") +
  labs(y = "Run Expectancy Added (REA)", 
       x = "Optimal Swing Decision %") +
  theme_classic() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5))

cor(team_summary$`Optimal %`, team_summary$REA) # r = 0.464
cor(team_summary$dxREA, team_summary$REA) # r = 0.480


# Player Leaderboard

qualified <- qualified %>%
  arrange(desc(REA))

qualified %>%
  head(n = 20) %>%
  rename(Player = player_name,
         ORG = player_team) %>%
  mutate(`Pitches/PA` = format(round(`Pitches/PA`, 2), nsmall = 2),
         `Swing %` = format(round(`Swing %`, 2), nsmall = 2),
         `O-Swing %` = format(round(`O-Swing %`, 2), nsmall = 2),
         `Z-Swing %` = format(round(`Z-Swing %`, 2), nsmall = 2),
         `Aggressive %` = format(round(`Aggressive %`, 2), nsmall = 2),
         `Optimal %` = format(round(`Optimal %`, 2), nsmall = 2),
         SOE = format(round(SOE, 1), nsmall = 1),
         sREA = format(round(sREA, 2), nsmall = 2),
         tREA = format(round(tREA, 2), nsmall = 2),
         xREA = format(round(xREA, 2), nsmall = 2),
         dxREA = format(round(dxREA, 2), nsmall = 2),
         REA = format(round(REA, 2), nsmall = 2)) %>%
  mutate(`Swing %` = cell_spec(`Swing %`, "html",
                                 color = ifelse(`Std Swing %` <= -1, "white", ifelse(`Std Swing %` >= 1, "white", "black")),
                                 background = ifelse(`Std Swing %` <= -1, "rgba(0, 0, 255, 0.7)", ifelse(`Std Swing %` >= 1, "rgba(255, 0, 0, 0.6)", "transparent"))),
         `O-Swing %` = cell_spec(`O-Swing %`, "html",
                                 color = ifelse(`Std O-Swing %` <= -1, "white", ifelse(`Std O-Swing %` >= 1, "white", "black")),
                                 background = ifelse(`Std O-Swing %` <= -1, "rgba(0, 0, 255, 0.7)", ifelse(`Std O-Swing %` >= 1, "rgba(255, 0, 0, 0.6)", "transparent"))),
         `Z-Swing %` = cell_spec(`Z-Swing %`, "html",
                                 color = ifelse(`Std Z-Swing %` <= -1, "white", ifelse(`Std Z-Swing %` >= 1, "white", "black")),
                                 background = ifelse(`Std Z-Swing %` <= -1, "rgba(0, 0, 255, 0.7)", ifelse(`Std Z-Swing %` >= 1, "rgba(255, 0, 0, 0.6)", "transparent"))),
         `Aggressive %` = cell_spec(`Aggressive %`, "html",
                                 color = ifelse(`Std Aggressive %` <= -1, "white", ifelse(`Std Aggressive %` >= 1, "white", "black")),
                                 background = ifelse(`Std Aggressive %`<= -1, "rgba(0, 0, 255, 0.7)", ifelse(`Std Aggressive %` >= 1, "rgba(255, 0, 0, 0.6)", "transparent")))) %>%
  dplyr::select(1, 3:17) %>%
  kbl(escape = F, format = 'html') %>%
  kable_classic_2(full_width = F) %>%
  kable_styling(bootstrap_options = "striped", font_size = 10) %>%
  column_spec(10, color = "white",
              background = spec_color(qualified$`Optimal %`, begin = 0.2, end = 0.8, alpha = 0.75)) %>%
  column_spec(16, color = "white",
              background = spec_color(qualified$REA, begin = 0.2, end = 0.8, alpha = 0.75))



#-------------------------------------------------------------------------------
# Cumulative Player Stats
#-------------------------------------------------------------------------------

full_data_refined <- full_data %>%
  drop_na(pred_RF_swing, pred_RF_take, pred_delta_run_exp_swing, dec_delta_run_exp, exp_delta_run_exp, optimal, low_prob) %>%
  arrange(ymd(game_date), game_pk, at_bat_number, pitch_number) %>%
  dplyr::select(game_date, player_name, batter, events, description, p_throws,balls, strikes, count,
                release_speed, pitch_type, outs_when_up, p_throws, stand,
                29:38, sz_top, sz_bot, launch_speed, release_speed, release_extension, game_pk, woba_value, woba_denom,
                estimated_ba_using_speedangle, estimated_woba_using_speedangle, at_bat_number, pitch_number,
                bat_score, fld_score, delta_run_exp, hit, barrel, in_zone, 101:112)

full_data_cum <- full_data_refined %>%
  mutate(game_ab = paste(game_pk, at_bat_number, sep = "")) %>%
  group_by(player_name, batter) %>%
  arrange(game_date, game_pk, at_bat_number, pitch_number) %>%
  group_by(player_name, batter) %>%
  mutate(
    # Cumulative Plate Appearances
    PA_cum = cumsum(!is.na(events)),
    # Cumulative Pitches
    Pitches_cum = row_number(),
    # Cumulative Swing %
    Swing_cum = cummean(swing),
    #Cumulative wOBA
    woba_value_na_rm = replace_na(woba_value, 0),
    woba_denom_na_rm = replace_na(woba_denom, 0),
    wOBA_cum = cumsum(woba_value_na_rm) / cumsum(woba_denom_na_rm),
    #Cumulative K %
    K_cum = cumsum(events %in% c("strikeout", "strikeout_double_play")) / PA_cum,
    #Cumulative BB %
    BB_cum = cumsum(events %in% c("walk", "hit_by_pitch")) / PA_cum,
    # Cumulative O-Swing % (Outside the Zone Swing %)
    O_Swing_cum = cumsum(swing & in_zone == 0) / cumsum(in_zone == 0),
    # Cumulative Z-Swing % (Inside the Zone Swing %)
    Z_Swing_cum = cumsum(swing & in_zone == 1) / cumsum(in_zone == 1),
    # Cumulative Aggressive %
    Aggressive_cum = cumsum(swing & low_prob == 1) / cumsum(low_prob == 1),
    # Cumulative Optimal %
    Optimal_cum = cummean(optimal),
    # Cumulative Swings Over Expected
    SOE_cum = cumsum(swing) - cumsum(prob_RF_swing),
    # Cumulative sREA (Swing Run Expectancy Added)
    sREA_cum = cumsum(ifelse(swing == 1, delta_run_exp, 0)),
    # Cumulative tREA (Take Run Expectancy Added)
    tREA_cum = cumsum(ifelse(swing == 0, delta_run_exp, 0)),
    # Cumulative xREA (Expected Run Expectancy Added)
    xREA_cum = cumsum(exp_delta_run_exp),
    # Cumulative dxREA (Decision Expected Run Expectancy Added)
    dxREA_cum = cumsum(dec_delta_run_exp),
    # Cumulative REA (Run Expectancy Added)
    REA_cum = cumsum(delta_run_exp)
  ) %>%
  ungroup()


#-------------------------------------------------------------------------------
# Empirical Bayes Shrinkage
#-------------------------------------------------------------------------------

# Load beta shrinkage function

beta_shrinkage_individual_prior <- function(df, col, prior_col, n_col, alpha_n) {
  #' Apply empirical Bayes shrinkage using the Beta distribution as a prior
  #' 
  #' @author Jonathan Waring 
  #'
  #' @description This function takes as input a data.frame and column specifications for calculating a 
  #' regressed version of a given column using empirical Bayes shrinkage. 
  #' 
  #' @param df A data frame or data frame extension (e.g. a tibble) object 
  #' @param col Column name in df you wish to regression to the mean
  #' @param prior_col Column name in df which contains the prior mean to shrink to 
  #' @param n_col Column name in df which contains the sample size of each row identifier
  #' @param alpha_n Cutoff point for sample size inclusion in mean/variance calculation of empirical prior
  #' 
  #' @return The same df object but with the added shrunken variable
  #' 
  #' @references https://kiwidamien.github.io/shrinkage-and-empirical-bayes-to-improve-inference.html
  
  # individual means
  mu <- df[[prior_col]] 
  # population variance
  var <- df %>% 
    filter(get(n_col)>=alpha_n) %>% 
    summarize(var=var(get(col),na.rm = T)) %>% 
    .$var
  # adjust for small sample sizes when alpha_n is large
  if(is.na(var)) {
    var <- df %>% 
      filter(get(n_col)>=alpha_n/2) %>% 
      summarize(var=var(get(col),na.rm = T)) %>% 
      .$var
  }
  # calculate beta distribution prior parameters from population mean and variance
  alpha0 <- (((1-mu)/var)-(1/mu)) * mu**2
  beta0 <- alpha0 * ((1/mu)-1)
  # get individual estimates of beta distribution parameters 
  alpha <- df[[col]] * df[[n_col]]
  beta <- df[[n_col]]
  # create new col with shrunken version of stat
  # new_col <- paste(col,"_eb_shrinkage",sep="")
  # new_df <- df %>% mutate(!!(new_col):=(alpha+alpha0)/(beta+alpha0+beta0))
  value <- (alpha+alpha0)/(beta+alpha0+beta0)
  return(value)
  
}


# Calculate Priors

full_data_cum <- full_data_cum %>%
  mutate(
    wOBA_prior = sum(woba_value_na_rm, na.rm = TRUE) / sum(woba_denom_na_rm, na.rm = TRUE),
    K_prior = sum(events %in% c("strikeout", "strikeout_double_play"), na.rm = TRUE) / sum(!is.na(events)),
    BB_prior = sum(events %in% c("walk", "hit_by_pitch"), na.rm = TRUE) / sum(!is.na(events)),
    Swing_prior = mean(swing, na.rm = TRUE),
    O_Swing_prior = sum(swing & in_zone == 0, na.rm = TRUE) / sum(in_zone == 0, na.rm = TRUE),
    Z_Swing_prior = sum(swing & in_zone == 1, na.rm = TRUE) / sum(in_zone == 1, na.rm = TRUE),
    Aggressive_prior = sum(swing & low_prob == 1, na.rm = TRUE) / sum(low_prob == 1, na.rm = TRUE),
    Optimal_prior = mean(optimal, na.rm = TRUE)
         )


# Perform EB Shrinkage

full_data_cum <- full_data_cum %>%
  mutate(
    wOBA_cum_EB = beta_shrinkage_individual_prior(., "wOBA_cum", "wOBA_prior", "PA_cum", 460),
    K_cum_EB = beta_shrinkage_individual_prior(., "K_cum", "K_prior", "PA_cum", 60),
    BB_cum_EB = beta_shrinkage_individual_prior(., "BB_cum", "BB_prior", "PA_cum", 120),
    Swing_cum_EB = beta_shrinkage_individual_prior(., "Swing_cum", "Swing_prior", "PA_cum", 60),
    O_Swing_cum_EB = beta_shrinkage_individual_prior(., "O_Swing_cum", "O_Swing_prior", "PA_cum", 60),
    Z_Swing_cum_EB = beta_shrinkage_individual_prior(., "Z_Swing_cum", "Z_Swing_prior", "PA_cum", 60),
    Aggressive_cum_EB = beta_shrinkage_individual_prior(., "Aggressive_cum", "Aggressive_prior", "PA_cum", 100),
    Optimal_cum_EB = beta_shrinkage_individual_prior(., "Optimal_cum", "Optimal_prior", "PA_cum", 100),
  )

# full_data_cum %>% filter(player_name == "Kwan, Steven") %>% View()



#-------------------------------------------------------------------------------
# Game Context Indicators
#-------------------------------------------------------------------------------

# Context indicators

full_data_complete <- full_data_cum %>%
  arrange(ymd(game_date), batter, at_bat_number, pitch_number) %>%
  mutate(
    p_type = case_when(
      description %in% c("ball", "blocked_ball") ~ "ball",
      description %in% c("called_strike", "foul", "foul_tip", "swinging_strike", "swinging_strike_blocked", "bunt_foul_tip", "foul_bunt", "missed_bunt") ~ "strike",
      TRUE ~ "other"
    )
  ) %>%
  group_by(game_pk, batter, at_bat_number) %>%
  mutate(
    # Encode as character for rle
    consecutive_count = sequence(rle(as.character(p_type))$lengths)
  ) %>%
  group_by(game_pk, batter, at_bat_number) %>%
  mutate(
    # Separating out the counts for balls and strikes
    consecutive_balls = if_else(p_type == "ball", consecutive_count, 0),
    consecutive_balls_pre = lag(consecutive_balls, default = 0),
    consecutive_strikes = if_else(p_type == "strike", consecutive_count, 0),
    consecutive_strikes_pre = lag(consecutive_strikes, default = 0)
  ) %>%
  ungroup() %>%
  # Group by game and batter to work with at-bat levels
  group_by(game_pk, batter, at_bat_number) %>%
  mutate(
    # Identify the last pitch description and outcome of the current at-bat
    current_AB_last_description = last(description),
    current_AB_last_outcome = last(events)
  ) %>%
  ungroup() %>%
  
  # Shift these to represent the previous at-bat for the next at-bat
  group_by(game_pk, batter) %>%
  mutate(
    previous_AB_description = lag(current_AB_last_description),
    previous_AB_outcome = lag(current_AB_last_outcome)
  ) %>%
  ungroup() %>%
  
  # Apply previous at-bat attributes to all pitches within the current at-bat
  group_by(game_pk, batter, at_bat_number) %>%
  mutate(
    # Since the value is constant within an at-bat, use first() to ensure uniformity
    previous_AB_description = first(previous_AB_description),
    previous_AB_outcome = first(previous_AB_outcome),
    
    # Create binary indicators for the previous at-bat's outcome
    previous_AB_hit = as.numeric(previous_AB_outcome %in% c("single", "double", "triple", "home_run")),
    previous_AB_walk = as.numeric(previous_AB_outcome %in% c("walk", "hit_by_pitch")),
    previous_AB_BIP = as.numeric(previous_AB_description == "hit_into_play" & !is.na(previous_AB_description)),
    previous_AB_SO = as.numeric(previous_AB_outcome %in% c("strikeout", "strikeout_double_play")),
    
    # First pitch indicators for the current at-bat
    first_pitch_description = first(description),
    first_pitch_called_strike = as.numeric(first_pitch_description == "called_strike"),
    first_pitch_strike = as.numeric(first_pitch_description %in% c("called_strike", "foul", "foul_tip", "swinging_strike", "swinging_strike_blocked", "bunt_foul_tip", "foul_bunt", "missed_bunt")),
    first_pitch_swing_and_miss = as.numeric(first_pitch_description %in% c("swinging_strike", "swinging_strike_blocked")),
    first_pitch_ball = as.numeric(first_pitch_description %in% c("ball", "blocked_ball")),
    
    # Indicators for the previous pitch within the same at-bat
    previous_pitch_called_strike = if_else(is.na(lag(description)), 0, as.numeric(lag(description) == "called_strike")),
    previous_pitch_strike = if_else(is.na(lag(description)), 0, as.numeric(lag(description) %in% c("called_strike", "foul", "foul_tip", "swinging_strike", "swinging_strike_blocked", "bunt_foul_tip", "foul_bunt", "missed_bunt"))),
    previous_pitch_swing_and_miss = if_else(is.na(lag(description)), 0, as.numeric(lag(description) %in% c("swinging_strike", "swinging_strike_blocked"))),
    previous_pitch_ball = if_else(is.na(lag(description)), 0, as.numeric(lag(description) %in% c("ball", "blocked_ball")))
  ) %>%
  ungroup() %>%
  
  # Add game context indicators
  mutate(
    trailing = as.numeric(bat_score < fld_score),
    run_differential = bat_score - fld_score,
    high_leverage = as.numeric(run_differential <= 3 & inning >= 7),
  )

#full_data_complete %>% filter(player_name == "Bryant, Kris") %>% view()

#write_csv(full_data_complete, "full_data_complete.csv")


#-------------------------------------------------------------------------------
# Summary Stats
#-------------------------------------------------------------------------------

hot_hand_summary <- full_data_complete %>%
  filter(!is.infinite(wOBA_cum_EB) & !is.na(wOBA_cum_EB)) %>%
  mutate(game_ab = paste(game_pk, at_bat_number, sep = "")) %>%
  group_by(previous_AB_hit) %>%
  summarise(PA = n_distinct(game_ab),
            Pitches = n(),
            `Pitches/PA` = n()/n_distinct(game_ab),
            `Player wOBA` = mean(wOBA_cum_EB, na.rm = TRUE),
            `Player K %` = mean(K_cum_EB, na.rm = TRUE),
            `Player BB %` = mean(BB_cum_EB, na.rm = TRUE),
            `In-Zone %` = mean(in_zone, na.rm = TRUE),
            `Swing Prob` = mean(prob_RF_swing, na.rm = TRUE),
            `Player Swing %` = mean(Swing_cum_EB, na.rm = TRUE),
            `Swing %` = mean(swing, na.rm = TRUE),
            `Player O-Swing %` = mean(O_Swing_cum_EB, na.rm = TRUE),
            `O-Swing %` = mean(swing[in_zone == 0], na.rm = TRUE),
            `Player Z-Swing %` = mean(Z_Swing_cum_EB, na.rm = TRUE),
            `Z-Swing %` = mean(swing[in_zone == 1], na.rm = TRUE),
            `Player Aggressive %` = mean(Aggressive_cum_EB, na.rm = TRUE),
            `Aggressive %` = mean(swing[low_prob == 1], na.rm = TRUE),
            `Player Optimal %` = mean(Optimal_cum_EB, na.rm = TRUE),
            `Optimal %` = mean(optimal, na.rm = TRUE),
            `SOE` = sum(swing, na.rm = TRUE) - sum(prob_RF_swing, na.rm = TRUE),
            `sREA` = sum(delta_run_exp[swing == 1], na.rm = TRUE),
            `tREA` = sum(delta_run_exp[swing == 0], na.rm = TRUE),
            `xREA` = sum(exp_delta_run_exp, na.rm = TRUE),
            `dxREA` = sum(dec_delta_run_exp, na.rm = TRUE),
            `REA` = sum(delta_run_exp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`sREA/PA` = sREA/PA,
         `tREA/PA` = tREA/PA,
         `xREA/PA` = xREA/PA,
         `dxREA/PA` = dxREA/PA,
         `REA/PA` = REA/PA)


prospect_summary <- full_data_complete %>%
  filter(!is.infinite(wOBA_cum_EB) & !is.na(wOBA_cum_EB)) %>%
  mutate(game_ab = paste(game_pk, at_bat_number, sep = "")) %>%
  group_by(run_differential) %>%
  summarise(PA = n_distinct(game_ab),
            Pitches = n(),
            `Pitches/PA` = n()/n_distinct(game_ab),
            `Player wOBA` = mean(wOBA_cum_EB, na.rm = TRUE),
            `Player K %` = mean(K_cum_EB, na.rm = TRUE),
            `Player BB %` = mean(BB_cum_EB, na.rm = TRUE),
            `In-Zone %` = mean(in_zone, na.rm = TRUE),
            `Swing Prob` = mean(prob_RF_swing, na.rm = TRUE),
            `Player Swing %` = mean(Swing_cum_EB, na.rm = TRUE),
            `Swing %` = mean(swing, na.rm = TRUE),
            `Player O-Swing %` = mean(O_Swing_cum_EB, na.rm = TRUE),
            `O-Swing %` = mean(swing[in_zone == 0], na.rm = TRUE),
            `Player Z-Swing %` = mean(Z_Swing_cum_EB, na.rm = TRUE),
            `Z-Swing %` = mean(swing[in_zone == 1], na.rm = TRUE),
            `Player Aggressive %` = mean(Aggressive_cum_EB, na.rm = TRUE),
            `Aggressive %` = mean(swing[low_prob == 1], na.rm = TRUE),
            `Player Optimal %` = mean(Optimal_cum_EB, na.rm = TRUE),
            `Optimal %` = mean(optimal, na.rm = TRUE),
            `SOE` = sum(swing, na.rm = TRUE) - sum(prob_RF_swing, na.rm = TRUE),
            `sREA` = sum(delta_run_exp[swing == 1], na.rm = TRUE),
            `tREA` = sum(delta_run_exp[swing == 0], na.rm = TRUE),
            `xREA` = sum(exp_delta_run_exp, na.rm = TRUE),
            `dxREA` = sum(dec_delta_run_exp, na.rm = TRUE),
            `REA` = sum(delta_run_exp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`sREA/PA` = sREA/PA,
         `tREA/PA` = tREA/PA,
         `xREA/PA` = xREA/PA,
         `dxREA/PA` = dxREA/PA,
         `REA/PA` = REA/PA)

prospect_summary2 <- full_data_complete %>%
  filter(!is.infinite(wOBA_cum_EB) & !is.na(wOBA_cum_EB)) %>%
  mutate(game_ab = paste(game_pk, at_bat_number, sep = "")) %>%
  group_by(trailing) %>%
  summarise(PA = n_distinct(game_ab),
            Pitches = n(),
            `Pitches/PA` = n()/n_distinct(game_ab),
            `Player wOBA` = mean(wOBA_cum_EB, na.rm = TRUE),
            `Player K %` = mean(K_cum_EB, na.rm = TRUE),
            `Player BB %` = mean(BB_cum_EB, na.rm = TRUE),
            `In-Zone %` = mean(in_zone, na.rm = TRUE),
            `Swing Prob` = mean(prob_RF_swing, na.rm = TRUE),
            `Player Swing %` = mean(Swing_cum_EB, na.rm = TRUE),
            `Swing %` = mean(swing, na.rm = TRUE),
            `Player O-Swing %` = mean(O_Swing_cum_EB, na.rm = TRUE),
            `O-Swing %` = mean(swing[in_zone == 0], na.rm = TRUE),
            `Player Z-Swing %` = mean(Z_Swing_cum_EB, na.rm = TRUE),
            `Z-Swing %` = mean(swing[in_zone == 1], na.rm = TRUE),
            `Player Aggressive %` = mean(Aggressive_cum_EB, na.rm = TRUE),
            `Aggressive %` = mean(swing[low_prob == 1], na.rm = TRUE),
            `Player Optimal %` = mean(Optimal_cum_EB, na.rm = TRUE),
            `Optimal %` = mean(optimal, na.rm = TRUE),
            `SOE` = sum(swing, na.rm = TRUE) - sum(prob_RF_swing, na.rm = TRUE),
            `sREA` = sum(delta_run_exp[swing == 1], na.rm = TRUE),
            `tREA` = sum(delta_run_exp[swing == 0], na.rm = TRUE),
            `xREA` = sum(exp_delta_run_exp, na.rm = TRUE),
            `dxREA` = sum(dec_delta_run_exp, na.rm = TRUE),
            `REA` = sum(delta_run_exp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`sREA/PA` = sREA/PA,
         `tREA/PA` = tREA/PA,
         `xREA/PA` = xREA/PA,
         `dxREA/PA` = dxREA/PA,
         `REA/PA` = REA/PA)

anchoring_summary_strike <- full_data_complete %>%
  filter(!is.infinite(wOBA_cum_EB) & !is.na(wOBA_cum_EB)) %>%
  filter(pitch_number > 1) %>%
  mutate(game_ab = paste(game_pk, at_bat_number, sep = "")) %>%
  group_by(first_pitch_strike) %>%
  summarise(PA = n_distinct(game_ab),
            Pitches = n(),
            `Pitches/PA` = n()/n_distinct(game_ab),
            `Player wOBA` = mean(wOBA_cum_EB, na.rm = TRUE),
            `Player K %` = mean(K_cum_EB, na.rm = TRUE),
            `Player BB %` = mean(BB_cum_EB, na.rm = TRUE),
            `In-Zone %` = mean(in_zone, na.rm = TRUE),
            `Swing Prob` = mean(prob_RF_swing, na.rm = TRUE),
            `Player Swing %` = mean(Swing_cum_EB, na.rm = TRUE),
            `Swing %` = mean(swing, na.rm = TRUE),
            `Player O-Swing %` = mean(O_Swing_cum_EB, na.rm = TRUE),
            `O-Swing %` = mean(swing[in_zone == 0], na.rm = TRUE),
            `Player Z-Swing %` = mean(Z_Swing_cum_EB, na.rm = TRUE),
            `Z-Swing %` = mean(swing[in_zone == 1], na.rm = TRUE),
            `Player Aggressive %` = mean(Aggressive_cum_EB, na.rm = TRUE),
            `Aggressive %` = mean(swing[low_prob == 1], na.rm = TRUE),
            `Player Optimal %` = mean(Optimal_cum_EB, na.rm = TRUE),
            `Optimal %` = mean(optimal, na.rm = TRUE),
            `SOE` = sum(swing, na.rm = TRUE) - sum(prob_RF_swing, na.rm = TRUE),
            `sREA` = sum(delta_run_exp[swing == 1], na.rm = TRUE),
            `tREA` = sum(delta_run_exp[swing == 0], na.rm = TRUE),
            `xREA` = sum(exp_delta_run_exp, na.rm = TRUE),
            `dxREA` = sum(dec_delta_run_exp, na.rm = TRUE),
            `REA` = sum(delta_run_exp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`sREA/PA` = sREA/PA,
         `tREA/PA` = tREA/PA,
         `xREA/PA` = xREA/PA,
         `dxREA/PA` = dxREA/PA,
         `REA/PA` = REA/PA)


anchoring_summary_ball <- full_data_complete %>%
  filter(!is.infinite(wOBA_cum_EB) & !is.na(wOBA_cum_EB)) %>%
  filter(pitch_number > 1) %>%
  mutate(game_ab = paste(game_pk, at_bat_number, sep = "")) %>%
  group_by(first_pitch_ball) %>%
  summarise(PA = n_distinct(game_ab),
            Pitches = n(),
            `Pitches/PA` = n()/n_distinct(game_ab),
            `Player wOBA` = mean(wOBA_cum_EB, na.rm = TRUE),
            `Player K %` = mean(K_cum_EB, na.rm = TRUE),
            `Player BB %` = mean(BB_cum_EB, na.rm = TRUE),
            `In-Zone %` = mean(in_zone, na.rm = TRUE),
            `Swing Prob` = mean(prob_RF_swing, na.rm = TRUE),
            `Player Swing %` = mean(Swing_cum_EB, na.rm = TRUE),
            `Swing %` = mean(swing, na.rm = TRUE),
            `Player O-Swing %` = mean(O_Swing_cum_EB, na.rm = TRUE),
            `O-Swing %` = mean(swing[in_zone == 0], na.rm = TRUE),
            `Player Z-Swing %` = mean(Z_Swing_cum_EB, na.rm = TRUE),
            `Z-Swing %` = mean(swing[in_zone == 1], na.rm = TRUE),
            `Player Aggressive %` = mean(Aggressive_cum_EB, na.rm = TRUE),
            `Aggressive %` = mean(swing[low_prob == 1], na.rm = TRUE),
            `Player Optimal %` = mean(Optimal_cum_EB, na.rm = TRUE),
            `Optimal %` = mean(optimal, na.rm = TRUE),
            `SOE` = sum(swing, na.rm = TRUE) - sum(prob_RF_swing, na.rm = TRUE),
            `sREA` = sum(delta_run_exp[swing == 1], na.rm = TRUE),
            `tREA` = sum(delta_run_exp[swing == 0], na.rm = TRUE),
            `xREA` = sum(exp_delta_run_exp, na.rm = TRUE),
            `dxREA` = sum(dec_delta_run_exp, na.rm = TRUE),
            `REA` = sum(delta_run_exp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`sREA/PA` = sREA/PA,
         `tREA/PA` = tREA/PA,
         `xREA/PA` = xREA/PA,
         `dxREA/PA` = dxREA/PA,
         `REA/PA` = REA/PA)

representativeness_summary_strike <- full_data_complete %>%
  filter(!is.infinite(wOBA_cum_EB) & !is.na(wOBA_cum_EB)) %>%
  mutate(game_ab = paste(game_pk, at_bat_number, sep = "")) %>%
  group_by(previous_pitch_strike) %>%
  summarise(PA = n_distinct(game_ab),
            Pitches = n(),
            `Pitches/PA` = n()/n_distinct(game_ab),
            `Player wOBA` = mean(wOBA_cum_EB, na.rm = TRUE),
            `Player K %` = mean(K_cum_EB, na.rm = TRUE),
            `Player BB %` = mean(BB_cum_EB, na.rm = TRUE),
            `In-Zone %` = mean(in_zone, na.rm = TRUE),
            `Swing Prob` = mean(prob_RF_swing, na.rm = TRUE),
            `Player Swing %` = mean(Swing_cum_EB, na.rm = TRUE),
            `Swing %` = mean(swing, na.rm = TRUE),
            `Player O-Swing %` = mean(O_Swing_cum_EB, na.rm = TRUE),
            `O-Swing %` = mean(swing[in_zone == 0], na.rm = TRUE),
            `Player Z-Swing %` = mean(Z_Swing_cum_EB, na.rm = TRUE),
            `Z-Swing %` = mean(swing[in_zone == 1], na.rm = TRUE),
            `Player Aggressive %` = mean(Aggressive_cum_EB, na.rm = TRUE),
            `Aggressive %` = mean(swing[low_prob == 1], na.rm = TRUE),
            `Player Optimal %` = mean(Optimal_cum_EB, na.rm = TRUE),
            `Optimal %` = mean(optimal, na.rm = TRUE),
            `SOE` = sum(swing, na.rm = TRUE) - sum(prob_RF_swing, na.rm = TRUE),
            `sREA` = sum(delta_run_exp[swing == 1], na.rm = TRUE),
            `tREA` = sum(delta_run_exp[swing == 0], na.rm = TRUE),
            `xREA` = sum(exp_delta_run_exp, na.rm = TRUE),
            `dxREA` = sum(dec_delta_run_exp, na.rm = TRUE),
            `REA` = sum(delta_run_exp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`sREA/PA` = sREA/PA,
         `tREA/PA` = tREA/PA,
         `xREA/PA` = xREA/PA,
         `dxREA/PA` = dxREA/PA,
         `REA/PA` = REA/PA)


representativeness_summary_ball <- full_data_complete %>%
  filter(!is.infinite(wOBA_cum_EB) & !is.na(wOBA_cum_EB)) %>%
  mutate(game_ab = paste(game_pk, at_bat_number, sep = "")) %>%
  group_by(previous_pitch_ball) %>%
  summarise(PA = n_distinct(game_ab),
            Pitches = n(),
            `Pitches/PA` = n()/n_distinct(game_ab),
            `Player wOBA` = mean(wOBA_cum_EB, na.rm = TRUE),
            `Player K %` = mean(K_cum_EB, na.rm = TRUE),
            `Player BB %` = mean(BB_cum_EB, na.rm = TRUE),
            `In-Zone %` = mean(in_zone, na.rm = TRUE),
            `Swing Prob` = mean(prob_RF_swing, na.rm = TRUE),
            `Player Swing %` = mean(Swing_cum_EB, na.rm = TRUE),
            `Swing %` = mean(swing, na.rm = TRUE),
            `Player O-Swing %` = mean(O_Swing_cum_EB, na.rm = TRUE),
            `O-Swing %` = mean(swing[in_zone == 0], na.rm = TRUE),
            `Player Z-Swing %` = mean(Z_Swing_cum_EB, na.rm = TRUE),
            `Z-Swing %` = mean(swing[in_zone == 1], na.rm = TRUE),
            `Player Aggressive %` = mean(Aggressive_cum_EB, na.rm = TRUE),
            `Aggressive %` = mean(swing[low_prob == 1], na.rm = TRUE),
            `Player Optimal %` = mean(Optimal_cum_EB, na.rm = TRUE),
            `Optimal %` = mean(optimal, na.rm = TRUE),
            `SOE` = sum(swing, na.rm = TRUE) - sum(prob_RF_swing, na.rm = TRUE),
            `sREA` = sum(delta_run_exp[swing == 1], na.rm = TRUE),
            `tREA` = sum(delta_run_exp[swing == 0], na.rm = TRUE),
            `xREA` = sum(exp_delta_run_exp, na.rm = TRUE),
            `dxREA` = sum(dec_delta_run_exp, na.rm = TRUE),
            `REA` = sum(delta_run_exp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`sREA/PA` = sREA/PA,
         `tREA/PA` = tREA/PA,
         `xREA/PA` = xREA/PA,
         `dxREA/PA` = dxREA/PA,
         `REA/PA` = REA/PA)


#-------------------------------------------------------------------------------
# Behavioral Data Visualization
#-------------------------------------------------------------------------------

# Line graphs (representativeness heuristic)

full_data_complete %>%
  group_by(consecutive_balls_pre) %>%
  summarize(swing_prob = mean(prob_RF_swing, na.rm = TRUE),
            swing_pct = mean(swing, na.rm = TRUE)) %>%
  pivot_longer(cols = 2:3, names_to = "Measure", values_to = "Percent") %>%
  mutate(Measure = factor(if_else(Measure == "swing_prob", "Ex. Swing %", "Swing %"))) %>%
  ggplot() + 
  geom_line(aes(x = consecutive_balls_pre, y = Percent, color = Measure), size = 2, alpha = 0.5) +
  geom_line(aes(x = consecutive_balls_pre, y = Percent), .%>% filter(consecutive_balls_pre == 3), 
            linetype = "dashed", color = "deepskyblue") +
  geom_point(aes(x = consecutive_balls_pre, y = Percent, color = Measure), size = 3) +
  scale_y_continuous(limits = c(0.3, 0.6)) +
  scale_fill_manual(values = c("deepskyblue", "palevioletred"),
                    name = "Measure",
                    labels = c("Ex. Swing %", "Swing %")) +
  scale_color_manual(values = c("deepskyblue", "palevioletred"),
                     name = "Measure",
                     labels = c("Ex. Swing %", "Swing %")) +
  labs(x = "Consecutive Balls Preceding Swing Decision", 
       y = "Value") +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, 'cm'),
        plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5))
    

full_data_complete %>%
  group_by(consecutive_strikes_pre) %>%
  summarize(swing_prob = mean(prob_RF_swing, na.rm = TRUE),
            swing_pct = mean(swing, na.rm = TRUE)) %>%
  pivot_longer(cols = 2:3, names_to = "Measure", values_to = "Percent") %>%
  mutate(Measure = factor(if_else(Measure == "swing_prob", "Ex. Swing %", "Swing %"))) %>%
  filter(consecutive_strikes_pre < 7) %>%
  ggplot() + 
  geom_line(aes(x = consecutive_strikes_pre, y = Percent, color = Measure), size = 2, alpha = 0.5) +
  geom_line(aes(x = consecutive_strikes_pre, y = Percent), .%>% filter(consecutive_strikes_pre == 3), 
            linetype = "dashed", color = "palevioletred") +
  geom_line(aes(x = consecutive_strikes_pre, y = Percent), .%>% filter(consecutive_strikes_pre == 4), 
            linetype = "dashed", color = "palevioletred") +
  geom_line(aes(x = consecutive_strikes_pre, y = Percent), .%>% filter(consecutive_strikes_pre == 5), 
            linetype = "dashed", color = "palevioletred") +
  geom_line(aes(x = consecutive_strikes_pre, y = Percent), .%>% filter(consecutive_strikes_pre == 6), 
            linetype = "dashed", color = "palevioletred") +
  geom_point(aes(x = consecutive_strikes_pre, y = Percent, color = Measure), size = 3) +
  scale_y_continuous(limits = c(0.4, 0.7)) +
  scale_x_continuous(n.breaks = 7) +
  scale_fill_manual(values = c("deepskyblue", "palevioletred"),
                    name = "Measure",
                    labels = c("Ex. Swing %", "Swing %")) +
  scale_color_manual(values = c("deepskyblue", "palevioletred"),
                     name = "Measure",
                     labels = c("Ex. Swing %", "Swing %")) +
  labs(x = "Consecutive Strikes Preceding Swing Decision", 
       y = "Value") +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, 'cm'),
        plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5))


# Line graph (prospect theory)

prospect_summary %>%
  filter(abs(run_differential) <= 5) %>%
  ggplot() + 
  geom_line(aes(x = run_differential, y = `Aggressive %`), size = 2, alpha = 0.5) +
  geom_point(aes(x = run_differential, y = `Aggressive %`), size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Run Differential", 
       y = "Aggressive Swing %") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 0.4)) +
  scale_x_continuous(n.breaks = 10) +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, 'cm'),
        plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5))




# Empirical Bayes Shrinkage

full_data_complete %>%
  dplyr::select(Aggressive_cum, Aggressive_cum_EB) %>%
  pivot_longer(cols = everything(), names_to = "EB", values_to = "Aggressive") %>%
  mutate(EB = factor(if_else(EB == "Aggressive_cum", "No EB Shrinkage", "EB Shrinkage"))) %>%
  ggplot(aes(x = Aggressive, fill = EB, color = EB)) +
  geom_density(alpha = 0.25, bw = 0.01, linewidth = 0.8) +
  scale_x_continuous(limits = c(0, 0.5)) +
  labs(y = "Density",
       x = "Aggressive Swing %",
       fill = "Treatment",
       color = "Treatment") +
  scale_fill_manual(values = c("No EB Shrinkage" = "deepskyblue", "EB Shrinkage" = "palevioletred")) +
  scale_color_manual(values = c("No EB Shrinkage" = "deepskyblue", "EB Shrinkage" = "palevioletred")) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, 'cm'),
        plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5))


# Anchoring Bias Bar Graph

anchoring_summary_strike %>%
  mutate(first_pitch_strike = as.factor(first_pitch_strike)) %>%
  mutate(first_pitch_strike = factor(if_else(first_pitch_strike == 0, "Ball", "Strike"))) %>%
  ggplot() + 
  geom_col(aes(x = first_pitch_strike, y = `Swing %`, color = first_pitch_strike, fill = first_pitch_strike), alpha = 0.5) +
  geom_point(aes(x = first_pitch_strike, y = `Swing Prob`, color = first_pitch_strike, fill = first_pitch_strike), size = 14) +
  geom_text(aes(x = first_pitch_strike, y = `Swing Prob`), color = "white", label = "Ex.%") +
  geom_text(aes(x = first_pitch_strike, y = (`Swing Prob`/2)), color = "white", label = "Obs.%") +
  labs(x = "First Pitch Result", 
       y = "Swing %") +
  theme_classic() +
  scale_fill_manual(values = c("Ball" = "deepskyblue", "Strike" = "palevioletred")) +
  scale_color_manual(values = c("Ball" = "deepskyblue", "Strike" = "palevioletred")) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position = "none",
        plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5))


# Prospect Bar Graph

prospect_summary2 %>%
  mutate(trailing = as.factor(trailing)) %>%
  mutate(trailing = fct_rev(factor(if_else(trailing == 0, "Winning (or Tied)", "Trailing")))) %>%
  ggplot() + 
  geom_col(aes(x = trailing, y = `Swing %`, color = trailing, fill = trailing), alpha = 0.5) +
  geom_point(aes(x = trailing, y = `Swing Prob`, color = trailing, fill = trailing), size = 14) +
  geom_text(aes(x = trailing, y = `Swing Prob`), color = "white", label = "Ex.%") +
  geom_text(aes(x = trailing, y = (`Swing Prob`/2)), color = "white", label = "Obs.%") +
  labs(x = "Game Context", 
       y = "Swing %") +
  theme_classic() +
  scale_fill_manual(values = c("Winning (or Tied)" = "deepskyblue", "Trailing" = "palevioletred")) +
  scale_color_manual(values = c("Winning (or Tied)" = "deepskyblue", "Trailing" = "palevioletred")) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position = "none",
        plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5))



#-------------------------------------------------------------------------------
# Behavioral Models
#-------------------------------------------------------------------------------

# Sample Data

mod_sample <- full_data_complete %>%
  filter(!is.infinite(wOBA_cum_EB) & !is.na(wOBA_cum_EB)) %>%
  filter(!is.na(K_cum)) %>%
  filter(!is.na(BB_cum)) %>%
  filter(!is.na(O_Swing_cum)) %>%
  filter(!is.na(Z_Swing_cum)) %>%
  filter(!is.na(Aggressive_cum)) %>%
  filter(!is.na(Optimal_cum)) %>%
  filter(!is.na(SOE_cum)) %>%
  filter(!is.na(sREA_cum)) %>%
  filter(!is.na(tREA_cum)) %>%
  filter(!is.na(xREA_cum)) %>%
  filter(!is.na(dxREA_cum)) %>%
  filter(!is.na(REA_cum)) %>%
  filter(!is.na(wOBA_prior)) %>%
  filter(!is.na(K_prior)) %>%
  filter(!is.na(BB_prior)) %>%
  filter(!is.na(Swing_prior)) %>%
  filter(!is.na(O_Swing_prior)) %>%
  filter(!is.na(Z_Swing_prior)) %>%
  filter(!is.na(Aggressive_prior)) %>%
  filter(!is.na(Optimal_prior)) %>%
  filter(!is.na(wOBA_cum_EB)) %>%
  filter(!is.na(K_cum_EB)) %>%
  filter(!is.na(BB_cum_EB)) %>%
  filter(!is.na(Swing_cum_EB)) %>%
  filter(!is.na(O_Swing_cum_EB)) %>%
  filter(!is.na(Z_Swing_cum_EB)) %>%
  filter(!is.na(Aggressive_cum_EB)) %>%
  filter(!is.na(Optimal_cum_EB)) %>%
  rename(count_balls = balls,
         count_strikes = strikes) %>%
  sample_n(size = 100000)


mod_test <- full_data_complete %>%
  filter(!is.infinite(wOBA_cum_EB) & !is.na(wOBA_cum_EB)) %>%
  filter(!is.na(K_cum)) %>%
  filter(!is.na(BB_cum)) %>%
  filter(!is.na(O_Swing_cum)) %>%
  filter(!is.na(Z_Swing_cum)) %>%
  filter(!is.na(Aggressive_cum)) %>%
  filter(!is.na(Optimal_cum)) %>%
  filter(!is.na(SOE_cum)) %>%
  filter(!is.na(sREA_cum)) %>%
  filter(!is.na(tREA_cum)) %>%
  filter(!is.na(xREA_cum)) %>%
  filter(!is.na(dxREA_cum)) %>%
  filter(!is.na(REA_cum)) %>%
  filter(!is.na(wOBA_prior)) %>%
  filter(!is.na(K_prior)) %>%
  filter(!is.na(BB_prior)) %>%
  filter(!is.na(Swing_prior)) %>%
  filter(!is.na(O_Swing_prior)) %>%
  filter(!is.na(Z_Swing_prior)) %>%
  filter(!is.na(Aggressive_prior)) %>%
  filter(!is.na(Optimal_prior)) %>%
  filter(!is.na(wOBA_cum_EB)) %>%
  filter(!is.na(K_cum_EB)) %>%
  filter(!is.na(BB_cum_EB)) %>%
  filter(!is.na(Swing_cum_EB)) %>%
  filter(!is.na(O_Swing_cum_EB)) %>%
  filter(!is.na(Z_Swing_cum_EB)) %>%
  filter(!is.na(Aggressive_cum_EB)) %>%
  filter(!is.na(Optimal_cum_EB)) %>%
  rename(count_balls = balls,
         count_strikes = strikes) %>%
  anti_join(mod_sample) %>%
  sample_n(size = 30000)

# Linear Regression Model (Swings)

linear_model <- lm(swing ~ prob_RF_swing + count_balls + count_strikes + pitch_number + Swing_cum_EB + Aggressive_cum_EB + 
                      Optimal_cum_EB + previous_AB_hit + previous_AB_walk +
                      previous_AB_SO + 
                     consecutive_balls_pre + I(consecutive_balls_pre^2) + consecutive_strikes_pre +
                      previous_pitch_ball + previous_pitch_strike + trailing, 
                    data = mod_sample)

summary(linear_model)


# Logistic Regression Model (Swings)

logistic_model <- glm(swing ~ prob_RF_swing + count_balls + count_strikes + pitch_number + Swing_cum_EB + Aggressive_cum_EB + 
                        Optimal_cum_EB + previous_AB_hit + previous_AB_walk +
                        previous_AB_SO + 
                        consecutive_balls_pre + I(consecutive_balls_pre^2) + consecutive_strikes_pre +
                        previous_pitch_ball + previous_pitch_strike + trailing, 
                      data = mod_sample, family = "binomial")

summary(logistic_model)



# Test Accuracy

prob_test_results <- predict(logistic_model, mod_test, type = "response")
prob_test_results <- as.numeric(prob_test_results)
prob_test_decision <- if_else(prob_test_results >= 0.5, 1, 0)

mean(prob_test_decision == mod_test$swing) #0.780


# ROC Curve for Logistic Regression

roc_curve_swing_prob <- pROC::roc(response = mod_test$swing, predictor = prob_test_results)
pROC::auc(roc_curve_swing_prob) #0.8609

ggroc(roc_curve_swing_prob , legacy.axes = TRUE) + theme_classic() + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color = "grey", linetype = "dashed")



# Residual Plot

fitted_probs <- predict(logistic_model, type = "response")

residuals_deviance <- residuals(logistic_model, type = "deviance", type.residuals = "response")

plot(fitted_probs, residuals_deviance,
     xlab = "Fitted Values", ylab = "Deviance Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "gray", lty = 2) # Add a horizontal line at 0


# Binned Residual Plot

binnedplot(fitted(logistic_model), 
           residuals(logistic_model, type = "response"), 
           nclass = NULL, 
           xlab = "Fitted Values", 
           ylab = "Average Residual", 
           main = "Binned Residuals vs Fitted Values", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")


# Q-Q Plot

qqnorm(residuals_deviance)
qqline(residuals_deviance, col = "gray", lwd = 3, lty = 2)


# Linearity Plots

mod_test$fitted_prob <- predict(logistic_model, newdata = mod_test, type = "response")

mod_test$log_odds <- log(mod_test$fitted_prob / (1 - mod_test$fitted_prob))

mod_test_sample <- mod_test %>% sample_n(size = 10000)


p1 <- ggplot(mod_test_sample , aes(x = prob_RF_swing, y = log_odds)) +
  geom_point(alpha = 0.01) +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Prob. Swing (RF)", y = "Logistic Model Log-Odds") +
  theme_classic() +
  theme(axis.text = element_text(size = 5),
        axis.title.y = element_text(size = 6),
        axis.title.x = element_text(size = 6))

p2 <- ggplot(mod_test_sample , aes(x = pitch_number, y = log_odds)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Pitch of PA", y = "Logistic Model Log-Odds") +
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 5),
        axis.title.x = element_text(size = 6))

p3 <- ggplot(mod_test_sample , aes(x = count_balls, y = log_odds)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(breaks = c(0, 1, 2, 3)) +
  labs(x = "Balls in Count", y = "Logistic Model Log-Odds") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 5),
        axis.title.x = element_text(size = 6))

p4 <- ggplot(mod_test_sample , aes(x = count_strikes, y = log_odds)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(breaks = c(0, 1, 2)) +
  labs(x = "Strikes in Count", y = "Logistic Model Log-Odds") +
  theme_classic() +
  theme(axis.text = element_text(size = 5),
        axis.title.y = element_text(size = 6),
        axis.title.x = element_text(size = 6))

p5 <- ggplot(mod_test_sample , aes(x = Swing_cum_EB, y = log_odds)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Player Swing % (EB)", y = "Logistic Model Log-Odds") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 5),
        axis.title.x = element_text(size = 6))

p6 <- ggplot(mod_test_sample , aes(x = Aggressive_cum_EB, y = log_odds)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Player Aggressive % (EB)", y = "Logistic Model Log-Odds") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 5),
        axis.title.x = element_text(size = 6))

p7 <- ggplot(mod_test_sample , aes(x = Optimal_cum_EB, y = log_odds)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Player Optimal % (EB)", y = "Logistic Model Log-Odds") +
  scale_x_continuous(breaks = c(0.55, 0.60, 0.65)) +
  theme_classic() +
  theme(axis.text = element_text(size = 5),
        axis.title.y = element_text(size = 6),
        axis.title.x = element_text(size = 6))

p8 <- ggplot(mod_test_sample , aes(x = previous_AB_hit, y = log_odds)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(breaks = c(0, 1)) +
  labs(x = "Previous PA Hit", y = "Logistic Model Log-Odds") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 5),
        axis.title.x = element_text(size = 6))

p9 <- ggplot(mod_test_sample , aes(x = previous_AB_walk, y = log_odds)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm", color = "red",) +
  scale_x_continuous(breaks = c(0, 1)) +
  labs(x = "Previous PA BB", y = "Logistic Model Log-Odds") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 5),
        axis.title.x = element_text(size = 6))

p10 <- ggplot(mod_test_sample , aes(x = previous_AB_SO, y = log_odds)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(breaks = c(0, 1)) +
  labs(x = "Previous PA SO", y = "Logistic Model Log-Odds") +
  theme_classic() +
  theme(axis.text = element_text(size = 5),
        axis.title.y = element_text(size = 6),
        axis.title.x = element_text(size = 6))

p11 <- ggplot(mod_test_sample , aes(x = consecutive_balls_pre, y = log_odds)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Consecutive Balls Pre", y = "Logistic Model Log-Odds") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 5),
        axis.title.x = element_text(size = 6))

p12 <- ggplot(mod_test_sample , aes(x = consecutive_strikes_pre, y = log_odds)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Consecutive Strikes Pre", y = "Logistic Model Log-Odds") +
  scale_x_continuous(limits = c(0, 6)) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 5),
        axis.title.x = element_text(size = 6))

p13 <- ggplot(mod_test_sample , aes(x = previous_pitch_ball, y = log_odds)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "loess", color = "red") +
  scale_x_continuous(breaks = c(0, 1)) +
  labs(x = "Previous Pitch Ball", y = "Logistic Model Log-Odds") +
  theme_classic() +
  theme(axis.text = element_text(size = 5),
        axis.title.y = element_text(size = 6),
        axis.title.x = element_text(size = 6))

p14 <- ggplot(mod_test_sample , aes(x = previous_pitch_strike, y = log_odds)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "loess", color = "red") +
  scale_x_continuous(breaks = c(0, 1)) +
  labs(x = "Previous Pitch Strike", y = "Logistic Model Log-Odds") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 5),
        axis.title.x = element_text(size = 6))

p15 <- ggplot(mod_test_sample, aes(x = trailing, y = log_odds)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "loess", color = "red") +
  scale_x_continuous(breaks = c(0, 1)) +
  labs(x = "Trailing", y = "Logistic Model Log-Odds") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 5),
        axis.title.x = element_text(size = 6))

(p1 | p2 | p3 | p4 | p5 | p6 | p7 | p8 | p9 | p10 | p11 | p12 | p13 | p14 | p15) +
plot_layout(ncol = 3, nrow = 5)


# Response Mean

mean(full_data$swing) #0.4753357



#-------------------------------------------------------------------------------
# Count-Independent Swing Probability Model
#-------------------------------------------------------------------------------

# Add Previous Pitch Type Variable

full_data_complete <- full_data_complete %>%
  group_by(game_pk, batter, at_bat_number) %>%
  mutate(previous_pitch_type = if_else(row_number() == 1, "FP", lag(pitch_type))) %>%
  ungroup()


# Modeling Subsets

prob_tune <- full_data_complete %>%
  mutate(swing = as.factor(swing)) %>%
  dplyr::select(plate_z, plate_x, pfx_z, pfx_x, release_speed, sz_bot, sz_top,
                pitch_type, release_extension, outs_when_up, p_throws,
                stand, on_1b, on_2b, on_3b, swing) %>%
  drop_na() %>%
  sample_n(size = 10000)

prob_train <- full_data_complete %>%
  mutate(swing = as.factor(swing)) %>%
  dplyr::select(plate_z, plate_x, pfx_z, pfx_x, release_speed, sz_bot, sz_top,
                pitch_type, release_extension, outs_when_up, p_throws,
                stand, on_1b, on_2b, on_3b, swing) %>%
  drop_na() %>%
  sample_n(size = 30000)

prob_test <- full_data_complete %>%
  mutate(swing = as.factor(swing)) %>%
  dplyr::select(plate_z, plate_x, pfx_z, pfx_x, release_speed, sz_bot, sz_top,
                pitch_type, release_extension, outs_when_up, p_throws,
                stand, on_1b, on_2b, on_3b, swing) %>%
  drop_na() %>%
  anti_join(prob_train) %>%
  sample_n(size = 10000)


# Define Train Control Object

train_control <- trainControl(method = "cv", 
                              number = 10,
                              search = "grid",
                              verboseIter = TRUE)


# Create Tuning Grid

grid <- expand.grid(.mtry = 4:8)


# Perform Cross Validation

tune_prob <- train(swing ~ ., 
                   data = prob_tune, 
                   method = "rf", 
                   metric = "Accuracy",
                   trControl = train_control, 
                   tuneGrid = grid,
                   ntree = 500)


# Cross Validation Results

tune_prob$bestTune$mtry




# Train Model

prob_model_count_ind <- randomForest(swing ~ ., 
                                     data = prob_train,
                                     mtry = 7, 
                                     ntree = 3000)


# Assess Variable Importance

varImpPlot(prob_model_count_ind)




# Test Accuracy

prob_test_results <- predict(prob_model_count_ind, prob_test, type = "prob")
prob_test_results <- prob_test_results[, 2]
prob_test_decision <- if_else(prob_test_results >= 0.5, 1, 0)

mean(prob_test_decision == prob_test$swing) #0.734


# ROC Curve for Random Forest

roc_curve_swing_prob <- roc(response = prob_test$swing, predictor = prob_test_results)
pROC::auc(roc_curve_swing_prob) #0.810

ggroc(roc_curve_swing_prob , legacy.axes = TRUE) + theme_classic() + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color = "grey", linetype = "dashed")


# Random Forest Predictions

prob_RF_swing_count_ind <- predict(prob_model_count_ind, full_data_complete, type = "prob")

prob_RF_swing_count_ind <- prob_RF_swing_count_ind[, 2]

decision_RF_swing_count_ind <- if_else(prob_RF_swing_count_ind >= 0.5, 1, 0)


# Bind Predictions

full_data_complete <- cbind(full_data_complete, prob_RF_swing_count_ind)


# Ahead in Count Variables

full_data_complete <- full_data_complete %>%
  mutate(pitches_ahead = balls - strikes,
         ahead_in_count = ifelse(pitches_ahead > 0, 1, 0),
         behind_in_count = ifelse(pitches_ahead < 0, 1, 0),
         even_in_count = ifelse(pitches_ahead == 0, 1, 0))


#full_data_complete %>% filter(player_name == "Bryant, Kris") %>% view()

#write_csv(full_data_complete, "full_data_complete.csv")


#-------------------------------------------------------------------------------
# Prospect Theory
#-------------------------------------------------------------------------------

# Summary Stats

prospect_count_summary <- full_data_complete %>%
  filter(!is.infinite(wOBA_cum_EB) & !is.na(wOBA_cum_EB)) %>%
  mutate(game_ab = paste(game_pk, at_bat_number, sep = "")) %>%
  mutate(std_pred_re_swing = as.numeric(scale(pred_delta_run_exp_swing))) %>%
  group_by(pitches_ahead) %>%
  summarise(PA = n_distinct(game_ab),
            Pitches = n(),
            `Pitches/PA` = n()/n_distinct(game_ab),
            `Player wOBA` = mean(wOBA_cum_EB, na.rm = TRUE),
            `Player K %` = mean(K_cum_EB, na.rm = TRUE),
            `Player BB %` = mean(BB_cum_EB, na.rm = TRUE),
            `In-Zone %` = mean(in_zone, na.rm = TRUE),
            `Swing Prob` = mean(prob_RF_swing_count_ind, na.rm = TRUE),
            `Player Swing %` = mean(Swing_cum_EB, na.rm = TRUE),
            `Swing %` = mean(swing, na.rm = TRUE),
            `Std. xsREA/Pitch` = mean(as.numeric(std_pred_re_swing), na.rm = TRUE),
            `Player O-Swing %` = mean(O_Swing_cum_EB, na.rm = TRUE),
            `O-Swing %` = mean(swing[in_zone == 0], na.rm = TRUE),
            `Player Z-Swing %` = mean(Z_Swing_cum_EB, na.rm = TRUE),
            `Z-Swing %` = mean(swing[in_zone == 1], na.rm = TRUE),
            `Player Aggressive %` = mean(Aggressive_cum_EB, na.rm = TRUE),
            `Aggressive %` = mean(swing[low_prob == 1], na.rm = TRUE),
            `Player Optimal %` = mean(Optimal_cum_EB, na.rm = TRUE),
            `Optimal %` = mean(optimal, na.rm = TRUE),
            `SOE` = sum(swing, na.rm = TRUE) - sum(prob_RF_swing, na.rm = TRUE),
            `sREA` = sum(delta_run_exp[swing == 1], na.rm = TRUE),
            `tREA` = sum(delta_run_exp[swing == 0], na.rm = TRUE),
            `xREA` = sum(exp_delta_run_exp, na.rm = TRUE),
            `dxREA` = sum(dec_delta_run_exp, na.rm = TRUE),
            `REA` = sum(delta_run_exp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`sREA/PA` = sREA/PA,
         `tREA/PA` = tREA/PA,
         `xREA/PA` = xREA/PA,
         `dxREA/PA` = dxREA/PA,
         `REA/PA` = REA/PA)

# Graph (Ex vs Obs)

full_data_complete %>%
  group_by(pitches_ahead) %>%
  summarize(swing_prob = mean(prob_RF_swing_count_ind, na.rm = TRUE),
            swing_pct = mean(swing, na.rm = TRUE)) %>%
  pivot_longer(cols = 2:3, names_to = "Measure", values_to = "Percent") %>%
  mutate(Measure = factor(if_else(Measure == "swing_prob", "Ex. Swing %", "Swing %"))) %>%
  ggplot() + 
  annotate("segment", x = 0, y = 0, xend = 0, yend = 0.426, alpha = 0.5, linetype = "dashed") +
  annotate("segment", x = 0, y = 0.483, xend = 0, yend = Inf, alpha = 0.5, linetype = "dashed") +
  geom_line(aes(x = pitches_ahead, y = Percent, color = Measure), size = 2, alpha = 0.5) +
  geom_line(aes(x = pitches_ahead, y = Percent), .%>% filter(pitches_ahead == -2), 
            linetype = "dashed", color = "palevioletred") +
  geom_line(aes(x = pitches_ahead, y = Percent), .%>% filter(pitches_ahead == -1), 
            linetype = "dashed", color = "palevioletred") +
  geom_line(aes(x = pitches_ahead, y = Percent), .%>% filter(pitches_ahead == 0), 
            linetype = "dashed", color = "deepskyblue") +
  geom_line(aes(x = pitches_ahead, y = Percent), .%>% filter(pitches_ahead == 2), 
            linetype = "dashed", color = "deepskyblue") +
  geom_line(aes(x = pitches_ahead, y = Percent), .%>% filter(pitches_ahead == 3), 
            linetype = "dashed", color = "deepskyblue") +
  geom_point(aes(x = pitches_ahead, y = Percent, color = Measure), size = 3) +
  coord_cartesian(ylim = c(0.05, 0.65)) +
  scale_x_continuous(n.breaks = 6) +
  scale_fill_manual(values = c("deepskyblue", "palevioletred"),
                    name = "Measure",
                    labels = c("Ex. Swing %", "Swing %")) +
  scale_color_manual(values = c("deepskyblue", "palevioletred"),
                     name = "Measure",
                     labels = c("Ex. Swing %", "Swing %")) +
  labs(x = "Pitches Ahead in Count", 
       y = "Value") +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, 'cm'),
        plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5))


# Graph (Ex vs Utility)

full_data_complete %>%
  mutate(std_pred_re_swing = as.numeric(scale(pred_delta_run_exp_swing)),
         pitch_SOE = swing - prob_RF_swing_count_ind,
         std_pitch_ROE = as.numeric(scale(pitch_SOE))) %>%
  group_by(pitches_ahead) %>%
  summarize(mean_std_pitch_SOE = mean(as.numeric(pitch_SOE), na.rm = TRUE),
            mean_srd_exp_re_swing = mean(as.numeric(std_pred_re_swing), na.rm = TRUE)) %>%
  pivot_longer(cols = 2:3, names_to = "Measure", values_to = "Value") %>%
  mutate(Measure = factor(if_else(Measure != "mean_srd_exp_re_swing", "Mean xsREA", "Mean SOE"))) %>%
  ggplot() + 
  annotate("segment", x = 0, y = -0.5, xend = 0, yend = -0.0567, alpha = 0.5, linetype = "dashed") +
  annotate("segment", x = 0, y = 0.13, xend = 0, yend = Inf, alpha = 0.5, linetype = "dashed") +
  geom_line(aes(x = pitches_ahead, y = Value, color = Measure), size = 2, alpha = 0.5) +
  geom_line(aes(x = pitches_ahead, y = Value), .%>% filter(pitches_ahead == -2), 
            linetype = "dashed", color = "palevioletred") +
  geom_line(aes(x = pitches_ahead, y = Value), .%>% filter(pitches_ahead == -1), 
            linetype = "dashed", color = "palevioletred") +
  geom_line(aes(x = pitches_ahead, y = Value), .%>% filter(pitches_ahead == 1), 
            linetype = "dashed", color = "palevioletred") +
  geom_line(aes(x = pitches_ahead, y = Value), .%>% filter(pitches_ahead == 0), 
            linetype = "dashed", color = "deepskyblue") +
  geom_line(aes(x = pitches_ahead, y = Value), .%>% filter(pitches_ahead == 2), 
            linetype = "dashed", color = "palevioletred") +
  geom_line(aes(x = pitches_ahead, y = Value), .%>% filter(pitches_ahead == 3), 
            linetype = "dashed", color = "deepskyblue") +
  geom_point(aes(x = pitches_ahead, y = Value, color = Measure), size = 3) +
  coord_cartesian(ylim = c(-0.4, 0.15)) +
  scale_x_continuous(n.breaks = 6) +
  scale_fill_manual(values = c("deepskyblue", "palevioletred"),
                    name = "Measure",
                    labels = c("Mean Std. sxREA", "Mean Std. SOE")) +
  scale_color_manual(values = c("deepskyblue", "palevioletred"),
                     name = "Measure",
                     labels = c("Mean Std. sxREA", "Mean Std. SOE")) +
  labs(x = "Pitches Ahead in Count", 
       y = "Value") +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, 'cm'),
        plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5))


#-------------------------------------------------------------------------------
# Prospect Theory Model
#-------------------------------------------------------------------------------

mod_sample <- full_data_complete %>%
  mutate(two_strikes = ifelse(strikes == 2, 1 ,0),
         two_behind = ifelse(pitches_ahead == -2, 1, 0),
         three_ahead = ifelse(pitches_ahead == 3, 1, 0)) %>%
  filter(!is.infinite(wOBA_cum_EB) & !is.na(wOBA_cum_EB)) %>%
  filter(!is.na(K_cum)) %>%
  filter(!is.na(BB_cum)) %>%
  filter(!is.na(O_Swing_cum)) %>%
  filter(!is.na(Z_Swing_cum)) %>%
  filter(!is.na(Aggressive_cum)) %>%
  filter(!is.na(Optimal_cum)) %>%
  filter(!is.na(SOE_cum)) %>%
  filter(!is.na(sREA_cum)) %>%
  filter(!is.na(tREA_cum)) %>%
  filter(!is.na(xREA_cum)) %>%
  filter(!is.na(dxREA_cum)) %>%
  filter(!is.na(REA_cum)) %>%
  filter(!is.na(wOBA_prior)) %>%
  filter(!is.na(K_prior)) %>%
  filter(!is.na(BB_prior)) %>%
  filter(!is.na(Swing_prior)) %>%
  filter(!is.na(O_Swing_prior)) %>%
  filter(!is.na(Z_Swing_prior)) %>%
  filter(!is.na(Aggressive_prior)) %>%
  filter(!is.na(Optimal_prior)) %>%
  filter(!is.na(wOBA_cum_EB)) %>%
  filter(!is.na(K_cum_EB)) %>%
  filter(!is.na(BB_cum_EB)) %>%
  filter(!is.na(Swing_cum_EB)) %>%
  filter(!is.na(O_Swing_cum_EB)) %>%
  filter(!is.na(Z_Swing_cum_EB)) %>%
  filter(!is.na(Aggressive_cum_EB)) %>%
  filter(!is.na(Optimal_cum_EB)) %>%
  rename(count_balls = balls,
         count_strikes = strikes) %>%
  sample_n(size = 100000)

# Linear Regression Model (Swings)

linear_model2 <- lm(swing ~ prob_RF_swing_count_ind + pred_delta_run_exp_swing + Z_Swing_cum_EB +
                     O_Swing_cum_EB + consecutive_balls_pre + I(consecutive_balls_pre^2) + consecutive_strikes_pre + pitch_number +
                     pitches_ahead,
                   data = mod_sample)

summary(linear_model2)


# Logistic Regression Model (Swings)

logistic_model2  <- glm(swing ~ prob_RF_swing_count_ind + pred_delta_run_exp_swing + Z_Swing_cum_EB +
                        O_Swing_cum_EB + consecutive_balls_pre + I(consecutive_balls_pre^2) + consecutive_strikes_pre + pitch_number +
                        pitches_ahead,
                      data = mod_sample, family = "binomial")

summary(logistic_model2)


# Stargazer

stargazer(linear_model2, logistic_model2, type = "latex", header = TRUE, align = TRUE, 
          font.size = "footnotesize", title = "Regression Results --- Prospect Theory", 
          dep.var.labels = c("Swing", "Swing"), 
          covariate.labels = c("Count-Ind. Prob. Swing (RF)",
                               "xREA Swing (vs. Take)", "Player Z-Swing \\% (EB)", 
                               "Player O-Swing \\% (EB)",
                               "Consecutive Balls Pre", "(Consecutive Balls Pre)\\textsuperscript{2}", "Consecutive Strikes Pre",
                               "Pitch of PA", "Pitches Ahead"), 
          notes = c("Covariates representing probabilities or percents are scaled as decimals."),
          omit.stat = c("LL", "ser", "f"), se = NULL)


# Linear Regression Model (Swings)

linear_model3 <- lm(swing ~ prob_RF_swing_count_ind + pred_delta_run_exp_swing + Z_Swing_cum_EB +
                      O_Swing_cum_EB + consecutive_balls_pre + I(consecutive_balls_pre^2) + consecutive_strikes_pre + pitch_number +
                      two_strikes + three_ahead + even_in_count, 
                    data = mod_sample)

summary(linear_model3)


# Logistic Regression Model (Swings)

logistic_model3  <- glm(swing ~ prob_RF_swing_count_ind + pred_delta_run_exp_swing + Z_Swing_cum_EB +
                          O_Swing_cum_EB + consecutive_balls_pre + I(consecutive_balls_pre^2) + consecutive_strikes_pre + pitch_number +
                          two_strikes + three_ahead + even_in_count, 
                        data = mod_sample, family = "binomial")

summary(logistic_model3)


# Stargazer

stargazer(linear_model3, logistic_model3, type = "latex", header = TRUE, align = TRUE, 
          font.size = "footnotesize", title = "Regression Results --- Prospect Theory (State-speficifc Indicators)", 
          dep.var.labels = c("Swing", "Swing"), 
          covariate.labels = c("Count-Ind. Prob. Swing (RF)",
                               "xREA Swing (vs. Take)", "Player Z-Swing \\% (EB)", 
                               "Player O-Swing \\% (EB)",
                               "Consecutive Balls Pre", "(Consecutive Balls Pre)\\textsuperscript{2}", "Consecutive Strikes Pre",
                               "Pitch of PA", "Two-strike Count", "3-0 Count", "Even Count"), 
          notes = c("Covariates representing probabilities or percents are scaled as decimals."),
          omit.stat = c("LL", "ser", "f"), se = NULL)


# Test Accuracy

prob_test_results <- predict(logistic_model3, mod_test, type = "response")
prob_test_results <- as.numeric(prob_test_results)
prob_test_decision <- if_else(prob_test_results >= 0.5, 1, 0)

mean(prob_test_decision == mod_test$swing) #0.771


# ROC Curve for Logistic Regression

roc_curve_swing_prob <- pROC::roc(response = mod_test$swing, predictor = prob_test_results)
pROC::auc(roc_curve_swing_prob) #0.8609

ggroc(roc_curve_swing_prob , legacy.axes = TRUE) + theme_classic() + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color = "grey", linetype = "dashed")



# Residual Plot

fitted_probs <- predict(logistic_model2, type = "response")

residuals_deviance <- residuals(logistic_model2, type = "deviance", type.residuals = "response")

plot(fitted_probs, residuals_deviance,
     xlab = "Fitted Values", ylab = "Deviance Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "gray", lty = 2) # Add a horizontal line at 0


# Binned Residual Plot

binnedplot(fitted(logistic_model), 
           residuals(logistic_model, type = "response"), 
           nclass = NULL, 
           xlab = "Fitted Values", 
           ylab = "Average Residual", 
           main = "Binned Residuals vs Fitted Values", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")


# Q-Q Plot

qqnorm(residuals_deviance)
qqline(residuals_deviance, col = "gray", lwd = 3, lty = 2)


#-------------------------------------------------------------------------------
# Runs Lost Summary
#-------------------------------------------------------------------------------

full_data_complete %>%
  mutate(`All Counts` = 1,  # Assuming you want to include all data points
         `3-0 Counts` = ifelse(count == "3-0", 1, 0), 
         `Even Counts` = ifelse(balls == strikes, 1, 0), 
         `Two-Strike Counts` = ifelse(strikes == 2, 1, 0)) %>%
  # Unpivot to long format to easily group by count context in one go
  pivot_longer(cols = c(`All Counts`, `3-0 Counts`, `Even Counts`, `Two-Strike Counts`),
               names_to = "count_context", values_to = "flag") %>%
  filter(flag == 1) %>%  # Keep only rows where the flag is 1
  group_by(count_context, swing) %>%  # Group by count context and swing decision
  summarize(Pitches = n(), 
            REA = mean(delta_run_exp, na.rm = TRUE), 
            .groups = 'drop')  # Drop grouping

full_data_complete %>% 
  group_by(optimal) %>% 
  summarise(mean = mean(delta_run_exp, na.rm = TRUE), 
            variance = var(delta_run_exp, na.rm = TRUE))

cor(full_data_complete$optimal, full_data_complete$delta_run_exp, use = "complete.obs")
cor(full_data_complete$aggressive, full_data_complete$delta_run_exp, use = "complete.obs")
cor(full_data_complete$exp_delta_run_exp, full_data_complete$delta_run_exp, use = "complete.obs")


# Optimal % by Count (Faceted)

pie_data <- full_data_complete %>%
  group_by(count, swing, optimal) %>%
  summarise(count_n = n(), .groups = 'drop')

ggplot(pie_data, aes(x = "", y = count_n, fill = combined, color = combined, alpha = combined)) +
  geom_bar(stat = "identity", position = "fill", linewidth = 0.25) +  
  coord_polar(theta = "y") +
  facet_wrap(~count, ncol = 3) +  # Facet by count
  scale_fill_manual(values = c("deepskyblue", "deepskyblue","palevioletred", "palevioletred"), 
                    name = "Decision and Optimality", 
                    labels = c("Take (Non-Optimal)", "Take (Optimal)", "Swing (Non-Optimal)", "Swing (Optimal)")) +
  scale_color_manual(values = c("deepskyblue", "deepskyblue","palevioletred", "palevioletred"), 
                    name = "Decision and Optimality", 
                    labels = c("Take (Non-Optimal)", "Take (Optimal)", "Swing (Non-Optimal)", "Swing (Optimal)")) +
  scale_alpha_manual(values = c(0.25, 0.5, 0.25, 0.5), 
                     name = "Decision and Optimality", 
                     labels = c("Take (Non-Optimal)", "Take (Optimal)", "Swing (Non-Optimal)", "Swing (Optimal)")) +
  labs(x = NULL, y = NULL, fill = "Decision and Optimality") +
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid = element_blank(), 
        strip.text = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.5, 'cm'))


#-------------------------------------------------------------------------------
# Swing Decision Tree Summary
#-------------------------------------------------------------------------------

mean(full_data_complete$swing) #0.47

full_data_complete %>%
  group_by(swing) %>%
  summarize(n = n(),
            balls = sum(description %in% c("ball", "blocked_ball", "hit_by_pitch", "pitchout")),
            strikes = sum(description %in% c("called_strike", "swinging_strike", "swinging_strike_blocked")),
            bip = sum(description == "hit_into_play"),
            foul = sum(description %in% c("foul", "foul_tip", "fould_bunt", "bunt_foul_tip"))) %>%
  ungroup() %>%
  mutate(pct_balls = balls / n,
         pct_strikes = strikes / n,
         pct_bip = bip / n,
         pct_foul = foul / n)


# Mean Take -> Strike

full_data_complete %>%
  filter(swing == 0) %>%
  filter(description %in% c("called_strike", "swinging_strike", "swinging_strike_blocked")) %>%
  pull(delta_run_exp) %>%
  mean(na.rm = TRUE) #-0.06


# Mean Take -> Ball

full_data_complete %>%
  filter(swing == 0) %>%
  filter(description %in% c("ball", "blocked_ball", "hit_by_pitch", "pitchout")) %>%
  pull(delta_run_exp) %>%
  mean(na.rm = TRUE) #0.05


# Mean Swing -> Strike

full_data_complete %>%
  filter(swing == 1) %>%
  filter(description %in% c("called_strike", "swinging_strike", "swinging_strike_blocked")) %>%
  pull(delta_run_exp) %>%
  mean(na.rm = TRUE) #0.12


# Mean Swing -> Foul

full_data_complete %>%
  filter(swing == 1) %>%
  filter(description %in% c("foul", "foul_tip", "fould_bunt", "bunt_foul_tip")) %>%
  pull(delta_run_exp) %>%
  mean(na.rm = TRUE) #0.05
  
  

# Mean Swing -> Single

full_data_complete %>%
  filter(swing == 1) %>%
  filter(events %in% c("single")) %>%
  pull(delta_run_exp) %>%
  mean(na.rm = TRUE) #0.49


# Mean Swing -> HR

full_data_complete %>%
  filter(swing == 1) %>%
  filter(events %in% c("home_run")) %>%
  pull(delta_run_exp) %>%
  mean(na.rm = TRUE) #1.38


# Mean Swing -> 2B/3B

full_data_complete %>%
  filter(swing == 1) %>%
  filter(events %in% c("double", "triple")) %>%
  pull(delta_run_exp) %>%
  mean(na.rm = TRUE) #0.80


# Mean Swing -> Out

full_data_complete %>%
  filter(swing == 1) %>%
  filter(events %in% c("double_play", "field_out", "fielders_choice_out", "force_out",
                       "grounded_into_double_play")) %>%
  pull(delta_run_exp) %>%
  mean(na.rm = TRUE) #-0.26


# Mean Swing -> Other

full_data_complete %>%
  filter(swing == 1) %>%
  filter(events %in% c("field_error", "fielders_choice", "sac_bunt", "sac_fly")) %>%
  pull(delta_run_exp) %>%
  mean(na.rm = TRUE) #0.12


# Proportions

full_data_complete %>%
  filter(swing == 1) %>%
  filter(description == "hit_into_play") %>%
  summarize(n = n(),
            out = sum(events %in% c("double_play", "field_out", "fielders_choice_out", "force_out",
                                    "grounded_into_double_play")),
            single = sum(events %in% c("single")),
            dt = sum(events %in% c("double", "triple")),
            hr = sum(events %in% c("home_run")),
            other = sum(events %in% c("field_error", "fielders_choice", "sac_bunt", "sac_fly"))) %>%
  ungroup() %>%
  mutate(pct_out = out / n,
         pct_single = single / n,
         pct_dt = dt / n,
         pct_hr = hr / n,
         pct_other = (n - out - single - dt - hr) / n)



