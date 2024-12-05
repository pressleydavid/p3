library(dplyr)
library(ggplot2)

### Random Forest
```{r}
# rf_model <- rand_forest(
#   mtry = tune(),
#   trees = 50,
#   min_n = 20
# ) |>
#   set_engine("ranger", importance = "impurity") |>
#   set_mode("classification")
#
# rf_grid <- grid_regular(
#   mtry(range = c(2, 6)),
#   levels = 5
# )
#
# rf_wkf <- workflow() |>
#   add_recipe(tree_rec1) |>
#   add_model(rf_model)
#
# control <- control_grid(
#   verbose = TRUE,         # Print progress updates during tuning
#   # allow_par = TRUE,       # Enable parallel processing
#   save_pred = TRUE,       # Save predictions for each resample
#   save_workflow = TRUE    # Save the workflow for later analysis
# )
# #Tune random forest using log loss
# rf_tuner <- rf_wkf |>
#   tune_grid(
#     control = control,
#     resamples = diabeetus_folds,
#     grid = rf_grid,
#     metrics = metric_set(accuracy, mn_log_loss)
#   )
#
# # get best RF parameters
# rf_tuner |> collect_metrics()
# best_rf <- select_best(rf_tuner, metric = "mn_log_loss")
# #Finalize workflow with best parameters
# final_wkf <- finalize_workflow(rf_wkf, best_rf)
# #Fit final model to training data
# final_rf_fit <- fit(final_wkf, data = train)
# #Print tree model
# rf_model <- final_rf_fit |> extract_fit_engine()
# # rpart.plot::rpart.plot(rf_model, roundint = FALSE)
#
# # Get proportions of OOB Samples
# rf_model$prediction.error
#
# # Make predictions on test set
# rf_predictions <- predict(final_rf_fit, new_data = test, type = "prob") %>%
#   bind_cols(test)
```


cat_vars <- names(diabeetus)[sapply(diabeetus,is.factor)]

diabeetus %>%
  select(Diabetes_binary, all_of(cat_vars)) %>%
  pivot_longer(-Diabetes_binary, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = Diabetes_binary)) +
  geom_bar(position = "fill") +
  facet_wrap(~variable, scales = "free_x") +
  scale_fill_viridis_d(option = "plasma", end = .7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Diabetes Status by Categorical Variables",
       x = "Variable Value",
       y = "Proportion",
       fill = "Diabetes Status")

ggsave("figures/health_indicators1.png",
       width = 12,
       height = 10)



ggsave("figures/health_indicators1.png",
       width = 12,
       height = 10)

diabeetus %>%
  select(Diabetes_binary, all_of(cat_vars)) %>%
  pivot_longer(-Diabetes_binary, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = Diabetes_binary)) +
  geom_bar(position = "fill") +
  facet_wrap(~variable, scales = "free_x") +
  scale_fill_viridis_d(option = "plasma", end = .7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Diabetes Status by Categorical Variables",
       x = "Variable Value",
       y = "Proportion",
       fill = "Diabetes Status")


# Calculate summary statistics
summary_stats <- diabeetus |>
  group_by(Diabetes_binary) |>
  summarize(count = n(),
    mean_BMI = mean(BMI, na.rm = TRUE),
    median_BMI = median(BMI, na.rm = TRUE)
)

summary_age <- diabeetus |>
  group_by(Diabetes_binary) |>
  count(Age, sort = TRUE) |>
  mutate(pct = n/sum(n) * 100) |>
  mutate(pct = round(pct,1)) |>
  arrange(Age, n, pct)



summ_cats <- diabeetus |>
  select(where(is.character), where(is.factor)) |>
  summarize(across(everything(),
                   list(
                     n_unique = \(x) n_distinct(x),
                     n_missing = \(x) sum(is.na(x))
                   )
  )) |>
  pivot_longer(
    everything(),
    names_to = c("column", "stat"),
    names_pattern = "(.*)_(.*)",
    values_to = "value"
  )

# Create the boxplot with labels for mean and median
by_bmi <- diabeetus %>%
  ggplot(aes(x = Diabetes_binary, y = BMI)) +
  geom_boxplot() +
  geom_text(data = summary_stats,
            aes(x = Diabetes_binary, y = mean_BMI, label = paste0("Mean: ", round(mean_BMI, 1))),
            vjust = -1.0, color = "blue", size = 3) +
  geom_text(data = summary_stats,
            aes(x = Diabetes_binary, y = median_BMI, label = paste0("Median: ", round(median_BMI, 1))),
            vjust = 1.5, color = "red", size = 3) +
  labs(title = "BMI by Diabetes Status",
       x = "Diabetes Status",
       y = "BMI") +
  theme_minimal()

# Display the plot
print(by_bmi)

  by_age <- ggplot(summary_age, aes(x = Age, y = pct, fill = Diabetes_binary)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Distribution of Age Groups by Diabetes Status",
    x = "Age Group",
    y = "Percentage",
    fill = "Diabetes Status"
  )

# Display the plot
print(by_age)

ggplot(summary_age, aes(x = Age, y = pct, fill = Diabetes_binary)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Percentage Distribution of Age Groups by Diabetes Status",
    x = "Age Group",
    y = "Percentage",
    fill = "Diabetes Status"
  )

# Create box plots for numerical variables grouped by diabetes status
library(tidyverse)

# Select numerical variables to plot
num_vars <- c("BMI", "MentHlth", "PhysHlth")

# Should I log transform BMI?
# Data are skewed: Y, but not by much, and not a large range of values spanning
# more than one order of magnitude

# Should I log transform PhysHlth / MentlHlth?
# Data are skewed: Y, but a discrete range (0-30) so no
# more than one order of magnitude



# Create long format data for plotting
diabetes_long <- diabeetus %>%
  select(Diabetes_binary, all_of(num_vars)) |>
  pivot_longer(cols = -Diabetes_binary,
               names_to = "variable",
               values_to = "value")

# Create box plots
ggplot(diabetes_long, aes(x = Diabetes_binary, y = value, fill = Diabetes_binary)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Distribution of Health Metrics by Diabetes Status",
    x = "Diabetes Status",
    y = "Value"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

### Based on the skewness of the distribution, how many people had poor mental and physical health between 20 and 30 days of the month
```{r}
#### Summary Table
mh_data <- diabeetus |>
  mutate(mh_grp = factor(
    MentHlth >= 29,
    levels = c(FALSE,TRUE),
    labels = c("Less than 29 days", "29 or more days"))) |>
  group_by(Diabetes_binary, mh_grp) |>
  summarize(count = n(), .groups = "drop") |>
  group_by(Diabetes_binary) |>
  mutate(percentage = count / sum(count) * 100)


ggplot(mh_data, aes(x = mh_grp, y = percentage, fill = Diabetes_binary)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(
    label = paste0(round(percentage, 1), "%")
  ),
  position = position_dodge(width = 1),
  size = 3,
  vjust = 1.5) +
  labs(
    x = "Days of Poor Mental Health",
    y = "Percentage",
    fill = "Diabetes Status",
    title = "Distribution of Mental Health Days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
# Summary Table
diabeetus |>
  mutate(ph_grp = factor(PhysHlth >= 29,
                         levels = c(FALSE,TRUE),
                         labels = c("Less than 29 days", "29 or more days")),
  ) |>
  tabyl(ph_grp) |>
  adorn_pct_formatting() |>
  rename("Days of Poor Physical Health" = ph_grp)


# Plot
ph_data <- diabeetus |>
  mutate(ph_grp = factor(
    PhysHlth >= 29,
    levels = c(FALSE,TRUE),
    labels = c("Less than 29 days", "29 or more days"))) |>
  group_by(Diabetes_binary, ph_grp) |>
  summarize(count = n(), .groups = "drop") |>
  group_by(Diabetes_binary) |>
  mutate(percentage = count / sum(count) * 100)


ggplot(ph_data, aes(x = ph_grp, y = percentage, fill = Diabetes_binary)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(
    label = paste0(round(percentage, 1), "%")
  ),
  position = position_dodge(width = 1),
  size = 3,
  vjust = 1.5) +
  labs(
    x = "Days of Poor Physical Health",
    y = "Percentage",
    fill = "Diabetes Status",
    title = "Distribution of Physical Health Days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```



# Covariation
ggplot(diabeetus, aes(x = Age, y = Diabetes_binary)) + geom_count()
# ggplot(diabeetus, aes(x = Age, y = Diabetes_binary)) + geom_tile(aes(fill = n))

# diabeetus |>
#   count(Age,Diabetes_binary) |>
#   ggplot(aes(x = Age, y = Diabetes_binary)) +
#   geom_count()


library(tidyverse)
library(patchwork)

# Function to create a standardized bar plot
create_bar_plot <- function(data, var_name) {
  data |>
    count(!!sym(var_name)) |>
    ggplot(aes(x = !!sym(var_name), y = n)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 10)
    ) +
    labs(title = var_name,
         x = NULL,
         y = "Count")
}

# Create list of plots for key variables of interest
plot_vars <- c("Diabetes_binary", "HighBP", "HighChol", "GenHlth",
               "PhysActivity", "Age")

plots <- map(plot_vars, ~create_bar_plot(diabeetus, .x))

# Combine plots using patchwork
combined_plot <- wrap_plots(plots, ncol = 3) +
  plot_annotation(
    title = 'Distribution of Key Health Indicators',
    theme = theme(plot.title = element_text(size = 16, hjust = 0.5))
  )

# Display the combined plot
combined_plot


############

# Create consolidated frequency tables
library(tidyverse)
library(knitr)

cat_vars <- c("Diabetes_binary", "HighBP", "HighChol", "CholCheck",
              "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity",
              "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare",
              "NoDocbcCost", "GenHlth", "DiffWalk", "Sex", "Age",
              "Education", "Income")

# Modified function to include variable name in output
create_freq_table <- function(data, var) {
  data |>
    count(!!sym(var)) |>
    mutate(
      variable = var,
      category = !!sym(var),
      prop = n / sum(n),
      pct = round(prop * 100, 1)
    ) |>
    select(variable, category, n, pct) |>
    arrange(desc(n))
}

# Create all tables at once and combine
all_freq_tables <- cat_vars |>
  map_dfr(~create_freq_table(diabeetus, .x)) |>
  mutate(
    pct = paste0(pct, "%"),
    n = format(n, big.mark = ",")
  )

# Print nicely formatted table
all_freq_tables |>
  group_by(variable) |>
  kable(
    col.names = c("Variable", "Category", "Count", "Percentage"),
    align = c("l", "l", "r", "r")
  )


### Stacked plots to show proportions of Diabetes_binary
# Function to create proportion plots for multiple variables
create_prop_plot <- function(data, var_name) {
  data %>%
    ggplot(aes(x = Diabetes_binary, fill = !!sym(var_name))) +
    geom_bar(position = "fill") +
    labs(
      title = paste("Proportion of", var_name),
      x = "Diabetes Status",
      y = "Proportion",
      fill = var_name
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal()
}

# Create plots for multiple variables
vars_to_plot <- c("Diabetes_binary", "HighBP", "HighChol", "CholCheck",
                  "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity",
                  "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare",
                  "NoDocbcCost",  "DiffWalk", "Sex" )

plot_list <- map(vars_to_plot, ~create_prop_plot(diabeetus, .x))

combined_plot <- wrap_plots(plot_list, ncol = 4) +
  plot_annotation(
    title = "Health Indicators by Diabetes Status",
    subtitle = "Proportional distribution of various health factors",
    # caption = "Data source: Diabetes Health Indicators Dataset"
  )

combined_plot

multi_level_vars <- c("GenHlth", "Age","Education", "Income")
multi_list <- map(multi_level_vars, ~create_prop_plot(diabeetus, .x))

combined_plot <- wrap_plots(multi_list, ncol = 2) +
  plot_annotation(
    title = "Multi-level Health Indicators by Diabetes Status",
    subtitle = "Proportional distribution of various health factors",
    # caption = "Data source: Diabetes Health Indicators Dataset"
  )

combined_plot


### Hand-rolling a 5-Fold CV
```{r}
nrow(diabeetus)
size_fold <- floor(nrow(diabeetus)/5)

#Reorder indices
set.seed(462)
random_indices <- sample(1:nrow(diabeetus), size = nrow(diabeetus), replace = FALSE)



# iterate through the indices vector and add appropriate observations to each data set
get_cv_splits <- function(data, num_folds){
  #get fold size
  size_fold <- floor(nrow(data)/num_folds)
  #get random indices to subset the data with
  random_indices <- sample(1:nrow(data), size = nrow(data), replace = FALSE)
  #create a list to save our folds in
  folds <- list()
  #now cycle through our random indices vector and take the appropriate observations to each fold
  for(i in 1:num_folds){
    if (i < num_folds) {
      fold_index <- seq(from = (i-1)*size_fold +1, to = i*size_fold, by = 1)
      folds[[i]] <- data[random_indices[fold_index], ]
    } else {
      fold_index <- seq(from = (i-1)*size_fold +1, to = length(random_indices), by = 1)
      folds[[i]] <- data[random_indices[fold_index], ]
    }
  }
  return(folds)
}
folds <- get_cv_splits(diabeetus, 5)
# folds[[1]]

#Fit data on 4 folds, test on 5th
#set test fold number
test_number <- 5
#pull out testing fold & remove from list
test <- folds[[test_number]]
folds[[test_number]] <- NULL
#check length == 4
length(folds)
# Combind tibbles saved in different folds
train <- purrr::reduce(folds, rbind)

#Fit model and check metric on test set
fit <- lm()


#Return list with folds of data

```


### Fit a Classification Tree Model
```{r}
attach(diabeetus)
classification_tree <- tree(Diabetes_binary ~ ., data = diabeetus)
summary(classification_tree)
plot(classification_tree)
text(classification_tree, pretty = 0)
```

### From Lesson (Regression & Classification Trees pdf)
```{r}
fitTree <- tree(Diabetes_binary ~ HighBP, data = diabeetus)
plot(fitTree)
text(fitTree, pretty = 0)
```



