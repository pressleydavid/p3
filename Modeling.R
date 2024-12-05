```{r, echo=FALSE}
library(tidyverse)
library(tidymodels)
library(tree)
library(doParallel)
library(rpart.plot)
library(ranger)
```
### Purpose: Select the best model for predicting Diabetes
Choose form of model
Fit model to data using some algorithm
Usually can be written as a problem where we minimize some loss function
Evaluate the model using a metric
RMSE very common for a numeric response
Ideally we want our model to predict well for observations it has yet to see!

A classification tree is a decision tree model used for categorical outcomes (like predicting yes/no). It recursively splits the data into groups based on predictor variables, aiming to create regions with similar response values. At each leaf node, it predicts the most common class in that region.

### Why 5-K CV?
### From ESLII: ch7: if the learning curve has a considerable slope at the given
training set size, five- or tenfold cross-validation will overestimate the true prediction error. Whether this bias is a drawback in practice depends on the objective. On the other hand, leave-one-out cross-validation has low bias but can have high variance. Overall, five- or tenfold cross-validation are recommended as a good compromise: see Breiman and Spector (1992) and Kohavi (1995). Figure 7.9 shows the prediction error and tenfold cross-validation curve

### Read in CSV and map factor levels
```{r}
diabeetus <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv", show_col_types = FALSE) |>
  mutate(Diabetes_binary = structure(
          fct_recode(as.character(Diabetes_binary), 
                      "No Diabetes" = "0",
                      "Diabetes" = "1"),
                      label = "Diabetes or Prediabetes"),
        HighBP = structure(
          fct_recode(as.character(HighBP),
                      "no" = "0",
                      "yes" = "1"),
                      label = "High Blood Pressure"),
        HighChol = structure(
          fct_recode(as.character(HighChol),
                      "no" = "0",
                      "yes" = "1"),
                      label = "High Cholesterol"),
        CholCheck = structure(
          fct_recode(as.character(CholCheck),
                     "no" = "0",
                     "yes" = "1"),
                      label = "Cholesterol Check Status", 
                      long_label = "Cholesterol Check within Past 5 years"),
        BMI = structure(
          as.integer(BMI),
          label = "BMI"),
        Smoker = structure(
          fct_recode(as.character(Smoker),
                      "no" = "0",
                      "yes" = "1"),
                      label = "Smoking Status",
                      long_label = "Have you smoked at least 100 cigarettes in your entire life? [Note: 5 packs = 100 cigarettes]"),
        Stroke = structure(
          fct_recode(as.character(Stroke),
                      "no" = "0",
                      "yes" = "1"),
                      label = "Stroke Status",
                      long_label = "(Ever told) you had a stroke"),
        HeartDiseaseorAttack = structure(
          fct_recode(as.character(HeartDiseaseorAttack),
                      "no" = "0",
                      "yes" = "1"),
                      label = "CHD/MI Status",
                      long_label = "Coronary Heart Disease (CHD) or Myocardial Infarction (MI)"),
        PhysActivity = structure(
          fct_recode(as.character(PhysActivity),
                      "no" = "0",
                      "yes" = "1"),
                      label = "Physical Activity Status",
                      long_label = "Physical Activity in past 30 days - not including job"),
        Fruits = structure(
          fct_recode(as.character(Fruits),
                      "no" = "0",
                      "yes" = "1"),
                      label = "Fruit Consumption",
                      long_label = "Consume Fruit 1 or more times per day"),
        Veggies = structure(
          fct_recode(as.character(Veggies),
                      "no" = "0",
                      "yes" = "1"),
                      label = "Vegetable Consumption",
                      long_label = "Consume Vegetables 1 or more times per day"),
        HvyAlcoholConsump = structure(
          fct_recode(as.character(HvyAlcoholConsump),
                      "no" = "0",
                      "yes" = "1"),
                      label = "Heavy Drinker",
                      long_label = "Heavy drinkers (adult men having more than 14 drinks per week and adult women having more than 7 drinks per week"),
        AnyHealthcare = structure(
          fct_recode(as.character(AnyHealthcare),
                      "no" = "0",
                      "yes" = "1"),
                      label = "Healthcare Coverage",
                      long_label = "Have any kind of health care coverage, including health insurance, prepaid plans such as HMO, etc"),
        NoDocbcCost = structure(
          fct_recode(as.character(NoDocbcCost),
                      "no" = "0",
                      "yes" = "1"),
                      label = "Healthcare Cost Unaffordable",
                      long_label = "Was there a time in the past 12 months when you needed to see a doctor but could not because of cost?"),
        GenHlth = structure(
          fct_recode(as.character(GenHlth),
                      "excellent" = "1",
                      "very good" = "2",
                      "good" = "3",
                      "fair" = "4",
                      "poor" = "5"),
                      label = "Overall Health",
                      long_label = "Overall Health (scale 1(excellent)-5(Poor)"),
        MentHlth = structure(
          as.integer(MentHlth),
            label = "Poor Mental Health Days",
            long_label = "how many days during the past 30 days was your mental health not good?"),
        PhysHlth = structure(
          as.integer(PhysHlth),
            label = "Poor Physical Health Days",
            long_label = "how many days during the past 30 days was your physical health not good?"),
        DiffWalk = structure(
          fct_recode(as.character(DiffWalk),
                      "no" = "0",
                      "yes" = "1"),
                      label = "Difficulty Walking Status",
                      long_label = "Do you have serious difficulty walking or climbing stairs?"),
        Sex = structure(
          fct_recode(as.character(Sex),
                      "female" = "0",
                      "male" = "1"),
                      label = "Sex",
                      long_label = "Sex"),
        Education = structure(
          fct_recode(as.character(Education),
                      "None" = "1" ,
                      "Elementary(Grades 1-8)" = "2",
                      "Some HS (Grades 9-11)" = "3",
                      "HS Grad" = "4",
                      "Some college (1-3 years)" = "5",
                      "College graduate" = "6"),
                      label = "Education Level",
                      long_label = "Education level (EDUCA codebook scale)"),
        Age = structure(
          fct_recode(as.character(Age),
                      "18-24" = "1",
                      "25-29" = "2",
                      "30-34" = "3",
                      "35-39" = "4",
                      "40-44" = "5",
                      "45-49" = "6",
                      "50-54" = "7",
                      "55-59" = "8",
                      "60-64" = "9",
                      "65-69" = "10", 
                      "70-74" = "11", 
                      "75-79" = "12",
                      ">80" = "13"),
                      label = "Age Category (5 year)",
                      long_label = "13-level age category (_AGEG5YR see codebook)"),
        Income = structure(
          fct_recode(as.character(Income),
                      "<10k" = "1",
                      "10k-15k" = "2",
                      "15k-20k" = "3",
                      "20k-25k" = "4",
                      "25k-35k" = "5",
                      "35k-50k" = "6",
                      "50k-75k" = "7",
                      ">75k" = "8"),
                      label = "Income Level",
                      long_label = "Income scale (INCOME2 see codebook)")
         )
```



### For categorical responses, we use log-loss (see notes for more detail)
### Tidymodels Implementation of a 5XCV
### Split into training and test
```{r}
set.seed(462)
split <- initial_split(diabeetus, prop = 0.7)
train <- training(split)
test <- testing(split)

# Create 5XCV splits of train
diabeetus_folds <- vfold_cv(train, v = 5)

# Create recipe with 5+ predictors, based on EDA selection
# 1. HighBP
# 2. HighChol
# 3. DiffWalk
# 4. HeartDiseaseorAttack
# 5. Smoker
# 6. Stroke

# 6 Predictors
tree_rec1 <- recipe(Diabetes_binary ~ HighBP + HighChol + BMI +
                 Age + GenHlth + PhysActivity, data = train) |>
  step_dummy(HighBP, HighChol, Age, GenHlth, PhysActivity)

# Create the decision tree model
tree_model <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = 50
) |>
  set_engine("rpart") |>
  set_mode("classification")

# Create workflow
tree_wkf <- workflow() |>
  add_recipe(tree_rec1) |>
  add_model(tree_model)


# tree_grid_reg <- grid_regular(cost_complexity(),
#                           tree_depth(5),
#                           levels = 5)

tree_grid_rand <- grid_random(cost_complexity(),
                              tree_depth(range = c(1,2)),
                              size = 5)

# Parallel processing setup (optional)
# cl <- makeCluster(detectCores() - 1)
# registerDoParallel(cl)

# Add verbosity and logging
control <- control_grid(
  verbose = TRUE,             
  save_pred = TRUE,         
  save_workflow = TRUE,
  allow_par = FALSE 
)

# Use CV to select tuning parameters with control
set.seed(462)
tuner <- tree_wkf |>
  tune_grid(
    resamples = diabeetus_folds,
    grid = tree_grid_rand,
    control = control,
    metrics = metric_set(accuracy, mn_log_loss)
  )


# Collect and inspect metrics
metrics <- tuner |> 
  collect_metrics() |>
  filter(.metric == "mn_log_loss") |>
  arrange(mean)

# Stop the parallel backend
# stopCluster(cl)

# Print metrics
print(metrics)

#Select best model based on log loss
best_tree <- select_best(tuner, metric = "mn_log_loss")
#Finalize workflow with best parameters
final_wf <- finalize_workflow(tree_wkf, best_tree)
#Fit final model to training data
final_tree_fit <- fit(final_wf, data = train)

#Print tree model
tree_model <- final_tree_fit |> extract_fit_engine()
rpart.plot::rpart.plot(tree_model, roundint = FALSE)



#Plot Metrics
# tuner %>%
#   collect_metrics() %>%
#   mutate(tree_depth = factor(tree_depth)) %>%
#   ggplot(aes(cost_complexity, mean, color = tree_depth)) +
#   geom_line(linewidth = 1.5, alpha = 0.6) +
#   geom_point(size = 2) +
#   facet_wrap(~ .metric, scales = "free", nrow = 2) +
#   scale_x_log10(labels = scales::label_number()) +
#   scale_color_viridis_d(option = "plasma", begin = .9, end = 0)
```



### Random Forest
```{r}
rf_model <- rand_forest(
  mtry = tune(),
  trees = 50,
  min_n = 20
) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

rf_grid <- grid_regular(
  mtry(range = c(2, 6)),
  levels = 5
)

rf_wkf <- workflow() |>
  add_recipe(tree_rec1) |>
  add_model(rf_model)

control <- control_grid(
  verbose = TRUE,         # Print progress updates during tuning
  # allow_par = TRUE,       # Enable parallel processing
  save_pred = TRUE,       # Save predictions for each resample
  save_workflow = TRUE    # Save the workflow for later analysis
)
#Tune random forest using log loss
rf_tuner <- rf_wkf |>
  tune_grid(
    control = control,
    resamples = diabeetus_folds,
    grid = rf_grid,
    metrics = metric_set(accuracy, mn_log_loss)
  )

# get best RF parameters
rf_tuner |> collect_metrics()
best_rf <- select_best(rf_tuner, metric = "mn_log_loss")
#Finalize workflow with best parameters
final_wkf <- finalize_workflow(rf_wkf, best_rf)
#Fit final model to training data
final_rf_fit <- fit(final_wkf, data = train)
#Print tree model
rf_model <- final_rf_fit |> extract_fit_engine()
# rpart.plot::rpart.plot(rf_model, roundint = FALSE)

# Get proportions of OOB Samples
rf_model$prediction.error

# Make predictions on test set
rf_predictions <- predict(final_rf_fit, new_data = test, type = "prob") %>%
  bind_cols(test)
```

### Select Best Model
```{r}
# Fit final model with best parameters
final_tree_wkf <- tree_wkf |>
  finalize_workflow(best_tree)

final_rf_wkf <- rf_wkf |>
  finalize_workflow(best_rf)

#Evaluate on test set
tree_test <- final_tree_wkf |>
  last_fit(split, metrics = metric_set(accuracy, mn_log_loss))

rf_test <- final_rf_wkf |>
  last_fit(split, metrics = metric_set(accuracy, mn_log_loss))

#Compare final results
winner <- bind_rows(
  tree_test |> collect_metrics() |> mutate(model = "Classification Tree"),
  rf_test |> collect_metrics() |> mutate(model = "Random Forest")
) |>
  select(model, .metric, .estimate)
winner
```

###Outcome
The Random Forest model performs better on both metrics:
- Higher accuracy (0.866 vs 0.861)
- Lower log loss (0.317 vs 0.403) 

Since we're primarily using log loss as our evaluation metric, the Random Forest wins since it has a lower log loss value of 0.317 compared to the Classification Tree: 0.403 

To put this in context:
- Log loss ranges from 0 to ∞, with 0 being perfect predictions
- Lower log loss indicates better probability estimates for class predictions
- The Random Forest's log loss of 0.317 indicates it's providing better probability estimates than the Classification Tree's 0.403

Therefore, Random Forest is the overall winner.

This makes theoretical sense because:
1. Random Forests typically outperform single trees by averaging many trees
2. The randomization in feature selection helps prevent overfitting
3. Random Forests generally provide better probability estimates due to their ensemble nature



[Click here for the Modeling Page](Modeling.html)
