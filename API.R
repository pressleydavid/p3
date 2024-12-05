library(plumber)
library(tidymodels)
library(yardstick)
library(tidyverse)
library(ggplot2)

#Import CSV file
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

# best_recipe <- recipe(Diabetes_binary ~ HighBP + HighChol + BMI +
#                       Age + GenHlth + PhysActivity,
#                       data = diabeetus) |>
#   step_dummy(all_nominal_predictors())
#
# # Create Random Forest Spec with best predictors
# rf_model <- rand_forest(
#   mtry = 4, # Based on best parameters from CV
#   trees = 50,
#   min_n = 20
# ) |>
#   set_engine("ranger") |>
#   set_mode("classification")
#
# rf_wkf <- workflow() |>
#   add_recipe(best_recipe) |>
#   add_model(rf_model)
#
# final_fit <- fit(rf_wkf, diabeetus)

# Save model for API endpoints
model <- readRDS("diabetes_model.rds")

#* @apiTitle Diabetes Prediction API

#* Make prediction
#* @param HighBP High blood pressure (0/1)
#* @param HighChol High cholesterol (0/1)
#* @param BMI Body mass index
#* @param Age Age category (1-13)
#* @param GenHlth General health (1-5)
#* @param PhysActivity Physical activity (0/1)
#* @get /pred
function(HighBP = "no", HighChol = "no", DiffWalk = "no", BMI = 25, Age = "50-54", GenHlth = "poor", HeartDiseaseorAttack = "no", Smoker = "no") {
  prediction_data <- data.frame(
    HighBP = factor(HighBP, levels = levels(diabeetus$HighBP)),
    HighChol = factor(HighChol, levels = levels(diabeetus$HighChol)),
    DiffWalk = factor(DiffWalk, levels = levels(diabeetus$DiffWalk)),
    BMI = as.numeric(BMI),
    Age = factor(Age, levels = levels(diabeetus$Age)),
    GenHlth = factor(GenHlth, levels = levels(diabeetus$GenHlth)),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = levels(diabeetus$HeartDiseaseorAttack)),
    Smoker = factor(Smoker, levels = levels(diabeetus$Smoker))
  )
  # Generate predictions
  predict(model, prediction_data, type = "prob")
}

#* API Information
#* @get /info
function() {
  list(
    name = "David Pressley",
    github_url = "https://pressleydavid.github.io/p3/EDA.html"
  )
}

#* Get confusion matrix visualization
#* @serializer png
#* @get /confusion
function() {
  # Create predictions using the fitted model
  preds <- predict(model, new_data = diabeetus, type = "class")

  # Create confusion matrix
  conf_data <- data.frame(
    truth = diabeetus$Diabetes_binary,
    prediction = preds$.pred_class
  )

  # Create plot
  p <- ggplot(conf_data, aes(x = truth, y = prediction)) +
    geom_jitter(alpha = 0.2, width = 0.2, height = 0.2) +
    labs(title = "Confusion Matrix",
         x = "True Value",
         y = "Predicted Value") +
    theme_minimal()

  print(p)
}

# Example API calls:
#http://127.0.0.1:8000/pred?HighBP=yes&HighChol=yes&DiffWalk=yes&BMI=30&Age=50-54&GenHlth=good&HeartDiseaseorAttack=yes&Smoker=yes

#http://127.0.0.1:8000/pred?HighBP=yes&HighChol=yes&DiffWalk=yes&BMI=45&Age=25-29&GenHlth=poor&HeartDiseaseorAttack=yes&Smoker=yes

#http://127.0.0.1:8000/pred?HighBP=yes&HighChol=no&DiffWalk=yes&BMI=35&Age=65-69&GenHlth=fair&HeartDiseaseorAttack=no&Smoker=yes


