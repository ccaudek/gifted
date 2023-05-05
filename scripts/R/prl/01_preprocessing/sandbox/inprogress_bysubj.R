#' ---
#' title: "Eating disorders Montecatini"
#' subtitle: "PRL parameters for patients/controls classification"
#' author: "Corrado Caudek"
#' date: "First version 2022-06-08. Last modified `r format(Sys.Date())`."
#' output:
#'   html_document:
#'     theme: readable
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#'     code_download: true
#' ---

#' ## Purpose
#' 
#' A script to classify patients/controls by using the hDDM parameters. Only
#' the four alpha parameters are used, so as to have the same number of 
#' 'features' as in the task-switching experiment.
#' 
#' Notes: 
#' 
#' When the individual parameters of hDDMrl are computed without
#' the knowledge of the group (NO split_by = is_patient), there is
#' no shrinkage that makes more similar to each other the participants'
#' individual parameters of each group. In these conditions, by using
#' the parameters computed in the food condition, no ability for clinical 
#' classification emerges: AUC = 0.55.
#' 
#' To generate the html, run:
#' `rmarkdown::render("scripts/R/scripts_prl/02_hddmrl_params_analyses/classification_tutorial.R")`
#' 
#' ## Prelims

suppressPackageStartupMessages({
  library("rprojroot")
  root <- has_file("eating_disorders_montecatini.Rproj")$make_fix_file()
  library("tidyverse")
  library("tidyr") 
  library("cmdstanr")
  library("lemon")
  library("bayesplot")
  theme_set(bayesplot::theme_default(base_family = "sans"))
  library("pROC")
  library("tidymodels")  
  library("readr")       
  library("vip") 
})


# Increase max print
options(max.print = .Machine$integer.max)

cores <- parallel::detectCores()
cores

source(root("scripts/functions", "funs_param_analyses.R"))
source(root("scripts/functions", "funs_param_classification_analyses.R"))


d <- 
  rio::import(
    root("scripts/python/PRL/classification", "params_invidivual_subjects.csv")
  )

d %>% 
  group_by(diag_cat) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  as.data.frame()

d %>% 
  group_by(diag_cat) %>% 
  summarise(
    n = n_distinct(subj_code)
  )


dat_hc <- d %>% 
  dplyr::filter(diag_cat == "HC")
dat_hc$is_patient <- 0

dat_an <- d %>% 
  dplyr::filter(diag_cat == "AN")
dat_an$is_patient <- 1
# length(unique(dat_an$subj_idx))



#' ## Start the loop here!
n_rep <- 2
accuracy <- rep(NA, n_rep)
roc_auc <- rep(NA, n_rep)

i <- 1

for (i in 1:n_rep) {
  
  # Select random 33 subjects from the HC group
  subj_idx_hc <- unique(dat_hc$subj_idx)
  chosen_subjects <- sample(subj_idx_hc, 33)
  dat31_hc <- dat_hc[dat_hc$subj_idx %in% chosen_subjects, ]
  
  # Select random 33 subjects from the AN group
  subj_idx_an <- unique(dat_an$subj_idx)
  chosen_subjects <- sample(subj_idx_an, 33)
  dat31_an <- dat_an[dat_an$subj_idx %in% chosen_subjects, ]
  
  two_groups31_df <- rbind(dat31_an, dat31_hc)
  
  dat <- two_groups31_df %>% 
    dplyr::select(
      is_patient, 
      alpha_food, pos_alpha_food, 
      alpha_neutral, pos_alpha_neutral,
      a_food, a_neutral, v_food, v_neutral, t_food, t_neutral 
    )
  
  dat$is_patient <- ifelse(
    dat$is_patient == 1, "patient", "control"
  )
  
  dat <- dat %>%  
    mutate_if(is.character, as.factor)
  
  
  diabetes_split <- initial_split(dat, prop = 0.6)
  diabetes_split
  
  # extract training and testing sets
  diabetes_train <- training(diabetes_split)
  diabetes_test <- testing(diabetes_split)
  
  # create CV object from training data
  diabetes_cv <- vfold_cv(diabetes_train)
  
  # define the recipe
  diabetes_recipe <- 
    # which consists of the formula (outcome ~ predictors)
    recipe(
      is_patient ~ ., 
      data = dat
    ) %>%
    # and some pre-processing steps
    step_normalize(all_numeric()) %>%
    step_knnimpute(all_predictors())
  
  diabetes_train_preprocessed <- diabetes_recipe %>%
    # apply the recipe to the training data
    prep(diabetes_train) %>%
    # extract the pre-processed training dataset
    juice()
  
  rf_model <- 
    # specify that the model is a random forest
    rand_forest() %>%
    # specify that the `mtry` parameter needs to be tuned
    set_args(mtry = tune()) %>%
    # select the engine/package that underlies the model
    set_engine("ranger", importance = "impurity") %>%
    # choose either the continuous regression or binary classification mode
    set_mode("classification") 
  
  # lr_model <-
  #   # specify that the model is a random forest
  #   logistic_reg() %>%
  #   # select the engine/package that underlies the model
  #   set_engine("glm") %>%
  #   # choose either the continuous regression or binary classification mode
  #   set_mode("classification")
  
  # set the workflow
  rf_workflow <- workflow() %>%
    # add the recipe
    add_recipe(diabetes_recipe) %>%
    # add the model
    add_model(rf_model)
  
  # specify which values eant to try
  rf_grid <- expand.grid(mtry = c(2, 3, 4))
  # extract results
  rf_tune_results <- rf_workflow %>%
    tune_grid(resamples = diabetes_cv, #CV object
              grid = rf_grid, # grid of values to try
              metrics = metric_set(roc_auc) # metrics we care about
    )
  
  # print results
  # rf_tune_results %>%
  #   collect_metrics()
  
  #' ## Finalize the workflow
  param_final <- rf_tune_results %>%
    select_best(metric = "accuracy")
  
  rf_workflow <- rf_workflow %>%
    finalize_workflow(param_final)
  
  rf_fit <- rf_workflow %>%
    # fit on the training set and evaluate on test set
    last_fit(diabetes_split)
  
  test_performance <- rf_fit %>% collect_metrics()
  #test_performance
  
  accuracy[i] <- as.numeric(test_performance[1, 3])
  roc_auc[i]  <- as.numeric(test_performance[2, 3])
  
  i <- i+1
  
}

hist(accuracy)
hist(roc_auc)
mean(accuracy)
mean(roc_auc)

  





#' ## Generate predictions from the test set
#' 
#' You can also extract the test set predictions themselves using the 
#' collect_predictions() function. Note that there are 192 rows in the 
#' predictions object below which matches the number of test set observations 
#' (just to give you some evidence that these are based on the test set rather 
#' than the training set).
#' 
test_predictions <- rf_fit %>% collect_predictions()
test_predictions

#' Since this is just a normal data frame/tibble object, we can generate 
#' summaries and plots such as a confusion matrix.
#' 
# Generate a confusion matrix
test_predictions %>% 
  conf_mat(truth = is_patient, estimate = .pred_class)

#' We could also plot distributions of the predicted probability distributions 
#' for each class.
#' 
test_predictions %>%
  ggplot() +
  geom_density(
    aes(x = .pred_patient, fill = is_patient), 
    alpha = 0.5
  )

#' If you’re familiar with purrr, you could use purrr functions to extract the 
#' predictions column using pull(). The following code does almost the same 
#' thing as collect_predictions(). You could similarly have done this with the 
#' .metrics column.
#' 
test_predictions <- rf_fit %>% pull(.predictions)
test_predictions

#' ## Fitting and using your final model
#' 
#' The previous section evaluated the model trained on the training data using 
#' the testing data. But once you’ve determined your final model, you often want 
#' to train it on your full dataset and then use it to predict the response for 
#' new data.
#' 
#' If you want to use your model to predict the response for new observations, 
#' you need to use the fit() function on your workflow and the dataset that you 
#' want to fit the final model on (e.g. the complete training + testing dataset).
#' 
final_model <- fit(rf_workflow, dat)

#' The final_model object contains a few things including the ranger object 
#' trained with the parameters established through the workflow contained in 
#' rf_workflow based on the data in diabetes_clean (the combined training and 
#' testing data).
#' 
final_model

#' ## Variable importance
#' 
#' If you want to extract the variable importance scores from your model, as 
#' far as I can tell, for now you need to extract the model object from the 
#' fit() object (which for us is called final_model). The function that extracts 
#' the model is pull_workflow_fit() and then you need to grab the fit object 
#' that the output contains.
#' 
ranger_obj <- extract_fit_parsnip(final_model)$fit
ranger_obj

#' Then you can extract the variable importance from the ranger object itself 
#' (variable.importance is a specific object contained within ranger output - 
#' this will need to be adapted for the specific object type of other models).
#' 
ranger_obj$variable.importance

