


d <- rio::import(here::here(data, raw, gifted_1))

# response: which image has been chosen in each trial
d$response <- d$is_target_img_chosen

d$feedback <- ifelse(
  d$feedback == 2, 0, d$feedback
)
d$feedback <- ifelse(d$feedback == 3, NA, d$feedback)

d$feedback <- 
  ifelse(d$rt < 150 | d$rt > 2499, NA, d$feedback)

d$rt1 <- 
  ifelse(d$rt < 150 | d$rt > 2499, NA, d$rt)

d$rt1 <- ifelse(d$trial == 1, NA, d$rt1)

d$is_patient <- ifelse(d$group == "patients", 1, 0)

d$is_rich_choice <- case_when(
  d$is_target_rewared_in_present_epoch & d$is_target_img_chosen ~ 1,
  !d$is_target_rewared_in_present_epoch & !d$is_target_img_chosen ~ 1,
  TRUE ~ 0
)

# Multiple imputation on NAs.
temp <- data.frame(
  rt1         = d$rt1, 
  trial       = d$trial,
  feedback    = d$feedback, 
  rich_choice = d$is_rich_choice,
  is_patient  = d$is_patient,
  response    = d$response
)

# Imputes the "best value" according to the linear regression model, also 
# known as regression imputation.
imp <- mice::mice(temp, method = "norm.predict", m = 1) 
temp <- complete(imp)

d$feedback <- ifelse(temp$feedback > 0.5, 1, 0)
d$rt <- temp$rt1 / 1000

# I want to number each subject in the data.frame so that subjects are 
# ordered sequentially, according to the order they appear in the data frame. 
# https://community.rstudio.com/t/why-does-group-indices-use-alphabetical-ordering/5452
# As suggested in the above link, I wrap group_indices in another function:
grpid = function(x) match(x, unique(x))
# then
d1 <- d %>% 
  mutate(subj_idx = group_indices(., subj_name) %>% grpid)
# In this manner, the variable subj_idx assigns an integer to each subject;
# this integer is ordered according to the sequence in which the subjects are 
# present in the data.frame.
# table(d3$subj_idx)
# unique(d3$subj_idx)

d1$stimulus_type <- factor(d1$stimulus_type)
d1$stimulus_type <- d1$stimulus_type %>% 
  forcats::fct_recode(
    neutral = "socialshame"
  )

d1 <- d1 %>% 
  dplyr::rename(
    stim = stimulus_type
  )

d1$split_by <- ifelse(d1$stim == "food", 0, 1)

df_for_hddm <- data.frame(
  subj_idx   = d1$subj_idx,
  response   = d1$response,
  stim       = d1$stim,
  rt         = d1$rt,
  trial      = d1$trial,
  split_by   = d1$split_by,
  feedback   = d1$feedback,
  is_patient = d1$is_patient,
  subj_code  = d1$subj_name,
  q_init     = 0.5
)

mydat <- df_for_hddm %>% 
  dplyr::arrange(subj_idx, trial, split_by)

rio::export(
  mydat, 
  here("data", "processed", "prl", "data_for_hddm", "hddm_input.csv")
)

lut <- data.frame(
  subj_idx = mydat$subj_idx, 
  subj_code = mydat$subj_code
)

rio::export(
  lut, 
  here("data", "processed", "prl", "data_for_hddm", "hddm_look_up_table.csv")
)

# # test data
# temp <- mydat[(mydat$subj_idx == 2 | mydat$subj_idx == 100), ]
# 
# temp$subj_idx <- ifelse(
#   temp$subj_idx == 100, 1, temp$subj_idx
# )
# 
# rio::export(
#   temp, 
#   here("data", "processed", "prl", "data_for_hddm", "prova.csv")
# )



