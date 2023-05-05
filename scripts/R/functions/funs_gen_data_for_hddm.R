

load_psychtoolkit_files <- function(GROUP) {
  
  dir <- here::here("data", "raw", GROUP)
  
  file_names <- as.character(list.files(path=dir, pattern = "PRL_"))
  n_files <- length(file_names)
  # n_files
  
  d_list <- list()
  
  for (i in 1:n_files) {
    
    d  <- read.table(here("data", "raw", GROUP, file_names[i]))
    
    d$subj_idx <- file_names[i]
    d$epoch <- d$V1
    d$target_position <- d$V2
    d$stimulus_type <- d$V3
    d$stimulus_class <- d$V4
    d$which_image_is_rewarded_more <- d$V5
    d$keypress <- d$V6 # (1=a, 2=l)
    d$rt <- d$V7
    d$feedback <- d$V8 # 1 = reward, 2 = punishment, 3 = too slow
    d$intertrial_delay <- d$V9

    d$epoch <- as.numeric(stri_sub(d$V1, 5, 5))
    
    # V7 key pressed by participant
    # 1 : "a" -> sx
    # 2 : "l" -> dx
    
    # participant has chosen the image to the right or to the left
    d$resp <- ifelse(
      d$V8 == 1, "sx",
      ifelse(d$V8 == 2, "dx", "ERROR")
    )
    
    d$is_positive_feedback <- case_when(
      d$V8 == 1 ~ 1,
      d$V8 == 2 ~ 0,
      d$V8 == 3 ~ NA
    )
    
    d$trial <- 1:160
    
    d_list[[i]] <- d
  }
  
  # convert list into data.frame
  df <- do.call(rbind.data.frame, d_list)
  
  df$trg_position <- str_sub(df$target_position,-2, -1) |>
    as.factor()
  
  df$resp <- ifelse(df$resp == "ERROR", NA, df$resp)
  
  df1 <- with(
    df,
    data.frame(
      subj_idx, epoch, trg_position, keypress, rt, feedback, intertrial_delay, 
      resp, is_positive_feedback, trial                       
    )
  )
  
  if (GROUP == "gifted_1") {
    saveRDS(df1, here("data", "processed", "prl", "gifted_1.rds"))
  } else {
    saveRDS(df1, here("data", "processed", "prl", "gifted_2.rds"))
  }
}



read_excel_code <- function(GROUP) {
  d <- readxl::read_excel(
    here("data", "raw", GROUP, "data.xlsx")
  )
  d
}


# gen_subj_name() ---------------------------------------------------------


#' @description 
#' generate subject code
#' @return data.frame.
#' @param data.frame.
gen_subj_name <- function(d) {
  
  library("stringi")
  
  d$nome <- d$nome_1
  d$cognome <- d$cognome_1
  d$anno_nascita <- d$anno_1

  d$mese <- ifelse(
    d$`mese_1` < 10, stri_join("0", as.character(d$`mese_1`), sep=''), as.character(d$`mese_1`)
  ) |>
    as.numeric()
  
  d$giorno <- ifelse(
    d$`giorno_1` < 10, 
    stri_join("0", as.character(d$`giorno_1`), sep=''), 
    as.character(d$`giorno_1`)
  ) |>
    as.numeric()
  
  d$sex <- ifelse(d$`sesso_1` == 1, "f",
                  ifelse(d$`sesso_1` == 2, "m", NA))
  
  d$subj_id <- tolower(
    stri_join(d$`nome_1`, d$`cognome_1`, d$`anno_1`, 
              d$mese, d$giorno, d$sex, 
              sep='_')
  )
  
  d$code_psytoolkit <- d$`esperimento_1`
  
  d$time_start <- d$TIME_start
  d$time_end <- d$TIME_end
  d$time_total <- d$TIME_total
  
  d
}

# join_prl_data_child_info() ---------------------------------------------------

# Function for generating the complete data (prl data + child info) for one
# group (es., gifted_1). This should be executed twice to get the data of the
# two groups. Then these data will be merged.
join_prl_data_child_info <- function(GROUP) {
  
  # Read individual psytoolkit PRL files and generate 2 RDS files.
  # here("data", "processed", "prl", "gifted_1.rds")
  # here("data", "processed", "prl", "gifted_2.rds")
  load_psychtoolkit_files(GROUP)
  
  # Read file data.xlsx
  excel_codes <- read_excel_code(GROUP)
  
  # Generate subject code and save subject info in child_info
  child_info <- gen_subj_name(excel_codes) |>
    dplyr::select(subj_id, code_psytoolkit, 
                  anno_nascita, mese, giorno, sex,
                  time_start, time_end, time_total)
  
  if (GROUP == "gifted_1") {
    prl_data <- readRDS(
      here("data", "processed", "prl", "gifted_1.rds")
    ) %>% 
      dplyr::rename("code_psytoolkit" = "subj_idx")
  } else {
    prl_data <- readRDS(
      here("data", "processed", "prl", "gifted_2.rds")
    ) %>% 
      dplyr::rename("code_psytoolkit" = "subj_idx")
  }
  
  # Join child info and PRL data
  df <- inner_join(
    child_info, prl_data, by = "code_psytoolkit"
  )
  
  df
}

# recode_vars_for_hddmrl() -----------------------------------------------------
# Get the input the total data frame (including the prl data and child info)

recode_vars_for_hddmrl <- function(d) {
  
  d$feedback <- NULL # old variable, redundant
  d$feedback <- d$is_positive_feedback
  d$is_positive_feedback <- NULL
  
  d$feedback <- ifelse(
    d$rt < 180 | d$rt > 2499, NA, d$feedback
  )
  
  d$rt1 <- ifelse(
    d$rt < 180 | d$rt > 2499, NA, d$rt
  )
  d$rt1 <- ifelse(d$trial == 1, NA, d$rt1)
  
  d$rt <- d$rt1
  d$rt1 <- NULL
  
  d$subj_id <- factor(d$subj_id)
  d$code_psytoolkit <- factor(d$code_psytoolkit)
  d$sex <- factor(d$sex)
  d$time_start <- factor(d$time_start)
  d$time_end <- factor(d$time_end)
  d$resp <- factor(d$resp)
  
  d$is_target_chosen <- ifelse(
    d$trg_position == d$resp, 1, 0
  )
  
  temp <- data.frame(
    subj_id = d$subj_id,
    sex = d$sex,
    epoch = d$epoch,
    keypress = d$keypress,
    rt = d$rt,
    feedback = d$feedback,
    trial = d$trial,
    resp = d$resp,
    trg_position = d$trg_position,
    is_target_chosen = d$is_target_chosen
  )
  
  MLIM <- mlim(temp, m=1, seed = 2022, tuning_time = 180) 
  
  d$feedback <- round(MLIM$feedback)
  d$rt <- MLIM$rt
  d$resp <- MLIM$resp
  d$is_target_chosen <- MLIM$is_target_chosen
  
  # For hddmrl
  d$response <- d$is_target_chosen
  d$split_by <- d$response
  d$subj_idx <- as.numeric(as.factor(as.character(d$subj_id)))
  d$q_init <- 0.5
  
  # 
  # 
  # performance <- d |>
  #   group_by(subj_id) |>
  #   summarize(
  #     avg_fdbk = mean(feedback)
  #   ) |>
  #   as.data.frame()
  # 
  # above_chance <- performance[performance$avg_fdbk > 0.55, ]
  # good_ids <- above_chance$subj_id
  # 
  # d1 <- d[d$subj_id %in% good_ids, ]
  # 
  # out <- d |> 
  #   group_by(trial) |> 
  #   summarize(
  #     y = mean(feedback)
  #   )
  # 
  # plot(out$trial, out$y, type = 'l')
  
  d
  
}


######################## fino a qui!!!


# gen_hddm_input() --------------------------------------------------------


gen_hddm_input <- function(address_clean_data) {
  
  d <- readRDS(address_clean_data)
  
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
  
}


gen_hddm_input_patients_only <- function(address_patients_data) {
  
  d <- readRDS(address_patients_data)
  
  # response: which image has been chosen in each trial
  d$response <- d$is_target_img_chosen
  
  d$feedback <- ifelse(
    d$feedback == 2, 0, d$feedback
  )
  d$feedback <- ifelse(d$feedback == 3, NA, d$feedback)
  
  d$feedback <- 
    ifelse(d$rt < 200 | d$rt > 2499, NA, d$feedback)
  
  d$rt1 <- 
    ifelse(d$rt < 200 | d$rt > 2499, NA, d$rt)
  
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
    response    = d$response,
    category    = d$category
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
    mutate(subj_idx = group_indices(., subj_code) %>% grpid)
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
    category   = d1$category,
    subj_code  = d1$subj_code,
    q_init     = 0.5
  )
  
  mydat <- df_for_hddm %>% 
    dplyr::arrange(subj_idx, trial, split_by)
  
  # mydat %>% 
  #   group_by(subj_code, category) %>% 
  #   summarise(
  #     avg_feedbk = mean(feedback)
  #   ) %>% 
  #   as.data.frame()
  
  
  rio::export(
    mydat, 
    here("data", "processed", "prl", "data_for_hddm", "hddm_input_patients_only.csv")
  )
  
  lut <- data.frame(
    subj_idx = mydat$subj_idx, 
    subj_code = mydat$subj_code
  )
  
  rio::export(
    lut, 
    here("data", "processed", "prl", "data_for_hddm", "hddm_look_up_table_patients_only.csv")
  )
  
}
  
  
gen_hddm_all_with_diagn_cat <- function(address_tot_with_diagn_cat) {
    
    d <- readRDS(address_tot_with_diagn_cat)
    
    # response: which image has been chosen in each trial
    d$response <- d$is_target_img_chosen
    
    d$feedback <- ifelse(
      d$feedback == 2, 0, d$feedback
    )
    d$feedback <- ifelse(d$feedback == 3, NA, d$feedback)
    
    d$feedback <- 
      ifelse(d$rt < 200 | d$rt > 2499, NA, d$feedback)
    
    d$rt1 <- 
      ifelse(d$rt < 200 | d$rt > 2499, NA, d$rt)
    
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
      response    = d$response,
      stim        = d$stimulus_type,
      category    = d$diagn_cat
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
      mutate(subj_idx = group_indices(., subj_code) %>% grpid)
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
      category   = d1$diagn_cat,
      subj_code  = d1$subj_code,
      q_init     = 0.5
    )
    
    mydat <- df_for_hddm %>% 
      dplyr::arrange(subj_idx, trial, split_by)

    rio::export(
      mydat, 
      here("data", "processed", "prl", "data_for_hddm", "gen_hddm_all_with_diagn_cat.csv")
    )
    
    lut <- data.frame(
      subj_idx = mydat$subj_idx, 
      subj_code = mydat$subj_code
    )
    
    rio::export(
      lut, 
      here("data", "processed", "prl", "data_for_hddm", "hddm_look_up_table_all_with_diagn_cat.csv")
    )
  
}



























# gen_hddm_input_and_clean_controls_and_patients() ------------------------


gen_hddm_input_and_clean_controls_and_patients <- function() {
  
  d <- readRDS(
    here("data", "processed", "prl", "complete_raw_data", "tot_raw_data_prl.rds")
  )
  
  # response: which image has been chosen in each trial
  d$response <- d$is_target_img_chosen
  
  # raw_data$trial <- rep(1:160, nrow(raw_data) / 160)
  
  # recode feedback as 0, 1
  d$feedback <- ifelse(
    d$feedback == 2, 0, d$feedback
  )
  d$feedback <- ifelse(d$feedback == 3, NA, d$feedback)
  
  # remove controls with too many NAs
  bad_ids_keypress <- find_subj_code_extreme_keypress(d)
  d1 <- d[!(d$subj_name %in% bad_ids_keypress), ]
  d1$subj_name <- factor(d1$subj_name)
  # table(d1$subj_name)
  # length(unique(d1$subj_name))
  
  # Compute is_rich_choice: whether the subject has chosen the stimulus with the
  # highest probability of being rewarded.
  d1$is_rich_choice <- case_when(
    d1$is_target_rewared_in_present_epoch & d1$is_target_img_chosen ~ 1,
    !d1$is_target_rewared_in_present_epoch & !d1$is_target_img_chosen ~ 1,
    TRUE ~ 0
  )
  
  d1$stimulus_type <- factor(d1$stimulus_type)
  d1$stimulus_type <- d1$stimulus_type %>% 
    forcats::fct_recode(
      neutral = "socialshame"
    )
  
  d2 <- d1 %>% 
    dplyr::select(
      subj_name, stimulus_type, epoch, target_position, keypress,
      rt, feedback, is_target_img_rewarded_in_first_epoch, resp,
      trial, group, response, is_rich_choice
    )
  
  foo <- d2 %>% 
    group_by(group, subj_name, stimulus_type) %>% 
    summarise(
      avg_rich_choices = mean(is_rich_choice, na.rm = TRUE),
      avg_feedback = mean(feedback, na.rm = TRUE),
      n = n()
    ) %>% 
    as.data.frame()
  
  # select only PRL with no-food stimuli
  foo1 <- foo %>% 
    dplyr::filter(stimulus_type == "neutral")
  
  # flag subjects with a proportion of rich choices <= 0.5 with no-food stimuli
  bad_ids_accuracy <- foo1[foo1$avg_rich_choices <= 0.5, ]$subj_name
  # bad_ids_accuracy <- foo[foo$avg_rich_choices <= 0.5, ]$subj_name
  
  # removed flagged subjects (both controls and patients)
  d3 <- d2[!(d2$subj_name %in% bad_ids_accuracy), ]
  # d2 <- d1[!(d1$subj_name %in% bad_ids_accuracy & d1$group == "controls"), ]
  # length(unique(d3$subj_name))
  # length(unique(d3[d3$group == "patients", ]$subj_name))
  
  d3 %>% 
    group_by(group, stimulus_type) %>% 
    summarise(
      avg_rich_choices = mean(is_rich_choice, na.rm = TRUE)
    )
  
  d3$feedback <- 
    ifelse(d3$rt < 50 | d3$rt > 2499, NA, d3$feedback)
  
  d3$rt1 <- 
    ifelse(d3$rt < 50 | d3$rt > 2499, NA, d3$rt)
  
  foo <- d3 %>% 
    group_by(group, subj_name, stimulus_type) %>% 
    summarise(
      sumNA = sum(is.na(rt1)),
      acc = mean(is_rich_choice, na.rm = TRUE),
      n = n()
    ) %>% 
    as.data.frame()
  
  # plot(foo$sumNA, foo$acc)
  
  # bad_ids_rt_nas <- 
  #   factor(unique(foo[foo$sumNA > 30 & foo$group == "controls", ]$subj_name))
  # 
  # d3 <- d2[!(d2$subj_name %in% bad_ids_rt_nas), ]
  # length(unique(d3$subj_name))
  
  
  # Multiple imputation on NAs.
  d3$is_patient <- ifelse(d3$group == "patients", 1, 0)
  d3$rt1 <- ifelse(d3$trial == 1, NA, d3$rt1)
  d3$stimulus_type <- factor(d3$stimulus_type)
  d3$is_stimulus_food <- ifelse(d3$stimulus_type == "food", 1, 0)
  d3$is_keypress_a <- ifelse(d3$keypress == 1, 1, 0)
  
  temp <- data.frame(
    rt1 = d3$rt1, 
    trial = d3$trial,
    feedback = d3$feedback, 
    is_rich_choice = d3$is_rich_choice,
    is_patient = d3$is_patient,
    response = d3$response,
    is_keypress_a = d3$is_keypress_a,
    is_stimulus_food = d3$is_stimulus_food,
    trial = d3$trial
  )
  
  # Imputes the "best value" according to the linear regression model, also 
  # known as regression imputation.
  imp <- mice::mice(temp, method = "norm.predict", m = 1) 
  temp <- complete(imp)
  
  d3$feedback <- ifelse(temp$feedback > 0.5, 1, 0)
  d3$rt <- temp$rt1 / 1000
  
  # I want to number each subject in the data.frame so that subjects are 
  # ordered sequentially, according to the order they appear in the data frame. 
  # https://community.rstudio.com/t/why-does-group-indices-use-alphabetical-ordering/5452
  # As suggested in the above link, I wrap group_indices in another function:
  grpid = function(x) match(x, unique(x))
  # then
  d4 <- d3 %>% 
    mutate(subj_idx = group_indices(., subj_name) %>% grpid)
  # In this manner, the variable subj_idx assigns an integer to each subject;
  # this integer is ordered according to the sequence in which the subjects are 
  # present in the data.frame.
  # table(d3$subj_idx)
  # unique(d3$subj_idx)
  
  d4 <- d4 %>% 
    dplyr::rename(
      stim = stimulus_type
    )
  d4$split_by <- ifelse(d4$stim == "food", 0, 1)
  
  df_for_hddm <- data.frame(
    subj_idx = d4$subj_idx,
    response = d4$response,
    stim = d4$stim,
    rt = d4$rt,
    trial = d4$trial,
    split_by = d4$split_by,
    feedback = d4$feedback,
    is_patient = d4$is_patient,
    subj_code = d4$subj_name,
    q_init = 0.5
  )
  
  # temp <- df_for_hddm[with(df_for_hddm, order(subj_idx, trial, split_by)), ]
  
  mydat <- df_for_hddm %>% 
    dplyr::arrange(subj_idx, trial, split_by)
  
  mydat %>% 
    group_by(is_patient) %>% 
    summarize(cnt = n_distinct(subj_idx))
  
  rio::export(
    mydat, 
    here("data", "processed", "prl", "data_for_hddm", "con_and_pat_cleaned_2021_10_18.csv")
  )
  
  lut <- data.frame(
    subj_idx = mydat$subj_idx, 
    subj_code = mydat$subj_code
  )
  
  rio::export(
    lut, 
    here("data", "processed", "prl", "data_for_hddm", "con_and_pat_look_up_table_2021_10_18.csv")
  )
  
  if(0) {
    # test data
    temp <- mydat[(mydat$subj_idx == 2 | mydat$subj_idx == 100), ]
    
    temp$subj_idx <- ifelse(
      temp$subj_idx == 100, 1, temp$subj_idx
    )
    
    rio::export(
      temp, 
      here("data", "processed", "prl", "data_for_hddm", "prova.csv")
    )
  }
  
}



# standardize_num_vars() --------------------------------------------------


#' Standardize data.
#' @description 
#' standardize all numeric variables of a data.frame.
#' @return A data.frame.
#' @param file Character, data file path.
standardize_num_vars <- function(df) {
  # Loop over each column.
  for (colName in names(df)) {
    # Check if the column contains numeric data.
    if(
      class(df[, colName]) == 'integer' | 
      class(df[, colName]) == 'numeric'
    ) {
      # Scale this column (scale() function applies z-scaling).
      df[, colName] <- scale(df[,colName]) %>% 
        as.numeric()
    }
  }
  df
}



# get_bad_ids_prop_posivive_feedback() ------------------------------------


#' @description 
#' check proportion of positive feedback.
#' @return vector with subj_code of bad participants.
get_bad_ids_prop_posivive_feedback <- function() {
  
  raw_data <- readRDS(
    here("data", "processed", "prl", "complete_raw_data", "tot_raw_data_prl.rds")
  )
  
  raw_data$fdb <- ifelse(
    raw_data$feedback == 1, 1, ifelse(raw_data$feedback == 2, 0, NA)
  )
  
  tbl_fdb <- raw_data %>% 
    group_by(subj_name) %>% 
    summarise(
      p = mean(fdb, na.rm = TRUE),
      med_rt = median(rt, na.rm = TRUE)
    ) %>% 
    as.data.frame()
  
  bad_tbl_fdb <- tbl_fdb %>% 
    dplyr::filter(
      p < 0.5
    )
  
  bad_tbl_fdb$subj_name
}



# get_patients_codes() ----------------------------------------------------


#' @description  get patients' subj_codes.
#' @return data.frame.
get_patients_codes <- function() {
  
  foo <- rio::import(
    here("data", "processed", "prl", "complete_raw_data", "tot_raw_data_prl.rds")
  )
  
  temp <- foo %>% 
    dplyr::select(subj_name, code_psytoolkit)
  
  temp$is_patient <- stringr::str_detect(temp$code_psytoolkit, "pazienti")
  
  temp1 <- temp[temp$is_patient == 1, ]
  
  out <- factor(unique(temp1$subj_name)) %>% 
    forcats::fct_drop()
  out <- out[!is.na(out)]
  out
}




# generate_rds_prl_params_and_quest_data() --------------------------------


#' @title generate RDS file for PRL params and quest data
#' @description generate RDS file for PRL params and quest data
#' @return An RDS file.
#' @export
#' @param vector with patients' subj_code 
generate_rds_prl_params_and_quest_data <- function(patients_codes) {
  
  # look-up table for the PRL task: integer IDs and subj_coe for all participants
  lookup_tbl <- rio::import(
    here::here("data", "processed", "prl", "output_hddm", "prl_lookup_table.csv")
  )
  
  # file with the output of the python hDDM analysis.
  # Correct the first line:
  # ii mean std 2.5q 25q 50q 75q 97.5q mc_err
  # Delete the last three lines and the lines with the estimates of the fixed
  # effects -- preserve only the estimates of the random effects
  params_prl_ddm <- rio::import(
    here::here("data", "processed", "prl", "output_hddm", "params_hddm.txt")
  )
  
  # get the first two characters from params_prl_ddm::
  params_prl_ddm$param <- substr(params_prl_ddm$ii, start = 1, stop = 2)
  # params_prl_ddm$param[1:10]
  # [1] "a_" "a_" "a_" "a_" "a_" "a_" "a_" "a_" "a_" "a_"
  
  # get integers
  params_prl_ddm$id_param_string <- stringr::str_remove(params_prl_ddm$ii, "[.]")
  params_prl_ddm$subj_idx <- readr::parse_number(params_prl_ddm$id_param_string)
  # params_prl_ddm$subj_idx[1:10]
  # [1] 0 1 2 3 4 5 6 7 8 9
  
  # Change the levels names of the PRL parameters factor  
  # summary(factor(params_prl_ddm$param))
  params_prl_ddm$is_food <- stringr::str_detect(params_prl_ddm$ii, "food")
  params_prl_ddm$is_social <- stringr::str_detect(params_prl_ddm$ii, "social")
  # summary(factor(params_prl_ddm$param))
  
  # Create a single columns with the names of the PRL parameters
  params_prl_ddm <- params_prl_ddm %>% 
    mutate(stim = case_when(
      is_food == TRUE  & is_social == FALSE ~ "food",      
      is_food == FALSE & is_social == TRUE  ~ "social",  
      TRUE                                  ~ "neither")) 
  # data.frame(params_prl_ddm$ii, params_prl_ddm$stim)[500:1000, ]
  
  params_prl_ddm$params <- params_prl_ddm$param %>% 
    dplyr::recode(
      "a_" = "a",
      "al" = "alpha_neg",
      "po" = "alpha_pos",
      "t_" = "t",
      "v_" = "v",
      "z_" = "z"
    )
  # summary(factor(params_prl_ddm$params))
  
  params_prl_ddm_clean <- params_prl_ddm %>% 
    dplyr::select(
      subj_idx, stim, params, mean
    ) %>% 
    dplyr::rename(
      value = mean
    )
  
  params_prl_ddm_clean$pp <- 
    paste(params_prl_ddm_clean$params, params_prl_ddm_clean$stim, sep="_")
  
  params_prl_ddm_clean$stim <- NULL
  params_prl_ddm_clean$params <- NULL
  # summary(factor(params_prl_ddm_clean$pp))
  # a_neither alpha_neg_food alpha_neg_social alpha_pos_food ...
  
  # create a wide data.frame with one column for each PRL parameter
  params_prl_wide <- params_prl_ddm_clean %>%
    pivot_wider(names_from = pp, values_from = value)
  
  # add subj_code
  params_hddm_prl_df <- left_join(params_prl_wide, lookup_tbl, by = "subj_idx") %>% 
    dplyr::rename(
      subj_code = subj_name
    )
  
  # final data.frame with the parameters of the hDDM model for the PRL task
  # read questionnaires data
  quest_data <- readRDS(
    here::here("data", "processed", "quest", "quest_data.rds")
  )
  
  # select only the questionnaires sub-scales
  quest_scales <- quest_data %>% 
    dplyr::select(
      subj_code, bsq14_tot, ros_tot, dass21_stress, dass21_anxiety, 
      dass21_dep, sias, mps_ps, mps_o, mps_cmd, mps_pepc, orto_tot,
      dieting, bulimia, oral_control, eat26_at_risk
    )
  
  # combine questionnaires data and PRL params
  params_quest_prl <- left_join(params_hddm_prl_df, quest_scales, by = "subj_code")
  
  # add is_patient columns
  # sort(quest_scales$subj_code)
  params_quest_prl$is_patient <- ifelse(
    params_quest_prl$subj_code %in% patients_codes, 1, 0
  )
  
  # create group variable: patient, at_risk, control
  params_quest_prl <- params_quest_prl %>% 
    mutate(group = case_when(
      is_patient == 1 ~ "patient",      
      is_patient == 0 & eat26_at_risk == 1 ~ "at_risk",  
      TRUE                                 ~ "control"
    )
    ) 
  # table(params_quest_prl$group)
  
  saveRDS(
    params_quest_prl,
    here::here("data", "processed", "prl", "prl_and_quest", 
               "prl_params_and_quest_data.rds")
  )
  
}



# find_bad_controls() -----------------------------------------------------


#' @description 
#' find bad controls, with a proportion of positive feedbacks too low in 
#' comparison to the group
#' @return vector. 
find_bad_controls <- function(THRESHOLD) {
  
  # find patients' subj_code
  patients_subj_codes <- find_subj_code_of_patients()
  
  # read raw PRL data
  d <- readRDS(
    here("data", "processed", "prl", "complete_raw_data", "tot_raw_data_prl.rds")
  ) %>% 
    dplyr::rename(
      subj_code = subj_name
    )
  
  # Convert all character columns to factors.
  d <- d %>% 
    mutate(across(where(is_character), as_factor))
  
  # add column distinguishing patients from controls
  d$is_patient <- ifelse(d$subj_code %in% patients_subj_codes, 1, 0)
  
  d$feedback_num <- d$is_positive_feedback %>% 
    forcats::fct_recode(
      "1" = "yes", 
      "0" = "no",
      "NA" = "no response"
    )
  d$feedback_num <- as.integer(as.character(d$feedback_num))
  
  foo <- d %>% 
    group_by(is_patient, subj_code) %>% 
    summarise(
      tot_reward = sum(feedback_num, na.rm = TRUE),
      n = n(),
      p = tot_reward / n
    )
  
  # foo %>% 
  #   as.data.frame()
  foo %>%
    ggplot(aes(x=p, color=is_patient, fill=is_patient)) +
    geom_histogram(alpha=0.6) +
    facet_wrap(~is_patient)
  
  foo$is_bad <- ifelse(foo$is_patient == 0 & foo$p <= THRESHOLD, 1, 0)
  
  bad_controls <- foo %>% 
    dplyr::filter(is_bad == 1)  
  
  bad_controls$subj_code <- factor(bad_controls$subj_code)
  
  bad_controls$subj_code
}



# find_bad_RTs_participants() ---------------------------------------------


#' @description 
#' find bad controls, with very short RTs
#' @return vector. 
find_bad_RTs_participants <- function(patients_codes) {
  
  # read raw PRL data
  d <- readRDS(
    here("data", "processed", "prl", "complete_raw_data", "tot_raw_data_prl.rds")
  ) %>% 
    dplyr::rename(
      subj_code = subj_name
    )
  
  # Convert all character columns to factors.
  d <- d %>% 
    mutate(across(where(is_character), as_factor))
  # sort(unique(d$subj_code))
  
  # add column distinguishing patients from controls
  d$is_patient <- ifelse(d$subj_code %in% patients_codes, 1, 0)
  
  d$feedback_num <- d$is_positive_feedback %>% 
    forcats::fct_recode(
      "1" = "yes", 
      "0" = "no",
      "NA" = "no response"
    )
  d$feedback_num <- as.integer(as.character(d$feedback_num))
  
  foo <- d %>% 
    group_by(is_patient, subj_code) %>% 
    summarise(
      tot_reward = sum(feedback_num, na.rm = TRUE),
      n = n(),
      p = tot_reward / n,
      mrt = median(rt, na.rm = TRUE)
    ) %>% 
    as.data.frame()
  
  # foo %>% 
  #   as.data.frame()
  foo %>%
    ggplot(aes(x=mrt, color=is_patient, fill=is_patient)) +
    geom_histogram(alpha=0.6) +
    facet_wrap(~is_patient)
  
  foo1 <- foo[foo$is_patient == 0, ]
  out <- boxplot(foo1$mrt)
  
  foo$is_bad <- ifelse((foo$is_patient == 0 & (foo$mrt <= 250 | foo$mrt > 950)), 1, 0)
  
  bad_rts <- foo %>% 
    dplyr::filter(is_bad == 1)  
  
  bad_rts$subj_code <- factor(bad_rts$subj_code)
  
  bad_rts$subj_code
}





# recode_predicted_weight() -----------------------------------------------


#' @description 
#' recode expected weight without eating control
#' @param vector with original data
#' @return vector. 
recode_predicted_weight <- function(d) {
  
  d$predicted_weight <- stringr::str_remove(d$predicted_weight, "[kg]")
  d$predicted_weight <- stringr::str_remove(d$predicted_weight, "[g]")
  
  d$predicted_weight <- recode(
    d$predicted_weight,
    "non sono in rado di stabilirlo con sicurezza, ma sono piuttosto sicura che sarebbe superiore a quello attuale" = "0",
    "40/42 " = "41",
    "60/65"  = "62.5",
    "58/60 " = "59"
  )
  d$predicted_weight <- as.numeric(as.character(d$predicted_weight))
  d$predicted_weight <- ifelse(d$predicted_weight == 0, NA, d$predicted_weight)
  d$predicted_weight
}



# find_subj_code_of_patients() --------------------------------------------


#' @description 
#' find subj_code for all patients.
#' @return vector.
find_subj_code_of_patients <- function() {
  
  require("purrr")
  
  d <- readRDS(
    here("data", "processed", "prl", "complete_raw_data", "tot_raw_data_prl.rds")
  ) %>% 
    dplyr::rename(
      subj_code = subj_name
    )
  
  # Convert all character columns to factors.
  d <- d %>% 
    mutate(across(where(is_character), as_factor))
  
  foo <- unique(d$code_psytoolkit)
  out <- str_match(d$code_psytoolkit, "pazienti")
  
  d$is_patient <- ifelse(out == "pazienti", 1, 0)
  
  pat_df <- d %>% 
    dplyr::filter(is_patient == 1)
  
  pat_df$subj_code <- pat_df$subj_code %>% 
    forcats::fct_drop()
  
  unique(pat_df$subj_code) %>% 
    purrr::discard(is.na)
}




# clean_complete_raw_data() -----------------------------------------------


clean_complete_raw_data <- function() {
  
  d <- readRDS(
    here(
      "data", "processed", "prl", "complete_raw_data", "tot_raw_data_prl.rds"
    )
  )
  
  d$feedback <- ifelse(d$feedback == 2, 0, d$feedback)
  d$feedback <- ifelse(d$feedback == 3, NA, d$feedback)
  
  pat <- d[d$group != "patients", ]
  
  pat %>% 
    group_by(subj_name, stimulus_type) %>% 
    summarise(
      avg_feedback = mean(feedback, na.rm = TRUE),
      mrt = median(rt, na.rm = TRUE),
      q10_rt = quantile(rt, probs = 0.1, na.rm = TRUE)
    ) %>% 
    as.data.frame()
  
  bysubj_median_rt <- d %>% 
    group_by(subj_name, stimulus_type) %>% 
    summarise(
      avg_feedback = mean(feedback, na.rm = TRUE),
      me_rt = median(rt, na.rm = TRUE),
      q10_rt = quantile(rt, probs = 0.1, na.rm = TRUE)
    ) 
  
  good_subj_median_rt <- bysubj_median_rt %>% 
    dplyr::filter(me_rt > 300 & me_rt < 2500 & avg_feedback > 0.5)
  
  d1 <- d[d$subj_name %in% good_subj_median_rt$subj_name, ]
  
  # remove subjects of the control group who performed poorly
  # (acc < 0.5)
  d1 %>% 
    group_by(group) %>% 
    filter(rt > 300) %>% 
    summarise(
      # overall_acc = mean(is_correct_resp),
      overall_feedback = mean(feedback, na.rm = TRUE),
      overall_rt = mean(rt),
      med_rt = median(rt),
      n = n()/160
    ) %>% 
    as.data.frame()
  
}



# gen_data_for_hddm() -----------------------------------------------------


#' @description 
# Generate CSV file to be used as input for hDDM.
gen_data_for_hddm <- function() {
  
  raw_data <- readRDS(
    here("data", "processed", "prl", "complete_raw_data", "tot_raw_data_prl.rds")
  )
  
  raw_data$subj_idx <- as.numeric(
    factor(as.character(raw_data$subj_name))
  ) 
  # sort(unique(raw_data$subj_idx))
  
  # response: which image has been chosen in each trial
  raw_data$response <- raw_data$is_target_img_chosen
  
  raw_data$rt <- raw_data$rt / 1000
  
  # raw_data$trial <- rep(1:160, nrow(raw_data) / 160)
  
  raw_data$feedback <- ifelse(
    raw_data$feedback == 2, 0, raw_data$feedback
  )
  raw_data$feedback <- ifelse(raw_data$feedback == 3, NA, raw_data$feedback)
  
  raw_data$rt <- ifelse(
    raw_data$rt < 0.1 | raw_data$rt > 2.49 | is.na(raw_data$feedback), 
    NA, raw_data$rt)
  # hist(raw_data$rt)
  
  raw_data_without_missing_data <- raw_data[!is.na(raw_data$rt), ]
  
  raw_data_without_missing_data <- 
    raw_data_without_missing_data[!is.na(raw_data_without_missing_data$subj_name), ]
  
  # correct one subject code 
  raw_data_without_missing_data$subj_name <- 
    raw_data_without_missing_data$subj_name %>% 
    forcats::fct_recode(
      "ma_te_2001_05_31_333_m" = "ma a_te_2001_05_31_333_m"
    )
  
  # create look-up table with the integer indicating the subject ID used for
  # hDDm and the corresponding subject code
  lookup_tbl <- raw_data_without_missing_data %>% 
    dplyr::select(subj_idx,	subj_name) 
  # sort(unique(lookup_tbl$subj_idx))
  
  
  deduped_tbl <- unique(lookup_tbl[, 1:2])
  rio::export(
    deduped_tbl, 
    here("data", "processed", "prl", "data_for_hddm", "prl_look_up_table.csv")
  )
  # ends here
  
  # select appropriate columns of combined_dat for hDDM
  ddm_dat <- raw_data_without_missing_data %>% 
    dplyr::select(
      subj_idx,	response,	stimulus,	rt,	trial,	feedback, group, subj_name
    ) %>% 
    dplyr::rename(
      "stim" = "stimulus"
    )
  
  ddm_dat$q_init = 0.5
  
  ddm_dat <- ddm_dat %>% 
    arrange(subj_idx)
  
  ddm_dat$split_by <- as.numeric(
    factor(as.character(ddm_dat$stim))
  ) - 1
  
  if (0) {
    data.frame(
      ddm_dat$split_by, ddm_dat$stim
    )[150:250, ]
  }
  # split_by = 0 : food
  # split_by = 1 : social
  
  # negative RTs for response == 0
  ddm_dat$rt <- ifelse(
    ddm_dat$response == 0, -ddm_dat$rt, ddm_dat$rt
  )
  
  # save data in CSV files
  rio::export(
    ddm_dat, 
    here("data", "processed", "prl", "data_for_hddm", "prl_data_for_hddm.csv")
  )
}





