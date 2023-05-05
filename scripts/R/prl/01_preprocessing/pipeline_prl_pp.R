library("makepipe")
library("here")
library("tidyverse")
library("stringi")
library("readxl")

source(here::here("src", "R", "functions", "funs_prl.R"))


# Read data --------------------------------------------------------------------
make_with_recipe(
  note = "Read individual psytoolkit PRL files and generate 4 RDS files",
  recipe = {
    load_psychtoolkit_files()
  },
  targets = c(
    here("data", "processed", "prl", "data_social", 
         "controls_with_psychtoolkit_code.rds"),
    here("data", "processed", "prl", "data_food", 
       "controls_with_psychtoolkit_code.rds"),
    here("data", "processed", "prl", "data_social", 
       "patients_with_psychtoolkit_code.rds"),
    here("data", "processed", "prl", "data_food", 
       "patients_with_psychtoolkit_code.rds")
  )
)

# Create data list -----------------------------------------------------------
make_with_recipe(
  note = "1. Read the RDS files files with raw PRL data for each group and 
  condition and create a data_list. We also add the participant's identifier. 
  2. For each element of the list, we correct the subject's identifier. 
  3. Remove PRL sessions with too many NAs, bind the data frames in list and 
  save cleaned file.",
  recipe = {
    data_list <- write_prl_raw_data_list()
    d_list <- correct_subj_names(data_list)
    binding_cleaned_data_frames(d_list)
  },
  targets = c(
    here("data", "processed", "prl", "complete_cleaned_raw_data", 
         "raw_data_prl_both_groups.rds")
  ),
  dependencies = c(
    here("data", "processed", "prl", "data_social", 
         "controls_with_psychtoolkit_code.rds"),
    here("data", "processed", "prl", "data_food", 
         "controls_with_psychtoolkit_code.rds"),
    here("data", "processed", "prl", "data_social", 
         "patients_with_psychtoolkit_code.rds"),
    here("data", "processed", "prl", "data_food", 
         "patients_with_psychtoolkit_code.rds")
  )
)

# Add diagnostic category --------------------------------------------------------------------
make_with_recipe(
  note = "Add diagnostic category.",
  recipe = {
    add_diagnostic_category()
  },
  targets = c(
    here::here("data", "processed", "prl", "raw_prl_data", "prl_tot_raw_data.rds")
  ),
  dependencies = c(
    here("data", "processed", "prl", "complete_cleaned_raw_data", 
         "raw_data_prl_both_groups.rds")
  )
)

# Add diagnostic category --------------------------------------------------------------------
make_with_recipe(
  note = "Add diagnostic category.",
  recipe = {
    write_input_for_hddmrl()
  },
  targets = c(
    here("data", "processed", "prl", "input_for_hddmrl", "hddm_input_v3.csv"),
    here("data", "processed", "prl", "input_for_hddmrl", "hddm_look_up_table_v3.csv")
  ),
  dependencies = c(
    here::here("data", "processed", "prl", "raw_prl_data", "prl_tot_raw_data.rds")
  )
)

# Display pipeline visualisation -----------------------------------------------
makepipe::show_pipeline()

pipe <- get_pipeline()
pipe$clean()


# End of file -------------------------------------------------------------

