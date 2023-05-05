

d <- rio::import(
  here::here("scripts", "python", "PRL", "classification", 
       "hddm_input_20220603v2.csv")
)

n_sessions_bysubj <- d %>% 
  group_by(subj_idx, subj_code, diag_cat) %>% 
  summarise(
    n_stim = n_distinct(stim)
  ) %>% 
  as.data.frame()

temp <- n_sessions_bysubj %>% 
  dplyr::filter(n_stim == 2 & (diag_cat %in% c("AN", "HC")))

table(temp$diag_cat)

keep_subj <- temp$subj_code

balanced_design <- d %>% 
  dplyr::filter(subj_code %in% keep_subj)

balanced_design$subj_idx <- 
  as.numeric(factor(as.character(balanced_design$subj_idx))) - 1

unique(balanced_design$subj_idx) %>% sort()

balanced_design_sorted <- arrange(balanced_design, subj_idx)

rio::export(
  balanced_design_sorted,
  root("scripts/python/PRL/classification", 
       "input_hddmrl_balanced_an_hc.csv")
)
