# Updated 6th May 2021

source("prelims.R")

# need to start with full list of all words by age, global_network$gloss1

ages <- globalthresholds_AOP_providence %>% filter(data_type == "target") %>% dplyr::select(Speaker, gloss1, age)

AOP_summ <- globalthresholds_AOP_providence %>%
  mutate(Speaker_AOP = paste(Speaker, AOP, sep="_"),
         AOP = as.numeric((AOP))) %>%
  filter(data_type == "actual")

AOP_list <- AOP_summ %>%
  split(., f = .$Speaker_AOP)

global_distance_summ <- globalthresholds_AOP_providence %>%
  mutate(Speaker_AOP = paste(Speaker, age, sep="_"))

## ACTUAL DATA

output <- vector("list", length(79)) #Prep a list to store your corr.test results
names <- names(AOP_list)

prepared_data_actual <- lapply(AOP_list, FUN = function(element) {
  data <- globalthresholds_AOP_providence %>% filter(data_type == "actual" & Speaker == element$Speaker)
  timepoint <- data %>% filter(AOP == element$AOP)                              # select the timepoint for each network
  timepoint <- timepoint$AOP
  unknown <- data %>% filter(AOP > timepoint)                                   # filter so that only to-be-learned words are included
  known <- data %>% filter(AOP <= timepoint)                          # also create df for known words
  output <- list(timepoint, known, unknown)                           # merge into a list for working on further
})

prepared_data_actual <- setNames(prepared_data_actual, paste0(names))

prepared_df_actual <- melt(prepared_data_actual)

timepoint_df_actual <- prepared_df_actual %>% filter(L2 == 1) %>%
  dplyr::select(value, L1) %>%
  rename("timepoint" = "value",
         "Speaker_AOP" = "L1")

known_actual <- prepared_df_actual %>% filter(L2 == 2) %>%
  dplyr::select(value, Speaker, gloss1, variable, L1) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP" = "L1")

unknown_actual <- prepared_df_actual %>% filter(L2 == 3) %>%
  dplyr::select(value, Speaker, gloss1, variable, L1) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP" = "L1")

known_list_actual <- vector("list", length(79))

for (i in unique(known_actual$Speaker_AOP)) {
  connected_words <- global_distance_summ %>%                              # for each known word in each session, 
    filter(data_type == "actual") %>%                                      # which words in the existing lexicon does it connect to and what is the distance?
    group_by(Speaker, age) %>%
    filter(Speaker == first(global_distance_summ$Speaker[which(global_distance_summ$Speaker_AOP == i)]) &
             age ==  first(global_distance_summ$age[which(global_distance_summ$Speaker_AOP == i)])) %>%
    filter((gloss1 %in% known_actual$gloss1 |                 # find word pairs with the words in it
              gloss2 %in% known_actual$gloss1) &
             distance_norm <= .25) %>%                 # filter out any pairs that don't meet the similarity threshold
    distinct(word_pair, .keep_all = T)# %>%             # get rid of repeats
  known_list_actual[[i]] <- connected_words
}

connected_words_red_actual <- known_list_actual[c(2:80)]
connected_words_melted_actual <- melt(connected_words_red_actual) %>%
  filter(variable == "distance_norm") %>%
  mutate(age = as.numeric(age))

feather::write_feather(connected_words_melted_actual, "Data/connected_words_melted_actual.feather")

connected_words_melted_actual <- feather::read_feather("Data/connected_words_melted_actual.feather") %>% mutate(age = as.numeric(age))


# Mean degree of each word in terms of all the words that it connects to

gloss_summ <- global_network %>%
  filter(data_type == "actual") %>%
  mutate(Speaker_gloss = paste(Speaker, gloss1, sep="_"))

gloss_list <- gloss_summ %>%        # create a list of all words connected to the speaker who produces them
  split(., f = .$Speaker_gloss)

 
connected_degree_list <- vector("list", length(gloss_list))

connected_degree_actual <-lapply(gloss_list, FUN = function(element) {
  AOP_data <- AOP_summ %>% filter(gloss1 == element$gloss1)
  first_prod <- AOP_data$AOP
  connections <- global_distance %>%
    filter(Speaker == element$Speaker  & data_type == "actual" &
             (gloss1  == element$gloss1 | gloss2 == element$gloss1) &
             distance_norm <= 0.25) %>%
    mutate(keep_meA = ifelse(gloss1 != element$gloss1 & (gloss1 %in% AOP_summ$gloss1[which(AOP_summ$AOP < first_prod)]),"y", "n")) %>%
    mutate(keep_meB = ifelse(gloss2 != element$gloss1 & (gloss2 %in% AOP_summ$gloss1[which(AOP_summ$AOP < first_prod)]),"y", "n")) %>%
    filter(keep_meA == "y" | keep_meB == "y") %>%
    distinct(word_pair, Speaker, distance, .keep_all = T) %>%
    mutate(known_word = ifelse(gloss1 == element$gloss1, gloss2, gloss1))
  connected_degree_list <- list(connections)
})

# next need to compute degree of each word in the known lexicon at each time point (might have this data somewhere around already)

connected_degree_actual_melted <- melt(connected_degree_actual) %>%
  filter(variable == "distance_norm") %>% rename("Speaker_gloss" = "L1",
                                                 "distance_norm" = "value") %>%
  dplyr::select(-keep_meA, -keep_meB, -variable, -L2)

feather::write_feather(connected_degree_actual_melted, "Data/connected_degree_actual_melted.feather")

connected_degree_actual_melted <- feather::read_feather("Data/connected_degree_actual_melted.feather") %>%
  mutate(age = as.numeric(age))

rlist::list.save(connected_degree_actual, "Data/connected_degree_actual.rdata")

connected_degree_actual <- rlist::list.load("Data/connected_degree_actual.rdata")

actual_global_degree <- feather::read_feather("Data/actual_globaldistance_list_degree.feather") %>%
  mutate(age = as.numeric(age))

known_degree_list <- vector("list", length(gloss_list))

known_words_degree_actual <-lapply(gloss_list, FUN = function(element) {
  connections <- connected_degree_actual_melted %>%
    filter(Speaker_gloss == element$Speaker_gloss)
  degrees <- actual_global_degree %>%
    filter(gloss1 %in% connections$known_word & (age < element$AOP)) %>%
    group_by(Speaker, age) %>%
    summarise(PAT_val = mean(degree))
  mean_degree_list <- list(degrees)
    })

AOP_summ_red <- AOP_summ %>% dplyr::select(Speaker, gloss1, AOP)

vocabsize_sub <- vocabsize %>% distinct(Speaker, age, vocab_size)

mean_degree_full_actual <- melt(known_words_degree_actual) %>%
  group_by(L1, variable) %>%
  mutate(rn = row_number()) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  separate(L1, into = c("remove", "gloss1"), sep = "_") %>%
  dplyr::select(-remove, -L2, -rn) %>%
  left_join(vocabsize_sub)

mean_degree_full_actual <- AOP_summ_red %>%
  left_join(mean_degree_full_actual) %>%
  filter(!is.na(PAT_val)) %>%
  mutate(data_type = "actual")

feather::write_feather(mean_degree_full_actual, "Data/mean_degree_full_actual.feather")

## TARGET DATA

output <- vector("list", length(79)) #Prep a list to store your corr.test results
names <- names(AOP_list)

prepared_data_target <- lapply(AOP_list, FUN = function(element) {
  data <- global_network %>% filter(data_type == "target" & Speaker == element$Speaker)
  timepoint <- data %>% filter(AOP == element$AOP)                              # select the timepoint for each network
  timepoint <- timepoint$AOP
  unknown <- data %>% filter(AOP > timepoint)                                   # filter so that only to-be-learned words are included
  known <- data %>% filter(AOP <= timepoint)                          # also create df for known words
  output <- list(timepoint, known, unknown)                           # merge into a list for working on further
})

prepared_data_target <- setNames(prepared_data_target, paste0(names))

prepared_df_target <- melt(prepared_data_target)

timepoint_df_target <- prepared_df_target %>% filter(L2 == 1) %>% 
  dplyr::select(value, L1) %>% 
  rename("timepoint" = "value",
         "Speaker_AOP" = "L1")

known_target <- prepared_df_target %>% filter(L2 == 2) %>% 
  dplyr::select(value, Speaker, gloss1, variable, L1) %>% 
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP" = "L1")

unknown_target <- prepared_df_target %>% filter(L2 == 3) %>%
  dplyr::select(value, Speaker, gloss1, variable, L1) %>% 
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP" = "L1")

output_connected_target <- vector("list", length(79)) #Prep a list to store your corr.test results

known_list_target <- vector("list", length(79))

for (i in unique(known_target$Speaker_AOP)) {
  connected_words <- global_distance_summ %>%                              # for each known word in each session, 
    filter(data_type == "target") %>%                                      # which words in the existing lexicon does it connect to and what is the distance?
    group_by(Speaker, age) %>%
    filter(Speaker == first(global_distance_summ$Speaker[which(global_distance_summ$Speaker_AOP == i)]) &
             age ==  first(global_distance_summ$age[which(global_distance_summ$Speaker_AOP == i)])) %>%
    filter((gloss1 %in% known_target$gloss1 |                 # find word pairs with the words in it
              gloss2 %in% known_target$gloss1) &
             distance_norm <= .25) %>%                 # filter out any pairs that don't meet the similarity threshold
    distinct(word_pair, .keep_all = T)# %>%             # get rid of repeats
  known_list_target[[i]] <- connected_words
}

connected_words_red_target <- known_list_target[c(2:79)]
connected_words_melted_target <- melt(connected_words_red_target) %>%
  filter(variable == "distance_norm") %>%
  mutate(age = as.numeric(age))

feather::write_feather(connected_words_melted_target, "Data/connected_words_melted_target.feather")

connected_words_melted_target <- feather::read_feather("Data/connected_words_melted_target.feather") %>% mutate(age = as.numeric(age))


# Mean degree of each word in terms of all the words that it connects to

gloss_summ <- global_network %>%
  filter(data_type == "target") %>%
  mutate(Speaker_gloss = paste(Speaker, gloss1, sep="_"))

gloss_list <- gloss_summ %>%        # create a list of all words connected to the speaker who produces them
  split(., f = .$Speaker_gloss)


connected_degree_list <- vector("list", length(gloss_list))

connected_degree_target <-lapply(gloss_list, FUN = function(element) {
  AOP_data <- AOP_summ %>% filter(gloss1 == element$gloss1)
  first_prod <- AOP_data$AOP
  connections <- global_distance %>%
    filter(Speaker == element$Speaker  & data_type == "target" &
             (gloss1  == element$gloss1 | gloss2 == element$gloss1) &
             distance_norm <= 0.25) %>%
    mutate(keep_meA = ifelse(gloss1 != element$gloss1 & (gloss1 %in% AOP_summ$gloss1[which(AOP_summ$AOP < first_prod)]),"y", "n")) %>%
    mutate(keep_meB = ifelse(gloss2 != element$gloss1 & (gloss2 %in% AOP_summ$gloss1[which(AOP_summ$AOP < first_prod)]),"y", "n")) %>%
    filter(keep_meA == "y" | keep_meB == "y") %>%
    distinct(word_pair, Speaker, distance, .keep_all = T) %>%
    mutate(known_word = ifelse(gloss1 == element$gloss1, gloss2, gloss1))
  connected_degree_list <- list(connections)
})

# next need to compute degree of each word in the known lexicon at each time point (might have this data somewhere around already)

connected_degree_target_melted <- melt(connected_degree_target) %>%
  filter(variable == "distance_norm") %>% rename("Speaker_gloss" = "L1",
                                                 "distance_norm" = "value") %>%
  dplyr::select(-keep_meA, -keep_meB, -variable, -L2)

feather::write_feather(connected_degree_target_melted, "Data/connected_degree_target_melted.feather")

connected_degree_target_melted <- feather::read_feather("Data/connected_degree_target_melted.feather") %>%
  mutate(age = as.numeric(age))

target_global_degree <- feather::read_feather("Data/target_globaldistance_list_degree.feather") %>%
  mutate(age = as.numeric(age))

rlist::list.save(connected_degree_target, "Data/connected_degree_target.rdata")

connected_degree_target <- rlist::list.load("Data/connected_degree_target.rdata")

target_global_degree <- feather::read_feather("Data/actual_globaldistance_list_degree.feather") %>%
  mutate(age = as.numeric(age))

known_degree_list <- vector("list", length(gloss_list))

known_words_degree_target <-lapply(gloss_list, FUN = function(element) {
  connections <- connected_degree_target_melted %>%
    filter(Speaker_gloss == element$Speaker_gloss)
  degrees <- target_global_degree %>%
    filter(gloss1 %in% connections$known_word & (age < element$AOP)) %>%
    group_by(Speaker, age) %>%
    summarise(PAT_val = mean(degree))
  mean_degree_list <- list(degrees)
})

AOP_summ_red <- AOP_summ %>% dplyr::select(Speaker, gloss1, AOP)

vocabsize_sub <- vocabsize %>% distinct(Speaker, age, vocab_size)

mean_degree_full_target <- melt(known_words_degree_target) %>%
  group_by(L1, variable) %>%
  mutate(rn = row_number()) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  separate(L1, into = c("remove", "gloss1"), sep = "_") %>%
  dplyr::select(-remove, -L2, -rn) %>%
  left_join(vocabsize_sub)

mean_degree_full_target <- AOP_summ_red %>%
  left_join(mean_degree_full_target) %>%
  filter(!is.na(PAT_val)) %>%
  mutate(data_type = "target")

feather::write_feather(mean_degree_full_target, "Data/mean_degree_full_target.feather")

mean_degree_full_target <- feather::read_feather("Data/mean_degree_full_target.feather")
mean_degree_full_actual <- feather::read_feather("Data/mean_degree_full_actual.feather")

global_network <- global_network %>% 
  rename("PAQ_val" = "degree") %>%
  dplyr::select(-age, -threshold)

mean_degree_full <- rbind(mean_degree_full_actual, mean_degree_full_target)

comparison_data <- read_csv("Data/large_files/comparison_data.csv") %>%
  distinct(Gloss, Speaker, Targetphon) %>%
  rename("gloss1" = "Gloss")

regression_data <- mean_degree_full %>% left_join(global_network) %>%
  group_by(Speaker, gloss1, data_type) %>%
  mutate(learned_next = ifelse(age == max(age), 1, 0)) %>%
  left_join(comparison_data) %>%
  ungroup() %>%
  mutate(AOP_scaled = scale(AOP, center = TRUE, scale = TRUE),    
         length_scaled = scale(Targetphon, center = TRUE, scale = TRUE)) %>%
         #PAT_weighted = PAT_val/vocab_size) %>%
  group_by(Speaker) %>%
  mutate(PAT_scaled = scale(PAT_val, center = TRUE, scale = TRUE),
         PAQ_scaled = scale(PAQ_val, center = TRUE, scale = TRUE))

chi_freq <- read_csv("Data/chi_freq.csv")
chi_freq_bychi <- read_csv("Data/chi_freq_bychi.csv")
chi_freq_byword <- read_csv("Data/chi_freq_byword.csv")

regression_data <- regression_data %>%
  left_join(chi_freq_byword) %>%
  mutate(freq = ifelse(is.na(freq), 0, freq)) %>%
  mutate(freq_scaled = scale(freq, center = TRUE, scale = TRUE),
         vocab_scaled = scale(vocab_size, center = TRUE, scale = TRUE))

#feather::write_feather(regression_data, "Data/regression_data.feather")
#regression_data <- feather::read_feather("Data/regression_data.feather")

#regression_data_1 <- regression_data %>% filter(learned_next == 1)

# predictors for AOP

pat_formula_AOP <- as.formula("AOP_scaled ~ PAT_scaled + (1|Speaker)")
paq_formula_AOP <- as.formula("AOP_scaled ~ PAQ_scaled + (1|Speaker)")
all_formula_AOP <- as.formula("AOP_scaled ~ PAT_scaled + PAQ_scaled + length_scaled + freq_scaled +
                             length_scaled:PAT_scaled + length_scaled:PAQ_scaled +
                             freq_scaled:PAT_scaled + freq_scaled:PAQ_scaled +
                              (1|Speaker)")

formulas <- list(pat_formula_AOP,  paq_formula_AOP, all_formula_AOP)

coefs_agg_all <- data.frame()

for (formula in formulas){
  
  reg_agg_target_AOP <- lmer(formula, data=subset(regression_data, data_type == "target"))
  reg_agg_actual_AOP <- lmer(formula,  data=subset(regression_data, data_type == "actual"))
  
  #Extract coeficnet and confidence intervals
  coef_agg_fun <- function (model, meas_name) {
    conf <- confint(model, method ="Wald") 
    conf <- data.frame(predictor = row.names(conf), conf) %>%
      dplyr::filter(!(predictor %in% c('.sig01', '.sigma', '(Intercept)')))
    
    coef <- coef(summary(model))
    data.frame(predictor = row.names(coef), coef) %>%
      dplyr::filter(predictor !='(Intercept)') %>%
      left_join(conf) %>%
      mutate(measure = meas_name)
  }
  
  coef_agg_target_AOP <- coef_agg_fun(reg_agg_target_AOP, 'target')
  coef_agg_actual_AOP <- coef_agg_fun(reg_agg_actual_AOP, 'actual')
  
  
  coef_agg_form_AOP <- coef_agg_target_AOP %>%
    bind_rows(coef_agg_actual_AOP) %>%
    mutate(Test = ifelse(toString(formula[3])=='PAT_scaled + PAQ_scaled + length_scaled + freq_scaled +
                             length_scaled:PAT_scaled + length_scaled:PAQ_scaled +
                             freq_scaled:PAT_scaled + freq_scaled:PAQ_scaled +
                              (1|Speaker)', 'Combined', 'Individual'))
  
  coefs_agg_all_AOP <- bind_rows(coef_agg_target_AOP, coef_agg_actual_AOP)
  
}

#feather::write_feather(coefs_agg_all_AOP, "Data/static_preds_all_AOP_RED.feather")    # reduced version of data, with lemmas only
feather::write_feather(coefs_agg_all_AOP, "Data/static_preds_all_AOP.feather")


# predictors for learned_next

pat_formula_LN <- as.formula("learned_next ~ PAT_scaled + (1|Speaker)")
paq_formula_LN <- as.formula("learned_next ~ PAQ_scaled + (1|Speaker)")
all_formula_LN <- as.formula("learned_next ~ PAT_scaled + PAQ_scaled + length_scaled + freq_scaled + AOP_scaled +
                             PAT_scaled:AOP_scaled + PAQ_scaled:AOP_scaled +
                             length_scaled:AOP_scaled + length_scaled:PAT_scaled + length_scaled:PAQ_scaled +
                             freq_scaled:AOP_scaled + freq_scaled:PAT_scaled + freq_scaled:PAQ_scaled +
                              (1|Speaker)")

formulas <- list(pat_formula_LN,  paq_formula_LN, all_formula_LN)

coefs_agg_all <- data.frame()

for (formula in formulas){
  
  reg_agg_target_LN <- glmer(formula, data=subset(regression_data, data_type == "target"), family = binomial())
  reg_agg_actual_LN <- glmer(formula,  data=subset(regression_data, data_type == "actual"), family = binomial())
  
  #Extract coeficnet and confidence intervals
  coef_agg_fun <- function (model, meas_name) {
    conf <- confint(model, method ="Wald")
    conf <- data.frame(predictor = row.names(conf), conf) %>%
      dplyr::filter(!(predictor %in% c('.sig01', '.sigma', '(Intercept)')))
    
    coef <- coef(summary(model))
    data.frame(predictor = row.names(coef), coef) %>%
      dplyr::filter(predictor !='(Intercept)') %>%
      left_join(conf) %>%
      mutate(measure = meas_name)
  }
  
  coef_agg_target_LN <- coef_agg_fun(reg_agg_target_LN, 'target')
  coef_agg_actual_LN <- coef_agg_fun(reg_agg_actual_LN, 'actual')


  coef_agg_form_LN <- coef_agg_target_LN %>%
    bind_rows(coef_agg_actual_LN) %>%
    mutate(Test = ifelse(toString(formula[3])=='PAT_scaled + PAQ_scaled + length_scaled + freq_scaled + AOP_scaled +
                             PAT_scaled:AOP_scaled + PAQ_scaled:AOP_scaled +
                             length_scaled:AOP_scaled + length_scaled:PAT_scaled + length_scaled:PAQ_scaled +
                             freq_scaled:AOP_scaled + freq_scaled:PAT_scaled + freq_scaled:PAQ_scaled +
                              (1|Speaker)',
                         'Combined', 'Individual'))

  coefs_agg_all_LN <- bind_rows(coef_agg_target_LN, coef_agg_actual_LN)

}

#feather::write_feather(coefs_agg_all_LN, "Data/static_preds_all_LN_RED.feather")
feather::write_feather(coefs_agg_all_LN, "Data/static_preds_all_LN.feather")
