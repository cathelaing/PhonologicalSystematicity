# Updated 10th July 2020

source("prelims.R")

library(wordbankr)

# This script creates a lexicon of all the words produced by infants in the dataset, including lemma and PoS data
# It gets a bit messy because the eventual dataset used for analysis (comparison_data) doesn't indicate PoS for each word; because of this, there is redundancy across the eventual
# lexicon, given that many words appear as (or are coded multiply as) more than one PoS category.

# The eventual lexicon, lexicon_complete.csv, can be used to match words in comparison_data by filtering only for those PoS that I want to include in the final analysis
# I recommend re-running the analysis also with only CDI words, given that the words used in the corpus are at times very idiosyncratic.

# lemma, mor and mor2 are taken directly from coding in CHILDES/CLAN, except for 714 words that had to be hand-coded due to having no PoS data. Many of these were coded as word play (wp)
# or child speech (chi - I think these are basically the same thing coded differently!) and so will be removed from the final analysis anyway.

##############################  Preliminary compiling of data - does not need re-running 

# base_alex.lex <- read_csv("CLAN_files/alex_lex.csv")
# base_lily.lex <- read_csv("CLAN_files/lily_lex.csv")
# base_violet.lex <- read_csv("CLAN_files/violet_lex.csv")
# base_william.lex <- read_csv("CLAN_files/william_lex.csv")
# base_naima.lex <- read_csv("CLAN_files/naima_lex.csv")
# 
# base.lex <- rbind(base_alex.lex, base_lily.lex, base_violet.lex, base_william.lex, base_naima.lex)
#   
# write_csv(base.lex, "Data/base.lex.csv")
# 
# base_alex.mor <- read_csv("CLAN_files/alex_mor.csv")
# base_lily.mor <- read_csv("CLAN_files/lily_mor.csv")
# base_violet.mor <- read_csv("CLAN_files/violet_mor.csv")
# base_william.mor <- read_csv("CLAN_files/william_mor.csv")
# base_naima.mor <- read_csv("CLAN_files/naima_mor.csv")
# 
# base.mor <- rbind(base_alex.mor, base_lily.mor, base_violet.mor, base_william.mor, base_naima.mor)
# 
# write_csv(base.mor, "Data/base.mor.csv")

##############################  Lexical data  ####################################

# Start by replacing all | with _ in base.mor.csv as these can't be searcher for within dplyr for some reason 
# Also replace all +_n with + (n_+n_butter+n_fly to n_butter+fly; replace +n_ with +, then _+ with _)

base.lex <- read_csv("Data/base.lex.csv") %>%
  rename("data" = "@UTF8")
base.mor <- read_csv("Data/base.mor.csv") %>%
  rename("data" = "@UTF8")
lexicon <- read_csv("Data/iweb_lexicon_updated.csv") %>%  # iWeb corpus
  dplyr::select(-wordID, -PoS)

base.lex_clean1 <- base.lex %>%
  filter(!str_detect(data, "From file"),
         !str_detect(data, "Speaker:"),
         !str_detect(data, "----"),
         !str_detect(data, "Total number of"),
         !str_detect(data, "Type/"),
         !str_detect(data, "with option"),
         !str_detect(data, "TTR"),
         !str_detect(data, ">"),
         !str_detect(data, "freq"),
         !str_detect(data, "ONLY")) %>%
  separate(col = data, into = c("n", "word"),
           sep = " ", remove = FALSE) %>%
  filter(grepl('^\\d+$', n)) %>% # remove non-numeric data from n
  distinct(word) %>%
  left_join(lexicon)

posessives <- base.lex_clean1 %>%
  filter(grepl("'s", word)) %>%
  separate(word, into = c("lemma", "poss"),
           sep = "'", remove = FALSE) %>%
  # mutate(lemma1 = lemma,
  #        lemma1 = str_replace(lemma1, "_", "+")) %>%
  dplyr::select(-poss)


base.lex_clean <- rbind(base.lex_clean1, posessives)

base.lex_clean <- base.lex_clean %>%
    distinct(lemma, word)

base.lex_clean %>% filter(is.na(lemma)) %>% tally()  # 2422 to check manually; do these below as I need to add morphological data

##############################  PoS data  ####################################

base.mor_clean <- base.mor %>%
  filter(!str_detect(data, "From file"),
         !str_detect(data, "Speaker:"),
         !str_detect(data, "Fri "),
         !str_detect(data, "ONLY"),
         !str_detect(data, ">"),
         !str_detect(data, "----"),
         !str_detect(data, "@"),
         !str_detect(data, "Aug-20"),
         !str_detect(data, "Total number of"),
         !str_detect(data, "Type/")) %>%
  separate(col = data, into = c("n", "data"),
           sep = " ", remove = TRUE) %>%
  dplyr::select(-n) %>%
  separate(col = "data", into = c("mor", "lemma"),
           sep = "_", remove = TRUE) %>%
  separate(col = lemma, into = c("lemma", "type"),    # lemma = full form
           sep = "-", remove = FALSE) %>%
  separate(col = mor, into = c("mor", "mor2"), 
           sep = ":", remove = FALSE) %>%
  mutate(lemma1 = str_replace(lemma, "\\&.*", ""),    # lemma1 = transcription form, showing consituents of the lemma
         lemma1 = tolower(lemma1)
         ) %>%
  distinct(lemma1, mor, mor2) %>%
  mutate(lemma = str_remove_all(lemma1, "[+]"),
         lemma = tolower(lemma)) %>%
  filter(mor != "n:let")

particles_subset <- base.mor_clean %>%
  filter(grepl("#", mor)) %>%
  separate(col = mor, into = c("mor2", "mor"), 
           sep = "#", remove = FALSE) %>%
  unite(lemma, mor2, lemma, sep = "", remove = FALSE) %>%
  unite(lemma1, mor2, lemma1, sep = "+", remove = FALSE) 

base.mor_clean <- rbind(base.mor_clean, particles_subset)

base.mor_clean <- base.mor_clean %>%                              # might need to go back and change this
  filter(!grepl("#", mor)) %>%
  mutate(lemma1 = ifelse(lemma == "comed", "come+d", lemma1),
         mor = ifelse(lemma == "comed", "v", mor),
         mor2 = ifelse(lemma == "comed", NA, mor2)) %>%
  mutate(mor2 = ifelse(mor2 == "step" | mor2 == "grand", "prop", mor2))

##############################  joining and checking  ####################################

full_lexicon <- base.lex_clean %>%
  mutate(word = tolower(word)) %>%
  distinct(word, lemma) %>%
  left_join(base.mor_clean) %>%
  distinct(word, lemma, mor, lemma1, .keep_all = TRUE)

lex_checks <- full_lexicon %>%
  filter(grepl("@", word),
         !grepl("@l", word)) %>%       # remove letters - will remove these from the full lexicon below
  separate(col = word, into = c("word", "mor"),
           sep = "@", remove = FALSE) %>%
  mutate(mor = as.factor(mor),
         mor = fct_recode(mor, "on" = "o",
                          "let" = "k",
                          "nons" = "n",
                          "singing" = "si"),
         mor = fct_collapse(mor, "chi" = c("b", "c", "f"),
                            "L2" = c("s:deu", "s:ell", "s:fra", "s:hin", "s:ita", "s:pan", "s:spa", "s:und", "s:yid")),
         lemma = ifelse(is.na(lemma), word, lemma),
         lemma1 = ifelse(is.na(lemma1), word, lemma1))

full_lexicon <- rbind(full_lexicon, lex_checks)

full_lexicon <- full_lexicon %>%
  filter(!grepl("@", word))

mini_lexicon <- full_lexicon %>%                 # create a mini lexicon to check against missing words in the checks below; 
  mutate(check = ifelse(is.na(lemma) & is.na(mor) & is.na(mor2) & is.na(lemma1), T, F)) %>%   # if words in lex_checks2 match a word in this df, they can be removed below
  filter(check == F)

lex_checks2 <- full_lexicon %>%
  mutate(check = ifelse(is.na(lemma) & is.na(mor) & is.na(mor2) & is.na(lemma1), T, F)) %>%
    mutate(inlex = ifelse(word %in% mini_lexicon$word, T, F)) %>%
  filter(inlex == F & check == T) %>%
  dplyr::select(-check, -inlex) %>%
  write_csv("Data/lexicon_checks2.csv")   # now need to code these manually :(

checks_done <- read_csv("Data/lexicon_checks_complete.csv") %>%
  filter(!is.na(lemma)) %>%                                              # remove words for which I couldn't establish a lemma
  mutate(lemma1 = ifelse(is.na(lemma1), lemma, lemma1))

complete_lexicon <- rbind(full_lexicon, checks_done)

complete_lexicon <- complete_lexicon %>%
  mutate(lemma = fct_recode(lemma, "poo" = "poop"), # change all poo + poopoo etc to poop....
         lemma = fct_recode(lemma, "yum" = "yummy"),
         mor = ifelse(word == "yummy", "adj", mor),
         lemma1 = ifelse(word == "yummy", "yum", lemma1)) 

more_checks_index <- complete_lexicon %>%
  group_by(word) %>%
  tally() %>%
  filter(n > 1)

more_checks <- complete_lexicon %>%
  mutate(check = ifelse(word %in% more_checks_index$word, T, F)) %>%
  filter(check == T)

##############################  complete data  ####################################

complete_lexicon %>% filter(mor == "n" | mor == "v") %>% tally()


feather::write_feather(complete_lexicon, "Data/lexicon.feather")

##############################  add CDI data  #########################################

lexicon_base <- feather::read_feather("Data/lexicon.feather") %>%
  distinct(word, lemma, mor, lemma1, .keep_all = TRUE) %>%
  mutate(filter = ifelse(is.na(lemma) & is.na(lemma1) & is.na(mor), T, F)) %>%
  filter(filter == F) %>%
  mutate(filter = ifelse(lemma == "#NAME?", T, F)) %>%
  filter(filter == F) %>%
  dplyr::select(-filter)

lexicon_base$lemma <- gsub('[[:punct:] ]+',' ',lexicon_base$lemma)

lexicon <- lexicon_base %>%
  mutate(lemma = as.factor(lemma),
         mor = as.factor(mor),
         word = as.factor(word),
         lemma = fct_recode(lemma, "woof woof" = "woof",                              # change a few words to make sure the lemma matches the CDI lemma
                            "quack quack" = "quack",
                            "baa baa" = "baa",
                            "choo choo" = "choo",
                            #"choo choo" = "choo+choo",
                            "yum yum" = "yum",
                            "uh oh" = "uhoh",
                            #"play dough" = "play doh",
                            "cockadoodledoo" = "cock a doo",
                            "cockadoodledoo" = "cock a doodle doo",
                            "grrr" = "grr",
                            "teddybear" = "teddy",
                            "beans" = "bean",
                            "carrots" = "carrot",
                            "cheerios" = "cheerio",
                            "grapes" = "grape",
                            #"ice cream" = "ice+cream",
                            "peas" = "pea",
                            "vitamins" = "vitamin",
                            "beads" = "bead",
                            "gloves" = "glove",
                            "mittens" = "mitten",
                            "sneaker" = "sneakers",
                            "lips" = "lip",
                            "play pen" = "playpen",
                            "good night" = "goodnight",
                            "thank you" = "thankyou")) %>%
  mutate(lemma = ifelse(word %in% c("fire+truck", "fire+trucks"), "fire truck", as.character(lemma)),
         lemma = ifelse(lemma %in% c("ow", "boo boo"), "owie", as.character(lemma)), 
         lemma = ifelse(word == "bubbles", "bubbles", as.character(lemma)),
         lemma = ifelse(word == "fry" & mor == "n", "french fries", as.character(lemma)), 
         lemma = ifelse(word == "chip" & mor == "n", "chips", as.character(lemma)),
         mor = ifelse(word == "pants" & mor == "v", "n", as.character(mor)), 
         mor = ifelse(word == "underpants", "n", as.character(mor)),
         mor = ifelse(word == "sneaker", "n", as.character(mor)),
         mor = ifelse(word == "sunglasses", "n", as.character(mor)),
         lemma = ifelse(word == "glasses", "glasses", as.character(lemma)),
         lemma = ifelse(lemma == "key" & mor == "n", "keys", as.character(lemma)),
         lemma = ifelse(word == "high+chair", "high chair", as.character(lemma)),
         lemma = ifelse(word == "rocking+chair", "rocking chair", as.character(lemma)),
         lemma = ifelse(word == "woods", "woods", as.character(lemma)),
         lemma = ifelse(word == "does", "does", as.character(lemma)),
         lemma = ifelse(word == "doesn't", "does", as.character(lemma)),
         lemma = ifelse(word == "gonna", "gonna", as.character(lemma)),
         lemma = ifelse(word == "gotta", "gotta", as.character(lemma)),
         lemma = ifelse(word == "wanna", "wanna", as.character(lemma)),
         lemma = ifelse(word == "were", "were", as.character(lemma)),
         lemma = ifelse(word == "weren't", "were", as.character(lemma))
         )

english_ws_items <- get_item_data("English (American)", "WS")       # import US English CDI data

english_ws_data <- get_instrument_data("English (American)", "WS")

eng_ws_data <- get_instrument_data(language = "English (American)",
                                   form = "WS",
                                   #items = c("item_1", "item_42"),
                                   administrations = TRUE,
                                   iteminfo = TRUE)

CDI_AoA_data_base <- fit_aoa(eng_ws_data, measure = "produces", method = "glmrob", proportion = 0.5)

CDI_AoA_data <- CDI_AoA_data_base %>%
  rename("lemma" = "uni_lemma")

CDI_AoA_data$lemma <- gsub("\\s*\\([^\\)]+\\)","", as.character(CDI_AoA_data2$lemma))  # remove words in brackets in $lemma

# inlex <- CDI_AoA_data %>% mutate(inlex = ifelse(lemma %in% lexicon$lemma, T, F)) %>%     # check that all lemmas in CDI are matched in the lexicon
#   filter(inlex == F & !is.na(lemma))

lexicon <- lexicon %>%
  mutate(inCDI = ifelse(lemma %in% CDI_AoA_data$lemma, T, F))

lexicon_CDI <- lexicon %>%
  left_join(CDI_AoA_data) %>%
  dplyr::select(word, lemma, lemma1, mor, mor2, inCDI, aoa, category, lexical_class)

############################## output ################## 

write_csv(lexicon_CDI, "Data/lexicon_CDI.csv")
