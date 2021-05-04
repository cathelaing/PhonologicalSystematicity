# Updated 28th April 2021

# This data takes the sample generated in data_cleaning.R and creates a series of phonetic distance values for each word in the dataframe

FULLsample <- feather::read_feather("Data/FULLsample.feather")


FULLsample$Session <- gsub("^[^.]*.", "", FULLsample$Session) # create variable to show session number in only numeric form
FULLsample$Session <- gsub('[\a\b]', '', FULLsample$Session)

# write_csv(FULLsample, "ProvidenceDataCHI.csv")

sample_IPAtarget <- FULLsample %>% select(ID, Speaker, Session, Gloss, 
                                          IPAtarget, IPAactual, 
                                          Targetphon, Actualphon, 
                                          TargetCV, ActualCV) # Create new dataframe to generate IPA segmental values

# substitute all target vowels for generic V because I don't care about vowels
sample_IPAtarget$Vremoved_target <- gsub("([
i
u
ɪ
ʊ
e
o
ə
ɛ
ʌ
ɔ
ɜ
æ                              
a
ɑ])", "V", sample_IPAtarget$IPAtarget)    # vowels taken from runnng Phone Inventory script in Phon

sample_IPAtarget$Vremoved_target <- gsub("VVV", "V", sample_IPAtarget$Vremoved_target)  # remove triphthongs to count as single vowel (following Monaghan et al 2010 but also because we're not looking at vowels here)
sample_IPAtarget$Vremoved_target <- gsub("VV", "V", sample_IPAtarget$Vremoved_target)  # remove diphthongs to count as single vowel (following Monaghan et al 2010 but also because we're not looking at vowels here)
sample_IPAtarget <- sample_IPAtarget %>% mutate(nsyl_target = stringr::str_count(Vremoved_target, "V"),
                                                nsyl_target = ifelse(nsyl_target == 0, 1, nsyl_target))


# substitute all actual vowels for generic V because I don't care about vowels here either

sample_IPAtarget$Vremoved_actual <- gsub("([ 
i
u
ɪ
ʊ
e
o
ə
ɛ
ʌ
ɔ
ɜ
æ
a
ɑ])", "V", sample_IPAtarget$IPAactual)    # vowels taken from runnng Phone Inventory script in Phon

sample_IPAtarget$Vremoved_actual <- gsub("VVV", "V", sample_IPAtarget$Vremoved_actual)  
sample_IPAtarget$Vremoved_actual <- gsub("VV", "V", sample_IPAtarget$Vremoved_actual)  
sample_IPAtarget <- sample_IPAtarget %>% mutate(nsyl_actual = stringr::str_count(Vremoved_actual, "V"),
                                                nsyl_actual = ifelse(nsyl_actual == 0, 1, nsyl_actual))


# Now split data by syllable structures since difference structures need treating differently when running a segment-by-segment comparison
# Create a new dataframe to gather this info, to be joined to sample_IPAtarget later

target_structures_sample <- as.data.frame(levels(sample_IPAtarget$TargetCV)) # list all structures in the data

target_structures_sample <- target_structures_sample %>%
  rename("TargetCV" = `levels(sample_IPAtarget$TargetCV)`)


# Create a new column that simplifies each structure by its 'core' syllabic properties

target_structures_sample$TargetCV_edited <- gsub("VVVV", "V", target_structures_sample$TargetCV)  
target_structures_sample$TargetCV_edited <- gsub("VVV", "V", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("VV", "V", target_structures_sample$TargetCV_edited)
target_structures_sample$TargetCV_edited <- gsub("G", "C", target_structures_sample$TargetCV_edited)  # counting glides as consonants, consistent with above
target_structures_sample$TargetCV_edited <- gsub("CCCC", "C", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("CCC", "C", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("CC", "C", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("^", "", target_structures_sample$TargetCV_edited)


target_structures_sample <- target_structures_sample %>%
  mutate(TargetCV_edited = as.factor(TargetCV_edited))

# Do the same for actual syllabic structure. This will allow for comparison of targetlikeness later on

sample_IPAtarget$ActualCV_edited <- gsub("VVVV", "V", sample_IPAtarget$ActualCV)  
sample_IPAtarget$ActualCV_edited <- gsub("VVV", "V", sample_IPAtarget$ActualCV_edited)  
sample_IPAtarget$ActualCV_edited <- gsub("VV", "V", sample_IPAtarget$ActualCV_edited)
sample_IPAtarget$ActualCV_edited <- gsub("G", "C", sample_IPAtarget$ActualCV_edited)  # counting glides as consonants, consistent with above
sample_IPAtarget$ActualCV_edited <- gsub("CCCC", "C", sample_IPAtarget$ActualCV_edited)  
sample_IPAtarget$ActualCV_edited <- gsub("CCC", "C", sample_IPAtarget$ActualCV_edited)  
sample_IPAtarget$ActualCV_edited <- gsub("CC", "C", sample_IPAtarget$ActualCV_edited)  
sample_IPAtarget$ActualCV_edited <- gsub("^", "", sample_IPAtarget$ActualCV_edited)



#levels(target_structures$structure_edited)

# create two new columns that alongside sample_IPAtarget$nsyl_target together will allow for filtering of specific word structures across the data

# openclosed = does the target form have a coda? (yes = closed, no = open)
# onset = is it a vowel (V) or consonant (C) at word onset?

target_structures_sample <- target_structures_sample %>% 
  mutate(openclosed = ifelse(TargetCV_edited %in% c("C", "CVC", "VC", "CVCVC", "VCVC", "CVCVC^CVC", "CVCVCVC", "VCVCVC", 
                                                     "CVCVCVCVC", "VCVCVCVC", "CVCVCVCVCVC", "VCVCVCVCVC", "CVCVCVCVCVCVC"), "closed", "open"),
         onset = ifelse(TargetCV_edited %in% c("V", "V^CV", "VC", "VCV", "VCVC", "VCVCV", "VCVCVC", "VCVCVCV", "VCVCVCVC", 
                                                "VCVCVCVCV", "VCVCVCVCVC", "VCVCVCVCVCV"), "V", "C"))

sample_IPAtarget <- sample_IPAtarget %>% left_join(target_structures_sample) # join with main dataframe

# Now each segment of each word needs to be separated in order to compare target forms with actual productions
# This process is done by syllable number, starting with target forms and then considering actual forms in relation to these

# For example: monosyllabic target /kat/ is separated into /k/~/a/~/t/ and then child's actual production is considered in relation to this
# Actual production might be a monosyllable ([kat]), or a disyllable [kaka] or multisyllabic [kakaka]. In each of these cases, /k/~/a/~/t/ as generated below
# is compared against the segments from the actual form 
# First all target monosyllables are compared with all target forms (from 1-6 syllables, V- and C-intial separately)
# Then disyllables (compared with 1-6 syllable forms, V- and C-initial), trisyllables, etc. up to 6-syllable words
# Words beyond 6 syllables tended to be produced with vocal play, and so were excluded from the analysis


nsyl_target_list <- sample_IPAtarget %>%
  split(., f = .$nsyl_target)

sample_IPAtarget_loop <- lapply(nsyl_target_list, FUN = function(element) {
  split_syl <- element %>% separate(Vremoved_target, c("S1C1_target", "S2C1_target", "S3C1_target", "S4C1_target", "S5C1_target", "S6C1_target", "SFC1_target"), "V") 
  split_clust <- split_syl %>% separate(S1C1_target, c("TS1C1", "TS1C2", "TS1C3", "TS1C4", "TS1C5"), sep = "(?<=.)") %>%
    separate(S2C1_target, c("TS2C1", "TS2C2", "TS2C3", "TS2C4", "TS2C5"), sep = "(?<=.)") %>%
    separate(S3C1_target, c("TS3C1", "TS3C2", "TS3C3", "TS3C4", "TS3C5"), sep = "(?<=.)") %>%
    separate(S4C1_target, c("TS4C1", "TS4C2", "TS4C3", "TS4C4", "TS4C5"), sep = "(?<=.)") %>%
    separate(S5C1_target, c("TS5C1", "TS5C2", "TS5C3", "TS5C4", "TS5C5"), sep = "(?<=.)") %>%
    separate(S6C1_target, c("TS6C1", "TS6C2", "TS6C3", "TS6C4", "TS6C5"), sep = "(?<=.)") %>%
    separate(SFC1_target, c("TSFC1", "TSFC2", "TSFC3", "TSFC4", "TSFC5"), sep = "(?<=.)")
})


# Now add segmental info re infants' actual productions to each DF

Vinitial <- sample_IPAtarget %>% filter(stringr::str_detect(ActualCV, "^V")) # DF for looking at V-intial structures only
Cinitial <- sample_IPAtarget %>% filter(stringr::str_detect(ActualCV, "^C")|stringr::str_detect(ActualCV, "^G"))    # DF for looking at C-intial structures only

# Remember to merge these subsets together once DF is organized

sample_IPAactual_loop <- lapply(sample_IPAtarget_loop, FUN = function(element) {
  split_syl_Cinit <- element %>% filter(ActualCV %in% Cinitial$ActualCV) %>%
    separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S6C1_actual", "SFC1_actual"), "V")
  split_clust_Cinit <- split_syl_Cinit %>% separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
    separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
    separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
    separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
    separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
    separate(S6C1_actual, c("AS6C1", "AS6C2", "AS6C3", "AS6C4", "AS6C5"), sep = "(?<=.)") %>%
    separate(SFC1_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
  split_syl_Vinit <- element %>% filter(ActualCV %in% Vinitial$ActualCV) %>%
    separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S6C1_actual", "SFC1_actual"), "V")
  split_clust_Vinit <- split_syl_Vinit %>% separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
    separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
    separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
    separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
    separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
    separate(S6C1_actual, c("AS6C1", "AS6C2", "AS6C3", "AS6C4", "AS6C5"), sep = "(?<=.)") %>%
    separate(SFC1_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
  sample_IPA_CVinit <- rbind(split_clust_Vinit, split_clust_Cinit)
})

actual_target_IPA_FULL <- do.call(rbind.data.frame, sample_IPAactual_loop)

# # Monosyllables
# 
# sample_IPAtarget_monosyl <- sample_IPAtarget %>% filter(nsyl_target == 1) %>% 
#   separate(Vremoved_target, c("S1C1_target", "S1CF_target"), "V") # take all target monosyllables and split into C1 and CF, removing vowels
# 
# sample_IPAtarget_monosyl <- sample_IPAtarget_monosyl %>%                     # break up clusters into individual segments
#   separate(S1C1_target, c("TS1C1", "TS1C2", "TS1C3", "TS1C4"), sep = "(?<=.)") %>%
#   separate(S1CF_target, c("TSFC1", "TSFC2", "TSFC3", "TSFC4"), sep = "(?<=.)")
# 
# # Repeat this below for all multisyllabic forms
# 
# # 2 syllables
# 
# sample_IPAtarget_disyl <- sample_IPAtarget %>% filter(nsyl_target == 2) %>% 
#   separate(Vremoved_target, c("S1C1_target", "S2C1_target", "S2CF_target"), "V") 
# 
# sample_IPAtarget_disyl <- sample_IPAtarget_disyl %>% 
#   separate(S1C1_target, c("TS1C1", "TS1C2", "TS1C3", "TS1C4"), sep = "(?<=.)") %>%
#   separate(S2C1_target, c("TS2C1", "TS2C2", "TS2C3", "TS2C4"), sep = "(?<=.)") %>%
#   separate(S2CF_target, c("TSFC1", "TSFC2", "TSFC3"), sep = "(?<=.)")
# 
# # 3 syllables
# 
# sample_IPAtarget_trisyl <- sample_IPAtarget %>% filter(nsyl_target == 3) %>% 
#   separate(Vremoved_target, c("S1C1_target", "S2C1_target", "S3C1_target", "S3CF_target"), "V") 
# 
# sample_IPAtarget_trisyl <- sample_IPAtarget_trisyl %>% 
#   separate(S1C1_target, c("TS1C1", "TS1C2", "TS1C3", "TS1C4"), sep = "(?<=.)") %>%
#   separate(S2C1_target, c("TS2C1", "TS2C2", "TS2C3", "TS2C4"), sep = "(?<=.)") %>%
#   separate(S3C1_target, c("TS3C1", "TS3C2", "TS3C3", "TS3C4"), sep = "(?<=.)") %>%
#     separate(S3CF_target, c("TSFC1", "TSFC2", "TSFC3", "TSFC4"), sep = "(?<=.)")
# 
# # 4 syllables
# 
# sample_IPAtarget_4syl <- sample_IPAtarget %>% filter(nsyl_target == 4) %>% 
#   separate(Vremoved_target, c("S1C1_target", "S2C1_target", "S3C1_target", "S4C1_target", "S4CF_target"), "V") 
# 
# sample_IPAtarget_4syl <- sample_IPAtarget_4syl %>% 
#   separate(S1C1_target, c("TS1C1", "TS1C2", "TS1C3", "TS1C4"), sep = "(?<=.)") %>%
#   separate(S2C1_target, c("TS2C1", "TS2C2", "TS2C3", "TS2C4"), sep = "(?<=.)") %>%
#   separate(S3C1_target, c("TS3C1", "TS3C2", "TS3C3", "TS3C4"), sep = "(?<=.)") %>%
#   separate(S4C1_target, c("TS4C1", "TS4C2", "TS4C3", "TS4C4"), sep = "(?<=.)") %>%
#   separate(S4CF_target, c("TSFC1", "TSFC2", "TSFC3", "TSFC4"), sep = "(?<=.)")
# 
# # 5 syllables
# 
# sample_IPAtarget_5syl <- sample_IPAtarget %>% filter(nsyl_target == 5) %>% 
#   separate(Vremoved_target, c("S1C1_target", "S2C1_target", "S3C1_target", "S4C1_target", "S5C1_target", "S5CF_target"), "V") 
# 
# sample_IPAtarget_5syl <- sample_IPAtarget_5syl %>% 
#   separate(S1C1_target, c("TS1C1", "TS1C2", "TS1C3", "TS1C4"), sep = "(?<=.)") %>%
#   separate(S2C1_target, c("TS2C1", "TS2C2", "TS2C3", "TS2C4"), sep = "(?<=.)") %>%
#   separate(S3C1_target, c("TS3C1", "TS3C2", "TS3C3", "TS3C4"), sep = "(?<=.)") %>%
#   separate(S4C1_target, c("TS4C1", "TS4C2", "TS4C3", "TS4C4"), sep = "(?<=.)") %>%
#   separate(S5C1_target, c("TS5C1", "TS5C2", "TS5C3", "TS5C4"), sep = "(?<=.)") %>%
#   separate(S5CF_target, c("TSFC1", "TSFC2", "TSFC3", "TSFC4"), sep = "(?<=.)")
# 
# # 6 syllables
# 
# sample_IPAtarget_6syl <- sample_IPAtarget %>% filter(nsyl_target == 6) %>% 
#   separate(Vremoved_target, c("S1C1_target", "S2C1_target", "S3C1_target", "S4C1_target", "S5C1_target", "S6C1_target", "S6CF_target"), "V") 
# 
# sample_IPAtarget_6syl <- sample_IPAtarget_6syl %>% 
#   separate(S1C1_target, c("TS1C1", "TS1C2", "TS1C3", "TS1C4"), sep = "(?<=.)") %>%
#   separate(S2C1_target, c("TS2C1", "TS2C2", "TS2C3", "TS2C4"), sep = "(?<=.)") %>%
#   separate(S3C1_target, c("TS3C1", "TS3C2", "TS3C3", "TS3C4"), sep = "(?<=.)") %>%
#   separate(S4C1_target, c("TS4C1", "TS4C2", "TS4C3", "TS4C4"), sep = "(?<=.)") %>%
#   separate(S5C1_target, c("TS5C1", "TS5C2", "TS5C3", "TS5C4"), sep = "(?<=.)") %>%
#   separate(S6C1_target, c("TS6C1", "TS6C2", "TS6C3", "TS6C4"), sep = "(?<=.)") %>%
#   separate(S6CF_target, c("TSFC1", "TSFC2", "TSFC3", "TSFC4"), sep = "(?<=.)")
# 
# 
# # Now add segmental info re infants' actual productions to each DF
# 
# Vinitial <- sample_IPAtarget %>% filter(stringr::str_detect(ActualCV, "^V")) # DF for looking at V-intial structures only
# Cinitial <- sample_IPAtarget %>% filter(stringr::str_detect(ActualCV, "^C")|stringr::str_detect(ActualCV, "^G"))    # DF for looking at C-intial structures only
# 
# # Remember to merge these subsets together once DF is organized
# 
# # Use following function to make sure all column names are included:
# 
# fncols <- function(data, cname) {
#   add <-cname[!cname%in%names(data)]
#   
#   if(length(add)!=0) data[add] <- NA
#   data
# }
# 
# # This helper function takes a list of column names and, if they aren't included in a dataframe, adds them. Use this at the end of each of the below
# # chunks to make sure that the sample_IPAactual* DFs all match up at the end when binding together
# 
# # Start with all C-initial target monosyllables (in the sample_IPAtarget_monosyl DF)
# # For each, take the infant's actual production and break it up into individual segments, as with the target forms above
# 
# sample_IPAactual_monosyl_C1 <- sample_IPAtarget_monosyl %>%         # take all actual monosyllables organized by target and split into C1 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 1) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S1CF_actual"), "V") 
# 
# sample_IPAactual_monosyl_C1 <- sample_IPAactual_monosyl_C1 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4"), sep = "(?<=.)") %>%
#   separate(S1CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4"), sep = "(?<=.)") 
#  
# sample_IPAactual_monosyl_C1 <- fncols(sample_IPAactual_monosyl_C1, c(
#                                       "AS1C1", 
#                                       "AS1C2", 
#                                       "AS1C3",
#                                       "AS1C4",
#                                       "AS1C5",
#                                       "ASFC1", 
#                                       "ASFC2", 
#                                       "ASFC3",
#                                       "ASFC4",
#                                       "ASFC5",
#                                       "AS2C1",
#                                       "AS2C2",
#                                       "AS2C3",
#                                       "AS2C4",
#                                       "AS2C5",
#                                       "AS3C1",
#                                       "AS3C2",
#                                       "AS3C3",
#                                       "AS3C4",
#                                       "AS3C5",
#                                       "AS4C1",
#                                       "AS4C2",
#                                       "AS4C3",
#                                       "AS4C4",
#                                       "AS4C5",
#                                       "AS5C1",
#                                       "AS5C2",
#                                       "AS5C3",
#                                       "AS5C4",
#                                       "AS5C5",
#                                       "AS6C1",
#                                       "AS6C2",
#                                       "AS6C3",
#                                       "AS6C4",
#                                       "AS6C5",
#                                       "TS1C1",
#                                       "TS1C2",
#                                       "TS1C3",
#                                       "TS1C4",
#                                       "TS2C1",
#                                       "TS2C2",
#                                       "TS2C3",
#                                       "TS2C4",
#                                       "TS3C1",
#                                       "TS3C2",
#                                       "TS3C3",
#                                       "TS3C4",
#                                       "TS4C1",
#                                       "TS4C2",
#                                       "TS4C3",
#                                       "TS4C4",
#                                       "TS5C1",
#                                       "TS5C2",
#                                       "TS5C3",
#                                       "TS5C4",
#                                       "TS6C1",
#                                       "TS6C2",
#                                       "TS6C3",
#                                       "TS6C4",
#                                       "TSFC1",
#                                       "TSFC2",
#                                       "TSFC3",
#                                       "TSFC4"))
# 
# # Do the same with V-initial monosyllables
# 
# sample_IPAactual_monosyl_V1 <- sample_IPAtarget_monosyl %>%         # take all V-initial actual monosyllables and split into C1 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 1) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S1CF_actual"), "V") 
# 
# sample_IPAactual_monosyl_V1 <- sample_IPAactual_monosyl_V1 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S1CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_monosyl_V1 <- fncols(sample_IPAactual_monosyl_V1, c( "AS1C1", 
#                                                                       "AS1C2", 
#                                                                       "AS1C3",
#                                                                       "AS1C4",
#                                                                       "AS1C5",
#                                                                       "ASFC1", 
#                                                                       "ASFC2", 
#                                                                       "ASFC3",
#                                                                       "ASFC4",
#                                                                       "ASFC5",
#                                                                       "AS2C1",
#                                                                       "AS2C2",
#                                                                       "AS2C3",
#                                                                       "AS2C4",
#                                                                       "AS2C5",
#                                                                       "AS3C1",
#                                                                       "AS3C2",
#                                                                       "AS3C3",
#                                                                       "AS3C4",
#                                                                       "AS3C5",
#                                                                       "AS4C1",
#                                                                       "AS4C2",
#                                                                       "AS4C3",
#                                                                       "AS4C4",
#                                                                       "AS4C5",
#                                                                       "AS5C1",
#                                                                       "AS5C2",
#                                                                       "AS5C3",
#                                                                       "AS5C4",
#                                                                       "AS5C5",
#                                                                       "AS6C1",
#                                                                       "AS6C2",
#                                                                       "AS6C3",
#                                                                       "AS6C4",
#                                                                       "AS6C5",
#                                                                       "TS1C1",
#                                                                       "TS1C2",
#                                                                       "TS1C3",
#                                                                       "TS1C4",
#                                                                       "TS2C1",
#                                                                       "TS2C2",
#                                                                       "TS2C3",
#                                                                       "TS2C4",
#                                                                       "TS3C1",
#                                                                       "TS3C2",
#                                                                       "TS3C3",
#                                                                       "TS3C4",
#                                                                       "TS4C1",
#                                                                       "TS4C2",
#                                                                       "TS4C3",
#                                                                       "TS4C4",
#                                                                       "TS5C1",
#                                                                       "TS5C2",
#                                                                       "TS5C3",
#                                                                       "TS5C4",
#                                                                       "TS6C1",
#                                                                       "TS6C2",
#                                                                       "TS6C3",
#                                                                       "TS6C4",
#                                                                       "TSFC1",
#                                                                       "TSFC2",
#                                                                       "TSFC3",
#                                                                       "TSFC4"))
# 
# # Repeat for disyllables
# 
# sample_IPAactual_monosyl_C2 <- sample_IPAtarget_monosyl %>%         # take all actual disyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 2) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S2CF_actual"), "V") 
# 
# sample_IPAactual_monosyl_C2 <- sample_IPAactual_monosyl_C2 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#     separate(S2CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_monosyl_C2 <- fncols(sample_IPAactual_monosyl_C2, c( "AS1C1", 
#                                                                       "AS1C2", 
#                                                                       "AS1C3",
#                                                                       "AS1C4",
#                                                                       "AS1C5",
#                                                                       "ASFC1", 
#                                                                       "ASFC2", 
#                                                                       "ASFC3",
#                                                                       "ASFC4",
#                                                                       "ASFC5",
#                                                                       "AS2C1",
#                                                                       "AS2C2",
#                                                                       "AS2C3",
#                                                                       "AS2C4",
#                                                                       "AS2C5",
#                                                                       "AS3C1",
#                                                                       "AS3C2",
#                                                                       "AS3C3",
#                                                                       "AS3C4",
#                                                                       "AS3C5",
#                                                                       "AS4C1",
#                                                                       "AS4C2",
#                                                                       "AS4C3",
#                                                                       "AS4C4",
#                                                                       "AS4C5",
#                                                                       "AS5C1",
#                                                                       "AS5C2",
#                                                                       "AS5C3",
#                                                                       "AS5C4",
#                                                                       "AS5C5",
#                                                                       "AS6C1",
#                                                                       "AS6C2",
#                                                                       "AS6C3",
#                                                                       "AS6C4",
#                                                                       "AS6C5",
#                                                                       "TS1C1",
#                                                                       "TS1C2",
#                                                                       "TS1C3",
#                                                                       "TS1C4",
#                                                                       "TS2C1",
#                                                                       "TS2C2",
#                                                                       "TS2C3",
#                                                                       "TS2C4",
#                                                                       "TS3C1",
#                                                                       "TS3C2",
#                                                                       "TS3C3",
#                                                                       "TS3C4",
#                                                                       "TS4C1",
#                                                                       "TS4C2",
#                                                                       "TS4C3",
#                                                                       "TS4C4",
#                                                                       "TS5C1",
#                                                                       "TS5C2",
#                                                                       "TS5C3",
#                                                                       "TS5C4",
#                                                                       "TS6C1",
#                                                                       "TS6C2",
#                                                                       "TS6C3",
#                                                                       "TS6C4",
#                                                                       "TSFC1",
#                                                                       "TSFC2",
#                                                                       "TSFC3",
#                                                                       "TSFC4"))
# 
# 
# sample_IPAactual_monosyl_V2 <- sample_IPAtarget_monosyl %>%         # take all V-initial actual disyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 2) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S2CF_actual"), "V") 
# 
# sample_IPAactual_monosyl_V2 <- sample_IPAactual_monosyl_V2 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#     separate(S2CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_monosyl_V2 <- fncols(sample_IPAactual_monosyl_V2, c( "AS1C1", 
#                                                                       "AS1C2", 
#                                                                       "AS1C3",
#                                                                       "AS1C4",
#                                                                       "AS1C5",
#                                                                       "ASFC1", 
#                                                                       "ASFC2", 
#                                                                       "ASFC3",
#                                                                       "ASFC4",
#                                                                       "ASFC5",
#                                                                       "AS2C1",
#                                                                       "AS2C2",
#                                                                       "AS2C3",
#                                                                       "AS2C4",
#                                                                       "AS2C5",
#                                                                       "AS3C1",
#                                                                       "AS3C2",
#                                                                       "AS3C3",
#                                                                       "AS3C4",
#                                                                       "AS3C5",
#                                                                       "AS4C1",
#                                                                       "AS4C2",
#                                                                       "AS4C3",
#                                                                       "AS4C4",
#                                                                       "AS4C5",
#                                                                       "AS5C1",
#                                                                       "AS5C2",
#                                                                       "AS5C3",
#                                                                       "AS5C4",
#                                                                       "AS5C5",
#                                                                       "AS6C1",
#                                                                       "AS6C2",
#                                                                       "AS6C3",
#                                                                       "AS6C4",
#                                                                       "AS6C5",
#                                                                       "TS1C1",
#                                                                       "TS1C2",
#                                                                       "TS1C3",
#                                                                       "TS1C4",
#                                                                       "TS2C1",
#                                                                       "TS2C2",
#                                                                       "TS2C3",
#                                                                       "TS2C4",
#                                                                       "TS3C1",
#                                                                       "TS3C2",
#                                                                       "TS3C3",
#                                                                       "TS3C4",
#                                                                       "TS4C1",
#                                                                       "TS4C2",
#                                                                       "TS4C3",
#                                                                       "TS4C4",
#                                                                       "TS5C1",
#                                                                       "TS5C2",
#                                                                       "TS5C3",
#                                                                       "TS5C4",
#                                                                       "TS6C1",
#                                                                       "TS6C2",
#                                                                       "TS6C3",
#                                                                       "TS6C4",
#                                                                       "TSFC1",
#                                                                       "TSFC2",
#                                                                       "TSFC3",
#                                                                       "TSFC4"))
# 
# # Trisyllables
# 
# sample_IPAactual_monosyl_C3 <- sample_IPAtarget_monosyl %>%         # take all actual trisyllables and split into C1, C2, C3 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 3) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S3CF_actual"), "V") 
# 
# sample_IPAactual_monosyl_C3 <- sample_IPAactual_monosyl_C3 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S3CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_monosyl_C3 <- fncols(sample_IPAactual_monosyl_C3, c( "AS1C1", 
#                                                                       "AS1C2", 
#                                                                       "AS1C3",
#                                                                       "AS1C4",
#                                                                       "AS1C5",
#                                                                       "ASFC1", 
#                                                                       "ASFC2", 
#                                                                       "ASFC3",
#                                                                       "ASFC4",
#                                                                       "ASFC5",
#                                                                       "AS2C1",
#                                                                       "AS2C2",
#                                                                       "AS2C3",
#                                                                       "AS2C4",
#                                                                       "AS2C5",
#                                                                       "AS3C1",
#                                                                       "AS3C2",
#                                                                       "AS3C3",
#                                                                       "AS3C4",
#                                                                       "AS3C5",
#                                                                       "AS4C1",
#                                                                       "AS4C2",
#                                                                       "AS4C3",
#                                                                       "AS4C4",
#                                                                       "AS4C5",
#                                                                       "AS5C1",
#                                                                       "AS5C2",
#                                                                       "AS5C3",
#                                                                       "AS5C4",
#                                                                       "AS5C5",
#                                                                       "AS6C1",
#                                                                       "AS6C2",
#                                                                       "AS6C3",
#                                                                       "AS6C4",
#                                                                       "AS6C5",
#                                                                       "TS1C1",
#                                                                       "TS1C2",
#                                                                       "TS1C3",
#                                                                       "TS1C4",
#                                                                       "TS2C1",
#                                                                       "TS2C2",
#                                                                       "TS2C3",
#                                                                       "TS2C4",
#                                                                       "TS3C1",
#                                                                       "TS3C2",
#                                                                       "TS3C3",
#                                                                       "TS3C4",
#                                                                       "TS4C1",
#                                                                       "TS4C2",
#                                                                       "TS4C3",
#                                                                       "TS4C4",
#                                                                       "TS5C1",
#                                                                       "TS5C2",
#                                                                       "TS5C3",
#                                                                       "TS5C4",
#                                                                       "TS6C1",
#                                                                       "TS6C2",
#                                                                       "TS6C3",
#                                                                       "TS6C4",
#                                                                       "TSFC1",
#                                                                       "TSFC2",
#                                                                       "TSFC3",
#                                                                       "TSFC4"))
# 
# sample_IPAactual_monosyl_V3 <- sample_IPAtarget_monosyl %>%         # take all V-initial actual trisyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 3) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S3CF_actual"), "V") 
# 
# sample_IPAactual_monosyl_V3 <- sample_IPAactual_monosyl_V3 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S3CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_monosyl_V3 <- fncols(sample_IPAactual_monosyl_V3, c( "AS1C1", 
#                                                                       "AS1C2", 
#                                                                       "AS1C3",
#                                                                       "AS1C4",
#                                                                       "AS1C5",
#                                                                       "ASFC1", 
#                                                                       "ASFC2", 
#                                                                       "ASFC3",
#                                                                       "ASFC4",
#                                                                       "ASFC5",
#                                                                       "AS2C1",
#                                                                       "AS2C2",
#                                                                       "AS2C3",
#                                                                       "AS2C4",
#                                                                       "AS2C5",
#                                                                       "AS3C1",
#                                                                       "AS3C2",
#                                                                       "AS3C3",
#                                                                       "AS3C4",
#                                                                       "AS3C5",
#                                                                       "AS4C1",
#                                                                       "AS4C2",
#                                                                       "AS4C3",
#                                                                       "AS4C4",
#                                                                       "AS4C5",
#                                                                       "AS5C1",
#                                                                       "AS5C2",
#                                                                       "AS5C3",
#                                                                       "AS5C4",
#                                                                       "AS5C5",
#                                                                       "AS6C1",
#                                                                       "AS6C2",
#                                                                       "AS6C3",
#                                                                       "AS6C4",
#                                                                       "AS6C5",
#                                                                       "TS1C1",
#                                                                       "TS1C2",
#                                                                       "TS1C3",
#                                                                       "TS1C4",
#                                                                       "TS2C1",
#                                                                       "TS2C2",
#                                                                       "TS2C3",
#                                                                       "TS2C4",
#                                                                       "TS3C1",
#                                                                       "TS3C2",
#                                                                       "TS3C3",
#                                                                       "TS3C4",
#                                                                       "TS4C1",
#                                                                       "TS4C2",
#                                                                       "TS4C3",
#                                                                       "TS4C4",
#                                                                       "TS5C1",
#                                                                       "TS5C2",
#                                                                       "TS5C3",
#                                                                       "TS5C4",
#                                                                       "TS6C1",
#                                                                       "TS6C2",
#                                                                       "TS6C3",
#                                                                       "TS6C4",
#                                                                       "TSFC1",
#                                                                       "TSFC2",
#                                                                       "TSFC3",
#                                                                       "TSFC4"))
# 
# # 4 syllables
# 
# sample_IPAactual_monosyl_C4 <- sample_IPAtarget_monosyl %>%         
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 4) %>%       # extract 4syl word and split into C1, C2, C3, C4 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S4CF_actual"), "V") 
# 
# sample_IPAactual_monosyl_C4 <- sample_IPAactual_monosyl_C4 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#     separate(S4CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_monosyl_C4 <- fncols(sample_IPAactual_monosyl_C4, c( "AS1C1", 
#                                                                       "AS1C2", 
#                                                                       "AS1C3",
#                                                                       "AS1C4",
#                                                                       "AS1C5",
#                                                                       "ASFC1", 
#                                                                       "ASFC2", 
#                                                                       "ASFC3",
#                                                                       "ASFC4",
#                                                                       "ASFC5",
#                                                                       "AS2C1",
#                                                                       "AS2C2",
#                                                                       "AS2C3",
#                                                                       "AS2C4",
#                                                                       "AS2C5",
#                                                                       "AS3C1",
#                                                                       "AS3C2",
#                                                                       "AS3C3",
#                                                                       "AS3C4",
#                                                                       "AS3C5",
#                                                                       "AS4C1",
#                                                                       "AS4C2",
#                                                                       "AS4C3",
#                                                                       "AS4C4",
#                                                                       "AS4C5",
#                                                                       "AS5C1",
#                                                                       "AS5C2",
#                                                                       "AS5C3",
#                                                                       "AS5C4",
#                                                                       "AS5C5",
#                                                                       "AS6C1",
#                                                                       "AS6C2",
#                                                                       "AS6C3",
#                                                                       "AS6C4",
#                                                                       "AS6C5",
#                                                                       "TS1C1",
#                                                                       "TS1C2",
#                                                                       "TS1C3",
#                                                                       "TS1C4",
#                                                                       "TS2C1",
#                                                                       "TS2C2",
#                                                                       "TS2C3",
#                                                                       "TS2C4",
#                                                                       "TS3C1",
#                                                                       "TS3C2",
#                                                                       "TS3C3",
#                                                                       "TS3C4",
#                                                                       "TS4C1",
#                                                                       "TS4C2",
#                                                                       "TS4C3",
#                                                                       "TS4C4",
#                                                                       "TS5C1",
#                                                                       "TS5C2",
#                                                                       "TS5C3",
#                                                                       "TS5C4",
#                                                                       "TS6C1",
#                                                                       "TS6C2",
#                                                                       "TS6C3",
#                                                                       "TS6C4",
#                                                                       "TSFC1",
#                                                                       "TSFC2",
#                                                                       "TSFC3",
#                                                                       "TSFC4"))
# 
# sample_IPAactual_monosyl_V4 <- sample_IPAtarget_monosyl %>%         
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 4) %>%       # take all V-initial actual 4syls and split into C1, C2 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S4CF_actual"), "V") 
# 
# sample_IPAactual_monosyl_V4 <- sample_IPAactual_monosyl_V4 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S4CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_monosyl_V4 <- fncols(sample_IPAactual_monosyl_V4, c( "AS1C1", 
#                                                                       "AS1C2", 
#                                                                       "AS1C3",
#                                                                       "AS1C4",
#                                                                       "AS1C5",
#                                                                       "ASFC1", 
#                                                                       "ASFC2", 
#                                                                       "ASFC3",
#                                                                       "ASFC4",
#                                                                       "ASFC5",
#                                                                       "AS2C1",
#                                                                       "AS2C2",
#                                                                       "AS2C3",
#                                                                       "AS2C4",
#                                                                       "AS2C5",
#                                                                       "AS3C1",
#                                                                       "AS3C2",
#                                                                       "AS3C3",
#                                                                       "AS3C4",
#                                                                       "AS3C5",
#                                                                       "AS4C1",
#                                                                       "AS4C2",
#                                                                       "AS4C3",
#                                                                       "AS4C4",
#                                                                       "AS4C5",
#                                                                       "AS5C1",
#                                                                       "AS5C2",
#                                                                       "AS5C3",
#                                                                       "AS5C4",
#                                                                       "AS5C5",
#                                                                       "AS6C1",
#                                                                       "AS6C2",
#                                                                       "AS6C3",
#                                                                       "AS6C4",
#                                                                       "AS6C5",
#                                                                       "TS1C1",
#                                                                       "TS1C2",
#                                                                       "TS1C3",
#                                                                       "TS1C4",
#                                                                       "TS2C1",
#                                                                       "TS2C2",
#                                                                       "TS2C3",
#                                                                       "TS2C4",
#                                                                       "TS3C1",
#                                                                       "TS3C2",
#                                                                       "TS3C3",
#                                                                       "TS3C4",
#                                                                       "TS4C1",
#                                                                       "TS4C2",
#                                                                       "TS4C3",
#                                                                       "TS4C4",
#                                                                       "TS5C1",
#                                                                       "TS5C2",
#                                                                       "TS5C3",
#                                                                       "TS5C4",
#                                                                       "TS6C1",
#                                                                       "TS6C2",
#                                                                       "TS6C3",
#                                                                       "TS6C4",
#                                                                       "TSFC1",
#                                                                       "TSFC2",
#                                                                       "TSFC3",
#                                                                       "TSFC4"))
# 
# # 5 syllables
# 
# sample_IPAactual_monosyl_C5 <- sample_IPAtarget_monosyl %>%         
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 5) %>%       # extract 5syl word and split into C1, C2, C3, C4, C5 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V") 
# 
# sample_IPAactual_monosyl_C5 <- sample_IPAactual_monosyl_C5 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_monosyl_C5 <- fncols(sample_IPAactual_monosyl_C5, c( "AS1C1", 
#                                                                       "AS1C2", 
#                                                                       "AS1C3",
#                                                                       "AS1C4",
#                                                                       "AS1C5",
#                                                                       "ASFC1", 
#                                                                       "ASFC2", 
#                                                                       "ASFC3",
#                                                                       "ASFC4",
#                                                                       "ASFC5",
#                                                                       "AS2C1",
#                                                                       "AS2C2",
#                                                                       "AS2C3",
#                                                                       "AS2C4",
#                                                                       "AS2C5",
#                                                                       "AS3C1",
#                                                                       "AS3C2",
#                                                                       "AS3C3",
#                                                                       "AS3C4",
#                                                                       "AS3C5",
#                                                                       "AS4C1",
#                                                                       "AS4C2",
#                                                                       "AS4C3",
#                                                                       "AS4C4",
#                                                                       "AS4C5",
#                                                                       "AS5C1",
#                                                                       "AS5C2",
#                                                                       "AS5C3",
#                                                                       "AS5C4",
#                                                                       "AS5C5",
#                                                                       "AS6C1",
#                                                                       "AS6C2",
#                                                                       "AS6C3",
#                                                                       "AS6C4",
#                                                                       "AS6C5",
#                                                                       "TS1C1",
#                                                                       "TS1C2",
#                                                                       "TS1C3",
#                                                                       "TS1C4",
#                                                                       "TS2C1",
#                                                                       "TS2C2",
#                                                                       "TS2C3",
#                                                                       "TS2C4",
#                                                                       "TS3C1",
#                                                                       "TS3C2",
#                                                                       "TS3C3",
#                                                                       "TS3C4",
#                                                                       "TS4C1",
#                                                                       "TS4C2",
#                                                                       "TS4C3",
#                                                                       "TS4C4",
#                                                                       "TS5C1",
#                                                                       "TS5C2",
#                                                                       "TS5C3",
#                                                                       "TS5C4",
#                                                                       "TS6C1",
#                                                                       "TS6C2",
#                                                                       "TS6C3",
#                                                                       "TS6C4",
#                                                                       "TSFC1",
#                                                                       "TSFC2",
#                                                                       "TSFC3",
#                                                                       "TSFC4"))
# 
# sample_IPAactual_monosyl_V5 <- sample_IPAtarget_monosyl %>%         
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 5) %>%       # extract 5syl word and split into C1, C2, C3, C4, C5 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V") 
# 
# sample_IPAactual_monosyl_V5 <- sample_IPAactual_monosyl_V5 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_monosyl_V5 <- fncols(sample_IPAactual_monosyl_V5, c( "AS1C1", 
#                                                                       "AS1C2", 
#                                                                       "AS1C3",
#                                                                       "AS1C4",
#                                                                       "AS1C5",
#                                                                       "ASFC1", 
#                                                                       "ASFC2", 
#                                                                       "ASFC3",
#                                                                       "ASFC4",
#                                                                       "ASFC5",
#                                                                       "AS2C1",
#                                                                       "AS2C2",
#                                                                       "AS2C3",
#                                                                       "AS2C4",
#                                                                       "AS2C5",
#                                                                       "AS3C1",
#                                                                       "AS3C2",
#                                                                       "AS3C3",
#                                                                       "AS3C4",
#                                                                       "AS3C5",
#                                                                       "AS4C1",
#                                                                       "AS4C2",
#                                                                       "AS4C3",
#                                                                       "AS4C4",
#                                                                       "AS4C5",
#                                                                       "AS5C1",
#                                                                       "AS5C2",
#                                                                       "AS5C3",
#                                                                       "AS5C4",
#                                                                       "AS5C5",
#                                                                       "AS6C1",
#                                                                       "AS6C2",
#                                                                       "AS6C3",
#                                                                       "AS6C4",
#                                                                       "AS6C5",
#                                                                       "TS1C1",
#                                                                       "TS1C2",
#                                                                       "TS1C3",
#                                                                       "TS1C4",
#                                                                       "TS2C1",
#                                                                       "TS2C2",
#                                                                       "TS2C3",
#                                                                       "TS2C4",
#                                                                       "TS3C1",
#                                                                       "TS3C2",
#                                                                       "TS3C3",
#                                                                       "TS3C4",
#                                                                       "TS4C1",
#                                                                       "TS4C2",
#                                                                       "TS4C3",
#                                                                       "TS4C4",
#                                                                       "TS5C1",
#                                                                       "TS5C2",
#                                                                       "TS5C3",
#                                                                       "TS5C4",
#                                                                       "TS6C1",
#                                                                       "TS6C2",
#                                                                       "TS6C3",
#                                                                       "TS6C4",
#                                                                       "TSFC1",
#                                                                       "TSFC2",
#                                                                       "TSFC3",
#                                                                       "TSFC4"))
# 
# # 6 syllables
# 
# sample_IPAactual_monosyl_C6 <- sample_IPAtarget_monosyl %>%         
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 6) %>%       # extract 5syl word and split into C1, C2, C3, C4, C5 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S6C1_actual", "S6CF_actual"), "V") 
# 
# sample_IPAactual_monosyl_C6 <- sample_IPAactual_monosyl_C6 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S6C1_actual, c("AS6C1", "AS6C2", "AS6C3", "AS6C4", "AS6C5"), sep = "(?<=.)") %>%
#   separate(S6CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_monosyl_C6 <- fncols(sample_IPAactual_monosyl_C6, c( "AS1C1", 
#                                                                       "AS1C2", 
#                                                                       "AS1C3",
#                                                                       "AS1C4",
#                                                                       "AS1C5",
#                                                                       "ASFC1", 
#                                                                       "ASFC2", 
#                                                                       "ASFC3",
#                                                                       "ASFC4",
#                                                                       "ASFC5",
#                                                                       "AS2C1",
#                                                                       "AS2C2",
#                                                                       "AS2C3",
#                                                                       "AS2C4",
#                                                                       "AS2C5",
#                                                                       "AS3C1",
#                                                                       "AS3C2",
#                                                                       "AS3C3",
#                                                                       "AS3C4",
#                                                                       "AS3C5",
#                                                                       "AS4C1",
#                                                                       "AS4C2",
#                                                                       "AS4C3",
#                                                                       "AS4C4",
#                                                                       "AS4C5",
#                                                                       "AS5C1",
#                                                                       "AS5C2",
#                                                                       "AS5C3",
#                                                                       "AS5C4",
#                                                                       "AS5C5",
#                                                                       "AS6C1",
#                                                                       "AS6C2",
#                                                                       "AS6C3",
#                                                                       "AS6C4",
#                                                                       "AS6C5",
#                                                                       "TS1C1",
#                                                                       "TS1C2",
#                                                                       "TS1C3",
#                                                                       "TS1C4",
#                                                                       "TS2C1",
#                                                                       "TS2C2",
#                                                                       "TS2C3",
#                                                                       "TS2C4",
#                                                                       "TS3C1",
#                                                                       "TS3C2",
#                                                                       "TS3C3",
#                                                                       "TS3C4",
#                                                                       "TS4C1",
#                                                                       "TS4C2",
#                                                                       "TS4C3",
#                                                                       "TS4C4",
#                                                                       "TS5C1",
#                                                                       "TS5C2",
#                                                                       "TS5C3",
#                                                                       "TS5C4",
#                                                                       "TS6C1",
#                                                                       "TS6C2",
#                                                                       "TS6C3",
#                                                                       "TS6C4",
#                                                                       "TSFC1",
#                                                                       "TSFC2",
#                                                                       "TSFC3",
#                                                                       "TSFC4"))
# 
# sample_IPAactual_monosyl_V6 <- sample_IPAtarget_monosyl %>%         
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 6) %>%       # extract 5syl word and split into C1, C2, C3, C4, C5 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S6C1_actual", "S6CF_actual"), "V") 
# 
# sample_IPAactual_monosyl_V6 <- sample_IPAactual_monosyl_V6 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S6C1_actual, c("AS6C1", "AS6C2", "AS6C3", "AS6C4", "AS6C5"), sep = "(?<=.)") %>%
#   separate(S6CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_monosyl_V6 <- fncols(sample_IPAactual_monosyl_V6, c("AS1C1", 
#                                                                       "AS1C2", 
#                                                                       "AS1C3",
#                                                                       "AS1C4",
#                                                                       "AS1C5",
#                                                                       "ASFC1", 
#                                                                       "ASFC2", 
#                                                                       "ASFC3",
#                                                                       "ASFC4",
#                                                                       "ASFC5",
#                                                                       "AS2C1",
#                                                                       "AS2C2",
#                                                                       "AS2C3",
#                                                                       "AS2C4",
#                                                                       "AS2C5",
#                                                                       "AS3C1",
#                                                                       "AS3C2",
#                                                                       "AS3C3",
#                                                                       "AS3C4",
#                                                                       "AS3C5",
#                                                                       "AS4C1",
#                                                                       "AS4C2",
#                                                                       "AS4C3",
#                                                                       "AS4C4",
#                                                                       "AS4C5",
#                                                                       "AS5C1",
#                                                                       "AS5C2",
#                                                                       "AS5C3",
#                                                                       "AS5C4",
#                                                                       "AS5C5",
#                                                                       "AS6C1",
#                                                                       "AS6C2",
#                                                                       "AS6C3",
#                                                                       "AS6C4",
#                                                                       "AS6C5",
#                                                                       "TS1C1",
#                                                                       "TS1C2",
#                                                                       "TS1C3",
#                                                                       "TS1C4",
#                                                                       "TS2C1",
#                                                                       "TS2C2",
#                                                                       "TS2C3",
#                                                                       "TS2C4",
#                                                                       "TS3C1",
#                                                                       "TS3C2",
#                                                                       "TS3C3",
#                                                                       "TS3C4",
#                                                                       "TS4C1",
#                                                                       "TS4C2",
#                                                                       "TS4C3",
#                                                                       "TS4C4",
#                                                                       "TS5C1",
#                                                                       "TS5C2",
#                                                                       "TS5C3",
#                                                                       "TS5C4",
#                                                                       "TS6C1",
#                                                                       "TS6C2",
#                                                                       "TS6C3",
#                                                                       "TS6C4",
#                                                                       "TSFC1",
#                                                                       "TSFC2",
#                                                                       "TSFC3",
#                                                                       "TSFC4"))
# 
# 
# # Join into single DF
# 
# actual_IPAtarget_monosyl <- rbind(sample_IPAactual_monosyl_C1,
#                                   sample_IPAactual_monosyl_V1,
#                                   sample_IPAactual_monosyl_C2, 
#                                   sample_IPAactual_monosyl_V2, 
#                                   sample_IPAactual_monosyl_C3, 
#                                   sample_IPAactual_monosyl_V3, 
#                                   sample_IPAactual_monosyl_C4,
#                                   sample_IPAactual_monosyl_V4,
#                                   sample_IPAactual_monosyl_C5, 
#                                   sample_IPAactual_monosyl_V5, 
#                                   sample_IPAactual_monosyl_C6, 
#                                   sample_IPAactual_monosyl_V6)
# # Now do target disyllables
# 
# sample_IPAactual_disyl_C1 <- sample_IPAtarget_disyl %>%         # take all actual disyllables and split into C1 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 1) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S1CF_actual"), "V") 
# 
# sample_IPAactual_disyl_C1 <- sample_IPAactual_disyl_C1 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S1CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_disyl_C1 <- fncols(sample_IPAactual_disyl_C1, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# 
# sample_IPAactual_disyl_V1 <- sample_IPAtarget_disyl %>%         # take all V-initial actual disyllables and split into C1 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 1) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S1CF_actual"), "V") 
# 
# sample_IPAactual_disyl_V1 <- sample_IPAactual_disyl_V1 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S1CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_disyl_V1 <- fncols(sample_IPAactual_disyl_V1, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# 
# sample_IPAactual_disyl_C2 <- sample_IPAtarget_disyl %>%         # take all actual disyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 2) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S2CF_actual"), "V") 
# 
# sample_IPAactual_disyl_C2 <- sample_IPAactual_disyl_C2 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_disyl_C2 <- fncols(sample_IPAactual_disyl_C2, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# 
# sample_IPAactual_disyl_V2 <- sample_IPAtarget_disyl %>%         # take all V-initial actual disyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 2) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S2CF_actual"), "V") 
# 
# sample_IPAactual_disyl_V2 <- sample_IPAactual_disyl_V2 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S2CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")  
# 
# sample_IPAactual_disyl_V2 <- fncols(sample_IPAactual_disyl_V2, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# sample_IPAactual_disyl_C3 <- sample_IPAtarget_disyl %>%         # take all actual trisyllables and split into C1, C2, C3 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 3) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S3CF_actual"), "V") 
# 
# sample_IPAactual_disyl_C3 <- sample_IPAactual_disyl_C3 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S3CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_disyl_C3 <- fncols(sample_IPAactual_disyl_C3, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# 
# sample_IPAactual_disyl_V3 <- sample_IPAtarget_disyl %>%         # take all V-initial actual trisyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 3) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S3CF_actual"), "V") 
# 
# sample_IPAactual_disyl_V3 <- sample_IPAactual_disyl_V3 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S3CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")  
# 
# sample_IPAactual_disyl_V3 <- fncols(sample_IPAactual_disyl_V3, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# sample_IPAactual_disyl_C4 <- sample_IPAtarget_disyl %>%        # there are 4 instances of words with 4+ syllables; 3 are onomatopoeia/word play   
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 4) %>%       # extract 4syl word and split into C1, C2, C3, C4 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S4CF_actual"), "V") 
# 
# sample_IPAactual_disyl_C4 <- sample_IPAactual_disyl_C4 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S4CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_disyl_C4 <- fncols(sample_IPAactual_disyl_C4, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# sample_IPAactual_disyl_V4 <- sample_IPAtarget_disyl %>%        # there are 4 instances of words with 4+ syllables; 3 are onomatopoeia/word play   
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 4) %>%       # extract 4syl word and split into C1, C2, C3, C4 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S4CF_actual"), "V") 
# 
# sample_IPAactual_disyl_V4 <- sample_IPAactual_disyl_V4 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S4CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_disyl_V4 <- fncols(sample_IPAactual_disyl_V4, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# 
# 
# sample_IPAactual_disyl_C5 <- sample_IPAtarget_disyl %>%        
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 5) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V") 
# 
# sample_IPAactual_disyl_C5 <- sample_IPAactual_disyl_C5 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_disyl_C5 <- fncols(sample_IPAactual_disyl_C5, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# sample_IPAactual_disyl_V5 <- sample_IPAtarget_disyl %>%        
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 5) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V") 
# 
# sample_IPAactual_disyl_V5 <- sample_IPAactual_disyl_V5 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_disyl_V5 <- fncols(sample_IPAactual_disyl_V5, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# sample_IPAactual_disyl_C6 <- sample_IPAtarget_disyl %>%        
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 6) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S6C1_actual", "S6CF_actual"), "V") 
# 
# sample_IPAactual_disyl_C6 <- sample_IPAactual_disyl_C6 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S6C1_actual, c("AS6C1", "AS6C2", "AS6C3", "AS6C4", "AS6C5"), sep = "(?<=.)") %>%
#   separate(S6CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_disyl_C6 <- fncols(sample_IPAactual_disyl_C6, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# sample_IPAactual_disyl_V6 <- sample_IPAtarget_disyl %>%        
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 6) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S6C1_actual", "S6CF_actual"), "V") 
# 
# sample_IPAactual_disyl_V6 <- sample_IPAactual_disyl_V6 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S6C1_actual, c("AS6C1", "AS6C2", "AS6C3", "AS6C4", "AS6C5"), sep = "(?<=.)") %>%
#   separate(S6CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_disyl_V6 <- fncols(sample_IPAactual_disyl_V6, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# # Join into single DF
# 
# actual_IPAtarget_disyl <- rbind(sample_IPAactual_disyl_C1,
#                                   sample_IPAactual_disyl_V1,
#                                   sample_IPAactual_disyl_C2, 
#                                   sample_IPAactual_disyl_V2, 
#                                   sample_IPAactual_disyl_C3, 
#                                   sample_IPAactual_disyl_V3, 
#                                   sample_IPAactual_disyl_C4,
#                                   sample_IPAactual_disyl_V4, 
#                                   sample_IPAactual_disyl_C5, 
#                                   sample_IPAactual_disyl_V5, 
#                                   sample_IPAactual_disyl_C6,
#                                   sample_IPAactual_disyl_V6)
# 
# # Now do target trisyllables
# 
# sample_IPAactual_trisyl_C1 <- sample_IPAtarget_trisyl %>%         # take all actual disyllables and split into C1 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 1) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S1CF_actual"), "V") 
# 
# sample_IPAactual_trisyl_C1 <- sample_IPAactual_trisyl_C1 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S1CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_trisyl_C1 <- fncols(sample_IPAactual_trisyl_C1, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# 
# sample_IPAactual_trisyl_V1 <- sample_IPAtarget_trisyl %>%         # take all V-initial actual disyllables and split into C1 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 1) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S1CF_actual"), "V") 
# 
# sample_IPAactual_trisyl_V1 <- sample_IPAactual_trisyl_V1 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S1CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_trisyl_V1 <- fncols(sample_IPAactual_trisyl_V1, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# 
# sample_IPAactual_trisyl_C2 <- sample_IPAtarget_trisyl %>%         # take all actual disyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 2) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S2CF_actual"), "V") 
# 
# sample_IPAactual_trisyl_C2 <- sample_IPAactual_trisyl_C2 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_trisyl_C2 <- fncols(sample_IPAactual_trisyl_C2, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# 
# sample_IPAactual_trisyl_V2 <- sample_IPAtarget_trisyl %>%         # take all V-initial actual disyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 2) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S2CF_actual"), "V") 
# 
# sample_IPAactual_trisyl_V2 <- sample_IPAactual_trisyl_V2 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S2CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")  
# 
# sample_IPAactual_trisyl_V2 <- fncols(sample_IPAactual_trisyl_V2, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# sample_IPAactual_trisyl_C3 <- sample_IPAtarget_trisyl %>%         # take all actual trisyllables and split into C1, C2, C3 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 3) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S3CF_actual"), "V") 
# 
# sample_IPAactual_trisyl_C3 <- sample_IPAactual_trisyl_C3 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S3CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_trisyl_C3 <- fncols(sample_IPAactual_trisyl_C3, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# 
# sample_IPAactual_trisyl_V3 <- sample_IPAtarget_trisyl %>%         # take all V-initial actual trisyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 3) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S3CF_actual"), "V") 
# 
# sample_IPAactual_trisyl_V3 <- sample_IPAactual_trisyl_V3 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S3CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")  
# 
# sample_IPAactual_trisyl_V3 <- fncols(sample_IPAactual_trisyl_V3, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# sample_IPAactual_trisyl_C4 <- sample_IPAtarget_trisyl %>%        # there are 4 instances of words with 4+ syllables; 3 are onomatopoeia/word play   
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 4) %>%       # extract 4syl word and split into C1, C2, C3, C4 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S4CF_actual"), "V") 
# 
# sample_IPAactual_trisyl_C4 <- sample_IPAactual_trisyl_C4 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S4CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_trisyl_C4 <- fncols(sample_IPAactual_trisyl_C4, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# sample_IPAactual_trisyl_V4 <- sample_IPAtarget_trisyl %>%        # there are 4 instances of words with 4+ syllables; 3 are onomatopoeia/word play   
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 4) %>%       # extract 4syl word and split into C1, C2, C3, C4 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S4CF_actual"), "V") 
# 
# sample_IPAactual_trisyl_V4 <- sample_IPAactual_trisyl_V4 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S4CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_trisyl_V4 <- fncols(sample_IPAactual_trisyl_V4, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# 
# 
# sample_IPAactual_trisyl_C5 <- sample_IPAtarget_trisyl %>%        
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 5) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V") 
# 
# sample_IPAactual_trisyl_C5 <- sample_IPAactual_trisyl_C5 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_trisyl_C5 <- fncols(sample_IPAactual_trisyl_C5, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# sample_IPAactual_trisyl_V5 <- sample_IPAtarget_trisyl %>%        
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 5) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V") 
# 
# sample_IPAactual_trisyl_V5 <- sample_IPAactual_trisyl_V5 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_trisyl_V5 <- fncols(sample_IPAactual_trisyl_V5, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# sample_IPAactual_trisyl_C6 <- sample_IPAtarget_trisyl %>%        
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 6) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S6C1_actual", "S6CF_actual"), "V") 
# 
# sample_IPAactual_trisyl_C6 <- sample_IPAactual_trisyl_C6 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S6C1_actual, c("AS6C1", "AS6C2", "AS6C3", "AS6C4", "AS6C5"), sep = "(?<=.)") %>%
#   separate(S6CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_trisyl_C6 <- fncols(sample_IPAactual_trisyl_C6, c("AS1C1", 
#                                                                  "AS1C2", 
#                                                                  "AS1C3",
#                                                                  "AS1C4",
#                                                                  "AS1C5",
#                                                                  "ASFC1", 
#                                                                  "ASFC2", 
#                                                                  "ASFC3",
#                                                                  "ASFC4",
#                                                                  "ASFC5",
#                                                                  "AS2C1",
#                                                                  "AS2C2",
#                                                                  "AS2C3",
#                                                                  "AS2C4",
#                                                                  "AS2C5",
#                                                                  "AS3C1",
#                                                                  "AS3C2",
#                                                                  "AS3C3",
#                                                                  "AS3C4",
#                                                                  "AS3C5",
#                                                                  "AS4C1",
#                                                                  "AS4C2",
#                                                                  "AS4C3",
#                                                                  "AS4C4",
#                                                                  "AS4C5",
#                                                                  "AS5C1",
#                                                                  "AS5C2",
#                                                                  "AS5C3",
#                                                                  "AS5C4",
#                                                                  "AS5C5",
#                                                                  "AS6C1",
#                                                                  "AS6C2",
#                                                                  "AS6C3",
#                                                                  "AS6C4",
#                                                                  "AS6C5",
#                                                                  "TS1C1",
#                                                                  "TS1C2",
#                                                                  "TS1C3",
#                                                                  "TS1C4",
#                                                                  "TS2C1",
#                                                                  "TS2C2",
#                                                                  "TS2C3",
#                                                                  "TS2C4",
#                                                                  "TS3C1",
#                                                                  "TS3C2",
#                                                                  "TS3C3",
#                                                                  "TS3C4",
#                                                                  "TS4C1",
#                                                                  "TS4C2",
#                                                                  "TS4C3",
#                                                                  "TS4C4",
#                                                                  "TS5C1",
#                                                                  "TS5C2",
#                                                                  "TS5C3",
#                                                                  "TS5C4",
#                                                                  "TS6C1",
#                                                                  "TS6C2",
#                                                                  "TS6C3",
#                                                                  "TS6C4",
#                                                                  "TSFC1",
#                                                                  "TSFC2",
#                                                                  "TSFC3",
#                                                                  "TSFC4"))
# 
# # No V-initial 6-syll productions of trisyllabic targets in data so omit this from script
# 
# # Join into single DF
# 
# actual_IPAtarget_trisyl <- rbind(sample_IPAactual_trisyl_C1,
#                                 sample_IPAactual_trisyl_V1,
#                                 sample_IPAactual_trisyl_C2, 
#                                 sample_IPAactual_trisyl_V2, 
#                                 sample_IPAactual_trisyl_C3, 
#                                 sample_IPAactual_trisyl_V3, 
#                                 sample_IPAactual_trisyl_C4,
#                                 sample_IPAactual_trisyl_V4, 
#                                 sample_IPAactual_trisyl_C5, 
#                                 sample_IPAactual_trisyl_V5, 
#                                 sample_IPAactual_trisyl_C6)
# 
# # Now do target 4-syl words
# 
# sample_IPAactual_4syl_C1 <- sample_IPAtarget_4syl %>%         # take all actual disyllables and split into C1 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 1) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S1CF_actual"), "V") 
# 
# sample_IPAactual_4syl_C1 <- sample_IPAactual_4syl_C1 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S1CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_4syl_C1 <- fncols(sample_IPAactual_4syl_C1, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# 
# sample_IPAactual_4syl_V1 <- sample_IPAtarget_4syl %>%         # take all V-initial actual disyllables and split into C1 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 1) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S1CF_actual"), "V") 
# 
# sample_IPAactual_4syl_V1 <- sample_IPAactual_4syl_V1 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S1CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_4syl_V1 <- fncols(sample_IPAactual_4syl_V1, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# 
# sample_IPAactual_4syl_C2 <- sample_IPAtarget_4syl %>%         # take all actual disyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 2) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S2CF_actual"), "V") 
# 
# sample_IPAactual_4syl_C2 <- sample_IPAactual_4syl_C2 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_4syl_C2 <- fncols(sample_IPAactual_4syl_C2, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# 
# sample_IPAactual_4syl_V2 <- sample_IPAtarget_4syl %>%         # take all V-initial actual disyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 2) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S2CF_actual"), "V") 
# 
# sample_IPAactual_4syl_V2 <- sample_IPAactual_4syl_V2 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S2CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")  
# 
# sample_IPAactual_4syl_V2 <- fncols(sample_IPAactual_4syl_V2, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# sample_IPAactual_4syl_C3 <- sample_IPAtarget_4syl %>%         # take all actual trisyllables and split into C1, C2, C3 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 3) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S3CF_actual"), "V") 
# 
# sample_IPAactual_4syl_C3 <- sample_IPAactual_4syl_C3 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S3CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_4syl_C3 <- fncols(sample_IPAactual_4syl_C3, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# 
# sample_IPAactual_4syl_V3 <- sample_IPAtarget_4syl %>%         # take all V-initial actual trisyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 3) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S3CF_actual"), "V") 
# 
# sample_IPAactual_4syl_V3 <- sample_IPAactual_4syl_V3 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S3CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")  
# 
# sample_IPAactual_4syl_V3 <- fncols(sample_IPAactual_4syl_V3, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# sample_IPAactual_4syl_C4 <- sample_IPAtarget_4syl %>%        # there are 4 instances of words with 4+ syllables; 3 are onomatopoeia/word play   
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 4) %>%       # extract 4syl word and split into C1, C2, C3, C4 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S4CF_actual"), "V") 
# 
# sample_IPAactual_4syl_C4 <- sample_IPAactual_4syl_C4 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S4CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_4syl_C4 <- fncols(sample_IPAactual_4syl_C4, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# sample_IPAactual_4syl_V4 <- sample_IPAtarget_4syl %>%        # there are 4 instances of words with 4+ syllables; 3 are onomatopoeia/word play   
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 4) %>%       # extract 4syl word and split into C1, C2, C3, C4 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S4CF_actual"), "V") 
# 
# sample_IPAactual_4syl_V4 <- sample_IPAactual_4syl_V4 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S4CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_4syl_V4 <- fncols(sample_IPAactual_4syl_V4, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# 
# 
# sample_IPAactual_4syl_C5 <- sample_IPAtarget_4syl %>%        
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 5) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V") 
# 
# sample_IPAactual_4syl_C5 <- sample_IPAactual_4syl_C5 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_4syl_C5 <- fncols(sample_IPAactual_4syl_C5, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# sample_IPAactual_4syl_V5 <- sample_IPAtarget_4syl %>%        
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 5) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V") 
# 
# sample_IPAactual_4syl_V5 <- sample_IPAactual_4syl_V5 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_4syl_V5 <- fncols(sample_IPAactual_4syl_V5, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# sample_IPAactual_4syl_C6 <- sample_IPAtarget_4syl %>%        
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 6) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S6C1_actual", "S6CF_actual"), "V") 
# 
# sample_IPAactual_4syl_C6 <- sample_IPAactual_4syl_C6 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S6C1_actual, c("AS6C1", "AS6C2", "AS6C3", "AS6C4", "AS6C5"), sep = "(?<=.)") %>%
#   separate(S6CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_4syl_C6 <- fncols(sample_IPAactual_4syl_C6, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# # No V-initial 6-syll productions of 4-syllabic targets in data so omit this from script
# 
# 
# # Join into single DF
# 
# actual_IPAtarget_4syl <- rbind(sample_IPAactual_4syl_C1,
#                                  sample_IPAactual_4syl_V1,
#                                  sample_IPAactual_4syl_C2, 
#                                  sample_IPAactual_4syl_V2, 
#                                  sample_IPAactual_4syl_C3, 
#                                  sample_IPAactual_4syl_V3, 
#                                  sample_IPAactual_4syl_C4,
#                                  sample_IPAactual_4syl_V4, 
#                                  sample_IPAactual_4syl_C5, 
#                                  sample_IPAactual_4syl_V5, 
#                                  sample_IPAactual_4syl_C6)
# 
# 
# # Now do target 5-syl words
# 
# # No V-initial or C-initial 1-syll productions of 5-syllabic targets in data so omit from script
# 
# sample_IPAactual_5syl_C2 <- sample_IPAtarget_5syl %>%         # take all actual disyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 2) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S2CF_actual"), "V") 
# 
# sample_IPAactual_5syl_C2 <- sample_IPAactual_5syl_C2 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_5syl_C2 <- fncols(sample_IPAactual_5syl_C2, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# 
# sample_IPAactual_5syl_V2 <- sample_IPAtarget_5syl %>%         # take all V-initial actual disyllables and split into C1, C2 and CF, removing vowels
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 2) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S2CF_actual"), "V") 
# 
# sample_IPAactual_5syl_V2 <- sample_IPAactual_5syl_V2 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S2CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")  
# 
# sample_IPAactual_5syl_V2 <- fncols(sample_IPAactual_5syl_V2, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# sample_IPAactual_5syl_C3 <- sample_IPAtarget_5syl %>%         # take all actual trisyllables and split into C1, C2, C3 and CF, removing vowels
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 3) %>%
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S3CF_actual"), "V") 
# 
# sample_IPAactual_5syl_C3 <- sample_IPAactual_5syl_C3 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S3CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_5syl_C3 <- fncols(sample_IPAactual_5syl_C3, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# 
# # No V-initial 3-syll productions of 5-syllabic targets in data so omit from script
# 
# 
# sample_IPAactual_5syl_C4 <- sample_IPAtarget_5syl %>%        # there are 4 instances of words with 4+ syllables; 3 are onomatopoeia/word play   
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 4) %>%       # extract 4syl word and split into C1, C2, C3, C4 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S4CF_actual"), "V") 
# 
# sample_IPAactual_5syl_C4 <- sample_IPAactual_5syl_C4 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S4CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_5syl_C4 <- fncols(sample_IPAactual_5syl_C4, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# sample_IPAactual_5syl_V4 <- sample_IPAtarget_5syl %>%        # there are 4 instances of words with 4+ syllables; 3 are onomatopoeia/word play   
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 4) %>%       # extract 4syl word and split into C1, C2, C3, C4 and CF, removing vowels
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S4CF_actual"), "V") 
# 
# sample_IPAactual_5syl_V4 <- sample_IPAactual_5syl_V4 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S4CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_5syl_V4 <- fncols(sample_IPAactual_5syl_V4, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# 
# 
# sample_IPAactual_5syl_C5 <- sample_IPAtarget_5syl %>%        
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 5) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V") 
# 
# sample_IPAactual_5syl_C5 <- sample_IPAactual_5syl_C5 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_5syl_C5 <- fncols(sample_IPAactual_5syl_C5, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# sample_IPAactual_5syl_V5 <- sample_IPAtarget_5syl %>%        
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 5) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V") 
# 
# sample_IPAactual_5syl_V5 <- sample_IPAactual_5syl_V5 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_5syl_V5 <- fncols(sample_IPAactual_5syl_V5, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# # No V-initial or C-initial 6-syll productions of 5-syllabic targets in data so omit from script
# 
# 
# # Join into single DF
# 
# actual_IPAtarget_5syl <- rbind(sample_IPAactual_5syl_C2, 
#                                  sample_IPAactual_5syl_V2, 
#                                  sample_IPAactual_5syl_C3, 
#                                  sample_IPAactual_5syl_C4,
#                                  sample_IPAactual_5syl_V4, 
#                                  sample_IPAactual_5syl_C5, 
#                                  sample_IPAactual_5syl_V5)
# 
# 
# 
# # Now do target 6-syl words
# 
# # No V-initial or C-initial 1,2,3 or 4-syll productions of 6-syllabic targets in data so omit from script
# 
# sample_IPAactual_6syl_C5 <- sample_IPAtarget_6syl %>%        
#   filter(ActualCV %in% Cinitial$ActualCV & nsyl_actual == 5) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V") 
# 
# sample_IPAactual_6syl_C5 <- sample_IPAactual_6syl_C5 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_6syl_C5 <- fncols(sample_IPAactual_6syl_C5, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# 
# # No V-initial 5-syll productions of 6-syllabic targets in data so omit from script
# # No C-initial 6-syll productions of 6-syllabic targets in data so omit from script
# 
# 
# sample_IPAactual_6syl_V6 <- sample_IPAtarget_6syl %>%        
#   filter(ActualCV %in% Vinitial$ActualCV & nsyl_actual == 6) %>%       
#   separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S6C1_actual", "S6CF_actual"), "V") 
# 
# sample_IPAactual_6syl_V6 <- sample_IPAactual_6syl_V6 %>% 
#   separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4", "AS1C5"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4", "AS2C5"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4", "AS3C5"), sep = "(?<=.)") %>%
#   separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4", "AS4C5"), sep = "(?<=.)") %>%
#   separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4", "AS5C5"), sep = "(?<=.)") %>%
#   separate(S6C1_actual, c("AS6C1", "AS6C2", "AS6C3", "AS6C4", "AS6C5"), sep = "(?<=.)") %>%
#   separate(S6CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4", "ASFC5"), sep = "(?<=.)")
# 
# sample_IPAactual_6syl_V6 <- fncols(sample_IPAactual_6syl_V6, c("AS1C1", 
#                                                                    "AS1C2", 
#                                                                    "AS1C3",
#                                                                    "AS1C4",
#                                                                    "AS1C5",
#                                                                    "ASFC1", 
#                                                                    "ASFC2", 
#                                                                    "ASFC3",
#                                                                    "ASFC4",
#                                                                    "ASFC5",
#                                                                    "AS2C1",
#                                                                    "AS2C2",
#                                                                    "AS2C3",
#                                                                    "AS2C4",
#                                                                    "AS2C5",
#                                                                    "AS3C1",
#                                                                    "AS3C2",
#                                                                    "AS3C3",
#                                                                    "AS3C4",
#                                                                    "AS3C5",
#                                                                    "AS4C1",
#                                                                    "AS4C2",
#                                                                    "AS4C3",
#                                                                    "AS4C4",
#                                                                    "AS4C5",
#                                                                    "AS5C1",
#                                                                    "AS5C2",
#                                                                    "AS5C3",
#                                                                    "AS5C4",
#                                                                    "AS5C5",
#                                                                    "AS6C1",
#                                                                    "AS6C2",
#                                                                    "AS6C3",
#                                                                    "AS6C4",
#                                                                    "AS6C5",
#                                                                    "TS1C1",
#                                                                    "TS1C2",
#                                                                    "TS1C3",
#                                                                    "TS1C4",
#                                                                    "TS2C1",
#                                                                    "TS2C2",
#                                                                    "TS2C3",
#                                                                    "TS2C4",
#                                                                    "TS3C1",
#                                                                    "TS3C2",
#                                                                    "TS3C3",
#                                                                    "TS3C4",
#                                                                    "TS4C1",
#                                                                    "TS4C2",
#                                                                    "TS4C3",
#                                                                    "TS4C4",
#                                                                    "TS5C1",
#                                                                    "TS5C2",
#                                                                    "TS5C3",
#                                                                    "TS5C4",
#                                                                    "TS6C1",
#                                                                    "TS6C2",
#                                                                    "TS6C3",
#                                                                    "TS6C4",
#                                                                    "TSFC1",
#                                                                    "TSFC2",
#                                                                    "TSFC3",
#                                                                    "TSFC4"))
# # Join into single DF
# 
# actual_IPAtarget_6syl <- rbind(sample_IPAactual_6syl_C5, 
#                                  sample_IPAactual_6syl_V6)
# 
# 
# 
# # Now join all DFs together (C- and V-initial) to create one full DF that shows target structures and the structures in the child's actual production
# 
# actual_target_IPA_FULL <- rbind(actual_IPAtarget_monosyl,
#                                 actual_IPAtarget_disyl, 
#                                 actual_IPAtarget_trisyl,
#                                 actual_IPAtarget_4syl,
#                                 actual_IPAtarget_5syl,
#                                 actual_IPAtarget_6syl)

#########

# The new DF has 193681 observations, compared with 193685 in the original sample: investigate missing items

#comparison_sample <- FULLsample %>% dplyr::select(ID, Speaker, Session, Gloss, IPAtarget, IPAactual, IPAtarget, IPAactual, nsyl_target, nsyl_actual,TargetCV, ActualCV)
comparison_final <- actual_target_IPA_FULL %>% dplyr::select(ID, 
                                                             Speaker, 
                                                             Session, 
                                                             Gloss,
                                                             IPAtarget, 
                                                             IPAactual,
                                                             nsyl_target,
                                                             nsyl_actual,
                                                             Targetphon,
                                                             Actualphon,
                                                             TargetCV, 
                                                             ActualCV, 
                                                             TargetCV_edited, 
                                                             ActualCV_edited
                                                             )
# 
# missing <- setdiff(comparison_sample, comparison_final)  # 298 items


# Four missing items are instances of vocal play with multisyllable reducplication in the actual form - should be omitted from analysis anyway

##########


# Create dataframe to represent distinctive features of each segment


distinctive.feature.matrix <- tribble(~Symbol, ~Sonorant, ~Consonantal, ~Voice, ~Nasal, ~Degree, ~Labial, ~Palatal, ~Pharyngeal, ~Round, ~Tongue, ~Radical,
                                      "p", -1, 1, -1, -1, 1, 1, 0, -1, 1, 0, 0,
                                      "b", -1, 1, 0, -1, 1, 1, 0, -1, 1, 0, 0,
                                      "t", -1, 1, -1, -1, 1, -1, 1, -1, -1, 1, 0,
                                      "d", -1, 1, 0, -1, 1, -1, 1, -1, -1, 1, 0,
                                      "k", -1, 1, -1, -1, 1, -1, -1, -1, -1, -1, 0,
                                      "ɡ", -1, 1, 0, -1, 1, -1, -1, -1, -1, -1, 0,
                                      "f", -0.5, 1, -1, -1, 0, -1, 1, -1, 1, 0, 0,
                                      "v", -0.5, 1, 0, -1, 0, -1, 1, -1, 1, 0, 0,
                                      "θ", -0.5, 1, -1, -1, 0, -1, 1, -1, -1, 0, 0,
                                      "ð", -0.5, 1, 0, -1, 0, -1, 1, -1, -1, 0, 0,
                                      "s", -0.5, 1, -1, -1, 0, -1, 1, -1, -1, 1, 0,
                                      "c", -0.5, 1, 0, -1, 0, -1, 1, -1, -1, -1, 0,   # infants produce /c/ in some instances, though this doesn't occur in target forms
                                      "z", -0.5, 1, 0, -1, 0, -1, 1, -1, -1, 1, 0,
                                      "h", -0.5, 1, 0, -1, 0, -1, -1, 1, -1, -1, -1,
                                      "ʃ", -0.5, 1, -1, -1, 0, -1, 0, -1, -1, 0, 0,
                                      "ʒ", -0.5, 1, 0, -1, 0, -1, 0, -1, -1, 0, 0,
                                      "ʧ", -0.8, 1, -1, -1, 1, -1, 0, -1, -1, 0, 0,
                                      "ʤ", -0.8, 1, 0, -1, 1, -1, 0, -1, -1, 0, 0,
                                      "m", 0, 0, 1, 1, 1, 1, 0, -1, 1, 0, 0,
                                      "n", 0, 0, 1, 1, 1, -1, 1, -1, -1, 1, 0,
                                      "ŋ", 0, 0, 1, 1, 1, -1, -1, -1, -1, -1, 0,
                                      "ɹ", 0.5, 0, 1, 0, -1, -1, -1, 1, 1, -1, -1,
                                      "r", 0.5, 0, 1, 0, -1, -1, -1, 1, 1, -1, -1,   # some rhotics in the data are coded as /r/
                                      "l", 0.5, 0, 1, 0, -1, -1, 1, -1, -1, 1, 0,
                                      "w", 0.8, 0, 1, 0, 0, 1, -1, -1, 1, -1, 0,
                                      "j", 0.8, 0, 1, 0, 0, -1, 0, -1, -1, 0, 1,
                                      "ɾ", 0.5, 1, 1, 0, -1, -1, -1, 1, -1, 1, 0,
                                      "ʙ", -0.5, 1, 0, -1, 1, 1, 0, -1, 1, 0, 0,
                                      "ʔ", -1, 0, 0, -1, 0, -1, -1, 1, -1, 1, 0)    # added manually as not defined in original. Drew from Cambridge Handboo of Phonology and
                                                                                    # similarities with /h/

# stuck here because the two lists don't match in size

colnames_target <- actual_target_IPA_FULL %>% dplyr::select(ID, starts_with("TS"))
colnames(colnames_target) <- sub("T","",colnames(colnames_target))
target_list <- setNames(lapply(names(colnames_target)[-1], function(x) cbind(colnames_target[1], colnames_target[x])), names(colnames_target)[-1])

output_target <- lapply(target_list, FUN = function(element) {
  target_segment <- data.frame(element,
                               distinctive.feature.matrix[match(element[,2], distinctive.feature.matrix$Symbol), 2:12], 
                               stringsAsFactors=FALSE) %>%
    replace(is.na(.), 0) %>%
    mutate(data_type = "Target")
})

colnames_actual <- actual_target_IPA_FULL %>% dplyr::select(ID, starts_with("AS"))
colnames(colnames_actual) <- sub("A","",colnames(colnames_actual))
actual_list <- setNames(lapply(names(colnames_actual)[-1], function(x) cbind(colnames_actual[1], colnames_actual[x])), names(colnames_actual)[-1])

output_actual <- lapply(actual_list, FUN = function(element) {
  target_segment <- data.frame(element,
                               distinctive.feature.matrix[match(element[,2], distinctive.feature.matrix$Symbol), 2:12], 
                               stringsAsFactors=FALSE)  %>%
    replace(is.na(.), 0) %>%
    mutate(data_type = "Actual")
})


output_full <- mapply(rbind,output_target,output_actual,SIMPLIFY=FALSE) # below I'll convert this into a DF for generating the global matrix

output_full_dist <- lapply(output_full, FUN = function(element) {
  target <- element %>% filter(data_type == "Target") 
  actual <- element %>% filter(data_type == "Actual")
  sonorant_df <- actual %>% mutate(sonorant_diff = (actual$Sonorant - target$Sonorant)^2)
  consonantal_df <- actual %>% mutate(consonantal_diff = (actual$Consonantal - target$Consonantal)^2)
  voice_df <- actual %>% mutate(voice_diff = (actual$Voice - target$Voice)^2)
  nasal_df <- actual %>% mutate(nasal_diff = (actual$Nasal - target$Nasal)^2)
  degree_df <- actual %>% mutate(degree_diff = (actual$Degree - target$Degree)^2)
  labial_df <- actual %>% mutate(labial_diff = (actual$Labial - target$Labial)^2)
  palatal_df <- actual %>% mutate(palatal_diff = (actual$Palatal - target$Palatal)^2)
  pharyngeal_df <- actual %>% mutate(pharyngeal_diff = (actual$Pharyngeal - target$Pharyngeal)^2)
  round_df <- actual %>% mutate(round_diff = (actual$Round - target$Round)^2)
  tongue_df <- actual %>% mutate(tongue_diff = (actual$Tongue - target$Tongue)^2)
  radical_df <- actual %>% mutate(radical_diff = (actual$Radical - target$Radical)^2)
  element_dist <- actual %>% mutate(final_dist = 
                                      sqrt(sonorant_df$sonorant_diff + 
                                             consonantal_df$consonantal_diff + 
                                             voice_df$voice_diff + 
                                             nasal_df$nasal_diff + 
                                             degree_df$degree_diff + 
                                             labial_df$labial_diff +
                                             palatal_df$palatal_diff + 
                                             pharyngeal_df$pharyngeal_diff + 
                                             round_df$round_diff + 
                                             tongue_df$tongue_diff + 
                                             radical_df$radical_diff))
  # element_dist_final <- element_dist %>% dplyr::select(-Sonorant, -Consonantal, - Voice, -Nasal, -Degree, -Labial, -Palatal, -Pharyngeal, -Round, -Tongue, -Radical)
})

dist_final_df <- as.data.frame(output_full_dist)

colnames(dist_final_df)[1] <- "unique"

dist_final <- dist_final_df %>% dplyr::select(unique, -ends_with("data_type") & -ends_with(".ID") & -!contains("final_dist")) %>%
  mutate(distance = rowSums(.[2:25])) %>%
  dplyr::select(unique, distance) %>%
  rename("ID" = "unique")

comparison_data <- comparison_final %>% left_join(dist_final)

# # turn NAs to null in all these sub DFs
# 
# S1C1T <- data.frame(S1C1_target = actual_target_IPA_FULL$TS1C1,
#                     S1C1_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS1C1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S1C1A <- data.frame(S1C1_actual=actual_target_IPA_FULL$AS1C1,
#                     S1C1_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS1C1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S1C2T <- data.frame(S1C2_target = actual_target_IPA_FULL$TS1C2,
#                     S1C2_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS1C2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
#          
# 
# S1C2A <- data.frame(S1C2_actual=actual_target_IPA_FULL$AS1C2,
#                     S1C2_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS1C2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number())  %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S1C3T <- data.frame(S1C3_target = actual_target_IPA_FULL$TS1C3,
#                     S1C3_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS1C3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S1C3A <- data.frame(S1C3_actual=actual_target_IPA_FULL$AS1C3,
#                     S1C3_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS1C3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S1C4A <- data.frame(S1C4_actual=actual_target_IPA_FULL$AS1C4,
#                     S1C4_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS1C4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S1C4T <- data.frame(S1C4_target = actual_target_IPA_FULL$TS1C4,
#                     S1C4_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS1C4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S2C1T <- data.frame(S2C1_target = actual_target_IPA_FULL$TS2C1,
#                     S2C1_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS2C1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S2C1A <- data.frame(S2C1_actual=actual_target_IPA_FULL$AS2C1,
#                     S2C1_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS2C1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S2C2T <- data.frame(S2C2_target = actual_target_IPA_FULL$TS2C2,
#                     S2C2_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS2C2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S2C2A <- data.frame(S2C2_actual=actual_target_IPA_FULL$AS2C2,
#                     S2C2_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS2C2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S2C3T <- data.frame(S2C3_target = actual_target_IPA_FULL$TS2C3,
#                     S2C3_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS2C3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S2C3A <- data.frame(S2C3_actual=actual_target_IPA_FULL$AS2C3,
#                     S2C3_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS2C3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S2C4A <- data.frame(S2C4_actual=actual_target_IPA_FULL$AS2C4,
#                     S2C4_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS2C4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S2C4T <- data.frame(S124_target = actual_target_IPA_FULL$TS2C4,
#                     S2C4_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS2C4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S3C1T <- data.frame(S3C1_target = actual_target_IPA_FULL$TS3C1,
#                     S3C1_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS3C1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S3C1A <- data.frame(S3C1_actual=actual_target_IPA_FULL$AS3C1,
#                     S3C1_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS3C1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S3C2T <- data.frame(S3C2_target = actual_target_IPA_FULL$TS3C2,
#                     S3C2_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS3C2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S3C2A <- data.frame(S3C2_actual=actual_target_IPA_FULL$AS3C2,
#                     S3C2_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS3C2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S3C3T <- data.frame(S3C3_target = actual_target_IPA_FULL$TS3C3,
#                     S3C3_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS3C3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S3C3A <- data.frame(S3C3_actual=actual_target_IPA_FULL$AS3C3,
#                     S3C3_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS3C3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S3C4A <- data.frame(S3C4_actual=actual_target_IPA_FULL$AS3C4,
#                     S3C4_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS3C4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S3C4T <- data.frame(S3C4_target = actual_target_IPA_FULL$TS3C4,
#                     S3C4_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS3C4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S4C1T <- data.frame(S4C1_target = actual_target_IPA_FULL$TS4C1,
#                     S4C1_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS4C1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S4C1A <- data.frame(S4C1_actual=actual_target_IPA_FULL$AS4C1,
#                     S4C1_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS4C1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S4C2T <- data.frame(S4C2_target = actual_target_IPA_FULL$TS4C2,
#                     S4C2_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS4C2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S4C2A <- data.frame(S4C2_actual=actual_target_IPA_FULL$AS4C2,
#                     S4C2_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS4C2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S4C3T <- data.frame(S4C3_target = actual_target_IPA_FULL$TS4C3,
#                     S4C3_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS4C3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S4C3A <- data.frame(S4C3_actual=actual_target_IPA_FULL$AS4C3,
#                     S4C3_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS4C3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S4C4A <- data.frame(S4C4_actual=actual_target_IPA_FULL$AS4C4,
#                     S4C4_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS4C4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S4C4T <- data.frame(S4C4_target = actual_target_IPA_FULL$TS4C4,
#                     S4C4_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS4C4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S5C1T <- data.frame(S5C1_target = actual_target_IPA_FULL$TS5C1,
#                     S5C1_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS5C1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S5C1A <- data.frame(S5C1_actual=actual_target_IPA_FULL$AS5C1,
#                     S5C1_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS5C1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S5C2T <- data.frame(S5C2_target = actual_target_IPA_FULL$TS5C2,
#                     S5C2_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS5C2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S5C2A <- data.frame(S5C2_actual=actual_target_IPA_FULL$AS5C2,
#                     S5C2_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS5C2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S5C3T <- data.frame(S5C3_target = actual_target_IPA_FULL$TS5C3,
#                     S5C3_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS5C3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S5C3A <- data.frame(S5C3_actual=actual_target_IPA_FULL$AS5C3,
#                     S5C3_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS5C3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S5C4A <- data.frame(S5C4_actual=actual_target_IPA_FULL$AS5C4,
#                     S5C4_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS5C4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S5C4T <- data.frame(S5C4_target = actual_target_IPA_FULL$TS5C4,
#                     S5C4_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS5C4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S6C1T <- data.frame(S6C1_target = actual_target_IPA_FULL$TS6C1,
#                     S6C1_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS6C1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S6C1A <- data.frame(S6C1_actual=actual_target_IPA_FULL$AS6C1,
#                     S6C1_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS6C1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S6C2T <- data.frame(S6C2_target = actual_target_IPA_FULL$TS6C2,
#                     S6C2_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS6C2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S6C2A <- data.frame(S6C2_actual=actual_target_IPA_FULL$AS6C2,
#                     S6C2_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS6C2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S6C3T <- data.frame(S6C3_target = actual_target_IPA_FULL$TS6C3,
#                     S6C3_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS6C3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# S6C3A <- data.frame(S6C3_actual=actual_target_IPA_FULL$AS6C3,
#                     S6C3_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS6C3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S6C4A <- data.frame(S6C4_actual=actual_target_IPA_FULL$AS6C4,
#                     S6C4_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$AS6C4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# S6C4T <- data.frame(S6C4_target = actual_target_IPA_FULL$TS6C4,
#                     S6C4_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TS6C4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# SFC1T <- data.frame(SFC1_target = actual_target_IPA_FULL$TSFC1,
#                     SFC1_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TSFC1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# SFC1A <- data.frame(SFC1_actual = actual_target_IPA_FULL$ASFC1,
#                     SFC1_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$ASFC1, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# SFC2T <- data.frame(SFC2_target = actual_target_IPA_FULL$TSFC2,
#                     SFC2_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TSFC2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# SFC2A <- data.frame(SFC2_actual = actual_target_IPA_FULL$ASFC2,
#                     SFC2_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$ASFC2, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# SFC3T <- data.frame(SFC3_target = actual_target_IPA_FULL$TSFC3,
#                     SFC3_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TSFC3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# SFC3A <- data.frame(SFC3_actual = actual_target_IPA_FULL$ASFC3,
#                     SFC3_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$ASFC3, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# SFC4T <- data.frame(SFC4_target = actual_target_IPA_FULL$TSFC4,
#                     SFC4_T=distinctive.feature.matrix[match(actual_target_IPA_FULL$TSFC4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_T.")), ~replace_na(., 0))
# 
# SFC4A <- data.frame(SFC4_actual = actual_target_IPA_FULL$ASFC4,
#                     SFC4_A=distinctive.feature.matrix[match(actual_target_IPA_FULL$ASFC4, distinctive.feature.matrix$Symbol), 2:12], 
#                     stringsAsFactors=FALSE) %>%
#   mutate(linenumber = row_number()) %>%
#   mutate_at(vars(contains("_A.")), ~replace_na(., 0))
# 
# 
# 
# comparison_data <- S1C1T %>% mutate(Speaker = comparison_final$Speaker,
#                                     Session = comparison_final$Session,
#                                     Gloss = comparison_final$Gloss,
#                                     IPAtarget = comparison_final$IPAtarget,
#                                     IPAactual = comparison_final$IPAactual,
#                                     nsyl_target = comparison_final$nsyl_target,
#                                     nsyl_actual = comparison_final$nsyl_actual,
#                                     Targetphon = comparison_final$Targetphon,
#                                     Actualphon = comparison_final$Actualphon,
#                                     TargetCV = comparison_final$TargetCV,
#                                     ActualCV = comparison_final$ActualCV,
#                                     TargetCV_edited = comparison_final$TargetCV_edited,
#                                     ActualCV_edited = comparison_final$ActualCV_edited
#                                     ) %>%
#   select(linenumber, Speaker, Session,
#          Gloss,
#          IPAtarget,
#          IPAactual,
#          Targetphon,
#          Actualphon,
#          nsyl_target,
#          nsyl_actual,
#          TargetCV,
#          ActualCV,
#          TargetCV_edited,
#          ActualCV_edited,
#          S1C1_target,
#          S1C1_T.Sonorant,
#          S1C1_T.Consonantal,
#          S1C1_T.Voice,
#          S1C1_T.Nasal,
#          S1C1_T.Degree,
#          S1C1_T.Labial,
#          S1C1_T.Palatal,
#          S1C1_T.Pharyngeal,
#          S1C1_T.Round,
#          S1C1_T.Tongue,
#          S1C1_T.Radical) %>%
#   left_join(S1C1A, by = "linenumber") %>%
#   left_join(S1C2A, by = "linenumber") %>%
#   left_join(S1C2T, by = "linenumber") %>%
#   left_join(S1C3A, by = "linenumber") %>%
#   left_join(S1C3T, by = "linenumber") %>%
#   left_join(S1C4A, by = "linenumber") %>%
#   left_join(S1C4T, by = "linenumber") %>%
#   left_join(S2C1A, by = "linenumber") %>%
#   left_join(S2C1T, by = "linenumber") %>%
#   left_join(S2C2A, by = "linenumber") %>%
#   left_join(S2C2T, by = "linenumber") %>%
#   left_join(S2C3A, by = "linenumber") %>%
#   left_join(S2C3T, by = "linenumber") %>%
#   left_join(S2C4A, by = "linenumber") %>%
#   left_join(S2C4T, by = "linenumber") %>%
#   left_join(S3C1A, by = "linenumber") %>%
#   left_join(S3C1T, by = "linenumber") %>%
#   left_join(S3C2A, by = "linenumber") %>%
#   left_join(S3C2T, by = "linenumber") %>%
#   left_join(S3C3A, by = "linenumber") %>%
#   left_join(S3C3T, by = "linenumber") %>%
#   left_join(S3C4A, by = "linenumber") %>%
#   left_join(S3C4T, by = "linenumber") %>%
#   left_join(S4C1A, by = "linenumber") %>%
#   left_join(S4C1T, by = "linenumber") %>%
#   left_join(S4C2A, by = "linenumber") %>%
#   left_join(S4C2T, by = "linenumber") %>%
#   left_join(S4C3A, by = "linenumber") %>%
#   left_join(S4C3T, by = "linenumber") %>%
#   left_join(S4C4A, by = "linenumber") %>%
#   left_join(S4C4T, by = "linenumber") %>%
#   left_join(S5C1A, by = "linenumber") %>%
#   left_join(S5C1T, by = "linenumber") %>%
#   left_join(S5C2A, by = "linenumber") %>%
#   left_join(S5C2T, by = "linenumber") %>%
#   left_join(S5C3A, by = "linenumber") %>%
#   left_join(S5C3T, by = "linenumber") %>%
#   left_join(S5C4A, by = "linenumber") %>%
#   left_join(S5C4T, by = "linenumber") %>%
#   left_join(S6C1A, by = "linenumber") %>%
#   left_join(S6C1T, by = "linenumber") %>%
#   left_join(S6C2A, by = "linenumber") %>%
#   left_join(S6C2T, by = "linenumber") %>%
#   left_join(S6C3A, by = "linenumber") %>%
#   left_join(S6C3T, by = "linenumber") %>%
#   left_join(S6C4A, by = "linenumber") %>%
#   left_join(S6C4T, by = "linenumber") %>%
#   left_join(SFC1A, by = "linenumber") %>%
#   left_join(SFC1T, by = "linenumber") %>%
#   left_join(SFC2A, by = "linenumber") %>%
#   left_join(SFC2T, by = "linenumber") %>%
#   left_join(SFC3A, by = "linenumber") %>%
#   left_join(SFC3T, by = "linenumber") %>%
#   left_join(SFC4A, by = "linenumber") %>%
#   left_join(SFC4T, by = "linenumber")
# 
# # Calculate feature distance between each segment in each syllable position
# 
# comparison_data_phondist <- comparison_data %>%
#   mutate(S1C1_Sonorant.diff = (S1C1_T.Sonorant - S1C1_A.Sonorant)^2,
#          S1C2_Sonorant.diff = (S1C2_T.Sonorant - S1C2_A.Sonorant)^2,
#          S1C3_Sonorant.diff = (S1C3_T.Sonorant - S1C3_A.Sonorant)^2,
#          S1C4_Sonorant.diff = (S1C4_T.Sonorant - S1C4_A.Sonorant)^2,
#          S2C1_Sonorant.diff = (S2C1_T.Sonorant - S2C1_A.Sonorant)^2,
#          S2C2_Sonorant.diff = (S2C2_T.Sonorant - S2C2_A.Sonorant)^2,
#          S2C3_Sonorant.diff = (S2C3_T.Sonorant - S2C3_A.Sonorant)^2,
#          S2C4_Sonorant.diff = (S2C4_T.Sonorant - S2C4_A.Sonorant)^2,
#          S3C1_Sonorant.diff = (S3C1_T.Sonorant - S3C1_A.Sonorant)^2,
#          S3C2_Sonorant.diff = (S3C2_T.Sonorant - S3C2_A.Sonorant)^2,
#          S3C3_Sonorant.diff = (S3C3_T.Sonorant - S3C3_A.Sonorant)^2,
#          S3C4_Sonorant.diff = (S3C4_T.Sonorant - S3C4_A.Sonorant)^2,
#          S4C1_Sonorant.diff = (S4C1_T.Sonorant - S4C1_A.Sonorant)^2,
#          S4C2_Sonorant.diff = (S4C2_T.Sonorant - S4C2_A.Sonorant)^2,
#          S4C3_Sonorant.diff = (S4C3_T.Sonorant - S4C3_A.Sonorant)^2,
#          S4C4_Sonorant.diff = (S4C4_T.Sonorant - S4C4_A.Sonorant)^2,
#          S5C1_Sonorant.diff = (S5C1_T.Sonorant - S5C1_A.Sonorant)^2,
#          S5C2_Sonorant.diff = (S5C2_T.Sonorant - S5C2_A.Sonorant)^2,
#          S5C3_Sonorant.diff = (S5C3_T.Sonorant - S5C3_A.Sonorant)^2,
#          S5C4_Sonorant.diff = (S5C4_T.Sonorant - S5C4_A.Sonorant)^2,
#          S6C1_Sonorant.diff = (S6C1_T.Sonorant - S6C1_A.Sonorant)^2,
#          S6C2_Sonorant.diff = (S6C2_T.Sonorant - S6C2_A.Sonorant)^2,
#          S6C3_Sonorant.diff = (S6C3_T.Sonorant - S6C3_A.Sonorant)^2,
#          S6C4_Sonorant.diff = (S6C4_T.Sonorant - S6C4_A.Sonorant)^2,
#          SFC1_Sonorant.diff = (SFC1_T.Sonorant - SFC1_A.Sonorant)^2,
#          SFC2_Sonorant.diff = (SFC2_T.Sonorant - SFC2_A.Sonorant)^2,
#          SFC3_Sonorant.diff = (SFC3_T.Sonorant - SFC3_A.Sonorant)^2,
#          SFC4_Sonorant.diff = (SFC4_T.Sonorant - SFC4_A.Sonorant)^2,
#          S1C1_Consonantal.diff = (S1C1_T.Consonantal - S1C1_A.Consonantal)^2,
#          S1C2_Consonantal.diff = (S1C2_T.Consonantal - S1C2_A.Consonantal)^2,
#          S1C3_Consonantal.diff = (S1C3_T.Consonantal - S1C3_A.Consonantal)^2,
#          S1C4_Consonantal.diff = (S1C4_T.Consonantal - S1C4_A.Consonantal)^2,
#          S2C1_Consonantal.diff = (S2C1_T.Consonantal - S2C1_A.Consonantal)^2,
#          S2C2_Consonantal.diff = (S2C2_T.Consonantal - S2C2_A.Consonantal)^2,
#          S2C3_Consonantal.diff = (S2C3_T.Consonantal - S2C3_A.Consonantal)^2,
#          S2C4_Consonantal.diff = (S2C4_T.Consonantal - S2C4_A.Consonantal)^2,
#          S3C1_Consonantal.diff = (S3C1_T.Consonantal - S3C1_A.Consonantal)^2,
#          S3C2_Consonantal.diff = (S3C2_T.Consonantal - S3C2_A.Consonantal)^2,
#          S3C3_Consonantal.diff = (S3C3_T.Consonantal - S3C3_A.Consonantal)^2,
#          S3C4_Consonantal.diff = (S3C4_T.Consonantal - S3C4_A.Consonantal)^2,
#          S4C1_Consonantal.diff = (S4C1_T.Consonantal - S4C1_A.Consonantal)^2,
#          S4C2_Consonantal.diff = (S4C2_T.Consonantal - S4C2_A.Consonantal)^2,
#          S4C3_Consonantal.diff = (S4C3_T.Consonantal - S4C3_A.Consonantal)^2,
#          S4C4_Consonantal.diff = (S4C4_T.Consonantal - S4C4_A.Consonantal)^2,
#          S5C1_Consonantal.diff = (S5C1_T.Consonantal - S5C1_A.Consonantal)^2,
#          S5C2_Consonantal.diff = (S5C2_T.Consonantal - S5C2_A.Consonantal)^2,
#          S5C3_Consonantal.diff = (S5C3_T.Consonantal - S5C3_A.Consonantal)^2,
#          S5C4_Consonantal.diff = (S5C4_T.Consonantal - S5C4_A.Consonantal)^2,
#          S6C1_Consonantal.diff = (S6C1_T.Consonantal - S6C1_A.Consonantal)^2,
#          S6C2_Consonantal.diff = (S6C2_T.Consonantal - S6C2_A.Consonantal)^2,
#          S6C3_Consonantal.diff = (S6C3_T.Consonantal - S6C3_A.Consonantal)^2,
#          S6C4_Consonantal.diff = (S6C4_T.Consonantal - S6C4_A.Consonantal)^2,
#          SFC1_Consonantal.diff = (SFC1_T.Consonantal - SFC1_A.Consonantal)^2,
#          SFC2_Consonantal.diff = (SFC2_T.Consonantal - SFC2_A.Consonantal)^2,
#          SFC3_Consonantal.diff = (SFC3_T.Consonantal - SFC3_A.Consonantal)^2,
#          SFC4_Consonantal.diff = (SFC4_T.Consonantal - SFC4_A.Consonantal)^2,
#          S1C1_Voice.diff = (S1C1_T.Voice - S1C1_A.Voice)^2,
#          S1C2_Voice.diff = (S1C2_T.Voice - S1C2_A.Voice)^2,
#          S1C3_Voice.diff = (S1C3_T.Voice - S1C3_A.Voice)^2,
#          S1C4_Voice.diff = (S1C4_T.Voice - S1C4_A.Voice)^2,
#          S2C1_Voice.diff = (S2C1_T.Voice - S2C1_A.Voice)^2,
#          S2C2_Voice.diff = (S2C2_T.Voice - S2C2_A.Voice)^2,
#          S2C3_Voice.diff = (S2C3_T.Voice - S2C3_A.Voice)^2,
#          S2C4_Voice.diff = (S2C4_T.Voice - S2C4_A.Voice)^2,
#          S3C1_Voice.diff = (S3C1_T.Voice - S3C1_A.Voice)^2,
#          S3C2_Voice.diff = (S3C2_T.Voice - S3C2_A.Voice)^2,
#          S3C3_Voice.diff = (S3C3_T.Voice - S3C3_A.Voice)^2,
#          S3C4_Voice.diff = (S3C4_T.Voice - S3C4_A.Voice)^2,
#          S4C1_Voice.diff = (S4C1_T.Voice - S4C1_A.Voice)^2,
#          S4C2_Voice.diff = (S4C2_T.Voice - S4C2_A.Voice)^2,
#          S4C3_Voice.diff = (S4C3_T.Voice - S4C3_A.Voice)^2,
#          S4C4_Voice.diff = (S4C4_T.Voice - S4C4_A.Voice)^2,
#          S5C1_Voice.diff = (S5C1_T.Voice - S5C1_A.Voice)^2,
#          S5C2_Voice.diff = (S5C2_T.Voice - S5C2_A.Voice)^2,
#          S5C3_Voice.diff = (S5C3_T.Voice - S5C3_A.Voice)^2,
#          S5C4_Voice.diff = (S5C4_T.Voice - S5C4_A.Voice)^2,
#          S6C1_Voice.diff = (S6C1_T.Voice - S6C1_A.Voice)^2,
#          S6C2_Voice.diff = (S6C2_T.Voice - S6C2_A.Voice)^2,
#          S6C3_Voice.diff = (S6C3_T.Voice - S6C3_A.Voice)^2,
#          S6C4_Voice.diff = (S6C4_T.Voice - S6C4_A.Voice)^2,
#          SFC1_Voice.diff = (SFC1_T.Voice - SFC1_A.Voice)^2,
#          SFC2_Voice.diff = (SFC2_T.Voice - SFC2_A.Voice)^2,
#          SFC3_Voice.diff = (SFC3_T.Voice - SFC3_A.Voice)^2,
#          SFC4_Voice.diff = (SFC4_T.Voice - SFC4_A.Voice)^2,
#          S1C1_Nasal.diff = (S1C1_T.Nasal - S1C1_A.Nasal)^2,
#          S1C2_Nasal.diff = (S1C2_T.Nasal - S1C2_A.Nasal)^2,
#          S1C3_Nasal.diff = (S1C3_T.Nasal - S1C3_A.Nasal)^2,
#          S1C4_Nasal.diff = (S1C4_T.Nasal - S1C4_A.Nasal)^2,
#          S2C1_Nasal.diff = (S2C1_T.Nasal - S2C1_A.Nasal)^2,
#          S2C2_Nasal.diff = (S2C2_T.Nasal - S2C2_A.Nasal)^2,
#          S2C3_Nasal.diff = (S2C3_T.Nasal - S2C3_A.Nasal)^2,
#          S2C4_Nasal.diff = (S2C4_T.Nasal - S2C4_A.Nasal)^2,
#          S3C1_Nasal.diff = (S3C1_T.Nasal - S3C1_A.Nasal)^2,
#          S3C2_Nasal.diff = (S3C2_T.Nasal - S3C2_A.Nasal)^2,
#          S3C3_Nasal.diff = (S3C3_T.Nasal - S3C3_A.Nasal)^2,
#          S3C4_Nasal.diff = (S3C4_T.Nasal - S3C4_A.Nasal)^2,
#          S4C1_Nasal.diff = (S4C1_T.Nasal - S4C1_A.Nasal)^2,
#          S4C2_Nasal.diff = (S4C2_T.Nasal - S4C2_A.Nasal)^2,
#          S4C3_Nasal.diff = (S4C3_T.Nasal - S4C3_A.Nasal)^2,
#          S4C4_Nasal.diff = (S4C4_T.Nasal - S4C4_A.Nasal)^2,
#          S5C1_Nasal.diff = (S5C1_T.Nasal - S5C1_A.Nasal)^2,
#          S5C2_Nasal.diff = (S5C2_T.Nasal - S5C2_A.Nasal)^2,
#          S5C3_Nasal.diff = (S5C3_T.Nasal - S5C3_A.Nasal)^2,
#          S5C4_Nasal.diff = (S5C4_T.Nasal - S5C4_A.Nasal)^2,
#          S6C1_Nasal.diff = (S6C1_T.Nasal - S6C1_A.Nasal)^2,
#          S6C2_Nasal.diff = (S6C2_T.Nasal - S6C2_A.Nasal)^2,
#          S6C3_Nasal.diff = (S6C3_T.Nasal - S6C3_A.Nasal)^2,
#          S6C4_Nasal.diff = (S6C4_T.Nasal - S6C4_A.Nasal)^2,
#          SFC1_Nasal.diff = (SFC1_T.Nasal - SFC1_A.Nasal)^2,
#          SFC2_Nasal.diff = (SFC2_T.Nasal - SFC2_A.Nasal)^2,
#          SFC3_Nasal.diff = (SFC3_T.Nasal - SFC3_A.Nasal)^2,
#          SFC4_Nasal.diff = (SFC4_T.Nasal - SFC4_A.Nasal)^2,
#          S1C1_Degree.diff = (S1C1_T.Degree - S1C1_A.Degree)^2,
#          S1C2_Degree.diff = (S1C2_T.Degree - S1C2_A.Degree)^2,
#          S1C3_Degree.diff = (S1C3_T.Degree - S1C3_A.Degree)^2,
#          S1C4_Degree.diff = (S1C4_T.Degree - S1C4_A.Degree)^2,
#          S2C1_Degree.diff = (S2C1_T.Degree - S2C1_A.Degree)^2,
#          S2C2_Degree.diff = (S2C2_T.Degree - S2C2_A.Degree)^2,
#          S2C3_Degree.diff = (S2C3_T.Degree - S2C3_A.Degree)^2,
#          S2C4_Degree.diff = (S2C4_T.Degree - S2C4_A.Degree)^2,
#          S3C1_Degree.diff = (S3C1_T.Degree - S3C1_A.Degree)^2,
#          S3C2_Degree.diff = (S3C2_T.Degree - S3C2_A.Degree)^2,
#          S3C3_Degree.diff = (S3C3_T.Degree - S3C3_A.Degree)^2,
#          S3C4_Degree.diff = (S3C4_T.Degree - S3C4_A.Degree)^2,
#          S4C1_Degree.diff = (S4C1_T.Degree - S4C1_A.Degree)^2,
#          S4C2_Degree.diff = (S4C2_T.Degree - S4C2_A.Degree)^2,
#          S4C3_Degree.diff = (S4C3_T.Degree - S4C3_A.Degree)^2,
#          S4C4_Degree.diff = (S4C4_T.Degree - S4C4_A.Degree)^2,
#          S5C1_Degree.diff = (S5C1_T.Degree - S5C1_A.Degree)^2,
#          S5C2_Degree.diff = (S5C2_T.Degree - S5C2_A.Degree)^2,
#          S5C3_Degree.diff = (S5C3_T.Degree - S5C3_A.Degree)^2,
#          S5C4_Degree.diff = (S5C4_T.Degree - S5C4_A.Degree)^2,
#          S6C1_Degree.diff = (S6C1_T.Degree - S6C1_A.Degree)^2,
#          S6C2_Degree.diff = (S6C2_T.Degree - S6C2_A.Degree)^2,
#          S6C3_Degree.diff = (S6C3_T.Degree - S6C3_A.Degree)^2,
#          S6C4_Degree.diff = (S6C4_T.Degree - S6C4_A.Degree)^2,
#          SFC1_Degree.diff = (SFC1_T.Degree - SFC1_A.Degree)^2,
#          SFC2_Degree.diff = (SFC2_T.Degree - SFC2_A.Degree)^2,
#          SFC3_Degree.diff = (SFC3_T.Degree - SFC3_A.Degree)^2,
#          SFC4_Degree.diff = (SFC4_T.Degree - SFC4_A.Degree)^2,
#          S1C1_Labial.diff = (S1C1_T.Labial - S1C1_A.Labial)^2,
#          S1C2_Labial.diff = (S1C2_T.Labial - S1C2_A.Labial)^2,
#          S1C3_Labial.diff = (S1C3_T.Labial - S1C3_A.Labial)^2,
#          S1C4_Labial.diff = (S1C4_T.Labial - S1C4_A.Labial)^2,
#          S2C1_Labial.diff = (S2C1_T.Labial - S2C1_A.Labial)^2,
#          S2C2_Labial.diff = (S2C2_T.Labial - S2C2_A.Labial)^2,
#          S2C3_Labial.diff = (S2C3_T.Labial - S2C3_A.Labial)^2,
#          S2C4_Labial.diff = (S2C4_T.Labial - S2C4_A.Labial)^2,
#          S3C1_Labial.diff = (S3C1_T.Labial - S3C1_A.Labial)^2,
#          S3C2_Labial.diff = (S3C2_T.Labial - S3C2_A.Labial)^2,
#          S3C3_Labial.diff = (S3C3_T.Labial - S3C3_A.Labial)^2,
#          S3C4_Labial.diff = (S3C4_T.Labial - S3C4_A.Labial)^2,
#          S4C1_Labial.diff = (S4C1_T.Labial - S4C1_A.Labial)^2,
#          S4C2_Labial.diff = (S4C2_T.Labial - S4C2_A.Labial)^2,
#          S4C3_Labial.diff = (S4C3_T.Labial - S4C3_A.Labial)^2,
#          S4C4_Labial.diff = (S4C4_T.Labial - S4C4_A.Labial)^2,
#          S5C1_Labial.diff = (S5C1_T.Labial - S5C1_A.Labial)^2,
#          S5C2_Labial.diff = (S5C2_T.Labial - S5C2_A.Labial)^2,
#          S5C3_Labial.diff = (S5C3_T.Labial - S5C3_A.Labial)^2,
#          S5C4_Labial.diff = (S5C4_T.Labial - S5C4_A.Labial)^2,
#          S6C1_Labial.diff = (S6C1_T.Labial - S6C1_A.Labial)^2,
#          S6C2_Labial.diff = (S6C2_T.Labial - S6C2_A.Labial)^2,
#          S6C3_Labial.diff = (S6C3_T.Labial - S6C3_A.Labial)^2,
#          S6C4_Labial.diff = (S6C4_T.Labial - S6C4_A.Labial)^2,
#          SFC1_Labial.diff = (SFC1_T.Labial - SFC1_A.Labial)^2,
#          SFC2_Labial.diff = (SFC2_T.Labial - SFC2_A.Labial)^2,
#          SFC3_Labial.diff = (SFC3_T.Labial - SFC3_A.Labial)^2,
#          SFC4_Labial.diff = (SFC4_T.Labial - SFC4_A.Labial)^2,
#          S1C1_Palatal.diff = (S1C1_T.Palatal - S1C1_A.Palatal)^2,
#          S1C2_Palatal.diff = (S1C2_T.Palatal - S1C2_A.Palatal)^2,
#          S1C3_Palatal.diff = (S1C3_T.Palatal - S1C3_A.Palatal)^2,
#          S1C4_Palatal.diff = (S1C4_T.Palatal - S1C4_A.Palatal)^2,
#          S2C1_Palatal.diff = (S2C1_T.Palatal - S2C1_A.Palatal)^2,
#          S2C2_Palatal.diff = (S2C2_T.Palatal - S2C2_A.Palatal)^2,
#          S2C3_Palatal.diff = (S2C3_T.Palatal - S2C3_A.Palatal)^2,
#          S2C4_Palatal.diff = (S2C4_T.Palatal - S2C4_A.Palatal)^2,
#          S3C1_Palatal.diff = (S3C1_T.Palatal - S3C1_A.Palatal)^2,
#          S3C2_Palatal.diff = (S3C2_T.Palatal - S3C2_A.Palatal)^2,
#          S3C3_Palatal.diff = (S3C3_T.Palatal - S3C3_A.Palatal)^2,
#          S3C4_Palatal.diff = (S3C4_T.Palatal - S3C4_A.Palatal)^2,
#          S4C1_Palatal.diff = (S4C1_T.Palatal - S4C1_A.Palatal)^2,
#          S4C2_Palatal.diff = (S4C2_T.Palatal - S4C2_A.Palatal)^2,
#          S4C3_Palatal.diff = (S4C3_T.Palatal - S4C3_A.Palatal)^2,
#          S4C4_Palatal.diff = (S4C4_T.Palatal - S4C4_A.Palatal)^2,
#          S5C1_Palatal.diff = (S5C1_T.Palatal - S5C1_A.Palatal)^2,
#          S5C2_Palatal.diff = (S5C2_T.Palatal - S5C2_A.Palatal)^2,
#          S5C3_Palatal.diff = (S5C3_T.Palatal - S5C3_A.Palatal)^2,
#          S5C4_Palatal.diff = (S5C4_T.Palatal - S5C4_A.Palatal)^2,
#          S6C1_Palatal.diff = (S6C1_T.Palatal - S6C1_A.Palatal)^2,
#          S6C2_Palatal.diff = (S6C2_T.Palatal - S6C2_A.Palatal)^2,
#          S6C3_Palatal.diff = (S6C3_T.Palatal - S6C3_A.Palatal)^2,
#          S6C4_Palatal.diff = (S6C4_T.Palatal - S6C4_A.Palatal)^2,
#          SFC1_Palatal.diff = (SFC1_T.Palatal - SFC1_A.Palatal)^2,
#          SFC2_Palatal.diff = (SFC2_T.Palatal - SFC2_A.Palatal)^2,
#          SFC3_Palatal.diff = (SFC3_T.Palatal - SFC3_A.Palatal)^2,
#          SFC4_Palatal.diff = (SFC4_T.Palatal - SFC4_A.Palatal)^2,
#          S1C1_Pharyngeal.diff = (S1C1_T.Pharyngeal - S1C1_A.Pharyngeal)^2,
#          S1C2_Pharyngeal.diff = (S1C2_T.Pharyngeal - S1C2_A.Pharyngeal)^2,
#          S1C3_Pharyngeal.diff = (S1C3_T.Pharyngeal - S1C3_A.Pharyngeal)^2,
#          S1C4_Pharyngeal.diff = (S1C4_T.Pharyngeal - S1C4_A.Pharyngeal)^2,
#          S2C1_Pharyngeal.diff = (S2C1_T.Pharyngeal - S2C1_A.Pharyngeal)^2,
#          S2C2_Pharyngeal.diff = (S2C2_T.Pharyngeal - S2C2_A.Pharyngeal)^2,
#          S2C3_Pharyngeal.diff = (S2C3_T.Pharyngeal - S2C3_A.Pharyngeal)^2,
#          S2C4_Pharyngeal.diff = (S2C4_T.Pharyngeal - S2C4_A.Pharyngeal)^2,
#          S3C1_Pharyngeal.diff = (S3C1_T.Pharyngeal - S3C1_A.Pharyngeal)^2,
#          S3C2_Pharyngeal.diff = (S3C2_T.Pharyngeal - S3C2_A.Pharyngeal)^2,
#          S3C3_Pharyngeal.diff = (S3C3_T.Pharyngeal - S3C3_A.Pharyngeal)^2,
#          S3C4_Pharyngeal.diff = (S3C4_T.Pharyngeal - S3C4_A.Pharyngeal)^2,
#          S4C1_Pharyngeal.diff = (S4C1_T.Pharyngeal - S4C1_A.Pharyngeal)^2,
#          S4C2_Pharyngeal.diff = (S4C2_T.Pharyngeal - S4C2_A.Pharyngeal)^2,
#          S4C3_Pharyngeal.diff = (S4C3_T.Pharyngeal - S4C3_A.Pharyngeal)^2,
#          S4C4_Pharyngeal.diff = (S4C4_T.Pharyngeal - S4C4_A.Pharyngeal)^2,
#          S5C1_Pharyngeal.diff = (S5C1_T.Pharyngeal - S5C1_A.Pharyngeal)^2,
#          S5C2_Pharyngeal.diff = (S5C2_T.Pharyngeal - S5C2_A.Pharyngeal)^2,
#          S5C3_Pharyngeal.diff = (S5C3_T.Pharyngeal - S5C3_A.Pharyngeal)^2,
#          S5C4_Pharyngeal.diff = (S5C4_T.Pharyngeal - S5C4_A.Pharyngeal)^2,
#          S6C1_Pharyngeal.diff = (S6C1_T.Pharyngeal - S6C1_A.Pharyngeal)^2,
#          S6C2_Pharyngeal.diff = (S6C2_T.Pharyngeal - S6C2_A.Pharyngeal)^2,
#          S6C3_Pharyngeal.diff = (S6C3_T.Pharyngeal - S6C3_A.Pharyngeal)^2,
#          S6C4_Pharyngeal.diff = (S6C4_T.Pharyngeal - S6C4_A.Pharyngeal)^2,
#          SFC1_Pharyngeal.diff = (SFC1_T.Pharyngeal - SFC1_A.Pharyngeal)^2,
#          SFC2_Pharyngeal.diff = (SFC2_T.Pharyngeal - SFC2_A.Pharyngeal)^2,
#          SFC3_Pharyngeal.diff = (SFC3_T.Pharyngeal - SFC3_A.Pharyngeal)^2,
#          SFC4_Pharyngeal.diff = (SFC4_T.Pharyngeal - SFC4_A.Pharyngeal)^2,
#          S1C1_Round.diff = (S1C1_T.Round - S1C1_A.Round)^2,
#          S1C2_Round.diff = (S1C2_T.Round - S1C2_A.Round)^2,
#          S1C3_Round.diff = (S1C3_T.Round - S1C3_A.Round)^2,
#          S1C4_Round.diff = (S1C4_T.Round - S1C4_A.Round)^2,
#          S2C1_Round.diff = (S2C1_T.Round - S2C1_A.Round)^2,
#          S2C2_Round.diff = (S2C2_T.Round - S2C2_A.Round)^2,
#          S2C3_Round.diff = (S2C3_T.Round - S2C3_A.Round)^2,
#          S2C4_Round.diff = (S2C4_T.Round - S2C4_A.Round)^2,
#          S3C1_Round.diff = (S3C1_T.Round - S3C1_A.Round)^2,
#          S3C2_Round.diff = (S3C2_T.Round - S3C2_A.Round)^2,
#          S3C3_Round.diff = (S3C3_T.Round - S3C3_A.Round)^2,
#          S3C4_Round.diff = (S3C4_T.Round - S3C4_A.Round)^2,
#          S4C1_Round.diff = (S4C1_T.Round - S4C1_A.Round)^2,
#          S4C2_Round.diff = (S4C2_T.Round - S4C2_A.Round)^2,
#          S4C3_Round.diff = (S4C3_T.Round - S4C3_A.Round)^2,
#          S4C4_Round.diff = (S4C4_T.Round - S4C4_A.Round)^2,
#          S5C1_Round.diff = (S5C1_T.Round - S5C1_A.Round)^2,
#          S5C2_Round.diff = (S5C2_T.Round - S5C2_A.Round)^2,
#          S5C3_Round.diff = (S5C3_T.Round - S5C3_A.Round)^2,
#          S5C4_Round.diff = (S5C4_T.Round - S5C4_A.Round)^2,
#          S6C1_Round.diff = (S6C1_T.Round - S6C1_A.Round)^2,
#          S6C2_Round.diff = (S6C2_T.Round - S6C2_A.Round)^2,
#          S6C3_Round.diff = (S6C3_T.Round - S6C3_A.Round)^2,
#          S6C4_Round.diff = (S6C4_T.Round - S6C4_A.Round)^2,
#          SFC1_Round.diff = (SFC1_T.Round - SFC1_A.Round)^2,
#          SFC2_Round.diff = (SFC2_T.Round - SFC2_A.Round)^2,
#          SFC3_Round.diff = (SFC3_T.Round - SFC3_A.Round)^2,
#          SFC4_Round.diff = (SFC4_T.Round - SFC4_A.Round)^2,
#          S1C1_Tongue.diff = (S1C1_T.Tongue - S1C1_A.Tongue)^2,
#          S1C2_Tongue.diff = (S1C2_T.Tongue - S1C2_A.Tongue)^2,
#          S1C3_Tongue.diff = (S1C3_T.Tongue - S1C3_A.Tongue)^2,
#          S1C4_Tongue.diff = (S1C4_T.Tongue - S1C4_A.Tongue)^2,
#          S2C1_Tongue.diff = (S2C1_T.Tongue - S2C1_A.Tongue)^2,
#          S2C2_Tongue.diff = (S2C2_T.Tongue - S2C2_A.Tongue)^2,
#          S2C3_Tongue.diff = (S2C3_T.Tongue - S2C3_A.Tongue)^2,
#          S2C4_Tongue.diff = (S2C4_T.Tongue - S2C4_A.Tongue)^2,
#          S3C1_Tongue.diff = (S3C1_T.Tongue - S3C1_A.Tongue)^2,
#          S3C2_Tongue.diff = (S3C2_T.Tongue - S3C2_A.Tongue)^2,
#          S3C3_Tongue.diff = (S3C3_T.Tongue - S3C3_A.Tongue)^2,
#          S3C4_Tongue.diff = (S3C4_T.Tongue - S3C4_A.Tongue)^2,
#          S4C1_Tongue.diff = (S4C1_T.Tongue - S4C1_A.Tongue)^2,
#          S4C2_Tongue.diff = (S4C2_T.Tongue - S4C2_A.Tongue)^2,
#          S4C3_Tongue.diff = (S4C3_T.Tongue - S4C3_A.Tongue)^2,
#          S4C4_Tongue.diff = (S4C4_T.Tongue - S4C4_A.Tongue)^2,
#          S5C1_Tongue.diff = (S5C1_T.Tongue - S5C1_A.Tongue)^2,
#          S5C2_Tongue.diff = (S5C2_T.Tongue - S5C2_A.Tongue)^2,
#          S5C3_Tongue.diff = (S5C3_T.Tongue - S5C3_A.Tongue)^2,
#          S5C4_Tongue.diff = (S5C4_T.Tongue - S5C4_A.Tongue)^2,
#          S6C1_Tongue.diff = (S6C1_T.Tongue - S6C1_A.Tongue)^2,
#          S6C2_Tongue.diff = (S6C2_T.Tongue - S6C2_A.Tongue)^2,
#          S6C3_Tongue.diff = (S6C3_T.Tongue - S6C3_A.Tongue)^2,
#          S6C4_Tongue.diff = (S6C4_T.Tongue - S6C4_A.Tongue)^2,
#          SFC1_Tongue.diff = (SFC1_T.Tongue - SFC1_A.Tongue)^2,
#          SFC2_Tongue.diff = (SFC2_T.Tongue - SFC2_A.Tongue)^2,
#          SFC3_Tongue.diff = (SFC3_T.Tongue - SFC3_A.Tongue)^2,
#          SFC4_Tongue.diff = (SFC4_T.Tongue - SFC4_A.Tongue)^2,
#          S1C1_Radical.diff = (S1C1_T.Radical - S1C1_A.Radical)^2,
#          S1C2_Radical.diff = (S1C2_T.Radical - S1C2_A.Radical)^2,
#          S1C3_Radical.diff = (S1C3_T.Radical - S1C3_A.Radical)^2,
#          S1C4_Radical.diff = (S1C4_T.Radical - S1C4_A.Radical)^2,
#          S2C1_Radical.diff = (S2C1_T.Radical - S2C1_A.Radical)^2,
#          S2C2_Radical.diff = (S2C2_T.Radical - S2C2_A.Radical)^2,
#          S2C3_Radical.diff = (S2C3_T.Radical - S2C3_A.Radical)^2,
#          S2C4_Radical.diff = (S2C4_T.Radical - S2C4_A.Radical)^2,
#          S3C1_Radical.diff = (S3C1_T.Radical - S3C1_A.Radical)^2,
#          S3C2_Radical.diff = (S3C2_T.Radical - S3C2_A.Radical)^2,
#          S3C3_Radical.diff = (S3C3_T.Radical - S3C3_A.Radical)^2,
#          S3C4_Radical.diff = (S3C4_T.Radical - S3C4_A.Radical)^2,
#          S4C1_Radical.diff = (S4C1_T.Radical - S4C1_A.Radical)^2,
#          S4C2_Radical.diff = (S4C2_T.Radical - S4C2_A.Radical)^2,
#          S4C3_Radical.diff = (S4C3_T.Radical - S4C3_A.Radical)^2,
#          S4C4_Radical.diff = (S4C4_T.Radical - S4C4_A.Radical)^2,
#          S5C1_Radical.diff = (S5C1_T.Radical - S5C1_A.Radical)^2,
#          S5C2_Radical.diff = (S5C2_T.Radical - S5C2_A.Radical)^2,
#          S5C3_Radical.diff = (S5C3_T.Radical - S5C3_A.Radical)^2,
#          S5C4_Radical.diff = (S5C4_T.Radical - S5C4_A.Radical)^2,
#          S6C1_Radical.diff = (S6C1_T.Radical - S6C1_A.Radical)^2,
#          S6C2_Radical.diff = (S6C2_T.Radical - S6C2_A.Radical)^2,
#          S6C3_Radical.diff = (S6C3_T.Radical - S6C3_A.Radical)^2,
#          S6C4_Radical.diff = (S6C4_T.Radical - S6C4_A.Radical)^2,
#          SFC1_Radical.diff = (SFC1_T.Radical - SFC1_A.Radical)^2,
#          SFC2_Radical.diff = (SFC2_T.Radical - SFC2_A.Radical)^2,
#          SFC3_Radical.diff = (SFC3_T.Radical - SFC3_A.Radical)^2,
#          SFC4_Radical.diff = (SFC4_T.Radical - SFC4_A.Radical)^2,
#         ) %>%
#   dplyr::select(linenumber, Speaker, Session, Gloss, 
#                 IPAtarget, IPAactual, 
#                 Targetphon, Actualphon, 
#                 TargetCV, ActualCV, 
#                 TargetCV_edited, ActualCV_edited, 
#                 contains(".diff")) %>%
#   mutate(S1C1.distance = (sqrt(S1C1_Sonorant.diff + S1C1_Consonantal.diff + S1C1_Voice.diff + S1C1_Nasal.diff + S1C1_Degree.diff + S1C1_Labial.diff +
#                                  S1C1_Palatal.diff + S1C1_Pharyngeal.diff + S1C1_Round.diff + S1C1_Tongue.diff + S1C1_Radical.diff)),
#          S1C2.distance = (sqrt(S1C2_Sonorant.diff + S1C2_Consonantal.diff + S1C2_Voice.diff + S1C2_Nasal.diff + S1C2_Degree.diff + S1C2_Labial.diff +
#                                 S1C2_Palatal.diff + S1C2_Pharyngeal.diff + S1C2_Round.diff + S1C2_Tongue.diff + S1C2_Radical.diff)),
#          S1C3.distance = (sqrt(S1C3_Sonorant.diff + S1C3_Consonantal.diff + S1C3_Voice.diff + S1C3_Nasal.diff + S1C3_Degree.diff + S1C3_Labial.diff +
#                                 S1C3_Palatal.diff + S1C3_Pharyngeal.diff + S1C3_Round.diff + S1C3_Tongue.diff + S1C3_Radical.diff)),
#          S1C4.distance = (sqrt(S1C4_Sonorant.diff + S1C4_Consonantal.diff + S1C4_Voice.diff + S1C4_Nasal.diff + S1C4_Degree.diff + S1C4_Labial.diff +
#                                  S1C4_Palatal.diff + S1C4_Pharyngeal.diff + S1C4_Round.diff + S1C4_Tongue.diff + S1C4_Radical.diff)),
#          S2C1.distance = (sqrt(S2C1_Sonorant.diff + S2C1_Consonantal.diff + S2C1_Voice.diff + S2C1_Nasal.diff + S2C1_Degree.diff + S2C1_Labial.diff +
#                                  S2C1_Palatal.diff + S2C1_Pharyngeal.diff + S2C1_Round.diff + S2C1_Tongue.diff + S2C1_Radical.diff)),
#          S2C2.distance = (sqrt(S2C2_Sonorant.diff + S2C2_Consonantal.diff + S2C2_Voice.diff + S2C2_Nasal.diff + S2C2_Degree.diff + S2C2_Labial.diff +
#                                 S2C2_Palatal.diff + S2C2_Pharyngeal.diff + S2C2_Round.diff + S2C2_Tongue.diff + S2C2_Radical.diff)),
#          S2C3.distance = (sqrt(S2C3_Sonorant.diff + S2C3_Consonantal.diff + S2C3_Voice.diff + S2C3_Nasal.diff + S2C3_Degree.diff + S2C3_Labial.diff +
#                                 S2C3_Palatal.diff + S2C3_Pharyngeal.diff + S2C3_Round.diff + S2C3_Tongue.diff + S2C3_Radical.diff)),
#          S2C4.distance = (sqrt(S2C4_Sonorant.diff + S2C4_Consonantal.diff + S2C4_Voice.diff + S2C4_Nasal.diff + S2C4_Degree.diff + S2C4_Labial.diff +
#                                  S2C4_Palatal.diff + S2C4_Pharyngeal.diff + S2C4_Round.diff + S2C4_Tongue.diff + S2C4_Radical.diff)),
#          S3C1.distance = (sqrt(S3C1_Sonorant.diff + S3C1_Consonantal.diff + S3C1_Voice.diff + S3C1_Nasal.diff + S3C1_Degree.diff + S3C1_Labial.diff +
#                                  S3C1_Palatal.diff + S3C1_Pharyngeal.diff + S3C1_Round.diff + S3C1_Tongue.diff + S3C1_Radical.diff)),
#          S3C2.distance = (sqrt(S3C2_Sonorant.diff + S3C2_Consonantal.diff + S3C2_Voice.diff + S3C2_Nasal.diff + S3C2_Degree.diff + S3C2_Labial.diff +
#                                 S3C2_Palatal.diff + S3C2_Pharyngeal.diff + S3C2_Round.diff + S3C2_Tongue.diff + S3C2_Radical.diff)),
#          S3C3.distance = (sqrt(S3C3_Sonorant.diff + S3C3_Consonantal.diff + S3C3_Voice.diff + S3C3_Nasal.diff + S3C3_Degree.diff + S3C3_Labial.diff +
#                                 S3C3_Palatal.diff + S3C3_Pharyngeal.diff + S3C3_Round.diff + S3C3_Tongue.diff + S3C3_Radical.diff)),
#          S3C4.distance = (sqrt(S3C4_Sonorant.diff + S3C4_Consonantal.diff + S3C4_Voice.diff + S3C4_Nasal.diff + S3C4_Degree.diff + S3C4_Labial.diff +
#                                  S3C4_Palatal.diff + S3C4_Pharyngeal.diff + S3C4_Round.diff + S3C4_Tongue.diff + S3C4_Radical.diff)),
#          S4C1.distance = (sqrt(S4C1_Sonorant.diff + S4C1_Consonantal.diff + S4C1_Voice.diff + S4C1_Nasal.diff + S4C1_Degree.diff + S4C1_Labial.diff +
#                                  S4C1_Palatal.diff + S4C1_Pharyngeal.diff + S4C1_Round.diff + S4C1_Tongue.diff + S4C1_Radical.diff)),
#          S4C2.distance = (sqrt(S4C2_Sonorant.diff + S4C2_Consonantal.diff + S4C2_Voice.diff + S4C2_Nasal.diff + S4C2_Degree.diff + S4C2_Labial.diff +
#                                 S4C2_Palatal.diff + S4C2_Pharyngeal.diff + S4C2_Round.diff + S4C2_Tongue.diff + S4C2_Radical.diff)),
#          S4C3.distance = (sqrt(S4C3_Sonorant.diff + S4C3_Consonantal.diff + S4C3_Voice.diff + S4C3_Nasal.diff + S4C3_Degree.diff + S4C3_Labial.diff +
#                                 S4C3_Palatal.diff + S4C3_Pharyngeal.diff + S4C3_Round.diff + S4C3_Tongue.diff + S4C3_Radical.diff)),
#          S4C4.distance = (sqrt(S4C4_Sonorant.diff + S4C4_Consonantal.diff + S4C4_Voice.diff + S4C4_Nasal.diff + S4C4_Degree.diff + S4C4_Labial.diff +
#                                  S4C4_Palatal.diff + S4C4_Pharyngeal.diff + S4C4_Round.diff + S4C4_Tongue.diff + S4C4_Radical.diff)),
#          S5C1.distance = (sqrt(S5C1_Sonorant.diff + S5C1_Consonantal.diff + S5C1_Voice.diff + S5C1_Nasal.diff + S5C1_Degree.diff + S5C1_Labial.diff +
#                                  S5C1_Palatal.diff + S5C1_Pharyngeal.diff + S5C1_Round.diff + S5C1_Tongue.diff + S5C1_Radical.diff)),
#          S5C2.distance = (sqrt(S5C2_Sonorant.diff + S5C2_Consonantal.diff + S5C2_Voice.diff + S5C2_Nasal.diff + S5C2_Degree.diff + S5C2_Labial.diff +
#                                  S5C2_Palatal.diff + S5C2_Pharyngeal.diff + S5C2_Round.diff + S5C2_Tongue.diff + S5C2_Radical.diff)),
#          S5C3.distance = (sqrt(S5C3_Sonorant.diff + S5C3_Consonantal.diff + S5C3_Voice.diff + S5C3_Nasal.diff + S5C3_Degree.diff + S5C3_Labial.diff +
#                                  S5C3_Palatal.diff + S5C3_Pharyngeal.diff + S5C3_Round.diff + S5C3_Tongue.diff + S5C3_Radical.diff)),
#          S5C4.distance = (sqrt(S5C4_Sonorant.diff + S5C4_Consonantal.diff + S5C4_Voice.diff + S5C4_Nasal.diff + S5C4_Degree.diff + S5C4_Labial.diff +
#                                  S5C4_Palatal.diff + S5C4_Pharyngeal.diff + S5C4_Round.diff + S5C4_Tongue.diff + S5C4_Radical.diff)),
#          S6C1.distance = (sqrt(S6C1_Sonorant.diff + S6C1_Consonantal.diff + S6C1_Voice.diff + S6C1_Nasal.diff + S6C1_Degree.diff + S6C1_Labial.diff +
#                                  S6C1_Palatal.diff + S6C1_Pharyngeal.diff + S6C1_Round.diff + S6C1_Tongue.diff + S6C1_Radical.diff)),
#          S6C2.distance = (sqrt(S6C2_Sonorant.diff + S6C2_Consonantal.diff + S6C2_Voice.diff + S6C2_Nasal.diff + S6C2_Degree.diff + S6C2_Labial.diff +
#                                  S6C2_Palatal.diff + S6C2_Pharyngeal.diff + S6C2_Round.diff + S6C2_Tongue.diff + S6C2_Radical.diff)),
#          S6C3.distance = (sqrt(S6C3_Sonorant.diff + S6C3_Consonantal.diff + S6C3_Voice.diff + S6C3_Nasal.diff + S6C3_Degree.diff + S6C3_Labial.diff +
#                                  S6C3_Palatal.diff + S6C3_Pharyngeal.diff + S6C3_Round.diff + S6C3_Tongue.diff + S6C3_Radical.diff)),
#          S6C4.distance = (sqrt(S6C4_Sonorant.diff + S6C4_Consonantal.diff + S6C4_Voice.diff + S6C4_Nasal.diff + S6C4_Degree.diff + S6C4_Labial.diff +
#                                  S6C4_Palatal.diff + S6C4_Pharyngeal.diff + S6C4_Round.diff + S6C4_Tongue.diff + S6C4_Radical.diff)),
#          SFC1.distance = (sqrt(SFC1_Sonorant.diff + SFC1_Consonantal.diff + SFC1_Voice.diff + SFC1_Nasal.diff + SFC1_Degree.diff + SFC1_Labial.diff +
#                                  SFC1_Palatal.diff + SFC1_Pharyngeal.diff + SFC1_Round.diff + SFC1_Tongue.diff + SFC1_Radical.diff)),
#          SFC2.distance = (sqrt(SFC2_Sonorant.diff + SFC2_Consonantal.diff + SFC2_Voice.diff + SFC2_Nasal.diff + SFC2_Degree.diff + SFC2_Labial.diff +
#                                 SFC2_Palatal.diff + SFC2_Pharyngeal.diff + SFC2_Round.diff + SFC2_Tongue.diff + SFC2_Radical.diff)),
#          SFC3.distance = (sqrt(SFC3_Sonorant.diff + SFC3_Consonantal.diff + SFC3_Voice.diff + SFC3_Nasal.diff + SFC3_Degree.diff + SFC3_Labial.diff +
#                                 SFC3_Palatal.diff + SFC3_Pharyngeal.diff + SFC3_Round.diff + SFC3_Tongue.diff + SFC3_Radical.diff)),
#          SFC4.distance = (sqrt(SFC4_Sonorant.diff + SFC4_Consonantal.diff + SFC4_Voice.diff + SFC4_Nasal.diff + SFC4_Degree.diff + SFC4_Labial.diff +
#                                  SFC4_Palatal.diff + SFC4_Pharyngeal.diff + SFC4_Round.diff + SFC4_Tongue.diff + SFC4_Radical.diff)),
#     distance = ifelse(is.na(S1C1.distance), 0, S1C1.distance) + 
#                   ifelse(is.na(S1C2.distance), 0, S1C2.distance) +
#          ifelse(is.na(S1C3.distance), 0, S1C3.distance) +
#          ifelse(is.na(S1C4.distance), 0, S1C4.distance) +
#          ifelse(is.na(S2C1.distance), 0, S2C1.distance) +
#          ifelse(is.na(S2C2.distance), 0, S2C2.distance) +
#          ifelse(is.na(S2C3.distance), 0, S2C3.distance) +
#          ifelse(is.na(S2C4.distance), 0, S2C4.distance) +
#          ifelse(is.na(S3C1.distance), 0, S3C1.distance) +
#          ifelse(is.na(S3C2.distance), 0, S3C2.distance) +
#          ifelse(is.na(S3C3.distance), 0, S3C3.distance) +
#          ifelse(is.na(S3C4.distance), 0, S3C4.distance) +
#          ifelse(is.na(S4C1.distance), 0, S4C1.distance) +
#          ifelse(is.na(S4C2.distance), 0, S4C2.distance) +
#          ifelse(is.na(S4C3.distance), 0, S4C3.distance) +
#          ifelse(is.na(S5C4.distance), 0, S5C4.distance) +
#          ifelse(is.na(S5C1.distance), 0, S5C1.distance) +
#          ifelse(is.na(S5C2.distance), 0, S5C2.distance) +
#          ifelse(is.na(S5C3.distance), 0, S5C3.distance) +
#          ifelse(is.na(S5C4.distance), 0, S5C4.distance) +
#          ifelse(is.na(S6C1.distance), 0, S6C1.distance) +
#          ifelse(is.na(S6C2.distance), 0, S6C2.distance) +
#          ifelse(is.na(S6C3.distance), 0, S6C3.distance) +
#          ifelse(is.na(S6C4.distance), 0, S6C4.distance) +
#          ifelse(is.na(SFC1.distance), 0, SFC1.distance) +
#          ifelse(is.na(SFC2.distance), 0, SFC2.distance) +
#          ifelse(is.na(SFC3.distance), 0, SFC3.distance) +
#          ifelse(is.na(SFC4.distance), 0, SFC4.distance)
#         ) %>% 
#   dplyr::select(linenumber, Speaker, Session, Gloss, 
#                 IPAtarget, IPAactual,
#                 Targetphon, Actualphon,
#                 TargetCV, ActualCV, 
#                 TargetCV_edited, ActualCV_edited, 
#                 distance)
# 
# # Check distances that are 0 but structures differ
# 
# # check_data <- comparison_data_phondist
# # 
# # check_data$TargetCV <- gsub("VVV", "V", check_data$TargetCV)
# # check_data$ActualCV <- gsub("VVV", "V", check_data$ActualCV)
# # check_data$TargetCV <- gsub("VV", "V", check_data$TargetCV)
# # check_data$ActualCV <- gsub("VV", "V", check_data$ActualCV)
# # 
# # check_data <- check_data %>%  mutate(TargetCV_edited = as.character(TargetCV_edited),
# #           ActualCV = as.character(ActualCV_edited),
# #          check = ifelse((TargetCV_edited != ActualCV_edited) & (distance == 0), T, F)) %>%
# #            filter(check == T) %>%
# #   #filter(TargetCV != "CV" & ActualCV != "CV" & TargetCV != "GV" & ActualCV != "GV") %>%
# #   dplyr::select(1:8, contains("distance"))
# 
# # All these mismatches are due to vowel differences, not consonant differences. I.e. consonants are the same in target and actual form.

# Session_data ------------------------------------------------------------

# Convert session info into age in months

comparison_data$years <- stri_sub(comparison_data$Session, 1, 2)
comparison_data$months <- stri_sub(comparison_data$Session, 3, 4)
comparison_data$days <- stri_sub(comparison_data$Session, 5, 6)

comparison_data <- comparison_data %>%
  mutate(years = as.numeric(years),
         months = as.numeric(months),
         days = as.numeric(days),
         age = (years*12) + months) %>%
  dplyr::select(-years, -months, -days) 

session_data <- comparison_data %>% group_by(Speaker, age) %>%
  tally() %>%
  filter(n > 1) %>%
  dplyr::select(Speaker, age) %>%
  group_by(Speaker, age) %>% 
  tally() %>%
  mutate(session_ordinal = row_number()) %>%
  dplyr::select(-n)

comparison_data <- comparison_data %>%
  left_join(session_data) %>%
  filter(!is.na(session_ordinal)) %>%
  mutate(session_ordinal = as.numeric(session_ordinal)) 

write_csv(comparison_data, "Data/large_files/comparison_data.csv")

# generate data for global matrix

distance_full_df <- as.data.frame(output_full)
colnames(distance_full_df)[1] <- "unique"
colnames(distance_full_df)[14] <- "data"

distance_full <- distance_full_df %>% dplyr::select(unique, -ends_with("data_type") & -ends_with(".ID")) %>%
  rename("ID" = "unique",
         "data_type" = "data") %>%
  left_join(comparison_data) %>%
  feather::write_feather("Data/large_files/distance_full.feather")



# Convert session info into age in months for comparison_data_phondist

# comparison_data_phondist$years <- stri_sub(comparison_data_phondist$Session, 1, 2)
# comparison_data_phondist$months <- stri_sub(comparison_data_phondist$Session, 3, 4)
# comparison_data_phondist$days <- stri_sub(comparison_data_phondist$Session, 5, 6)
# 
# comparison_data_phondist <- comparison_data_phondist %>%
#   mutate(years = as.numeric(years),
#          months = as.numeric(months),
#          days = as.numeric(days),
#          age = (years*12) + months) %>%
#   dplyr::select(-years, -months, -days) 
# 
# session_data_phondist <- comparison_data_phondist %>% group_by(Speaker, age) %>%
#   tally() %>%
#   filter(n > 1) %>%
#   dplyr::select(Speaker, age) %>%
#   group_by(Speaker, age) %>% 
#   tally() %>%
#   mutate(session_ordinal = row_number()) %>%
#   dplyr::select(-n)
# 
# comparison_data_phondist <- comparison_data_phondist %>%
#   left_join(session_data_phondist) %>%
#   filter(!is.na(session_ordinal)) %>%
#   mutate(session_ordinal = as.numeric(session_ordinal)) 
# 
# write_csv(comparison_data_phondist, "Data/large_files/comparison_data_phondist.csv")
