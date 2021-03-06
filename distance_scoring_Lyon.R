# Updated 30th March 2021
# This data takes the sample generated in data_cleaning.R and creates a series of phonetic distance values for each word in the dataframe

FULLsample_Lyon <- feather::read_feather("Data/FULLsample_Lyon.feather")


FULLsample_Lyon$Session <- gsub("^[^.]*.", "", FULLsample_Lyon$Session) # create variable to show session number in only numeric form
FULLsample_Lyon$Session <- gsub('[\a\b]', '', FULLsample_Lyon$Session)

# write_csv(FULLsample_Lyon, "ProvidenceDataCHI.csv")

sample_IPAtarget_Lyon <- FULLsample_Lyon %>% select(ID, Speaker, Session, Gloss, 
                                          IPAtarget, IPAactual, 
                                          Targetphon, Actualphon, 
                                          TargetCV, ActualCV) # Create new dataframe to generate IPA segmental values

#paste0(unique(unlist(strsplit(sample_IPAtarget_Lyon$IPAtarget, ''))), collapse = "") # find all IPA vowels

# m e a ɑ k u ʁ ə ɡ d n ɔ l w ɛ o t j b i ø s p v ʃ ʒ  y ɥ  œ f ŋ r ʀ z ʤ ʧ ɲ ç

# substitute all target vowels for generic V because I don't care about vowels
sample_IPAtarget_Lyon$Vremoved_target <- gsub("([
e 
a
ɑ
u
ə
ɔ
ɛ
o
i
ø
y
ɥ
œ])", "V", sample_IPAtarget_Lyon$IPAtarget)    # vowels taken from runnng Phone Inventory script in Phon

sample_IPAtarget_Lyon$Vremoved_target <- gsub("VVV", "V", sample_IPAtarget_Lyon$Vremoved_target)  # remove triphthongs to count as single vowel (following Monaghan et al 2010 but also because we're not looking at vowels here)
sample_IPAtarget_Lyon$Vremoved_target <- gsub("VV", "V", sample_IPAtarget_Lyon$Vremoved_target)  # remove diphthongs to count as single vowel (following Monaghan et al 2010 but also because we're not looking at vowels here)
sample_IPAtarget_Lyon <- sample_IPAtarget_Lyon %>% mutate(nsyl_target = stringr::str_count(Vremoved_target, "V"),
                                                nsyl_target = ifelse(nsyl_target == 0, 1, nsyl_target))

#paste0(unique(unlist(strsplit(sample_IPAtarget_Lyon$IPAactual, ''))), collapse = "") # find all IPA vowels

# m ø e a y ʌ w h ʀ ɛ v k b o t n d l ɥ u ɡ i j ɔ ɲ p s ɑ ŋ f ʒ r z ɪ ʃ ɣ ə c ʔ V ɱ g x

# substitute all actual vowels for generic V because I don't care about vowels here either

sample_IPAtarget_Lyon$Vremoved_actual <- gsub("([ø e a y ʌ ɛ o ɥ u i ɔ ɑ ɪ ə])", "V", sample_IPAtarget_Lyon$IPAactual)    # vowels taken from runnng Phone Inventory script in Phon

sample_IPAtarget_Lyon$Vremoved_actual <- gsub("VVV", "V", sample_IPAtarget_Lyon$Vremoved_actual)  
sample_IPAtarget_Lyon$Vremoved_actual <- gsub("VV", "V", sample_IPAtarget_Lyon$Vremoved_actual)  
sample_IPAtarget_Lyon <- sample_IPAtarget_Lyon %>% mutate(nsyl_actual = stringr::str_count(Vremoved_actual, "V"),
                                                nsyl_actual = ifelse(nsyl_actual == 0, 1, nsyl_actual))


# Now split data by syllable structures since difference structures need treating differently when running a segment-by-segment comparison
# Create a new dataframe to gather this info, to be joined to sample_IPAtarget_Lyonlater

target_structures_sample <- as.data.frame(levels(sample_IPAtarget_Lyon$TargetCV)) # list all structures in the data

target_structures_sample <- target_structures_sample %>%
  rename("TargetCV" = `levels(sample_IPAtarget_Lyon$TargetCV)`)


# Create a new column that simplifies each structure by its 'core' syllabic properties

target_structures_sample$TargetCV_edited <- gsub("ː", "", target_structures_sample$TargetCV)
target_structures_sample$TargetCV_edited <- gsub("VVVV", "V", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("VVV", "V", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("VV", "V", target_structures_sample$TargetCV_edited)
target_structures_sample$TargetCV_edited <- gsub("[(G g)]", "C", target_structures_sample$TargetCV_edited)  # counting glides as consonants, consistent with above
target_structures_sample$TargetCV_edited <- gsub("CCCC", "C", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("CCC", "C", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("CC", "C", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("^", "", target_structures_sample$TargetCV_edited)
#target_structures_sample$TargetCV_edited <- gsub("(..)", "", target_structures_sample$TargetCV_edited)



target_structures_sample <- target_structures_sample %>%
  mutate(TargetCV_edited = as.factor(TargetCV_edited))

# Do the same for actual syllabic structure. This will allow for comparison of targetlikeness later on

actual_structures_sample <- as.data.frame(levels(sample_IPAtarget_Lyon$ActualCV)) # list all structures in the data

actual_structures_sample <- actual_structures_sample %>%
  rename("ActualCV" = `levels(sample_IPAtarget_Lyon$ActualCV)`)


# Create a new column that simplifies each structure by its 'core' syllabic properties

actual_structures_sample$ActualCV_edited <- gsub("ː", "", actual_structures_sample$ActualCV)
actual_structures_sample$ActualCV_edited <- gsub("VVVV", "V", actual_structures_sample$ActualCV_edited)  
actual_structures_sample$ActualCV_edited <- gsub("VVV", "V", actual_structures_sample$ActualCV_edited)  
actual_structures_sample$ActualCV_edited <- gsub("VV", "V", actual_structures_sample$ActualCV_edited)
actual_structures_sample$ActualCV_edited <- gsub("[(G g)]", "C", actual_structures_sample$ActualCV_edited)  # counting glides as consonants, consistent with above
actual_structures_sample$ActualCV_edited <- gsub("CCCC", "C", actual_structures_sample$ActualCV_edited)  
actual_structures_sample$ActualCV_edited <- gsub("CCC", "C", actual_structures_sample$ActualCV_edited)  
actual_structures_sample$ActualCV_edited <- gsub("CC", "C", actual_structures_sample$ActualCV_edited)  
actual_structures_sample$ActualCV_edited <- gsub("^", "", actual_structures_sample$ActualCV_edited)


actual_structures_sample <- actual_structures_sample %>%
  mutate(ActualCV_edited = as.factor(ActualCV_edited))


# create two new columns that alongside sample_IPAtarget$nsyl_target together will allow for filtering of specific word structures across the data

#levels(sample_IPAtarget_Lyon$TargetCV_edited)

# openclosed = does the target form have a coda? (yes = closed, no = open)
# onset = is it a vowel (V) or consonant (C) at word onset?

# target_structures_sample <- target_structures_sample %>% 
#   mutate(openclosed = ifelse(TargetCV_edited %in% c("C", "CVC", "VC", "CVCVC", "VCVC", "CVCVC^CVC", "CVCVCVC", "VCVCVC", 
#                                                      "CVCVCVCVC", "VCVCVCVC", "CVCVCVCVCVC", "VCVCVCVCVC", "CVCVCVCVCVCVC"), "closed", "open"),
#          onset = ifelse(TargetCV_edited %in% c("V", "V^CV", "VC", "VCV", "VCVC", "VCVCV", "VCVCVC", "VCVCVCV", "VCVCVCVC", 
#                                                 "VCVCVCVCV", "VCVCVCVCVC", "VCVCVCVCVCV"), "V", "C"))

sample_IPAtarget_Lyon <- sample_IPAtarget_Lyon %>% left_join(target_structures_sample) %>%
  left_join(actual_structures_sample)  # join with main dataframe

# Now each segment of each word needs to be separated in order to compare target forms with actual productions
# This process is done by syllable number, starting with target forms and then considering actual forms in relation to these

# For example: monosyllabic target /kat/ is separated into /k/~/a/~/t/ and then child's actual production is considered in relation to this
# Actual production might be a monosyllable ([kat]), or a disyllable [kaka] or multisyllabic [kakaka]. In each of these cases, /k/~/a/~/t/ as generated below
# is compared against the segments from the actual form 
# First all target monosyllables are compared with all target forms (from 1-6 syllables, V- and C-intial separately)
# Then disyllables (compared with 1-6 syllable forms, V- and C-initial), trisyllables, etc. up to 6-syllable words
# Words beyond 6 syllables tended to be produced with vocal play, and so were excluded from the analysis

nsyl_target_list <- sample_IPAtarget_Lyon %>%
  split(., f = .$nsyl_target)

sample_IPAtarget_loop <- lapply(nsyl_target_list, FUN = function(element) {
  split_syl <- element %>% separate(Vremoved_target, c("S1C1_target", "S2C1_target", "S3C1_target", "S4C1_target", "S5C1_target", "SFC1_target"), "V") 
  split_clust <- split_syl %>% separate(S1C1_target, c("TS1C1", "TS1C2", "TS1C3", "TS1C4"), sep = "(?<=.)") %>%
    separate(S2C1_target, c("TS2C1", "TS2C2", "TS2C3", "TS2C4"), sep = "(?<=.)") %>%
    separate(S3C1_target, c("TS3C1", "TS3C2", "TS3C3", "TS3C4"), sep = "(?<=.)") %>%
    separate(S4C1_target, c("TS4C1", "TS4C2", "TS4C3", "TS4C4"), sep = "(?<=.)") %>%
    separate(S5C1_target, c("TS5C1", "TS5C2", "TS5C3", "TS5C4"), sep = "(?<=.)") %>%
    separate(SFC1_target, c("TSFC1", "TSFC2", "TSFC3", "TSFC4"), sep = "(?<=.)")
})


# Now add segmental info re infants' actual productions to each DF

Vinitial <- sample_IPAtarget_Lyon%>% filter(stringr::str_detect(ActualCV, "^V")) # DF for looking at V-intial structures only
Cinitial <- sample_IPAtarget_Lyon%>% filter(stringr::str_detect(ActualCV, "^C")|stringr::str_detect(ActualCV, "^G"))    # DF for looking at C-intial structures only

# Remember to merge these subsets together once DF is organized

sample_IPAactual_loop <- lapply(sample_IPAtarget_loop, FUN = function(element) {
  split_syl_Cinit <- element %>% filter(ActualCV %in% Cinitial$ActualCV) %>%
    separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V")
  split_clust_Cinit <- split_syl_Cinit %>% separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4"), sep = "(?<=.)") %>%
    separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4"), sep = "(?<=.)") %>%
    separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4"), sep = "(?<=.)") %>%
    separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4"), sep = "(?<=.)") %>%
    separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4"), sep = "(?<=.)") %>%
    separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4"), sep = "(?<=.)")
  split_syl_Vinit <- element %>% filter(ActualCV %in% Vinitial$ActualCV) %>%
    separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual", "S5CF_actual"), "V")
  split_clust_Vinit <- split_syl_Vinit %>% separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3", "AS1C4"), sep = "(?<=.)") %>%
    separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3", "AS2C4"), sep = "(?<=.)") %>%
    separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3", "AS3C4"), sep = "(?<=.)") %>%
    separate(S4C1_actual, c("AS4C1", "AS4C2", "AS4C3", "AS4C4"), sep = "(?<=.)") %>%
    separate(S5C1_actual, c("AS5C1", "AS5C2", "AS5C3", "AS5C4"), sep = "(?<=.)") %>%
    separate(S5CF_actual, c("ASFC1", "ASFC2", "ASFC3", "ASFC4"), sep = "(?<=.)")
  sample_IPA_CVinit <- rbind(split_clust_Vinit, split_clust_Cinit)
})

actual_target_IPA_FULL_Lyon <- do.call(rbind.data.frame, sample_IPAactual_loop)

actual_target_IPA_FULL_Lyon %>% filter(nsyl_actual > 5 & nsyl_target < 5) # check for any errors in the data

# 2 words are clearly mistranscribed (la in target, 10-syl actual); filter these out

actual_target_IPA_FULL_Lyon <- actual_target_IPA_FULL_Lyon %>% 
  mutate(filter_out = ifelse(nsyl_actual == 10 & nsyl_target == 1, T, F)) %>% 
  filter(filter_out == F) %>% dplyr::select(-filter_out)

#########

# The new DF has 127488 observations, matching the original sample

comparison_sample <- FULLsample_Lyon %>% dplyr::select(ID, Speaker, Session, Gloss, IPAtarget, IPAactual, IPAtarget, IPAactual, TargetCV, ActualCV)
comparison_final <- actual_target_IPA_FULL_Lyon %>% dplyr::select(ID, 
                                                             Speaker, 
                                                             Session, 
                                                             Gloss,
                                                             IPAtarget, 
                                                             IPAactual,
                                                             TargetCV, 
                                                             ActualCV
                                                             )

missing <- setdiff(comparison_sample, comparison_final)  # 298 items

feather::write_feather(actual_target_IPA_FULL_Lyon, "Data/actual_target_IPA_FULL_Lyon.feather")

##########


# Create dataframe to represent distinctive features of each segment

# Extra French segments:

# ʀ +1 Sonorant, +1 Consonantal, +1 Voice, -1 Nasal,  -1 Degree, -1 Labial, -1 Palatal, -1 Pharyngeal, -1 Round, 0 Tongue, -1 Radical,
# ɲ +1 Sonorant, +1 Consonantal, +1 Voice, +1 Nasal,  +1 Degree, -1 Labial, +1 Palatal, -1 Pharyngeal, -1 Round, +1 Tongue, 0 Radical,
# r  +1 Sonorant, +1 Consonantal, +1 Voice, -1 Nasal,  -1 Degree, -1 Labial, -1 Palatal, -1 Pharyngeal, 0 Round, 0 Tongue, -1 Radical,
# ɣ -0.5 Sonorant, +1 Consonantal, +1 Voice, 0 Nasal,  0 Degree, -1 Labial, -1 Palatal, -1 Pharyngeal, 0 Round, 0 Tongue, -1 Radical,
# ɱ +1 Sonorant, +1 Consonantal, +1 Voice, +1 Nasal,  1 Degree, +1 Labial, -1 Palatal, -1 Pharyngeal, -1 Round, 0 Tongue, 0 Radical,
# x -0.5 Sonorant, +1 Consonantal, -1 Voice, 0 Nasal,  0 Degree, -1 Labial, -1 Palatal, -1 Pharyngeal, 0 Round, 0 Tongue -1 Radical,
# ʁ -0.5 Sonorant, +1 Consonantal, -1 Voice, 0 Nasal,  0 Degree, -1 Labial, -1 Palatal, +1 Pharyngeal, 0 Round, 0 Tongue -1 Radical,
# ç -0.5 Sonorant, +1 Consonantal, -1 Voice, 0 Nasal,  0 Degree, -1 Labial, +1 Palatal, -1 Pharyngeal, 0 Round, 0 Tongue -1 Radical,

# All derived from Phoible


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
                                      "c", -1, 1, 0, -1, 0, -1, 1, -1, -1, -1, 0,   # infants produce /c/ in some instances, though this doesn't occur in target forms
                                      "z", -0.5, 1, 0, -1, 0, -1, 1, -1, -1, 1, 0,
                                      "h", -0.5, 1, 0, -1, 0, -1, -1, 1, -1, -1, -1,
                                      "ʃ", -0.5, 1, -1, -1, 0, -1, 0, -1, -1, 0, 0,
                                      "ʒ", -0.5, 1, 0, -1, 0, -1, 0, -1, -1, 0, 0,
                                      "ʧ", -0.8, 1, -1, -1, 1, -1, 0, -1, -1, 0, 0,
                                      "ʤ", -0.8, 1, 0, -1, 1, -1, 0, -1, -1, 0, 0,
                                      "m", 0, 0, 1, 1, 1, 1, 0, -1, 1, 0, 0,
                                      "n", 0, 0, 1, 1, 1, -1, 1, -1, -1, 1, 0,
                                      "ŋ", 0, 0, 1, 1, 1, -1, -1, -1, -1, -1, 0,
                                      "l", 0.5, 0, 1, 0, -1, -1, 1, -1, -1, 1, 0,
                                      "w", 0.8, 0, 1, 0, 0, 1, -1, -1, 1, -1, 0,
                                      "j", 0.8, 0, 1, 0, 0, -1, 0, -1, -1, 0, 1,
                                      "ɾ", 0.5, 1, 1, 0, -1, -1, -1, 1, -1, 1, 0,
                                      "ʙ", -0.5, 1, 0, -1, 1, 1, 0, -1, 1, 0, 0,
                                      "ʀ", 1, 1, 1, -1, -1, -1, -1, -1, -1, 0, -1,
                                      "ɲ", 1, 1, 1, 1, 1, -1, 1, -1, -1, 1, 0,
                                      "r",  1, 1, 1, -1, -1, -1, -1, -1, 0, 0, -1,
                                      "ɣ", -0.5 , 1, 1, 0,  0, -1, -1, -1, 0, 0, -1,
                                      "ɱ", 1, 1, 1, 1,  1, 1, -1, -1, -1, 0, 0,
                                      "x", -0.5, 1, -1, 0,  0, -1, -1, -1, 0, 0,  -1,
                                      "ʁ", -0.5, 1, -1, 0,  0, -1, -1, 1, 0, 0,  -1,
                                      "ç", -0.5, 1, -1, 0,  0, -1, 1, -1, 0, 0,  -1,
                                      "ɲ", 1, 1, 1, 1, 1, -1, 1, -1, -1, +1, 0,
                                      "ʔ", -1, 0, 0, -1, 0, -1, -1, 1, -1, 1, 0)    # added manually as not defined in original. Drew from Cambridge Handbook of Phonology and
                                                                                    # similarities with /h/


colnames_target <- actual_target_IPA_FULL_Lyon %>% dplyr::select(ID, starts_with("TS"))
colnames(colnames_target) <- sub("T","",colnames(colnames_target))
target_list <- setNames(lapply(names(colnames_target)[-1], function(x) cbind(colnames_target[1], colnames_target[x])), names(colnames_target)[-1])

output_target <- lapply(target_list, FUN = function(element) {
  target_segment <- data.frame(element,
                               distinctive.feature.matrix[match(element[,2], distinctive.feature.matrix$Symbol), 2:12], 
                               stringsAsFactors=FALSE) %>%
    replace(is.na(.), 0) %>%
    mutate(data_type = "Target")
})


colnames_actual <- actual_target_IPA_FULL_Lyon %>% dplyr::select(ID, starts_with("AS"))
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

# Session_data ------------------------------------------------------------

# Convert session info into age in months

comparison_data$Session <- gsub("[^0-9]", "", comparison_data$Session)  # remove a b c from session numbers
comparison_data$Session <- paste0("0", comparison_data$Session) # add leading 0 to session numbers

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

#write_csv(comparison_data, "Data/large_files/comparison_data_Lyon.csv")

# data_summ_Lyon <- comparison_data_Lyon %>%
#   #mutate(id = group_indices(., Speaker, session_ordinal)) %>%    # give each combo an id with group_indices
#   group_by(Speaker, age, Gloss) %>%
#   summarize_all(.funs = list(mean))
# 
# data_summ_Lyon <- data_summ_Lyon %>%
#   mutate(subj_session = paste(Speaker, age, sep="_")) %>%
#   feather::write_feather("Data/large_files/data_summ_Lyon.feather")

# generate data for global matrix

distance_full_df <- as.data.frame(output_full)
colnames(distance_full_df)[1] <- "unique"
colnames(distance_full_df)[14] <- "data"

distance_full <- distance_full_df %>% dplyr::select(unique, -ends_with("data_type") & -ends_with(".ID")) %>%
  rename("ID" = "unique",
         "data_type" = "data") %>%
  left_join(comparison_data) %>%
feather::write_feather("Data/large_files/distance_full_Lyon.feather")

