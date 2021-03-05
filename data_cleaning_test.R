
# Now run this script! 

# This script takes the .csv files generated in Phon and cleans up the data to include only the words used in the analysis

# read IPA fonts:

options("encoding" = "UTF-8")

kLogFileName <- "parser.log"
log <- function(msg="") {
  con <- file(kLogFileName, "a")
  tryCatch({
    cat(iconv(msg, to="UTF-8"), file=con, sep="\n")
  },
  finally = {
    close(con)
  })
}

# You should have already done the following:

# In Phon: Specialized > WordMatch
#  Untick: Stress pattern, Include length diacritics, Include stress markers
#  Tick: Exact match, Syllable count, Ignore diacritics, Include syllable boundaries

# Data is imported as Excel file; before importing into R, go through each file and save as a .csv
# then open each file individually in Excel and go through the following processes to make the data readable in this script:
# (this doesn't work in R as far as I can tell)

# - stress markersˈˌ
# - length marker ː
# - nasalized vowels ̃
# - syllabic consonants ̩
# - aspiration ʰ (be sure to match case)
# - rhoticity ˞
# - Not sure: ^
# - dark /l/: ɫ
# - Also replaced IPA /g/ with keyboard <g> as it wasn't reading for some reason
# - and r with <r> as also wasn't reading

# # TO CHECK [all seems to work fine in current version]:
# # I started by manually removing diacritics from the .csv file - all syllabic markers, length markers and aspiration. I also removed any glottal stops that were in 
# # consonant clusters. The code should then run properly as it can read all other IPA symbols.


# read in the file:

sample_data <- read_csv("Data/Phon_outputs/Report_Alex.csv") # change this to match the file path to your data

# If you have multiple .csv files, read them all in individually and then do the following (remove the #s to make it work):

# sample_data <- rbind(sample_data1,
#                      sample_data2,
#                      sample_data3,
#                      sample_data4)

remove.list <- paste(c(            # Create a list of tokens to remove from the dataset, which is called using grepl further down
  '@l',       # remove all tokens of alphabetic letters - these all have very similr prosodic structures (n=1492)
  '@wp',      # remove word play tokens since these don't have a real target form (n=241). I am keeping onomatopoeia and child words, however
  '&',        # remove all interjections & hesitations (n=1384)
  '@b',       # remove babytalk words that don't have a clear adult target
  '@c',       # not sure what @c refers to but remove these as well as they are made-up forms
  'r',
  'S',        
  "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), # these tokens are alphabetic letters but haven't been coded as @l
  collapse = '|') 

FULLsample <- sample_data %>%
  dplyr::select(Speaker,  # select columns I want to keep
                Age,
                Session,
                Orthography,
                `IPA Target`,
                `IPA Actual`,
                `Exact Match`,
                `IPA Target CV`,
                `IPA Actual CV`,
                `CV Match`,
                `IPA Target Syllable Count`,
                `IPA Actual Syllable Count`,
                `Syllable Count Match`) %>%
  rename(
    "IPAtarget" = `IPA Target`,
    "IPAactual" = `IPA Actual`,
    "IPAmatch" = `Exact Match`, 
    "TargetCV" = `IPA Target CV`, 
    "ActualCV" = `IPA Actual CV`, 
    "CVmatch" = `CV Match`, 
    "Targetphon" = `IPA Target Syllable Count`, 
    "Actualphon" = `IPA Actual Syllable Count`, 
    "Sylmatch" = `Syllable Count Match`,
    "Gloss" = "Orthography"
  ) %>%
  filter(Gloss != "xxx" &
           Gloss != "xxx:" &
           Gloss != "yyy" & 
           Gloss != "(.)" &
           IPAtarget != "*" &
           IPAactual != "*" &
           IPAactual != "(.)") %>%
  mutate(Gloss = factor(Gloss),
         TargetCV = factor(TargetCV),
         ActualCV = factor(ActualCV)) %>%
  tibble::rowid_to_column("ID") %>%
  filter(!grepl(remove.list, Gloss)) # filter out everything from remove.list that's found in the column Gloss

FULLsample$Gloss <- gsub("(@).*", "\\1", FULLsample$Gloss)
FULLsample$Gloss <- str_replace_all(FULLsample$Gloss, "[^[:alnum:]]", "")


feather::write_feather(FULLsample, "Data/FULLsample_name.feather")   # replace name with your name :)
