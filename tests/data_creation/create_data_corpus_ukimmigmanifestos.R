# load packages
library(dplyr)
library(tidyr)
library(readtext)
library(quanteda)
library(quanteda.corpora)

########################
### 1. Load and aggregate crowdcoded data ----
########################

## 1.1 Load APSR data on economic policy/social policy/neither ----

data_readtext_uk_econsocial <- readtext("tests/data_creation/data_uk_policyarea.zip",
                                         ignore_missing_files = TRUE, encoding = "utf-8")

# load the "master sentences" with meta data on manifestos and years
load("tests/data_creation/master.sentences.Rdata")

sentences_metadata <- sentences %>% 
    select(sentenceid, manifestoid, party, year) # select relevant variables

# merge metadata on manifestos with crowd coded texts
data_readtext_uk_econsocial <- left_join(data_readtext_uk_econsocial, sentences_metadata, by = "sentenceid")

# exclude screeners from data frame
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
    dplyr::filter(!grepl('Code this sentence as', sentence_text))

# recode policy area
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
    mutate(class_policyarea = ifelse(policy_area == 1, "Not Economic or Social",
                                       ifelse(policy_area == 2, "Economic",
                                              ifelse(policy_area == 3, "Social", NA))))

# create numeric indicator for aggregation
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
    mutate(class_policyarea_num = ifelse(policy_area == 1, 0,
                                        ifelse(policy_area == 2, -1,
                                               ifelse(policy_area == 3, 1, NA))))

# create numeric indicator for aggregation of direction
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
    mutate(class_policyarea_direction_num = ifelse(!is.na(soc_scale), soc_scale,
                                                  ifelse(!is.na(econ_scale), econ_scale, NA)))

# aggregate data to the level of sentences
data_uk_econsocial <- data_readtext_uk_econsocial %>% 
    group_by(manifestoid, sentenceid, sentence_text, pre_sentence, post_sentence) %>% 
    summarise(n_codings = n(),
              class_policyarea_mean = mean(class_policyarea_num, na.rm = TRUE),
              class_policyarea_direction_mean = mean(class_policyarea_direction_num, na.rm = TRUE))


# create variable with the policy area based on the aggregated coding
data_uk_econsocial <- data_uk_econsocial %>% 
    mutate(class_policyarea = ifelse(class_policyarea_mean < 0, "Economic",
                                       ifelse(class_policyarea_mean == 0, "Not Economic or Social",
                                              ifelse(class_policyarea_mean > 0, "Social", NA))))


# create variable with the direction of the policy area based on the aggregated coding
data_uk_econsocial <- data_uk_econsocial %>% 
    mutate(class_policyarea_direction = ifelse(class_policyarea == "Economic" & class_policyarea_direction_mean < 0, "Left",
                                           ifelse(class_policyarea == "Economic" & class_policyarea_direction_mean > 0, "Right",
                                                  ifelse(class_policyarea == "Economic" & class_policyarea_direction_mean == 0, "Neither left nor right",
                                                         ifelse(class_policyarea == "Social" & class_policyarea_direction_mean < 0, "Liberal",
                                                                ifelse(class_policyarea == "Social" & class_policyarea_direction_mean > 0, "Conservative",
                                                                       ifelse(class_policyarea == "Social" & class_policyarea_direction_mean == 0, "Neither liberal nor conservative", NA)))))))

# seperate the variable manifestoid into Party and Year
data_uk_econsocial <- data_uk_econsocial %>% 
    separate(manifestoid, into = c("Party", "Year"),
             remove = FALSE)

# select some of the variables and add three additional variables
data_uk_man_econsocial <- data_uk_econsocial %>% 
    ungroup() %>% 
    select(text = sentence_text, sentenceid, Party, Year, 
           class_policyarea, class_policyarea_mean,
           class_policyarea_direction, class_policyarea_direction_mean) 

# replace nan values in class_direction_mean with na
data_uk_man_econsocial$class_policyarea_direction_mean[is.nan(data_uk_man_econsocial$class_policyarea_direction_mean)] <- NA



## 1.2 Load APSR data on immigration ----
data_readtext_uk_immigration <- readtext("tests/data_creation/data_uk_immigration.zip",
                                         ignore_missing_files = TRUE)

# exclude screeners
data_readtext_uk_immigration <- data_readtext_uk_immigration %>% 
    subset(manifestoid != "screener")

# use manifestoid to create a party and year variable
data_readtext_uk_immigration <- data_readtext_uk_immigration %>% 
    separate(manifestoid, into = c("Party", "Year")) %>% 
    filter(Party != "Coalition") # remove coalition agreement

# get the class
data_readtext_uk_immigration <- data_readtext_uk_immigration %>% 
    mutate(class_immigration_num = ifelse(policy_area == 4, 1, 0))

# aggregate to the level of setence
data_uk_immigration <- data_readtext_uk_immigration %>% 
    group_by(Party, Year, sentenceid, sentence_text) %>% 
    summarise(n_codings = n(),
              class_immigration_mean = mean(class_immigration_num, na.rm = TRUE),
              class_immigration_direction_mean = mean(immigr_scale, na.rm = TRUE))

# replace nan values in class_direction_mean with na
data_uk_immigration$class_immigration_direction_mean[is.nan(data_uk_immigration$class_immigration_direction_mean)] <- NA

# use the average evaluations to construct the majority-rule based classification
data_uk_immigration <- data_uk_immigration %>% 
    mutate(class_immigration = ifelse(class_immigration_mean >= 0.5, "Immigration", "Not immigration")) %>% 
    mutate(class_immigration_direction = ifelse(class_immigration_direction_mean > 0, 
                                         "Against",
                                         ifelse(class_immigration_direction_mean < 0, "Supportive", 
                                                ifelse(class_immigration_direction_mean == 0, "Neutral", NA))))

# select some of the variables and add three additional variables
data_uk_immig_man_2010 <- data_uk_immigration %>% 
    ungroup() %>% 
    select(text = sentence_text, sentenceid, Party, Year, class_immigration, 
           class_immigration_mean,
           class_immigration_direction,
           class_immigration_direction_mean) 


########################
## 1.3 Merge both crowdcoded datasets ----
########################

# (the 2010 manifestos have been coded both with regards to 
# immigration AND econ/social/neither)

# select certain variables needed for the manifesto coding
data_uk_immig_man_2010_select <- data_uk_immig_man_2010 %>% 
    select(sentenceid, text, starts_with("class_"), "Party", "Year")

# full join of both crowdcoded datasets
dat_uk_crowdcoded <- full_join(data_uk_man_econsocial,
                               data_uk_immig_man_2010_select, by = c("sentenceid"))

names(dat_uk_crowdcoded)

table(dat_uk_crowdcoded$Party.x)
table(dat_uk_crowdcoded$Party.y)

# this must be the same value, otherwise mergins is not correct 
# (the last three number are sentences from Lab, Con, and LD which are in both manifestos)
stopifnot(nrow(dat_uk_crowdcoded) == 7322 + 18544 - 855 - 1240 - 1349)

# rename and remove unnessary variables
dat_uk_man_crowdcoded_clean <- dat_uk_crowdcoded %>% 
    mutate(Party = ifelse(is.na(Party.x), Party.y, Party.x)) %>% 
    mutate(Year = ifelse(is.na(Year.x), Year.y, Year.x)) %>% 
    mutate(text = ifelse(is.na(text.x), text.y, text.x)) %>% 
    select(-c(Party.x, Party.y, Year.x, Year.y, text.x, text.y))


########################
## 2. Add data_corpus_ukmanifestos from quanteda.corpora package ----
########################

# Since some of the manifestos are already coded in terms of social/eoconomic/neither,
# we remove these documents from the non-annotated corpus

# get overview of crowdcoded manifesto
manifestos_ukseconsocial <- dat_uk_man_crowdcoded_clean %>% 
    ungroup() %>% 
    select(Party, Year) %>% 
    unique() %>% 
    mutate(party_year = paste(Party, Year, sep = "_"))

# create new docvar used for removing crowdcoded manifestos 
# from not-annotated corpus
docvars(data_corpus_ukmanifestos, "party_year") <- paste(
    docvars(data_corpus_ukmanifestos, "Party"),
    docvars(data_corpus_ukmanifestos, "Year"), sep = "_"
)

data_corpus_ukmanifestos_subset <- data_corpus_ukmanifestos %>% 
    corpus_subset(!party_year %in% manifestos_ukseconsocial$party_year)

ndoc(data_corpus_ukmanifestos)
ndoc(data_corpus_ukmanifestos_subset)

# check whether Lab, Con, Lib manifestos are excluded for time between 1987 and 2010 (yes!)
table(docvars(data_corpus_ukmanifestos_subset, "party_year"))

# reshape corpus of UK manifestos from the quanteda.corpora package to sentences
data_corpus_ukmanifestos_sentences <- data_corpus_ukmanifestos_subset %>% 
    corpus_subset(Type == "natl") %>% 
    corpus_reshape(to = "sentences")

# transform to data frame for easier adjustments of document-level variables
data_uk_man <- data.frame(
    text = texts(data_corpus_ukmanifestos_sentences),
    docvars(data_corpus_ukmanifestos_sentences)
)


########################
## 3. Import additional manifestos from 2015 and 2017 elections ----
########################

## Note: data retrieved from http://polidoc.net

data_uk_1517 <- readtext("tests/data_creation/data_uk_manifestos_20152017.zip",
                            ignore_missing_files = TRUE,
                            encoding = "utf-8",
                            docvarsfrom = "filenames",
                            docvarnames = c("Year", "Party"))

# create corpus and reshape to level of sentences
data_corpus_ukmanifestos_1517 <- corpus(data_uk_1517) %>% 
    corpus_reshape(to = "sentences")

# transform to data frame for easier adjustments of document-level variables
data_uk_man_1517 <- data.frame(
    text = texts(data_corpus_ukmanifestos_1517),
    docvars(data_corpus_ukmanifestos_1517)
)

# make Year to a factor variable in all three data frames
data_uk_man$Year <- as.factor(data_uk_man$Year)
dat_uk_man_crowdcoded_clean$Year <- as.factor(dat_uk_man_crowdcoded_clean$Year)
data_uk_man_1517$Year <- as.factor(data_uk_man_1517$Year)


########################
## 4. Combine manifestos and create corpus ----
########################

data_uk_manifestos <- bind_rows(data_uk_man, dat_uk_man_crowdcoded_clean, data_uk_man_1517)

data_uk_manifestos <- data_uk_manifestos %>% 
    mutate(Country = "UK",
           Language = "En") %>% 
    select(-c(party_year, sentenceid))


# create unique id for each sentence and select only necessary variables
data_uk_manifestos_selectvars <- data_uk_manifestos %>% 
    select(text,
           country = Country,
           language = Language, 
           party = Party,
           year = Year,
           starts_with("class_"))

# create binary indicator whether sentence was crowdcoded

man_crowdcoded_immig <- data_uk_immig_man_2010_select %>% 
    select(Party, Year) %>% 
    unique() %>% 
    mutate(party_year = paste(Party, Year, sep = "_"))

man_crowdcoded_policyarea <- data_uk_econsocial %>% 
    ungroup() %>% 
    select(Party, Year) %>% 
    unique() %>% 
    mutate(party_year = paste(Party, Year, sep = "_"))

data_uk_manifestos_selectvars <- data_uk_manifestos_selectvars %>% 
    mutate(party_year = paste(party, year, sep = "_"))

## create variable indicating whether sentence was crowdsourced for one or both exercises
data_uk_manifestos_selectvars <- data_uk_manifestos_selectvars %>% 
    mutate(crowdcoded_policyarea = ifelse(party_year %in% man_crowdcoded_policyarea$party_year, 
                                          TRUE, FALSE)) %>%
    mutate(crowdcoded_immigration = ifelse(party_year %in% man_crowdcoded_immig$party_year,
                                         TRUE, FALSE)) %>% 
    select(-party_year)
      
table(data_uk_manifestos_selectvars$crowdcoded_immigration)   
table(data_uk_manifestos_selectvars$crowdcoded_policyarea)   

# create text corpus
corp <- corpus(data_uk_manifestos_selectvars)

docvars(corp, "ntoken_sent") <- ntoken(corp, remove_punct = TRUE)

ndoc(corp)

# remove observation with 0 tokens (just a long horizontal line in 
# the last sentence of the 2001 Dem manifestos)
corp_small <- corp %>% 
    corpus_subset(ntoken_sent > 0)

# transform back to data frame to create nice doc_id after having removed "empty" sentences
dat_corpus <- data.frame(
    text = texts(corp_small),
    docvars(corp_small)
)

dat_corpus <- dat_corpus %>% 
    arrange(year, party) %>% 
    group_by(party, year) %>% 
    mutate(sentence_no = 1:n()) %>% 
    mutate(doc_id = paste(party, year, sentence_no, sep = "_")) %>% 
    select(-sentence_no) %>% 
    ungroup()

dat_corpus$class_policyarea <- factor(dat_corpus$class_policyarea)
dat_corpus$class_policyarea_direction <- factor(dat_corpus$class_policyarea_direction)
dat_corpus$class_immigration <- factor(dat_corpus$class_immigration)
dat_corpus$class_immigration_direction <- factor(dat_corpus$class_immigration_direction)

dat_corpus$text <- as.character(dat_corpus$text)

data_corpus_manifestosentsUK <- corpus(dat_corpus,
                                       docid_field = "doc_id")

# add corpus to package
usethis::use_data(data_corpus_manifestosentsUK, overwrite = TRUE)
