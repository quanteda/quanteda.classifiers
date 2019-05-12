# load packages
library(dplyr)
library(tidyr)
library(car)
library(readtext)
library(quanteda)
library(quanteda.corpora)

########################
### 1. Load and aggregate crowdcoded data ----
########################

## 1.1 Load APSR data on economic policy/social policy/neither ----

data_readtext_uk_econsocial <- readtext("tests/data_creation/data_uk_policyarea.zip",
                                         ignore_missing_files = TRUE, encoding = "utf-8")

# exclude screeners from data frame
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
  dplyr::filter(!grepl('Code this sentence as', sentence_text))

# number of codings
nrow(data_readtext_uk_econsocial)

length(unique(data_readtext_uk_econsocial$sentenceid))

## for some sentences, special characters are displayed differently depending on the
## crowd coding job. Therefore, I remove sentence_text here and merge it later 
## using "master.sentences.Rdata" which does not contain these encoding errors

data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
  select(-sentence_text)

# load the "master sentences" with meta data on manifestos and years
load("tests/data_creation/master.sentences.Rdata")

sentences_metadata <- sentences %>% 
    select(sentenceid, manifestoid, party, year) # select relevant variables

# merge metadata on manifestos with crowd coded texts
data_readtext_uk_econsocial <- left_join(data_readtext_uk_econsocial, sentences_metadata, by = "sentenceid")

length(unique(data_readtext_uk_econsocial$sentenceid))

# recode policy area
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
    mutate(class_policyarea = ifelse(policy_area == 1, "Not Economic or Social",
                                       ifelse(policy_area == 2, "Economic",
                                              ifelse(policy_area == 3, "Social", NA))))

nrow(data_readtext_uk_econsocial)

# create numeric indicator for aggregation
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
    mutate(class_policyarea_num = ifelse(policy_area == 1, 0,
                                        ifelse(policy_area == 2, -1,
                                               ifelse(policy_area == 3, 1, NA))))

# # create numeric indicator for aggregation of direction
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>%
    mutate(class_policyarea_direction_num = ifelse(!is.na(soc_scale), soc_scale,
                                                  ifelse(!is.na(econ_scale), econ_scale, NA)))

# aggregate data to the level of sentences
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
    group_by(manifestoid, sentenceid) %>% 
    mutate(crowd_econsocial_n = n(),
           class_policyarea_mean = mean(class_policyarea_num, na.rm = TRUE))

# create variable with the policy area based on the aggregated coding
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
    mutate(class_policyarea = ifelse(class_policyarea_mean < 0, "Economic",
                                       ifelse(class_policyarea_mean == 0, "Not Economic or Social",
                                              ifelse(class_policyarea_mean > 0, "Social", NA))))

# here I make sure that the policy direction mean only takes 
# into account the majority category and not also the minority category position values
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
  mutate(class_policy_direction_num = 
           ifelse(class_policyarea == "Economic" & class_policyarea_num == -1, class_policyarea_direction_num, 
                  ifelse(class_policyarea == "Social" & class_policyarea_num == 1, class_policyarea_direction_num,
                         NA)))

# aggregate data to the level of sentences
data_uk_econsocial <- data_readtext_uk_econsocial %>% 
  group_by(manifestoid, sentenceid, crowd_econsocial_n, class_policyarea) %>% 
  summarise(class_policyarea_direction_mean = mean(class_policy_direction_num, na.rm = TRUE),
         class_policyarea_mean = mean(class_policyarea_num, na.rm = TRUE)) %>% 
  ungroup() 

# create variable with the direction of the policy area based on the aggregated coding
data_uk_econsocial <- data_uk_econsocial %>% 
    mutate(class_policyarea_direction = ifelse(class_policyarea == "Economic" & between(class_policyarea_direction_mean, -2, -1.00000000011), "Very left",
                                               ifelse(class_policyarea == "Economic" & between(class_policyarea_direction_mean, 1, 0.0000000001), "Left",
                                                  ifelse(class_policyarea == "Economic" & class_policyarea_direction_mean == 0, "Neither left nor right",
                                                         ifelse(class_policyarea == "Economic" & between(class_policyarea_direction_mean, 0.00000001, 1), "Right",
                                                         ifelse(class_policyarea == "Economic"& between(class_policyarea_direction_mean, 1.00000001, 2), "Very right",
                                                                ifelse(class_policyarea == "Social" & between(class_policyarea_direction_mean, -2, -1.00000000011), "Very liberal",
                                                                       ifelse(class_policyarea == "Social" & between(class_policyarea_direction_mean, 1, 0.0000000001), "Liberal",
                                                                              ifelse(class_policyarea == "Social" & class_policyarea_direction_mean == 0, "Neither liberal nor conservative",
                                                                                     ifelse(class_policyarea == "Social" & between(class_policyarea_direction_mean, 0.00000001, 1), "Conservative",
                                                                                            ifelse(class_policyarea == "Social" & between(class_policyarea_direction_mean, 1.0000001, 2), "Very conservative", 
                                                                                                   NA)))))))))))

# separate the variable manifestoid into Party and Year
data_uk_econsocial <- data_uk_econsocial %>% 
    separate(manifestoid, into = c("Party", "Year"),
             remove = FALSE)

# merge text from "sentences" data frame which does not contain encoding errors

dat_sentences <- sentences %>% 
  select(sentence_text, sentenceid)

data_uk_econsocial <- left_join(data_uk_econsocial, dat_sentences, 
                                    by = "sentenceid")

# select some of the variables and add three additional variables
data_uk_man_econsocial <- data_uk_econsocial %>% 
    ungroup() %>% 
    select(text = sentence_text, sentenceid, Party, Year, 
           class_policyarea, class_policyarea_mean,
           class_policyarea_direction, class_policyarea_direction_mean,
           crowd_econsocial_n) 

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
    separate(manifestoid, into = c("Party", "Year")) # %>% 
   # filter(Party != "Coalition") # remove coalition agreement

# get the class
data_readtext_uk_immigration <- data_readtext_uk_immigration %>% 
    mutate(class_immigration_num = ifelse(policy_area == 4, 1, 0))

# aggregate to the level of setence
data_uk_immigration <- data_readtext_uk_immigration %>% 
    group_by(Party, Year, sentenceid, sentence_text) %>% 
    summarise(crowd_immigration_n = n(),
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
           class_immigration_direction_mean, 
           crowd_immigration_n) 


########################
## 1.3 Merge both crowdcoded datasets ----
########################

# (the 2010 manifestos have been coded both with regards to 
# immigration AND econ/social/neither)

# select certain variables needed for the manifesto coding
data_uk_immig_man_2010_select <- data_uk_immig_man_2010 %>% 
    select(sentenceid, text, starts_with("class_"), Party, Year,
           crowd_immigration_n)

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
    select(-c(party_year, sentenceid))


# create unique id for each sentence and select only necessary variables
data_uk_manifestos_selectvars <- data_uk_manifestos %>% 
    select(text,
           party = Party,
           year = Year,
           starts_with("class_"),
           crowd_immigration_n,
           crowd_econsocial_n)

# create binary indicator whether sentence was crowdcoded

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
    select(-c(sentence_no, ntoken_sent, class_policyarea_direction_mean)) %>% 
    ungroup()

# rename some variables based on https://github.com/quanteda/quanteda.classifiers/pull/8

dat_corpus_renamed <- dat_corpus %>% 
  rename(crowd_econsocial_label = class_policyarea,
         crowd_econsocial_mean = class_policyarea_mean,
         crowd_econsocial_dir = class_policyarea_direction,
         crowd_immigration_label = class_immigration,
         crowd_immigration_mean = class_immigration_mean,
         crowd_immigration_dir = class_immigration_direction,
         crowd_immigration_dir_mean = class_immigration_direction_mean)


table(dat_corpus$party)

dat_corpus_renamed <- dat_corpus_renamed %>% 
  mutate(party = ifelse(party == "Comm", "CP", party)) %>% 
  mutate(party = ifelse(party == "Gr", "Greens", party)) %>% 
  mutate(party = ifelse(party == "OMRL", "MRLP", party)) %>% 
  mutate(party = ifelse(party == "PCy", "PC", party))


recode_party <- c("'Coalition'='Coalition Agreement';
                  'BNP'='British National Party';
                  'CAP'='Community Action Party';
                  'Con'='Conservative Party';
                  'Comm'='Communist Party';
                  'CP'='Communist Party';
                  'Dem'='Democratic Party';
                  'DUP'='Democratic Unionist Party';
                  'EDP'='English Democrats';
                  'EIP'='English Independence Party';
                  'FSP'='Free Scotland Party';
                  'FW'='Forward Wales';
                  'Greens'='Green Party';
                  'IGV'='Independent Green Voice';
                  'LA'='Left Alliance';
                  'Lab'='Labour Party';
                  'Lib'='Liberal Party';
                  'LD'='Liberal Demoracts';
                  'LibSDP'='Social Democratic Party';
                  'MK'='Mebyon Kernow - the Party for Cornwall';
                  'MRLP'='Official Monster Raving Loony Party';
                  'ND'='National Democrats';
                  'NIA'='Northern Ireland Alliance';
                  'PA'='Prolife Alliance';
                  'PC'='Plaid Cymru';
                  'PP'='Peace Party';
                  'PUP'='Progressive Unionist Party';
                  'PVP'='Protest Vote Party';
                  'Resp'='Respect';
                  'RT'='Richard Taylor Personal Manifesto';
                  'Scon'='Scottish Conservative Party';
                  'SDLP'='Social Democratic and Labour Party';
                  'SEP'='Socialist Equality Party';
                  'SF'='Sinn Féin';
                  'SGr'='Scottish Green Party';
                  'SLab'='Scottish Labour Party';
                  'SLD'='Scottish Liberal Democrats';
                  'SNP'='Scottish National Party';
                  'SP'='Socialist Party';
                  'SCon'='Scottish Conservative Party';
                  'SSoc'='Scottish Socialist Party';
                  'SSP'='Scottish Socialist Party';
                  'Stuck'='Stuckist Party';
                  'TW'='Third Way';
                  'UKIP'='UK Independence Party';
                  'UUP'='Ulster Unionist Party';
                  'Ver'='Veritas Party'")



dat_corpus_renamed <- dat_corpus_renamed %>% 
  mutate(partyname = car::recode(party, recode_party))

table(dat_corpus_renamed$partyname)

length(unique(dat_corpus_renamed$partyname))

dat_corpus_renamed <- dat_corpus_renamed %>% 
  select(doc_id, text, party, partyname, year, 
         crowd_econsocial_label, crowd_econsocial_dir,
         crowd_econsocial_mean,
         crowd_econsocial_n,
         crowd_immigration_label, crowd_immigration_dir,
         crowd_immigration_mean,
         crowd_immigration_n)

dat_corpus_renamed$crowd_econsocial_label <- factor(dat_corpus_renamed$crowd_econsocial_label)
dat_corpus_renamed$crowd_econsocial_dir <- factor(dat_corpus_renamed$crowd_econsocial_dir)
dat_corpus_renamed$crowd_immigration_label <- factor(dat_corpus_renamed$crowd_immigration_label)
dat_corpus_renamed$crowd_immigration_dir <- factor(dat_corpus_renamed$crowd_immigration_dir)
dat_corpus_renamed$year <- as.integer(dat_corpus_renamed$year)
dat_corpus_renamed$party <- as.factor(dat_corpus_renamed$party)
dat_corpus_renamed$partyname <- as.factor(dat_corpus_renamed$partyname)
dat_corpus_renamed$crowd_immigration_n <- as.integer(dat_corpus_renamed$crowd_immigration_n)
dat_corpus_renamed$crowd_econsocial_n <- as.integer(dat_corpus_renamed$crowd_econsocial_n)

dat_corpus_renamed$text <- as.character(dat_corpus_renamed$text)

data_corpus_manifestosentsUK <- corpus(dat_corpus_renamed,
                                       docid_field = "doc_id")

# add corpus to package
usethis::use_data(data_corpus_manifestosentsUK, overwrite = TRUE)


table(docvars(data_corpus_manifestosentsUK, "party"))
