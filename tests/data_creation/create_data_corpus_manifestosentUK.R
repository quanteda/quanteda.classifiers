########################
### Create UK election manifesto dataset on the level of sentences
########################

# load packages
library(dplyr)
library(tidyr)
library(car)
library(readtext)
library(quanteda)
library(quanteda.corpora)
library(spacyr)

########################
### 1. Load and aggregate crowdcoded data ----
########################

## 1.1 Load APSR data on economic policy/social policy/neither ----

data_readtext_uk_econsocial <- readtext("tests/data_creation/data_uk_policyarea.zip",
                                         ignore_missing_files = TRUE, encoding = "utf-8")

# exclude screeners from data frame using a regular expression
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
  dplyr::filter(!grepl('Code this sentence as', sentence_text))

## For some sentences, special characters are displayed differently depending on the
## crowd coding job. Therefore, I remove sentence_text here and merge it later 
## using "master.sentences.Rdata" which does not contain these encoding errors

data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
  select(-sentence_text)

# load "master sentences" from the APSR replication data with metadata on manifestos and years
load("tests/data_creation/master.sentences.Rdata")

sentences_metadata <- sentences %>% 
    select(sentenceid, manifestoid, party, year) # select relevant variables

# merge metadata on manifestos with crowd coded texts
data_readtext_uk_econsocial <- left_join(data_readtext_uk_econsocial, sentences_metadata, by = "sentenceid")

# create numeric indicator for aggregation of policy area
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
    mutate(class_policyarea_num = ifelse(policy_area == 1, 0,
                                        ifelse(policy_area == 2, -1,
                                               ifelse(policy_area == 3, 1, NA))))

# create numeric indicator for aggregation of direction
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
    mutate(crowd_econsocial_label = ifelse(class_policyarea_mean < 0, "Economic",
                                       ifelse(class_policyarea_mean == 0, "Not Economic or Social",
                                              ifelse(class_policyarea_mean > 0, "Social", NA))))


# here I make sure that the policy direction mean only takes 
# into account the majority category and not also the minority category position values
data_readtext_uk_econsocial <- data_readtext_uk_econsocial %>% 
  mutate(class_policy_direction_num = 
           ifelse(crowd_econsocial_label == "Economic" & class_policyarea_num == -1, class_policyarea_direction_num, 
                  ifelse(crowd_econsocial_label == "Social" & class_policyarea_num == 1, class_policyarea_direction_num,
                         NA)))

# aggregate data to the level of sentences
data_uk_econsocial <- data_readtext_uk_econsocial %>% 
  group_by(manifestoid, sentenceid, crowd_econsocial_n, crowd_econsocial_label) %>% 
  summarise(crowd_econsocial_mean = mean(class_policy_direction_num, na.rm = TRUE)) %>% 
  ungroup() 

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
           crowd_econsocial_label, crowd_econsocial_mean,
           crowd_econsocial_n) 

# replace nan values in class_direction_mean with na
data_uk_man_econsocial$crowd_econsocial_mean[is.nan(data_uk_man_econsocial$crowd_econsocial_mean)] <- NA


## 1.2 Load APSR data on immigration ----
data_readtext_uk_immigration <- readtext("tests/data_creation/data_uk_immigration.zip",
                                         ignore_missing_files = TRUE)

# exclude screeners
data_readtext_uk_immigration <- data_readtext_uk_immigration %>% 
    subset(manifestoid != "screener")

# use manifestoid to create a party and year variable
data_readtext_uk_immigration <- data_readtext_uk_immigration %>% 
    separate(manifestoid, into = c("Party", "Year")) 

# get the class
data_readtext_uk_immigration <- data_readtext_uk_immigration %>% 
    mutate(class_immigration_num = ifelse(policy_area == 4, 1, 0))

# aggregate to the level of setence
data_uk_immigration <- data_readtext_uk_immigration %>% 
    group_by(Party, Year, sentenceid, sentence_text) %>% 
    summarise(crowd_immigration_n = n(),
              class_immigration_num = mean(class_immigration_num, na.rm = TRUE),
              crowd_immigration_mean = mean(immigr_scale, na.rm = TRUE))

# replace nan values in class_direction_mean with na
data_uk_immigration$crowd_immigration_mean[is.nan(data_uk_immigration$crowd_immigration_mean)] <- NA

# use the average evaluations to construct the majority-rule based classification
data_uk_immigration <- data_uk_immigration %>% 
    mutate(crowd_immigration_label = ifelse(class_immigration_num >= 0.5, "Immigration", "Not immigration")) 

# select some of the variables and add three additional variables
data_uk_immig_man_2010 <- data_uk_immigration %>% 
    ungroup() %>% 
    select(text = sentence_text, sentenceid, Party, Year, crowd_immigration_label, 
           crowd_immigration_mean,
           crowd_immigration_n) 


########################
## 1.3 Merge both crowdcoded datasets ----
########################

# Note that the 2010 manifestos have been coded both with regards to 
# immigration AND econ/social/neither

# full join of both crowdcoded datasets
dat_uk_crowdcoded <- full_join(data_uk_man_econsocial,
                               data_uk_immig_man_2010, by = c("sentenceid"))

table(dat_uk_crowdcoded$Party.x)
table(dat_uk_crowdcoded$Party.y)

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

# remove manifestos that have been crowdcoded
data_corpus_ukmanifestos_subset <- data_corpus_ukmanifestos %>% 
    corpus_subset(!party_year %in% manifestos_ukseconsocial$party_year)

ndoc(data_corpus_ukmanifestos)
ndoc(data_corpus_ukmanifestos_subset)

# check whether Lab, Con, Lib manifestos are excluded for time between 1987 and 2010 (yes!)
table(docvars(data_corpus_ukmanifestos_subset, "party_year"))

# only keep manifestos from national general elections (Type == "natl")
data_corpus_ukmanifestos_sentences <- data_corpus_ukmanifestos_subset %>% 
    corpus_subset(Type == "natl") 
    
# transform to data frame for easier adjustments of document-level variables
data_uk_man <- data.frame(
    doc_id = docnames(data_corpus_ukmanifestos_sentences),
    text = texts(data_corpus_ukmanifestos_sentences),
    docvars(data_corpus_ukmanifestos_sentences),
    stringsAsFactors = FALSE
)

# use spacyr to tokenize manifestos to sentence-level
spacy_initialize(model = "en")

data_uk_man_sentences <- spacy_tokenize(data_uk_man,
                                        remove_separators = FALSE,
                                        what = "sentence", 
                                        output = "data.frame") 

data_uk_man_sentences <- rename(data_uk_man_sentences, text = token)


# merge metadata
data_uk_man_sentences <- left_join(data_uk_man_sentences, 
                                   select(data_uk_man, -c("text")),
                                   by = "doc_id")


########################
## 3. Import additional manifestos from 2010, 2015, and 2017 elections ----
########################

## Note: data retrieved from http://polidoc.net (2010 and 2015) and from parties' websites (2019)

data_uk_1519 <- readtext("tests/data_creation/data_uk_manifestos_2015-2019.zip",
                            ignore_missing_files = TRUE,
                            encoding = "utf-8",
                            docvarsfrom = "filenames",
                            docvarnames = c("Year", "Party"))


# tokenize documents to the level of sentences
data_uk_man_1519_sentences <- spacy_tokenize(data_uk_1519,
                                             remove_separators = FALSE,
                                             what = "sentence", 
                                             output = "data.frame") 

# merge metadata
data_uk_man_1519_sentences <- left_join(data_uk_man_1519_sentences, 
                                   select(data_uk_1519, -c("text")),
                                   by = "doc_id")

data_uk_man_1519_sentences <- rename(data_uk_man_1519_sentences, text = token)


########################
## 4. Combine manifestos and create corpus ----
########################

dat_uk_man_crowdcoded_clean$Year <- as.integer(dat_uk_man_crowdcoded_clean$Year)

data_uk_manifestos <- bind_rows(data_uk_man_sentences, 
                                dat_uk_man_crowdcoded_clean, 
                                data_uk_man_1519_sentences)


# create unique id for each sentence and select only necessary variables
data_uk_manifestos_selectvars <- data_uk_manifestos %>% 
    select(-c(party_year, sentenceid)) %>% 
    mutate(year = as.factor(Year)) %>% 
    select(text,
           party = Party,
           year,
           starts_with("crowd_"))

# create a quanteda corpus
corp <- corpus(data_uk_manifestos_selectvars)

# count the number of tokens per sentence (after removing punctuation characters)
docvars(corp, "ntoken_sent") <- ntoken(corp, remove_punct = TRUE)

# remove observation with 0 tokens (i.e. only punctuation character)
corp_small <- corp %>% 
    corpus_subset(ntoken_sent > 0)

# transform back to data frame to create nice doc_id after having removed "empty" sentences
dat_corpus <- data.frame(
    text = texts(corp_small),
    docvars(corp_small),
    stringsAsFactors = FALSE
)

dat_corpus <- dat_corpus %>% 
    arrange(year, party) %>% 
    group_by(party, year) %>% 
    mutate(sentence_no = 1:n()) %>% 
    mutate(doc_id = paste(party, year, sentence_no, sep = "_")) %>% 
    ungroup()

# rename variables
dat_corpus_renamed <- dat_corpus %>% 
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

dat_corpus_renamed <- dat_corpus_renamed %>% 
  select(doc_id, text, party, partyname, year, 
         crowd_econsocial_label, crowd_econsocial_mean,
         crowd_econsocial_n, crowd_immigration_label, 
         crowd_immigration_mean, 
         crowd_immigration_n) 

dat_corpus_renamed$crowd_econsocial_label <- factor(dat_corpus_renamed$crowd_econsocial_label)
dat_corpus_renamed$crowd_immigration_label <- factor(dat_corpus_renamed$crowd_immigration_label)
dat_corpus_renamed$year <- as.integer(as.character(dat_corpus_renamed$year))
dat_corpus_renamed$party <- as.factor(dat_corpus_renamed$party)
dat_corpus_renamed$partyname <- as.factor(dat_corpus_renamed$partyname)
dat_corpus_renamed$crowd_immigration_n <- as.integer(dat_corpus_renamed$crowd_immigration_n)
dat_corpus_renamed$crowd_econsocial_n <- as.integer(dat_corpus_renamed$crowd_econsocial_n)

# create final corpus
data_corpus_manifestosentsUK <- corpus(dat_corpus_renamed,
                                       docid_field = "doc_id")

# add corpus to package
usethis::use_data(data_corpus_manifestosentsUK, overwrite = TRUE)
