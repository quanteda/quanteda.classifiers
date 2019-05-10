#' A sentence-level corpus of annotated or machine-readable UK party manifestos, 1945--2017
#' 
#' A text corpus of publicly available, machine-readable party manifestos from the 
#' United Kingdom, published between 1945 and 2017. 
#' Manifestos from the three main parties (Labour Party, Conservatives, Liberal Democrats) 
#' between 1987 and 2010 are crowd-sourced in terms of Economic Policy and Social Policy, 
#' and the direction of Economic Policy and Social Policy. All manifestos from the 2010 
#' General Election have been crowd coded in terms of immigration policy, 
#' and the direction of immigration policy. 
#' For more information on the coding approach see 
#' \href{https://doi.org/10.1017/S0003055416000058}{Benoit et al. (2016)}.  
#' The corpus contains the aggregated labels on the level of sentences.
#' Note that the segmentation into sentences does not always work correctly due to missing punctuation. 
#' The Examples below show to use \link[quanteda]{corpus_trim} for removing very short and 
#' very long sentences.
#' @format 
#'   The corpus consists of 68,947 documents (i.e. sentences) and includes the following 
#'   document-level variables: \describe{
#'   \item{party}{factor; abbreviation of the party that wrote the manifesto.}
#'   \item{partyname}{factor; party that wrote the manifesto.}
#'   \item{year}{4-digit year of the election.}
#'   \item{crowd_econsocial_label}{A factor variable indicating whether a majority of crowd workers 
#'   labeled a sentence as Economic Policy, Social Policy, or Neither. The variable has missing values 
#'   (NA) for all non-annotated manifestos.}
#'   \item{crowd_econsocial_dir}{A factor indicating the direction of a sentence if it was coded as 
#'   Economic Policy or Social Policy by a majority of crowd coders. 
#'   The variable has missing values (NA) for all non-annotated manifestos or if a sentence was coded as
#'   "Not Economic or Social".}
#'   \item{crowd_econsocial_mean}{Integer indicating the average evaluation used to construct "crowd_econsocial_label". 
#'   The variable has missing values (NA) for all non-annotated manifestos. This variable indicates 
#'   the (dis)agreement between crowd workers. The variable ranges from -1 (all coders labeled sentence 
#'   as referring to Economic Policy) to +1 (all coders labeled sentence as Social Policy).}
#'   \item{crowd_econsocial_dir_mean}{Integer indicating the average evaluation used to construct 
#'   "crowd_econsocial_dir". The variable has missing values (NA) for all non-annotated manifestos or 
#'   if a sentence was coded as "Not Economic or Social". This variable indicates 
#'   the (dis)agreement between crowd workers. The variable ranges from -2 (all coders labeled sentence 
#'   as "Very left" (for Economic Policy) or "Very liberal (for Social Policy)) to +2 
#'   (all coders assigned the label "Very right"/"Very conservative").}
#'   \item{crowd_econsocial_n}{Integer representing the number of coders who contributed to the 
#'   mean score for the sentence.}
#'   \item{crowd_immigration}{Integer indicating whether the majority of crowd workers
#'   labeled a sentence as referring to immigration. The variable has missing values (NA) for all non-annotated manifestos.}
#'   \item{crowd_immigration_dir}{Integer indicating the direction of a sentence 
#'   (Against, Neutral, Supportive) if it was coded as referring to immigration. 
#'   The variable has missing values (NA) for all non-annotated manifestos or if a sentence was coded not 
#'   coded as referring to immigration policy.}
#'   \item{crowd_immigration_mean}{Integer ranging between 0 and 1. 
#'   0 implies that none of the crowd coders labeled the sentence as referring to immigration;
#'   1 implies that all crowd coders labeled the sentence as referring to immigration. 
#'   The variable has missing values (NA) for all non-annotated manifestos or if a sentence was 
#'   not coded as referring to immigration policy.}
#'   \item{crowd_immigration_dir_mean}{Integer ranging between -1 and 1; a value of 
#'   - 1 indicates that all crowd coders labeled the sentence as "Supportive" of immigration, 
#'   a value of 1 means that all coders labeled the sentences as "Against" immigration. 
#'   This variable indicates the (dis)agreement between crowd workers.}
#'   \item{crowd_immigration_n}{Integer representing the number of coders who contributed to the 
#'   mean score for the sentence.}
#'   }
#' @examples 
#' \donttest{
#' # remove very short and very long sentences
#' corp_trimmed <- data_corpus_manifestosentsUK %>% 
#'     quanteda::corpus_trim(min_ntoken = 1, max_ntoken = 80)
#' 
#' # keep only crowd coded manifestos (with respect to economic and social policy)
#' corp_crowdeconsocial <- data_corpus_manifestosentsUK %>% 
#'     corpus_subset(!is.na(crowd_econsocial))
#' 
#' # keep only crowd coded manifestos (with respect to immigration policy)
#' corp_crowdimmig <- data_corpus_manifestosentsUK %>% 
#'     corpus_subset(!is.na(crowd_immigration))
#' }
#' @references Benoit, K., Conway, D., Lauderdale, B.E., Laver, M., & Mikhaylov, S. (2016). 
#'   \href{https://doi.org/10.1017/S0003055416000058}{Crowd-sourced Text Analysis: 
#'   Reproducible and Agile Production of Political Data}.
#'   \emph{American Political Science Review}, 100,(2), 278--295.
#' @format
#'  A \link[quanteda]{corpus} object.
#' @keywords data
"data_corpus_manifestosentsUK"


#' A multilingual text corpus of speeches from a European Parliament debate on coal subsidies in 2010, 
#' with individual crowd codings as the unit of observation
#' 
#' A text corpus of officially translated speeches from a European Parliament debate concerning a Commission 
#' report proposing an extension to a regulation permitting state aid to uncompetitive coal mines. Each
#' speech is available in six languages: English, German, Greek, Italian, Polish and Spanish. 
#' The unit of observation is the individual crowd coding of each natural sentence.
#' For more information on the coding approach see 
#' \href{https://doi.org/10.1017/S0003055416000058}{Benoit et al. (2016)}.  
#' @format 
#'   The corpus consists of 16,806 documents (i.e. codings of a sentence) and includes the following 
#'   document-level variables: \describe{
#'   \item{sentence_id}{Factor variable with a unique identifier for each sentence.}
#'   \item{crowd_subsidy_label}{Factor variable indicating whether a coder labeled the sentence 
#'   as "Pro-Subsidy", "Anti-Subsidy" or "Neutral or inapplicable".}
#'   \item{language}{Factor indicating the language (translation) of the speech.}
#'   \item{name_last}{Last name of the speaker.}
#'   \item{name_first}{First name of the speaker.}
#'   \item{ep_group}{EP party group of the speaker.}
#'   \item{country}{Factor variable indicating the speaker's country of origin.}
#'   \item{vote}{Factor indicating the speaker's vote on the proposal (For/Against/Abstain/NA).}
#'   \item{coder_id}{Factor variable with a unique identifier for each crowd coder.}
#'   \item{coder_trust}{Numeric variable indicating the "trust score" (i.e. the proportion of correctly answered 
#'   gold questions and screeners), which can theoretically range between 0 and 1. Only coders with trust scores above 
#'   0.8 are included in the corpus.}
#'   }
#' @examples 
#' \donttest{
#' # select only sentences from speeches in a specific language (e.g. English)
#' corp_en <- data_corpus_manifestosentsUK %>% 
#'     quanteda::corpus_subset(language == "English")
#' }
#' @references Benoit, K., Conway, D., Lauderdale, B.E., Laver, M., & Mikhaylov, S. (2016). 
#'   \href{https://doi.org/10.1017/S0003055416000058}{Crowd-sourced Text Analysis: 
#'   Reproducible and Agile Production of Political Data}. 
#'   \emph{American Political Science Review}, 100,(2), 278--295.
#' @format
#'  A \link[quanteda]{corpus} object.
#' @keywords data
"data_corpus_EPcoaldebate"
