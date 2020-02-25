#' Sentence-level corpus of UK party manifestos 1945--2017, partially annotated
#'
#' @description A text corpus of sentences from publicly available party manifestos from the
#' United Kingdom, published between 1945 and 2017.  Some manifestos sentences
#' have been rated in terms of the direction of policy using crowd-sourced coders.
#'
#' @description The manifestos from the
#' three main parties (Labour Party, Conservatives, Liberal Democrats) between
#' 1987 and 2010 have been labelled as Economic Policy, Social
#' Policy, or Other, and rated in terms of the direction of Economic Policy and
#' Social Policy.  All party
#' manifestos from the 2010 General Election have been crowd-coded in terms of
#' immigration policy, and the direction of immigration policy. For more
#' information on the coding approach see
#' [Benoit et al. (2016)](https://doi.org/10.1017/S0003055416000058).
#'
#' @description The
#' corpus contains the aggregated crowd coding values on the level of sentences.
#' Note that the segmentation into sentences does not always work correctly due
#' to missing punctuation. See Examples for how to remove very short and very
#' long sentences using [corpus_trim][quanteda::corpus_trim].
#' @format
#'   The corpus consists of 69,280 documents (i.e. sentences) and includes the following
#'   document-level variables: \describe{
#'   \item{party}{factor; abbreviation of the party that wrote the manifesto.}
#'   \item{partyname}{factor; party that wrote the manifesto.}
#'   \item{year}{integer; 4-digit year of the election.}
#'   \item{crowd_econsocial_label}{factor; indicates the majority label assigned
#'   by crowd workers (Economic Policy, Social Policy, or Neither). The variable
#'   has missing values (`NA`) for all non-annotated manifestos.}
#'   \item{crowd_econsocial_mean}{numeric; the direction of statements coded as
#'   "Economic Policy" or "Social Policy" based on the aggregated crowd codings.
#'   The variable is the mean of the scores assigned by the workers workers who
#'   coded the sentence and who allocated the sentence to the "majority"
#'   category. The variable ranges from -2 to +2.
#'
#'   For the statements aggregated as "Economic" Policy, -2 corresponds to "Very
#'   left"; +2 corresponds to "Very right". For the statements aggregated as
#'   "Social Policy"  -2 corresponds to "Very liberal"; +2 corresponds to "Very
#'   conservative". The variable has missing values (NA) for all sentences that
#'   were aggregated as "Neither" and for all non-annotated manifestos.)}
#'   \item{crowd_econsocial_n}{integer; the number of coders who contributed to the
#'   mean score `crowd_econsocial_mean`.}
#'   \item{crowd_immigration_label}{Factor indicating whether the majority of
#'   crowd workers labelled a sentence as referring to immigration or not. The
#'   variable has missing values (`NA`) for all non-annotated manifestos.}
#'   \item{crowd_immigration_mean}{numeric; the direction
#'   of statements coded as "Immigration" based on the aggregated crowd codings.
#'   The variable is the mean of the scores assigned by workers who coded a
#'   sentence and who allocated the sentence to the "Immigration" category. The
#'   variable ranges from -1 ("Negative and closed immigration policy") to +1
#'   (Favorable and open immigration policy). The variable has missing values
#'   (`NA`) for all non-annotated manifestos or if a sentence was not coded as
#'   referring to immigration policy based on the aggregation of crowd codings.}
#'   \item{crowd_immigration_n}{integer; the number of coders who
#'   contributed to the
#'   mean score `crowd_immigration_mean`.}
#'   }
#' @examples
#' \donttest{
#' library("quanteda")
#'
#' # remove very short and very long sentences
#' corp_trimmed <-
#'     corpus_trim(data_corpus_manifestosentsUK, min_ntoken = 1, max_ntoken = 80)
#'
#' # keep only crowd coded manifestos (with respect to economic and social policy)
#' corp_crowdeconsocial <-
#'     corpus_subset(data_corpus_manifestosentsUK, !is.na(crowd_econsocial_label))
#'
#' # keep only crowd coded manifestos (with respect to immigration policy)
#' corp_crowdimmig <-
#'     corpus_subset(data_corpus_manifestosentsUK, !is.na(crowd_immigration_label))
#' }
#' @references Benoit, K., Conway, D., Lauderdale, B.E., Laver, M., & Mikhaylov, S. (2016).
#'   [Crowd-sourced Text Analysis:
#'   Reproducible and Agile Production of Political Data](https://doi.org/10.1017/S0003055416000058).
#'   *American Political Science Review*, 100,(2), 278--295.
#' @format
#'  A [corpus][quanteda::corpus] object.
#' @keywords data
"data_corpus_manifestosentsUK"


#' Crowd-labelled sentence corpus from a 2010 EP debate on coal subsidies
#'
#' @description A multilingual text corpus of speeches from a European
#'   Parliament debate on coal subsidies in 2010, with individual crowd codings
#'   as the unit of observation.  The sentences are drawn from officially
#'   translated speeches from a debate over a European Parliament debate
#'   concerning a Commission report proposing an extension to a regulation
#'   permitting state aid to uncompetitive coal mines.
#'
#' @description Each speech is available in six languages: English, German,
#'   Greek, Italian, Polish and Spanish. The unit of observation is the
#'   individual crowd coding of each natural sentence. For more information on
#'   the coding approach see
#'   [Benoit et al. (2016)](https://doi.org/10.1017/S0003055416000058).
#' @format
#'   The corpus consists of 16,806 documents (i.e. codings of a sentence) and includes the following
#'   document-level variables: \describe{
#'   \item{sentence_id}{character; a unique identifier for each sentence}
#'   \item{crowd_subsidy_label}{factor; whether a coder labelled the sentence
#'   as "Pro-Subsidy", "Anti-Subsidy" or "Neutral or inapplicable"}
#'   \item{language}{factor; the language (translation) of the speech}
#'   \item{name_last}{character; speaker's last name}
#'   \item{name_first}{character; speaker's first name}
#'   \item{ep_group}{factor; abbreviation of the EP party group of the speaker}
#'   \item{country}{factor; the speaker's country of origin}
#'   \item{vote}{factor; the speaker's vote on the proposal (For/Against/Abstain/NA)}
#'   \item{coder_id}{character; a unique identifier for each crowd coder}
#'   \item{coder_trust}{numeric; the "trust score" from the Crowdflower platform used to code the
#'    sentences, which can theoretically range between 0 and 1. Only coders with trust scores above
#'    0.8 are included in the corpus.}
#'   }
#' @references Benoit, K., Conway, D., Lauderdale, B.E., Laver, M., & Mikhaylov,
#'   S. (2016). [Crowd-sourced
#'   Text Analysis: Reproducible and Agile Production of Political Data](https://doi.org/10.1017/S0003055416000058).
#'   *American Political Science Review*, 100,(2), 278--295.
#' @format
#'  A [corpus][quanteda::corpus] object.
#' @keywords data
"data_corpus_EPcoaldebate"

#' Large Movie Review Dataset from Maas et. al. (2011)
#' 
#' A corpus object containing a dataset for sentiment classification containing
#' 25,000 highly polar movie reviews for training, and 25,000 for testing, from
#' Maas et. al. (2011).
#' @format The corpus docvars consist of:
#'   \describe{
#'   \item{docnumber}{serial (within set and polarity) document number}
#'   \item{rating}{user-assigned movie rating on a 1-10 point integer scale}
#'   \item{set}{used for test v. training set}
#'   \item{polarity}{either `neg` or `pos` to indicate whether the 
#'     movie review was negative or positive.  See Maas et al (2011) for the 
#'     cut-off values that governed this assignment.}
#'   }
#' @references Andrew L. Maas, Raymond E. Daly, Peter T. Pham, Dan Huang, Andrew
#'   Y. Ng, and Christopher Potts. (2011).
#'   "[Learning
#'   Word Vectors for Sentiment Analysis](http://ai.stanford.edu/~amaas/papers/wvSent_acl2011.pdf)". The 49th Annual Meeting of the
#'   Association for Computational Linguistics (ACL 2011).
#' @source <http://ai.stanford.edu/~amaas/data/sentiment/>
#' @keywords data
"data_corpus_LMRD"
