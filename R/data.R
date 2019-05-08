#' A sentence-level corpus of UK party manifestos, 1945--2017
#' 
#' A text corpus of publicly available, machine-readable party manifestos from the 
#' United Kingdom, published between 1945 and 2017. 
#' Manifestos from the three main parties (Lab, Con, LD) between 1978 and 2010 are crowd-sourced 
#' in terms of Economic Policy and Social Policy, and the direction of Economic Policy 
#' and Social Policy. All manifestos from the 2010 General Election have been 
#' crowd coded in terms of immigration policy, and the direction of immigration policy. 
#' For more information on the coding approach see 
#' \href{https://doi.org/10.1017/S0003055416000058}{Benoit et al. (2016)}.  
#' The corpus contains the aggregated labels on the level of sentences.
#' Note that the segmentation into sentences does not always work correctly due to missing punctuation. 
#' Very short or long sentences can be removed by subsetting the corpus based on the 
#' document-level variable "ntoken_sent" using \link[quanteda]{corpus_subset}.
#' @format 
#'   The corpus consists of 68,947 documents (i.e. sentences). and includes the following 
#'   document-level variables: \describe{
#'   \item{country}{A character indicating the country of the manifesto.}
#'   \item{language}{A character indicating the language of the manifesto.}
#'   \item{party}{A character indicating the party that wrote the manifesto.}
#'   \item{year}{4-digit year of the election.}
#'   \item{class_policyarea}{A factor variable indicating whether a majority of crowd workers 
#'   labeled a sentence as Economic Policy, Social Policy, or Neither. The variable has missing values 
#'   (NA) for all non-annotated manifestos.}
#'   \item{class_policyarea_mean}{The average evaluation used to construct "class_policyarea". 
#'   The variable has missing values (NA) for all non-annotated manifestos.}
#'   \item{class_policyarea_direction}{A factor indicating the direction of a sentence if it was coded as 
#'   Economic Policy or Social Policy by a majority of crowd coders. 
#'   The variable has missing values (NA) for all non-annotated manifestos or if a sentence was coded as
#'   "Not Economic or Social".}
#'   \item{class_policyarea_direction_mean}{The average evaluation used to construct 
#'   "class_policyarea_direction". The variable has missing values (NA) for all non-annotated manifestos or 
#'   if a sentence was coded as "Not Economic or Social".}
#'   \item{class_immigration}{A factor variable indicating whether the majority of crowd workers
#'   labeled a sentence as referring to immigration. The variable has missing values (NA) for all non-annotated manifestos.}
#'   \item{class_immigration_direction}{A factor indicating the direction of a sentence 
#'   (Against, Neutral, Supportive) if it was coded as referring to immigration. 
#'   The variable has missing values (NA) for all non-annotated manifestos or if a sentence was coded not 
#'   coded as referring to immigration policy.}
#'   \item{class_immigration_mean}{A numeric variable, ranging between 0 and 1. 
#'   0 implies that none of the crowd coders labeled the sentence as referring to immigration;
#'   1 implies that all crowd coders labeled the sentence as referring to immigration. 
#'   The variable has missing values (NA) for all non-annotated manifestos or if a sentence was
#'   not coded as referring to immigration policy.}
#'   \item{class_immigration_direction_mean}{A numeric variable, ranging between -1 and 1; a value of 
#'   - 1 indicates that all crowd coders labeled the sentence as "Supportive" of immigration, 
#'   a value of 1 means that all coders labeled the sentences as "Against" immigration.}
#'   \item{crowdcoded_policyarea}{A variable containing logical values indicating whether a sentence 
#'   has been crowd coded with regard to Economic Policy, Social Policy, or Not Economic or Social (TRUE) 
#'   or whether a sentence is unlabeled (FALSE).}
#'   \item{crowdcoded_immigration}{A variable containing logical values indicating whether a sentence 
#'   has been crowd coded with regard to immigration policy (TRUE) or whether a sentence is 
#'   unlabeled (FALSE).}
#'   \item{ntoken_sent}{A numeric variable with the number of tokens of the sentence after 
#'   removing punctuation characters.}
#'   }
#' @references Benoit, K., Conway, D., Lauderdale, B.E., Laver, M., & Mikhaylov, S. (2016). 
#' "\href{https://doi.org/10.1017/S0003055416000058}{Crowd-sourced Text Analysis: 
#'   Reproducible and Agile Production of Political Data}." 
#'   \emph{American Political Science Review} 100(2): 278--295.
#' @format
#'  A \link[quanteda]{corpus} object.
#' @keywords data
"data_corpus_manifestosentsUK"
