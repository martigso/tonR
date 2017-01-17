#' Read CoNLL-type data from Talk of Norway
#'
#' A function for reading the CoNLL data used in the Talk of Norway project for legislative speeches
#' in the Norwegian parliament (1998-2016). Operates as a wrapper for \code{\link[utils]{read.csv}}
#'
#' @usage read.conll(id, keep = "all", rmStopwords = TRUE, rmLength = 1000)
#'
#' @param tonFolder Character vector specifying either absolute or relative path to the talk-of-norway repository folder
#' @param id Character string specifying id of the file to read into R.
#' @param keep Character string that specifies what parts-of-speech should be returned. Possible values are:
#'        "subst", "verb", "sbu", "prep", "det", "adj", "clb", "adv", "pron", "<komma>", "konj".
#' @param rmWords Character vector of either "no" or of words to remove.
#'
#' @family CoNLL
#' @seealso \code{\link[utils]{read.csv}}
#'
#' @examples
#' # If the 'talk-of-norway' repository is placed in the folder below the tonR-package:
#' speech <- read.conll("../talk-of-norway/data/annotations/tale100001.tsv")
#' barplot(table(speech$part_of_speech))
#' @export
read.conll <- function(tonFolder,
                       id,
                       keep = "all",
                       rmWords = "no",
                       rmLength = 1000){

  file <- paste0(tonFolder, "/data/annotations/", id, ".tsv")

  # Reading the data
  conll_df <- read.csv(file, sep = "\t", header = FALSE, stringsAsFactors = FALSE, quote = "",
                       row.names = NULL)
  if(ncol(conll_df) == 5){
    colnames(conll_df) <- c("index", "token", "lemma", "part_of_speech", "features")
  }

  # Reformatting the CoNLL-format from newline = new sentence to numeric sentence count variable
  conll_df$sentence <- 1
  if(nrow(conll_df) >= 2){
    for(i in 2:nrow(conll_df)){
      conll_df$sentence[i] <- ifelse(conll_df$index[i] < conll_df$index[i-1], conll_df$sentence[i-1]+1, conll_df$sentence[i-1])
    }
  }

  # Reordering the variable order
  conll_df <- conll_df[, c("sentence", names(conll_df[1:(ncol(conll_df)-1)]))]

  if(("all" %in% keep) == FALSE){
    conll_df <- conll_df[which(conll_df$part_of_speech %in% keep), ]
  }

  if(rmWords != "no"){
    conll_df <- conll_df[which((conll_df$token %in% rmWords) == FALSE), ]
  }

  return(conll_df)
}
