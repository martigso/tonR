#' Read CoNLL-type data from Talk of Norway
#'
#' A function for reading the CoNLL data used in the Talk of Norway project for legislative speeches
#' in the Norwegian parliament (1998-2016). Operates as a wrapper for \code{\link[utils]{read.csv}}
#'
#' @usage read.conll(file)
#'
#' @param file Character string specifying the path to .tsv-file to read into R
#'
#' @family CoNLL
#' @seealso \code{\link[utils]{read.csv}}
#'
#' @examples
#' # If the 'talk-of-norway' repository is placed in the folder below the tonR-package:
#' speech <- read.conll("../talk-of-norway/data/annotations/tale100001.tsv")
#' barplot(table(speech$part_of_speech))
#' @export
read.conll <- function(file){

  # Testing that the input is a .tsv-file
  if(sapply(strsplit(file, "\\."), "[[", 2) != "tsv"){
    stop("Only .tsv format accepted currently")
  }

  # Reading the data
  conll_df <- read.csv(file, sep = "\t", header = FALSE, row.names = NULL,
                       col.names = c("index", "token", "lemma", "part_of_speech", "features"))

  # Reformatting the CoNLL-format from newline = new sentence to numeric sentence count variable
  conll_df$sentence <- 1
  for(i in 2:nrow(conll_df)){
    conll_df$sentence[i] <- ifelse(conll_df$index[i] < conll_df$index[i-1], conll_df$sentence[i-1]+1, conll_df$sentence[i-1])
  }

  # Reordering the variable order
  conll_df <- conll_df[, c("sentence", names(conll_df[1:(ncol(conll_df)-1)]))]

  return(conll_df)
}
