#' Calculate F_1 scores based on SVM evaluations
#'
#' A function for calculating F_1 scores based on SVM evaluations
#'
#' @usage Fscores_maker(actual, pred)
#'
#' @param data Data frame containing the actual and predicted values of the property under consideration
#' @param actual Character string of the column name of the the actual values of the property under consideration
#' @param pred Character string of the column name of the the predicted values of the property under consideration.
#'        Must be same length as \code{actual}.
#'
#' @family fscores
#' @seealso \code{\link[e1071]{svm}}
#'          \code{\link[tonR]{Fscores_boot}}
#'
#' @examples
#' data(tonDemo)
#' Fscores_maker(tonDemo, "party_id", "class_token")
#' @export
#'
Fscores_maker <- function(data = NULL, actual, pred){

  if(is.null(data) == FALSE){

    data[, actual] <- factor(data[, actual], levels = c("SV", "A", "Sp", "KrF", "V", "H", "FrP"))
    data[, pred] <- factor(data[, pred], levels = c("SV", "A", "Sp", "KrF", "V", "H", "FrP"))

    baseline <- as.matrix(table(actual = data[, actual], pred = data[, pred]))

  } else {

    actual <- factor(actual, levels = c("SV", "A", "Sp", "KrF", "V", "H", "FrP"))
    pred <- factor(pred, levels = c("SV", "A", "Sp", "KrF", "V", "H", "FrP"))

    baseline <- as.matrix(table(actual, pred))
  }

  N <- sum(baseline)
  N_class <- nrow(baseline)
  correct <- diag(baseline)
  N_actual <- apply(baseline, 1, sum)
  N_pred <- apply(baseline, 2, sum)

  p <- N_actual / N # distribution of instances over the actual classes
  q <- N_pred / N # distribution of instances over the predicted classes

  accuracy <- sum(correct) / N
  precision <- correct / N_pred
  recall <- correct / N_actual
  f1 <- 2 * precision * recall / (precision + recall)
  f1_macro <- 2 * mean(precision) * mean(recall) / (mean(precision) + mean(recall))

  out <- data.frame(labs = rownames(baseline),
                    precision, recall, f1, f1_macro, accuracy,
                    stringsAsFactors = FALSE, row.names = NULL)

  return(out)
}
