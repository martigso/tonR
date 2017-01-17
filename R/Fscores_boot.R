#' Bootstrap F_1 scores based on SVM evaluations
#'
#' A function for calculating F_1 scores based on SVM evaluations
#'
#' @usage Fscores_maker(actual, pred)
#'
#' @param data Data frame containing the actual and predicted values of the property under consideration
#' @param actual Character string of the column name of the the actual values of the property under consideration
#' @param pred Character string of the column name of the the predicted values of the property under consideration.
#'        Must be same length as \code{actual}.
#' @param subset_rows A vector containing the rownames for the data to bootstrap. For full data use \code{subset_rows = rownames(data)}
#' @param nsim Integer specifying number of bootstraps to do.
#' @param cores Integer specifying number of cores to simulate on. For Linux users only...sorry Windows.
#'
#' @family fscores
#' @seealso \code{\link[e1071]{svm}}
#'
#' @examples
#' # To be done
#' @export
#'
Fscores_boot <- function(data, actual, pred, subset_rows, nsim = 1000, seed = 8949, cores){

  full_data <- lapply(pred, function(x){
    tonR::Fscores_maker(data[subset_rows, ], actual, x)
  })
  names(full_data) <- pred

  set.seed(seed)

  agg <- list()
  if((suppressWarnings(require(parallel, quietly = TRUE)) == TRUE) & (Sys.info()[1] == "Linux")){
    agg <- lapply(1:nsim, function(x){

      rows <- sample(1:nrow(data[subset_rows, ]), ceiling(nrow(data[subset_rows, ]) / 2))

      sim <- list()
      sim <- lapply(pred, function(y) {
        tmp <- data.frame(tmp_actual = data[rows, actual],
                          tmp_pred = data[rows, y],
                          stringsAsFactors = FALSE)
        tonR::Fscores_maker(tmp, actual = "tmp_actual", pred = "tmp_pred")
      })

      names(sim) <- pred

      return(sim)

    })
  } else {
    agg <- mclapply(1:nsim, function(x){

      rows <- sample(1:nrow(data[subset_rows, ]), ceiling(nrow(data[subset_rows, ]) / 2))

      sim <- list()
      sim <- lapply(pred, function(x) {
        tonR::Fscores_maker(data = NULL, actual = data[rows, actual], pred = data[rows, x])
      })

      names(sim) <- pred

      return(sim)
    })
  }

  fscore_data <- list(full_data = full_data,
                      sims = agg)

  return(fscore_data)
}
