#' LIX readability score for Norwegian
#'
#' A function for calculating the LIX readability score
#'
#' @usage liks(actual, pred)
#'
#' @param n_words Integer specifying number of words in a document
#' @param n_breaks Integer specifying number of breaks (period, comma, capital letters)
#' @param n_longwords Integer specifying number of words longer than six letters in a document
#'
#'
#' @examples
#' # To be done
#' @export
#'
lix <- function(n_words, n_breaks, n_longwords){
  lix <- (n_words / n_breaks) + ((n_longwords * 100) / n_words)
  return(lix)
}

# To-do for this function:
# Make the input be vector(s) of text
# Possibly also allow corpus from quanteda or/and tm?

# Start of a test:
# lix
#
#
# test <- "Kartleggingen av datamaterialet gjennom innholdsanalyse bærer også med seg noen metodologiske problemer. Ordene som identifiserer utenrikspolitisk vektlegging (tabell 2) ble hovedsakelig satt sammen på grunnlag av egen kunnskap om utenrikspolitikk og ved å studere den historiske konteksten til hver president. Gitt at listen virkelig gir en god representasjon av det underliggende målet (utenrikspolitiske ytringer), er resultatene ganske reliable. Samme sett av tekster og den samme automatiske prosedyren vil gi identiske resultater. Validiteten er det verre med; den reduseres kraftig dersom sentrale ord eller ordgrupperinger blir utelatt. Likevel vil det være umulig å finne en liste av ord og ordgrupperinger som er uttømmende, og på et punkt vil ikke flere gi mer informasjon til estimeringen. Spørsmålet, som ikke kan besvares her, er om denne listen er nær dette punktet. Utvalgsmetoden er uansett bedre enn å velge taler etter preferanse, fordi en slik utvelgingsmetode fort kan føre til utvalg som bekrefter de foreliggende hypotesene"
# n_words <- stringi::stri_count_words(test)
# n_breaks <- stringr::str_count(test, "\\s|\\.|\\.\\s|\\;|\\:|\\?")
# n_longwords <- length(which(nchar(unlist(strsplit(test, "\\s|\\.|\\.\\s|\\;|\\:|\\?"))) > 6))
#
# test2 <- readLines("https://www.idunn.no/ip/2015/01/tilbakeholden_og_ekspansiv_liberalisme_-_om_utenrikspoliti")
#
# test2[[496]]
