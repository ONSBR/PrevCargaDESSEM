#' PrevCargaDESSEM
#'
#' Funcoes de execucao do modelo de previsao de carga PrevCargaDESSEM
#'
#' @section Funcoes para uso no pacote prevcargadessem:
#' \itemize{
#' \item\code{\link{obterModelos}} - Obtem os modelos disponiveis para execucao
#' \item\code{\link{previsordessem}} - Execute o PrevCargaDESSEM
#' }
#'
#' @docType package
#' @name prevcargadessem
#'
#' @import data.table
#' @import feasts
#' @import urca
#' @import parallel
#' @importFrom WeightSVM tune_wsvm
#' @importFrom lubridate hours %m+% date minutes is.Date with_tz force_tz %m-% day
#' @importFrom stats predict
#' @importFrom neuralnet neuralnet
#' @importFrom jsonlite fromJSON
#' @importFrom fable as_tsibble ARIMA %>%
#' @importFrom fabletools model forecast
#' @importFrom rlist list.filter
#' @importFrom stats as.formula complete.cases lm sd splinefun
#'
NULL




