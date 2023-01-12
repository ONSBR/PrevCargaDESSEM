
#' Execute o PrevCargaDESSEM
#'
#' @param deck Caminho para a pasta contendo os arquivos do deck de entrada (a pasta deve conter apenas os arquivos para a execucao) \cr
#' ou lista contento as tabelas nomeadas ("CARGAHIST","EXOGENAHIST","COMBINA","FERIADOS","PATAMARES","EXOGENAPREV")
#' @param dataPrev data do primeiro dia de previsao do modelo (D-1 do DESSEM)
#' @param horizonte "SEMOPER" (default) para previsoes ate o final da semana operativa de dataPrev\cr
#' "10DIAS" para previsoes ate 10 dias a frente, limitado pelo horizonte da variavel exogena prevista
#' @param modelos Modelos que se deseja rodar (default sao todos os disponiveis)\cr
#' A lista de modelos disponiveis pode ser obtida pela funcao \code{obterModelos()}\cr
#' Para execucao do modelo de combinacao, a opcao default deve ser mantida
#' @param saida "CSV" - escreve um arquivo csv no diretorio de saida\cr
#' "DESSEM" - escreve um arquivo txt com os campos dos blocos de carga do modelo DESSEM
#' "PLOT" - emite um plot com as previsoes
#' @param dirSaida Diretorio onde serao escritos os arquivos de saida do modelo
#' @param maxTreads Numero maximo de processos concorrentes do PrevCargaDESSEM (default - numero de nucleos da maquina menos 1)
#'
#' @return data.table com as previsoes feitas pelo do modelo
#' @export
#'
previsordessem <- function(deck,dataPrev,horizonte = c("SEMOPER","10DIAS"),modelos = obterModelos(),
                           saida = c("CSV","DESSEM","PLOT"), dirSaida = ".",maxTreads = NULL){
   ## Se length modelos < length(obterModelos()) somente roda modelos else combina
   ## Verifica se previsao de temperatura condiz com horizonte
   cabecalho("2.0.0")
   deck <- TransformaDeck(deckEntrada(deck),dataPrev,horizonte)
   if(!endsWith(dirSaida,'/')) dirSaida <- paste0(dirSaida,'/')
   arqLog <- paste0(dirSaida,format(Sys.time(),'%Y%m%d%H%M%S'),"_",dataPrev,"_logprevcargadessem.txt")
   dadosMdl <- execModelos(deck,maxTreads,modelos,arqLog,"MULTICL")
   message("CONCLUIDA EXECUCAO DOS MODELOS")
   dadosMdlSH <- do.call("rbind",lapply(modelos,desagregaModeloSH,dadosMdl))
   message("CONCLUIDA DESAGREGACAO SEMI-HORARIA")

   if(!is.null(deck$COMBINA) & !length(modelos) < length(obterModelos())){
      dadosMdlSH <- aplicaPesos(dadosMdlSH,deck$COMBINA)
      message("CONCLUIDA COMBINACAO DE MODELOS")
   }

   dadosMdlSH[,Passo := NULL]

   if("PLOT" %in% saida){
      plotOutputData(dadosMdlSH,dirSaida)
      message("CONCLUIDA SAIDA GRAFICA")
   }

   if("CSV" %in% saida){
      writeOutCSV(dadosMdlSH,dataPrev,dirSaida)
      message("CONCLUIDA SAIDA CSV SEMI-HORARIA")
   }

   if("DESSEM" %in% saida & "Combinado" %in% dadosMdlSH$Modelo){
      if(!"PATAMARES" %in% names(deck)){
         warning("Tabela de patamares faltante, impossivel gerar bloco DESSEM")
      } else {
         writeOutDP(dadosMdlSH,deck$PATAMARES,dataPrev,dirSaida)
         message("CONCLUIDA SAIDA TXT COM ESTRUTURA PARA ENTRADA NOS BLOCOS DE CARGA DO MODELO DESSEM")
      }
   }
   message("PROCESSO FINALIZADO")
   return(dadosMdlSH)
}


