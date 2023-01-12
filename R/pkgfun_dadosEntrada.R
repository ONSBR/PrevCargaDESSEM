#' Transforma os decks de entrada em arquivos de Series Historicas e de Dados Diarios
#'
#' @param deck Deck de entrada
#' @param horizonteprevisao Horizonte de previsao Semana Operativa ou 10 dias c("SEMOPER","10DIAS")
#' @param dataInicioPrev data de inicio da previsao
#'
#' @return Dados para entrada de modelos
#'
TransformaDeck <- function(deck,dataInicioPrev,horizonteprevisao = c("SEMOPER","10DIAS")){
   #------------------------------------------------------ -
   # HORIZONTE DE PREVISAO ====
   #------------------------------------------------------ -
   if(!is.Date(dataInicioPrev)){
      dataInicioPrev <- as.Date(dataInicioPrev)
   }
   if(is.numeric(horizonteprevisao)) horizonteprevisao <- c("SEMOPER","10DIAS")[horizonteprevisao + 1]
   if (horizonteprevisao[1]=="SEMOPER") {
      if(wday(dataInicioPrev)==6){
         dataFimPrev <- dataInicioPrev + 7
      } else {
         dataFimPrev <- dataInicioPrev + (6 - as.numeric(format(dataInicioPrev + 1, format='%w')))
      }
   } else if(horizonteprevisao[1] == "10DIAS"){
      dataFimPrev <- dataInicioPrev + 9
      maxDataDisp <- max(as.Date(format(deck[["EXOGENAPREV"]]$DataHora,'%F')))
      if(dataFimPrev > maxDataDisp){
         message("Exogena prevista nao disponivel para o horizonte desejado")
         message("Ajustanto horizonte...")
         dataFimPrev <- maxDataDisp
      }
   } else {
      stop("Variavel Horizonte de Previsao fora dos padroes suportados")
   }

   message(paste("Horizonte de previsao informado:",dataInicioPrev,"a",dataFimPrev))

   dias_previsao=seq(dataInicioPrev, dataFimPrev, "days")
   numdiasprev=length(dias_previsao)

   #------------------------------------------------------ -
   # DADOS DE CARGA E MONTAGEM DA SERIE HISTORICA ====
   #------------------------------------------------------ -

   dtCarga <- copy(deck[["CARGAHIST"]])
   dtCarga[,DataHora := DataHora %m-% hours(1)]
   dtCarga <- dtCarga[,.(DataHora,Data = as.Date(format(DataHora,'%F')),Prev = F,Variavel = "Carga",Valor = Carga)]

   message("CONCLUIDA LEITURA DOS DADOS DE CARGA")



   #================================ =
   # LEITURA DO ARQUIVO COM FERIADOS
   #================================ =

   dtFeriados <- copy(deck[["FERIADOS"]][,.(Data, DiaEspecial = Tipo)])
   message("CONCLUIDA LEITURA DOS DADOS DE FERIADOS")

   #============================================ =
   # LEITURA DO ARQUIVO COM DADOS DE TEMPERATURA ====
   #============================================ =

   dtExogena <- copy(deck[["EXOGENAHIST"]])
   dtExogena[,`:=` (Data = as.Date(format(DataHora,'%F')), Prev = F)]
   dtExogena <- melt(dtExogena,id.vars = c("DataHora","Data","Prev"),variable.name = "Variavel",value.name = "Valor")

   message("CONCLUIDA LEITURA DOS DADOS DE EXOGENA HISTORICA")

   #=============================================== =
   # LEITURA DO ARQUIVO COM PREVISAO DE TEMPERATURA
   #=============================================== =

   dtExogenaPrev <- copy(deck[["EXOGENAPREV"]])
   dtExogenaPrev[,`:=` (Data = as.Date(format(DataHora,'%F')), Prev = T)]
   dtExogenaPrev <- melt(dtExogenaPrev[Data <= dataFimPrev],id.vars = c("DataHora","Data","Prev"),variable.name = "Variavel",value.name = "Valor")
   message("CONCLUIDA LEITURA DOS DADOS DE EXOGENA PREVISTA")

   #====================================== =
   # CRIA DATA.FRAME COM DADOS HISTORICOS
   #====================================== =

   dtExogena <- rbind(dtExogena,dtExogenaPrev)
   dtDados <- rbind(dtCarga,dtExogena)
   dtDados <- dcast(dtDados,DataHora + Data + Prev ~ Variavel,value.var = "Valor")
   dtDados <- dtFeriados[dtDados, on = "Data"]
   dtDados[is.na(DiaEspecial),DiaEspecial := 0]
   dtDados[,HVZ := format(as.POSIXct(format(Data,"%F 12:00:00"),"America/Sao_Paulo"),'%Z')]
   dtDados[HVZ == "-02",HV := 1]
   dtDados[!HVZ == "-02",HV := 0]
   dtDados[,HVZ := NULL]

   DADOSLIDOS= c(deck[names(deck) %in% c("COMBINA","PATAMARES")],
                 list(SERIECOMP=dtDados,DIASPREV = dias_previsao,NDIASPREV = numdiasprev))

   return(DADOSLIDOS)
}


#' Leitura dos decks de entrada
#'
#' @param deck Caminho dos arquivos ou lista contendo os data.frames das tabelas de entrada
#' @param arqsAllowed (opcional) lista de tabelas de entrada para filtro
#'
#' @return lista de data.frames com dados do deck de entrada do modelo
#'
deckEntrada <- function(deck,arqsAllowed = c("CARGAHIST","EXOGENAHIST","COMBINA",
                                             "FERIADOS","PATAMARES","EXOGENAPREV")){
   if(is.character(deck)){
      patFil <- paste(paste0('.*',arqsAllowed,'.*'),collapse = "|")
      arqs <- list.files(deck,pattern = patFil,full.names = T)
      arqs_nomes <-  gsub('^.*_|\\.csv','',list.files(deck,pattern = patFil))
      deck <- lapply(arqs,fread,sep = ";",dec = ",")
      names(deck) <- arqs_nomes
   } else if(is.list(deck)) {
      deck <- deck[arqsAllowed]
      if(!is.data.table(deck[[1]])){
         deck <- lapply(deck,data.table)
      }
   } else {
      stop("Deck de entrada fora dos padroes suportados")
   }
   if(length(deck) > length(arqsAllowed)) stop("Verificar arquivos de entrada, possibilidade de duplicatas")
   if(length(deck) < length(arqsAllowed)) warning(paste("Arquivos faltantes:",
                                                        paste(arqsAllowed[which(!arqsAllowed %in% names(deck))],collapse = ",")))
   return(deck)
}
