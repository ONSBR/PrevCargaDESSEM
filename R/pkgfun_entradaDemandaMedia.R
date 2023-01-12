
#' Obtenha a entrada de dados para modelos de demanda media diaria
#'
#' @param tipoEntrada tipo de entrada do modelo: \cr
#' ML_DEFAULT - Entrada default para modelos de Machine Learning com Lags,Lags de Dias Especiais e Dummyes de Dias Especiais\cr
#' TS_DEFAULT - Entrada default para modelos de Series Temporais com Dummyes de: Dias Especiais, Dias da Semana e Meses\cr
#' TS_FACTOR - Entrada para modelos de Series Temporais com colunas de Dias Especiais, Dias da Semana e Meses em fatores
#' @param diaSemana filtro de dias da semana de 1 a 7 (dom a sab)
#' @param diaAno filtro de dias do ano de 1 a 366
#' @param lagsmodelo lags dos modelos (somente para as entradas de Machine Learning - ML)

#' Estrutura os dados de entrada para os modelos de demanda media
#'
#' @param serie serie de dados em demanda media a serem estruturados
#' @param tipoEntrada tipo de entrada do modelo: \cr
#' ML_DEFAULT - Entrada default para modelos de Machine Learning com Lags,Lags de Dias Especiais e Dummyes de Dias Especiais\cr
#' TS_DEFAULT - Entrada default para modelos de Series Temporais com Dummyes de: Dias Especiais, Dias da Semana e Meses\cr
#' TS_FACTOR - Entrada para modelos de Series Temporais com colunas de Dias Especiais, Dias da Semana e Meses em fatores\cr
#' ML_LOCAL - Entrada default para modelos de Machine Learning com Lags,Lags de Dias Especiais e Dummyes de Dias Especiais
#' @param diaSemana dias da semana para filtro
#' @param diaAno dias do ano para filtro
#' @param lagsmodelo lags de demanda e ou exogena
#'
#' @return data.table com os dados
#'
#'
entradaDemandaMedia <- function(serie,
                                tipoEntrada = c("ML_DEFAULT","ML_LOCAL","TS_DEFAULT","TS_FACTOR"),
                                diaSemana = 1:7,
                                diaAno = 1:366,
                                lagsmodelo = c(1,2,7)){
   # Validacoes
   tipoEntrada <- validaEntrada(c("ML_DEFAULT","ML_LOCAL","TS_DEFAULT","TS_FACTOR"),tipoEntrada)

   seriediaria <- copy(serie)

   # Tipos Entradas
   if(tipoEntrada == "ML_DEFAULT"){
      #seriediaria <- aggregaSerie(dadosHist,tipotemp,normaliza,constnorm,constnormtemp)
      seriediaria_ml <- entradaDemandaMediaDefault_ML(seriediaria,lagsmodelo,diaSemana,diaAno)
      return(list(serieDiaria = seriediaria, serieDiariaModelo = seriediaria_ml))
   } else if (tipoEntrada == "ML_LOCAL"){
      #seriediaria <- aggregaSerie(deckLeitora$SERIEHIST,tipotemp,normaliza,constnorm,constnormtemp)
      seriediaria_ml <- entradaDemandaMediaLocal_ML(seriediaria,lagsmodelo,diaSemana,diaAno)
      return(list(serieDiaria = seriediaria, serieDiariaModelo = seriediaria_ml))
   }else if (tipoEntrada == "TS_DEFAULT"){
      #seriediaria <- aggregaSerie(deckLeitora$SERIEHIST,tipotemp,normaliza,constnorm,constnormtemp)
      seriediaria_ts <- entradaTimeSeriesDefault(seriediaria)
      return(list(serieDiaria = seriediaria, serieDiariaModelo = seriediaria_ts))
   } else if (tipoEntrada == "TS_FACTOR"){
      #seriediaria <- aggregaSerie(deckLeitora$SERIEHIST,tipotemp,normaliza,constnorm,constnormtemp)
      seriediaria_ts <- entradaTimeSeriesFactor(seriediaria)
      return(list(serieDiaria = seriediaria, serieDiariaModelo = seriediaria_ts))
   }else {
      stop("Tipo de entrada nao e uma opcao valida")
   }
}


##### DEMANDA MEDIA DIARIA - MODELO DEFAULT MACHINE LEARNING
entradaDemandaMediaDefault_ML <- function(seriediaria,lagsmodelo,diaSemana,diaAno){
   seriediariaD_lags <- copy(seriediaria)
   # Obtendo lags das exogenas
   exogenas <- getExoCols(names(seriediariaD_lags))
   if(length(exogenas)>0){
      seriediariaD_lags <- obtemLagsExogenas(lagsmodelo,exogenas,seriediariaD_lags)
   }
   # Obtendo lags da carga
   seriediariaD_lags <- obtemLags(lagsmodelo,seriediariaD_lags,diaSemana,diaAno,T,F)
   # Inclui horario de verao, dias especiais e temperatura
   seriediariaD_lags <- inclui_dummy_DE(seriediariaD_lags)
   seriediariaD_lags <- inclui_dummy_dia(seriediariaD_lags)
   seriediariaD_lags <- inclui_dummy_mes(seriediariaD_lags)
   seriediariaD_lags[,HV := NULL]
   return(seriediariaD_lags)
}

##### DEMANDA MEDIA DIARIA - MODELO LOCAL MACHINE LEARNING
entradaDemandaMediaLocal_ML <- function(seriediaria,lagsmodelo,diaSemana,diaAno){
   seriediaria_lags <- copy(seriediaria)
   # Obtendo lags das exogenas
   exogenas <- getExoCols(names(seriediaria_lags))
   if(length(exogenas)>0){
      seriediaria_lags <- obtemLagsExogenas(lagsmodelo,exogenas,seriediaria_lags)
   }
   # Obtendo lags da carga
   seriediaria_lags <- obtemLags(lagsmodelo,seriediaria_lags,diaSemana,diaAno,F,T,T)
   seriediaria_lags[,HV := NULL]
   return(seriediaria_lags)
}


##### DEMANDA MEDIA DIARIA - MODELO DEFAULT TIME SERIES
entradaTimeSeriesDefault <- function(seriediaria){
   seriediaria_ts <- inclui_dummy_DE(seriediaria,filtra_nc = F)
   seriediaria_ts <- inclui_dummy_dia(seriediaria_ts)
   seriediaria_ts <- inclui_dummy_mes(seriediaria_ts)
   return(seriediaria_ts)
}

##### DEMANDA MEDIA DIARIA - MODELO DEFAULT TIME SERIES
entradaTimeSeriesFactor <- function(seriediaria){
   seriediaria_ts <- copy(seriediaria)
   seriediaria_ts[,DiaEspecial := factor(paste0("F",sprintf('%02d',DiaEspecial)),levels = paste0("F",sprintf('%02d',0:12)))]
   seriediaria_ts[,DiaSemana := factor(paste0("D",wday(Data)),levels = paste0("D",1:7))]
   seriediaria_ts[,Mes := factor(paste0("M",sprintf('%02d',month(Data))),levels = paste0("M",sprintf('%02d',1:12)))]

   # seriediaria_ts[,TempMen := max(TempMax),.(year(Data),Mes)]
   # seriediaria_ts[,TempMax := TempMax - TempMen]
   # seriediaria_ts[,TempMen := NULL]

   return(seriediaria_ts)
}

##### Inclui DUMMY de feriados
inclui_dummy_DE <- function(serie,filtra_nc = TRUE){
   serie_lags_DE <- serie
   serie_lags_DE[,DiaEspecial_c := paste0("F",sprintf('%02d',DiaEspecial))]
   if(filtra_nc){
      serie_lags_DE <- serie_lags_DE[DiaEspecial_c != "F12"]
   }
   serie_lags_DE <- dcast(serie_lags_DE,
                          as.formula(paste0(paste(names(serie_lags_DE[,-"DiaEspecial_c"]),collapse = "+"),"~DiaEspecial_c")),
                             fun.aggregate = length)
   serie_lags_DE[,F00 := NULL]
   serie_lags_DE[,DiaEspecial := NULL]
   return(serie_lags_DE)
}

##### Inclui DUMMY de dias da semana
inclui_dummy_dia <- function(serie){
   serie[,DS := paste0("D",wday(Data))]
   serie <- dcast(serie, as.formula(paste0(paste(names(serie[,-"DS"]),collapse = "+"),"~DS")),
                          fun.aggregate = length)
   serie[,D1 := NULL]
   return(serie)
}

##### Inclui DUMMY de meses
inclui_dummy_mes <- function(serie){
   serie[,MS := paste0("M",sprintf('%02d',month(Data)))]
   serie <- dcast(serie, as.formula(paste0(paste(names(serie[,-"MS"]),collapse = "+"),"~MS")),
                  fun.aggregate = length)
   serie[,M01 := NULL]
   return(serie)
}

