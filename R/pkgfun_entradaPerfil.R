

#' Obtenha a entrada de dados para modelos horarios de perfil
#'
#' @param tipoEntrada tipo de entrada do modelo:\cr
#' DEFAULT - Entrada default para modelo de perfil\cr
#' ML_PERFILFER - Entrada para perfil de feriados
#' @param diaSemana filtro de dias da semana de 1 a 7 (dom a sab)
#' @param hora filtro de horas do dia de 0 a 23
#' @param lagsmodelo lags dos modelos em horas
#' @param serie serie de dados a serem estruturados
#' @param hv indicador de horario de verao para filtro
#' @param diaAno dias do ano para filtro
#' @param lagsdsFer lags em dia de semana em relacao a feriados
#' @param tpFer tipo de feriados
#' @param addHourFac adiciona coluna de dummy de horas
#'
#' @return data.table com os dados
#'
entradaPerfil <- function(serie,
                          tipoEntrada = c("DEFAULT","ML_PERFILFER"), hv = c(0,1),
                          diaSemana = 1:7, hora = 0:23,diaAno = 1:366,lagsmodelo = c(168,336),
                          lagsdsFer = NULL,tpFer = NULL,addHourFac = TRUE){
   # Validacoes
   tipoEntrada <- validaEntrada(c("DEFAULT","ML_PERFILFER"),tipoEntrada)
   # Agregacao da Serie Horaria em Diaria e normalizacao
   seriehist <- copy(serie)


   if(tipoEntrada == "DEFAULT"){
      return(entradaPerfilDefault(seriehist,lagsmodelo,diaSemana,hora,diaAno,hv,F,F))
   } else if(tipoEntrada == "ML_PERFILFER"){
      return(entradaPerfilFeriadoReg(seriehist,hrlag = lagsmodelo,dslag = lagsdsFer,hora = hora,
                                     diaAno = diaAno,hv = hv,tpFer = tpFer,addHourFac = addHourFac))
   } else {
      stop("Tipo de entrada nao e uma opcao valida")
   }
}

entradaPerfilDefault <- function(seriehist,lagsmodelo,diaSemana = 1:7,hora = 0:23,diaAno = 1:366,hv = 0:1,diaEsp = FALSE,diaEspLag = FALSE){
   seriehist_lags <- obtemLagsHora(seriehist,lagsmodelo,diaSemana,hora,diaAno,hv,diaEsp,diaEspLag)
   return(seriehist_lags)
}



inclui_CargaMediaRef <- function(seriehist,normConfig){
   const <- getNormalizaCarga(normConfig,seriehist[Prev == F])
   seriehist[,CargaMed := mean(Carga,na.rm = T)/const,.(Data)]
   seriehist[is.nan(CargaMed),CargaMed := NA_real_]
}

entradaPerfilFeriadoReg <- function(seriehist,dslag = c(1,7),hrlag = NULL,hora = 0:23,diaAno = 1:366,hv = 0:1,tpFer = 1:12,addHourFac = TRUE){
   if(!is.null(hrlag)){
      seriehist <- obtemLagsHora(seriehist,lags = hrlag,hora = hora,diaAno = diaAno,diaEsp = T)
   }
   seriehist_lags <- obtemLagsDiaSemana(seriehist,dslag,diaAno,hv,tpFer,hora)
   if(addHourFac){
      seriehist_lags[,Hora := factor(hour(DataHora),labels = sprintf('%02d',0:23))]
   }
   if(length(unique(seriehist_lags$DiaEspecial)) > 1){
      seriehist_lags[,DiaEspecial := factor(paste0("F",sprintf('%02d',DiaEspecial)),levels = paste0("F",sprintf('%02d',0:12)))]
   } else {
      seriehist_lags[,DiaEspecial := NULL]
   }
   seriehist_lags[,HV := NULL]
   return(seriehist_lags)
}



