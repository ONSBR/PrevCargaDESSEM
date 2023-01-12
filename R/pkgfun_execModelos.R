


execDemandaMedia <- function(dataPrev,serie,config_mdl){
   ### Obtem atraves do arquivo de configuracao a funcao a ser utilizada no modelo
   funcModelo <- get(config_mdl$funcModeloDemandaMedia)
   ### Aplica a funcao de treinamento do modelo especificada no arquivo de configuracao
   mdl <- funcModelo(dataPrev,serie,config_mdl)
   ### Efetua a inferencia pelo modelo treinado
   infer <- predict(mdl$modeloTreinado,mdl$dadosEntrada$serieDiariaModelo[Data == dataPrev])
   ### Atualizando na serie de dados o dado de carga previsto
   serie[Data == dataPrev,Carga := infer[1]]
   return(list(mdl = mdl,infer = infer))
}

execDemandaMediaTS <- function(serie,config_mdl){
   ### Obtem atraves do arquivo de configuracao a funcao a ser utilizada no modelo
   funcModelo <- get(config_mdl$funcModeloDemandaMedia)
   ### Aplica a funcao de treinamento do modelo especificada no arquivo de configuracao
   mdl <- funcModelo(serie,config_mdl)
   ### Efetua a inferencia pelo modelo treinado
   infer <- forecast(mdl$modeloTreinado,mdl$dadosInfer)
   ### Atualizando na serie de dados o dado de carga previsto
   infer <- data.table(infer)[,.(Data,Carga.infer = .mean)]
   addValuesDMRef(serie,infer)
   return(list(mdl = mdl,infer = infer))
}


execPerfil <-function(hora,dataPrev,serie,config_mdl,infer_cm = NULL){
   funcModelo <- get(config_mdl$funcModeloPerfil)
   mdl <- funcModelo(dataPrev,hora,serie,config_mdl)
   dataInfer <- mdl$dadosEntrada[Data == dataPrev]
   if(!is.null(infer_cm)){
      dataInfer[,CargaMed := infer_cm]
   }
   infer <- predict(mdl$modeloTreinado$best.model,dataInfer)
   if(is.null(infer_cm)){
      serie[Data == dataPrev & hour(DataHora) == hora,Carga := infer[1]]
   } else {
      serie[Data == dataPrev & hour(DataHora) == hora,`:=`(CargaMed = infer_cm,Carga = infer[1])]
   }
   return(list(mdl = mdl,infer = infer))
}

execDMPerfil <- function(dataPrev,serieDM,seriePF,config_mdl){
   message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Prevendo demanda media para",dataPrev))
   dm <- execDemandaMedia(dataPrev,serieDM,config_mdl)
   message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Prevendo perfil para",dataPrev))
   perf <- lapply(0:23,execPerfil,dataPrev,seriePF,config_mdl,dm$infer)
   rm(dm)
   rm(perf)
   gc(verbose = F)
}

execDMTSPerfil <- function(datasPrev,serieDM,seriePF,config_mdl,constNorm){
   message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Prevendo demanda media para",paste(datasPrev,collapse = ",")))
   dm <- suppressWarnings(execDemandaMediaTS(serieDM,config_mdl))
   dm$infer[,Carga.infer := Carga.infer/constNorm]
   addValuesDMRef(seriePF,dm$infer,"CargaMed")
   rm(dm)
   gc()
   perf <- execDatasPerfil(datasPrev,seriePF,config_mdl,constNorm)
   rm(perf)
   gc(verbose = F)
}

execDatasPerfil <- function(datasPrev,seriePF,config_mdl,constNorm){
   dats <- lapply(format(datasPrev),execHorasPerfil,seriePF,config_mdl,constNorm)
}

execHorasPerfil <- function(dataPrev,seriePF,config_mdl,constNorm){
   if(sum(seriePF[Data == dataPrev]$DiaEspecial)==0){
      message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Prevendo perfil para",dataPrev))
      perf <- lapply(0:23,execPerfil,dataPrev,seriePF,config_mdl)
   } else {
      message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Prevendo perfil de feriado para",dataPrev))
      RL_perfilFeriado(dataPrev,seriePF,config_mdl,constNorm)
   }
}

processaModelosDMPerfil <- function(deck,config_mdl){
   message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Estruturando series de Perfil e Demanda Media..."))
   serieDM <- aggregaSerie(serieHist = copy(deck$SERIECOMP),
                           tipotemp = config_mdl$exogenaOper,
                           normaliza = any(config_mdl$normalizacao$dm$carga$flag,config_mdl$normalizacao$dm$exogena$flag),
                           constnorm = getNormalizaCarga(config_mdl$normalizacao$dm$carga,deck$SERIECOMP[Prev==F]),
                           constnormtemp = getNormalizaExogena(config_mdl$normalizacao$dm$exogena,deck$SERIECOMP[Prev==F]))

   seriePF <- copy(deck$SERIECOMP)
   inclui_CargaMediaRef(seriePF,config_mdl$normalizacao$perfil$cargadm)
   normalizaSerieRef(serieHist = seriePF,
                     constnorm = config_mdl$normalizacao$perfil$carga$oper,
                     constnormtemp = getNormalizaExogena(config_mdl$normalizacao$exogena,deck$SERIECOMP[Prev==F]))
   message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Executando previsoes..."))
   previsoes <- lapply(format(deck$DIASPREV),execDMPerfil,serieDM,seriePF,config_mdl)
   rm(previsoes)
   rm(serieDM)
   gc()
   message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Desnormalizando previsoes..."))
   seriePF[,CargaMed := CargaMed * getNormalizaCarga(config_mdl$normalizacao$perfil$cargadm,deck$SERIECOMP[Prev==F])]
   seriePF[,Carga := Carga * CargaMed]
   return(seriePF[Prev == T,.(DataHora,Carga)])
}

processaModelosDMTSPerfil <- function(deck,config_mdl){
   message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Estruturando series de Perfil e Demanda Media..."))
   serieDM <- aggregaSerie(serieHist = copy(deck$SERIECOMP),
                           tipotemp = config_mdl$exogenaOper,
                           normaliza = any(config_mdl$normalizacao$carga$flag,config_mdl$normalizacao$exogena$flag) ,
                           constnorm = getNormalizaCarga(config_mdl$normalizacao$carga,deck$SERIECOMP[Prev==F]),
                           constnormtemp = getNormalizaExogena(config_mdl$normalizacao$exogena,deck$SERIECOMP[Prev==F]))

   seriePF <- copy(deck$SERIECOMP)
   inclui_CargaMediaRef(seriePF,config_mdl$normalizacao$perfil$cargadm)
   normalizaSerieRef(serieHist = seriePF,
                     constnorm = config_mdl$normalizacao$perfil$carga$oper,
                     constnormtemp = getNormalizaExogena(config_mdl$normalizacao$perfil$exogena,deck$SERIECOMP[Prev==F]))
   message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Executando previsoes..."))
   execDMTSPerfil(format(deck$DIASPREV),serieDM,seriePF,config_mdl,getNormalizaCarga(config_mdl$normalizacao$perfil$cargadm,deck$SERIECOMP[Prev==F]))
   rm(serieDM)
   gc()
   message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Desnormalizando previsoes..."))
   seriePF[,CargaMed := CargaMed * getNormalizaCarga(config_mdl$normalizacao$perfil$cargadm,deck$SERIECOMP[Prev==F])]
   seriePF[,Carga := Carga * CargaMed]
   return(seriePF[Prev == T,.(DataHora,Carga)])
}

















