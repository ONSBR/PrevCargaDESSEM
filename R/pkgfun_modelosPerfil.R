

SVMhorario <- function(dataPrev,hora,serie,param_entrada){
   cfg <- obterDiaEspPerfilConfig(dataPrev,serie,param_entrada)
   funcEntrada <- get(param_entrada$funcDadosPerfil)
   dadosEntrada <- funcEntrada(serie = serie,
                               tipoEntrada = cfg$tpEntrada,
                               diaSemana = cfg$ds,
                               hora = hora,
                               diaAno = cfg$diasAno,
                               lagsmodelo = cfg$hrlag,
                               hv = serie[Data == dataPrev]$HV[1],
                               lagsdsFer = cfg$lagds,
                               tpFer = cfg$diaEsp,
                               addHourFac = F)
   formulaSVM <- getFormula(names(dadosEntrada[,-param_entrada$colunaRemoverPerfil,with = F]))
   dadosTreinamento <- dadosEntrada[!is.na(Carga)]
   #message(paste(param_entrada$nomModelo,format(Sys.time(),'- %F %T'),"-",nrow(dadosTreinamento),"amostras no conjunto de treinamento de perfil"))
   modeloSVM <- tune_wsvm(formulaSVM,iter.max=500,
                          weight=getSVMWeights(param_entrada$alfa,nrow(dadosTreinamento)),
                          data=dadosTreinamento,
                          ranges = param_entrada$ranges,
                          kernel = param_entrada$kernel)
   return(list(modeloTreinado = modeloSVM,dadosEntrada = dadosEntrada, parametros = param_entrada))
}

RL_perfilFeriado <- function(dataPrev,seriePF,config_mdl,constNorm){
   serie <- copy(seriePF)
   serie[,CargaMed := CargaMed * constNorm]
   cmed <- serie[Data == dataPrev]$CargaMed[1]
   serie[,Carga := Carga * CargaMed]
   serie[,CargaMed := NULL]
   diaEsp <- serie[Data == dataPrev]$DiaEspecial[1]
   infoFer <- list.filter(config_mdl$janelaDadosFer,diaEsp %in% tipoDiaEsp)[[1]]
   diaAno <- obtemJanelaDiaria(dataPrev,infoFer$janelaDados)
   HV <- serie[Data == dataPrev]$HV[1]
   sdHist <- obtemSD(serie[Prev == F],diaAno = diaAno,hv = HV,tpFer = diaEsp)
   padronizaCargaHorariaRef(serie)
   entradamdl <- entradaPerfilFeriadoReg(serie,
                                         dslag = infoFer$lagsdiasemana,
                                         diaAno = diaAno,
                                         hv = HV,
                                         tpFer = diaEsp)
   #message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'),"-",nrow(entradamdl[!is.na(Carga)]),"amostras no conjunto de treinamento de perfil de feriado"))
   mdlformula <- getFormula(names(entradamdl[,-config_mdl$colunaRemoverPerfil,with = F]))
   mdl <- lm(mdlformula,entradamdl[!is.na(Carga)])
   infer <- predict(mdl,entradamdl[Data == dataPrev])
   infer <- (infer*sdHist + cmed)
   cmedn <- mean(infer)
   infer <- infer/cmedn
   seriePF[Data == dataPrev,`:=`(Carga = infer,CargaMed = cmedn/constNorm)]
}


