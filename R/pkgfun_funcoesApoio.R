obtemLag <- function(lag,seriediaria,dia = 1:7,diaAno = 1:366,diaEsp = F,diaEspLag = F){
   seriediariaD <- seriediaria[(lag+1):nrow(seriediaria)]
   seriediariaD <- seriediariaD[wday(Data) %in% dia & yday(Data) %in% diaAno]
   if(!diaEsp){
      seriediariaD <- seriediariaD[DiaEspecial == 0]
      seriediariaD[,DiaEspecial := NULL]
   }
   seriediariaD[,LAG := paste0("LAG_",lag)]
   seriediariaD$VALLAG <- seriediaria[Data %in% (seriediariaD$Data-lag)]$Carga
   if(diaEspLag){
      seriediariaD$FERLAG <- as.numeric(seriediaria[Data %in% (seriediariaD$Data-lag)]$DiaEspecial > 0)
      seriediariaD <- melt(seriediariaD,id.vars = c(names(seriediariaD[,-c("VALLAG","FERLAG")])),
                           measure.vars = c("VALLAG","FERLAG"),
                           variable.name = "LAG2",value.name = "VALLAG")
      seriediariaD[,LAG:=ifelse(LAG2=="VALLAG",LAG,sub('_','_FER_',LAG))]
      seriediariaD[,LAG2 :=NULL]
   }
   return(seriediariaD)
}

obtemLagExo <- function(lag,exo,seriediaria){
   seriediariaD <- seriediaria[(lag+1):nrow(seriediaria)]
   seriediariaD[,LAG := paste0("LAGEXO",gsub('Exo','',exo),'_',lag)]
   seriediariaD$VALLAG <- seriediaria[Data %in% (seriediariaD$Data-lag)][,exo,with = F]
   return(seriediariaD)
}

obtemLagsExo <- function(exo,lags,seriediaria){
   dados <- do.call("rbind", lapply(lags,obtemLagExo,exo,seriediaria))
   return(dados)
}

obtemLagsExogenas <- function(lags,exogenas,seriediaria){
   dados <- do.call("rbind", lapply(exogenas,obtemLagsExo,lags,seriediaria))
   dados <- dcast(dados,as.formula(paste0(paste(names(dados[,-c("LAG","VALLAG")]),collapse = "+"),"~LAG")), value.var = "VALLAG")
   return(dados)
}



obtemLagHora <- function(lagh,seriehist,dia = 1:7,hora = 0:23,diaAno = 1:366,hv = 0:1,diaEsp = F,diaEspLag = F){
   seriehistD <- seriehist[DataHora > (min(DataHora) %m+% hours(lagh))]
   seriehistD <- seriehistD[hour(DataHora) %in% hora & yday(DataHora) %in% diaAno & wday(DataHora) %in% dia & HV %in% hv]
   if(!diaEsp){
      seriehistD <- seriehistD[DiaEspecial == 0]
      seriehistD[,DiaEspecial := NULL]
   }
   if(length(hv)==1){
      seriehistD[,HV := NULL]
   }
   seriehistD[,LAG := paste0("LAG_",lagh)]
   seriehistD[,VALLAG := seriehist[DataHora %in% (seriehistD$DataHora %m-% hours(lagh))]$Carga]
   if(diaEspLag){
      seriefer <- unique(seriehist[,.(DatFer = Data,FERLAG = as.numeric(DiaEspecial))])
      seriehistD[,DatFer := as.Date(format(DataHora %m-% hours(lagh),'%F'))]
      seriehistD <- seriehistD[seriefer, on = "DatFer",nomatch=0]
      seriehistD <- melt(seriehistD,id.vars = names(seriehistD[,-c("VALLAG","FERLAG")]),
                         measure.vars = c("VALLAG","FERLAG"),
                         variable.name = "LAG2",value.name = "VALLAG")
      seriehistD[,LAG:=ifelse(LAG2=="VALLAG",LAG,sub('_','_FER_',LAG))]
      seriehistD[,LAG2 :=NULL]
   }
   return(seriehistD)
}


obtemLags <- function(lags,seriediaria,dia = 1:7,diaAno = 1:366,diaEsp = T,diaEspLag = T,rmAmostraPq = T){
   dados <- do.call("rbind", lapply(lags,obtemLag,seriediaria,dia,diaAno,diaEsp,diaEspLag))
   if(rmAmostraPq & diaEspLag){
      dados[startsWith(LAG,'LAG_FER'),CNT := sum(VALLAG),.(LAG)]
      dados <- dados[is.na(CNT) | CNT >= 3]
      dados[,CNT := NULL]
   }
   dados <- dcast(dados,as.formula(paste0(paste(names(dados[,-c("LAG","VALLAG")]),collapse = "+"),"~LAG")), value.var = "VALLAG")
   dados <- dados[Prev == T | complete.cases(dados)]
   return(dados)
}

obtemLagsHora <- function(seriehist,lags,dia = 1:7,hora = 0:23,diaAno = 1:366,hv = 0:1,diaEsp = F,diaEspLag = F){
   dados <- do.call("rbind", lapply(lags,obtemLagHora,seriehist,dia,hora,diaAno,hv,diaEsp,diaEspLag))
   dados <- dcast(dados,as.formula(paste0(paste(names(dados[,-c("LAG","VALLAG")]),collapse = "+"),"~LAG")), value.var = "VALLAG")
   dados <- dados[Prev == T | complete.cases(dados)]
   return(dados)
}

obtemLagsDiaSemana <- function(seriehist,dsLags,diaAno,hv,tpFer,hora = 0:23){
   dados <- do.call("rbind", lapply(dsLags,obtemLagDiaSemana,seriehist,diaAno,hv,tpFer,hora))
   dados <- dcast(dados,as.formula(paste0(paste(names(dados[,-c("LAG","VALLAG")]),collapse = "+"),"~LAG")), value.var = "VALLAG")
   dados <- dados[Prev == T | complete.cases(dados)]
   return(dados)
}

obtemLagDiaSemana <- function(lag,seriehist,diaAno,hv,tpFer,hora){
   seriehistD <- seriehist[8:nrow(seriehist)]
   seriehistD <- seriehistD[DiaEspecial %in% tpFer & yday(Data) %in% diaAno & hour(DataHora) %in% hora]
   seriehistD[,LAG := paste0("LAGDS_",lag)]
   seriehistD[lag < wday(Data),DataLag := paste(Data - (wday(Data)-lag),hour(DataHora))]
   seriehistD[lag >= wday(Data),DataLag := paste(Data - (7 - (lag - wday(Data))),hour(DataHora))]
   seriehist <- copy(seriehist)
   seriehist[,DataLag := paste(Data,hour(DataHora))]
   seriehistD <- seriehist[,.(DataLag,VALLAG = Carga)][seriehistD, on = "DataLag"]
   seriehistD[,DataLag := NULL]
   return(seriehistD)
}

obtemSD <- function(seriehist,diaAno,hv,tpFer,dataRef = NULL){
   seriehistD <- seriehist[DiaEspecial %in% tpFer & yday(Data) %in% diaAno & HV %in% hv]
   if(!is.null(dataRef)){
      seriehistD <- seriehistD[Data == dataRef]
   }
   return(sd(seriehistD$Carga,na.rm = T))
}

aggregaSerie <- function(serieHist,tipotemp,normaliza,constnorm,constnormtemp){
   if(!is.character(tipotemp)){tipotemp <- c("SEM","MAX","MED","MXMN")[tipotemp + 1]}
   if(!normaliza){
      constnorm <- 1
      constnormtemp <- 1
   }
   exo_cols <- grep('Exo_.*',names(serieHist),value = T)
   key_cols <- names(serieHist)[which(!names(serieHist) %in% c("DataHora","Carga",exo_cols))]
   if(tipotemp == "MED"){
      serieHistD <- serieHist[,lapply(.SD,mean),by = key_cols,.SDcols = c("Carga",exo_cols)]
      serieHistD <- serieHistD[,lapply(.SD,dividepor,constnormtemp),by = c(key_cols,"Carga"),.SDcols = exo_cols]
      serieHistD[,Carga := Carga/constnorm]
      return(serieHistD)
   } else if(tipotemp == "MXMN"){
      serieHistD <- serieHist[,unlist(c(list(Carga = mean(Carga)),lapply(.SD, mxmn)),recursive = F),by = key_cols,.SDcols = c(exo_cols)]
      exo_cols <- grep('Exo_.*',names(serieHistD),value = T)
      serieHistD <- serieHistD[,lapply(.SD,dividepor,constnormtemp),by = c(key_cols,"Carga"),.SDcols = exo_cols]
      serieHistD[,Carga := Carga/constnorm]
      return(serieHistD)
   } else if(tipotemp == "MAX"){
      serieHistD <- serieHist[,c(list(Carga = mean(Carga)),lapply(.SD, max)),by = key_cols,.SDcols = exo_cols]
      serieHistD <- serieHistD[,lapply(.SD,dividepor,constnormtemp),by = c(key_cols,"Carga"),.SDcols = exo_cols]
      serieHistD[,Carga := Carga/constnorm]
      return(serieHistD)
   } else if(tipotemp == "SEM"){
      return(serieHist[,.(Carga = mean(Carga)/constnorm),key_cols])
   } else {
      stop("Agregacao de exogena fora dos padroes de entrada")
   }
}

dividepor <- function(x,const){
   return(x/const)
}

mxmn <- function(x) {
   return(list(MAX = max(x), MIN = min(x)))
}

normalizaSerieRef <- function(serieHist,constnorm,constnormtemp){
   serieHist[,Carga := normalizafun(Carga,constnorm),.(Data)]
   exo_cols <- getExoCols(names(serieHist))
   serieHist[,(exo_cols) := lapply(.SD,normalizafun,constnormtemp),by = .(Data),.SDcols = exo_cols]
}

normalizafun <- function(x, normfun){
   if(is.character(normfun)){
      normfun <- get(normfun)
      return(x/normfun(x))
   } else {
      return(x/normfun)
   }
}

getExoCols <- function(cols){
   return(grep('Exo_.*',cols,value = T))
}

getFuncTemp <- function(tipotemp){
   if(!is.character(tipotemp)){tipotemp <- c("SEM","MAX","MED","MXMN")[tipotemp + 1]}
   if(tipotemp == "MED"){
      return(mean)
   } else if(tipotemp == "MXMN"){
      return(rangelst)
   } else if(tipotemp == "MAX"){
      return(max)
   } else if(tipotemp == "SEM"){
      return(max)
   } else {
      stop("Tipo de temperatura fora dos padroes de entrada")
   }
}

rangelst <- function(x){
   rng <- range(x)
   list(list(TempMin = rng[1],TempMax = rng[2]))
}

getSVMWeights <- function(alfawsvm,ndiashist,k = .15){
   pesowsvm=(1-alfawsvm)^(seq(ndiashist,1,-1)*k)
   if(sum(pesowsvm)>0 & alfawsvm>0){
      pesowsvm=pesowsvm/sum(pesowsvm)
   } else {
      pesowsvm=rep(1,length(pesowsvm))
   }
   return(pesowsvm)
}

padronizaCargaHorariaRef <- function(seriehist,sdmean = F){
   if(sdmean){
      seriehist[,`:=`(Media = mean(Carga),DesvPad = sd(Carga)),.(Data)]
   }
   seriehist[,Carga := scale(Carga),.(Data)]
}

modelCores <- function(modelo){
   formula=as.formula(formula)
   neuralformula=formula
   modelo=nnet(formula=neuralformula,data=dados,size=c(tipokernel),weights=pesowsvm,maxit=1000,trace=F)
   estimado=predict(modelo,dados[,-dim(dados)[2]])

   modelo=wsvm(Y~.,weight=pesowsvm,data=dados)
   estimado=predict(modelo,dados[,-dim(dados)[2]])
}

validaEntrada <- function(dominio,valor){
   if(is.numeric(valor)){
      valor <- dominio[valor + 1]
   } else {
      valor <- valor[1]
   }
   return(valor)
}

getNormalizaCarga <- function(normConfig,serie){
   val <- ifelse(!is.null(normConfig$oper),get(normConfig$oper)(serie$Carga),1)
   val <- ifelse(!is.null(normConfig$const),val * normConfig$const,val)
   val <- ifelse(normConfig$flag,val,1)
   return(val)
}

getNormalizaExogena <- function(normConfig,serie){
   exo_cols <- getExoCols(names(serie))
   val <- ifelse(!is.null(normConfig$oper),get(normConfig$oper)(serie[,exo_cols,with = F],na.rm=T),1)
   val <- ifelse(!is.null(normConfig$const),val * normConfig$const,val)
   val <- ifelse(normConfig$flag,val,1)
   return(val)
}

obtemLagSemiHora <- function(lagsh,seriehist,dia = NULL,semihora = NULL,diaEsp = F){
   seriehistD <- seriehist[Data > (min(Data) + lagsh)]
   if(!is.null(dia)){
      seriehistD <- seriehistD[wday(Data) == dia]
   }
   if(!is.null(semihora)){
      seriehistD <- seriehistD[format(DataHora,'%H:%M') %in% semihora]
   }
   seriehistD[,LAG := paste0("LAG_",lagsh)]
   seriehistD[,VALLAG := seriehist[DataHora %in% (seriehistD$DataHora %m-% minutes(30*lagsh))]$Carga]
   if(diaEsp){
      seriefer <- unique(seriehist[,.(Data,FERLAG = as.numeric(DiaEspecial))])
      seriehistD[,DatFer := as.Date(format(DataHora %m-% minutes(30*lagsh),'%F'))]
      seriehistD <- seriehistD[seriefer, on = "Data",nomatch=0]
      seriehistD <- melt(seriehistD,id.vars = c("DataHora","Data","Carga","LAG"),
                         measure.vars = c("VALLAG","FERLAG"),
                         variable.name = "LAG2",value.name = "VALLAG")
      seriehistD[,LAG:=ifelse(LAG2=="VALLAG",LAG,sub('_','_FER_',LAG))]
      seriehistD[,LAG2 :=NULL]
   }
   return(seriediariaD)
}


obtemJanelaDiaria <- function(dataPrev,janela = c(45,45)){
   if(is.character(dataPrev)) dataPrev <- as.Date(dataPrev)
   dini <- dataPrev - janela[1]
   dfim <- dataPrev + janela[2]
   return(unique(yday(seq.Date(dini,dfim,"days"))))
}

obterDiaEspDMConfig <- function(dataPrev,serie,param_entrada){
   if("DiaEspecial" %in% names(serie) & serie[Data == dataPrev]$DiaEspecial[1] > 0 &
      length(list.filter(param_entrada$janelaDadosFer,serie[Data == dataPrev]$DiaEspecial[1] %in% tipoDiaEsp)) > 0){
      return(list(
         tpEntrada = param_entrada$tipoEntradaDMFer,
         diasAno = 1:366,
         ds = 1:7
      ))
   } else {
      return(list(
         tpEntrada = param_entrada$tipoEntradaDM,
         diasAno = obtemJanelaDiaria(dataPrev,param_entrada$janelaDados),
         ds = wday(as.Date(dataPrev))
      ))
   }
}

obterDiaEspPerfilConfig <- function(dataPrev,serie,param_entrada){
   if("DiaEspecial" %in% names(serie) & serie[Data == dataPrev]$DiaEspecial[1] > 0 &
      length(list.filter(param_entrada$janelaDadosFer,serie[Data == dataPrev]$DiaEspecial[1] %in% tipoDiaEsp)) > 0){
      infoFer = list.filter(param_entrada$janelaDadosFer,serie[Data == dataPrev]$DiaEspecial[1] %in% tipoDiaEsp)[[1]]
      return(list(
         tpEntrada = param_entrada$tipoEntradaPerfilFer,
         diasAno = obtemJanelaDiaria(dataPrev,infoFer$janelaDados),
         diaEsp = 1:11,
         lagds = infoFer$lagsdiasemana,
         hrlag = 1,
         ds = 1:7
      ))
   } else {
      return(list(
         tpEntrada = param_entrada$tipoEntradaPerfil,
         diasAno = obtemJanelaDiaria(dataPrev,param_entrada$janelaDados),
         diaEsp = NULL,
         lagds = NULL,
         hrlag = param_entrada$lagsPerfil,
         ds = wday(as.Date(dataPrev))
      ))
   }
}

obterEntradaFormulaDM <- function(dataPrev,serie,param_entrada){
   cfg <- obterDiaEspDMConfig(dataPrev,serie,param_entrada)
   funcEntrada <- get(param_entrada$funcDadosDemandaMedia)
   dadosEntrada <- funcEntrada(serie = serie,
                               tipoEntrada = cfg$tpEntrada,
                               diaSemana = cfg$ds,
                               diaAno = cfg$diasAno,
                               lagsmodelo = param_entrada$lagsDemandaMedia)
   formulaMDL <- getFormula(names(dadosEntrada$serieDiariaModelo[,-param_entrada$colunaRemover,with = F]))
   return(list(dadosEntrada = dadosEntrada, formulaMDL = formulaMDL))
}

obterEntradaFormulaDM_Reg <- function(serie,param_entrada){
   funcEntrada <- get(param_entrada$funcDadosDemandaMedia)
   dadosEntrada <- funcEntrada(serie = serie,
                               tipoEntrada = param_entrada$tipoEntradaDM)
   formulaMDL <- getFormula(names(dadosEntrada$serieDiariaModelo[,-param_entrada$colunaRemover,with = F]),
                            lagsExoReg = param_entrada$lagsExogenaDia)
   return(list(dadosEntrada = dadosEntrada, formulaMDL = formulaMDL))
}



filtraDadosTreinamento <- function(dadosEntrada){
   return(dadosEntrada$serieDiariaModelo[!is.na(Carga)])
}


getFormula <- function(cols, var = "Carga",lagsExoReg = NULL){
   explicativas <- cols[which(cols != var)]
   if(!is.null(lagsExoReg)){
      explicativas <- c(explicativas,getLagsExoReg(getExoCols(explicativas),lagsExoReg))
   }
   frm <- as.formula(paste0(var," ~ ",paste(explicativas,collapse = " + ")))
   return(frm)
}

getLagsExoReg <- function(exo_cols,lags){
   return(paste(unlist(lapply(exo_cols,lagsExoStr,lags)),collapse = "+"))
}

lagsExoStr <- function(exo_col,lags){
   return(paste0("lag(",exo_col,",",lags,")"))
}

getConfig <- function(){
   fromJSON(txt = system.file("extdata","model_parameters.json",package = "prevcargadessem"),simplifyDataFrame = F,simplifyMatrix = F)
   #fromJSON(txt = "inst/extdata/model_parameters.json",simplifyDataFrame = F,simplifyMatrix = F)
}

getConfigModel <- function(modelo){
   getConfig()[[modelo]]
}


#' Obtem os modelos disponiveis para execucao
#'
#' @return Vetor com os modelos disponiveis para execucao
#' @export
#'
obterModelos <- function(){
   names(getConfig())
}

addValuesDMRef <- function(serie,infer,var = "Carga"){
   dats <- format(infer$Data)
   a <- lapply(dats,addValueDMRef,serie,infer,var)
}

addValueDMRef <- function(dat,serie,infer,var){
   serie[Data == dat,(var) := infer[Data == dat]$Carga.infer]
}

desagregaModeloSH <- function(modelo,dadosMdl){
   dadosMdlFlt <- copy(dadosMdl[Modelo == modelo])
   dadosMdlFlt[,DataHora := DataHora %m+% hours()]

   y <- dadosMdlFlt[order(DataHora)]$Carga
   x1 <- seq(1,nrow(dadosMdlFlt)*2-1,2)
   x2 <- seq(2,nrow(dadosMdlFlt)*2,2)
   xout <- 1:(nrow(dadosMdlFlt)*2)
   fun_spline_1 <- splinefun(x1, y, "monoH.FC")
   fun_spline_2 <- splinefun(x2, y, "monoH.FC")
   yout <- fun_spline_1(xout)/2 + fun_spline_2(xout)/2
   dt_aj <- data.table(SemiHora = 1:(nrow(dadosMdlFlt)*2),Hora = rep(1:nrow(dadosMdlFlt),2)[order(rep(1:nrow(dadosMdlFlt),2))],
                       Vin = rep(y,2)[order(rep(1:nrow(dadosMdlFlt),2))],Vout = yout)
   dt_aj[,Vm := mean(Vout),.(Hora)]
   dt_aj[,VoutAj := Vout*Vin/Vm]
   dt_a <- copy(dadosMdlFlt)
   dt_a[,DataHora := DataHora %m-% minutes(30)]
   dt <- funion(dadosMdlFlt,dt_a)
   dt <- dt[order(DataHora)]
   dt[,Carga := dt_aj$VoutAj]
   dt[,Passo := 1:(nrow(dadosMdlFlt)*2)]
   return(dt)
}

plotOutputData <- function(dadosMdlSH,dirSaida){
   dados <- copy(dadosMdlSH)
   dados[,Modelo := as.factor(Modelo)]
   gg <- ggplot2::ggplot(dados,ggplot2::aes(x= DataHora, y = Carga, group = Modelo, color = Modelo))
   gg <- gg + ggplot2::geom_line()
   gg <- gg + ggplot2::labs(y = "Carga (MW)")
   ggplot2::ggsave(filename = paste0(dirSaida,"PLOT_PrevCarga_",format(min(dados$DataHora,na.rm=T),'%Y%m%d'),'_',format(max(dados$DataHora,na.rm=T),'%Y%m%d'),'.jpg'),
          plot = gg,device = "jpeg",width = 1280,height = 720,scale = 2.5,units = "px",dpi = "retina")
}

writeOutDP <- function(dadosMdlSH,dtPat,dataPrev,dirSaida){
   dadosMdlD <- copy(dadosMdlSH[Modelo == "Combinado"])
   dadosMdlD[,DataHora := DataHora %m-% minutes(30)]
   dadosMdlD[,`:=` (Hora = hour(DataHora),Mes = month(DataHora),DiaSemana = wday(DataHora))]
   dadosMdlD <- dadosMdlD[dtPat, on = c("Mes","DiaSemana"),allow.cartesian = T,nomatch = 0]
   dadosMdlD[,PAT := Hora >= as.numeric(substr(HoraIni,1,2)) & Hora <= as.numeric(substr(HoraFim,1,2))]
   dadosMdlD <- dadosMdlD[PAT == T]
   dadosMdlDSH <- dadosMdlD[date(DataHora) == as.Date(dataPrev) + 1,
                            .(Data = date(DataHora),
                              Dia = day(DataHora),
                              HoraIni = hour(DataHora),
                              SemiHora = ifelse(minute(DataHora) == 30,1,0),Carga = round(Carga))]
   dadosMdlDPT <- dadosMdlD[date(DataHora) > as.Date(dataPrev) + 1,.(Carga = mean(Carga)),
                            .(Data = date(DataHora), HoraIni,Patamar,Intervalo)]

   dadosMdlDPT <- dadosMdlDPT[,.(Data,
                                 Dia = day(Data),
                                 HoraIni = as.numeric(substr(HoraIni,1,2)),
                                 SemiHora = ifelse(substr(HoraIni,4,5) == "30",1,0),Carga = round(Carga))]
   dadosMdlDPT <- rbind(dadosMdlDSH[order(Data,HoraIni,SemiHora)],dadosMdlDPT[order(Data,HoraIni,SemiHora)])
   dadosMdlDPT[,Data := NULL]
   fwrite(dadosMdlDPT,paste0(dirSaida,'SaidaPrevCargaDESSEM_BL_',dataPrev,'.txt'),sep = '\t')
}

writeOutCSV <- function(dadosMdlSH,dataPrev,dirSaida){
   fwrite(dadosMdlSH,paste0(dirSaida,'SaidaPrevCargaDESSEM_',dataPrev,'.csv'),sep = ";",dec = ",",dateTimeAs = "write.csv")
}

aplicaPesos <- function(dadosMdlSH,dtCOMB){
   dadosMdlSHCmb <- copy(dadosMdlSH)
   dadosMdlSHCmb <- dadosMdlSHCmb[dtCOMB[,.(Modelo,Coeficientes,Passo)], on = c("Modelo","Passo")]
   dadosMdlSHCmbIntc <- dadosMdlSHCmb[Modelo == "(Intercept)",.(Passo,Coeficientes)]
   dadosMdlSHCmb <- dadosMdlSHCmb[!Modelo == "(Intercept)",.(Carga = sum(Carga * Coeficientes)),.(DataHora,Passo)]
   if(nrow(dadosMdlSHCmbIntc)>0){
      dadosMdlSHCmb <- dadosMdlSHCmb[dadosMdlSHCmbIntc, on = "Passo"]
      dadosMdlSHCmb[,`:=`(Carga = Carga + Coeficientes, Coeficientes = NULL, Modelo = "Combinado")]
   } else {
      dadosMdlSHCmb[,Modelo := "Combinado"]
   }
   dadosMdlSH <- rbind(dadosMdlSHCmb,dadosMdlSH)
   return(dadosMdlSH)
}
