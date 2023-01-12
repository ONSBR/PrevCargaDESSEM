execModelos <- function(deck,maxTreads,modelos,arqLog,parMode = "PARALLEL"){
   execMode = "PAR"
   if(!is.null(maxTreads)){
      if(maxTreads == -1){
         execMode = "SER"
         message("SERIAL")
      } else {
         message(parMode)
      }
   } else {
      message(parMode)
   }
   if(execMode == "SER"){
      set.seed(1234)
      mdlData <- lapply(modelos,execModelo,deck)
      set.seed(NULL)
   } else if(parMode == "PARALLEL"){
      message(paste("EXECUTANDO MODELOS - DETALHADO NO LOG:",arqLog))
      CL <- configCluster(maxTreads,arqLog)
      mdlData <- parLapply(cl = CL,modelos,execModelo,deck)
      stopCluster(CL)
   } else if(parMode == "MULTICL"){
      message(paste("EXECUTANDO MODELOS - DETALHADO NO LOG:",arqLog))
      mdlData <- execModelosMultiCL(getMultiClusterExecPlan(maxTreads,modelos),modelos,deck,arqLog)
   }# else if(parMode == "FUTURE"){
   #    setDTthreads(1)
   #    configFuture(maxTreads)
   #    mdlData <- future_lapply(modelos,execModelo,deck,future.seed = 1234,future.scheduling = FALSE)
   #    setDTthreads(NULL)
   # } else if(parMode == "CALLR"){
   #    setDTthreads(1)
   #    configCallR(maxTreads)
   #    mdlData <- future_lapply(modelos,execModelo,deck,future.seed = 1234,future.scheduling = FALSE)
   #    setDTthreads(NULL)
   # }
   if(!is.null(mdlData)){
      dadosOut <- do.call("rbind",mdlData)
   } else {
      dadosOut <- mdlData
   }
   return(dadosOut)
}

execModelo <- function(modelo,deck){
   tryCatch({
      config_mdl <- getConfigModel(modelo)
      message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Processando..."))
      processaModelo <- get(config_mdl$funcProcessamento)
      mdl_results <- processaModelo(deck,config_mdl)
      mdl_results[,Modelo := modelo]
      message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Finalizado..."))
      gc()
      return(mdl_results)
   },error = function(e){
      message(e)
      message(paste(config_mdl$nomModelo,format(Sys.time(),'- %F %T'), "- Finalizado com ERRO..."))
      return(NULL)
   })
}

# configFuture <- function(maxTreads){
#    if(Sys.info()["sysname"] == "Windows"){
#       ncoresMach <- limitedCores(length(availableWorkers()) - 1,maxTreads)
#       plan(multisession,workers = ncoresMach)
#    } else {
#       ncoresMach <- limitedCores(availableCores() - 1,maxTreads)
#       plan(multicore,workers = ncoresMach)
#    }
# }

getMultiClusterExecPlan <- function(maxTreads,modelos){
   ncoresMach <- limitedCores(detectCores() - 1,maxTreads)
   if(length(modelos) <= ncoresMach){
      ncoresMach <- length(modelos)
   } else {
      ncoresMachI <- rep(ncoresMach,length(modelos) %/% ncoresMach)
      ncoresMachR <- length(modelos) %% ncoresMach
      if(ncoresMachR > 0){
         ncoresMach <- c(ncoresMachI,ncoresMachR)
      } else {
         ncoresMach <- ncoresMachI
      }
   }
   execPlan <- getExecPlanModelosMultiCluster(ncoresMach,modelos)
   return(execPlan)
}

execModelosCL <- function(execPlan,modelos,deck,arqLog,rbindResults = F){
   message(paste("Configurando Cluster - Modelos:",paste(modelos[execPlan$mdlRun],collapse = ", ")))
   CL <- configCluster(execPlan$ncores,arqLog)
   mdlData <- parLapply(cl = CL,modelos[execPlan$mdlRun],execModelo,deck)
   stopCluster(CL)
   rm(CL)
   gc()
   if(rbindResults & !is.null(mdlData)){
      mdlData <- do.call("rbind",mdlData)
   }
   return(mdlData)
}

execModelosMultiCL <- function(execPlan,modelos,deck,arqLog){
   mdlDataAll <- lapply(execPlan,execModelosCL,modelos,deck,arqLog,T)
   return(mdlDataAll)
}

# configCallR <- function(maxTreads){
#    ncoresMach <- limitedCores(availableCores() - 1,maxTreads)
#    plan(future.callr::callr,workers = ncoresMach)
# }


limitedCores <- function(ncoresMach,maxTreads){
   if(!is.null(maxTreads)){
      if(maxTreads < ncoresMach){
         ncoresMach <- maxTreads
      }
   }
   return(ncoresMach)
}

configCluster <- function(maxTreads,arqLog){
   ncoresMach <- detectCores() - 1
   if(!is.null(maxTreads)){
      if(maxTreads < ncoresMach){
         ncoresMach <- maxTreads
      }
   }
   if(Sys.info()["sysname"] == "Windows"){
      CL <- makeCluster(ncoresMach,outfile = arqLog)
      loadCluster(CL)
      clusterExport(cl=CL,
                    varlist = names(environment()),
                    envir = environment())
   } else {
      CL <- makeForkCluster(nnodes = ncoresMach,outfile = arqLog)
   }
   return(CL)
}

getExecPlanModelosMultiCluster <- function(ncoresMach,modelos){
   if(length(ncoresMach)==1){
      mdlRun <- list(1:ncoresMach)
   } else if(length(ncoresMach) > 1){
      mdlRun <- list(1:ncoresMach[1])
      for(i in 2:length(ncoresMach)){
         mdlRun <- c(mdlRun,list((max(mdlRun[[i-1]])+1):(max(mdlRun[[i-1]])+ncoresMach[i])))
      }
   } else {
      stop("Erro na configuracao do cluster - numero de cores invalido")
   }
   execPlan <- lapply(1:length(ncoresMach),function(i) list(ncores = ncoresMach[i], mdlRun = unlist(mdlRun[i])))
   return(execPlan)
}


loadCluster <- function(cl){
   clusterEvalQ(cl,suppressPackageStartupMessages(require(neuralnet)))
   clusterEvalQ(cl,suppressPackageStartupMessages(require(data.table)))
   clusterEvalQ(cl,suppressPackageStartupMessages(require(lubridate)))
   clusterEvalQ(cl,suppressPackageStartupMessages(require(jsonlite)))
   clusterEvalQ(cl,suppressPackageStartupMessages(require(fpp3)))
   clusterEvalQ(cl,suppressPackageStartupMessages(require(mgcv)))
   clusterEvalQ(cl,suppressPackageStartupMessages(require(WeightSVM)))
   clusterEvalQ(cl,suppressPackageStartupMessages(require(rlist)))
   clusterEvalQ(cl,setDTthreads(1))
   clusterEvalQ(cl,set.seed(1234))
}


