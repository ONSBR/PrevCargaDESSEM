filesfun <- list.files("R/",".*pkgfun_.*R$",full.names = T)
lapply(filesfun,source)
source("tests/init.R")
source("tests/env_var.R")
source("tests/deckaws.R")
#library(prevcargadessem)

if(!sessionInfo()$R.version$major == "4") stop("Versao do R incompativel")


dataPrev = "2022-01-30"
area = "SECO"
#deck <- "tests/deckn/"
#deck <- deckEntrada("deckn/")
#deckLeitora <- TransformaDeck(deckEntrada("deckn/"))
#deck = 'D:/Development/R/CadernoTestesPCDS200/SECO_2021-12-03/'
deck <- deckPartial(dataPrev,0,area,"2.0.0",F,7,F)$deck
df <- previsordessem(deck,dataPrev,saida = c(),dirSaida = "tests/")




modelos <- obterModelos()[1:4]
horizonte = "10DIAS"
maxTreads = -1
saida = c()
saida = c("CSV","DESSEM","PLOT")

deck2 <- TransformaDeck(deck,dataPrev,horizonte)
df <- execModelos(deck2,maxTreads = maxTreads,modelos = modelos,parMode = "MULTICL",arqLog = "tests/log.txt")




df <- previsordessem(deck,dataPrev,saida = saida,dirSaida = "tests/",maxTreads = 4,horizonte = horizonte,modelos = modelos)


deck <- TransformaDeck(deck,dataPrev,"10DIAS")
deck <- deckLeitora
modelo <- obterModelos()[1]
config_mdl <- getConfigModel(modelo)
param_entrada <- config_mdl

df <- execModelo(modelo,deck)

df <- processaModelosDMPerfil(deck,config_mdl)


plot(df$Carga,type = "l")



config_mdl <- getConfigModel(modelo)
param_entrada <- config_mdl






# SECO_2021-10-03
# SECO_2021-10-07
# SECO_2021-10-10
# SECO_2021-10-11
# SECO_2021-10-24
# SECO_2021-10-31
# SECO_2021-11-04
# SECO_2021-11-07
# SECO_2022-01-02
# NE_2021-08-14
# NE_2021-10-03
# NE_2021-10-26
# NE_2021-11-01
# NE_2021-11-05
# NE_2021-11-06
# S_2021-10-03
# S_2021-10-26
# S_2021-11-28
# S_2021-11-30
# S_2021-12-26
# N_2021-06-08
# N_2021-08-14
# N_2021-09-01
# N_2021-10-03
# N_2021-10-26
# N_2021-11-28
# N_2021-11-30
# N_2021-12-26










e <- new.env()
filesfun <- list.files("R/",".*pkgfun_.*R$",full.names = T)
lapply(filesfun,source,local = e)
message(paste0('"',paste(names(e),collapse = '","'),'"'))

source("R/dadosEntrada.R")
source("R/infousuario.R")
source("R/filtro.R")
source("R/funcoesApoio.R")
source("R/entradaDemandaMedia.R")
source("R/entradaHoraria.R")
source("R/modelosDemandaMedia.R")
source("R/modelosPerfil.R")
source("R/execModelos.R")
source("R/env_var.R")
source("R/deckaws.R")




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
                    varlist = c("obtemLagExo","mxmn","desagregaModeloSH","getExecPlanModelosMultiCluster","aggregaSerie","execModelo","obtemLagSemiHora","writeOutCSV","obtemSD","TransformaDeck","execDemandaMedia","entradaPerfilFeriadoReg","inclui_dummy_DE","obtemLagDiaSemana","limitedCores","execDatasPerfil","dividepor","modelCores","writeOutDP","execDemandaMediaTS","padronizaCargaHorariaRef","inclui_CargaMediaRef","obterDiaEspDMConfig","getNormalizaCarga","execModelosCL","obterEntradaFormulaDM","execDMPerfil","getExoCols","aplicaPesos","SVMhorario","getConfig","obtemLagsExogenas","obterEntradaFormulaDM_Reg","entradaDemandaMediaDefault_ML","validaEntrada","obtemLag","getLagsExoReg","getNormalizaExogena","loadCluster","entradaPerfil","getFuncTemp","normalizafun","configCluster","obtemJanelaDiaria","entradaDemandaMediaLocal_ML","entradaTimeSeriesFactor","rangelst","getFormula","obtemLags","inclui_dummy_mes","addValueDMRef","execDMTSPerfil","processaModelosDMTSPerfil","obterModelos","RN_DemandaMedia","deckEntrada","execPerfil","getSVMWeights","inclui_dummy_dia","processaModelosDMPerfil","addValuesDMRef","obtemLagsDiaSemana","execHorasPerfil","RD_DemandaMediaHyndman","entradaTimeSeriesDefault","lagsExoStr","cabecalho","normalizaSerieRef","entradaPerfilDefault","getMultiClusterExecPlan","execModelosMultiCL","getConfigModel","obterDiaEspPerfilConfig","plotOutputData","entradaDemandaMedia","obtemLagsExo","RL_perfilFeriado","filtraDadosTreinamento","obtemLagsHora","execModelos","SVM_DemandaMedia","obtemLagHora"),
                    envir = environment())
   } else {
      CL <- makeForkCluster(nnodes = ncoresMach,outfile = arqLog)
   }
   return(CL)
}
