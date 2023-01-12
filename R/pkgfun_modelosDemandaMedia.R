
SVM_DemandaMedia <- function(dataPrev,serie,param_entrada){
   input <- obterEntradaFormulaDM(dataPrev,serie,param_entrada)
   dadosTreinamento <- filtraDadosTreinamento(input$dadosEntrada)
   #message(paste(param_entrada$nomModelo,format(Sys.time(),'- %F %T'),"-",nrow(dadosTreinamento),"amostras no conjunto de treinamento de demanda media"))
   modeloSVM <- tune_wsvm(input$formulaMDL,iter.max=500,
                          weight=getSVMWeights(param_entrada$alfa,nrow(dadosTreinamento)),
                          data=dadosTreinamento,
                          ranges = param_entrada$ranges,
                          kernel = param_entrada$kernel)
   return(list(modeloTreinado = modeloSVM$best.model,dadosEntrada = input$dadosEntrada, parametros = param_entrada))
}



RN_DemandaMedia <- function(dataPrev,serie,param_entrada){
   input <- obterEntradaFormulaDM(dataPrev,serie,param_entrada)
   dadosTreinamento <- filtraDadosTreinamento(input$dadosEntrada)
   #message(paste(param_entrada$nomModelo,format(Sys.time(),'- %F %T'),"-",nrow(dadosTreinamento),"amostras no conjunto de treinamento de demanda media"))
   modeloRN <- neuralnet(formula=input$formulaMDL,
                         data=dadosTreinamento,
                         hidden=param_entrada$cells)
   return(list(modeloTreinado = modeloRN,dadosEntrada = input$dadosEntrada, parametros = param_entrada))
}


RD_DemandaMediaHyndman <- function(serie,param_entrada){
   input <- obterEntradaFormulaDM_Reg(serie,param_entrada)
   dadosTreinamento <-  as_tsibble(input$dadosEntrada$serieDiariaModelo[Prev==F],index = "Data")
   dadosInfer <- as_tsibble(input$dadosEntrada$serieDiariaModelo[Prev==T],index = "Data")
   fit <- dadosTreinamento %>% model(ARIMA(input$formulaMDL))
   return(list(modeloTreinado = fit,dadosEntrada = input$dadosEntrada,
               parametros = param_entrada,dadosInfer = dadosInfer))
}


