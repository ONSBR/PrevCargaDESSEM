#10 @title PREVISORDESSEM1
#10 @name PREVISORDESSEM1
#10
#10 @description Compute load forecasts
#10
#10 @Usage previsordessem1(caminho,caminhosaida,treinamodelo,tipoinput,tipotemp,tipotempdia,naosimulada,intercambio,horizonteprevisao,diasprevisaosemihora,tipokernel,tipocombinada,alfawsvm,nomerdata)
#10
#10 @details
#10 \itemize{
#10  \item caminho is the directory name with data files
#10  \item caminhosaida is the directory name with results files
#10  \item treinamodelo two options: 1 for training models, 0 for read previously trained model parameters (default=1)
#10  \item tipoinput two options: 1 for data files in data.frame format, 0 for data files in csv format (default=1)
#10  \item tipotemp 3 options: 0 to not consider temperature, 1 to consider maximum daily temperature, 2 average daily temperature, 3 maximum and minimum daily temperature (default=1). This option only affects the average daily demand forecasting model.
#10  \item tipotempdia a vector with eigth logical variables, a variable for each day over a horizon with 8 days, in each day set 1 to consider temperature and 0 to non consider temperature (dafault=c(1,1,1,1,1,1,1,1)). This option only affects the average daily demand forecasting model.
#10  \item naosimulada two options: 1 to consider unsupervised generation, 0 otherwise (default=0)
#10  \item intercambio two options: 1 to consider exchange, 0 otherwise (default=0)
#10  \item horizonteprevisao three options: 0 to forecasts up to the end of operative week, 1 to forecasts up to 8 days ahead, 2 to forecasts up to final date indicated in the file HORIZONTE.csv (default=2)
#10  \item diasprevisaosemihora number of first days with half hourly load forecasts, the following forecasts are expressed in load levels (default=3)
#10  \item tipokernel three options: 0 to SVM with RBF kernel, 1 to SVM with linear kernel, >=2 to MLP Neural Network (default=0). This option only affects the average daily demand forecasting model.
#10  \item tipocombinada two options: 0 to run a selected forecast model , 1 to run all forecasts models and combines the results (default=0)
#10  \item alfawsvm smoothing constant: a number in the interval [0,0.85]
#10  \item nomerdata filename with filtered data
#10 }
#10
#10 @return
#10 \itemize{
#10  \item CARGA_GLOBAL data.frame with half-hourly load forecasts (also in CSV format, the output file PREVCARGADESSEM_CARGA_GLOBAL.csv)
#10  \item GERACAO_NAO_DESPACHADA data.frame with hourly unsupervised generation forecasts (also in CSV format, the output file PREVCARGADESSEM_GERACAO_NAO_SUPERVISIONADA_HORARIA.csv)
#10  \item SAIDADESSEM data.frame with load forecasts in DESSEM format (also in TXT format, SAIDADESSEM.TXT)
#10  \item DADOS data.frame with hourly profiles of load and temperature (data and forecasts)
#10  \item PREVHORARIA data.frame with hourly load forecasts (also in CSV format, output file PREVCARGADESSEM_CARGA_GLOBAL_HORARIA.csv)
#10 }
#10
#10 @author Brazilian Electric Power Research Center - CEPEL
#10
#10

previsordessem1 <-function(deckLeitora,caminhosaida = ".",
                          tipotemp = 1,horizonteprevisao = "10DIAS",
                          diasprevisaosemihora = 3,
                          tipokernel = 1,alfawsvm = 0){
   # caminho = path dos dados
   # caminhosaida = path arquivos de saida
   # treinamodelo = 0 usa parametros de modelo previamente treinado
   #              = 1 treina modelo
   # tipoinput = 0 importa arquivos csv
   #             1 importa data.frame
   # tipotemp  = 0 sem temperatura
   #           = 1 temperatura maxima do dia
   #           = 2 temperatura media do dia
   #	        = 3 temperaturas maxima e minima do dia
   # tipotempdia = vetor de 8 posicoes, cada posicao macarda com 0 para nao usa temperatura e 1 para usa temperatura
   # naosimulada = 0 nao considera nao supervisionada
   #             = 1 considera nao supervisionada
   # intercambio = 0 nao considera intercambio
   #             = 1 considera intercambio
   # horizonteprevisao = 0 horizonte de previsao depende do dia da semana em que e feita a previsao
   #                   = 1 horizonte de previsao oito dias a frente
   #                   = 2 horizonte de previsao ate data final informada
   # diasprevisaosemihora = dias do horizonte de previsao com resolucao semi-horaria contando a partir do primeiro dia de previsao
   # tipokernel = 0 svm kernel radial
   #            = 1 svm kernel linear
   #	         = 2 rede neural feedforward

   ### Definicoes ====
   ### VOLTAR AQUI ====

   # alfawsvm = constante de alisamente entre 0 e 0.85
   if (alfawsvm<0) alfawsvm=0
   if (alfawsvm>0.85) alfawsvm=0.85
   prevnaivefuzzy=4 # previsao de carga em feriados feita pelo naive fuzzy tipo 4 (melhor entre os tipos 1 e 2)
   tipocombinada = 0
   #subsistema=strsplit(prefixo,"_")[[1]][1]

   options(scipen=999)
   if (diasprevisaosemihora>10) diasprevisaosemihora=3

   DADOS = deckLeitora
   SAIDALEITORA=deckLeitora
   DADOS_FILTRADOS = deckLeitora



   # if (tipocombinada<=1){
   #    # CHAMADA DA ROTINA DE FILTRAGEM DE DADOS HISTORICOS DA CARGA DESPACHADA
   #    message("TRATAMENTO DE DADOS")
   #    # numeroclusters=40
   #    perfis=DADOS$DATAHIST
   #    DADOS_FILTRADOS=FILTRAGEM(40,DADOS$DATAHIST,DADOS$SERIEHIST,DADOS,1,tipotemp,alfawsvm) # FILTRA CARGA DESPACHADA
   #    serie_carga_global=DADOS_FILTRADOS$SERIEHIST_FILTRADAS$seriecargafiltrada
   #    if (naosimulada==1) {
   #       serie_nao_simulada=FILTRANAOSIMULADA(serie_nao_simulada) # FILTRA GERACAO NAO SIMULADA
   #       serie_carga_global=serie_carga_global+serie_nao_simulada # agrega geracao nao simulada para formar a carga global filtrada
   #       # SUBSTITUI A CARGA DESPACHADA PELA CARGA GLOBAL NO OBJETO DADOS_FILTRADOS
   #       aux=data.frame(matrix(serie_carga_global,nrow=dim(DADOS_FILTRADOS$DATAHIST_FILTRADOS)[1],ncol=24,byrow=T))
   #       aux1=rownames(DADOS_FILTRADOS$DATAHIST_FILTRADOS)
   #       DADOS_FILTRADOS$DATAHIST_FILTRADOS[,1:24]=aux
   #       rownames(DADOS_FILTRADOS$DATAHIST_FILTRADOS)=aux1
   #       DADOS_FILTRADOS$SERIEHIST_FILTRADAS$seriecargafiltrada=serie_carga_global
   #    }
   #
   #    SAIDAFILTRAGEM=DADOS_FILTRADOS
   #
   #    if (tipocombinada>0){
   #       #SALVA UM ARQUIVO R.DATA COM OS DADOS E OS DADOS FILTRADOS
   #       if (tipoinput==0) nomerdata=as.character(basename(caminho))
   #       if (tipoinput==1) nomerdata=paste(caminho$HORIZONTE[1,2],caminho$HORIZONTE[1,3],caminho$HORIZONTE[1,4],sep="-")
   #       auxnome=nomerdata
   #       n=1
   #       while (file.exists(file.path(caminhosaida,paste0("DADOS_FILTRADOS_",nomerdata,".RData")))){
   #          nomerdata=paste0(auxnome,"_",n)
   #          n=n+1
   #       }
   #       save(SAIDALEITORA,SAIDAFILTRAGEM, file = file.path(caminhosaida,paste0("DADOS_FILTRADOS_",nomerdata,".RData")))
   #    }
   # }else{
   #    message("CARREGA DADOS TRATADOS")
   #    perfis=DADOS$DATAHIST
   #    load(file.path(caminhosaida,paste0("DADOS_FILTRADOS_",nomerdata,".RData")))
   #    DADOS_FILTRADOS=SAIDAFILTRAGEM
   #    serie_carga_global=DADOS_FILTRADOS$SERIEHIST_FILTRADAS$seriecargafiltrada
   #    if (naosimulada==1) {
   #       serie_nao_simulada=FILTRANAOSIMULADA(serie_nao_simulada) # FILTRA GERACAO NAO SIMULADA
   #       serie_carga_global=serie_carga_global+serie_nao_simulada # agrega geracao nao simulada para formar a carga global filtrada
   #       # SUBSTITUI A CARGA DESPACHADA PELA CARGA GLOBAL NO OBJETO DADOS_FILTRADOS
   #       aux=data.frame(matrix(serie_carga_global,nrow=dim(DADOS_FILTRADOS$DATAHIST_FILTRADOS)[1],ncol=24,byrow=T))
   #       aux1=rownames(DADOS_FILTRADOS$DATAHIST_FILTRADOS)
   #       DADOS_FILTRADOS$DATAHIST_FILTRADOS[,1:24]=aux
   #       rownames(DADOS_FILTRADOS$DATAHIST_FILTRADOS)=aux1
   #       DADOS_FILTRADOS$SERIEHIST_FILTRADAS$seriecargafiltrada=serie_carga_global
   #    }
   # }


   constnorm=1.25*max(deckLeitora$SERIEHIST$Carga) # constante de normalizacao
   ultimo_carga_horaria_verificada=tail(deckLeitora$SERIEHIST$Carga,1)

   #==============================================
   # TREINAMENTO DOS MODELOS DE PREVISAO DA CARGA
   #==============================================
   message("TREINA MODELOS E EXECUTA PREVISAO")
   tipos_de_dia= unique(deckLeitora$DATAPREV$DiaEspecial) # tipos de dias no horizonte de previsao

   # MODELO 1: PREVISAO DA DEMANDA MEDIA DIARIA, UM MODELO PARA CADA DIA DA SEMANA
   lagsmodelo1=c(1,2,7) # numero de dias
   modelo1=treinamodelo1(deckLeitora,lagsmodelo1,tipotemp,tipokernel,constnorm,datasprev,alfawsvm) # modelo de previsao da demanda media diaria

   # message("Treina modelo 1 para previsao da demanda media diaria")
   # if (tipotemp>0 & (min(tipotempdia)==0)) {
   #    # ENTRA AQUI SE CONSIDERA TEMPERATURA POREM NAO PARA TODOS OS DIAS
   #    modelo1semtemp=treinamodelo1(DADOS_FILTRADOS,lagsmodelo1,0,tipokernel,constnorm,datasprev,alfawsvm) # modelo de previsao da demanda media diaria
   #    message("Treina modelo 1 para previsao da demanda media diaria sem temperatura")
   # }

   # # MODELO 2: PREVISAO DO PERFIL CURVA DE CARGA EM DIAS DE FERIADO NACIONAL 1 E 2 E NATAL E ANO NOVO 6
   # if ((length(which(tipos_de_dia==1 | tipos_de_dia==2 | tipos_de_dia==6))>0)&(prevnaivefuzzy==0)) {
   #    modelo2<-treinamodelo2(DADOS_FILTRADOS,tipotemp,alfawsvm)
   #    message("Treina modelo 2 para previsao do perfil de carga de feriados")
   # }
   #
   # # MODELO 3: PREVISAO CURVA DE CARGA VESPERA DE FERIADOS
   # if (length(which(tipos_de_dia==8 | tipos_de_dia==9))>0) {
   #    if (prevnaivefuzzy==0) prevnaivefuzzy=4
   #    #modelo3<-treinamodelo3(DADOS_FILTRADOS,tipotemp,alfawsvm)
   #    #message("Treina modelo 3 para previsao do perfil de carga de vespera de feriados")
   # }
   #
   # # MODELO 4: PREVISAO CURVA DE CARGA PARA DIA NORMAL E DIA ESPECIAL
   # lagsm4=c(7,14)
   # if (prevnaivefuzzy==0) {
   #    if (length(which(tipos_de_dia==0 | tipos_de_dia==11))>=0) {
   #       modelo4<-treinamodelo4(DADOS_FILTRADOS,lagsm4,tipotemp,constnorm,alfawsvm)
   #       message("Treina modelo 4 para previsao do perfil de carga de dia normal e especial")
   #    }
   #    if (tipotemp>0 & (min(tipotempdia)==0)) {
   #       # ENTRA AQUI SE CONSIDERA TEMPERATURA POREM NAO PARA TODOS OS DIAS
   #       modelo4semtemp<-treinamodelo4(DADOS_FILTRADOS,lagsm4,0,constnorm,alfawsvm) # modelo de previsao da demanda media diaria
   #       message("Treina modelo 4 para previsao do perfil de carga de dia normal e especial sem temperatura")
   #    }
   # }
   #
   # # MODELO 5: PREVISAO DO PERFIL DE CARGA PARA QUARTA FEIRA DE CINZAS
   # if ((length(which(tipos_de_dia==5))>0)&(prevnaivefuzzy==0)) {
   #    modelo5<-treinamodelo5(DADOS_FILTRADOS,tipotemp,alfawsvm)
   #    message("Treina modelo 5 para previsao do perfil de carga da quarta feira de cinzas")
   # }
   #
   # # MODELO 6: PREVISAO DO PERFIL DE CARGA PARA O CARNAVAL
   # if ((length(which(tipos_de_dia==3 | tipos_de_dia==4))>0)&(prevnaivefuzzy==0)) {
   #    modelo6<-treinamodelo6(DADOS_FILTRADOS,tipotemp,alfawsvm)
   #    message("Treina modelo 6 para previsao do perfil de carga do carnaval")
   # }
   #
   # # MODELO 7: PREVISAO CURVA DE CARGA PARA DIA APOS FERIADOS
   # if ((length(which(tipos_de_dia==7 | tipos_de_dia==10))>0)&(prevnaivefuzzy==0)) {
   #    modelo7<-treinamodelo7(DADOS_FILTRADOS,tipotemp,alfawsvm)
   #    message("Treina modelo 7 para previsao do perfil de carga apos feriado")
   # }
   #
   # # MODELO 8: PREVISAO DA DEMANDA MEDIA DA VESPERA DE FERIADO
   # if ((length(which(tipos_de_dia==9))>0)&(prevnaivefuzzy==0)) {
   #    modelo8<-treinademandamediavespera(DADOS_FILTRADOS,constnorm)
   #    message("Treina modelo 8 para demanda media da vespera de feriado")
   # }


   #==========================
   # PREVISOES DE CARGA GLOBAL
   #==========================
   datainicial=datasprev[1] # primeiro dia do horizonte de previsao
   datafinal=tail(datasprev,1)
   datas=seq(as.Date(datainicial),as.Date(datafinal),1)
   conjuntoHIST=DADOS_FILTRADOS$DATAHIST_FILTRADOS
   colnames(conjuntoHIST)=paste("X",seq(1,dim(conjuntoHIST)[2],1),sep="")
   conjuntoPREV=DADOS$DATAPREV
   aux=cbind(matrix(0,dim(conjuntoPREV)[1],24),conjuntoPREV[,1:2],conjuntoPREV[,3:26])
   colnames(aux)=paste("X",seq(1,dim(conjuntoHIST)[2],1),sep="")
   BASEDADOS=rbind(deckLeitora$DATAHIST,deckLeitora$DATAPREV) # base com todos os dados dos periodos historico e de previsao
   rm(aux)
   rm(conjuntoHIST)
   rm(conjuntoPREV)
   seriediaria = apply(DADOS_FILTRADOS$DATAHIST_FILTRADOS[,1:24], 1, mean) # serie historica da carga
   dadospu = sweep(DADOS_FILTRADOS$DATAHIST_FILTRADOS[,1:24],1,seriediaria,FUN='/') # perfis de carga pu do historico
   previsaoMW=c()
   verificadoMW=c()
   indice0=grep(as.character(datas[1]),names(seriediaria))
   if (length(indice0)==0) {
      indice0=length(seriediaria)
   } else {
      indice0=indice0-1
   }
   previsaomediadiaria=c()
   previsaoperfildiario=dadospu[1:indice0,1:24]
   previsaoperfildiarioMW=BASEDADOS[1:length(seriediaria),1:24]
   verificadomediadiaria=c()
   seriediaria1=seriediaria
   #seriediaria1temp=apply(BASEDADOS[,27:50],1,max)
   seriediaria1temp=apply(BASEDADOS[,37:45],1,max)
   seriediariaferiados=BASEDADOS[,25]

   # recodifica feriados para o modelo de previsao de dia normal
   # a variavel seriediariaferiados serve para indicar se dias anteriores a previsao sao feriados
   seriediariaferiados[which(BASEDADOS[,25]==2)]=1 # feriado com perfil de domingo
   seriediariaferiados[which(BASEDADOS[,25]==3)]=1 # segunda de Carnaval
   seriediariaferiados[which(BASEDADOS[,25]==4)]=1 # terca de Carnaval
   seriediariaferiados[which(BASEDADOS[,25]==5)]=0 # quarta de cinzas
   seriediariaferiados[which(BASEDADOS[,25]==6)]=1 # Natal e 1 de janeiro
   seriediariaferiados[which(BASEDADOS[,25]==7)]=1 # dia apos Natal e 1 de janeiro
   seriediariaferiados[which(BASEDADOS[,25]==8)]=1 # vespera de Natal
   seriediariaferiados[which(BASEDADOS[,25]==9)]=1 # vespera de feriado
   seriediariaferiados[which(BASEDADOS[,25]==10)]=1 # dia apos feriado
   seriediariaferiados[which(BASEDADOS[,25]==11)]=1 # dia especial e recesso

   nomesdiasdasemana=c("domingo","segunda-feira","terca-feira","quarta-feira","quinta-feira","sexta-feira","sabado")

   # PERCORRE DIAS DO HORIZONTE DE PREVISAO
   for (i in 1:length(datas)) {

      # DADOS DO DIA DE PREVISAO
      message(paste("Previsao para o dia",as.character(datas[i])))
      semaine=weekdays1(as.Date(datas[i]))
      DIASEMANA=grep(semaine,nomesdiasdasemana)
      indice=grep(as.character(datas[i]),rownames(BASEDADOS))
      xlags=seriediaria1[indice-lagsmodelo1]/constnorm # valores passados da carga
      if (tipotemp>0) xlagstemp=seriediaria1temp[indice-lagsmodelo1]/100. # valores passados da carga
      xlagsferiados=seriediariaferiados[indice-lagsmodelo1]

      auxxferiado=BASEDADOS[indice,25] # MODIFICADO 17 JULHO 2018
      vetorxferiado=rep(0,10) # vetor com 10 elementos que marca o tipo de feriado

      if (auxxferiado==1) vetorxferiado[1]=1 # feriado nacional perfil de sabado
      if (auxxferiado==2) vetorxferiado[2]=1 # feriado nacional perfil de domingo
      if ((auxxferiado==3) | (auxxferiado==4)) vetorxferiado[3]=1 # segunda e terca de carnaval
      if (auxxferiado==5) vetorxferiado[4]=1 # quarta feira de cinzas
      if (auxxferiado==6) vetorxferiado[5]=1 # Natal e 1 de Janeiro
      if (auxxferiado==7) vetorxferiado[6]=1 # Dia apos Natal e 1 de Janeiro
      if (auxxferiado==8) vetorxferiado[7]=1 # Vespera de Natal e 1 de Janeiro
      if (auxxferiado==9) vetorxferiado[8]=1 # Vespera de feriado
      if (auxxferiado==10) vetorxferiado[9]=1 # Dia apos feriado
      if (auxxferiado==11) vetorxferiado[10]=1 # Dia especial e recesso FIM MODIFICA 17 JULHO 2018

      xverao=BASEDADOS[indice,26]
      if (tipotemp>0) {
         #xtempmax=apply(BASEDADOS[indice,27:50],1,max)/100. # temperatura MAXIMA diaria
         xtempmax=apply(BASEDADOS[indice,37:45],1,max)/100. # temperatura MAXIMA diaria
         xtempmedia=apply(BASEDADOS[indice,27:50],1,mean)/100. # temperatura MEDIA diaria
         xtempmin=apply(BASEDADOS[indice,27:50],1,min)/100. # temperatura MINIMA diaria
         #xtempmaxdelta=c(0,diff(apply(BASEDADOS[,27:50],1,max)/100.,1))
         xtempmaxdelta=c(0,diff(apply(BASEDADOS[,37:45],1,max)/100.,1))
         xtempmaxdelta=xtempmaxdelta[indice]
         prevtempmax=xtempmax
         prevtempmedia=xtempmedia
      }

      flaglagsferiado=sum(xlagsferiados) # conta feriados nos dias anteriores ao dia da previsao , veja vetor xlagsferiados

      tipotempaux=tipotempdia[i]*tipotemp

      if ((sum(vetorxferiado)==0)&(flaglagsferiado==0)) { # MODIFICADO 17 JULHO 2018
         # TREINA MODELO LOCAL DEMANDA MEDIA DIARIA NORMAL ENTRA AQUI APENAS SE DIA NORMAL PRECEDIDO DE DIAS NORMAIS
         inputdiasemana=semaine
         inputmes=as.numeric(substr(as.character(datas[i]),6,7))
         modelolocal=treinademandamedia(DADOS_FILTRADOS,lagsmodelo1,inputdiasemana,inputmes,tipokernel,tipotempaux,constnorm,prevtempmax,prevtempmedia,alfawsvm)
         # MODELO LOCAL SUBSTITUI MODELO 1 NOS CASOS DE DIA NORMAL PRECEDIDO DE DIAS NORMAIS
      }

      if (vetorxferiado[1]==1 | vetorxferiado[2]==1){
         # TREINA MODELO LOCAL DEMANDA MEDIA DIARIA FERIADO ENTRA AQUI SE FERIADO NACIONAL COM PERFIL DE SABADO OU DOMINGO
         modelolocal_feriado=treinademandamedia_feriado(DADOS_FILTRADOS,lagsmodelo1,tipokernel,tipotempaux,constnorm,alfawsvm)
         # MODELO LOCAL SUBSTITUI MODELO 2 NOS CASOS DE FERIADOS NACIONAIS TIPO 1 E TIPO 2
      }


      ####################################
      # PREVISAO DA DEMANDA MEDIA DIARIA #
      ####################################
      xferiado1=vetorxferiado

      if (tipotempaux==0)dados=data.frame(t(xlags),t(xferiado1),xverao)
      if (tipotempaux==1)dados=data.frame(t(xlags),t(xferiado1),xverao,xtempmax)
      if (tipotempaux==2)dados=data.frame(t(xlags),t(xferiado1),xverao,xtempmedia)
      if (tipotempaux==3)dados=data.frame(t(xlags),t(xferiado1),xverao,xtempmax,xtempmin)

      names(dados)=paste("X",seq(1,length(dados),1),sep="")
      if (tipotempaux>0) {
         # ENTRA AQUI SE CONSIDERA TEMPERATURA
         if (tipokernel>=2){
            #previsao_media=compute(modelo1[[DIASEMANA]],dados)$net.result*constnorm  # PREVISAO DEMANDA MEDIA POR REDE NEURAL
            previsao_media=predict(modelo1[[DIASEMANA]],dados)*constnorm
         }else{
            previsao_media=predict(modelo1[[DIASEMANA]],dados) # PREVISAO DEMANDA MEDIA POR SVM OU REGRESSAO QUANTILICA
         }
      } else {
         if (tipotemp==0) {
            # PREVISAO SEM TEMPERATURA
            if (tipokernel>=2){
               #previsao_media=compute(modelo1[[DIASEMANA]],dados)$net.result*constnorm  # PREVISAO DEMANDA MEDIA POR REDE NEURAL
               previsao_media=predict(modelo1[[DIASEMANA]],dados)*constnorm  # PREVISAO DEMANDA MEDIA POR REDE NEURAL
            }else{
               previsao_media=predict(modelo1[[DIASEMANA]],dados) # PREVISAO DEMANDA MEDIA POR SVM OU REGRESSAO QUANTILICA
            }
         } else {
            # PREVISAO SEM TEMPERATURA
            if (tipokernel>=2){
               #previsao_media=compute(modelo1semtemp[[DIASEMANA]],dados)$net.result*constnorm # PREVISAO DEMANDA MEDIA POR REDE NEURAL
               previsao_media=predict(modelo1semtemp[[DIASEMANA]],dados)*constnorm # PREVISAO DEMANDA MEDIA POR REDE NEURAL
            }else{
               previsao_media=predict(modelo1semtemp[[DIASEMANA]],dados) # PREVISAO DEMANDA MEDIA POR SVM OU REGRESSAO QUANTILICA
            }
         }
      }

      if ((sum(vetorxferiado)!=0)|(flaglagsferiado>0)) { # MODIFICADO 17 JULHO 2018
         # ENTRA AQUI SE DIA NAO E NORMAL PRECEDIDO DE DIAS NORMAIS
         if (vetorxferiado[1]==1 | vetorxferiado[2]==1){
            # ENTRA AQUI SE DIA FERIADO TIPO 1 OU 2
            if (vetorxferiado[1]==1) {
               chaveferiado=0 # feriado perfil sabado
            }else{
               chaveferiado=1 # feriado perfil domingo
            }
            if (semaine=="sabado" | semaine=="domingo") {
               chavediautil=0
            } else {
               chavediautil=1
            }
            chaveverao=BASEDADOS[indice,26]
            if (tipotempaux==0) dadosmodelolocal=data.frame(t(xlags),chaveferiado,chaveverao,chavediautil)
            if (tipotempaux==1) dadosmodelolocal=data.frame(xtempmax,t(xlags),chaveferiado,chaveverao,chavediautil)
            if (tipotempaux==2) dadosmodelolocal=data.frame(xtempmedia,t(xlags),chaveferiado,chaveverao,chavediautil)
            if (tipotempaux==3) dadosmodelolocal=data.frame(xtempmax,xtempmin,t(xlags),chaveferiado,chaveverao,chavediautil)
            names(dadosmodelolocal)=paste("X",seq(1,length(dadosmodelolocal),1),sep="")
            if (tipokernel>=2){
               #previsaolocal1=compute(modelolocal_feriado,dadosmodelolocal)$net.result*constnorm # PREVISAO POR REDE NEURAL
               previsaolocal1=predict(modelolocal_feriado,dadosmodelolocal)*constnorm # PREVISAO POR REDE NEURAL
            }else{
               previsaolocal1=predict(modelolocal_feriado$modelo,dadosmodelolocal)*constnorm # PREVISAO POR SVM OU REGRESSAO QUANTILICA
            }
            seriediaria1[indice]=previsaolocal1 # atualiza seriediaria1 com a ultima previsao
            previsaomediadiaria=c(previsaomediadiaria,previsaolocal1) # serie de previsoes da demanda media diaria
         }else{
            if ((BASEDADOS[indice,25]==9)&(prevnaivefuzzy==0)){
               # ENTRA AQUI SE VESPERA DE FERIADO (TIPO 9)
               previsaolocal1=as.numeric((coef(modelo8$modelo) [1]+coef(modelo8$modelo)[2]*dados[1])*constnorm) # PREVISAO PELO MODELO DE VESPERA DE FERIADO
               seriediaria1[indice]=previsaolocal1 # atualiza seriediaria1 com a ultima previsao
               previsaomediadiaria=c(previsaomediadiaria,previsaolocal1) # serie de previsoes da demanda media diaria
            } else {
               # OUTROS FERIADOS E DIAS ESPECIAIS PREVISAO FEITA PELO MODELO GLOBAL
               if (tipokernel>=2){
                  seriediaria1[indice]=previsao_media # atualiza seriediaria1 com a ultima previsao
                  previsaomediadiaria=c(previsaomediadiaria,(previsao_media)) # serie de previsoes da demanda media diaria
               }else{
                  seriediaria1[indice]=previsao_media*constnorm # atualiza seriediaria1 com a ultima previsao
                  previsaomediadiaria=c(previsaomediadiaria,(previsao_media)*constnorm) # serie de previsoes da demanda media diaria
               }
            }
         }
      } else {
         # ENTRA AQUI SE DIA NORMAL PRECEDIDO DE DIAS NORMAIS COM PREVISAO FEITA PELO MODELO LOCAL
         if (length(which(xlagsferiados!=0))>0) xlagsferiados[which(xlagsferiados!=0)]=1
         if (tipotempaux==0) dadosmodelolocal=data.frame(t(xlags),t(xlagsferiados))
         if (tipotempaux==1) dadosmodelolocal=data.frame(t(xlags),t(xlagsferiados),xtempmax)
         if (tipotempaux==2) dadosmodelolocal=data.frame(t(xlags),t(xlagsferiados),xtempmedia)
         if (tipotempaux==3) dadosmodelolocal=data.frame(t(xlags),t(xlagsferiados),xtempmax,xtempmin)
         names(dadosmodelolocal)=paste("X",seq(1,length(dadosmodelolocal),1),sep="")
         if (tipokernel>=2){
            if (modelolocal$chavemodelorq==0) {
               # RNA
               #previsaolocal1=compute(modelolocal$modelo,dadosmodelolocal)$net.result*constnorm # PREVISAO DEMANDA MEDIA POR REDE NEURAL
               previsaolocal1=predict(modelolocal$modelo,dadosmodelolocal)*constnorm # PREVISAO DEMANDA MEDIA POR REDE NEURAL
            } else {
               # REGRESSAO QUANTILICA
               previsaolocal1=predict(modelolocal$modelo,dadosmodelolocal)*constnorm # PREVISAO DEMANDA MEDIA POR SVM
            }
         }else{
            # SVM
            previsaolocal1=predict(modelolocal$modelo,dadosmodelolocal)*constnorm # PREVISAO DEMANDA MEDIA POR SVM
         }
         seriediaria1[indice]=previsaolocal1 # atualiza seriediaria1 com a ultima previsao
         previsaomediadiaria=c(previsaomediadiaria,previsaolocal1) # serie de previsoes da demanda media diaria
      }
      verificadomediadiaria=c(verificadomediadiaria,mean(as.numeric(BASEDADOS[indice,1:24])))

      #message(previsaomediadiaria)

      ##############################
      # PREVISAO DO PERFIL HORARIO #
      ##############################
      TEMPERATURA=BASEDADOS[indice,27:50]/100. # temperatura horaria do dia da previsao
      MES=as.numeric(substr(as.character(datas[i]),6,7))

      xferiado=BASEDADOS[indice,25] #tipo do dia da previsao
      if (xferiado==9) { # MOIFICADO 18 JULHO 2018
         # vespera de feriado no sabado ou no domingo
         if (DIASEMANA==1 | DIASEMANA==7) xferiado=0
      }
      if (xferiado==7 | xferiado==10) { # MODIFICADO 18 JULHO 2018
         # pos feriado em sabado e domingo
         if (DIASEMANA==1 | DIASEMANA==7) xferiado=0
      }
      if (xferiado==0 | xferiado==11) {

         # previsao curva de carga de dia normal e dia especial CHAMA MODELO 4
         xxferiado=0
         if (xferiado==11) xxferiado=1 # DUMMY DIA ESPECIAL
         if (prevnaivefuzzy==0) {
            vetor_mes0=rep(0,length(modelo4[[25]]))
            vetor_mes0[which(modelo4[[25]]==MES)]=1
         }
         diasemana0=rep(0,7)
         diasemana0[DIASEMANA]=1
         verao0=xverao
         previsaodemandamedia0=previsaomediadiaria[i]/constnorm

         # TREINA MODELO4 COM JANELA MENSAL
         if (xxferiado==0) {
            tipomes=MES
            tipoferiado=xxferiado
            tipodiasemana=DIASEMANA
            tipoverao=xverao
            modelo4local<-treinamodelo4local(DADOS_FILTRADOS,lagsm4,tipotempaux,MES,xxferiado,DIASEMANA,xverao,constnorm,alfawsvm)
         }

         if ((prevnaivefuzzy==0)|(xferiado==0)){
            #fuzzy apenas para feriados
            auxperfil=c()
            for (j in 1:24) {
               YYLAG=previsaoperfildiario[(indice-lagsm4),j]
               if (tipotempaux>0) {
                  if (xxferiado==1){
                     # entra aqui se dia especial
                     if (modelo4[[26]]==1) {
                        input=c(as.numeric(TEMPERATURA[j]),vetor_mes0,verao0,diasemana0,xxferiado,previsaodemandamedia0,YYLAG)
                     } else {
                        input=c(as.numeric(TEMPERATURA[j]),vetor_mes0,verao0,diasemana0,previsaodemandamedia0,YYLAG)
                     }

                     names(input)=paste("X",seq(1,length(input),1),sep="")
                     valor=predict(modelo4[[j]]$best.model,t(input)) # previsao perfil carga horaria em pu
                  } else {
                     # entra aqui se dia normal
                     input=c(as.numeric(TEMPERATURA[j]),YYLAG,previsaodemandamedia0)
                     names(input)=paste("X",seq(1,length(input),1),sep="")
                     valor=predict(modelo4local[[j]]$best.model,t(input)) # previsao perfil carga horaria em pu
                  }
               } else {
                  if ((xxferiado==1)&(prevnaivefuzzy==0)){
                     # entra aqui se dia especial
                     if (tipotemp==0) {
                        if (modelo4[[26]]==1) {
                           input=c(vetor_mes0,verao0,diasemana0,xxferiado,previsaodemandamedia0,YYLAG)
                        } else {
                           input=c(vetor_mes0,verao0,diasemana0,previsaodemandamedia0,YYLAG)
                        }
                        names(input)=paste("X",seq(1,length(input),1),sep="")
                        valor=predict(modelo4[[j]]$best.model,t(input)) # previsao perfil carga horaria em pu
                     } else {
                        if (modelo4[[26]]==1) {
                           input=c(vetor_mes0,verao0,diasemana0,xxferiado,previsaodemandamedia0,YYLAG)
                        } else {
                           input=c(vetor_mes0,verao0,diasemana0,previsaodemandamedia0,YYLAG)
                        }
                        names(input)=paste("X",seq(1,length(input),1),sep="")
                        valor=predict(modelo4semtemp[[j]]$best.model,t(input)) # previsao perfil carga horaria em pu
                     }
                  }else{
                     if ((prevnaivefuzzy==0)|(xxferiado==0)){
                        # entra aqui se dia normal
                        input=c(YYLAG,previsaodemandamedia0)
                        names(input)=paste("X",seq(1,length(input),1),sep="")
                        valor=predict(modelo4local[[j]]$best.model,t(input)) # previsao perfil carga horaria em pu
                     }
                  }
               }

               previsaoMW=c(previsaoMW,(valor*previsaomediadiaria[i]))
               verificadoMW=c(verificadoMW,as.numeric(BASEDADOS[indice,j]))
               auxperfil=c(auxperfil,valor)
            }

         }else{
            #previsao por naive-fuzzy para dia com feriado estadual ou municipal 11
            previsao_naivefuzzy=naivefuzzy(BASEDADOS,datas,i,prevnaivefuzzy,previsaomediadiaria[i])
            if ((prevnaivefuzzy==1)|(prevnaivefuzzy==3)){
               previsaoMW=c(previsaoMW,previsao_naivefuzzy$previsao*previsaomediadiaria[i])
            }else{
               previsaoMW=c(previsaoMW,previsao_naivefuzzy$previsao)
            }
         }


      } else {
         if (xferiado==8 | xferiado==9) {
            # previsao da curva de carga da vespera de feriados 9 e vesperas de Natal e Ano Novo 8 CHAMA MODELO 3
            xxferiado=0
            if (xferiado==8) xxferiado=1 # MARCA VESPERA DE NATAL E VESPERA DE ANO NOVO
            if (prevnaivefuzzy==0){
               vetor_mes4=rep(0,(length(modelo3[[25]])))
               vetor_mes4[which(modelo3[[25]]==MES)]=1
               if (DIASEMANA==2 | DIASEMANA==5) {
                  # MARCA VESPERAS DE FERIADOS NAS SEGUNDAS E QUINTAS
                  diasemana4=1
               } else {
                  diasemana4=0
               }
               auxperfil=c()

               for (j in 1:24) {
                  if (tipotemp>0) {
                     input=c(as.numeric(TEMPERATURA[j]),vetor_mes4,xxferiado,diasemana4)
                  } else {
                     input=c(vetor_mes4,xxferiado,diasemana4)
                  }

                  names(input)=paste("X",seq(1,length(input),1),sep="")
                  valor=predict(modelo3[[j]]$best.model,t(input)) # previsao perfil carga horaria em pu
                  previsaoMW=c(previsaoMW,valor*previsaomediadiaria[i])
                  verificadoMW=c(verificadoMW,as.numeric(BASEDADOS[indice,j]))
                  auxperfil=c(auxperfil,valor)
               }
            }else{
               previsao_naivefuzzy=naivefuzzy(BASEDADOS,datas,i,prevnaivefuzzy,previsaomediadiaria[i])
               previsaoMW=c(previsaoMW,previsao_naivefuzzy$previsao)
            }
         } else {
            if (xferiado==5) {
               # Previsao do perfil de carga da quarta feira de cinzas CHAMA MODELO 5
               xxverao=xverao
               auxperfil=c()

               if (prevnaivefuzzy==0){
                  for (j in 1:24) {
                     if (tipotemp>0) {
                        input=c(as.numeric(TEMPERATURA[j]),xxverao)
                     } else {
                        input=c(xxverao)
                     }
                     names(input)=paste("X",seq(1,length(input),1),sep="")
                     #valor=predict(modelo5[[j]]$best.model,t(input)) # previsao carga horaria em pu
                     valor=predict(modelo5[[j]],t(input)) # previsao perfil carga horaria em pu
                     previsaoMW=c(previsaoMW,valor*previsaomediadiaria[i])
                     verificadoMW=c(verificadoMW,as.numeric(BASEDADOS[indice,j]))
                     auxperfil=c(auxperfil,valor)
                  }
               }else{
                  #previsao por naive-fuzzy
                  previsao_naivefuzzy=naivefuzzy(BASEDADOS,datas,i,prevnaivefuzzy,previsaomediadiaria[i])
                  if ((prevnaivefuzzy==1)|(prevnaivefuzzy==3)){
                     previsaoMW=c(previsaoMW,previsao_naivefuzzy$previsao*previsaomediadiaria[i])
                  }else{
                     previsaoMW=c(previsaoMW,previsao_naivefuzzy$previsao)
                  }
               }
            } else {
               if (xferiado==3 | xferiado==4) {
                  # Previsao de perfil de carga para Segunda e Terca de Carnaval CHAMA MODELO 6
                  xxverao=xverao
                  auxperfil=c()

                  if ((prevnaivefuzzy==0)){
                     for (j in 1:24) {
                        if (tipotemp>0){
                           # considera a temperatura
                           if (xferiado==3){
                              input=c(as.numeric(TEMPERATURA[j]),xxverao,0) # segunda de carnaval
                           } else {
                              input=c(as.numeric(TEMPERATURA[j]),xxverao,1) # terca de carnaval
                           }
                        } else {
                           # Nao considera a temperatura
                           if (xferiado==3){
                              input=c(xxverao,0) # segunda de carnaval
                           } else {
                              input=c(xxverao,1) # terca de carnaval
                           }
                        }
                        names(input)=paste("X",seq(1,length(input),1),sep="")
                        valor=predict(modelo6[[j]]$best.model,t(input)) # previsao perfil carga horaria em pu
                        previsaoMW=c(previsaoMW,valor*previsaomediadiaria[i])
                        verificadoMW=c(verificadoMW,as.numeric(BASEDADOS[indice,j]))
                        auxperfil=c(auxperfil,valor)
                     }
                  }else{
                     #previsao por naive-fuzzy
                     previsao_naivefuzzy=naivefuzzy(BASEDADOS,datas,i,prevnaivefuzzy,previsaomediadiaria[i])
                     if ((prevnaivefuzzy==1)|(prevnaivefuzzy==3)){
                        previsaoMW=c(previsaoMW,previsao_naivefuzzy$previsao*previsaomediadiaria[i])
                     }else{
                        previsaoMW=c(previsaoMW,previsao_naivefuzzy$previsao)
                     }
                  }
               } else {
                  if (xferiado==7 | xferiado==10) {
                     # Previsao da curva de carga de dia apos feriados nacionais 10 e dia apos Natal e Ano Novo 7 CHAMA MODELO 7
                     xxferiado=0
                     if (xferiado==7) xxferiado=1 # dia seguinte ao Natal e Ano Novo
                     if (prevnaivefuzzy==0){
                        vetor_mes4=rep(0,length(modelo7[[25]]))
                        vetor_mes4[which(modelo7[[25]]==MES)]=1
                        if (DIASEMANA==6) { # se dia apos feriado e uma sexta
                           diasemana4=1
                        } else {
                           diasemana4=0
                        }
                        verao4=xverao
                        auxperfil=c()

                        for (j in 1:24) {
                           if (tipotemp>0) {
                              input=c(as.numeric(TEMPERATURA[j]),verao4,vetor_mes4,xxferiado,diasemana4)
                           } else {
                              input=c(verao4,vetor_mes4,xxferiado,diasemana4)
                           }
                           names(input)=paste("X",seq(1,length(input),1),sep="")
                           valor=predict(modelo7[[j]]$best.model,t(input)) # previsao perfil carga horaria em pu
                           previsaoMW=c(previsaoMW,valor*previsaomediadiaria[i])
                           verificadoMW=c(verificadoMW,as.numeric(BASEDADOS[indice,j]))
                           auxperfil=c(auxperfil,valor)
                        }
                     }else{
                        #previsao por naive-fuzzy
                        previsao_naivefuzzy=naivefuzzy(BASEDADOS,datas,i,prevnaivefuzzy,previsaomediadiaria[i])
                        if ((prevnaivefuzzy==1)|(prevnaivefuzzy==3)){
                           previsaoMW=c(previsaoMW,previsao_naivefuzzy$previsao*previsaomediadiaria[i])
                        }else{
                           previsaoMW=c(previsaoMW,previsao_naivefuzzy$previsao)
                        }
                     }
                  } else {
                     # previsao da curva de carga de feriados nacionais e Natal e Ano Novo CHAMA MODELO 2
                     xxferiado=c(0,0) # feriado com cara de sabado
                     if (xferiado==2) xxferiado=c(1,0) # feriado com cara de domingo
                     if (xferiado==6) xxferiado=c(0,1) # Natal e Ano Novo
                     xxverao=xverao
                     auxperfil=c()

                     #if ((prevnaivefuzzy==0))|(xferiado==2)){
                     if ((prevnaivefuzzy==0)&(xferiado!=6)){
                        for (j in 1:24) {
                           if (tipotemp>0) {
                              input=c(as.numeric(TEMPERATURA[j]),xxferiado,xxverao)
                           } else {
                              input=c(xxferiado,xxverao)
                           }
                           names(input)=paste("X",seq(1,length(input),1),sep="")
                           valor=predict(modelo2[[j]]$best.model,t(input)) # previsao perfil carga horaria em pu

                           previsaoMW=c(previsaoMW,valor*previsaomediadiaria[i])
                           verificadoMW=c(verificadoMW,as.numeric(BASEDADOS[indice,j]))
                           auxperfil=c(auxperfil,valor)
                        }
                     }else{
                        #previsao por naive-fuzzy
                        previsao_naivefuzzy=naivefuzzy(BASEDADOS,datas,i,prevnaivefuzzy,previsaomediadiaria[i])
                        if ((prevnaivefuzzy==1)|(prevnaivefuzzy==3)){
                           previsaoMW=c(previsaoMW,previsao_naivefuzzy$previsao*previsaomediadiaria[i])
                        }else{
                           previsaoMW=c(previsaoMW,previsao_naivefuzzy$previsao)
                        }
                     }
                  }
               }
            }
         }
      }

      if ((xferiado!=0 & xferiado!=11)&(prevnaivefuzzy>0)){
         #fuzzy apenas para feriados
         if (prevnaivefuzzy!=2){
            if (prevnaivefuzzy!=4){
               medianf=mean(previsao_naivefuzzy$previsao)
               # usa modelo de demanda media diaria SVM - regressao linear
               previsao=(previsao_naivefuzzy$previsao/medianf)*previsaomediadiaria[i]
               BASEDADOS[indice,1:24]=previsao
            }else{
               if (i>0){
                  indicebase=which(rownames(BASEDADOS)==(datas[i]-1))
                  baseMW=BASEDADOS[indicebase,24]
                  previsao=(previsao_naivefuzzy$previsao)
                  BASEDADOS[indice,1:24]=previsao
               }else{
                  previsao=(previsao_naivefuzzy$previsao/medianf)*previsaomediadiaria[i]
                  BASEDADOS[indice,1:24]=previsao
               }
            }
         }else{
            ### usa a previsao FUZZY em MW
            previsao=previsao_naivefuzzy$previsao
            BASEDADOS[indice,1:24]=previsao
         }
      }else{
         # dia normal ou feriado estadual
         if (xferiado==11) {
            auxperfil=previsao_naivefuzzy$previsao/mean(previsao_naivefuzzy$previsao)
            previsao=previsao_naivefuzzy$previsao
            BASEDADOS[indice,1:24]=previsao
         } else {
            auxperfil=auxperfil/mean(auxperfil)
            previsao=auxperfil*previsaomediadiaria[i]
            BASEDADOS[indice,1:24]=previsao
         }
      }

      ######################
      # APLICA HEURISTICAS #
      ######################
      #if (xferiado>0){
      if ((xferiado!=0 & xferiado!=11)&(prevnaivefuzzy>0)){
         ###SELECIONA PADROES PARA A REGRESSAO
         if (xferiado==0){
            if (BASEDADOS[indice-1,25]==0){
               indicedia=which((BASEDADOS[,25]==xferiado)&(weekdays1(rownames(BASEDADOS))==weekdays1(datas[i])))
               indicedia=indicedia[which(rownames(BASEDADOS)[indicedia]<datas[1])]
               indicedia=tail(indicedia,15)
            }else{
               if (weekdays1(rownames(BASEDADOS)[indice])=='sabado'){
                  indicedia=which((weekdays1(rownames(BASEDADOS))=='sexta-feira')&((BASEDADOS[,25]==BASEDADOS[indice-1,25])))
                  indicedia=indicedia+1
                  indicedia=indicedia[which(rownames(BASEDADOS)[indicedia]<datas[1])]
                  indicedia=tail(indicedia,15)
               }else{
                  indicedia=which((BASEDADOS[,25]==xferiado))
                  if (BASEDADOS[indice-1,25]==11){
                     indicedia=indicedia[which(weekdays1(rownames(BASEDADOS)[indicedia])==weekdays1(rownames(BASEDADOS)[indice]))]
                  }else{
                     indicedia=indicedia[which((BASEDADOS[indicedia-1,25]==BASEDADOS[indice-1,25]))]
                  }
                  indicedia=indicedia[which(rownames(BASEDADOS)[indicedia]<datas[1])]
                  if (length(indicedia)==0) indicedia=which((BASEDADOS[,25]==xferiado))
               }
            }
         }else{
            if ((xferiado==1)|(xferiado==2)){
               indicedia=which((BASEDADOS[,25]==xferiado)&(substr(rownames(BASEDADOS),6,7)==substr(datas[i],6,7)))
               if (length(indicedia)<=10){
                  mesdata=as.numeric(substr(datas[i],6,7))
                  tipodia=xferiado
                  tipo0=BASEDADOS[,25]
                  prevdata=datas[i]
                  if (mesdata==1) indicedia=which((tipo0==tipodia)&((substr(rownames(BASEDADOS),6,7)==substr(prevdata,6,7))|(as.numeric(substr(rownames(BASEDADOS),6,7))==mesdata+1)|(as.numeric(substr(rownames(BASEDADOS),6,7))==12)))
                  if (mesdata==12) indicedia=which((tipo0==tipodia)&((substr(rownames(BASEDADOS),6,7)==substr(prevdata,6,7))|(as.numeric(substr(rownames(BASEDADOS),6,7))==1)|(as.numeric(substr(rownames(BASEDADOS),6,7))==mesdata-1)))
                  if ((mesdata!=1)|(mesdata!=12)) indicedia=which((tipo0==tipodia)&((substr(rownames(BASEDADOS),6,7)==substr(prevdata,6,7))|(as.numeric(substr(rownames(BASEDADOS),6,7))==mesdata+1)|(as.numeric(substr(rownames(BASEDADOS),6,7))==mesdata-1)))
               }
               if ((weekdays1(rownames(BASEDADOS)[indice])=='sabado')|(weekdays1(rownames(BASEDADOS)[indice])=='domingo')){
                  indicedia=indicedia[which((weekdays1(rownames(BASEDADOS)[indicedia])=='sabado')|(weekdays1(rownames(BASEDADOS)[indicedia])=='domingo'))]
               }else{
                  indicedia=indicedia[which((weekdays1(rownames(BASEDADOS)[indicedia])!='sabado')&(weekdays1(rownames(BASEDADOS)[indicedia])!='domingo'))]
               }
            }else{
               if (xferiado==11){
                  indicedia=which((BASEDADOS[,25]==11)|(BASEDADOS[,25]==1))
               }else{

                  if ((xferiado==6)|(xferiado==7)|(xferiado==8)){
                     indicedia=which((BASEDADOS[,25]==xferiado))
                  }else{
                     indicedia=which((BASEDADOS[,25]==xferiado)&(substr(rownames(BASEDADOS),6,7)==substr(datas[i],6,7)))
                     if (length(indicedia)<=2){
                        mesdata=as.numeric(substr(datas[i],6,7))
                        tipodia=xferiado
                        tipo0=BASEDADOS[,25]
                        prevdata=datas[i]
                        if (mesdata==1) indicedia=which((tipo0==tipodia)&((substr(rownames(BASEDADOS),6,7)==substr(prevdata,6,7))|(as.numeric(substr(rownames(BASEDADOS),6,7))==mesdata+1)|(as.numeric(substr(rownames(BASEDADOS),6,7))==12)))
                        if (mesdata==12) indicedia=which((tipo0==tipodia)&((substr(rownames(BASEDADOS),6,7)==substr(prevdata,6,7))|(as.numeric(substr(rownames(BASEDADOS),6,7))==1)|(as.numeric(substr(rownames(BASEDADOS),6,7))==mesdata-1)))
                        if ((mesdata!=1)|(mesdata!=12)) indicedia=which((tipo0==tipodia)&((substr(rownames(BASEDADOS),6,7)==substr(prevdata,6,7))|(as.numeric(substr(rownames(BASEDADOS),6,7))==mesdata+1)|(as.numeric(substr(rownames(BASEDADOS),6,7))==mesdata-1)))
                     }
                  }
               }
            }

         }

         indicedia=indicedia[which(rownames(BASEDADOS)[indicedia]<datas[1])]

         #TESTA SE FORAM ENCONTRADOS PADROES SEMELHANTES NO HISTORICO
         if (length(indicedia)>0){

            if (indicedia[1]==1) indicedia=indicedia[-1]
            if (length(which((indicedia-7)<=0))>0) indicedia=indicedia[-which((indicedia-7)<=0)]




            #tempreg=apply(BASEDADOS[indicedia,27:50],1,max)
            tempreg=apply(BASEDADOS[indicedia,37:45],1,max)
            mediareg=apply(BASEDADOS[indicedia,1:24],1,mean)
            maxreg=apply(BASEDADOS[indicedia,1:24],1,max)
            mediaregant=apply(BASEDADOS[indicedia-1,1:24],1,mean)
            maxregant=apply(BASEDADOS[indicedia-1,1:24],1,max)
            mediaregant7=apply(BASEDADOS[indicedia-7,1:24],1,mean)
            flagvesferiado=rep(0,length(tempreg))
            flagvesferiado[which(BASEDADOS[indicedia-1,25]>0)]=1

            ###CALCULA OS LIMITES DE CONFIANCA
            testaregressao=0
            if (BASEDADOS[indice-1,25]==0){ # SE A VESPERA E DIA NORMAL
               if ((xferiado==6)|(xferiado==7)|(xferiado==8)){
                  dutil=rep(0,length(mediareg))###dummy para sinalizar se dia util ou nao
                  if (sum((weekdays1(rownames(BASEDADOS)[indicedia])==(('sabado'))| weekdays1(rownames(BASEDADOS)[indicedia])==(('domingo'))))>0){
                     dutil[which((weekdays1(rownames(BASEDADOS)[indicedia])==(('sabado'))| weekdays1(rownames(BASEDADOS)[indicedia])==(('domingo'))))]=1
                  }
                  #dadosm=data.frame(dutil,mediaregant,mediareg)
                  dadosm=data.frame(mediaregant,mediareg)
                  colnames(dadosm)=c("X1","Y")
                  modelo=lm(Y~X1,data=dadosm)
                  entradaflag=0
                  if ((weekdays1(datas[i])=='sabado')|(weekdays1(datas[i])=='domingo')) entradaflag=1
                  #x0=data.frame(entradaflag,mean(as.numeric(BASEDADOS[indice-1,1:24])))
                  x0=data.frame(mean(as.numeric(BASEDADOS[indice-1,1:24])))
                  names(x0)=c("X1")
                  conf=0.90
                  limsup=predict(modelo,x0,interval="predict",level=conf)[3] #lim superior intervalo confianca
                  liminf=predict(modelo,x0,interval="predict",level=conf)[2] # lim inferior intervalo confianca
                  yhat=predict(modelo,x0,interval="predict",level=conf)[1] # prev primeira hora dia seguinte
                  testaregressao=1
               }else{
                  dadosm=data.frame(tempreg,mediaregant7,mediaregant,mediareg)
                  if ((dim(dadosm)[1]<=dim(dadosm)[2])|(tipotemp==0)){
                     dadosm=data.frame(mediaregant,mediareg)
                     if (dim(dadosm)[1]>dim(dadosm)[2]){
                        colnames(dadosm)=c("X2","Y")
                        modelo=lm(Y~X2,data=dadosm)
                        x0=data.frame(mean(as.numeric(BASEDADOS[indice-1,1:24])))
                        names(x0)=c("X2")
                        conf=0.90
                        limsup=predict(modelo,x0,interval="predict",level=conf)[3] #lim superior intervalo confianca
                        liminf=predict(modelo,x0,interval="predict",level=conf)[2] # lim inferior intervalo confianca
                        yhat=predict(modelo,x0,interval="predict",level=conf)[1] # prev primeira hora dia seguinte
                        testaregressao=1
                     }
                  }else{
                     colnames(dadosm)=c("X1","X2","X3","Y")
                     modelo=lm(Y~X1+X2+X3,data=dadosm)
                     #x0=data.frame(max(as.numeric(BASEDADOS[indice,27:50])),mean(as.numeric(BASEDADOS[indice-7,1:24])),mean(as.numeric(BASEDADOS[indice-1,1:24])))
                     x0=data.frame(max(as.numeric(BASEDADOS[indice,37:45])),mean(as.numeric(BASEDADOS[indice-7,1:24])),mean(as.numeric(BASEDADOS[indice-1,1:24])))
                     names(x0)=c("X1","X2","X3")
                     conf=0.90
                     limsup=predict(modelo,x0,interval="predict",level=conf)[3] #lim superior intervalo confianca
                     liminf=predict(modelo,x0,interval="predict",level=conf)[2] # lim inferior intervalo confianca
                     yhat=predict(modelo,x0,interval="predict",level=conf)[1] # prev primeira hora dia seguinte
                     testaregressao=1
                  }
               }
            }else{  # SE A VESPERA E DIA ESPECIAL
               #### CASO O DIA DA PREVISAO E TIPO 11
               if (BASEDADOS[indice-1,25]==11){
                  dadosm=data.frame(tempreg,mediaregant7,mediareg)
                  if ((dim(dadosm)[1]<=dim(dadosm)[2])|(tipotemp==0)){
                     dadosm=data.frame(mediaregant7,mediareg)
                     if (dim(dadosm)[1]>dim(dadosm)[2]){
                        colnames(dadosm)=c("X2","Y")
                        modelo=lm(Y~X2,data=dadosm)
                        x0=data.frame(mean(as.numeric(BASEDADOS[indice-7,1:24])))
                        names(x0)=c("X2")
                        conf=0.90
                        limsup=predict(modelo,x0,interval="predict",level=conf)[3] #lim superior intervalo confianca
                        liminf=predict(modelo,x0,interval="predict",level=conf)[2] # lim inferior intervalo confianca
                        yhat=predict(modelo,x0,interval="predict",level=conf)[1] # prev primeira hora dia seguinte
                        testaregressao=1
                     }
                  }else{
                     colnames(dadosm)=c("X1","X2","Y")
                     modelo=lm(Y~X1+X2,data=dadosm)
                     #x0=data.frame(max(as.numeric(BASEDADOS[indice,27:50])),mean(as.numeric(BASEDADOS[indice-7,1:24])))
                     x0=data.frame(max(as.numeric(BASEDADOS[indice,37:45])),mean(as.numeric(BASEDADOS[indice-7,1:24])))
                     names(x0)=c("X1","X2")
                     conf=0.90
                     limsup=predict(modelo,x0,interval="predict",level=conf)[3] #lim superior intervalo confianca
                     liminf=predict(modelo,x0,interval="predict",level=conf)[2] # lim inferior intervalo confianca
                     yhat=predict(modelo,x0,interval="predict",level=conf)[1] # prev primeira hora dia seguinte
                     testaregressao=1
                  }
               }
               if (BASEDADOS[indice-1,25]!=11){
                  if ((xferiado==6)|(xferiado==7)|(xferiado==8)){
                     dutil=rep(0,length(mediareg))###dummy para sinalizar se dia util ou nao
                     if (sum((weekdays1(rownames(BASEDADOS)[indicedia])==(('sabado'))| weekdays1(rownames(BASEDADOS)[indicedia])==(('domingo'))))>0){
                        dutil[which((weekdays1(rownames(BASEDADOS)[indicedia])==(('sabado'))| weekdays1(rownames(BASEDADOS)[indicedia])==(('domingo'))))]=1
                     }
                     #dadosm=data.frame(dutil,mediaregant,mediareg)
                     dadosm=data.frame(mediaregant,mediareg)
                     colnames(dadosm)=c("X1","Y")
                     modelo=lm(Y~X1,data=dadosm)
                     entradaflag=0
                     if ((weekdays1(datas[i])=='sabado')|(weekdays1(datas[i])=='domingo')) entradaflag=1
                     #x0=data.frame(entradaflag,mean(as.numeric(BASEDADOS[indice-1,1:24])))
                     x0=data.frame(mean(as.numeric(BASEDADOS[indice-1,1:24])))
                     names(x0)=c("X1")
                     conf=0.90
                     limsup=predict(modelo,x0,interval="predict",level=conf)[3] #lim superior intervalo confianca
                     liminf=predict(modelo,x0,interval="predict",level=conf)[2] # lim inferior intervalo confianca
                     yhat=predict(modelo,x0,interval="predict",level=conf)[1] # prev primeira hora dia seguinte
                     testaregressao=1
                  }else{
                     dadosm=data.frame(tempreg,mediaregant7,mediaregant,mediareg)
                     if ((dim(dadosm)[1]<=dim(dadosm)[2])|(tipotemp==0)){
                        dadosm=data.frame(mediaregant,mediareg)
                        if (dim(dadosm)[1]>dim(dadosm)[2]){
                           colnames(dadosm)=c("X2","Y")
                           modelo=lm(Y~X2,data=dadosm)
                           x0=data.frame(mean(as.numeric(BASEDADOS[indice-1,1:24])))
                           names(x0)=c("X2")
                           conf=0.90
                           limsup=predict(modelo,x0,interval="predict",level=conf)[3] #lim superior intervalo confianca
                           liminf=predict(modelo,x0,interval="predict",level=conf)[2] # lim inferior intervalo confianca
                           yhat=predict(modelo,x0,interval="predict",level=conf)[1] # prev primeira hora dia seguinte
                           testaregressao=1
                        }
                     }else{
                        colnames(dadosm)=c("X1","X2","X3","Y")
                        modelo=lm(Y~X1+X2+X3,data=dadosm)
                        #x0=data.frame(max(as.numeric(BASEDADOS[indice,27:50])),mean(as.numeric(BASEDADOS[indice-7,1:24])),mean(as.numeric(BASEDADOS[indice-1,1:24])))
                        x0=data.frame(max(as.numeric(BASEDADOS[indice,37:45])),mean(as.numeric(BASEDADOS[indice-7,1:24])),mean(as.numeric(BASEDADOS[indice-1,1:24])))
                        names(x0)=c("X1","X2","X3")
                        conf=0.90
                        limsup=predict(modelo,x0,interval="predict",level=conf)[3] #lim superior intervalo confianca
                        liminf=predict(modelo,x0,interval="predict",level=conf)[2] # lim inferior intervalo confianca
                        yhat=predict(modelo,x0,interval="predict",level=conf)[1] # prev primeira hora dia seguinte
                        testaregressao=1
                     }
                  }
               }
            }

            ### APLICA A REGRESSAO
            mediaprev=mean(as.numeric(BASEDADOS[indice,1:24]))
            mediaprevold=mediaprev
            if (testaregressao>0){
               maximo=max(previsao)
               maxant=max(as.numeric(BASEDADOS[indice-1,1:24]))
               fatcarga=mediaprev/maximo
               if ((mediaprev>limsup)|(mediaprev<liminf)){
                  if (((xferiado==1)|(xferiado==2)|(xferiado==6))&((BASEDADOS[indice-1,25]==0)|(BASEDADOS[indice-1,25]==8)|(BASEDADOS[indice-1,25]==9))){
                     if (maximo>0.95*maxant){
                        mediaprev=(mediaprev+liminf)/2
                     }else{
                        #####
                     }
                  }else{
                     if (mediaprev>limsup){
                        mediaprev=limsup
                     }else{
                        mediaprev=liminf
                     }
                  }
               }else{
                  if (((xferiado==1)|(xferiado==2)|(xferiado==6))&((BASEDADOS[indice-1,25]==0)|(BASEDADOS[indice-1,25]==8)|(BASEDADOS[indice-1,25]==9))){
                     if (maximo>0.95*maxant){
                        if (mediaprev>yhat){
                           mediaprev=(mediaprev+yhat)/2
                        }else{
                           mediaprev=(mediaprev+liminf)/2
                        }
                     }else{
                        if ((((mediaprev+yhat)/2)/fatcarga)<maxant) mediaprev=(mediaprev+yhat)/2
                     }
                  }else{
                     mediaprev=(mediaprev+yhat)/2
                  }
               }
               previsaonova=(previsao/mean(previsao))*mediaprev
            }else{
               previsaonova=previsao
            }

            ###VERIFICA A RELACAO DA CARGA COM O DIA ANTERIOR
            limsup=mean(mediareg/mediaregant)+2*sd(mediareg/mediaregant)
            liminf=mean(mediareg/mediaregant)-2*sd(mediareg/mediaregant)

            previsao=previsaonova

            if ((mean(previsao)/mean(as.numeric(BASEDADOS[indice-1,1:24])))>limsup) previsaonova=(previsao/mean(previsao))*mean(as.numeric(BASEDADOS[indice-1,1:24]))*mean(mediareg/mediaregant)
            if ((mean(previsao)/mean(as.numeric(BASEDADOS[indice-1,1:24])))<liminf) previsaonova=(previsao/mean(previsao))*mean(as.numeric(BASEDADOS[indice-1,1:24]))*mean(mediareg/mediaregant)


            #if ((mean(previsao)/mean(as.numeric(BASEDADOS[indice-1,1:24])))>limsup) previsaonova=(previsao/mean(previsao))*mean(as.numeric(BASEDADOS[indice-1,1:24]))*limsup
            #if ((mean(previsao)/mean(as.numeric(BASEDADOS[indice-1,1:24])))<liminf) previsaonova=(previsao/mean(previsao))*mean(as.numeric(BASEDADOS[indice-1,1:24]))*liminf
         }else{
            # ENTRA AQUI SE NAO HA PADROES SEMELHANTES E NAO APLICA HEURISTICA
            previsaonova=previsao
         }

      }else{
         # ENTRA AQUI SE DIA NORMAL POIS NAO APLICA HEURISTICA
         previsaonova=previsao
      }

      #######################################################################################################
      # REALIZA PROGRAMACAO QUADRATICA PARA AJUSTAR A PREVISAO NO CASO DE DEGRAU ENTRE AS PREVISOES DIARIAS #
      #######################################################################################################
      ultimodado=BASEDADOS[indice-1,24]
      if (ultimodado<previsaonova[1]) {
         message("ajuste por programacao quadratica")
         previsaonova=pquadratica(ultimodado,previsaonova)
      }

      #############################
      # ARMAZENA A PREVISAO FINAL #
      #############################
      previsaonova=as.numeric(previsaonova)
      BASEDADOS[indice,1:24]=previsaonova
      seriediaria1[indice]=mean(previsaonova)
      previsaoperfildiario=rbind(previsaoperfildiario,previsaonova/mean(previsaonova))
      previsaoperfildiarioMW=rbind(previsaoperfildiarioMW,previsaonova)
      previsao=previsaonova
   }

   rownames(previsaoperfildiarioMW)=as.character(seq(as.Date(rownames(previsaoperfildiarioMW)[1]),as.Date(tail(datas,1)),1))

   #====================================
   # PREVISAO DA GERACAO NAO DESPACHADA
   #====================================
   # LEITURA DE DADOS E MONTAGEM DE SERIES DE USINAS NAO SIMULADAS
   if (naosimulada==1) {
      message("PREVISAO DA GERACAO NAO DESPACHADA")
      NSIMULA=prevunsim(serie_nao_simulada,datas)
   } else {
      NSIMULA=0
   }

   #================================
   # Historico e Previsoes horarias
   #================================
   serie_carga_global=c()
   nomeshoras=c()
   for (i in 1:length(datas)) {
      indice=grep(datas[i],rownames(previsaoperfildiarioMW))
      serie_carga_global=c(serie_carga_global,as.numeric(previsaoperfildiarioMW[indice,]))
      nomeshoras=c(nomeshoras,rep(as.character(datas[i]),24))
   }
   names(serie_carga_global)=nomeshoras
   if (naosimulada==1) {
      serie_nao_simulada=NSIMULA
   }

   #=================================================
   # CORRIGE PREVISOES COM DADOS PARA O DIA CORRENTE
   #=================================================
   if (ioutsample==1) {
      if (naosimulada==0) {
         serie_carga_global_corrigida=c()
         for (i in 1:length(datas)) {
            serie_carga_global_corrigida=c(serie_carga_global_corrigida,rep(0,24))
         }
         # preeenche serie com dados verificados carga (equivale ao cargahist + intercambio)
         if (is.matrix(DADOS$MATRIZ_PERFIS_CARGA_OUTSAMPLE)==F) {
            serie_carga_global_corrigida[1:length(as.numeric(DADOS$MATRIZ_PERFIS_CARGA_OUTSAMPLE))]=as.numeric(DADOS$MATRIZ_PERFIS_CARGA_OUTSAMPLE)
         } else {
            fim=0
            for (i in 1:dim(DADOS$MATRIZ_PERFIS_CARGA_OUTSAMPLE)[1]) {
               serie_carga_global_corrigida[(fim+1):(fim+24)]=as.numeric(DADOS$MATRIZ_PERFIS_CARGA_OUTSAMPLE[i,])
               fim=fim+24
            }
         }

         # ajuta previsao da carga global
         indice0=which(serie_carga_global_corrigida==0)
         if (length(indice0)>0) {
            for (i in 1:length(indice0)){
               taxa=serie_carga_global[indice0[i]]/serie_carga_global[indice0[i]-1]
               serie_carga_global_corrigida[indice0[i]]=taxa*serie_carga_global_corrigida[indice0[i]-1]
            }
         }
      } else {
         # preeenche serie com dados verificados carga (equivale ao cargahist + intercambio)
         serie_carga_global_corrigida=c()
         for (i in 1:length(datas)) {
            serie_carga_global_corrigida=c(serie_carga_global_corrigida,rep(0,24))
         }
         # preeenche serie com dados verificados carga (equivale ao cargahist + intercambio)
         if (is.matrix(DADOS$MATRIZ_PERFIS_CARGA_OUTSAMPLE)==F) {
            serie_carga_global_corrigida[1:length(as.numeric(DADOS$MATRIZ_PERFIS_CARGA_OUTSAMPLE))]=as.numeric(DADOS$MATRIZ_PERFIS_CARGA_OUTSAMPLE)
         } else {
            fim=0
            for (i in 1:dim(DADOS$MATRIZ_PERFIS_CARGA_OUTSAMPLE)[1]) {
               serie_carga_global_corrigida[(fim+1):(fim+24)]=as.numeric(DADOS$MATRIZ_PERFIS_CARGA_OUTSAMPLE[i,])
               fim=fim+24
            }
         }

         # soma previsao da geracao nao supervisionada aos dados verificados da carga
         indicenao0=which(serie_carga_global_corrigida>0) # valores nao nulos no vetor de valores verificados da carga
         if (length(indicenao0)>0) {
            for (i in 1:length(indicenao0)){
               serie_carga_global_corrigida[indicenao0[i]]=serie_carga_global_corrigida[indicenao0[i]]+serie_nao_simulada[indicenao0[i]]
            }
         }

         # ajuta previsao da carga global
         indice0=which(serie_carga_global_corrigida==0) # valores nulos no vetor de valores verificados da carga
         if (length(indice0)>0) {
            for (i in 1:length(indice0)){
               taxa=serie_carga_global[indice0[i]]/serie_carga_global[indice0[i]-1]
               serie_carga_global_corrigida[indice0[i]]=taxa*serie_carga_global_corrigida[indice0[i]-1]
            }
         }
      }
      serie_carga_global=serie_carga_global_corrigida
   }


   #=======================
   # IMPRIME CARGA HORARIA
   #=======================
   separador=DADOS$separador
   tags=c()
   for (i in 1:length(datas)){
      tags=c(tags,as.character(seq(ISOdate(substr(datas[i],1,4),substr(datas[i],6,7),substr(datas[i],9,10),0,1,0),by="min",length.out=1441)[seq(60,1440,60)]))
   }
   names(serie_carga_global)=tags
   arquivosaida=paste(caminhosaida,"/","PREVCARGADESSEM_CARGA_GLOBAL_HORARIA.csv",sep="")
   if (separador==0) {
      write.table(serie_carga_global,arquivosaida,sep=";",dec=".",col.names = NA,row.names = TRUE)
   } else {
      write.table(serie_carga_global,arquivosaida,sep=";",dec=",",col.names = NA,row.names = TRUE)
   }


   #==============================
   # Desagrega serie em meia hora
   #==============================
   message("CALCULA PREVISAO SEMI HORARIA")
   previsao_carga_global_horaria=serie_carga_global
   previsao_nao_simulada_horaria=serie_nao_simulada
   serie_carga_global_meia_hora=desagrega30minutos(serie_carga_global,datas,ultimo_carga_horaria_verificada)
   if (naosimulada==1) serie_nao_simulada_meia_hora=desagrega30minutos(serie_nao_simulada,datas,ultimo_carga_horaria_verificada_gera_nao_simulada)

   #====================================
   # Agrega serie em patamares de carga
   #====================================
   message("AGREGA PREVISOES EM PATAMARES")
   previsao_carga_global=agregapatamar(serie_carga_global_meia_hora,datas,DADOS,BASEDADOS,diasprevisaosemihora)
   if (naosimulada==1) previsao_nao_simulada=agregapatamar(serie_nao_simulada_meia_hora,datas,DADOS,BASEDADOS,diasprevisaosemihora)

   #====================
   # RELATORIO DE SAIDA
   #====================
   message("GRAVANDO ARQUIVOS DE SAIDA")
   separador=DADOS$separador
   arquivosaida=paste(caminhosaida,"/","PREVCARGADESSEM_CARGA_GLOBAL.csv",sep="")
   arquivosaida1=paste(caminhosaida,"/","PREVCARGADESSEM_GERACAO_NAO_SUPERVISIONADA.csv",sep="")
   if (separador==0) {
      write.table(previsao_carga_global,arquivosaida,sep=";",dec=".",col.names = NA,row.names = TRUE)
      if (naosimulada==1) write.table(previsao_nao_simulada,arquivosaida1,sep=";",dec=".",col.names = NA,row.names = TRUE)
   } else {
      write.table(previsao_carga_global,arquivosaida,sep=";",dec=",",col.names = NA,row.names = TRUE)
      if (naosimulada==1) write.table(previsao_nao_simulada,arquivosaida1,sep=";",dec=",",col.names = NA,row.names = TRUE)
   }

   # grava arquivos com previsoes horarias
   arquivosaida=paste(caminhosaida,"/","PREVCARGADESSEM_CARGA_GLOBAL_HORARIA.csv",sep="")
   arquivosaida1=paste(caminhosaida,"/","PREVCARGADESSEM_GERACAO_NAO_SUPERVISIONADA_HORARIA.csv",sep="")
   if (separador==0) {
      write.table(previsao_carga_global_horaria,arquivosaida,sep=";",dec=".",col.names = NA,row.names = TRUE)
      if (naosimulada==1) write.table(previsao_nao_simulada_horaria,arquivosaida1,sep=";",dec=".",col.names = NA,row.names = TRUE)
   } else {
      write.table(previsao_carga_global_horaria,arquivosaida,sep=";",dec=",",col.names = NA,row.names = TRUE)
      if (naosimulada==1) write.table(previsao_nao_simulada_horaria,arquivosaida1,sep=";",dec=",",col.names = NA,row.names = TRUE)
   }


   #=================================
   # Grava arquivo no formato dessem
   #=================================
   message("GRAVA ARQUIVO COM PREVISOES NO FORMATO DESSEM")
   gravadessem(previsao_carga_global,caminhosaida,subsistema)
   message("EXECUCAO CONCLUIDA COM SUCESSO")

   matriz=previsao_carga_global[,1]
   names(matriz)=rownames(previsao_carga_global)
   matriz=cbind(matriz)
   colnames(matriz)="MW"

   if (naosimulada==1) {
      return(list(CARGA_GLOBAL=previsao_carga_global,GERACAO_NAO_DESPACHADA=previsao_nao_simulada,SAIDADESSEM=matriz,DADOS=BASEDADOS,PREVHORARIA=previsao_carga_global_horaria))
   } else {
      #return(list(CARGA_GLOBAL=previsao_carga_global,GERACAO_NAO_DESPACHADA=0,SAIDADESSEM=matriz,SAIDADESSEM=matriz,DADOS=BASEDADOS,PREVHORARIA=previsao_carga_global_horaria))
      return(list(CARGA_GLOBAL=previsao_carga_global,SAIDADESSEM=matriz,PREVHORARIA=previsao_carga_global_horaria))
   }
   # FIM
}


#' @title PREVISORDESSEM
#' @name PREVISORDESSEM
#'
#' @description Used to call the module PREVISORDESSEM that computes the short-term load forecasts up to 192 hours ahead
#'
#' @param previsordessem(caminho,caminhosaida,tipoinput,tipotemp,tipotempdia,horizonteprevisao,diasprevisaosemihora,tipokernel,tipocombinada,prefixo,alfa)
#'
#' @details
#' \itemize{
#'  \item caminho is the directory name with data files
#'  \item caminhosaida is the directory name with results files
#'  \item tipoinput two options: 1 for data files in data.frame format, 0 for data files in csv format (default=1)
#'  \item tipotemp 3 options: 0 to not consider temperature, 1 to consider maximum daily temperature, 2 average daily temperature, 3 maximum and minimum daily temperature (default=1). This option only affects the average daily demand forecasting model.
#'  \item tipotempdia a vector with eigth logical variables, a variable for each day over a horizon with 8 days, in each day set 1 to consider temperature and 0 to non consider temperature (dafault=c(1,1,1,1,1,1,1,1)). This option only affects the average daily demand forecasting model.
#'  \item horizonteprevisao three options: 0 to forecasts up to the end of operative week, 1 to forecasts up to 8 days ahead, 2 to forecasts up to final date indicated in the file HORIZONTE.csv (default=2)
#'  \item diasprevisaosemihora number of first days with half hourly load forecasts, the following forecasts are expressed in load levels (default=3)
#'  \item tipokernel three options: 0 to SVM with RBF kernel, 1 to SVM with linear kernel, >=2 to MLP Neural Network (default=0). This option only affects the average daily demand forecasting model.
#'  \item tipocombinada two options: 0 to run a selected forecast model , 1 to run all forecasts models and combines the results (default=0)
#'  \item prefixo: name of area and date (YYYY-MM-DD), for example "SECO_2020-01-19"
#'  \item alfa: smoothing constant: a number in the interval [0,0.85]
#' }
#'
#' @return
#' \itemize{
#'  \item CARGA_GLOBAL data.frame with half-hourly load forecasts (also in CSV format, the output file PREVCARGADESSEM_CARGA_GLOBAL.csv)
#'  \item GERACAO_NAO_DESPACHADA data.frame with hourly unsupervised generation forecasts (also in CSV format, the output file PREVCARGADESSEM_GERACAO_NAO_SUPERVISIONADA_HORARIA.csv)
#'  \item SAIDADESSEM data.frame with load forecasts in DESSEM format (also in TXT format, SAIDADESSEM.TXT)
#'  \item DADOS data.frame with hourly profiles of load and temperature (data and forecasts)
#'  \item PREVHORARIA data.frame with hourly load forecasts (also in CSV format, output file PREVCARGADESSEM_CARGA_GLOBAL_HORARIA.csv)
#' }
#'
#' @author Brazilian Electric Power Research Center - CEPEL
#'
#'
#'
#' @export
#previsordessem<-function(session,caminho,caminhosaida,treinamodelo,tipoinput,tipotemp,tipotempdia,naosimulada,intercambio,horizonteprevisao,diasprevisaosemihora,tipokernel,tipocombinada,prevnaivefuzzy,prefixo){
previsordessem<-function(caminho,caminhosaida,tipoinput,tipotemp,tipotempdia,horizonteprevisao,diasprevisaosemihora,tipokernel,tipocombinada,prefixo,alfa){

   session=NULL
   #VERIFICA OS PARAMETROS DE ENTRADA
   #session
   #if (missing(session)){
   #	session=NULL
   #	#message('session default - . ')
   #}else{
   #	teste=try(length(session), silent = TRUE)
   #	if (class(teste)=="try-error"){
   #	session=NULL
   #	#message('caminhosaida default = .')
   #	}
   #}

   sessionUI <<- session #compatibiliza versao para rshiny


   # VERSAO DO PROGRAMA



   treinamodelo=1
   # if (missing(treinamodelo)){
   # treinamodelo=1
   # message('treinamodelo default = 1')
   # }else{
   # teste=try(if (get("treinamodelo")) { }, silent = TRUE)
   # if (class(teste)=="try-error"){
   # treinamodelo=1
   # message('treinamodelo default = 1')
   # }
   # }

   #tipoinput
   if (missing(tipoinput)){
      tipoinput=1
      message('tipoinput default = 1')
   }else{
      teste=try(if (get("tipoinput")) { }, silent = TRUE)
      if (class(teste)=="try-error"){
         tipoinput=1
         message('tipoinput default = 1')
      }
   }
   #tipotemp
   if (missing(tipotemp)){
      tipotemp=1
      message('tipotemp default = 1')
   }else{
      teste=try(if (get("tipotemp")) { }, silent = TRUE)
      if (class(teste)=="try-error"){
         tipotemp=1
         message('tipotemp default = 1')
      }
   }
   #tipotempdia
   if (missing(tipotempdia)){
      tipotempdia=c(1,1,1,1,1,1,1,1)
      message('tipotempdia default = (1,1,1,1,1,1,1,1)')
   }else{
      teste=try(length(tipotempdia), silent = TRUE)
      if (class(teste)=="try-error"){
         tipotempdia=c(1,1,1,1,1,1,1,1)
         message('tipotempdia default = (1,1,1,1,1,1,1,1)')
      }
   }

   naosimulada=0
   # #if (missing(naosimulada)){
   # #	naosimulada=0
   # #	message('naosimulada default = 0')
   # #}else{
   # teste=try(if (get("naosimulada")) { }, silent = TRUE)
   # if (class(teste)=="try-error"){
   # naosimulada=0
   # message('naosimulada default = 0')
   # }
   # }

   intercambio=0
   # if (missing(intercambio)){
   # intercambio=0
   # message('intercambio default = 0')
   # }else{
   # teste=try(if (get("intercambio")) { }, silent = TRUE)
   # if (class(teste)=="try-error"){
   # intercambio=0
   # message('intercambio default = 0')
   # }
   # }

   #horizonteprevisao
   if (missing(horizonteprevisao)){
      horizonteprevisao=2
      message('horizonteprevisao default = 2')
   }else{
      teste=try(if (get("horizonteprevisao")) { }, silent = TRUE)
      if (class(teste)=="try-error"){
         horizonteprevisao=2
         message('horizonteprevisao default = 2')
      }
   }
   #diasprevisaosemihora
   if (missing(diasprevisaosemihora)){
      diasprevisaosemihora=3
      message('diasprevisaosemihora default = 3')
   }else{
      teste=try(if (get("diasprevisaosemihora")) { }, silent = TRUE)
      if (class(teste)=="try-error"){
         diasprevisaosemihora=3
         message('diasprevisaosemihora default = 3')
      }
   }
   #tipokernel
   if (missing(tipokernel)){
      tipokernel=0
      message('tipokernel default = 0')
   }else{
      teste=try(if (get("tipokernel")) { }, silent = TRUE)
      if (class(teste)=="try-error"){
         tipokernel=0
         message('tipokernel default = 0')
      }
   }

   #tipocombinada
   if (missing(tipocombinada)){
      tipocombinada=0
      message('tipocombinada default = 0')
   }else{
      teste=try(if (get("tipocombinada")) { }, silent = TRUE)
      if (class(teste)=="try-error"){
         tipocombinada=0
         message('tipocombinada default = 0')
      }
   }

   #prefixo
   if (missing(prefixo)){
      prefixo='inexistente'
      #message('session default - . ')
   }else{
      teste=try(length(prefixo), silent = TRUE)
      if (class(teste)=="try-error"){
         prefixo='inexistente'
         #message('caminhosaida default = .')
      }else{
         prefixo=paste0(prefixo,"_")
      }

   }

   #TESTA SE EXISTE ARQUIVO "PARAMETROS.csv". SE NAO HOUVER, GERA O ARQUIVO UTILIZANDO O PREFIXO INFORMADO NA CHAMADA DA FUNCAO
   if ((prefixo!="inexistente")&(tipoinput==0)){
      tabelaparametros=cbind("SUBSISTEMA","CARGAHIST","TEMPHIST","FERIADOS","HORIZONTE","TEMPPREV","HORAVERAO",",","NSIMHISTHORARIA","NSIMHISTDIARIA","INTERCAMBIO","PATAMARES","COMBINA")
      tabelaparametros=rbind(tabelaparametros,cbind(prefixo,paste(prefixo,"CARGAHIST",sep=""),paste(prefixo,"TEMPHIST",sep=""),paste(prefixo,"FERIADOS",sep=""),paste(prefixo,"HORIZONTE",sep=""),paste(prefixo,"TEMPPREV",sep=""),paste(prefixo,"HORAVERAO",sep=""),",",paste(prefixo,"NSUPHISTHORARIA",sep=""),paste(prefixo,"NSUPHISTDIARIA",sep=""),paste(prefixo,"INTERCAMBIO",sep=""),paste(prefixo,"PATAMARES",sep=""),paste(prefixo,"COMBINA",sep="")))
      write.table(tabelaparametros,file.path(caminho,"PARAMETROS.csv"),sep=";",col.names=FALSE,row.names=FALSE)
   }


   if (tipocombinada==0){
      if (missing(alfa)){
         alfa=0
         message('alfa default = 0')
      }else{
         teste=try(if (get("alfa")) { }, silent = TRUE)
         if (class(teste)=="try-error"){
            alfa=0
            message('alfa default = 0')
         } else {
            message(paste0('alfa  = ',alfa))
         }
      }

      # ##### verifica limites alfa
      # if (alfa<0) alfa=0
      # if (alfa>0.85) alfa=0.85
      alfa=0
      nomerdata=paste0(prefixo,'_unica')
      previsordessem1(caminho,caminhosaida,treinamodelo,tipoinput,tipotemp,tipotempdia,naosimulada,intercambio,horizonteprevisao,diasprevisaosemihora,tipokernel,0,alfa,nomerdata)
   }else{
      if (missing(alfa)){
         alfa=0
         message('alfa default no tratamento de dados = 0')
      }else{
         teste=try(if (get("alfa")) { }, silent = TRUE)
         if (class(teste)=="try-error"){
            alfa=0
            message('alfa default no tratamento de dados = 0')
         }
      }
      alfa=0
      combinada(caminho,caminhosaida,treinamodelo,tipoinput,3,tipotempdia,naosimulada,intercambio,horizonteprevisao,diasprevisaosemihora,tipokernel,1,alfa,prefixo)
   }
}



#10 @title COMBINADA
#10 @name COMBINADA
#10
#10 @description Used to apply the several load forecasts models and compute the combined prediction
#10
#10 @Usage combinada(caminho,caminhosaida,treinamodelo,tipoinput,tipotemp,tipotempdia,naosimulada,intercambio,horizonteprevisao,diasprevisaosemihora,tipokernel,tipocombinada,alfa)
#10
#10 @details
#10 \itemize{
#10  \item caminho is the directory name with data files
#10  \item caminhosaida is the directory name with results files
#10  \item treinamodelo two options: 1 for training models, 0 for read previously trained model parameters (default=1)
#10  \item tipoinput two options: 1 for data files in data.frame format, 0 for data files in csv format (default=1)
#10  \item tipotemp 3 options: 0 to not consider temperature, 1 to consider maximum daily temperature, 2 average daily temperature, 3 maximum and minimum daily temperature (default=1). This option only affects the average daily demand forecasting model.
#10  \item tipotempdia a vector with eigth logical variables, a variable for each day over a horizon with 8 days, in each day set 1 to consider temperature and 0 to non consider temperature (dafault=c(1,1,1,1,1,1,1,1)). This option only affects the average daily demand forecasting model.
#10  \item naosimulada two options: 1 to consider unsupervised generation, 0 otherwise (default=0)
#10  \item intercambio two options: 1 to consider exchange, 0 otherwise (default=0)
#10  \item horizonteprevisao three options: 0 to forecasts up to the end of operative week, 1 to forecasts up to 8 days ahead, 2 to forecasts up to final date indicated in the file HORIZONTE.csv (default=2)
#10  \item diasprevisaosemihora number of first days with half hourly load forecasts, the following forecasts are expressed in load levels (default=3)
#10  \item tipokernel three options: 0 to SVM with RBF kernel, 1 to SVM with linear kernel, >=2 to MLP Neural Network (default=0). This option only affects the average daily demand forecasting model.
#10  \item tipocombinada two options: 0 to run a selected forecast model , 1 to run all forcasts models and combines the results (default=0)
#10  \item alfa smoothing constant: a number in the interval [0,0.85]
#10 }
#10
#10
#10 @return
#10 \itemize{
#10  \item CARGA_GLOBAL data.frame with half-hourly load forecasts (also in CSV format, the output file PREVCARGADESSEM_CARGA_GLOBAL.csv)
#10  \item SAIDADESSEM data.frame with load forecasts in DESSEM format (also in TXT format, SAIDADESSEM.TXT)
#10 }
#10
#10 @author Brazilian Electric Power Research Center - CEPEL
#10
#10
#10
#10
#10 @export
combinada<-function(caminho,caminhosaida,treinamodelo,tipoinput,tipotemp,tipotempdia,naosimulada,intercambio,horizonteprevisao,diasprevisaosemihora,tipokernel,tipocombinada,alfa,prefixo){

   library(WeightSVM)
   library(e1071,warn.conflicts = FALSE)

   # tipotemp  = 0 sem temperatura
   #           = 1 temperatura maxima do dia
   #           = 2 temperatura media do dia
   #			= 3 temperaturas maxima e minima do dia

   # tipokernel = 0 kernel radial
   #            = 1 kernel linear
   #			 = 2 utiliza redes neurais

   resultados=list()
   previsao=rep(0,48)
   subsistema=strsplit(prefixo,"_")[[1]][1]

   #===========================
   # CHAMADA DA ROTINA LEITORA
   #===========================

   if (tipoinput==0) {
      if (dir.exists(caminho)) {
         arquivo="PARAMETROS.csv" # arquivo com parametros de execucao
         DADOS=LEITORA(caminho,arquivo,naosimulada,intercambio,tipotemp,horizonteprevisao) # leitura arquivo de dados em formato csv
      } else {
         stop("Nao foi encontrado o diretorio com os dados para o previsor")
      }
   } else {
      dataframes=caminho
      if (exists("dataframes")) {
         DADOS=LEITORADATAFRAME(dataframes,naosimulada,intercambio,tipotemp,horizonteprevisao) # leitura de dados no formato data.frame
      } else {
         stop("Nao foi encontrado o data.frame com os dados para o previsor")
      }
   }

   # SAIDALEITORA=list(DADOS)
   # names(SAIDALEITORA)='DADOS'

   datasprev=rownames(DADOS$DATAPREV) # DATAS DOS DIAS DO PERIODO PREVISAO
   numdiasprev=length(datasprev) # numero de dias de previsao
   separador=DADOS$separador
   ioutsample=DADOS$ioutsample
   numeroclusters=40


   ####LEITURA DO ARQUIVO OU DATAFRAME CONTENDO OS PESOS DA COMBINACAO
   if (tipoinput==0) peso=DADOS$COMBINA
   if (tipoinput==1) peso=caminho$COMBINA

   #=============================
   # MONTA SERIE DE CARGA GLOBAL
   #=============================
   # o intercambio ja foi agregado na leitura dos dados (rotinas LEITORA e LEITORADATAFRAME)
   message("MONTAGEM DA SERIE DE CARGA GLOBAL")
   if (naosimulada==1){
      serie_carga=DADOS$SERIEHIST[,1]
      datas_carga=rownames(DADOS$DATAHIST)
      serie_nao_simulada=rep(0,length(serie_carga))
      for (i in 1:length(datas_carga)) {
         indice1=grep(datas_carga[i],rownames(DADOS$NSHORA$HORA))
         if (length(indice1)>0) {
            # valores horarios da geracao nao simulada
            serie_nao_simulada[((i-1)*24+1):((i-1)*24+24)]=as.numeric(DADOS$NSHORA$HORA[indice1,])
         } else {
            indice2=grep(datas_carga[i],names(DADOS$NSDIA))
            if (length(indice2)>0) {
               # valores diarios convertidos para horarios da geracao nao simulada
               # pega perfil da semana anterior
               if (mean(serie_nao_simulada[((i-8)*24+1):((i-8)*24+24)])>0) {
                  pu=serie_nao_simulada[((i-8)*24+1):((i-8)*24+24)]/mean(serie_nao_simulada[((i-8)*24+1):((i-8)*24+24)]) # perfil da semana anterior
               } else {
                  pu=1
               }
               if (as.numeric(DADOS$NSDIA[indice2])>0) {
                  serie_nao_simulada[((i-1)*24+1):((i-1)*24+24)]=as.numeric(DADOS$NSDIA[indice2])*pu
               } else {
                  serie_nao_simulada[((i-1)*24+1):((i-1)*24+24)]=serie_nao_simulada[((i-8)*24+1):((i-8)*24+24)]
               }
            } else {
               # para periodo no final da serie sem dados de geracao nao simulada, usa dados da semana anterior
               serie_nao_simulada[((i-1)*24+1):((i-1)*24+24)]=serie_nao_simulada[((i-8)*24+1):((i-8)*24+24)]
            }
         }
      }
   } else {
      serie_carga=DADOS$SERIEHIST[,1]
      serie_nao_simulada=rep(0,length(serie_carga))
   }

   # CHAMADA DA ROTINA DE FILTRAGEM DE DADOS HISTORICOS DA CARGA DESPACHADA
   message("TRATAMENTO DE DADOS")
   # numeroclusters=40
   perfis=DADOS$DATAHIST
   DADOS_FILTRADOS=FILTRAGEM(numeroclusters,perfis,serie_carga,DADOS,treinamodelo,tipotemp,alfa) # FILTRA CARGA DESPACHADA
   serie_carga_global=DADOS_FILTRADOS$SERIEHIST_FILTRADAS$seriecargafiltrada
   if (naosimulada==1) {
      serie_nao_simulada=FILTRANAOSIMULADA(serie_nao_simulada) # FILTRA GERACAO NAO SIMULADA
      serie_carga_global=serie_carga_global+serie_nao_simulada # agrega geracao nao simulada para formar a carga global filtrada
      # SUBSTITUI A CARGA DESPACHADA PELA CARGA GLOBAL NO OBJETO DADOS_FILTRADOS
      aux=data.frame(matrix(serie_carga_global,nrow=dim(DADOS_FILTRADOS$DATAHIST_FILTRADOS)[1],ncol=24,byrow=T))
      aux1=rownames(DADOS_FILTRADOS$DATAHIST_FILTRADOS)
      DADOS_FILTRADOS$DATAHIST_FILTRADOS[,1:24]=aux
      rownames(DADOS_FILTRADOS$DATAHIST_FILTRADOS)=aux1
      DADOS_FILTRADOS$SERIEHIST_FILTRADAS$seriecargafiltrada=serie_carga_global
   }

   # SAIDAFILTRAGEM=list(DADOS_FILTRADOS)
   # names(SAIDAFILTRAGEM)='DADOSFILTRADOS'

   SAIDALEITORA=DADOS
   SAIDAFILTRAGEM=DADOS_FILTRADOS

   #SALVA UM ARQUIVO R.DATA COM OS DADOS E OS DADOS FILTRADOS
   #if (tipoinput==0) nomerdata=as.character(basename(caminho))
   if (tipoinput==0) nomerdata=prefixo
   if (tipoinput==1) nomerdata=paste(caminho$HORIZONTE[1,2],caminho$HORIZONTE[1,3],caminho$HORIZONTE[1,4],sep="-")

   auxnome=nomerdata
   n=1
   while (file.exists(file.path(caminhosaida,paste0("DADOS_FILTRADOS_",nomerdata,".RData")))){
      nomerdata=paste0(auxnome,"_",n)
      n=n+1
   }

   save(SAIDALEITORA,SAIDAFILTRAGEM, file = file.path(caminhosaida,paste0("DADOS_FILTRADOS_",nomerdata,".RData")))

   ####RODA CONFIGURACOES COM PESO MAIOR QUE ZERO

   npassos=peso[dim(peso)[1],4]
   modelos=unique(peso[,3])
   modelos=modelos[-1]
   listatk=c()
   for (i in 1:length(modelos)){
      listatk=rbind(listatk,peso[which(peso[,3]==modelos[i])[1],c(1:3)])
   }


   n=1
   alfa=0

   for (i in 1:(dim(listatk)[1])){
      tipotemp=listatk[i,1]
      tipokernel=listatk[i,2]
      #alfa=peso[i,5]
      # if (peso[i,5]>0.85) alfa=0.85
      # if (peso[i,5]<0) alfa=0
      # message(paste0('alfa = ',alfa))

      resultados=c(resultados,list(previsordessem1(caminho,caminhosaida,treinamodelo,tipoinput,tipotemp,tipotempdia,naosimulada,intercambio,horizonteprevisao,diasprevisaosemihora,tipokernel,2,alfa,nomerdata)))
      message(paste0(listatk[i,3]," - Concluido"))
      n=n+1
   }

   names(resultados)=listatk[,3]
   save(resultados,resultados, file = file.path(caminhosaida,paste0("result_",nomerdata,".RData")))

   serieprev=c()
   hprev=dim(resultados[[1]]$CARGA_GLOBAL)[1]
   if (npassos>hprev) npassos=hprev

   for (i in 1:npassos){
      indmodelosprev=which(peso[,4]==i)
      indmodelosprev=indmodelosprev[-1]
      previsao=0
      for (ii in 1:length(indmodelosprev)){
         indresult=which(names(resultados)==peso[indmodelosprev[ii],3])
         previsao=previsao+resultados[[indresult]]$CARGA_GLOBAL[i,2]*peso[indmodelosprev[ii],5]

         intercept=peso[which((peso[,3]=='(Intercept)')&(peso[,4]==i)),5]
         previsao=previsao+intercept


      }
      serieprev=c(serieprev,previsao)
   }

   previsao=serieprev
   names(previsao)=rownames(resultados[[1]]$CARGA_GLOBAL)



   datasprev=rownames(DADOS$DATAPREV) # DATAS DOS DIAS DO PERIODO PREVISAO
   numdiasprev=length(datasprev) # numero de dias de previsao
   ioutsample=DADOS$ioutsample

   #==========================
   # PREVISOES DE CARGA GLOBAL
   #==========================
   datainicial=datasprev[1] # primeiro dia do horizonte de previsao
   datafinal=tail(datasprev,1)
   datas=seq(as.Date(datainicial),as.Date(datafinal),1)
   conjuntoHIST=DADOS_FILTRADOS$DATAHIST_FILTRADOS
   colnames(conjuntoHIST)=paste("X",seq(1,dim(conjuntoHIST)[2],1),sep="")
   conjuntoPREV=DADOS$DATAPREV
   aux=cbind(matrix(0,dim(conjuntoPREV)[1],24),conjuntoPREV[,1:2],conjuntoPREV[,3:26])
   colnames(aux)=paste("X",seq(1,dim(conjuntoHIST)[2],1),sep="")
   BASEDADOS=rbind(conjuntoHIST,aux) # base com todos os dados dos periodos historico e de previsao
   rm(aux)
   rm(conjuntoHIST)
   rm(conjuntoPREV)


   indice=which(substr(names(previsao),15,16)=='00')
   previsaohoraria=previsao[indice]


   #=======================
   # IMPRIME CARGA HORARIA
   #=======================

   arquivosaida=paste(caminhosaida,"/","PREVCARGADESSEM_CARGA_GLOBAL_HORARIA.csv",sep="")
   if (separador==0) {
      write.table(previsaohoraria,arquivosaida,sep=";",dec=".",col.names = NA,row.names = TRUE)
   } else {
      write.table(previsaohoraria,arquivosaida,sep=";",dec=",",col.names = NA,row.names = TRUE)
   }


   #====================================
   # Agrega serie em patamares de carga
   #====================================
   message("AGREGA PREVISAO COMBINADA EM PATAMARES")
   previsao_carga_global=agregapatamar(previsao,datas,DADOS,BASEDADOS,diasprevisaosemihora)
   if (naosimulada==1) previsao_nao_simulada=agregapatamar(serie_nao_simulada_meia_hora,datas,DADOS,BASEDADOS,diasprevisaosemihora)



   #====================
   # RELATORIO DE SAIDA
   #====================
   message("GRAVANDO ARQUIVOS DE SAIDA")
   arquivosaida=paste(caminhosaida,"/","PREVCARGADESSEM_CARGA_GLOBAL.csv",sep="")
   arquivosaida1=paste(caminhosaida,"/","PREVCARGADESSEM_GERACAO_NAO_SUPERVISIONADA.csv",sep="")
   if (separador==0) {
      write.table(previsao_carga_global,arquivosaida,sep=";",dec=".",col.names = NA,row.names = TRUE)
      if (naosimulada==1) write.table(previsao_nao_simulada,arquivosaida1,sep=";",dec=".",col.names = NA,row.names = TRUE)
   } else {
      write.table(previsao_carga_global,arquivosaida,sep=";",dec=",",col.names = NA,row.names = TRUE)
      if (naosimulada==1) write.table(previsao_nao_simulada,arquivosaida1,sep=";",dec=",",col.names = NA,row.names = TRUE)
   }


   #=================================
   # Grava arquivo no formato dessem
   #=================================
   message("GRAVA ARQUIVO COM PREVISAO COMBINADA NO FORMATO DESSEM")
   gravadessem(previsao_carga_global,caminhosaida,subsistema)
   message("EXECUCAO REALIZADA COM SUCESSO")

   matriz=previsao_carga_global[,1]
   names(matriz)=rownames(previsao_carga_global)
   matriz=cbind(matriz)
   colnames(matriz)="MW"


   setwd(caminhosaida)
   file.remove(file.path(caminhosaida,paste0("DADOS_FILTRADOS_",nomerdata,".RData")))


   message('PREVISAO COMBINADA CONCLUIDA COM SUCESSO')

   return(list(CARGA_GLOBAL=previsao_carga_global,SAIDADESSEM=matriz))
}
