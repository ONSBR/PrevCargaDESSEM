FILTRAGEM=function(numeroclusters,perfis,seriecarga,DADOS,treinamodelo,tipotemp,alfawsvm){

   # PARAMETROS DE ENTRADA: numeroclusters: numero de clusters da analise de agrupamentos
   #                        perfis: matriz contendo dados de carga, temperatura e indicacao de feriado/
   #                                horario de verao em cada linha (carga e temperatura em bases horarias)
   #                        seriecarga: serie temporal da carga em bases horarias
   #                        TIPOTEMP=0 NAO FILTRA ARTQUIVO DE TEMPERATURA
   # VARIAVEIS DE SAIDA: DATAHIST_FILTRADOS: data.frame contendo dados para cada dia do historico
   #                                         cada linha representa um dia #                     colunas de 1 a 24 guardam a demanda media horaria
   #                                         coluna 25 indicacao de feriado
   #                                         coluna 26 indicacao de horario de verao
   #                                         colunas 27 a 50 guardam as temperaturas medias horarias
   #                     SERIEHIST_FILTRADAS: data.frame contendo dados historicos de carga, temperatura, feriados
   #                                          e horario de verao na forma de serie temporais
   #                                          coluna 1 serie temporal da carga filtrada
   #                                          coluna 2 serie temporal com indicacao de feriados
   #                                          coluna 3 serie temporal com indicacao de horario de verao
   #                                          coluna 4 serie temporal da temperatura media horaria filtrada
   #                     TABELAS_NAIVE_BAYES: tabelas do classificador naive bayes
   #                     TIPOLOGIAS: perfis tipicos em pu da media diaria
   perfis = perfis[,2:51]
   carga=perfis[,1:24]
   feriados=perfis[,25]
   verao=perfis[,26]
   datas=DADOS$DATAHIST$Data

   #=====================================
   # TRATAMENTO DOS DADOS DE TEMPERATURA
   #=====================================
   temperatura=matrix(0,dim(carga)[1],dim(carga)[2])
   if (tipotemp!=0) {
      # Remove NA da temperarura
      diastemperaturaNA=rep(0,dim(perfis)[1])
      diastemperaturaNA[which(is.na(apply(perfis[,27:50],1,mean))==T)]=1 # perfis com NA
      meses=as.numeric(substr(datas,6,7))
      anos=as.numeric(substr(datas,1,4))
      indiceNA=which(is.na(apply(perfis[,27:50],1,mean))==T)
      if (length(indiceNA)>0){
         # entra aqui se ha NA (lacuna) na temperatura
         for (i in 1:length(indiceNA)){
            NAperfil=26+which(is.na(perfis[indiceNA[i],27:50])==T) # horas do perfil com NA (colunas da matriz perfis com temperatura)
            if (indiceNA[i]>1 & indiceNA[i]<dim(perfis)[1]) {
               if (diastemperaturaNA[indiceNA[i]-1]==0 & diastemperaturaNA[indiceNA[i]+1]==0) {
                  perfis[indiceNA[i],NAperfil]=0.5*(perfis[(indiceNA[i]-1),NAperfil]+perfis[(indiceNA[i]+1),NAperfil])
                  diastemperaturaNA[indiceNA[i]]=0
               }
            }
            if (indiceNA[i]>1 & diastemperaturaNA[indiceNA[i]]==1 & diastemperaturaNA[indiceNA[i]-1]==0) {
               perfis[indiceNA[i],NAperfil]=perfis[(indiceNA[i]-1),NAperfil]
               diastemperaturaNA[indiceNA[i]]=0
            }
            if (indiceNA[i]<dim(perfis)[1] & diastemperaturaNA[indiceNA[i]]==1 & diastemperaturaNA[indiceNA[i]+1]==0) {
               perfis[indiceNA[i],NAperfil]=perfis[(indiceNA[i]+1),NAperfil]
               diastemperaturaNA[indiceNA[i]]=0
            }
            if (diastemperaturaNA[indiceNA[i]]==1) {
               # entra aqui se NA ainda nao foi corrigido
               indicemesano=which(meses==as.numeric(substr(datas[indiceNA[i]],6,7)) & anos==as.numeric(substr(datas[indiceNA[i]],1,4)) & diastemperaturaNA==0)
               if (length(indicemesano)>0) {
                  perfis[indiceNA[i],NAperfil]=apply(perfis[indicemesano,NAperfil],2,mean)
               } else {
                  indicemesano=which(diastemperaturaNA==0)
                  perfis[indiceNA[i],NAperfil]=apply(perfis[indicemesano,NAperfil],2,mean)
               }
               rm(indicemesano)
               diastemperaturaNA[indiceNA[i]]=0
            }
         }
      }
      #serietemperatura=c()
      #for (i in 1:dim(perfis)[1]) {
      #    serietemperatura=c(serietemperatura,as.numeric(perfis[i,27:50]))
      #}
      serietemperatura=as.vector(t(perfis[,27:50]))
      temperatura=perfis[,27:50]
      rm(indiceNA)
      rm(diastemperaturaNA)
      rm(meses)
      rm(anos)

      # remove outliers da temperatura
      # padroniza variavel temperatura: x1 = (temperatura-xbarra)/S
      x1=(serietemperatura-mean(serietemperatura))/sd(serietemperatura)
      x1suave=ksmooth(seq(1,length(x1),1),x1,bandwidth=50) # serie suavizada por Nadaraya-Watson
      limsup=x1suave$y+3.5*sd(x1suave$y-x1) # limite superior do intervalo de confianca
      liminf=x1suave$y-3.5*sd(x1suave$y-x1) # limite inferior do intervalo de confianca
      indiceoutliers1=which(x1<liminf) # indice dos outliers
      indiceoutliers2=which(x1>limsup) # indice dos outliers
      x1[indiceoutliers1]=x1suave$y[indiceoutliers1] # substituicao dos outliers por valores suavizados
      x1[indiceoutliers2]=x1suave$y[indiceoutliers2] # substituicao dos outliers por valores suavizados
      serietemperatura_filtrada=x1*sd(serietemperatura)+mean(serietemperatura) # serie filtrada
      # perfis de temperatura filtrada
      temperatura=matrix(serietemperatura_filtrada,dim(carga)[1],dim(carga)[2],byrow=T)
      rm(x1)
      rm(x1suave)
      rm(limsup)
      rm(liminf)
      rm(indiceoutliers1)
      rm(indiceoutliers2)
   }
   #===============================
   # TRATAMENTO DOS DADOS DE CARGA
   #===============================
   # substitui valores negativos por zero
   seriecarga=ifelse(seriecarga<0,0,seriecarga)

   # identifica erros grosseiros com a ajusta do boxplot da serie temporal da carga
   outliers=boxplot(seriecarga,plot=F)$out
   outliers=as.numeric(names(table(outliers)))
   noutliers=length(outliers)

   if (length(which(seriecarga>boxplot(seriecarga,plot=F)$stats[5,1]*1.05))>0){
      seriecarga[which(seriecarga>boxplot(seriecarga,plot=F)$stats[5,1]*1.05)]=0 #limite superior
   }
   if (length(which(seriecarga<=boxplot(seriecarga,plot=F)$stats[1,1]))>0){
      seriecarga[which(seriecarga<=boxplot(seriecarga,plot=F)$stats[1,1]/2)]=0  #limite inferior
   }

   carga=data.frame(matrix(seriecarga,dim(carga)[1],dim(carga)[2], byrow = TRUE))
   rownames(carga)=rownames(perfis)


   # percorre perfis diarios de carga e verifica status do perfil
   # status = 0 perfil diario e bom
   nperfiscarga=dim(carga)[1]
   status=rep(0,nperfiscarga) # guarda o numero de valores errados
   for (i in 1:nperfiscarga) {
      somalinha=sum(carga[i,])
      if (somalinha==0) {
         # entra aqui se nao ha dados de carga para o dia i
         status[i]=-1 # -1 indice perfil com todos os valores nulos
      } else {
         aux=c()
         if (noutliers>0){
            aux=which(carga[i,]==0)
            # for (j in 1:noutliers) {
            # aux=c(aux,which(carga[i,]==outliers[j]))
            # carga[i,which(carga[i,]==outliers[j])]=0
            # }
         }
         contador=length(aux)
         if (contador>0) status[i]=contador # status = numero de outliers no perfil diario
      }
   }


   #=========================================
   # ANALISE DE AGRUPAMENTOS DOS PERFIS BONS
   #=========================================
   # faz analise de agrupamentos dos perfis com status=0 (perfis bons)
   # perfis em pu da media
   indice=which(status!=0)
   indice12=which(feriados==12) # marca dias de blecautes e jogos da copa
   if (length(indice)>0) {
      if (length(indice12)>0) {
         perfispu=carga[-unique(c(indice,indice12)),] # apenas perfis com status=0 (perfis bons) e sem blecautes e copa
      } else {
         perfispu=carga[-indice,] # apenas perfis com status=0 (perfis bons)
      }
   } else {
      if (length(indice12)>0) {
         perfispu=carga[-indice12,] # apenas perfis com status=0 (perfis bons) e sem blecautes e copa
      } else {
         perfispu=carga
      }
   }
   media=apply(perfispu,1,mean) # medias dos perfis diarios
   perfispu=sweep(perfispu,1,media,FUN="/") # aqui coloca perfis em pu da media
   diasemana=weekdays1(as.Date(rownames(perfispu)))
   mes=substr(rownames(perfispu),6,7)
   resultado=hclust(dist(perfispu),"ward.D2") # executa analise de agrupamentos dos perfis

   # pega dados dos perfis que participaram dos clusters
   if (tipotemp!=0){
      if (length(indice)>0) {
         if (length(indice12)>0) {
            xtemperatura=temperatura[-unique(c(indice,indice12)),] # apenas temperaturas dos dias com perfis bons e sem blecautes e copa
         } else {
            xtemperatura=temperatura[-indice,] # apenas temperaturas dos dias com perfis bons
         }
      } else {
         if (length(indice12)>0) {
            xtemperatura=temperatura[-indice12,] # sem blecautes e copa
         } else {
            xtemperatura=temperatura
         }
      }
      xmediatemperatura=apply(xtemperatura,1,median) # mediana da temperatura
      xmintemperatura=apply(xtemperatura,1,min) # temperatura minima
      xmaxtemperatura=apply(xtemperatura,1,max) # temperatura maxima
   }
   if (length(indice)>0) {
      if (length(indice12)>0) {
         xferiados=as.character(feriados[-unique(c(indice,indice12))]) # apenas feriados dos dias com perfis bons e sem blecautes e copa
         xverao=verao[-unique(c(indice,indice12))] # apenas horario de verao com perfis bons
      } else {
         xferiados=as.character(feriados[-indice]) # apenas feriados dos dias com perfis bons e sem blecautes e copa
         xverao=verao[-indice] # apenas horario de verao com perfis bons
      }
   } else {
      if (length(indice12)>0) {
         xferiados=as.character(feriados[-indice12]) # apenas feriados dos dias com perfis bons e sem blecautes e copa
         xverao=verao[-indice12] # apenas horario de verao com perfis bons e sem blecautes e copa
      } else {
         xferiados=as.character(feriados) # apenas feriados dos dias com perfis bons
         xverao=verao # apenas horario de verao com perfis bons
      }
   }
   xverao=ifelse(xverao==1,"VERAO","HNORMAL")

   # pega solucao com numeroclusters
   clusters=cutree(resultado,numeroclusters)
   xclusters = as.character(paste("C",clusters,sep=""))
   perfis_tipicos=c()
   for (i in 1:numeroclusters) {
      indicecurvasnocluster=which(clusters==i)
      perfis_tipicos=rbind(perfis_tipicos,apply(perfispu[indicecurvasnocluster,],2,mean))
   }
   rownames(perfis_tipicos)=as.character(paste("C",seq(1,numeroclusters,1),sep=""))
   colnames(perfis_tipicos)=as.character(paste("Hora.",seq(1,24,1),sep=""))

   dadosNB=data.frame(diasemana,mes,xferiados,xverao,xclusters) # dados classificados para o Naive Bayes

   #==================================================================
   # AJUSTA SVM PARA ESTIMACAO DA DEMANDA MEDIA DIARIA
   # O RESULTADO SERA UTILIZADO NO AJUSTE DOS DIAS SEM DADOS DE CARGA
   #==================================================================
   if (tipotemp!=0) {
      XX1=apply(temperatura,1,median)/100
      XX2=apply(temperatura,1,max)/100
      XX3=apply(temperatura,1,min)/100
   }
   XX4=feriados
   XX5=verao
   XX6=as.numeric(substr(rownames(carga),6,7))/12 # mes
   XX7=matrix(0,length(weekdays1(as.Date(rownames(carga)))),6) # dia da semana
   nomesdiassemanas=c("domingo","segunda-feira","terca-feira","quarta-feira","quinta-feira","sexta-feira","sabado")
   diasemana=weekdays1(as.Date(rownames(carga)))
   for (i in 1:6) {
      XX7[grep(nomesdiassemanas[i],diasemana),i]=1
   }
   YY=apply(carga,1,mean)/100000
   YY=loess(YY~seq(1,length(YY),1),span=0.1)$fitted

   lags=c(1,7)
   # forma padroes de entrada/saida excluindo os casos com missing data na carga
   XX1n=c()
   XX2n=c()
   XX3n=c()
   XX4n=c()
   XX5n=c()
   XX6n=c()
   XX7n=c()
   XX8n=c()
   YYn=c()

   for (i in max(lags+1):length(YY)) {
      if (sum(status[c(i,(i-lags))])==0 & XX4[i]!=12) {
         # entra aqui se os dias envolvidos apresentam perfis bons e sem blecuate e jogo da copa
         if (tipotemp!=0) {
            XX1n=c(XX1n,XX1[i])
            XX2n=c(XX2n,XX2[i])
            XX3n=c(XX3n,XX3[i])
         }
         XX4n=c(XX4n,(XX4[i]/12)) # MODIFICADO 19 JULHO 2018
         XX5n=c(XX5n,XX5[i])
         XX6n=c(XX6n,XX6[i])
         XX7n=rbind(XX7n,XX7[i,])
         aux=YY[(i-lags)] # vetor com valores passados da demanda
         XX8n=rbind(XX8n,aux)
         YYn=c(YYn,YY[i])
      }
   }
   rownames(XX8n)=NULL
   if (tipotemp!=0) {
      datatrain=data.frame(XX1n,XX2n,XX3n,XX4n,XX5n,XX6n,XX7n,XX8n,YYn)
      colnames(datatrain)[13:(13+(length(lags)-1))]=paste("Lag",lags,sep="")
   } else {
      datatrain=data.frame(XX4n,XX5n,XX6n,XX7n,XX8n,YYn)
      colnames(datatrain)[10:(10+(length(lags)-1))]=paste("Lag",lags,sep="")
   }

   if (treinamodelo==1) {
      pesowsvm=(1-alfawsvm)^seq(length(YYn),1,-1)
      if(sum(pesowsvm)>0 & alfawsvm>0){
         pesowsvm=pesowsvm/sum(pesowsvm)
      } else {
         pesowsvm=rep(1,length(pesowsvm))
      }
      # Ajusta um SVM
      #aux=tune(svm, YYn~., data = datatrain,ranges = list(epsilon=c(0.01,0.05,0.1), gamma = c(0.01,0.05,0.1), cost = c(1000,10000))) # COMENTADO 19 JULHO 2018
      aux=tune_wsvm(YYn~.,weight=pesowsvm, data = data.frame(datatrain),ranges = list(epsilon=c(0.01,0.1), cost = c(1,10)))
      ksv=aux$best.model
      save(ksv,file="svm1.rda")
   } else {
      load(file="svm1.rda")
   }
   previsao=predict(ksv,datatrain[,-dim(datatrain)[2]])
   # demandas medias estimadas
   demandasmediasestimadas=previsao*100000
   #rm(XX1n);rm(XX2n);rm(XX3);rm(XX4n);rm(XX5n);rm(XX6n);rm(XX7n);rm(XX8n);rm(YYn)

   #==============================================
   # CORRIGE CASOS SEM DADOS DE CARGA (STATUS -1)
   #==============================================
   # inicializa matriz com carga filtrada
   cargafiltrada=carga

   # classificador Naive Bayes
   # tabelas de probabilidades apriori e condicionais
   tabelas=naiveBayes(xclusters~diasemana+mes+xferiados+xverao,laplace=1,data=dadosNB)
   #indice=which(status==-1)
   indice=which(status!=0)
   if (length(indice)>0) {
      x1=weekdays1(as.Date(rownames(carga)[indice]))
      x1mes=substr(as.Date(rownames(carga)[indice]),6,7)
      x2=as.character(feriados[indice])
      x3=ifelse(verao[indice]>0,"VERAO","HNORMAL")
      entrada=data.frame(x1,x1mes,x2,x3)
      names(entrada)=c("diasemana","mes","xferiados","xverao")
      # previsao para preeenchimento dos dias sem dados de carga
      ## clusterparamissingdata=predict(classificador,newdata=entrada)
      ## calculo da posteriori sem usar a rotina predict
      clusterparamissingdata=c()
      for (i in 1:dim(entrada)[1]) {
         coluna1=which(x1[i]==colnames(tabelas$tables$diasemana))
         coluna1mes=which(x1mes[i]==colnames(tabelas$tables$mes))
         coluna2=which(x2[i]==colnames(tabelas$tables$xferiados))
         coluna3=which(x3[i]==colnames(tabelas$tables$xverao))
         produto=rep(0,numeroclusters)
         for (j in 1:numeroclusters) {
            if (x2[i]==0) {
               # entra aqui se dia normal
               produto[j]=tabelas$apriori[j]*tabelas$tables$diasemana[j,coluna1]
               produto[j]=produto[j]*tabelas$tables$mes[j,coluna1mes]
               produto[j]=produto[j]*tabelas$tables$xferiados[j,coluna2]
               produto[j]=produto[j]*tabelas$tables$xverao[j,coluna3]
            } else {
               if (x2[i]!=12){
                  # se feriado ou dia especial
                  produto[j]=tabelas$apriori[j]*tabelas$tables$diasemana[j,coluna1]
                  produto[j]=produto[j]*tabelas$tables$xferiados[j,coluna2]
                  produto[j]=produto[j]*tabelas$tables$xverao[j,coluna3]
               } else {
                  # blecaute ou jogo da copa
                  produto[j]=tabelas$apriori[j]*tabelas$tables$diasemana[j,coluna1]
                  produto[j]=produto[j]*tabelas$tables$xverao[j,coluna3]
               }
            }
         }
         clusterparamissingdata=c(clusterparamissingdata,names(tabelas$apriori[which(produto==max(produto))]))
         # preenche dias sem dados de carga
         XX1=mean(as.numeric(temperatura[indice[i],]))/100
         XX2=max(as.numeric(temperatura[indice[i],]))/100
         XX3=min(as.numeric(temperatura[indice[i],]))/100
         XX4=feriados[indice[i]]
         if (XX4==12) {
            XX4=0
         } else {
            XX4=XX4/12
         }
         XX5=verao[indice[i]]
         XX6=as.numeric(substr(rownames(carga[indice[i],]),6,7))/12 # mes
         XX7=rep(0,6)
         aux=weekdays1(as.Date(rownames(carga[indice[i],])))
         if (grep(aux,nomesdiassemanas)<7) XX7[grep(aux,nomesdiassemanas)]=1
         XX8=YY[(indice[i]-lags)] # vetor com valores passados da demanda
         if (tipotemp==0) padrao=c(XX4,XX5,XX6,XX7,XX8)
         if (tipotemp>0) padrao=c(XX1,XX2,XX3,XX4,XX5,XX6,XX7,XX8)
         names(padrao)=colnames(datatrain[-length(datatrain)])
         demandasmediasestimadas=predict(ksv,t(padrao))*100000
         indicecurvasnocluster=which(clusterparamissingdata[i]==xclusters)
         curvaproxima=apply(perfispu[indicecurvasnocluster,],2,mean)*demandasmediasestimadas
         indice0=which(cargafiltrada[indice[i],]==0)
         if (status[indice[i]]==1){ # lacuna de 1 hora
            for (j in indice0){
               if (j==1){
                  cargafiltrada[indice[i],j]=cargafiltrada[indice[i],2]*curvaproxima[1]/curvaproxima[2]
               }else{
                  cargafiltrada[indice[i],j]=cargafiltrada[indice[i],(j-1)]*curvaproxima[j]/curvaproxima[j-1]
               }
            }
         }else{
            curvaproxima=apply(perfispu[indicecurvasnocluster,],2,mean)*demandasmediasestimadas
            indice0=which(cargafiltrada[indice[i],]==0)
            cargafiltrada[indice[i],indice0]=curvaproxima[indice0]
         }
      }
   }

   #===============================================
   # PASSA FILTRO NADARAYA WATSON NA SERIE DE CARGA
   #===============================================
   seriecargafiltrada=as.vector(t(cargafiltrada))

   # padroniza variavel
   x1=(seriecargafiltrada-mean(seriecargafiltrada))/sd(seriecargafiltrada)
   x1suave=ksmooth(seq(1,length(x1),1),x1,bandwidth=50) # serie suavizada por Nadaraya-Watson
   limsup=x1suave$y+3.5*sd(x1suave$y-x1) # limite superior do intervalo de confianca
   liminf=x1suave$y-3.5*sd(x1suave$y-x1) # limite inferior do intervalo de confianca
   indiceoutliers1=which(x1<liminf) # indice dos outliers
   indiceoutliers2=which(x1>limsup) # indice dos outliers
   x1[indiceoutliers1]=x1suave$y[indiceoutliers1] # substituicao dos outliers por valores suavizados
   x1[indiceoutliers2]=x1suave$y[indiceoutliers2] # substituicao dos outliers por valores suavizados
   seriecargafiltrada=x1*sd(seriecargafiltrada)+mean(seriecargafiltrada) # serie filtrada

   #========================
   # MONTA OBJETOS DE SAIDA
   #========================
   serieferiado=as.vector(t(outer(feriados,rep(1,24))))
   serieverao=as.vector(t(outer(verao,rep(1,24))))

   if (tipotemp==0) {
      SERIEHIST_FILTRADAS=data.frame(seriecargafiltrada,serieferiado,serieverao,rep(0,length(seriecargafiltrada)))
   } else {
      SERIEHIST_FILTRADAS=data.frame(seriecargafiltrada,serieferiado,serieverao,serietemperatura_filtrada)
   }

   cargafiltrada=matrix(seriecargafiltrada,dim(carga)[1],dim(carga)[2],byrow=T)
   DATAHIST_FILTRADOS=data.frame(cargafiltrada,feriados,verao,temperatura)
   rownames(DATAHIST_FILTRADOS)=rownames(perfis)

   # EXPORTA LISTA DADOSFILTRADOS
   DADOSFILTRADOS=list(DATAHIST_FILTRADOS=DATAHIST_FILTRADOS,SERIEHIST_FILTRADAS=SERIEHIST_FILTRADAS,TABELAS_NAIVE_BAYES=tabelas,TIPOLOGIAS=perfis_tipicos)
   return(DADOSFILTRADOS)
}
