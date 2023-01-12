library(lubridate)
library(data.table)
library(aws.s3)
library(jsonlite)
library(fasttime)
library(paws)

### Geracao de Decks -----------------------------------------------------------------------------------------------------------------------

s3source(object = "prevcarga/decks/source/apoio.R",bucket = Sys.getenv("BUCKET_CONFIG"))


deckPartial <- function(dataPrev,horaIni,area,vs,tVerif,ndias,oficial = FALSE){
   configjs <- s3read_using(fromJSON,object = "prevcarga/PCDS/config/config.json",bucket = Sys.getenv("BUCKET_CONFIG"))
   # Leitura de decks do s3
   deck <- get_bucket(Sys.getenv("BUCKET_CONFIG"),prefix = paste0(configjs$deckPath,area))
   namesDeck <- unlist(lapply(deck,function(x){gsub('.csv.gz','',basename(x$Key))}))
   deck <- lapply(deck,function(d){
      s3read_using(fread,sep=";",dec=",", object = d$Key,bucket = Sys.getenv("BUCKET_CONFIG"))
   })
   names(deck) <- namesDeck

   dataHoraPrev <- as.POSIXct(paste(dataPrev,sprintf('%02d',horaIni)),format = '%F %H',tz="UTC")
   dataPrev <- as.Date(dataPrev)

   # Preparacao dos decks
   deck$CARGAHIST[,din_referencia := fastPOSIXct(din_referencia,"UTC") %m+% hours(1)]
   deck$TEMPHIST[,din_referencia := fastPOSIXct(din_referencia,"UTC")]
   deck$FERIADOS[,dat_diaespecial := as.Date(dat_diaespecial)]

   #### GAMBIARRA!!!!!!!!! FAZER DIREITO!!!!!!
   deck$FERIADOS[cod_prevcarga == 1,cod_prevcarga := 2]
   deck$FERIADOS <- deck$FERIADOS[!cod_prevcarga %in% c(9,10)]
   ####


   deck$CARGAHIST <- deck$CARGAHIST[din_referencia <= dataHoraPrev]
   deck$TEMPHIST  <- deck$TEMPHIST[din_referencia < dataHoraPrev]
   fer <- deck$FERIADOS[dat_diaespecial <= dataPrev %m+% days(ndias) & dat_diaespecial >= dataPrev]
   deck$FERIADOS <- deck$FERIADOS[dat_diaespecial < (dataPrev + ndias)]

   if(tVerif){
      deck$TEMPPREV <- deck$TEMPHIST[din_referencia <= dataPrev %m+% days(ndias+1) &
                                        din_referencia >= dataHoraPrev,.(Ano = year(din_referencia),Mes = month(din_referencia), Dia = day(din_referencia),
                                                                         Hora = hour(din_referencia), Min = minute(din_referencia), Temperatura = val_tmp)]
   } else {
      tempPrev <- deck$TEMPPREVHIST
      tempPrev[,din_origemprevisaoutc := fastPOSIXct(din_origemprevisaoutc,"UTC")]
      tempPrev[,din_referencia := fastPOSIXct(din_referencia,"UTC")]
      tempPrev <- tempPrev[din_referencia >= dataHoraPrev & din_referencia < dataPrev %m+% days(ndias+1)]
      tempPrev <- tempPrev[order(din_referencia,-din_origemprevisaoutc)]
      tempPrev[,dup := duplicated(din_referencia)]
      tempPrev <- tempPrev[dup ==F]
      tempPrev <- tempPrev[,.(DataHora = din_referencia, Exo_Temperatura = val_tmp)]
      if(nrow(tempPrev)<(24*(ndias+1)-horaIni)) warning("Dados de temperatura prevista faltantes")
      deck$EXOGENAPREV <- tempPrev
   }

   deck$CARGAHIST <- deck$CARGAHIST[,.(DataHora = din_referencia, Carga = val_carga)]
   deck$EXOGENAHIST <- deck$TEMPHIST[,.(DataHora = din_referencia, Exo_Temperatura = val_tmp)]
   deck$FERIADOS <- deck$FERIADOS[order(dat_diaespecial),.(Data = dat_diaespecial, Tipo = cod_prevcarga)]

   dados <- list(deck = deck[c("CARGAHIST","EXOGENAHIST","FERIADOS","EXOGENAPREV","PATAMARES")])

   dados$area <- area
   dados$diaEstudo <- format(dataHoraPrev,'%F')
   dados$dataHoraEstudo <- format(dataHoraPrev,'%F %T')
   dados$diaFimPrev <- format(dataPrev %m+% days(ndias+1),'%F %T')
   dados$versaoModelo <- vs
   dados$tVerif <- tVerif
   dados$bucketPub <- configjs$deckPubBucket
   dados$pathPub <- configjs$deckPubPath
   dados$bucketResult <- configjs$bucketResult
   dados$pathResult <- configjs$pathResult
   dados$ndias  <- ndias
   dados$tipocarga <- "CAGB"
   dados$origem <- paste0("PCDS",gsub('\\.','',vs))
   dados$periodo <- "TM"
   dados$mdl <- deck$MODELOS

   return(dados)
}

deckPrevCargaPMO <- function(dataPrev,area,numDia = 7,caminho = ""){

   deck <- deckPartial(dataPrev = dataPrev,0,area,"",F,numDia-1)
   deck <- deck$deck
   deck$CARGAHIST[,Data := date(DataHora %m-% hours(1))]
   deck$CARGAHIST <- deck$CARGAHIST[,.(Carga = mean(Carga)),.(Data)]

   deck$EXOGENAHIST[,Data := date(DataHora)]
   deck$EXOGENAHIST <- deck$EXOGENAHIST[,.(Exo_TemperaturaMax = max(Exo_Temperatura),
                                           Exo_TemperaturaMed = mean(Exo_Temperatura),
                                           Exo_TemperaturaMin = min(Exo_Temperatura)),.(Data)]
   deck$EXOGENAPREV[,Data := date(DataHora)]
   deck$EXOGENAPREV <- deck$EXOGENAPREV[,.(Exo_TemperaturaMax = max(Exo_Temperatura),
                                           Exo_TemperaturaMed = mean(Exo_Temperatura),
                                           Exo_TemperaturaMin = min(Exo_Temperatura)),.(Data)]
   deck <- deck[c("CARGAHIST","EXOGENAHIST","FERIADOS","EXOGENAPREV")]

   lapply(names(deck),function(i) fwrite(deck[[i]],paste0(caminho,area,"_",dataPrev,"_", i,".csv"),sep=";",dec=","))
   return(deck)
}
