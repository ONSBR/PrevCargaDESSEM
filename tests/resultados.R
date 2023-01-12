library(ggplot2)




dadosMdl[,Modelo := as_factor(Modelo)]

fwrite(dadosMdl,paste0('resultados_',dataPrev,'.csv'),sep = ";", dec= ",")


gg <- ggplot(dadosMdl,aes(x= DataHora, y = Carga, group = Modelo, color = Modelo)) + geom_line()
gg
