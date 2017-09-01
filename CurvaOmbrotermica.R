### Gera Curva Ombrotérmica a partir de dados de ###         
# Estações meteriológicas
# ou Alvares, C.A., Stape, J.L., Sentelhas, P.C., Gonçalves, J.L.M.; Sparovek, G., 2013. Köppen's climate classification map for Brazil. Meteorologische Zeitschrift, DOI: http://dx.doi.org/10.1127/0941-2948/2013/0507

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  Curva Ombrotérmica: PrecipitacaoTotal e (TempCompensadaMedia*2) ###


###---------------------------------------------------------------------###


if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, raster)

###---------------------------------------------------------------------###
rm(list = ls()) #; gc()
memory.limit(size = 4e3) 

dir.root= "F:/GitHub/DivFito"; setwd(dir.root)
dir.result = "F:/GitHub/DivFito/Result" #; setwd(dir.result)


###---------------------------------------------------------------------###

# Koppen Brasil, Alvares (2013)
koopenBR <- fread("F:/GitHub/DivFito/KoppenBR/KoppenBrazilianmunicipalities.txt")

# data <- data.frame(fread(file.choose()), stringsAsFactors = F)
# k = koopenBR$`IBGE-Code` %in% data$IBGE.Code

data <- data.frame(fread("LocalidadesRevisao.txt"), stringsAsFactors = F) 
k = trim(koopenBR$Municipality) %in% trim(c(data$county)) & 
   trim(koopenBR$State) %in% trim(c(data$stateProvince)) 

m = koopenBR[k, 1:6]
month.label = lapply('T_', gsub, replacement = '', x = colnames(koopenBR[k, 7:18]))[[1]]

image.save = T

for(i in 1:NROW(m))
{
  
  k = trim(koopenBR$Municipality) == trim(c(m$Municipality[i])) & 
      trim(koopenBR$State) == trim(c(m$State[i])) 
  
  t = as.numeric(t(koopenBR[k, 7:18]*2))
  r = as.numeric(t(koopenBR[k, 19:30]))
  
  file.jpg = gsub(' ', '_',paste0(dir.result,'/',m$Köppen[i],'_',m$Municipality[i],'_',m$State[i],'.jpg'))
  nome.mapa = paste0(m$Municipality[i], '-', m$State[i], ' Köppen (', m$Köppen[i],')')
  
 if (image.save==T){jpeg(file.jpg)}
  plot(r, type="l" , col=4, 
       #ylim=c(-1, max(a$PrecipitacaoTotal)), 
       ylim=c(-1, 500), 
       #xlim=range(1:12),
       main = nome.mapa)
  lines(t, col=2)
  if (image.save==T){dev.off()}
}

###---------------------------------------------------------------------###

# baixar dados de http://www.inmet.gov.br/portal/index.php?r=bdmep/bdmep
# Série histporica - Dados Mensais
# Precipitação Total (mm) e Temp Compensada Média(ºC)
# criar subpasta BDMEP-INMET e salvar os dados de cada estação "INMET-NomeDaEstacao.txt"
# limpar manualemtne anos com falhas nos registros mensais
# 

###---------------------------------------------------------------------###

# Informar parâmetros:

dir.root= "D:/GitHub/DivFito/BDMEP-INMET"; setwd(dir.root)

###---------------------------------------------------------------------###

file = list.files(pattern = 'INMET-')
for(f in 1:length(file))
{
  file.txt = file[f]
  file.jpg = paste0(substr(file.txt,7,nchar(file.txt)-4),'.jpg')
  a = read.table(file.txt, sep = "\t", header = T); head(a)
  nome.mapa = substr(file.txt,7,nchar(file.txt)-4)
  nome.mapa =  paste0(nome.mapa,'-',
                      substr(range(as.Date(a$Data, "%d/%m/%Y"))[1],1,4),'-',
                      substr(range(as.Date(a$Data, "%d/%m/%Y"))[2],1,4))
  a$Tx2 <- a$TempCompensadaMedia*2
  jpeg(file.jpg)
  plot(a$PrecipitacaoTotal, type="l" , col=4, 
       #ylim=c(-1, max(a$PrecipitacaoTotal)), 
       ylim=c(-1, 500), 
      #xlim=range(1:12),
       main = nome.mapa)
  lines(a$Tx2, col=2)
  dev.off()
}

###---------------------------------------------------------------------###

# dados baixados do Diva, a temperatura não é a média e sim a sima da (max+mim)/2 
# para avaliar o padrão

dataclima <- read.csv('D:/GitHub/DivFito/LocalidadesRevisao.txt',header=T, sep='\t',dec='.') 

dataclima <- dataclima[dataclima$TipoCoordenada%in%c('original','ajustada'),]

for(d in 1:nrow(dataclima))
{
  prec <- as.numeric(dataclima[d ,paste0('prec',(1:12))])
  tmax <- dataclima[d ,paste0('tmax',(1:12))]
  tmin <- dataclima[d ,paste0('tmin',(1:12))]
  tmedx2 <- as.numeric((tmin+tmax)/2*2)
  #tmedx2 <- as.numeric((tmax)*2)

  file.jpg = paste0(dataclima$Municipality[d],dataclima$codigo.referencia[d],'.jpg')
  nome.mapa = paste0(dataclima$referencia[d],dataclima$Municipality[d])

  jpeg(file.jpg)
  
  plot(prec, type="l" , col=4, 
       #ylim=c(-1, max(a$PrecipitacaoTotal)), 
       ylim=c(-1, 500), 
       #xlim=range(1:12),
       main = nome.mapa)
  lines(tmedx2, col=2)   
  
  dev.off()
}
