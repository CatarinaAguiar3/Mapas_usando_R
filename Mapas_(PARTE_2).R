# MAPAS (PARTE 2) ---------------------------------------------------------
#Autora:Catarina Aguiar

#Resumo:
#Será utilizada duas bases de dados espaciais que no final serão unidas para
#a criação de uma mapa

#as bases utilizadas são dos municipios brasileiros 
#e dos estabelecimentos de saúde do Brasil


dir()
setwd('C://R//Analise Espacial')

# Pacotes -----------------------------------------------------------------
rm(list = ls(all = TRUE))

library(sidrar)
library(tidyverse)
library(sf)
library(geobr)
library(magrittr)

# Base de Dados espacial dos municipios brasileiros -----------------------------------------------------
?geobr

brasil <- read_municipality()
#Salvar a base de dados
brasil %>% write_sf(brasil, file="Brasil.shp")

#Estado de MG
MG <- brasil %>% filter(
  abbrev_state == "MG"
)

rm(MG_estab1)

# Base de dados espacial unidades de saúde no Brasil ---------------------------------------------------------

#read_health_facilities() é uma função que baixa os 
#dados de geolocalização de unidades de saúde cadastradas no CNES

#Estabelecimentos de saúde do Brasil
estabelecimentos <- read_health_facilities()

#Mapa dos Estabelecimentos de saúde de MG
MG_estab <- estabelecimentos %>% filter(
  abbrev_state == "MG") 


# Mapa de pontos com duas camadas -------------------------------------
#Cada ponto no mapa reprenta um estabelecimento de saúde em MG
ggplot()+geom_sf(data=MG) +geom_sf(data= MG_estab)


# Mapa categorico ---------------------------------------------------------

#Número de estabelecimentos em cada municipio de MG
n_estab_MG <- MG_estab%>% group_by(
  code_muni) %>% summarise(numero_estab= n())
 
#Média do número de estabelcimentos de MG
summary(n_estab_MG)


#Tirar a coluna geom e salvar como Data.Frame
#É necessário fazer isto pois a coluna "geom" possui pares de coordenadas geográficas
#que impediriam a união do data.frame "n_est" com o data.frame "MG"
n_est <- n_estab_MG %>% data.frame() %>% select(-geom)


#Criar as categorias que servirão como legenda
n_est<- n_est%>% mutate (
  code_muni=as.numeric(code_muni),
  legenda=cut(
    numero_estab,
    breaks=c(-Inf,52, 100, 1000, Inf),
    labels= c("Até 52", "De 52 até 100", "De 100 até 1000", "Acima de 1000")
  )
)




# Juntar as bases e mapa final --------------------------------------------

#Alterar os números da coluna code_muni da base de dados "brasil"
MG<-brasil %>% filter(abbrev_state=="MG") %>% mutate(
  code_muni=as.numeric(code_muni),
  code_muni=trunc(code_muni/10))

#Juntar as bases "MG" e "n_est"
bases_juntas <- MG %>% left_join(
  n_est, by="code_muni"
)

#Criar o gráfico
ggplot(bases_juntas)+ geom_sf(aes(fill= legenda))
