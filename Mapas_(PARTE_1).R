#ANÁLISE ESPACIAL -CRIAÇÃO DE MAPAS DE ESTADOS E MUNÍCIPIOS

#Autora:Catarina Aguiar

install.packages("geobr")
install.packages("sf")

library(tidyverse)
library(sf)
library(geobr)

# Abrir diretório ---------------------------------------------------------


setwd('C:/R/Analise Espacial')
dir()

# Importar a base ---------------------------------------------------------
#A função read_municipality permite baixar dados espaciais dos municípios brasileiros
#Para mais informções acessar o link: https://search.r-project.org/CRAN/refmans/geobr/html/read_municipality.html
brasil <- read_municipality()
view(brasil)

#Salvar a base de dados
brasil %>% st_write("Analise_Espacial_Brasil.shp")

#Criar o gráfico
ggplot(brasil) +
geom_sf()


#Mapa da cidade de SP
sp_brasil <- brasil %>%  filter(
  name_muni == "São Paulo") 

ggplot(sp_brasil) +
  geom_sf()


#Mapa da cidade do RJ
RJ_brasil <- brasil %>%  filter(
  name_muni == "Rio De Janeiro"
) 

ggplot(RJ_brasil) +
  geom_sf()


# Mapa dos estados --------------------------------------------------------

estado_RJ <- brasil %>% filter(
 abbrev_state== "RJ") %>% ggplot() + geom_sf()

estado_RJ


# Robô --------------------------------------------------------------------
#Criar uma função para automatizar a criação de mapas dos estados
mapas_estados = function(a){
  brasil %>% filter(
    abbrev_state== a
  ) %>% ggplot()+geom_sf()
}

#Mapa do RJ
# Chamando a função mapas_estados cujo argumento é RJ
mapas_estados("RJ")


## Mapas Estados ####
func_estados=function(a){
  brasil <- read_municipality()
  
  brasil %>% filter(
    abbrev_state== a
  ) %>% ggplot()+geom_sf()
}

# Chamando a função mapas_estados cujo argumento é CE
func_estados("CE")

## Função Municipios ####
#Criar uma função para automatizar a criação de mapas dos municipios 
func_municipios=function(a){
  brasil <- read_municipality()
  
  brasil %>% filter(
    name_muni== a
  ) %>% ggplot()+geom_sf()
}

# Chamando a função mapas_municipios cujo argumento é Angra dos Reis
func_municipios("Angra Dos Reis")


# Loop --------------------------------------------------------------------
#Criação de um Loop para gerar e salvar mapas automaticamente

#Vetor com os estados de interesse
interesse <- c("RJ", "SP", "ES" , "MG")

#Loop que vai salvar o mapa dos estados na pasta "mapas_estados" no formato pdf
for (estado in interesse){
  nome=str_c("mapas_estados/", estado,".pdf")
  
  brasil %>% filter(
    abbrev_state== estado
  ) %>% ggplot()+geom_sf()
  
  ggsave(nome)
}
  

