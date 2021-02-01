# CARREGANDO BIBLIOTECAS ---------------

library(rio)
library(lubridate)
library(stringr)
library(tidyverse)
library(plm)
library(haven)
library(reshape2)
library(geobr)
library(reshape2)
library(sf)
library(raster)
library(spData)
library(tmap)
# CRIANDO FUNÇÕES E VETORES

# nomes de colunas da tabela de leitos

vetor_leitos <- c("municipio", "adm_federal", "adm_estadual", "adm_municipal", "adm_pub_outras",
  "sociedade_mista", "empresarial", "s_fim_lucrativo", "total")

vetor_nome_mapa <- c("municipio", "id_municipio", "ano_ref",
                     "leidecriacao", "area", "geometry")

vetor_zonadamata <- c("313670", #jf
                      "314390", #muriae     
                      "316990", #ubá
                      "316070", #santos dummont
                      "313940", #manhuaçu
                      "311530", #cataguases
                      "315210", #ponte nova
                      "317130", #viçosa
                      "313840", #leopoldina
                      "310150") #além paraíba

# função para transformar NA em 0 

zeradora <- function(x){
 ifelse(x == "-", 0, as.double(x)  )
}

# função para calcular e arredondar valores em porcentagem

geradora_de_porcento <- function (x,y){
  round((x/y)*100, 2)
}

# ABRINDO AS BASE -------------------------

# populacao residente em minas gerais - estimativas do IBGE para 2020
pop_estimada <- read.csv("pop_estimada.csv", header=FALSE)%>%
  filter( str_starts(V1, pattern = "31"))%>%
  rename(id_municipio = V1, municipio = V2, pop2020 = V3)%>%
  mutate(pop2020 = as.numeric(pop2020),
         id_municipio = str_sub(id_municipio,1,6))


# leitos municipais 
leitos_municipio <- import("leitos_pub_priv_municipio.xlsx")%>%
  filter(!Município == is.na(Município))
  
colnames(leitos_municipio)<- vetor_leitos

  
    
# MODIFICANDO A BASE -----------------  
  
   # retirando "-" e criando codigo municipal
leitos_municipio_imp <- leitos_municipio%>%
   # aplicando funcao zeradora em todas as colunas
    mutate(across(-1, zeradora),
        #criando cod municipal
      id_municipio = str_sub(municipio, 1,6),
      across(-c("municipio", "id_municipio"), as.numeric),
        # criando novas colunas
      pub = adm_federal+ adm_estadual+adm_municipal+adm_pub_outras,
      priv = empresarial +s_fim_lucrativo +sociedade_mista,
      porcentagem_pub = geradora_de_porcento(pub,total),
      porcentagem_priv = geradora_de_porcento(priv,total)) 
 

# JUNTANDO AS BASES -------------------------

leitos_municipio_imp2<- leitos_municipio_imp%>%
left_join(pop_estimada, by = "id_municipio")%>%
# left_join(mapa_minas,by = "id_municipio" )%>%
    #criando colunas de interesse
    mutate(leito_pub_pcapita = (pub/pop2020)*1000,
           leito_priv_pcapita = (priv/pop2020)*1000,
           leito_pcapita = (total/pop2020)*1000)


# GRAFICOS ----------------------------------

#Subbase zona da mata
leitos_zm <- leitos_municipio_imp2%>%
  filter(id_municipio %in% vetor_zonadamata)%>%
  mutate(nome_municipio = str_sub(municipio.x, 7, ),
         acima_oms = if_else(leito_pcapita>4,"acima da média","abaixo da média"),
         acima_oms_pub = if_else(leito_pub_pcapita>4,"acima da média","abaixo da média"),
         acima_oms_priv = if_else(leito_priv_pcapita>4,"acima da média","abaixo da média"))
  
# leitos totais
ggplot(data= leitos_zm, 
       aes(x = reorder(nome_municipio, desc(leito_pcapita)), y=leito_pcapita, fill = acima_oms)) +
       geom_col() +  geom_hline(yintercept = 4, color = "red", linetype = 2) +
       labs(x = "Município",
            y = "Leitos a cada mil habitantes",
            title = "Número de leitos por habitante na Zona da Mata Mineira",
            subtitle = "Em vermelho, média recomendada pela OMS") +
      theme_classic() + theme(legend.key = element_rect(fill = "white"), 
      legend.background = element_rect(fill = "white"), 
      legend.position = "left") +labs(fill = "Acima da média da OMS?", caption = "Fonte: CNES/DATASUS
Elaboração : JFemDados")
         
# leitos privados
ggplot(data= leitos_zm, 
       aes(x = reorder(nome_municipio, desc(leito_priv_pcapita)), y=leito_priv_pcapita, fill = acima_oms_priv)) +
  geom_col() +  geom_hline(yintercept = 4, color = "red", linetype = 2) +
  labs(x = "Município",
       y = "Leitos públicos a cada mil habitantes",
       title = "Número de leitos privados por habitante na Zona da Mata Mineira",
       subtitle = "Em vermelho, média recomendada pela OMS") +
  theme_classic() + theme(legend.key = element_rect(fill = "white"), 
                          legend.background = element_rect(fill = "white"), 
                          legend.position = "left") +labs(fill = "Acima da média da OMS?", caption = "Fonte: CNES/DATASUS
Elaboração : JFemDados")

# leitos de adm publica
ggplot(data= leitos_zm, 
       aes(x = reorder(nome_municipio, desc(leito_pub_pcapita)), y=leito_pub_pcapita, fill = acima_oms_pub)) +
  geom_col() +  geom_hline(yintercept = 4, color = "red", linetype = 2) +
  labs(x = "Município",
       y = "Leitos públicos a cada mil habitantes",
       title = "Número de leitos públicos por habitante na Zona da Mata Mineira",
       subtitle = "Em vermelho, média recomendada pela OMS") +
  theme_classic() + theme(legend.key = element_rect(fill = "white"), 
                          legend.background = element_rect(fill = "white"), 
                          legend.position = "left") +labs(fill = "Acima da média da OMS?", caption = "Fonte: CNES/DATASUS
Elaboração : JFemDados")

# leitos totais
ggplot(data= leitos_zm, 
       aes(x = nome_municipio, y=porcentagem_priv)) +
  geom_bar(position = "fill") +  geom_hline(yintercept = 50, color = "red", linetype = 2) +
  labs(x = "Município",
       y = "Proporção de leitos de administração pública e privada",
       title = "Número de leitos por habitante na Zona da Mata Mineira",
       subtitle = "Em vermelho, média recomendada pela OMS") +
  theme_classic() + theme(legend.key = element_rect(fill = "white"), 
                          legend.background = element_rect(fill = "white"), 
                          legend.position = "left") +labs(fill = "Acima da média da OMS?", caption = "Fonte: CNES/DATASUS   
Elaboração : JFemDados")







# MAPAS ------------------------------------

 # criando um mapa com quais municípios tem maior porcentagem de leitos privados

# abrindo mapa de Minas Gerais
corrompidos <- st_is_valid(mapa_minas)

mapa_minas <- st_read("mapaminas/MG_Municipio_2020_set.shp")%>%
  set_names(vetor_nome_mapa)%>%
  mutate(id_municipio = str_sub(id_municipio,1,6))%>%
  filter(geometry == corrompidos)

corrompidos <- st_is_valid(mapa_minas)

tm_shape(mapa_minas)+tm_fill()

st_is_valid(mapa_minas)

















#mortalidade por CID juiz de fora
jf_por_cid_ano <- read_excel("C:/Users/Matheus/Desktop/mort_por_cid_jf.xlsx")

jf_por_cid_ano2 <- jf_por_cid_ano%>%
  rename(causa = `Categoria CID-10`)%>%
  mutate(cid = str_sub(causa,1,3))




jf_por_cid_ano2<- melt(jf_por_cid_ano2, id.vars = c("cid"), 
                        measure.vars = c("2012", "2013", "2014", "2015", "2016", "2017",
                                         "2018", "2019", "Total"))


dengue <- jf_por_cid_ano2%>%
  filter(cid == 'A90')