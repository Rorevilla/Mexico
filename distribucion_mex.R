#SHAPEFILES OBTENIDOS DE: http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825296520_s.zip
#Población obtenida de: https://www.inegi.org.mx/app/descarga/    indicadores/población

library(sf)
library(ggplot2)
library(openxlsx)
library(tidyverse)
library(scales)
library(patchwork)
library(hrbrthemes)
library(magrittr)

download.file(url = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825296520_s.zip",, destfile = "poligonos_mex.zip")
unzip("poligonos_mex.zip",exdir="702825296520_s")
location_municipios = paste(getwd(),"702825296520_s/32_Entidades_Federativas/MUNICIPIOS.shp",sep="/")
municipios <- st_read(location_municipios)%>% st_transform(4483)
location_estados = paste(getwd(),"702825296520_s/32_Entidades_Federativas/ESTADOS.shp",sep="/")
estados <- st_read(location_estados)%>% st_transform(4483)

centroides <- st_centroid(municipios) %>% st_transform(4483)%>% as.data.frame()

download.file(url = "https://www.inegi.org.mx/contenidos/masiva/indicadores/temas/estructura/estructura_00_xlsx.zip", destfile = "poblacion_municipio.zip")
unzip(zipfile = "poblacion_municipio.zip")
poblacion_municipio = read.xlsx("~/estructura_00.xlsx") %>% filter(indicador %in% c("Población total hombres","Población total mujeres")) %>% filter(entidad!="00") %>% filter(municipio!="000")
poblacion_municipio <- poblacion_municipio %>% select(entidad,municipio,indicador,pob_2010='2010')
poblacion_municipio <- poblacion_municipio %>% left_join(centroides,by=c("entidad"="CVE_ENT","municipio"="CVE_MUN"))
poblacion_municipio <- poblacion_municipio %>% mutate(geometry=str_remove_all(geometry,"[()]"))
poblacion_municipio <- poblacion_municipio %>% mutate(geometry=str_remove_all(geometry,"[-c,]")) %>% separate(geometry, c("latitud","longitud"),sep=" ")
poblacion_municipio$longitud <- as.numeric(poblacion_municipio$longitud)
poblacion_municipio$latitud <- as.numeric(poblacion_municipio$latitud)
poblacion_municipio$pob_2010 <- as.numeric(poblacion_municipio$pob_2010)

poblacion_municipio %>% group_by(entidad) %>% summarise(pob_2010=sum(pob_2010)) %>% ungroup() %>% top_n(5,pob_2010) %>% arrange(desc(pob_2010))
poblacion_municipio %>% filter(entidad=="15"|entidad=="09")%$% sum(pob_2010)/poblacion_municipio %>% drop_na() %$% sum(pob_2010)



a = ggplot() + 
    geom_sf(data = estados,fill="gray72",color="gray90")+
    geom_sf(data = estados %>% filter(CVE_ENT=="15"|CVE_ENT=="09"),fill="red3",color="white")+
    scale_x_continuous(position = "top")+
    coord_sf()+
    theme_ipsum_rc()+
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())


b = ggplot(poblacion_municipio,aes(x=longitud %>% round(1),y=pob_2010,fill=indicador)) +
    geom_bar(stat="identity")+
    scale_fill_manual(values=c("dodgerblue","firebrick1"))+
    coord_flip()+
    scale_y_continuous(labels = comma,position = "right")+
    scale_x_continuous(position = "top")+
    labs(y="POPULATION")+
    theme_ipsum_rc()+
    theme(legend.position = "none")+
    theme(axis.title.y = element_blank())+
    theme(panel.grid.minor = element_blank())+
    theme(axis.title.x = element_text(hjust = 1, vjust=3, colour="darkgrey",size=12,face="bold"))

c = ggplot(poblacion_municipio,aes(x=latitud %>% round(1)*(-1),y=pob_2010,fill=indicador)) +
    geom_bar(stat="identity")+
    scale_fill_manual(values=c("dodgerblue","firebrick1"))+
    scale_y_reverse(labels = comma,position = "left")+
    scale_x_continuous(position = "bottom",breaks=seq(-90,-115,-5))+
    labs(y="POPULATION")+
    theme_ipsum_rc()+
    theme(legend.position = "none")+
    theme(axis.title.x = element_blank())+
    theme(panel.grid.minor = element_blank())+
    theme(axis.title.y = element_text(hjust = 0, vjust=3, colour="black",size=12,face="bold"))

ggsave("mapa.svg",plot=a,width = 9,height=6)
ggsave("longitud.svg",plot=b,width = 6,height=6)
ggsave("latitud.svg",plot=c,width = 9,height=6)
