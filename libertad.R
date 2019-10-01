install.packages("readr")
libertad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-14/libertad.csv")

install.packages("maps")
install.packages("rworldmap")
library(rworldmap)
library(maps)
library(sp)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(sf)
library(grid)
library(gridExtra)
library(ggpubr)
library(ggrepel)
install.packages("extrafont")
library(extrafont)
install.packages("countrycode")
library(countrycode)
library(viridis)
library(viridisLite)


#Paso para crear grafica

country=countrycode::codelist

country=country%>%select(genc3c,country.name.en)

world_map <- map_data("world")
world_map$region[world_map$subregion=='Portugal']='Portugal'

data_pais=libertad%>%
  left_join(country, by=c('codigo_iso'='genc3c'))%>%
  left_join(world_map, by=c('country.name.en'='region'))%>%
  filter(anio==max(libertad$anio))

#seleccionar los paises menor libertad

menor_lib=libertad%>%filter(anio==2016)%>%top_n(-10,libertad_humana_puntaje)%>%
  select(pais,libertad_humana_puntaje)%>%
  arrange(libertad_humana_puntaje)

#seleccionar los paises mayor libertad
mayor_lib=libertad%>%filter(anio==2016)%>%top_n(10,libertad_humana_puntaje)%>%
  select(pais,libertad_humana_puntaje)%>%
  arrange(libertad_humana_puntaje)

# etiqueta de paises en mapa
data_p_menos=data_pais%>%
  filter(pais %in% menor_lib$pais)%>%group_by(pais)%>%
  summarise(lat=mean(lat,na.rm = T),
            long=mean(long,na.rm = T))%>%
  left_join(menor_lib)

data_p_mayor=data_pais%>%
  filter(pais %in% mayor_lib$pais)%>%group_by(pais)%>%
  summarise(lat=mean(lat,na.rm = T),
            long=mean(long,na.rm = T))%>%
  left_join(mayor_lib)

#combinacion de datos 
df_leyenda=rbind(data_p_mayor,data_p_menos)

#Evaluar cual es el maximo y minimo de la base de datos_pais
vmax <- max(data_pais$libertad_humana_puntaje, na.rm=T)
vmin <- min(data_pais$libertad_humana_puntaje, na.rm=T)

#Crear imagen para incoporar en mapa

imgage <- png::readPNG('/home/mirefresa/Escritorio/Proyecto R/Indice de Libertad/ekg-2753763_960_720.png')

#Graficar mapa

ggplot() +  
  annotation_custom(rasterGrob(imgage, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                              -Inf, Inf, -Inf, Inf)+
  geom_polygon(data=world_map, aes(x = long, y = lat, group = group),fill='Bisque')+
  geom_polygon(data=data_pais, aes(x = long, y = lat, group = group,fill=libertad_humana_puntaje))+
  scale_fill_viridis(option=('B'),name="Puntaje Libertad Humana",
                     begin = 0, end = 1,
                     limits = c(vmin,vmax), na.value="gray99")+ 
  geom_label_repel(aes(label = paste(pais,'\n',libertad_humana_puntaje),
                       x=long,
                       y=lat),
                   data = data_p_menos,  
                   size = 2.5,  fill ='#330066',
                   family="Babel Sans" ,
                   color='white',fontface = 'bold',
                   box.padding = unit(0.2, "lines"),
                   point.padding = unit(0.2, "lines"))+
  geom_label_repel(aes(label = paste(pais,'\n',libertad_humana_puntaje),
                       x=long,
                       y=lat),
                   data = data_p_mayor,  
                   size = 2.5,  fill ='#e8fa5bff',
                   family="Babel Sans" ,
                   color='black',fontface = 'bold',
                   box.padding = unit(0.2, "lines"),
                   point.padding = unit(0.2, "lines"))+ 
  labs(title = 'Indice de Libertad')+
       labs(caption='datos año: 2016')+
  theme_void()+
  theme(
    legend.background = element_rect(fill='#7d7f7d'),
    legend.text = element_text(color='white'),
    plot.background = element_rect(fill='#7d7f7d',color ='#7d7f7d' ),
    panel.background =element_rect(fill='#7d7f7d'),
    legend.position="bottom",
    legend.key = element_rect(fill = "#7d7f7d", color = NA),
    legend.title = element_text(color = "white", size = 10, hjust = 0.3),
    plot.title = element_text(family =     "Mr Bedfort"    ,
                              hjust = 0.5,
                              size=20,colour = '#453781FF',
                              face = 'bold'),
    plot.caption = element_text(family =     "Atma Light"     ,
                                hjust =0.8,
                                size=10, face = 'bold',
                                colour = '#453781FF' )
  )
  







