# Paquetes

library(tidyverse)
library(extrafont)
library(scales)
library(plotly)
library(htmlwidgets)
library(showtext)
library(tint)
library(rgdal)
library(rgeos)
library(ggiraph)
library(miniUI)
library(units)
library(reactable)
library(zoo)
library(lubridate)
library(treemapify)
library(wesanderson)
library(ggsci)
library("Cairo")
library(gganimate)
library(ggsci)
#library(wesanderson)
#library(ggsci)
#library(RColorBrewer)
library(rcartocolor)
#library(NineteenEightyR)

Fechahoy<- "Corte al 12 de febrero de 2021"
capa_munison <- readOGR("Shapes", layer="MUNSON")
capa_son <- readOGR("Shapes", layer="ENTSON")

Casos <- read_csv("Bases/Casosdiarios.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1"))
POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                   locale = locale(encoding = "ISO-8859-1"))

# Mapa incidencia


Casossemana <- Casos %>% group_by(MUNICIPIO) %>% 
  mutate(diasemana = weekdays(Fecha), Casossemana = rollsum(NUEVOS, 7, align="right", fill = 0)) %>% 
  filter(diasemana=="domingo") %>% 
  left_join(POBMUN, by = "CVEGEO") 
Casossemana <- Casossemana %>% mutate (INCIDENCIA= round((Casossemana*100000)/POB,1))
Casossemana$INCIDENCIA[Casossemana$INCIDENCIA==0] <- NA
# Muyalto <- quantile(Casossemana$INCIDENCIA, 0.90, na.rm=TRUE)
# Alto <- quantile(Casossemana$INCIDENCIA, 0.75, na.rm=TRUE)
# Medio <- quantile(Casossemana$INCIDENCIA, 0.50, na.rm=TRUE)
# Bajo <- quantile(Casossemana$INCIDENCIA, 0.25, na.rm=TRUE)
# casossempob <- Casossemana %>% mutate(IS=if_else(INCIDENCIA>(round(Muyalto,0)),5, 
#                                                  if_else(INCIDENCIA>(round(Alto,0)),4, 
#                                                          if_else(INCIDENCIA>(round(Medio,0)),3,
#                                                                  if_else(INCIDENCIA>(round(Bajo,0)),2,1)))))
casossempob <- Casossemana %>% mutate(IS=if_else(INCIDENCIA>=100,4, 
                                                 if_else(INCIDENCIA>=50,3,
                                                         if_else(INCIDENCIA>=10,2,1)))) 
casossempob <- casossempob %>%  mutate(id=CVEGEO)

capa_munison <- readOGR("Shapes", layer="MUNSON")
capa_reg <- readOGR("Shapes", layer="REGSON")
capa_munison_df <- fortify(capa_munison, region="concat")
capa_munison_inci<- inner_join(capa_munison_df, casossempob, by="id")


discrete <- c("4" = "#CE3F41","3" = "#FFA17B","2" = "#FECF7D", "1" = "#31859C")
subtitulo <- "Casos de covid-19 en los últimos 7 días por 100 mil habitantes\nCorte al 12/02/2021"
marcas <- c( "Alta\n(100 o más)", "Substancial\n(50-99)", "Moderada\n(10-49)","Baja\n(+0-9)")
romp <- c("4", "3", "2", "1")

Mapa_incidencia<- ggplot(capa_munison_inci, aes(map_id = id)) +
  geom_polygon(data=capa_munison, aes(x=long, y=lat, group=group), 
               fill="gray90", color="white", size=0.12) +
  geom_map(aes(fill = factor(IS)),color = "white",size=0.22, map = capa_munison_df) + 
  scale_fill_manual(values = discrete, 
                    breaks= romp, 
                    labels = marcas) +
  theme_void() +
  theme(plot.title = (element_text(family = "Lato Black", size = 20, color = "black")),
        plot.subtitle = (element_text(family = "Lato Light", size = 8, color = "#01787E")),
        plot.margin = margin(0.5, 0.5, 0.25, 0.4, "cm"),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color="black", size=3),
        legend.key.height = unit (0.5, "cm"), legend.key.width = unit (0.2, "cm"), axis.text = element_blank(),
        legend.text = element_text(family = "Lato", size = 6, color = "black"),
        legend.title = element_text(family = "Lato Black", size = 5, color = "black"),
        plot.caption = element_text(family = "Lato Light", size = 6.5, color = "gray40"),
        axis.title = element_blank()) +
  labs(y = NULL, x = NULL, title  = "Incidencia semanal", 
       subtitle = subtitulo,  fill = NULL, 
       caption ="Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Estado de Sonora")+
  geom_polygon(data=capa_reg, aes(x=long, y=lat, group=group), 
               fill="transparent", color="black", size=0.2)
ggsave("Gráficos/mincidencia.png",Mapa_incidencia, bg = "transparent", height = 12, width = 12, units = "cm", dpi = 800, type = 'cairo')



diacasossemana <- seq(min(casossempob$Fecha), max(casossempob$Fecha),1)
SonoraMCsemanal  <- filter(casossempob,Fecha %in% diacasossemana)

capa_munison_df <- fortify(capa_munison, region="concat")
capa_munison_casos<- inner_join(capa_munison_df, SonoraMCsemanal, by="id")


Mapa_inci <- function(capa_son, capa_munison_casos) { ggplot(capa_munison_casos, aes(map_id = id)) +
        geom_polygon(data=capa_munison, aes(x=long, y=lat, group=group), 
                 fill="gray90", color="white", size=1) +
        geom_map(aes(fill = as.factor(IS)),color = "white",size=1, map = capa_munison_df) + 
        geom_polygon(data=capa_son, aes(x=long, y=lat, group=group), 
                 fill="transparent", color="black", size=1) +
    scale_fill_manual(values = discrete, breaks= romp, 
                      labels = marcas)+
    
    theme_void() +
    theme(plot.title = (element_text(family = "Lato Black", size = 54, color = "black")),
          plot.subtitle = (element_text(family = "Lato Light", size = 22, color = "#01787E")),
          plot.margin = margin(1, 1, 1, 1.5, "cm"),
          legend.position = c(0.18,0.4),
          plot.background = element_rect(fill = "white", color="black", size=3),
          legend.key.height = unit (3, "cm"), legend.key.width = unit (0.75, "cm"), axis.text = element_blank(),
          legend.text = element_text(family = "Lato", size = 20, color = "black"),
          legend.title = element_text(family = "Lato Black", size = 28, color = "black"),
          plot.caption = element_text(family = "Lato Light", size = 20, color = "gray40"),
          axis.title = element_blank()) +
    labs(axis = NULL, y = NULL, x = NULL, title = "Incidencia semanal", subtitle = "Casos de covid-19 en los 7 días anteriores por 100 mil habitantes",
         caption ="Elaboración Luis Armando Moreno con información de la Secretaría de Salud estatal")
}


Incisemanaanim <- Mapa_inci(capa_son, capa_munison_casos) + 
  transition_manual(Fecha) +
  shadow_mark() +
  labs(fill = "Viernes \n {current_frame}")

gifincisem <- animate(Incisemanaanim, end_pause = 6, fps = 20,duration = 30, width = 950, height =950, renderer = gifski_renderer())
anim_save("./Gráficos/Incidenciasemanal.gif", animation=gifincisem)
   
