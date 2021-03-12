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

Fechahoy <- "Corte al 11 de marzo de 2021"

POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                   locale = locale(encoding = "ISO-8859-1"))

# Carga base estatal
Sonora.DF <- read_csv("Bases/ST_SonoraInformesCOVID.csv", 
                      col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))
Sonora.DF.hoy <- filter(Sonora.DF, Fecha == Sys.Date())
Sonora.DF.hoy <- select(Sonora.DF.hoy, Hospitalizados, Ambulatorios.Activos, Decesos, Recuperados)
Sonora.DF.hoy <- rename(Sonora.DF.hoy, "Ambulatorios activos"= Ambulatorios.Activos)
Sonora.DF.hoy <- gather(Sonora.DF.hoy, key= Estatus, value= Casos.confirmados) 

# Gráfico Treemap confirmados estatales
Sonora.DF.hoy$label <- c(paste0(Sonora.DF.hoy$Estatus, "\n ",  Sonora.DF.hoy$Casos.confirmados, "\n ", (round((Sonora.DF.hoy$Casos.confirmados/(sum(Sonora.DF.hoy$Casos.confirmados))*100), digits = 1)),"%"))
ggplot(Sonora.DF.hoy, aes(area = Casos.confirmados, fill= Estatus, label= Sonora.DF.hoy$label)) + geom_treemap(color = "white", size=5) +
  scale_fill_manual(values= c("#9BD7D7", "#D175A3", "#FAC090", "#95B3D7")) +
  geom_treemap_text(family = "Lato", colour = "white", place = "topleft",
                    grow = FALSE) +  theme_void() +
  theme(plot.title = (element_text(family = "Lato Black", size = 15, color = "#01A2AC")), rect = element_rect(fill = "transparent"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.key.height = unit (2, "cm"), axis.text = element_blank(),
        legend.text = element_text(family = "Lato Light", size = 6, color = "black"),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Lato Light", size = 8, color = "gray50"),
        axis.title = element_blank()) +
  labs(axis = NULL, y = NULL, x = NULL, title  = NULL,  fill = NULL, caption = NULL)

# Bases municipales
Casos <- read_csv("Bases/Casosdiarios.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1"))
casosacumdia <- filter(Casos,Fecha==Sys.Date())
casosacumdiaorder <- arrange(casosacumdia,CASOS, desc(MUNICIPIO))
casosacumdia2 <- mutate(casosacumdiaorder,id=CVEGEO)


Decesos <- read_csv("Bases/Decesosdiarios.csv", 
                    col_types = cols(DECESOS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"))
decesosacumdia <- filter(Decesos,Fecha==Sys.Date())
decesosacumdiaorder <- arrange(decesosacumdia,DECESOS, desc(MUNICIPIO))
decesosacumdia2 <- mutate(decesosacumdiaorder,id=CVEGEO)
decesosacumdia3 <- rename(decesosacumdia2, NUEVOSD=NUEVOS)

# Movimientos día
casosmov <- filter(casosacumdia2,NUEVOS!=0)
decesosmov <- filter(decesosacumdia3,NUEVOSD!=0)
movtbl<-full_join(casosmov,decesosacumdia3)
movtbl<-full_join(movtbl,decesosmov)
write.csv(movtbl,'ResultadoCSV/movtlb.csv')

# Mapa de movimientos
capa_munison <- readOGR("Shapes", layer="MUNSON")
capa_son <- readOGR("Shapes", layer="ENTSON")
capa_munison_df <- fortify(capa_munison, region="CVEMUN")
capa_munison_casos<- inner_join(capa_munison_df, casosacumdia2, by="id")
capa_munison_casos <- mutate(capa_munison_casos, movimiento=if_else(NUEVOS>0,1,0))

discrete <- c("0" = "#58bcbc", "1" = "#01787e")

Mapa_mov <- ggplot(capa_munison_casos, aes(map_id = id)) +
  geom_polygon(data=capa_son, aes(x=long, y=lat, group=group), 
               fill="transparent", color="black", size=1.5) +
  geom_polygon(data=capa_munison, aes(x=long, y=lat, group=group), 
               fill="gray90", color="white", size=0.3) +
  geom_map_interactive(aes(fill = factor(movimiento)),color = "white",size=0.5, map = capa_munison_df) + 
  scale_fill_manual(values = discrete, breaks= c("0", "1"))+
  theme_void() +
  theme(plot.title = (element_text(family = "Lato Black", size = 20, color = "black")),
        plot.subtitle = (element_text(family = "Lato Light", size = 8, color = "#01787E")),
        plot.margin = margin(0.5, 0.5, 0.25, 0.4, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.key.height = unit (0.5, "cm"), legend.key.width = unit (0.3, "cm"), axis.text = element_blank(),
        legend.text = element_text(family = "Lato", size = 6, color = "black"),
        legend.title = element_text(family = "Lato Black", size = 5, color = "black"),
        plot.caption = element_text(family = "Lato Light", size = 6, color = "gray40"),
        axis.title = element_blank()) +
  labs(y = NULL, x = NULL, title  = NULL, 
       subtitle = NULL,  fill = NULL, 
       caption = NULL)
ggsave("Gráficos/movimientos.png",Mapa_mov, bg = "transparent", height = 25.9, width = 25.9, units = "cm", type = 'cairo')

#Casos trayectoria Promedio vs Acumulado

Casosprom <- Casos %>% group_by(MUNICIPIO) %>% mutate(Casos.media.7d=round(rollmeanr(x=NUEVOS, 7, fill = 0),1)) %>%  filter(CASOS>500)

Casosd <- ggplot(subset(Casosprom, MUNICIPIO %in% c("Hermosillo", "Cajeme", "Nogales", "San Luis Río Colorado", "Navojoa", "Caborca", "Guaymas"))) +
  geom_line(mapping = aes(x = CASOS, y = Casos.media.7d, color= MUNICIPIO, label= MUNICIPIO), size=1.5, alpha=0.6, arrow=arrow(type="open", length=unit(0.20,"cm"))) +
  scale_color_locuszoom() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#01A2AC"),  
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text = element_text(family = "Lato", size =10),
        panel.grid.major = element_line(colour = "white", size= 1), 
        panel.grid.minor = element_line(colour = "white", size=0.2), plot.background = element_rect(fill = "white", color = "black", size = 5),
        axis.title.x = element_text(family = "Lato Light", size = 15, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 15, hjust=1), 
        plot.caption = element_text(family = "Lato", size = 10, color = "#01A2AC"),
        panel.background = element_rect(fill = "gray95"), 
        legend.text = element_text(family = "Lato", size = 12),
        legend.position = "top", legend.justification="left") +
  labs(y = "Casos diarios\n(promedio móvil 7 días, log)", 
       x = "Casos acumulados (log)",legend= NULL, title  = "Casos de Covid-19\nen los municipios de Sonora", 
       subtitle= Fechahoy, caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com") + 
  scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10")

ggsave("Gráficos/casosdacum.png",Casosd, bg = "transparent", height = 25, width = 25, units = "cm", type = 'cairo')

#Decesos trayectoria Promedio vs Acumulado

Decesosprom <- Decesos %>% group_by(MUNICIPIO) %>% mutate(Decesos.media.7d=round(rollmeanr(x=NUEVOS, 7, fill = 0),1)) 

Decesosd <- ggplot(subset(Decesosprom, MUNICIPIO %in% c("Hermosillo", "Cajeme", "Nogales", "San Luis Río Colorado", "Navojoa", "Caborca", "Guaymas"))) +
  geom_line(mapping = aes(x = DECESOS, y = Decesos.media.7d, color= MUNICIPIO), size=1.5, alpha=0.6, arrow=arrow(type="open", length=unit(0.20,"cm"))) +
  scale_color_locuszoom() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#993366"), 
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text = element_text(family = "Lato", size =10),
        panel.grid.major = element_line(colour = "white", size= 1), 
        panel.grid.minor = element_line(colour = "white", size=0.2), 
        plot.background = element_rect(fill = "white", color = "black", size = 5),
        axis.title.x = element_text(family = "Lato Light", size = 15, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 15, hjust=1), plot.caption = element_text(family = "Lato", size = 10, color = "#993366"),
        panel.background = element_rect(fill = "gray95"), legend.text = element_text(family = "Lato", size = 12),
        legend.position = "top", legend.justification="left") +
  labs(y = "Decesos diarios\n(promedio móvil 7 días)", 
       x = "Decesos acumulados",legend= NULL, 
       title  = "Decesos de Covid-19\nen los municipios de Sonora", 
       subtitle= Fechahoy, 
       caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com") + 
  scale_y_continuous (expand = c(0, 0)) + 
  scale_x_continuous (expand = c(0, 0))

ggsave("Gráficos/decesosdacum.png",Decesosd, bg = "transparent", height = 25, width = 25, units = "cm", type = 'cairo')

Sonora.Hosp <-Sonora.DF %>%  
  filter(Fecha >= as.Date('2020-08-05') & Fecha <= Sys.Date())


Gravgraf <- ggplot(Sonora.Hosp) +
  geom_area(aes(x= Fecha, y= Hospitalizados, fill = "Hospitalizados"), color = "#E26B0A", size= 1, alpha=0.75) +
  geom_area(aes(x= Fecha, y= Graves, fill= "Graves")) +
  geom_area(aes(x= Fecha, y= Criticos, fill= "Críticos")) +
  scale_fill_manual(name="", values= c("Hospitalizados" = "#FABF8F", "Graves" = "#E26B0A", "Críticos" = "#974706"  ), 
                    breaks = c("Hospitalizados", "Graves", "Críticos")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(expand=c(0,0)) +
  theme_minimal() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#E26B0A"), 
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text.x = element_text(family = "Lato", size =10),
        panel.grid.major = element_line(colour = "white", size= 1), 
        panel.grid.minor = element_line(colour = "white", size=0.2), 
        plot.background = element_rect(fill = "white", color = "black", size = 5),
        axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), plot.caption = element_text(family = "Lato", size = 10, color = "#E26B0A"),
        panel.background = element_rect(fill = "gray95"), legend.text = element_text(family = "Lato", size = 12),
        legend.position = "top", legend.justification="left") + 
  labs(legend= NULL, 
       title  = "Hospitalizados", 
       subtitle= Fechahoy, 
       caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com")
ggsave("Gráficos/hospitalizados.png",Gravgraf, bg = "transparent", height = 25, width = 25, units = "cm", type = 'cairo')

Casosd2 <- ggplot(subset(Casosprom, MUNICIPIO %in% c("Empalme","Huatabampo", "Etchojoa", "San Ignacio Río Muerto", "Benito Juárez", "Bácum"))) +
  geom_line(mapping = aes(x = CASOS, y = Casos.media.7d, color= MUNICIPIO, label= MUNICIPIO), size=1.5, alpha=0.6, arrow=arrow(type="open", length=unit(0.20,"cm"))) +
  scale_color_locuszoom() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#01A2AC"),  
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text = element_text(family = "Lato", size =10),
        panel.grid.major = element_line(colour = "white", size= 1), 
        panel.grid.minor = element_line(colour = "white", size=0.2), plot.background = element_rect(fill = "white", color = "black", size = 5),
        axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), 
        plot.caption = element_text(family = "Lato", size = 10, color = "#01A2AC"),
        panel.background = element_rect(fill = "gray95"), 
        legend.text = element_text(family = "Lato", size = 12),
        legend.position = "top", legend.justification="left") +
  labs(y = "Casos diarios\n(promedio móvil 7 días)", 
       x = "Casos acumulados",legend= NULL, title  = "Casos de Covid-19\nen los municipios de Sonora", 
       subtitle= Fechahoy, caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com")

ggsave("Gráficos/casosdacum2.png",Casosd2, bg = "transparent", height = 25, width = 25, units = "cm", type = 'cairo')

#Decesos trayectoria Promedio vs Acumulado

Decesosprom <- Decesos %>% group_by(MUNICIPIO) %>% mutate(Decesos.media.7d=round(rollmeanr(x=NUEVOS, 7, fill = 0),1)) 

Decesosd2 <- ggplot(subset(Decesosprom, MUNICIPIO %in% c("Empalme", "Huatabampo", "Etchojoa", "San Ignacio Río Muerto", "Benito Juárez", "Bácum"))) +
  geom_line(mapping = aes(x = DECESOS, y = Decesos.media.7d, color= MUNICIPIO, label= MUNICIPIO), size=1.5, alpha=0.6, arrow=arrow(type="open", length=unit(0.20,"cm"))) +
  scale_color_locuszoom() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#993366"), 
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text = element_text(family = "Lato", size =10),
        panel.grid.major = element_line(colour = "white", size= 1), 
        panel.grid.minor = element_line(colour = "white", size=0.2), 
        plot.background = element_rect(fill = "white", color = "black", size = 5),
        axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), plot.caption = element_text(family = "Lato", size = 10, color = "#993366"),
        panel.background = element_rect(fill = "gray95"), legend.text = element_text(family = "Lato", size = 12),
        legend.position = "top", legend.justification="left") +
  labs(y = "Decesos diarios\n(promedio móvil 7 días)", 
       x = "Decesos acumulados",legend= NULL, 
       title  = "Decesos de Covid-19\nen los municipios de Sonora", 
       subtitle= Fechahoy, 
       caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com") + 
  scale_y_continuous (expand = c(0, 0)) + 
  scale_x_continuous (expand = c(0, 0))

ggsave("Gráficos/decesosdacum2.png",Decesosd2, bg = "transparent", height = 25, width = 25, units = "cm", type = "cairo")

# Mapa incidencia

#discrete <- c("5" = "black", "4" = "#005155","3" = "#01787E","2" = "#01A2AC", "1" = "#58BCBC")


Casossemana <- Casos %>% group_by(MUNICIPIO) %>% 
  mutate(diasemana = weekdays(Fecha), Casossemana = rollsum(NUEVOS, 7, align="right", fill = 0)) %>% 
  filter(diasemana==weekdays(max(as.Date(Fecha)))) %>% 
  left_join(POBMUN, by = "CVEGEO") 
Casossemana <- Casossemana %>% mutate (INCIDENCIA= round((Casossemana*100000)/POB,1))
Casossemana$INCIDENCIA[Casossemana$INCIDENCIA==0] <- NA
# casossempob <- Casossemana %>% 
#   mutate(IS=if_else(INCIDENCIA>(round(quantile(casossempob$INCIDENCIA, 0.90, na.rm=TRUE),0)),5, 
#                     if_else(INCIDENCIA>(round(quantile(casossempob$INCIDENCIA, 0.75, na.rm=TRUE),0)),4, 
#                             if_else(INCIDENCIA>(round(quantile(casossempob$INCIDENCIA, 0.50, na.rm=TRUE),0)),3,
#                                     if_else(INCIDENCIA>(round(quantile(casossempob$INCIDENCIA, 0.25, na.rm=TRUE),0)),2,1))))) %>% 
casossempob <- Casossemana %>% mutate(IS=if_else(INCIDENCIA>=99.9,4, 
                                                 if_else(INCIDENCIA>49.9,3,
                                                         if_else(INCIDENCIA>9.9,2,1)))) %>% 
  filter(Fecha==max(as.Date(Fecha)))
casossempob <- casossempob %>%  mutate(id=CVEGEO)

capa_munison <- readOGR("Shapes", layer="MUNSON")
capa_reg <- readOGR("Shapes", layer="REGSON")
capa_munison_df <- fortify(capa_munison, region="concat")
capa_munison_inci<- inner_join(capa_munison_df, casossempob, by="id")


#discrete <-  rev(carto_pal(5, "Temps"))
discrete <- c("4" = "#CE3F41","3" = "#FFA17B","2" = "#FECF7D", "1" = "#31859C")
subtitulo <- "Casos de covid-19 en los últimos 7 días por 100 mil habitantes\nCorte al 11/03/2021"
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
        legend.key.height = unit (1.2, "cm"), legend.key.width = unit (0.3, "cm"), axis.text = element_blank(),
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

Mapa_incivoid<- ggplot(capa_munison_inci, aes(map_id = id)) +
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
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.key.height = unit (0.5, "cm"), legend.key.width = unit (0.3, "cm"), axis.text = element_blank(),
        legend.text = element_text(family = "Lato", size = 6, color = "black"),
        legend.title = element_text(family = "Lato Black", size = 5, color = "black"),
        plot.caption = element_text(family = "Lato Light", size = 6, color = "gray40"),
        axis.title = element_blank()) +
  labs(y = NULL, x = NULL, title  = NULL, 
       subtitle = NULL,  fill = NULL, 
       caption = NULL)+
  geom_polygon(data=capa_reg, aes(x=long, y=lat, group=group), 
               fill="transparent", color="black", size=0.2)
ggsave("Gráficos/mincidenciav.png",Mapa_incivoid, bg = "transparent", height = 12, width = 12, units = "cm", dpi = 800, type = 'cairo')



# Gráfico estatal
Sonora.DF <- mutate(Sonora.DF, Casos.diarios= Confirmados - lag(Confirmados, default = Confirmados[1], order_by=Fecha))
Sonora.DF <- mutate(Sonora.DF, Decesos.diarios= Decesos - lag(Decesos, default = Decesos[1], order_by=Fecha))
Sonora.DF <- mutate(Sonora.DF, Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1))
Sonora.DF <- mutate(Sonora.DF, Decesos.media.7d=round(rollmeanr(x=Decesos.diarios,7, fill=NA),1))
Sonora.DF <- mutate(Sonora.DF, Pruebas.diarias= Pruebas - lag(Pruebas, default = Pruebas[1]))
Sonora.DF <- mutate(Sonora.DF, Pruebas.media.7d=round(rollmeanr(x=Pruebas.diarias,7, fill=NA),1))
Sonora.DF <- mutate(Sonora.DF, Incidencia= round((Confirmados / 30.74745),2))
Sonora.DF <- mutate(Sonora.DF, Letalidad= round((Decesos / Confirmados)*100,2))
Sonora.DF <- mutate(Sonora.DF, Mortalidad= round((Decesos / 30.74745)*100,2))
Sonora.DF <- mutate(Sonora.DF, Positividad= round((Pruebas / Confirmados)*100,2))
Sonora.DF <- mutate(Sonora.DF, Gravedad= round((Graves / Hospitalizados)*100,1))
Sonora.DF <- mutate(Sonora.DF, IMSS= round((D_IMSS / C_IMSS)*100,1))
Sonora.DF <- mutate(Sonora.DF, SSA= round((D_SSA / C_SSA)*100,1))
Sonora.DF <- mutate(Sonora.DF, ISSSTESON= round((D_ISSSTESON / C_ISSSTESON)*100,1))
Sonora.DF <- mutate(Sonora.DF, ISSSTE= round((D_ISSSTE / C_ISSSTE)*100,1))
Sonora.DF <- mutate(Sonora.DF, SEDENA= round((D_SEDENA / C_SEDENA)*100,1))
Sonora.DF <- mutate(Sonora.DF, SEMAR= round((D_SEMAR / C_SEMAR)*100,1))


# Casos diarios Estatal
CasosSon <- ggplot(Sonora.DF) +
  geom_area(aes(x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=1.5, arrow=arrow(type="open", length=unit(0.20,"cm")))+
  geom_point(aes(x= Fecha, y= Casos.diarios, color = "Casos diarios"), fill= "#01787E", size = 1.7, stroke=0.8, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#58BCBC", "Casos diarios" = "#01787E")) + 
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#01787E", "Casos diarios" = "white")) +
  scale_y_continuous(expand = c(0, 5)) +
  scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  geom_hline(yintercept=94.1, linetype="dashed", color = "red") +
   # geom_segment(aes(x = as.Date("2020-06-01"), y = 450, xend = as.Date("2020-08-01"), yend = 569),
   #            size = 1.5, color = "black",
   #           arrow = arrow(length = unit(0.02, "npc"))) +
    # geom_segment(aes(x = as.Date("2020-12-11"), y = 518, xend = as.Date("2021-01-21"), yend = 589),
    #            size = 1.5, color = "black",
    #            arrow = arrow(length = unit(0.02, "npc"))) +
    # geom_text(aes(x = as.Date("2020-11-15"), y = 455,
    #               label = "22/01/2021\n589 casos"), stat = "unique", family = "Lato Black",
    #           size = 5, color = "black")+
   # geom_text(aes(x = as.Date("2020-05-15"), y = 450,
   #               label = "05/08/2020\n570 casos"), stat = "unique", family = "Lato Black",
   #           size = 5, color = "black")+
   # geom_text(aes(x = as.Date("2020-11-15"), y = 515, 
   #               label = "5 de los últimos\n8 días con más de\n500 casos"), stat = "unique", family = "Lato Black", size = 5, color = "#01787E")+
  geom_curve(aes(x = as.Date("2020-11-01"), y = 400, xend = as.Date("2021-02-27"), yend = 94.1),
            size = 1, color = "black",
           arrow = arrow(length = unit(0.02, "npc"))) +
  geom_text(aes(x = as.Date("2020-11-01"), y = 450,
               label = "Menor promedio móvil\n7 días en más de 10 meses\n94.1 casos diarios\n28/02/2021"), stat = "unique", family = "Lato Black",
           size = 5, color = "black")+

  theme_bw() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#01A2AC"),  
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text = element_text(family = "Lato", size =10),
        plot.background = element_rect(fill = "white", color = "black", size = 5),
        axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), 
        plot.caption = element_text(family = "Lato", size = 10, color = "#01A2AC"),
        legend.text = element_text(family = "Lato", size = 12),
        legend.position = "top",  legend.justification="left") +
  labs(y = "Casos confirmados", 
       x = NULL,legend= NULL, title  = "Casos diarios\n de Covid-19 en Sonora", 
       subtitle= Fechahoy, caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com")
CasosSon

ggsave("Gráficos/Casosdiarios.png",CasosSon, bg = "transparent", height = 25, width = 30, units = "cm", dpi = 400, type = 'cairo')

  
DecesosSon <- ggplot(Sonora.DF) +
  geom_area(aes(x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=1.5, arrow=arrow(type="open", length=unit(0.20,"cm")))+
  geom_point(aes(x= Fecha, y= Decesos.diarios, color = "Decesos diarios"), fill= "#73264D", size = 1.7, stroke=0.8, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Decesos diarios" = "#73264D", "Tendencia promedio móvil 7 días" = "#D075A3")) + 
  scale_color_manual(name="", values= c("Decesos diarios" = "white","Tendencia promedio móvil 7 días" = "#73264D")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,80)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), as.Date("2021-03-20")), date_breaks = "1 month", date_labels = "%B") +
  # geom_curve(aes(x = as.Date("2020-09-04"), y = 64, xend = as.Date("2020-08-14"), yend = 78),
  #            size = 1.5, color = "black",
  #            arrow = arrow(length = unit(0.02, "npc"))) +
  # geom_curve(aes(x = as.Date("2020-12-01"), y = 65, xend = as.Date("2021-01-22"), yend = 76.5),
  #            size = 1.5, color = "black",
  #            arrow = arrow(length = unit(0.02, "npc"))) +
  # geom_text(aes(x = as.Date("2020-11-15"), y = 65,
  #               label = "21/01/2021\n77 decesos"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black")+
  # geom_text(aes(x = as.Date("2020-09-04"), y = 61,
  #               label = "78 decesos\n13/08/2020"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black")+
  #   geom_text(aes(x = as.Date("2020-11-15"), y = 73,
  #                 label = "2da. mayor cantidad\nen lo que va\nde la contingencia"), stat = "unique", family = "Lato Black",
  #             size = 5, color = "#993366")+
theme_bw() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#993366"),  
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text = element_text(family = "Lato", size =10),
        plot.background = element_rect(fill = "white", color = "black", size = 5),
        axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), 
        plot.caption = element_text(family = "Lato", size = 10, color = "#993366"),
        legend.text = element_text(family = "Lato", size = 12),
        legend.position = "top",  legend.justification="left") +
  labs(y = "Decesos confirmados", 
       x = NULL,legend= NULL, title  = "Decesos confirmados diarios\n por Covid-19 en Sonora", 
       subtitle= Fechahoy, caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com")  
  
  # #theme(
  #   #legend.position = "none",
  #   #panel.background = element_rect(fill= "white"),
  #   axis.text.y = element_text(family = "Lato", size = 10, color = "black"),
  #   axis.text.x = element_text(family = "Lato", size = 10, color = "black"),axis.line = element_line(colour = "black"),
  #   axis.title = element_text(family = "Lato", size = 8)) + 
  # labs(y = NULL, x = NULL) 
DecesosSon

ggsave("Gráficos/Decesosdiarios.png",DecesosSon, bg = "transparent", height = 25, width = 30, units = "cm", dpi = 400, type = 'cairo')

Casosconfd <-Casos %>% group_by(MUNICIPIO) %>% 
  rename(Casos.diarios=NUEVOS) %>% 
  mutate(Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1))

CasosMun <- Casosconfd %>% filter(MUNICIPIO=="Cajeme") %>%  ggplot() +
  geom_area(aes(x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=1.5, arrow=arrow(type="open", length=unit(0.20,"cm")))+
  geom_point(aes(x= Fecha, y= Casos.diarios, color = "Casos diarios"), fill= "#01787E", size = 1.7, stroke=0.8, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#58BCBC", "Casos diarios" = "#01787E")) + 
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#01787E", "Casos diarios" = "white")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,210)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), as.Date("2021-03-20")), date_breaks = "1 month", date_labels = "%B") +
  # geom_segment(aes(x = as.Date("2020-07-15"), y = 350, xend = as.Date("2020-07-31"), yend = 394),
  #              size = 1.5, color = "black",
  #              arrow = arrow(length = unit(0.02, "npc"))) +
  # geom_segment(aes(x = as.Date("2020-12-15"), y = 175, xend = as.Date("2021-01-21"), yend = 196),
  #              size = 1.5, color = "black",
  #              arrow = arrow(length = unit(0.02, "npc"))) +
  #geom_segment(aes(x = as.Date("2020-12-03"), y = 315, xend = as.Date("2021-01-13"), yend = 394),
  #            size = 1.5, color = "black",
  #           arrow = arrow(length = unit(0.02, "npc"))) +
  #geom_segment(aes(x = as.Date("2020-12-03"), y = 315, xend = as.Date("2021-01-07"), yend = 378),
  #             size = 1.5, color = "black",
  #             arrow = arrow(length = unit(0.02, "npc"))) +
  # geom_text(aes(x = as.Date("2020-12-01"), y = 175,
  #               label = "21/01/2021\n196 casos"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black")+
  # geom_text(aes(x = as.Date("2020-12-01"), y = 195, label = "3er. día superando\n el máximoen lo que va\nde la contingencia"), stat = "unique", 
  #           family = "Lato Black", size = 5, color = "#01787E")+
  # geom_text(aes(x = as.Date("2020-07-10"), y = 330,
  #               label = "01/08/2020\n396 casos"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black")+
  theme_bw() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#01A2AC"),  
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text = element_text(family = "Lato", size =10),
        plot.background = element_rect(fill = "white", color = "black", size = 5),
        axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), 
        plot.caption = element_text(family = "Lato", size = 10, color = "#01A2AC"),
        legend.text = element_text(family = "Lato", size = 12),
        legend.position = "top",  legend.justification="left") +
  labs(y = "Casos confirmados", 
       x = NULL,legend= NULL, title  = "Casos diarios\n de Covid-19 en Cajeme", 
       subtitle= Fechahoy, caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com")
CasosMun

ggsave("Gráficos/diarioCaj.png",CasosMun, bg = "transparent", height = 25, width = 30, units = "cm", dpi = 400, type = 'cairo')






Casosconfd <-Casos %>% group_by(MUNICIPIO) %>% 
  rename(Casos.diarios=NUEVOS) %>% 
  mutate(Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1))

CasosMun <- Casosconfd %>% filter(MUNICIPIO=="Hermosillo") %>%  ggplot() +
  geom_area(aes(x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=1.5, arrow=arrow(type="open", length=unit(0.20,"cm")))+
  geom_point(aes(x= Fecha, y= Casos.diarios, color = "Casos diarios"), fill= "#01787E", size = 1.7, stroke=0.8, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#58BCBC", "Casos diarios" = "#01787E")) + 
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#01787E", "Casos diarios" = "white")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,400)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), as.Date("2021-03-20")), date_breaks = "1 month", date_labels = "%B") +
    # geom_segment(aes(x = as.Date("2020-07-15"), y = 350, xend = as.Date("2020-07-31"), yend = 394),
    #            size = 1.5, color = "black",
    #           arrow = arrow(length = unit(0.02, "npc"))) +
    # geom_segment(aes(x = as.Date("2020-12-03"), y = 315, xend = as.Date("2021-01-14"), yend = 359),
    #           size = 1.5, color = "black",
    #            arrow = arrow(length = unit(0.02, "npc"))) +
  #geom_segment(aes(x = as.Date("2020-12-03"), y = 315, xend = as.Date("2021-01-13"), yend = 394),
   #            size = 1.5, color = "black",
    #           arrow = arrow(length = unit(0.02, "npc"))) +
  #geom_segment(aes(x = as.Date("2020-12-03"), y = 315, xend = as.Date("2021-01-07"), yend = 378),
  #             size = 1.5, color = "black",
  #             arrow = arrow(length = unit(0.02, "npc"))) +
   #  geom_text(aes(x = as.Date("2020-11-15"), y = 310,
   #                label = "16/01/2021\n359 casos"), stat = "unique", family = "Lato Black",
   #            size = 5, color = "black")+
   # geom_text(aes(x = as.Date("2020-11-15"), y = 350, label = "3era. vez\nen 10 días con más\nde 300 casos"), stat = "unique", 
   #           family = "Lato Black", size = 5, color = "#01787E")+
   #  geom_text(aes(x = as.Date("2020-07-10"), y = 330,
   #                label = "01/08/2020\n396 casos"), stat = "unique", family = "Lato Black",
   #            size = 5, color = "black")+
  theme_bw() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#01A2AC"),  
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text = element_text(family = "Lato", size =10),
        plot.background = element_rect(fill = "white", color = "black", size = 5),
        axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), 
        plot.caption = element_text(family = "Lato", size = 10, color = "#01A2AC"),
        legend.text = element_text(family = "Lato", size = 12),
        legend.position = "top",  legend.justification="left") +
  labs(y = "Casos confirmados", 
       x = NULL,legend= NULL, title  = "Casos diarios\n de Covid-19 en Hermosillo", 
       subtitle= Fechahoy, caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com")
CasosMun

ggsave("Gráficos/diarioMun2.png",CasosMun, bg = "transparent", height = 25, width = 30, units = "cm", dpi = 400, type = 'cairo')

# DEcesos Hermosillo
Decesosconfd <-Decesos %>% group_by(MUNICIPIO) %>% 
  rename(Decesos.diarios=NUEVOS) %>% 
  mutate(Decesos.media.7d=round(rollmeanr(x=Decesos.diarios, 7, fill = NA),1))

DecesosSon <- Decesosconfd %>% filter(MUNICIPIO=="Hermosillo") %>% ggplot() +
  geom_area(aes(x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=1.5, arrow=arrow(type="open", length=unit(0.20,"cm")))+
  geom_point(aes(x= Fecha, y= Decesos.diarios, color = "Decesos diarios"), fill= "#73264D", size = 1.7, stroke=0.8, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Decesos diarios" = "#73264D", "Tendencia promedio móvil 7 días" = "#D075A3")) + 
  scale_color_manual(name="", values= c("Decesos diarios" = "white","Tendencia promedio móvil 7 días" = "#73264D")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,50)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), as.Date("2021-03-20")), date_breaks = "1 month", date_labels = "%B") +
  #geom_curve(aes(x = as.Date("2020-11-11"), y = 42, xend = as.Date("2020-08-14"), yend = 49),
              #size = 1.5, color = "black",
              #arrow = arrow(length = unit(0.02, "npc"))) +
   #  geom_segment(aes(x = as.Date("2020-12-15"), y = 40, xend = as.Date("2021-01-15"), yend = 47),
   #             size = 1.5, color = "black",
   #            arrow = arrow(length = unit(0.02, "npc"))) +
   #  geom_text(aes(x = as.Date("2020-11-28"), y = 40,
   #               label = "16/01/2021\n47 decesos"), stat = "unique", family = "Lato Black",
   #            size = 5, color = "black")+
   # geom_text(aes(x = as.Date("2020-08-30"), y = 61,
   #               label = "78 decesos\n13/08/2020"), stat = "unique", family = "Lato Black",
   #           size = 5, color = "black")+
   #  geom_text(aes(x = as.Date("2020-11-28"), y = 45,
   #                label = "2da. mayor cantidad\nen lo que va\nde la contingencia"), stat = "unique", family = "Lato Black",
   #            size = 5, color = "#993366")+
  theme_bw() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#993366"),  
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text = element_text(family = "Lato", size =10),
        plot.background = element_rect(fill = "white", color = "black", size = 5),
        axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), 
        plot.caption = element_text(family = "Lato", size = 10, color = "#993366"),
        legend.text = element_text(family = "Lato", size = 12),
        legend.position = "top",  legend.justification="left") +
  labs(y = "Decesos confirmados", 
       x = NULL,legend= NULL, title  = "Decesos diarios\n de Covid-19 en Hermosillo", 
       subtitle= Fechahoy, caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com")  

# #theme(
#   #legend.position = "none",
#   #panel.background = element_rect(fill= "white"),
#   axis.text.y = element_text(family = "Lato", size = 10, color = "black"),
#   axis.text.x = element_text(family = "Lato", size = 10, color = "black"),axis.line = element_line(colour = "black"),
#   axis.title = element_text(family = "Lato", size = 8)) + 
# labs(y = NULL, x = NULL) 
DecesosSon

ggsave("Gráficos/DecesosdiariosH.png",DecesosSon, bg = "transparent", height = 25, width = 30, units = "cm", dpi = 400, type = 'cairo')

