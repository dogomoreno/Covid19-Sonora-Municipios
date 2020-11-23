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
capa_munison_df <- fortify(capa_munison, region="CVEGEO")
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
  theme(plot.title = (element_text(family = "Lato Black", size = 15, color = "#01A2AC")), rect = element_rect(fill = "transparent"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.key.height = unit (2, "cm"), axis.text = element_blank(),
        legend.text = element_text(family = "Lato Light", size = 6, color = "black"),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Lato Light", size = 8, color = "gray50"),
        axis.title = element_blank()) +
  labs(axis = NULL, y = NULL, x = NULL, title  = NULL,  fill = NULL, caption = NULL)
ggsave("Gráficos/movimientos.png",Mapa_mov, bg = "transparent", height = 25.9, width = 25.9, units = "cm", type = 'cairo')

#Casos trayectoria Promedio vs Acumulado

Fechahoy<- "Corte al 22 de noviembre de 2020"

Casosprom <- Casos %>% group_by(MUNICIPIO) %>% mutate(Casos.media.7d=round(rollmeanr(x=NUEVOS, 7, fill = 0),1)) 

Casosd <- ggplot(subset(Casosprom, MUNICIPIO %in% c("Hermosillo", "Cajeme", "Nogales", "San Luis Río Colorado", "Navojoa", "Caborca", "Guaymas", "Cananea"))) +
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
        axis.title.x = element_text(family = "Lato Light", size = 15, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 15, hjust=1), plot.caption = element_text(family = "Lato", size = 10, color = "#993366"),
        panel.background = element_rect(fill = "gray95"), legend.text = element_text(family = "Lato", size = 12),
        legend.position = "top", legend.justification="left") +
  labs(y = "Decesos diarios\n(promedio móvil 7 días)", 
       x = "Decesos acumulados",legend= NULL, 
       title  = "Decesos de Covid-19\nen los municipios de Sonora", 
       subtitle= Fechahoy, 
       caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com") + 
  scale_y_continuous (expand = c(0, 0), limit = c(0, 30)) + 
  scale_x_continuous (expand = c(0, 0), limit = c(0, 1000))

ggsave("Gráficos/decesosdacum.png",Decesosd, bg = "transparent", height = 25, width = 25, units = "cm", type = 'cairo')

Gravgraf <- ggplot(Sonora.DF) +
  geom_area(aes(x= Fecha, y= Hospitalizados, fill = "Hospitalizados"), color = "#E26B0A", size= 1, alpha=0.75) +
  geom_area(aes(x= Fecha, y= Graves, fill= "Graves")) +
  geom_area(aes(x= Fecha, y= Intubado, fill= "Críticos")) +
  scale_fill_manual(name="", values= c("Hospitalizados" = "#FABF8F", "Graves" = "#E26B0A", "Críticos" = "#974706"  ), 
                    breaks = c("Hospitalizados", "Graves", "Críticos")) +
  scale_y_continuous(expand = c(0, 40)) +
  theme_minimal() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#E26B0A"), 
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text = element_text(family = "Lato", size =10),
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
  scale_y_continuous (expand = c(0, 0), limit = c(0, 3)) + 
  scale_x_continuous (expand = c(0, 0), limit = c(0, 50))

ggsave("Gráficos/decesosdacum2.png",Decesosd2, bg = "transparent", height = 25, width = 25, units = "cm", type = "cairo")


#discrete <- c("5" = "black", "4" = "#005155","3" = "#01787E","2" = "#01A2AC", "1" = "#58BCBC")

Casossemana <- Casos %>% mutate(Semana = isoweek(Fecha)) %>% group_by(Semana) %>% 
  mutate (Reporte=max(as.Date(Fecha))) %>% ungroup()
Casossem <- group_by(Casossemana, CVEGEO, MUNICIPIO, Reporte, Semana) %>% 
  summarise('CASOS SEMANALES' = sum(NUEVOS), ACUMULADOS=max(CASOS)) 
casossempob <- left_join(Casossem, POBMUN, by = "CVEGEO") 
casossempob  <- casossempob %>% mutate (INCIDENCIA= round((`CASOS SEMANALES`*100000)/POB,1))
casossempob$INCIDENCIA[casossempob$INCIDENCIA==0] <- NA
casossempob  <- casossempob %>% filter(Semana==47)
casossempob   <- mutate(casossempob , IS=if_else(INCIDENCIA>143,5, if_else(INCIDENCIA>59,4, if_else(INCIDENCIA>30,3,if_else(INCIDENCIA>15,2,1)))))
casossempob <-casossempob %>%  mutate(id=CVEGEO)

capa_munison <- readOGR("Shapes", layer="MUNSON")
capa_reg <- readOGR("Shapes", layer="REGSON")
capa_munison_df <- fortify(capa_munison, region="concat")
capa_munison_inci<- inner_join(capa_munison_df, casossempob, by="id")


discrete <-  rev(carto_pal(5, "Temps"))
marcas <- c( "+143", "59-143", "30-59","15-30", "0-15")
subtitulo <- "Casos de covid-19 por 100 mil habitantes\nCorte al 22/11/2020 | Semana 46"
marcas <- c( "+143", "59-143", "30-59","15-30", "0-15")

Mapa_incidencia<- ggplot(capa_munison_inci, aes(map_id = id)) +
  geom_polygon(data=capa_munison, aes(x=long, y=lat, group=group), 
               fill="gray90", color="white", size=0.12) +
  geom_map(aes(fill = factor(IS)),color = "white",size=0.22, map = capa_munison_df) + 
  scale_fill_manual(values = discrete, 
                    breaks= c("5", "4", "3", "2", "1"), 
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
        plot.caption = element_text(family = "Lato Light", size = 5, color = "gray40"),
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
                    breaks= c("5", "4", "3", "2", "1"), 
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
        plot.caption = element_text(family = "Lato Light", size = 5, color = "gray40"),
        axis.title = element_blank()) +
  labs(y = NULL, x = NULL, title  = NULL, 
       subtitle = NULL,  fill = NULL, 
       caption = NULL)+
  geom_polygon(data=capa_reg, aes(x=long, y=lat, group=group), 
               fill="transparent", color="black", size=0.2)
ggsave("Gráficos/mincidenciav.png",Mapa_incivoid, bg = "transparent", height = 12, width = 12, units = "cm", dpi = 800, type = 'cairo')

