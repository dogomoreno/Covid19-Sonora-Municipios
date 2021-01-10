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

Fechahoy<- "Corte al 09 de enero de 2021"

Casosprom <- Casos %>% group_by(MUNICIPIO) %>% mutate(Casos.media.7d=round(rollmeanr(x=NUEVOS, 7, fill = 0),1)) 

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
  scale_y_continuous (expand = c(0, 0), limit = c(0, 50)) + 
  scale_x_continuous (expand = c(0, 0), limit = c(0, 1400))

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
  scale_y_continuous (expand = c(0, 0), limit = c(0, 10)) + 
  scale_x_continuous (expand = c(0, 0), limit = c(0, 75))

ggsave("Gráficos/decesosdacum2.png",Decesosd2, bg = "transparent", height = 25, width = 25, units = "cm", type = "cairo")

# Mapa incidencia

discrete <- c("5" = "black", "4" = "#005155","3" = "#01787E","2" = "#01A2AC", "1" = "#58BCBC")


Casossemana <- Casos %>% group_by(MUNICIPIO) %>% 
  mutate(diasemana = weekdays(Fecha), 'CASOS SEMANALES' = rollsum(NUEVOS, 7, align="right", fill = 0)) %>% 
  filter(diasemana==weekdays(Sys.Date())) %>% 
  left_join(POBMUN, by = "CVEGEO") 
Casossemana <- Casossemana %>% mutate (INCIDENCIA= round((Casossemana*100000)/POB,1))
Casossemana$INCIDENCIASEM[Casossemana$INCIDENCIASEM==0] <- NA
casossempob <- Casossemana %>% 
  mutate(IS=if_else(INCIDENCIA>162,5, if_else(INCIDENCIASEM>59,4, if_else(INCIDENCIASEM>30,3,if_else(INCIDENCIASEM>15,2,1))))) %>% filter(Fecha==Sys.Date())
casossempob <- casossempob %>%  mutate(id=CVEGEO)

capa_munison <- readOGR("Shapes", layer="MUNSON")
capa_reg <- readOGR("Shapes", layer="REGSON")
capa_munison_df <- fortify(capa_munison, region="concat")
capa_munison_inci<- inner_join(capa_munison_df, casossempob, by="id")


#discrete <-  rev(carto_pal(5, "Temps"))
subtitulo <- "Casos de covid-19 por 100 mil habitantes\nCorte al 04/01/2021 | Semana 2021-1"
marcas <- c( "+162", "59-162", "30-59","15-30", "0-15")

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
  geom_area(aes(x= Fecha, y= Casos.media.7d, fill= "Promedio móvil 7d"), alpha=0.3)+
  geom_line(aes(x= Fecha, y= Casos.media.7d, color= "Promedio móvil 7d"), linetype= "solid", size=1.5)+
  geom_point(aes(x= Fecha, y= Casos.diarios), fill= "#01787E", color = "white", size = 1.7, stroke=0.8, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Promedio móvil 7d" = "#58BCBC", "Casos diarios" = "#01787E")) + 
  scale_color_manual(name="", values= c("Promedio móvil 7d" = "#01787E")) +
  scale_y_continuous(expand = c(0, 5)) +
  scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  # geom_curve(aes(x = as.Date("2020-08-27"), y = 510, xend = as.Date("2020-08-06"), yend = 518),
  #            size = 1.5, color = "black",
  # #            arrow = arrow(length = unit(0.02, "npc"))) +
  #  geom_curve(aes(x = as.Date("2020-11-01"), y = 400, xend = as.Date("2021-01-06"), yend = 487),
  #             size = 1.5, color = "black",
  #             arrow = arrow(length = unit(0.02, "npc"))) +
  #  geom_text(aes(x = as.Date("2020-10-15"), y = 420,
  #                label = "07/01/2021\n492 casos"), stat = "unique", family = "Lato Black",
  #            size = 5, color = "black")+
  # geom_text(aes(x = as.Date("2020-09-08"), y = 480,
  #               label = "05/08/2020\n518 casos"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black")+
  # geom_text(aes(x = as.Date("2020-10-15"), y = 470, 
  #               label = "Mayor cantidad en\nmás de 5 meses"), stat = "unique", family = "Lato Black", size = 5, color = "#01787E")+
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
        legend.text = element_blank(),
        legend.position = "none") +
  labs(y = "Casos confirmados", 
       x = NULL,legend= NULL, title  = "Casos diarios\n de Covid-19 en Sonora", 
       subtitle= Fechahoy, caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com")
CasosSon

ggsave("Gráficos/Casosdiarios.png",CasosSon, bg = "transparent", height = 25, width = 30, units = "cm", dpi = 400, type = 'cairo')

  
DecesosSon <- ggplot(Sonora.DF) +
  geom_area(aes(x= Fecha, y= Decesos.media.7d, fill= "Promedio móvil 7d"), alpha=0.3)+
  geom_line(aes(x= Fecha, y= Decesos.media.7d, color= "Promedio móvil 7d"), linetype= "solid", size=1.5)+
  geom_point(aes(x= Fecha, y= Decesos.diarios, fill= "Decesos diarios"), color = "white", size = 1.7, stroke=0.8, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Decesos diarios" = "#73264D", "Promedio móvil 7d" = "#D075A3")) + 
  scale_color_manual(name="", values= c("Promedio móvil 7d" = "#73264D")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,80)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), as.Date("2021-01-10")), date_breaks = "1 month", date_labels = "%B") +
# geom_curve(aes(x = as.Date("2020-09-04"), y = 64, xend = as.Date("2020-08-14"), yend = 78),
#            size = 1.5, color = "black",
#            arrow = arrow(length = unit(0.02, "npc"))) +
 # geom_curve(aes(x = as.Date("2020-11-11"), y = 40, xend = as.Date("2021-01-05"), yend = 17),
 #            size = 1.5, color = "black",
 #            arrow = arrow(length = unit(0.02, "npc"))) +
 # geom_text(aes(x = as.Date("2020-11-11"), y = 43,
 #               label = "07/01/2021\n17 decesos"), stat = "unique", family = "Lato Black",
 #           size = 5, color = "black")+
# geom_text(aes(x = as.Date("2020-08-30"), y = 61,
#               label = "78 decesos\n13/08/2020"), stat = "unique", family = "Lato Black",
#           size = 5, color = "black")+
  # geom_text(aes(x = as.Date("2020-11-20"), y = 65,
  #               label = "Mayor cantidad\nen 121 días"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "#993366")+
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
        legend.text = element_blank(),
        legend.position = "none") +
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

CasosMun <- Casosconfd %>% filter(MUNICIPIO=="Hermosillo") %>%  ggplot() +
  geom_area(aes(x= Fecha, y= Casos.media.7d, fill= "Promedio móvil 7d"), alpha=0.3)+
  geom_line(aes(x= Fecha, y= Casos.media.7d, color= "Promedio móvil 7d"), linetype= "solid", size=1.5)+
  geom_point(aes(x= Fecha, y= Casos.diarios), fill= "#01787E", color = "white", size = 1.7, stroke=0.8, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Promedio móvil 7d" = "#58BCBC", "Casos diarios" = "#01787E")) + 
  scale_color_manual(name="", values= c("Promedio móvil 7d" = "#01787E")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,400)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), as.Date("2021-01-10")), date_breaks = "1 month", date_labels = "%B") +
  #  geom_segment(aes(x = as.Date("2020-07-15"), y = 350, xend = as.Date("2020-07-31"), yend = 394),
  #             size = 1.5, color = "black",
  #             arrow = arrow(length = unit(0.02, "npc"))) +
  #  geom_segment(aes(x = as.Date("2020-12-03"), y = 315, xend = as.Date("2021-01-06"), yend = 376),
  #             size = 1.5, color = "black",
  #             arrow = arrow(length = unit(0.02, "npc"))) +
  #  geom_text(aes(x = as.Date("2020-11-15"), y = 310,
  #                label = "07/01/2021\n378 casos"), stat = "unique", family = "Lato Black",
  #            size = 5, color = "black")+
  # geom_text(aes(x = as.Date("2020-11-15"), y = 350, label = "2da. mayor cantidad\nen lo que va de\n la contingencia"), stat = "unique", 
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
        legend.text = element_blank(),
        legend.position = "none") +
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
  geom_area(aes(x= Fecha, y= Decesos.media.7d, fill= "Promedio móvil 7d"), alpha=0.3)+
  geom_line(aes(x= Fecha, y= Decesos.media.7d, color= "Promedio móvil 7d"), linetype= "solid", size=1.5)+
  geom_point(aes(x= Fecha, y= Decesos.diarios, fill= "Decesos diarios"), color = "white", size = 1.7, stroke=0.8, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Decesos diarios" = "#73264D", "Promedio móvil 7d" = "#D075A3")) + 
  scale_color_manual(name="", values= c("Promedio móvil 7d" = "#73264D")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,50)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), as.Date("2021-01-10")), date_breaks = "1 month", date_labels = "%B") +
   #geom_curve(aes(x = as.Date("2020-11-11"), y = 42, xend = as.Date("2020-08-14"), yend = 49),
              #size = 1.5, color = "black",
              #arrow = arrow(length = unit(0.02, "npc"))) +
   # geom_curve(aes(x = as.Date("2020-11-01"), y = 20, xend = as.Date("2021-01-05"), yend = 11),
   #            size = 1.5, color = "black",
   #            arrow = arrow(length = unit(0.02, "npc"))) +
   # geom_text(aes(x = as.Date("2020-11-01"), y = 22,
   #               label = "07/01/2021\n11 decesos"), stat = "unique", family = "Lato Black",
   #           size = 5, color = "black")+
  # geom_text(aes(x = as.Date("2020-08-30"), y = 61,
  #               label = "78 decesos\n13/08/2020"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black")+
   # geom_text(aes(x = as.Date("2020-11-11"), y = 38,
   #               label = "Iguala la mayor cantidad\nen 146 días"), stat = "unique", family = "Lato Black",
   #           size = 5, color = "#993366")+
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
        legend.text = element_blank(),
        legend.position = "none") +
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

