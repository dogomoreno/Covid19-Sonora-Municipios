library(tidyverse)
library(extrafont)
library(scales)
library(showtext)
library(tint)
library(miniUI)
library(units)
library(lubridate)
library(zoo)
library(Cairo)
#library(wesanderson)
#library(ggsci)
#library(RColorBrewer)
library(rcartocolor)
#library(NineteenEightyR)


#discreta <- c("5" = "#893F59", "4" = "#ECA48E","3" = "#FECF7D","2" = "#9BD7D7", "1" = "gray80")
#discreta <- c("5" = "black", "4" = "#005155","3" = "#01787E","2" = "#01A2AC", "1" = "#58BCBC")
#discreta <- wes_palette("Zissou1", 5, type = "discrete")
#discreta <- pal_ucscgb("default", alpha=0.5)(5)
#discreta <- brewer.pal(5, "Spectral")
discreta <- rev(carto_pal(5, "Temps"))
#discreta <- miami1(n = 5, alpha = 0.9)

Casos <- read_csv("Bases/Casosdiarios.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1"))
POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                   locale = locale(encoding = "ISO-8859-1"))

#Estilo del gráfico
paragraf <- theme(plot.title = (element_text(family = "Lato Black", size = 32, color = "black")),
                  plot.subtitle = (element_text(family = "Lato Light", size = 12, color = "#01787E")),
                  legend.key.height = unit (1, "cm"), legend.position = "right",   
                  axis.text.y = element_text(family = "Lato Light", size = 15, color = "black"), 
                  axis.text.x = element_text(family = "Lato Light", size =11, color = "black"),
                  legend.text = element_text(family = "Lato", size = 8, color = "black"),
                  panel.background = element_rect(fill="gray97") ,
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.title = element_text(family = "Lato Black", size = 8, color = "black"),
                  plot.caption = element_text(family = "Lato Light", size =10, color = "gray50"),
                  axis.title = element_text(family = "Lato", size = 12))
subtitulo <- "Incidencia semanal de casos de covid-19\nCorte al 29/11/2020 | Semana 48"
marcas <- c( "+143", "59-143", "30-59","15-30", "0-15")

#Río Sonora
Region <- "Río Sonora"
Casossemana <- Casos %>% mutate(Semana = isoweek(Fecha)) %>% group_by(Semana) %>% 
  mutate (Reporte=max(as.Date(Fecha))) %>% ungroup()
Casossem <- group_by(Casossemana, CVEGEO, MUNICIPIO, Reporte, Semana) %>% 
  summarise('CASOS SEMANALES' = sum(NUEVOS), ACUMULADOS=max(CASOS)) 
casossempob <- left_join(Casossem, POBMUN, by = "CVEGEO") 
casossempob  <- casossempob %>% mutate (INCIDENCIA= round((`CASOS SEMANALES`*100000)/POB,1))
casossempob$INCIDENCIA[casossempob$INCIDENCIA==0] <- NA
#casossempob  <- casossempob %>% filter(Semana!=46)
casossempob   <- mutate(casossempob , IS=if_else(INCIDENCIA>143,5, if_else(INCIDENCIA>59,4, if_else(INCIDENCIA>30,3,if_else(INCIDENCIA>15,2,1)))))
casossempob  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempob) + 
  geom_tile(mapping = aes(x = Semana, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks= c("5", "4", "3", "2", "1"), 
                    labels = marcas)+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_continuous(breaks = seq(from = 12, to = 48, by = 1))+
  theme_minimal() +
  labs(y = NULL, x = "Semana (lunes-domingo)", title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption ="Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Estado de Sonora") +
  paragraf 

ggsave(paste("Gráficos/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")


# Sierra Alta
Region <- "Sierra Alta"
Casossemana <- Casos %>% mutate(Semana = isoweek(Fecha)) %>% group_by(Semana) %>% 
  mutate (Reporte=max(as.Date(Fecha))) %>% ungroup()
Casossem <- group_by(Casossemana, CVEGEO, MUNICIPIO, Reporte, Semana) %>% 
  summarise('CASOS SEMANALES' = sum(NUEVOS), ACUMULADOS=max(CASOS)) 
casossempob <- left_join(Casossem, POBMUN, by = "CVEGEO") 
casossempob  <- casossempob %>% mutate (INCIDENCIA= round((`CASOS SEMANALES`*100000)/POB,1))
casossempob$INCIDENCIA[casossempob$INCIDENCIA==0] <- NA
#casossempob  <- casossempob %>% filter(Semana!=46)
casossempob   <- mutate(casossempob , IS=if_else(INCIDENCIA>143,5, if_else(INCIDENCIA>59,4, if_else(INCIDENCIA>30,3,if_else(INCIDENCIA>15,2,1)))))
casossempob  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempob) + 
  geom_tile(mapping = aes(x = Semana, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks= c("5", "4", "3", "2", "1"), 
                    labels = marcas)+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_continuous(breaks = seq(from = 12, to = 48, by = 1))+
  theme_minimal() +
  labs(y = NULL, x = "Semana (lunes-domingo)", title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption ="Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Estado de Sonora") +
  paragraf 

ggsave(paste("Gráficos/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")

# Sierra Centro
Region <- "Sierra Centro"
Casossemana <- Casos %>% mutate(Semana = isoweek(Fecha)) %>% group_by(Semana) %>% 
  mutate (Reporte=max(as.Date(Fecha))) %>% ungroup()
Casossem <- group_by(Casossemana, CVEGEO, MUNICIPIO, Reporte, Semana) %>% 
  summarise('CASOS SEMANALES' = sum(NUEVOS), ACUMULADOS=max(CASOS)) 
casossempob <- left_join(Casossem, POBMUN, by = "CVEGEO") 
casossempob  <- casossempob %>% mutate (INCIDENCIA= round((`CASOS SEMANALES`*100000)/POB,1))
casossempob$INCIDENCIA[casossempob$INCIDENCIA==0] <- NA
#casossempob  <- casossempob %>% filter(Semana!=45)
casossempob   <- mutate(casossempob , IS=if_else(INCIDENCIA>143,5, if_else(INCIDENCIA>59,4, if_else(INCIDENCIA>30,3,if_else(INCIDENCIA>15,2,1)))))
casossempob  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempob) + 
  geom_tile(mapping = aes(x = Semana, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks= c("5", "4", "3", "2", "1"), 
                    labels = marcas)+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_continuous(breaks = seq(from = 12, to = 48, by = 1))+
  theme_minimal() +
  labs(y = NULL, x = "Semana (lunes-domingo)", title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption ="Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Estado de Sonora") +
  paragraf 

ggsave(paste("Gráficos/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")

#Sur
Region <- "Sur"
Casossemana <- Casos %>% mutate(Semana = isoweek(Fecha)) %>% group_by(Semana) %>% 
  mutate (Reporte=max(as.Date(Fecha))) %>% ungroup()
Casossem <- group_by(Casossemana, CVEGEO, MUNICIPIO, Reporte, Semana) %>% 
  summarise('CASOS SEMANALES' = sum(NUEVOS), ACUMULADOS=max(CASOS)) 
casossempob <- left_join(Casossem, POBMUN, by = "CVEGEO") 
casossempob  <- casossempob %>% mutate (INCIDENCIA= round((`CASOS SEMANALES`*100000)/POB,1))
casossempob$INCIDENCIA[casossempob$INCIDENCIA==0] <- NA
#casossempob  <- casossempob %>% filter(Semana!=45)
casossempob   <- mutate(casossempob , IS=if_else(INCIDENCIA>143,5, if_else(INCIDENCIA>59,4, if_else(INCIDENCIA>30,3,if_else(INCIDENCIA>15,2,1)))))
casossempob  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempob) + 
  geom_tile(mapping = aes(x = Semana, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks= c("5", "4", "3", "2", "1"), 
                    labels = marcas)+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_continuous(breaks = seq(from = 12, to = 48, by = 1))+
  theme_minimal() +
  labs(y = NULL, x = "Semana (lunes-domingo)", title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption ="Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Estado de Sonora") +
  paragraf 

ggsave(paste("Gráficos/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")


#Centro Norte
Region <- "Centro Norte"
Casossemana <- Casos %>% mutate(Semana = isoweek(Fecha)) %>% group_by(Semana) %>% 
  mutate (Reporte=max(as.Date(Fecha))) %>% ungroup()
Casossem <- group_by(Casossemana, CVEGEO, MUNICIPIO, Reporte, Semana) %>% 
  summarise('CASOS SEMANALES' = sum(NUEVOS), ACUMULADOS=max(CASOS)) 
casossempob <- left_join(Casossem, POBMUN, by = "CVEGEO") 
casossempob  <- casossempob %>% mutate (INCIDENCIA= round((`CASOS SEMANALES`*100000)/POB,1))
casossempob$INCIDENCIA[casossempob$INCIDENCIA==0] <- NA
#casossempob  <- casossempob %>% filter(Semana!=45)
casossempob   <- mutate(casossempob , IS=if_else(INCIDENCIA>143,5, if_else(INCIDENCIA>59,4, if_else(INCIDENCIA>30,3,if_else(INCIDENCIA>15,2,1)))))
casossempob  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempob) + 
  geom_tile(mapping = aes(x = Semana, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks= c("5", "4", "3", "2", "1"), 
                    labels = marcas)+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_continuous(breaks = seq(from = 12, to = 48, by = 1))+
  theme_minimal() +
  labs(y = NULL, x = "Semana (lunes-domingo)", title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption ="Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Estado de Sonora") +
  paragraf 

ggsave(paste("Gráficos/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")



#Noroeste
Region <- "Noroeste"
Casossemana <- Casos %>% mutate(Semana = isoweek(Fecha)) %>% group_by(Semana) %>% 
  mutate (Reporte=max(as.Date(Fecha))) %>% ungroup()
Casossem <- group_by(Casossemana, CVEGEO, MUNICIPIO, Reporte, Semana) %>% 
  summarise('CASOS SEMANALES' = sum(NUEVOS), ACUMULADOS=max(CASOS)) 
casossempob <- left_join(Casossem, POBMUN, by = "CVEGEO") 
casossempob  <- casossempob %>% mutate (INCIDENCIA= round((`CASOS SEMANALES`*100000)/POB,1))
casossempob$INCIDENCIA[casossempob$INCIDENCIA==0] <- NA
#casossempob  <- casossempob %>% filter(Semana!=45)
casossempob   <- mutate(casossempob , IS=if_else(INCIDENCIA>143,5, if_else(INCIDENCIA>59,4, if_else(INCIDENCIA>30,3,if_else(INCIDENCIA>15,2,1)))))
casossempob  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempob) + 
  geom_tile(mapping = aes(x = Semana, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks= c("5", "4", "3", "2", "1"), 
                    labels = marcas)+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_continuous(breaks = seq(from = 12, to = 48, by = 1))+
  theme_minimal() +
  labs(y = NULL, x = "Semana (lunes-domingo)", title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption ="Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Estado de Sonora") +
  paragraf 

ggsave(paste("Gráficos/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")


#Río Sonora
Region <- "Río Sonora"
Casossemana <- Casos %>% mutate(Semana = isoweek(Fecha)) %>% group_by(Semana) %>% 
  mutate (Reporte=max(as.Date(Fecha))) %>% ungroup()
Casossem <- group_by(Casossemana, CVEGEO, MUNICIPIO, Reporte, Semana) %>% 
  summarise('CASOS SEMANALES' = sum(NUEVOS), ACUMULADOS=max(CASOS)) 
casossempob <- left_join(Casossem, POBMUN, by = "CVEGEO") 
casossempob  <- casossempob %>% mutate (INCIDENCIA= round((`CASOS SEMANALES`*100000)/POB,1))
casossempob$INCIDENCIA[casossempob$INCIDENCIA==0] <- NA
#casossempob  <- casossempob %>% filter(Semana!=45)
casossempob   <- mutate(casossempob , IS=if_else(INCIDENCIA>143,5, if_else(INCIDENCIA>59,4, if_else(INCIDENCIA>30,3,if_else(INCIDENCIA>15,2,1)))))
casossempob  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempob) + 
  geom_tile(mapping = aes(x = Semana, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks= c("5", "4", "3", "2", "1"), 
                    labels = marcas)+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_continuous(breaks = seq(from = 12, to = 48, by = 1))+
  theme_minimal() +
  labs(y = NULL, x = "Semana (lunes-domingo)", title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption ="Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Estado de Sonora") +
  paragraf 

ggsave(paste("Gráficos/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")










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
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), as.Date("2021-03-31")), date_breaks = "1 month", date_labels = "%B") +
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
  labs(y = NULL, 
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
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), as.Date("2021-03-31")), date_breaks = "1 month", date_labels = "%B") +
  # geom_hline(yintercept=62, linetype="dashed", color = "red") +  
  # geom_text(aes(x = as.Date("2020-04-15"), y = 68,
  #               label = "62 casos"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "red")+
  
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
        axis.text = element_text(family = "Lato", size =11),
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
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), as.Date("2021-03-31")), date_breaks = "1 month", date_labels = "%B") +
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


