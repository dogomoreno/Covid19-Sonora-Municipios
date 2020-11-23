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
subtitulo <- "Incidencia semanal de casos de covid-19\nCorte al 22/11/2020 | Semana 47"
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
                    breaks= breaks= c("5", "4", "3", "2", "1"), 
                    labels = marcas)+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_continuous(breaks = seq(from = 11, to = 47, by = 1))+
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
  scale_x_continuous(breaks = seq(from = 11, to = 47, by = 1))+
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
  scale_x_continuous(breaks = seq(from = 11, to = 47, by = 1))+
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
  scale_x_continuous(breaks = seq(from = 11, to = 47, by = 1))+
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
  scale_x_continuous(breaks = seq(from = 11, to = 47, by = 1))+
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
  scale_x_continuous(breaks = seq(from = 11, to = 47, by = 1))+
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
  scale_x_continuous(breaks = seq(from = 11, to = 47, by = 1))+
  theme_minimal() +
  labs(y = NULL, x = "Semana (lunes-domingo)", title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption ="Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Estado de Sonora") +
  paragraf 

ggsave(paste("Gráficos/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")

