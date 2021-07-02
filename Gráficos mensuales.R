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
library(directlabels)
library(ggtext)

Fechames <- "Corte al 01 de julio de 2021 | Confirmados acumulados por mes"
fuente <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Estado de Sonora\n*Por continuidad, la fecha de corte se asume como la del día anterior al reporte. | www.luisarmandomoreno.com"
temames <-  theme(axis.line.x = element_line(linetype = "solid"), axis.line.y = element_blank(),
                      plot.margin = margin(10, 25, 10, 25),
                      plot.title = element_markdown(family = "Lato Black", size = 25),  
                      plot.subtitle = element_text(family = "Lato Light", size = 12, color = "black"), legend.title = element_blank(),
                      axis.text.x = element_text(family = "Lato", size =8),   panel.grid= element_blank(),
                      axis.text.y = element_blank(),
                      plot.background = element_rect(fill = "white", color = "black", size = 3),
                      axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                      axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                      plot.caption = element_text(family = "Lato", size = 6, color = "black"),
                      legend.text = element_text(family = "Lato", size = 8),
                      legend.position = "none",  legend.justification="left", plot.title.position = 'plot', plot.caption.position = 'plot')

Casos <- read_csv("Bases/Casosdiarios.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1"))
Decesos <- read_csv("Bases/Decesosdiarios.csv", 
                    col_types = cols(DECESOS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"))

Casosmes <- Casos %>%mutate(mesnum=month(Fecha), mes = months.Date(Fecha),  año = year(Fecha)) %>% select(año, mesnum, mes, Fecha, NUEVOS)
Casosmes <- Casosmes %>% group_by(año, mesnum, mes) %>% summarise(Fecha=min(Fecha), casos.mensuales=sum(NUEVOS)) %>% select (Fecha, mesnum, mes, casos.mensuales) %>% 
  filter(Fecha<as.Date("2021-07-01"))
Casosmes$Fecha[1]<- as.Date("2020-03-01")

Decesosmes <- Decesos %>%mutate(mesnum=month(Fecha), mes = months.Date(Fecha),  año = year(Fecha)) %>% select(año, mesnum, mes, Fecha, NUEVOS)
Decesosmes <- Decesosmes %>% group_by(año, mesnum, mes) %>% summarise(Fecha=min(Fecha), decesos.mensuales=sum(NUEVOS)) %>% select (Fecha, mesnum, mes, decesos.mensuales)%>% 
  filter(Fecha<as.Date("2021-07-01"))
Decesosmes$Fecha[1]<- as.Date("2020-04-01")


Casosmesson <- ggplot(Casosmes) +
  geom_col(aes(x=Fecha, y= casos.mensuales, fill= casos.mensuales), color= "#005156", size=0.3, width=20) +
  scale_fill_gradient2(low = "#DEF2F2", mid= "#01A2AC", high = "#005156", midpoint = 5000) +
  geom_text( data = subset(Casosmes, Fecha<as.Date("2020-04-30")), aes(x=Fecha, y= casos.mensuales, label= scales::comma(casos.mensuales)), family="Lato Black", size= 5, color="black", angle=90, hjust = -0.2) +
  geom_text( data = subset(Casosmes,Fecha>as.Date("2020-04-30")), aes(x=Fecha, y= casos.mensuales, label= scales::comma(casos.mensuales)), family="Lato Black", size= 5, color="white", angle=90, hjust = 1.1) +
  # geom_text(aes(x = as.Date("2020-04-30"), y = 8000,
  #            label = "69,936 casos acumulados"), stat = "unique", family = "Lato Black",
  #            size = 5, color = "black", hjust =1) +
  scale_x_date(expand=c(0,0), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temames +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#01A2AC';>Casos mensuales</span>", 
       subtitle= Fechames, caption =fuente)
Casosmesson

ggsave("Gráficos/MesCas.png",Casosmesson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)

Decesosmesson <- ggplot(Decesosmes) +
  geom_col(aes(x=Fecha, y= decesos.mensuales, fill= decesos.mensuales), color= "#4D1933", size=0.3, width=20) +
  scale_fill_gradient2(low = "#F0D1E0", mid= "#993366", high = "#4D1933", midpoint = 600) +
  geom_text( data = subset(Decesosmes, Fecha<as.Date("2020-04-30")), aes(x=Fecha, y= decesos.mensuales, label= decesos.mensuales), family="Lato Black", size= 5, color="black", angle=90, hjust = -0.2) +
  geom_text( data = subset(Decesosmes,Fecha>as.Date("2020-04-30")), aes(x=Fecha, y= decesos.mensuales, label= decesos.mensuales), family="Lato Black", size= 5, color="white", angle=90, hjust = 1.2) +
  # geom_text(aes(x = as.Date("2020-03-30"), y = 800,
  #               label = "5,991 decesos acumulados"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black", hjust =1) +
  scale_x_date(expand=c(0,0), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temames +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#993366';>Decesos mensuales</span>", 
       subtitle= Fechames, caption =fuente)
Decesosmesson

ggsave("Gráficos/MesDes.png",Decesosmesson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)


