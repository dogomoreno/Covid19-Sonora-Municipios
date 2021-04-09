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

lundom <- "jueves"
Fechasem <- "Corte al 08 de abril de 2021 | Confirmados acumulados de viernes a jueves"
Fechadom <- "Corte al 08 de abril de 2021 | Cifras al jueves de cada semana"
Fechahoy <- "Corte al 08 de abril de 2021"
fuente <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com"
temaejes <- theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(10, 25, 10, 25),
                  plot.title = element_markdown(family = "Lato Black", size = 25),  
                  plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                  strip.text = element_text(family = "Lato Black", size = 10),
                  axis.text = element_text(family = "Lato", size =6),
                  plot.background = element_rect(fill = "white", color = "black", size = 3),
                  axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                  axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                  plot.caption = element_text(family = "Lato", size = 6),
                  legend.text = element_blank(),
                  legend.position = "none", plot.title.position = 'plot', plot.caption.position = 'plot')

temasinejes <-  theme(axis.line.x = element_line(linetype = "solid"), axis.line.y = element_blank(),
                      plot.margin = margin(10, 25, 10, 25),
                      plot.title = element_markdown(family = "Lato Black", size = 25),  
                      plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                      axis.text.x = element_text(family = "Lato", size =6, angle=90, hjust=0.95,vjust=0.5),   panel.grid= element_blank(),
                      axis.text.y = element_blank(),
                      plot.background = element_rect(fill = "white", color = "black", size = 3),
                      axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                      axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                      plot.caption = element_text(family = "Lato", size = 6, color = "black"),
                      legend.text = element_text(family = "Lato", size = 8),
                      legend.position = "none",  legend.justification="left", plot.title.position = 'plot', plot.caption.position = 'plot')

# Carga base estatal
Sonora.DF <- read_csv("Bases/ST_SonoraInformesCOVID.csv", 
                      col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))
Sonora.DF <- mutate(Sonora.DF, Positividad.acum= round((Confirmados / Pruebas)*100,1))


# Gráfico estatal
Casossemana <- Sonora.DF %>% mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom)

Casossemana <- mutate(Casossemana, Casos.semana= Confirmados - lag(Confirmados, default = Confirmados[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, Decesos.semana= Decesos - lag(Decesos, default = Decesos[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, Pruebas.semana= Pruebas - lag(Pruebas, default = Pruebas[1]))
Casossemana <- mutate(Casossemana, Positividad.semanal= round((Casos.semana / Pruebas.semana)*100,1))
Casossemana <- mutate(Casossemana, ISSSTESON= C_ISSSTESON - lag(C_ISSSTESON, default = C_ISSSTESON[1], order_by=Fecha))


# Gráfico Treemap confirmados estatales
Sonora.DF.hoy <- filter(Sonora.DF, Fecha == as.Date("2021-04-08"))
Sonora.DF.hoy <- select(Sonora.DF.hoy, Hospitalizados, Ambulatorios.Activos, Decesos, Recuperados)
Sonora.DF.hoy <- rename(Sonora.DF.hoy, "Ambulatorios activos"= Ambulatorios.Activos)
Sonora.DF.hoy <- gather(Sonora.DF.hoy, key= Estatus, value= Casos.confirmados) 
tituestatus <- paste0("<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br>Estatus de los <span style = 'color:#01A2AC';>", prettyNum(as.numeric(sum(Sonora.DF.hoy$Casos.confirmados)), big.mark=",", preserve.width="none"), "</span> casos confirmados")


Sonora.DF.hoy$label <- c(paste0(Sonora.DF.hoy$Estatus, "\n ",prettyNum(as.numeric(Sonora.DF.hoy$Casos.confirmados), big.mark=",", preserve.width="none"), "\n ", (round((Sonora.DF.hoy$Casos.confirmados/(sum(Sonora.DF.hoy$Casos.confirmados))*100), digits = 1)),"%"))
Hosp.hoy <- Sonora.DF.hoy %>% filter(Estatus=="Hospitalizados")

hosplab <- Hosp.hoy$label

Estatus <- ggplot(Sonora.DF.hoy, aes(area = Casos.confirmados, fill= Estatus, label= Sonora.DF.hoy$label)) + geom_treemap( size=2, color= "white") +
  scale_fill_manual(values= c("#01A2AC", "#993366", "#F79646", "#4BACC6")) +
  scale_color_manual(values=c("#005156","#4D1933", "#984807",  "#215968" )) +
  geom_treemap_text(family = "Lato Black", colour = "white", place = "topleft",
                    grow = FALSE, size=10) +  theme_void() +
  #annotate(geom = "text", x = 0.93, y = 0.99, label = hosplab, color= "white", family= "Lato Black",  hjust=1, size=2) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_void() + temasinejes +
  theme(plot.tag = element_text(family = "Lato Black", size = 6,color = "#F79646",hjust=0),
        plot.tag.position = c(0.900, 0.84), axis.line.x = element_blank(), axis.text.x = element_blank())+
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = tituestatus, tag = hosplab, 
       subtitle= Fechahoy, caption =fuente)
Estatus

ggsave("Gráficos jueves/s02.png",Estatus , width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)


# Casos diarios Estatal

Casossemson <- ggplot(Casossemana) +
  geom_col(aes(x=Fecha, y= Casos.semana, fill= Casos.semana), color= "#005156", size=0.15) +
  scale_fill_gradient2(low = "#DEF2F2", mid= "#01A2AC", high = "#005156", midpoint = 1500) +
  geom_text( aes(x=Fecha, y= Casos.semana, label= Casos.semana), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#01A2AC';>Casos semanales</span>", 
       subtitle= Fechasem, caption =fuente)
Casossemson

ggsave("Gráficos jueves/s03.png",Casossemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)


# Decesos diarios Estatal

Decesossemson <- ggplot(Casossemana) +
  geom_col(aes(x=Fecha, y= Decesos.semana, fill= Decesos.semana), color= "#4D1933", size=0.15) +
  scale_fill_gradient2(low = "#F0D1E0", mid= "#993366", high = "#4D1933", midpoint = 200) +
  geom_text( aes(x=Fecha, y= Decesos.semana, label= Decesos.semana), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#993366';>Decesos semanales</span>", 
       subtitle= Fechasem, caption =fuente)
Decesossemson

ggsave("Gráficos jueves/s09.png",Decesossemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)

# Hospitalizados diarios Estatal

Hospsemana <- Casossemana %>% filter(Fecha >= as.Date("2020-08-09"))
Hospsemson <- ggplot(Hospsemana) +
  geom_col(aes(x=Fecha, y= Hospitalizados, fill= Hospitalizados), color= "#984807", size=0.5, width=5) +
  scale_fill_gradient2(low = "#FCD5B5", mid= "#F79646", high = "#984807", midpoint = 300) +
  geom_text( aes(x=Fecha, y= Hospitalizados, label= Hospitalizados), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Hospsemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#F79646';>Hospitalizados al jueves de cada semana</span>", 
       subtitle= Fechadom, caption =fuente)
 Hospsemson

ggsave("Gráficos jueves/s04.png",Hospsemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)

# Activos leves

Activossem <- Casossemana %>% filter(Fecha >= as.Date("2020-06-25"))
Activos <- ggplot(Activossem) +
  geom_col(aes(x=Fecha, y= Ambulatorios.Activos, fill= Ambulatorios.Activos), color= "#3B9494", size=0.5, width=5) +
  scale_fill_gradient2(low = "#BCE4E4", mid= "#58BCBC", high = "#3B9494", midpoint = 1500) +
  geom_text( aes(x=Fecha, y= Ambulatorios.Activos, label= Ambulatorios.Activos), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#58BCBC';>Ambulatorios activos al jueves de cada semana</span>", 
       subtitle= Fechadom, caption =fuente)
Activos

ggsave("Gráficos jueves/s05.png",Activos, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)

# Casos ISSSTESON

casosisst <- Casossemana %>% filter(Fecha >= as.Date("2020-05-28"))
ISSSTESON <- ggplot(casosisst) +
  geom_col(aes(x=Fecha, y= ISSSTESON, fill= ISSSTESON), color= "#984807", size=0.5, width=5) +
  scale_fill_gradient2(low = "#FCD5B5", mid= "#F79646", high = "#984807", midpoint = 170) +
  geom_text( aes(x=Fecha, y= ISSSTESON, label= ISSSTESON), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = casosisst$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#F79646';>Casos atendidos en ISSSTESON semanalmente</span>", 
       subtitle= Fechasem, caption =fuente)
ISSSTESON

ggsave("Gráficos jueves/ISSSTESON.png",ISSSTESON, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)

# Pruebas semanales

Pruebassemson <- ggplot(Casossemana) +
  geom_col(aes(x=Fecha, y= Pruebas.semana, fill= Pruebas.semana), color="#215968", size=0.15) +
  scale_fill_gradient2(low = "#B7DEE8", mid= "#4BACC6", high = "#215968", midpoint = 2300) +
  geom_text( aes(x=Fecha, y= Pruebas.semana, label= Pruebas.semana), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#4BACC6';>Pruebas semanales</span>", 
       subtitle= Fechasem, caption =fuente)
Pruebassemson

ggsave("Gráficos jueves/s11.png",Pruebassemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)

#Positividad

library(directlabels)
Positividad <- ggplot() +
  geom_line(data= Sonora.DF, aes(x=Fecha, y= Positividad.acum), color= "#215968", linetype= "solid", size=1, alpha=0.6)+
  geom_line(data= Casossemana, aes(x=Fecha, y= Positividad.semanal), color= "#4BACC6", linetype= "solid", size=1, alpha=0.6)+
  geom_point( data = subset(Sonora.DF , Fecha == max(Fecha)), aes(x=Fecha, y= Positividad.acum), fill="#215968", size=2 , shape=21, color="white", stroke=1) +
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= Positividad.semanal), fill="#4BACC6", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Sonora.DF , Fecha == max(Fecha)), aes(x=Fecha, y= Positividad.acum, label = paste0("Positividad\nacumulada\n", Positividad.acum,"%", sep="")), color="#215968", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= Positividad.semanal,  label = paste0("\n\nPositividad\núlt. 7 días\n", Positividad.semanal,"%", sep="")), color="#4BACC6", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,100), breaks=seq(0,100,20)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), (max(as.Date(Sonora.DF$Fecha)) + 32)), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = "%", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#4BACC6';>Positividad de resultados de pruebas</span>", 
       subtitle= Fechahoy, caption =fuente)
Positividad
ggsave("Gráficos jueves/s12.png",Positividad, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)

# Municipales 

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

HilloCasos <- Casos %>% group_by(MUNICIPIO) %>% 
  mutate(diasemana = weekdays(Fecha), casos.hmo = rollsum(NUEVOS, 7, align="right", fill = 0)) %>% rename(ACUMULADOS=CASOS) %>% 
  filter(diasemana==lundom) %>% filter(MUNICIPIO=="Hermosillo") %>% select(Fecha, casos.hmo) 


HilloDecesos <- Decesos %>% group_by(MUNICIPIO) %>% 
  mutate(diasemana = weekdays(Fecha), decesos.hmo = rollsum(NUEVOS, 7, align="right", fill = 0)) %>% rename(ACUMULADOS=DECESOS) %>% 
  filter(diasemana==lundom) %>% filter(MUNICIPIO=="Hermosillo") %>% select(Fecha, decesos.hmo)

Casossemana <- Casossemana %>% left_join(HilloCasos, by="Fecha") %>% left_join(HilloDecesos, by="Fecha") %>% mutate(casos.resto= Casos.semana-casos.hmo, decesos.resto=Decesos.semana-decesos.hmo)

Casosmuni <- ggplot(Casossemana) +
  geom_line(aes(x=Fecha, y= casos.resto), linetype = "solid", color = "#F79646", size=0.8, alpha=0.3) +
  geom_line(aes(x=Fecha, y= casos.hmo), linetype = "solid", color = "#01A2AC", size=0.8, alpha=0.3)+
  geom_point(aes(x=Fecha, y= casos.resto), size=0.8, color = "#F79646", alpha=0.6) +
  geom_point(aes(x=Fecha, y= casos.hmo), size=0.8, color = "#01A2AC", alpha=0.6)+
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= casos.resto), fill="#F79646", size=2 , shape=21, color="white", stroke=1) +
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= casos.hmo), fill="#01A2AC", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= casos.resto,  label = paste0("Otros municipios ", casos.resto, sep="")), color="#F79646", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= casos.hmo, label = paste0("Hermosillo ", casos.hmo, sep="")), color="#01A2AC", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"),  (max(as.Date(Sonora.DF$Fecha)) + 62)), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = TRUE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = "Casos confirmados", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#01A2AC';>Casos semanales</span>", 
       subtitle= Fechasem, caption =fuente)
Casosmuni
ggsave("Gráficos jueves/s06.png",Casosmuni, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)

Decesosmuni <- ggplot(Casossemana) +
  geom_line(aes(x=Fecha, y= decesos.resto), linetype = "solid", color = "#4BACC6", size=0.8, alpha=0.3) +
  geom_line(aes(x=Fecha, y= decesos.hmo), linetype = "solid", color = "#993366", size=0.8, alpha=0.3)+
  geom_point(aes(x=Fecha, y= decesos.resto), size=0.8, color = "#4BACC6", alpha=0.6) +
  geom_point(aes(x=Fecha, y= decesos.hmo), size=0.8, color = "#993366", alpha=0.6)+
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= decesos.resto), fill="#4BACC6", size=2 , shape=21, color="white", stroke=1) +
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= decesos.hmo), fill="#993366", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= decesos.resto,  label = paste0("Otros municipios ", decesos.resto, sep="")), color="#4BACC6", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= decesos.hmo, label = paste0("Hermosillo ", decesos.hmo, sep="")), color="#993366", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), (max(as.Date(Sonora.DF$Fecha)) + 62)), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = TRUE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = "Decesos confirmados", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#993366';>Decesos semanales</span>", 
       subtitle= Fechasem, caption =fuente)
Decesosmuni
ggsave("Gráficos jueves/s14.png",Decesosmuni, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)
