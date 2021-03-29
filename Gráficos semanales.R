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

Fechahoy <- "Corte al 28 de febrero de 2021"

# Carga base estatal
Sonora.DF <- read_csv("Bases/ST_SonoraInformesCOVID.csv", 
                      col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))


# Gráfico estatal
Casossemana <- Sonora.DF %>% mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==weekdays(max(as.Date(Fecha))))

Casossemana <- mutate(Casossemana, Casos.semana= Confirmados - lag(Confirmados, default = Confirmados[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, Decesos.semana= Decesos - lag(Decesos, default = Decesos[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, Pruebas.semana= Pruebas - lag(Pruebas, default = Pruebas[1]))


# Casos diarios Estatal
Casossemson <- ggplot(Casossemana) +
  geom_col(aes(x=Fecha, y= Casos.semana, fill= Casos.semana), color="white") +
  scale_fill_gradient(low = "#58BCBC", high = "black") +
  geom_text( aes(x=Fecha, y= Casos.semana, label= Casos.semana), family="Lato", size= 4, color="white", angle=90, hjust = 1.1) +
  theme_bw() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#01A2AC"),  
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text.x = element_text(family = "Lato", size =10),
        axis.text.y = element_text(family = "Lato", size =10, angle = 90, vjust = 0.5),
        plot.background = element_rect(fill = "white", color = "black", size = 5),
        axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), 
        plot.caption = element_text(family = "Lato", size = 10, color = "#01A2AC"),
        legend.text = element_text(family = "Lato", size = 12),
        legend.position = "none",  legend.justification="left") +
  labs(y = "Casos confirmados acumulados", 
       x = NULL,legend= NULL, title  = "66,642 casos acumulados\n de covid-19 en Sonora", 
       subtitle= Fechahoy, caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com")
Casossemson

ggsave("Gráficos/Casossemanal.png",Casossemson, bg = "transparent", height = 25, width = 30, units = "cm", dpi = 400, type = 'cairo')

Decesosacum <- ggplot(Sonoramesseg) +
  geom_area(aes(x= Fecha, y= Decesos), fill= "#D075A3", alpha=0.2)+
  geom_area(aes(x= Fecha, y= Febrerodecesos), fill= "#D075A3", alpha=0.5)+
  geom_line(aes(x= Fecha, y= Decesos), color= "#73264D", linetype= "solid", size=1.5)+
  scale_y_continuous(expand = c(0, 5), label=comma) +
  scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  geom_curve(aes(x = as.Date("2021-01-01"), y = 4600, xend = as.Date("2021-02-15"), yend = 3100),
             size = 1.5, color = "black", alpha=0.5,
             arrow = arrow(length = unit(0.02, "npc"))) +
  geom_text(aes(x = as.Date("2021-01-01"), y = 4900,
                label = "Febrero 2021\n601 decesos"), stat = "unique", family = "Lato Black",
            size = 5, color = "black")+
  theme_bw() +
  theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(1, 1, 0.5, 0.8, "cm"),
        plot.title = element_text(family = "Lato Black", size = 40,color = "#73264D"),  
        plot.subtitle = element_text(family = "Lato Light", size = 16, color = "black"), legend.title = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 16),
        axis.text.x = element_text(family = "Lato", size =10),
        axis.text.y = element_text(family = "Lato", size =10, angle = 90, vjust = 0.5),
        plot.background = element_rect(fill = "white", color = "black", size = 5),
        axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), 
        plot.caption = element_text(family = "Lato", size = 10, color = "#73264D"),
        legend.text = element_text(family = "Lato", size = 12),
        legend.position = "none",  legend.justification="left") +
  labs(y = "Decesos confirmados acumulados", 
       x = NULL,legend= NULL, title  = "5,716 decesos acumulados\n por covid-19 en Sonora", 
       subtitle= Fechahoy, caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com")
Decesosacum

ggsave("Gráficos/DecesosacumFEB.png",Decesosacum, bg = "transparent", height = 25, width = 30, units = "cm", dpi = 400, type = 'cairo')
