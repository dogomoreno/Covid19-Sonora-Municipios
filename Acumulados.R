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


Sonorames <- Sonora.DF %>% mutate(mesnum=month(Fecha), mes = months.Date(Fecha),  año = year(Fecha)) %>% select(año, mesnum, mes, Fecha, Confirmados, Decesos, Casos.diarios, Decesos.diarios)
acummes <- Sonorames %>% group_by(año,mesnum, mes) %>% summarise(Confirmados=sum(Casos.diarios), Decesos=sum(Decesos.diarios))
write.csv(acummes, "ResultadoCSV/acummes.csv")
Sonorafebrero <- Sonorames %>% filter(mes=="febrero") %>% rename (FebreroConfirmados=Confirmados, Febrerodecesos=Decesos) 
Sonorafebrero <- Sonorafebrero %>% select (Fecha, FebreroConfirmados, Febrerodecesos)

#Sonoraenero <- Sonorames %>% filter(mes=="enero") %>% rename (EneroConfirmados=Confirmados, Enerodecesos=Decesos) 
#Sonoraenero <- Sonoraenero %>% select (Fecha, EneroConfirmados, Enerodecesos)


Sonoramesseg <- Sonorames %>% left_join(Sonorafebrero)
#Sonoramesseg <- Sonoramesseg %>% left_join(Sonoraenero)

Sonorames[is.na(Sonorames)] = 0

Casos.mes <- Sonorames %>% group_by(año, mesnum, mes) %>% summarise (Casos=sum(Casos.diarios), Decesos=sum(Decesos.diarios))
Sonorarect <- Sonoramesseg %>%   filter(Fecha==as.Date("2021-02-01"))


# Casos diarios Estatal
Casosacum <- ggplot(Sonoramesseg) +
  geom_area(aes(x= Fecha, y= Confirmados), fill= "#58BCBC", alpha=0.2)+
  geom_area(aes(x= Fecha, y= FebreroConfirmados), fill= "#58BCBC", alpha=0.5)+
  geom_line(aes(x= Fecha, y= Confirmados), color= "#01787E", linetype= "solid", size=1.5)+
  scale_y_continuous(expand = c(0, 5), label=comma) +
  scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
    geom_curve(aes(x = as.Date("2021-01-01"), y = 56500, xend = as.Date("2021-02-15"), yend = 45000),
               size = 1.5, color = "black", alpha=0.5,
               arrow = arrow(length = unit(0.02, "npc"))) +
  geom_text(aes(x = as.Date("2021-01-01"), y = 60000,
                label = "Febrero 2021\n4,707 casos"), stat = "unique", family = "Lato Black",
            size = 5, color = "black")+
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
Casosacum

ggsave("Gráficos/CasosacumBEB.png",Casosacum, bg = "transparent", height = 25, width = 30, units = "cm", dpi = 400, type = 'cairo')

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



casosmes <- Casos %>% mutate(mesnum=month(Fecha), mes = months.Date(Fecha),  año = year(Fecha))  %>% group_by(año, mes, mesnum) %>% summarise(Casos=sum(NUEVOS))
