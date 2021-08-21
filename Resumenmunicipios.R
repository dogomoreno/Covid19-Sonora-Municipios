rm(list=ls())


# Paquetes
  
  library(tidyverse)
  library(extrafont)
  library(scales)
  library(showtext)
  library(units)
  library(zoo)
  library(lubridate)
  library("Cairo")
  library(directlabels)
  library(ggtext)
  library(patchwork)
  
  
  # Bases municipales
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
  
  Fechahoy <- "Al reporte del 21 de agosto de 2021"
  fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Estado de Sonora\nPor continuidad, la fecha de corte se asume como la del día anterior al reporte. | www.luisarmandomoreno.com"
  
  POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                     locale = locale(encoding = "ISO-8859-1"))
  
  temaejes <- theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(10, 25, 10, 25),
                    plot.title = element_markdown(family = "Lato Black", size = 15),  
                    plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                    strip.text = element_text(family = "Lato Black", size = 10),
                    axis.text = element_text(family = "Lato", size =6),
                    plot.background = element_blank(),
                    axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                    axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                    plot.caption = element_text(family = "Lato", size = 6),
                    legend.text = element_blank(),
                    legend.position = "none", plot.title.position = 'plot', plot.caption.position = 'plot')
  
  
  Casosconfd <-Casos %>% group_by(MUNICIPIO) %>% 
    rename(Casos.diarios=NUEVOS) %>% 
    mutate(Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1))
  
  
  Decesosconfd <-Decesos %>% group_by(MUNICIPIO) %>% 
    rename(Decesos.diarios=NUEVOS) %>% 
    mutate(Decesos.media.7d=round(rollmeanr(x=Decesos.diarios, 7, fill = NA),1))
  
  
  CasosDecesos <- Casosconfd %>% left_join(Decesosconfd, by= c("Fecha", "CVEGEO", "MUNICIPIO"))
  CasosDecesos$DECESOS[is.na(CasosDecesos$DECESOS)] <- 0
  CasosDecesos$Decesos.diarios[is.na(CasosDecesos$Decesos.diarios)] <- 0
  
  plot_municipio <- function(x = "Hermosillo") {
    tmp <- CasosDecesos %>%
      filter(MUNICIPIO == x)
    tmp2 <- tmp %>% filter(Fecha==max(as.Date(Fecha)))
    p1 <- ggplot(tmp) +
      geom_area(aes(x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
      geom_line(aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
      geom_point(aes(x= Fecha, y= Casos.diarios), color = "white", fill= "#01787E", size = 0.9, stroke=0.4, alpha=0.65, shape = 21) +
      scale_fill_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#58BCBC", "Casos diarios" = "#01787E")) + 
      scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#01787E", "Casos diarios" = "white")) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, (max(tmp$Casos.diarios)+5))) +
      scale_x_date(expand=c(0,0), date_breaks = "1 month", date_labels = "%B", limits=c(as.Date("2020-03-16"), (Sys.Date()+5))) +
      theme_bw() + temaejes +
      theme(legend.text = element_text(family = "Lato", size = 8), legend.background = element_rect(fill="transparent"),
            legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
            legend.key = element_rect(fill="transparent")) +
      labs(y = NULL, 
           x = NULL,legend= NULL, title  = paste0("<span style = 'color:#01A2AC';>Casos confirmados acumulados: ", prettyNum(as.numeric(max(tmp$CASOS)), big.mark=",", preserve.width="none"),"</span>"), 
           subtitle= paste0("Casos confirmados hoy: ",tmp2$Casos.diarios), caption =NULL)
    
    p2 <- ggplot(tmp) +
      geom_area(aes(x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.3)+
      geom_line(aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
      geom_point(aes(x= Fecha, y= Decesos.diarios), color = "white", fill= "#73264D", size = 0.9, stroke=0.4, alpha=0.65, shape = 21) +
      scale_fill_manual(name="", values= c("Decesos diarios" = "#73264D", "Tendencia promedio móvil 7 días" = "#D075A3")) + 
      scale_color_manual(name="", values= c("Decesos diarios" = "white","Tendencia promedio móvil 7 días" = "#73264D")) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, (max(tmp$Decesos.diarios)+2))) +
      scale_x_date(expand=c(0,0), date_breaks = "1 month", date_labels = "%B", limits=c(as.Date("2020-03-16"), (Sys.Date()+5))) + 
      theme_bw() + temaejes +
      theme(legend.text = element_text(family = "Lato", size = 8), legend.background = element_rect(fill="transparent"),
            legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
            legend.key = element_rect(fill="transparent")) +
      labs(y = NULL, 
           x = NULL,legend= NULL, title  =  paste0("<span style = 'color:#993366';> Decesos confirmados acumulados: ", prettyNum(as.numeric(max(tmp$DECESOS)), big.mark=",", preserve.width="none"),"</span>"), 
           subtitle= paste0("Decesos confirmados hoy: ",tmp2$Decesos.diarios), caption =NULL)
    
    patchwork <- (p1 / p2)
    p3 <- patchwork + plot_annotation(
      title = paste0("<span style = 'font-size:12pt'>Covid-19 en Sonora:</span><br>",x),
      subtitle = Fechahoy,
      caption = fuente, theme= theme(
        plot.title = element_markdown(family = "Lato Black", size = 30),  
        plot.subtitle = element_text(family = "Lato Light", size = 12, color = "black"),
        plot.caption = element_text(family = "Lato", size = 8), plot.title.position = 'plot', 
        plot.caption.position = 'plot', plot.margin = margin(10, 25, 10, 25), 
        plot.background = element_rect(fill = "white", color = "black", size = 3)))
    
  
    ggsave(paste0("municipales/", x,".png"),p3, width = 5 * (16/9), height = 10, type = "cairo", dpi = 300)
    
  }
  for (k in unique(CasosDecesos$MUNICIPIO)) {
    plot_municipio(k)
  }
  
  
  
