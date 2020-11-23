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
library(leaflet)
library("Cairo")
library(htmltools)



# Bases municipales
POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()),locale = locale(encoding = "ISO-8859-1"))
Casos <- read_csv("Bases/Casosdiarios.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1"))
casosacumdia <- filter(Casos,Fecha=="2020-11-21")
casosacumdiaorder <- arrange(casosacumdia,CASOS, desc(MUNICIPIO))
casosacumdia2 <- mutate(casosacumdiaorder,id=CVEGEO)

Decesos <- read_csv("Bases/Decesosdiarios.csv", 
                    col_types = cols(DECESOS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"))
decesosacumdia <- filter(Decesos,Fecha=="2020-11-21")
decesosacumdiaorder <- arrange(decesosacumdia,DECESOS, desc(MUNICIPIO))
decesosacumdia2 <- mutate(decesosacumdiaorder,id=CVEGEO)

casos_s <- group_by(Casos, CVEGEO, MUNICIPIO)
casos_ult <- casos_s %>% filter(NUEVOS!=0) %>% arrange(desc(Fecha)) %>% slice(1)
casos_ult_dias <- mutate(casos_ult, Dias_ult=as.numeric(Sys.Date()-Fecha)) 
casos_ult_dias <- mutate(casos_ult_dias, clasfdias=if_else(Dias_ult>30,5, if_else(Dias_ult>14,4, if_else(Dias_ult>7,3,if_else(Dias_ult>0,2,1)))))
casos_ult_dias <- mutate(casos_ult_dias, clasfdias=as.numeric(clasfdias))
diaspmap <- casos_ult_dias %>% rename(FechaUC=Fecha) %>% select(FechaUC, CVEGEO, Dias_ult, clasfdias)

decesosacumdia3 <- rename(decesosacumdia,'DECESOS NUEVOS'=NUEVOS)
casosacumdia3 <- rename(casosacumdia,'CASOS NUEVOS'=NUEVOS)
casosdecesos <-left_join(casosacumdia3, decesosacumdia3,by = c("CVEGEO","Fecha","MUNICIPIO"))
casosdecesospob <- left_join(casosdecesos, POBMUN, by = "CVEGEO")
Indicadores <- casosdecesospob %>% mutate (INCIDENCIAACUM = round((CASOS*100000)/POB,1), MORTALIDADACUM = round((DECESOS*100000)/POB,1), LETALIDAD = round((DECESOS*100/CASOS),1))
Indicadores <- rename(Indicadores, POBLACION=POB) 

Casossemana <- Casos %>% mutate(Semana = isoweek(Fecha)) %>% group_by(Semana) %>% 
  mutate (Reporte=max(as.Date(Fecha))) %>% ungroup()
Casossem <- group_by(Casossemana, CVEGEO, MUNICIPIO, Reporte, Semana) %>% 
  summarise('CASOS SEMANALES' = sum(NUEVOS)) 
casossempob <- left_join(Casossem, POBMUN, by = "CVEGEO") 
casossempob  <- casossempob %>% mutate (INCIDENCIASEM= round((`CASOS SEMANALES`*100000)/POB,1))
casossempob$INCIDENCIASEM[casossempob$INCIDENCIASEM==0] <- NA
casossempob  <- casossempob %>% filter(Semana==47)
casossempob   <- mutate(casossempob , IS=if_else(INCIDENCIASEM>143,5, if_else(INCIDENCIASEM>59,4, if_else(INCIDENCIASEM>30,3,if_else(INCIDENCIASEM>15,2,1)))))
Casossemanales <- casossempob %>% select(CVEGEO, MUNICIPIO, 'CASOS SEMANALES', INCIDENCIASEM, IS)

Decesossemana <- Decesos %>% mutate(Semana = isoweek(Fecha)) %>% group_by(Semana) %>% 
  mutate (Reporte=max(as.Date(Fecha))) %>% ungroup()
Decesossem <- group_by(Decesossemana, CVEGEO, MUNICIPIO, Reporte, Semana) %>% 
  summarise('DECESOS SEMANALES' = sum(NUEVOS)) 
Decesossempob <- left_join(Decesossem, POBMUN, by = "CVEGEO") 
Decesossempob  <- Decesossempob %>% mutate (MORTALIDADSEM= round((`DECESOS SEMANALES`*100000)/POB,1))
Decesossempob$MORTALIDADSEM[Decesossempob$MORTALIDADSEM==0] <- NA
Decesossempob  <- Decesossempob %>% filter(Semana==47)
Decesossempob   <- mutate(Decesossempob , MS=if_else(MORTALIDADSEM>22,5, if_else(MORTALIDADSEM>9,4, if_else(MORTALIDADSEM>4,3,if_else(MORTALIDADSEM>2,2,1)))))
Decesossemanales <- Decesossempob %>% select(CVEGEO, MUNICIPIO, 'DECESOS SEMANALES', MORTALIDADSEM, MS)



Mapa <- Indicadores %>%  left_join(diaspmap, by = c("CVEGEO","MUNICIPIO")) %>% 
  left_join(Casossemanales, by = c("CVEGEO","MUNICIPIO")) %>% left_join(Decesossemanales, by = c("CVEGEO","MUNICIPIO"))%>% rename(concat=CVEGEO) %>% 
  mutate(IA=if_else(INCIDENCIAACUM>1357,5, if_else(INCIDENCIAACUM>907,4, if_else(INCIDENCIAACUM>508,3,if_else(INCIDENCIAACUM>304,2,1)))))%>% 
  mutate(MA=if_else(MORTALIDADACUM>168,5, if_else(MORTALIDADACUM>107,4, if_else(MORTALIDADACUM>77,3,if_else(MORTALIDADACUM>50,2,1)))))



capa_munison <- readOGR("Shapes", layer="MUNSON",  encoding = "UTF-8", use_iconv=TRUE)
NOMMAY <- capa_munison@data %>% select(concat, NOM_MUN) %>% mutate(NOMMAY=toupper(NOM_MUN))
capa_munison <- capa_munison %>%  merge(Mapa) %>% merge(NOMMAY)


#incipal <- low = "#58BCBC", high = "black"
#incipal <-  colorNumeric(c("#58BCBC", "black"), domain = capa_munison$INCIDENCIA, na.color ="#d9d9d9")
#mortpal <-  colorNumeric(c("#993366", "black"), domain = capa_munison$MORTALIDAD, na.color ="#d9d9d9")
diaspal <-  colorFactor(c("#A04A69","#ECA48E", "#FECF7D","#0397A1", "#215968"), levels= c("1","2","3","4","5"), domain = capa_munison$clasfdias, na.color ="#d9d9d9")
incipal <-  colorFactor(c("black","#005155", "#01787E","#01A2AC", "#58BCBC"), levels= c("5","4","3","2","1"), na.color ="#d9d9d9")
mortpal <-  colorFactor(c("black","#4D1A33","#73264D", "#993366","#D075A3"), levels= c("5","4","3","2","1"), na.color ="#d9d9d9")
#discreta <- c("5" = "black", "4" = "#005155","3" = "#01787E","2" = "#01A2AC", "1" = "#58BCBC")

labs <- c("Muy baja","Baja", "Media", "Alta", "Muy alta")
diaslab <- c( "0", "1-7", "8-14", "15-30","+30")

#ultpal <- ("Días desde que se \n confirmó el último caso", 
 #          values = discreta, breaks= c("5", "4", "3", "2", "1"), 
  #         labels = c( "+30", "15-30", "8-14","1-7", "0")),
 #c("1" = "#A04A69", "2" = "#ECA48E","3" = "#FECF7D","4" = "#0397A1", "5" = "#215968")
#letpal low= "#993366", high = "black"

popup <- paste0(
  "<b>", as.character(capa_munison$NOMMAY), "</b>",     "<br>",                     
  "<b>", "Casos acumulados: ",        "</b>",   as.character(capa_munison$CASOS)   ,      "<br>",
  "<b>", "Casos última semana: ",           "</b>",   as.character(capa_munison$'CASOS SEMANALES'),      "<br>", 
  "<b>", "Decesos acumulados: ",           "</b>",   as.character(capa_munison$DECESOS) ,      "<br>", 
  "<b>", "Decesos última semana: ",           "</b>",   as.character(capa_munison$'DECESOS SEMANALES'),      "<br>",
  "<b>", "Día del último caso confirmado ",  "</b>",   as.Date(capa_munison$FechaUC) , "<br>")  %>% lapply(htmltools::HTML)




mapacovid <- leaflet(capa_munison) %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addLayersControl( 
      baseGroups = c("INCIDENCIA ACUMULADA","INCIDENCIA ÚLTIMA SEMANA", "MORTALIDAD ACUMULADA", "MORTALIDAD ÚLTIMA SEMANA"), 
      options = layersControlOptions(collapsed = FALSE)) %>% 
    addPolygons(data= capa_munison,
                stroke= TRUE,
                weight=1.2,                   
              opacity=1,
              fillColor = ~incipal(capa_munison$IA),
              #label = capa_munison$NOMMAY,
              color= "white",
              fillOpacity = 1,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              label=popup, 
              labelOptions = labelOptions(noHide = F, direction = "left",
                                                       style = list(
                                                         "color" = "black",
                                                         "font-family" = "Lato",
                                                         "font-style" = "regular",
                                                         "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                         "font-size" = "13px",
                                                         "border-color" = "rgba(0,0,0,0.5)"
                                                       )),
              group= "INCIDENCIA ACUMULADA") %>%
  addPolygons(data= capa_munison,
              stroke= TRUE,
              weight=1.2,                   
              opacity=1,
              fillColor = ~incipal(capa_munison$IS),
              #label = capa_munison$NOMMAY,
              color= "white",
              fillOpacity = 1,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              label=popup, 
              labelOptions = labelOptions(noHide = F, direction = "left",
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Lato",
                                            "font-style" = "regular",
                                            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                            "font-size" = "13px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "INCIDENCIA ÚLTIMA SEMANA") %>%
  addPolygons(data= capa_munison,
              stroke= TRUE,
              weight=1.2,                   
              opacity=1,
              fillColor = ~mortpal(capa_munison$MS),
              color= "white",
              fillOpacity = 1,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              label=popup, 
              labelOptions = labelOptions(noHide = F, direction = "left",
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Lato",
                                            "font-style" = "regular",
                                            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                            "font-size" = "13px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "MORTALIDAD ÚLTIMA SEMANA") %>%
  addPolygons(data= capa_munison,
              stroke= TRUE,
              weight=1.2,                   
              opacity=1,
              fillColor = ~mortpal(capa_munison$MA),
              color= "white",
              fillOpacity = 1,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              label=popup, 
              labelOptions = labelOptions(noHide = F, direction = "left",
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Lato",
                                            "font-style" = "regular",
                                            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                            "font-size" = "13px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "MORTALIDAD ACUMULADA") %>%
  addLegend(position = "bottomright", pal = mortpal, values = ~capa_munison$MA, opacity=1, group= "MORTALIDAD", 
            labFormat = function(type, cuts, p) {  # Here's the trick
              paste0(labs)},
            title = "MORTALIDAD", na.label = "N/A")  %>%
  addLegend(position = "bottomright", pal = incipal, values = ~capa_munison$IA, opacity=1, group= "INCIDENCIA", 
              labFormat = function(type, cuts, p) {  # Here's the trick
               paste0(labs)},
              title = "INCIDENCIA", na.label = "N/A") 


mapacovid

htmlwidgets::saveWidget(mapacovid, "mapacovid.html")
