Casos <- read_csv("Bases/Casosdiarios.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1"))
casosacumdia <- filter(Casos,MUNICIPIO=="Hermosillo")
casosacumdia2 <- mutate(casosacumdia,id=CVEGEO)

Decesos <- read_csv("Bases/Decesosdiarios.csv", 
                    col_types = cols(DECESOS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"))
decesosacumdia <- filter(Decesos,MUNICIPIO=="Hermosillo")
decesosacumdia2 <- mutate(decesosacumdia,id=CVEGEO)

decesosacumdia3 <- rename(decesosacumdia,'DECESOS NUEVOS'=NUEVOS)
casosacumdia3 <- rename(casosacumdia,'CASOS NUEVOS'=NUEVOS)
casosdecesos <-left_join(casosacumdia3, decesosacumdia3,by = c("CVEGEO","Fecha","MUNICIPIO"))
casosdecesospob <- left_join(casosdecesos, POBMUN, by = "CVEGEO")
Indicadores <- casosdecesospob %>% mutate (INCIDENCIAACUM = round((CASOS*100000)/POB,1), MORTALIDADACUM = round((DECESOS*100000)/POB,1), LETALIDAD = round((DECESOS*100/CASOS),1))
Indicadores <- rename(Indicadores, POBLACION=POB)
Indicadores <- mutate(Indicadores,id=CVEGEO)

Fechahoy <- "Corte al 06 de enero de 2021"

Letalidad <- Indicadores %>% ggplot() +
  #geom_area(aes(x= Fecha, y= Decesos.media.7d, fill= "Promedio móvil 7d"), alpha=0.3)+
  geom_line(aes(x= Fecha, y= LETALIDAD), color= "#993366", linetype= "solid", size=2)+
  #geom_point(aes(x= Fecha, y= LETALIDAD), fill= "#993366", color = "white", size = 2, stroke=1, alpha=1, shape = 21) +
  #scale_fill_manual(name="", values= c("LETALIDAD1" = "#73264D", "Promedio móvil 7d" = "#D075A3")) + 
  #scale_color_manual(name="", values= c("Promedio móvil 7d" = "#73264D")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,8.5)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-08"), as.Date("2021-01-10")), date_breaks = "1 month", date_labels = "%B") +
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
  labs(y = "Decesos por cada 100 casos de covid-19", 
       x = NULL,legend= NULL, title  = "Letalidad\n de Covid-19 en Hermosillo", 
       subtitle= Fechahoy, caption ="\nFuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com")  

Letalidad

ggsave("Gráficos/letalidadH.png",Letalidad, bg = "transparent", height = 25, width = 25, units = "cm", type = "cairo")
