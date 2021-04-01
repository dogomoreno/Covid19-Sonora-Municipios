sum# Compute percentages
Sonora.DF.hoy$fraction = Sonora.DF.hoy$Casos.confirmados / sum(Sonora.DF.hoy$Casos.confirmados)

# Compute the cumulative percentages (top of each rectangle)
Sonora.DF.hoy$ymax = cumsum(Sonora.DF.hoy$fraction)

# Compute the bottom of each rectangle
Sonora.DF.hoy$ymin = c(0, head(Sonora.DF.hoy$ymax, n=-1))
# Compute label position
Sonora.DF.hoy$labelPosition <- (Sonora.DF.hoy$ymax + Sonora.DF.hoy$ymin) / 2
library(ggrepel)
Estatus2 <- ggplot(Sonora.DF.hoy, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill= Estatus, color=Estatus))+
  geom_rect()  +
  geom_text_repel( x=5, aes(y=labelPosition, label=label, color=Estatus), size=4, family="Lato Black", 
                   point.padding = unit(1.8, "lines"),  direction = "y", force = 0, seed = 42,  hjust = 0) +
  annotate("text", x = -3, y = 0, label = paste0("CASOS", "\n", "CONFIRMADOS", "\n", prettyNum(as.numeric(sum(Sonora.DF.hoy$Casos.confirmados)), big.mark=","), sep=""), 
           family="Lato Black", size= 8, color="black" ) +
  xlim(c(-3, 5)) +
  coord_polar(theta = "y", start = pi / 3, clip = "off") +
  scale_fill_manual(values= c("#9BD7D7", "#D175A3", "#FAC090", "#95B3D7")) +
  scale_color_manual(values=c("#01A2AC", "#993366", "#F79646", "#4BACC6" )) + 
  theme_void() +
  theme(axis.line = element_blank(),
        plot.margin = margin(0.8, 0.8, 0.3, 0.6, "cm"),
        plot.title = element_text(family = "Lato Black", size = 20,color = "#01A2AC"),  
        plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
        axis.text = element_blank(),   panel.grid= element_blank(),
        plot.background = element_rect(fill = "white", color = "black", size = 3),
        axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
        plot.caption = element_text(family = "Lato", size = 6, color = "#01A2AC"),
        legend.text = element_text(family = "Lato", size = 8),
        legend.position = "none",  legend.justification="left") +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "Estatus de los casos confirmados\nde Covid-19 en Sonora", 
       subtitle= Fechahoy, caption ="Fuente: Secretaría de Salud del Estado de Sonora\nwww.luisarmandomoreno.com")
Estatus2

ggsave("Gráficos/SemEst2.png",Estatus2 , width = 6, height =6.9 , type = "cairo", dpi = 300)