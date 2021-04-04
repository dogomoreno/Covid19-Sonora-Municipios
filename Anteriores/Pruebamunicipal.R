
CasosDecesosdia <- filter(CasosDecesos,Fecha==max(as.Date(Fecha)))




mun <- "Hermosillo"

CasosDecesosmun <- CasosDecesos %>% filter(MUNICIPIO==mun) 
CasosDecesosdia <- filter(CasosDecesosmun,Fecha==max(as.Date(Fecha)))

p1 <- ggplot(CasosDecesosmun) +
  geom_area(aes(x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(aes(x= Fecha, y= Casos.diarios, color = "Casos diarios"), fill= "#01787E", size = 0.9, stroke=0.4, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#58BCBC", "Casos diarios" = "#01787E")) + 
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#01787E", "Casos diarios" = "white")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(expand=c(0,0), date_breaks = "1 month", date_labels = "%B") +
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 8), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.9),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = paste0("<span style = 'color:#01A2AC';>Casos confirmados acumulados: ", prettyNum(as.numeric(max(CasosDecesosmun$CASOS)), big.mark=",", preserve.width="none"),"</span>"), 
       subtitle= "Casos confirmados diariamente", caption =NULL)

p2 <- ggplot(CasosDecesosmun) +
  geom_area(aes(x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(aes(x= Fecha, y= Decesos.diarios, color = "Decesos diarios"), fill= "#73264D", size = 0.9, stroke=0.4, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Decesos diarios" = "#73264D", "Tendencia promedio móvil 7 días" = "#D075A3")) + 
  scale_color_manual(name="", values= c("Decesos diarios" = "white","Tendencia promedio móvil 7 días" = "#73264D")) +
  scale_y_continuous(expand = c(0, 1)) +
  scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") + 
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 8), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.9),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  =  paste0("<span style = 'color:#993366';> Decesos confirmados acumulados: ", prettyNum(as.numeric(max(CasosDecesosmun$DECESOS)), big.mark=",", preserve.width="none"),"</span>"), 
       subtitle= "Decesos confirmados diariamente", caption =NULL)

patchwork <- (p1 / p2)
p3 <- patchwork + plot_annotation(
  title = paste0("<span style = 'font-size:12pt'>Covid-19 en Sonora:</span><br>",mun),
  subtitle = Fechahoy,
  caption = fuente, theme= theme(
    plot.title = element_markdown(family = "Lato Black", size = 30),  
    plot.subtitle = element_text(family = "Lato Light", size = 12, color = "black"),
    plot.caption = element_text(family = "Lato", size = 8), plot.title.position = 'plot', 
    plot.caption.position = 'plot', plot.margin = margin(10, 25, 10, 25), 
    plot.background = element_rect(fill = "white", color = "black", size = 3)))

p3
ggsave(paste0("Gráficos municipales/", mun,".png"),p3, width = 5 * (16/9), height = 10, type = "cairo", dpi = 300)
