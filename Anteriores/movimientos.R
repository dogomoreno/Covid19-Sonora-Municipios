# Mapa de movimientos
capa_munison <- readOGR("Shapes", layer="MUNSON")
capa_son <- readOGR("Shapes", layer="ENTSON")
capa_munison_df <- fortify(capa_munison, region="CVEMUN")
capa_munison_casos<- inner_join(capa_munison_df, casosacumdia2, by="id")
capa_munison_casos <- mutate(capa_munison_casos, movimiento=if_else(NUEVOS>0,1,0))

discrete <- c("0" = "#58bcbc", "1" = "#01787e")

Mapa_mov <- ggplot(capa_munison_casos, aes(map_id = id)) +
  geom_polygon(data=capa_son, aes(x=long, y=lat, group=group), 
               fill="transparent", color="black", size=1.5) +
  geom_polygon(data=capa_munison, aes(x=long, y=lat, group=group), 
               fill="gray90", color="white", size=0.3) +
  geom_map_interactive(aes(fill = factor(movimiento)),color = "white",size=0.5, map = capa_munison_df) + 
  scale_fill_manual(values = discrete, breaks= c("0", "1"))+
  theme_void()+
  theme(plot.title = (element_text(family = "Lato Black", size = 20, color = "black")),
        plot.subtitle = (element_text(family = "Lato Light", size = 8, color = "#01787E")),
        plot.margin = margin(0.5, 0.5, 0.25, 0.4, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.key.height = unit (0.5, "cm"), legend.key.width = unit (0.3, "cm"), axis.text = element_blank(),
        legend.text = element_text(family = "Lato", size = 6, color = "black"),
        legend.title = element_text(family = "Lato Black", size = 5, color = "black"),
        plot.caption = element_text(family = "Lato Light", size = 6, color = "gray40"),
        axis.title = element_blank()) +
  labs(y = NULL, x = NULL, title  = NULL, 
       subtitle = NULL,  fill = NULL, 
       caption = NULL)+
  ggsave("Gráficos/movimientos.png",Mapa_mov, bg = "transparent", height = 25.9, width = 25.9, units = "cm", type = 'cairo')
