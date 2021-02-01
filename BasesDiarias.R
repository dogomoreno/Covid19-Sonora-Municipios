# Paquetes
library(tidyverse)
library(lubridate)


# Casos Diarios
Casosdiarios <- read_csv("Bases/ST_CasosMunicipalesSonora.csv",
                         locale = locale(encoding = "ISO-8859-1"))
Casosdiarios[is.na(Casosdiarios)] <- 0
Casosdiarios<- Casosdiarios %>%
  rename(CVEGEO=CVE_MUN, MUNICIPIO = NOM_MUN) %>% 
  gather( key= "Fecha", value= "CASOS", ends_with(c("2020","2021"))) %>%
  mutate(Fecha = as.Date(Fecha,format = "%d/%m/%Y")) %>% 
  group_by(MUNICIPIO, CVEGEO) %>% 
  mutate(NUEVOS= (CASOS - lag(CASOS, default = 0, order_by=Fecha)))%>% 
  select (Fecha, CVEGEO, MUNICIPIO, CASOS, NUEVOS) %>% 
  write.csv('Bases/Casosdiarios.csv')

# Decesos Diarios
Decesosdiarios <- read_csv("Bases/ST_DecesosMunicipalesSonora.csv",
                         locale = locale(encoding = "ISO-8859-1"))
Decesosdiarios[is.na(Decesosdiarios)] <- 0
Decesosdiarios<- Decesosdiarios %>%
  rename(CVEGEO=CVE_MUN, MUNICIPIO = NOM_MUN) %>% 
  gather( key= "Fecha", value= "DECESOS", ends_with(c("2020","2021"))) %>%
  mutate(Fecha = as.Date(Fecha,format = "%d/%m/%Y")) %>% 
  group_by(MUNICIPIO, CVEGEO) %>% 
  mutate(NUEVOS= DECESOS - lag(DECESOS, default = 0, order_by=Fecha)) %>% 
  select (Fecha, CVEGEO, MUNICIPIO, DECESOS, NUEVOS) %>% 
  write.csv('Bases/Decesosdiarios.csv')

