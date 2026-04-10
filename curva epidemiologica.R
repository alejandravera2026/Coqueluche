data1 <- data %>% filter(CLASIFICACION_MANUAL !="Caso invalidado por epidemiología" &
                           ANIO_MIN_INTERNACION == 2025)

curva_epidemiologica <- data1 %>%
  group_by(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION) %>%
  summarise(n = n ())%>%
  ungroup()

curva_epidemiologica <- curva_epidemiologica %>%
  complete(ANIO_MIN_INTERNACION, 
           SEPI_MIN_INTERNACION = 1: 53,
           fill = list (n=0))
curva_epidemiologica <- curva_epidemiologica %>%
  mutate(SE = str_pad(SEPI_MIN_INTERNACION, width = 2, side = "left", pad = "0"))

colores <- c("#377eb8")
library(ggplot2)       
grafico_curva <-  ggplot(curva_epidemiologica, aes(y= n, x = SE)) +
  geom_col(fill = colores) +
  labs(title = "Casos de IRAG e IRAG por semana epidemiológica-2025",
       caption = "Fuente SNVS 2.0",
       x = "Semana epidemiológica",
       y=  " Casos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 9, vjust = 0.5))

grafico_curva
