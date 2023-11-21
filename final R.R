# paquetes

install.packages("languageserver")
install.packages("tidyverse")
install.packages("duckdb")
install.packages("arrow")

library("duckdb")
library(ggplot2)
library(dplyr)
library(arrow)
library(tidyverse)

generales <- read_parquet("C:\\Users\\ianda\\Downloads\\ResultadosElectorales_1v.parquet")

PASO <- read_parquet("C:\\Users\\ianda\\Downloads\\ResultadosElectorales_PASO_2023.parquet")

dataAgrupado = read.csv("S:\\Github\\infoviz-TP\\ResultadosElectoralesPres.csv")

con <- duckdb::dbConnect(duckdb())

# archivos
# ResultadosElectorales_PASO_2023.parquet
# ResultadosElectorales_1v.parquet

# ----------------------------------------------------------------------------------

# preguntas abril

# pregunta 1
# entre las PASO y las generales, ¿fue mas gente a votar?

preg_1 <- dbGetQuery(con, "SELECT eleccion_tipo, sum(votos_cantidad) as votos FROM read_parquet('ResultadosElectorales_*.parquet') where cargo_id = 1 group by 1 order by 2;")
preg_1

preg_1_plot <- ggplot(data = preg_1, aes(y = eleccion_tipo, x = votos)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma_format()) + # Formatear los números en el eje x con comas
  geom_text(aes(label = scales::comma(votos), y = eleccion_tipo),
    vjust = 0.5,
    color = "white",
    fontface = "bold",
    size = 4,
    position = position_stack(vjust = 0.5)
  ) # Posicionar el texto en el centro de la barra

preg_1_plot

# pregunta 2
# ¿Cuáles fueron las agrupaciones más votadas en cada elección?

preg_2 <- dbGetQuery(con, "SELECT
  eleccion_tipo,
  CASE
    WHEN agrupacion_nombre = 'FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD' THEN 'FRENTE DE IZQUIERDA'
    ELSE agrupacion_nombre
  END as agrupacion_nombre,
  sum(votos_cantidad) as votos
FROM read_parquet('ResultadosElectorales_*.parquet')
WHERE cargo_id = 1 AND agrupacion_nombre IS NOT NULL
GROUP BY eleccion_tipo, agrupacion_nombre
ORDER BY 3 DESC, 1, 2 -- Ordenar por votos descendentes, eleccion_tipo y agrupacion_nombre
LIMIT 10;")

preg_2

colores_personalizados <- c("steelblue", "#FCBF49")

preg_2_plot <- ggplot(preg_2, aes(x = agrupacion_nombre, y = votos, fill = eleccion_tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Top 5 Agrupaciones con Más Votos por Tipo de Elección",
    y = "Votos",
    fill = "Tipo de Elección"
  ) +
  scale_y_continuous(labels = scales::comma) + # Formato de etiquetas con comas
  scale_fill_manual(values = colores_personalizados) + # Colores personalizados
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1), axis.title.x = element_blank()) # Ajuste del tamaño y ángulo de las etiquetas


preg_2_plot


# pregunta 3
# de las 5 provincias con más votos: entre las PASO y las generales, ¿fue mas gente a votar?

preg_3 <- dbGetQuery(con, "
  SELECT eleccion_tipo, distrito_nombre, SUM(votos_cantidad) as votos
  FROM read_parquet('ResultadosElectorales_*.parquet')
  WHERE cargo_id = 1
    AND distrito_nombre IN ('Buenos Aires', 'Córdoba', 'Santa Fe', 'Ciudad Autónoma de Buenos Aires', 'Mendoza')
  GROUP BY eleccion_tipo, distrito_nombre
  ORDER BY distrito_nombre, votos DESC
")

preg_3

preg_3 <- preg_3 %>%
  mutate(distrito_nombre = ifelse(distrito_nombre == "Ciudad Autónoma de Buenos Aires", "CABA", distrito_nombre))


preg_3_plot <- ggplot(preg_3, aes(x = distrito_nombre, y = votos, fill = eleccion_tipo)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  labs(
    title = "Top 5 provincias con más votos según el tipo",
    x = "Distrito",
    y = "Cantidad de Votos"
  ) +
  scale_y_continuous(labels = scales::comma) + # Formato de etiquetas con comas
  scale_fill_manual(values = colores_personalizados) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

preg_3_plot

# pregunta 4
# CABA: votos no positivos

preg_4 <- dbGetQuery(con, "
  SELECT eleccion_tipo, votos_tipo, distrito_nombre, SUM(votos_cantidad) as votos
  FROM read_parquet('ResultadosElectorales_*.parquet')
  WHERE cargo_id = 1
    AND votos_tipo != 'POSITIVO' AND distrito_nombre IN ('Ciudad Autónoma de Buenos Aires')
  GROUP BY eleccion_tipo, distrito_nombre, votos_tipo
  ORDER BY distrito_nombre, votos DESC
")
preg_4

nuevo_orden <- c("PASO", "GENERAL")

preg_4$eleccion_tipo <- factor(preg_4$eleccion_tipo, levels = nuevo_orden)

preg_4_plot <- ggplot(preg_4, aes(x = eleccion_tipo, y = votos, group = votos_tipo, color = votos_tipo)) +
  geom_line(size = 1) +
  labs(
    title = "CABA: votos no positivos",
    y = "Cantidad de Votos"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

print(preg_4_plot)

# pregunta 5
# cual es la relacion entre frente de todos y el resto de las agrupaciones usando un dotplot por mesa. el eje y es los votos de frete de todos y el eje x es los votos del resto de las agrupaciones


dataAgrupado %>%
  filter(agrupacion_nombre %in% c("UNION POR LA PATRIA", "LA LIBERTAD AVANZA")) %>%
  select(mesa_electores, agrupacion_nombre, votos_cantidad) %>%
  spread(agrupacion_nombre, votos_cantidad, fill = 0) %>%
  ggplot(aes(x = "UNION POR LA PATRIA", y = "LA LIBERTAD AVANZA")) +
  geom_point() +
  labs(
    title = "Relationship between UNION POR LA PATRIA and LA LIBERTAD AVANZA",
    x = "Votes of UNION POR LA PATRIA",
    y = "Votes of LA LIBERTAD AVANZA"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ids = dataAgrupado %>%
    group_by(eleccion_tipo,id, mesa_electores) %>%
    summarise(votos = sum(votos_cantidad)) %>%
    #mostrar los que tienen mas votos que mesa_electores
    filter(votos > mesa_electores)%>%
    #Selecciona la columna id
    select(id) %>%
    #Convierte a vector
    pull()

