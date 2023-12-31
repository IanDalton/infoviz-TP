# paquetes

install.packages("languageserver")
install.packages("tidyverse")
install.packages("duckdb")

library("duckdb")
library(ggplot2)
library(dplyr)

con <- duckdb::dbConnect(duckdb())

# ------------------------------------------------------------------------------

# pregunta 1
# Entre las PASO, generales y segunda vuelta,  ¿fue mas gente a votar?

preg_1 <- dbGetQuery(con, "SELECT eleccion_tipo, sum(votos_cantidad) as votos FROM read_parquet('ResultadosElectorales_*.parquet') where cargo_id = 1 group by 1 order by 2;")
preg_1

orden_deseado <- c("SEGUNDA VUELTA","GENERAL","PASO")
preg_1$eleccion_tipo <- factor(preg_1$eleccion_tipo, levels = orden_deseado)

preg_1_plot <- ggplot(data=preg_1, aes(y=eleccion_tipo, x=votos)) +
  geom_bar(stat="identity", fill="#6994A8") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma_format()) +
  geom_text(aes(label = scales::comma(votos), y = eleccion_tipo),
            vjust = 0.5,  
            color = "white",
            fontface = "bold",
            size = 4,
            position = position_stack(vjust = 0.5)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

print(preg_1_plot)

# pregunta 2
# ¿Cuál fue la variación de votos que tuvieron las agrupaciones?

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
ORDER BY 3 DESC, 1, 2 
LIMIT 10;")

preg_2

orden_tipo_eleccion <- c("SEGUNDA VUELTA","GENERAL","PASO")
preg_2$eleccion_tipo <- factor(preg_2$eleccion_tipo, levels = orden_tipo_eleccion)

colores_personalizados <- c('#215974','#6994A8','#C7C8C8')

preg_2 <- preg_2 %>%
  mutate(agrupacion_nombre = reorder(agrupacion_nombre, votos))

preg_2_plot <- ggplot(preg_2, aes(x = votos, y = agrupacion_nombre, fill = eleccion_tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = NULL,
    fill = NULL
  ) +
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = colores_personalizados) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6, angle = 0, hjust = 1),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5))

print(preg_2_plot)

# pregunta 3
# De las 5 provincias con más votos: entre cada elección, ¿fue mas gente a votar?

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

colores_personalizados <- c('#215974','#6994A8','#C7C8C8')

orden_tipo_eleccion <- c("SEGUNDA VUELTA","GENERAL","PASO")
preg_3$eleccion_tipo <- factor(preg_3$eleccion_tipo, levels = orden_tipo_eleccion)

preg_3$distrito_nombre <- factor(preg_3$distrito_nombre, levels = c('Córdoba', 'Santa Fe', 'Mendoza', 'CABA', 'Buenos Aires'))

preg_3 <- preg_3 %>%
  mutate(distrito_nombre = reorder(distrito_nombre, votos))

preg_3_plot <- ggplot(preg_3, aes(x = votos, y = distrito_nombre, fill = eleccion_tipo)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = colores_personalizados) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6, angle = 0, hjust = 1),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

print(preg_3_plot)

# pregunta 4
# Entre las PASO, generales y segunda vuelta,¿cuantos votos no positivos hubo?

preg_4 <- dbGetQuery(con, "
  SELECT eleccion_tipo, votos_tipo, SUM(votos_cantidad) as votos
  FROM read_parquet('ResultadosElectorales_*.parquet')
  WHERE cargo_id = 1
    AND votos_tipo != 'POSITIVO' 
  GROUP BY eleccion_tipo, votos_tipo
  ORDER BY votos DESC
")
preg_4

nuevo_orden <- c("PASO", "GENERAL","SEGUNDA VUELTA") 

preg_4$eleccion_tipo <- factor(preg_4$eleccion_tipo, levels = nuevo_orden)

preg_4_plot <- ggplot(preg_4, aes(x = eleccion_tipo, y = votos, group = votos_tipo, color = votos_tipo)) +
  geom_line(size = 1) +
  labs(
       y = NULL,
       x = NULL) +
  scale_y_continuous(labels = scales::comma)
  theme_minimal() +   
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1), axis.title.x = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))

print(preg_4_plot)
