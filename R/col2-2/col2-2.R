################################
##                            ##
## Colección 2 - Entrada 2    ##
##                            ##
################################

# 0. Cargar las librerías

options(scipen = 999) # Evitar la notación científica

library(tidyverse)
library(rstanarm)
library(ggplot2)
library(bayesplot)
library(posterior)
library(gt)

# Cargar la tabla de superstores sales
superstore <- read_csv(
  "blog_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

# 1. Distribución de las ganancias
ggplot(superstore,
       aes(x = Profit)) +
  geom_histogram(
    bins = 40,
    fill = "#2C7FB8",
    color = "white"
  ) +
  labs(
    title = "Distribución de las ganancias",
    x = "Profit",
    y = "Número de ventas"
  )

# Con zoom
ggplot(superstore,
       aes(x = Profit)) +
  geom_histogram(
    bins = 40,
    fill = "#2C7FB8",
    color = "white"
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Distribución de las ganancias",
    x = "Profit",
    y = "Número de ventas"
  )

# 2. Distribución de los descuentos
ggplot(superstore,
       aes(x = Discount)) +
  geom_histogram(
    bins = 20,
    fill = "#41AE76",
    color = "white"
  ) +
  labs(
    title = "Distribución de los descuentos",
    x = "Descuento",
    y = "Número de ventas"
  )

# Con zoom
ggplot(superstore,
       aes(x = Discount)) +
  geom_histogram(
    bins = 20,
    fill = "#41AE76",
    color = "white"
  ) +
  coord_cartesian(ylim = c(0, 500)) +
  labs(
    title = "Distribución de los descuentos",
    x = "Discount",
    y = "Número de ventas"
  )

# 3. Relación del descuento con las ganancias
ggplot(
  superstore,
  aes(
    x = Discount,
    y = Profit
  )
) +
  geom_point(
    alpha = 0.20,
    color = "#2C7FB8"
  ) +
  labs(
    title = "Ganancias y descuentos",
    x = "Descuento",
    y = "Ganancias"
  )

# Con zoom
ggplot(
  superstore,
  aes(
    x = Discount,
    y = Profit
  )
) +
  geom_point(
    alpha = 0.20,
    color = "#2C7FB8"
  ) +
  coord_cartesian(ylim = c(-500, 500)) +
  labs(
    title = "Ganancias y descuentos",
    x = "Descuento",
    y = "Ganancias"
  )

# 4. Modelo para conocer cómo afectan el descuento sobre las 
# ganancias 
modelos_profit <- stan_glm(
  Profit ~ Discount,
  data = superstore,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  seed = 1234,
  refresh = 0
)
modelo_profit # produce:
#             Median MAD_SD
#(Intercept)   67.7    2.8
#Discount    -249.6   10.5
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 228.5    1.7 

# 5. Crear una tablas con 100 divisiones o cortes al intervalo
# del descuento mayor y menor
nuevo <- tibble(
  Discount = seq(
    min(superstore$Discount),
    max(superstore$Discount),
    length.out = 100
  )
)
nuevo # produce:
# A tibble: 100 × 1
#   Discount
#   <dbl>
#1  0      
#2  0.00808
#3  0.0162 
#4  0.0242 
#5  0.0323 
#6  0.0404 
#7  0.0485 
#8  0.0566 
#9  0.0646 
#10  0.0727 
# ℹ 90 more rows
# ℹ Use `print(n = ...)` to see more rows

# 6. A partir de los 100 cortes del intervalo del descuento, crear
# 1000 predicciones para cada uno de los 100 cortes según el modelo
pred_media <- posterior_linpred( # linpred viene de predicción
  modelo_profit,
  newdata = nuevo
)
pred_media # produce:
#iterations        1        2        3        4        5        6
#      [1,] 66.21591 64.33713 62.45835 60.57957 58.70080 56.82202
#      [2,] 68.40164 66.27222 64.14281 62.01339 59.88398 57.75456
#      [3,] 66.67375 64.58503 62.49631 60.40759 58.31888 56.23016
#      [4,] 67.73225 65.69996 63.66767 61.63538 59.60309 57.57080
#      ...
#      [8,] 69.22587 67.18135 65.13683 63.09231 61.04779 59.00327
#      [9,] 71.09846 68.93679 66.77511 64.61344 62.45176 60.29008
#[ reached 'max' / getOption("max.print") -- omitted 991 rows, 94 cols]

# 7. Crear la media , el límite inferior y el límite superior de las
# 1000 proyecciones de cada unos de los 100 cortes del intervalo del
# descuento
nuevo <- nuevo |>
  mutate(
    media = apply(pred_media, 2, median), # 2 para calcular en base
                                        # a las columnas
    li = apply(pred_media, 2, quantile, probs = 0.05),
    ls = apply(pred_media, 2, quantile, probs = 0.95)
  )
nuevo # produce:
# A tibble: 100 × 4
#  Discount media    li    ls
#     <dbl> <dbl> <dbl> <dbl>
#1  0        67.7  63.0  72.4
#2  0.00808  65.7  61.0  70.2
#3  0.0162   63.7  59.1  68.1
#4  0.0242   61.7  57.1  66.0
#...
#8  0.0566   53.6  49.4  57.6
#9  0.0646   51.5  47.4  55.5
# ℹ 90 more rows
# ℹ Use `print(n = ...)` to see more rows

# 8. Diagrama de dispersión que relaciona el descuento con las 
# ganancias
ggplot(superstore,
       aes(
         x = Discount,
         y = Profit
       )) +
  geom_point(
    alpha = 0.20,
    color = "gray50"
  ) +
  # Crear una cinta con la dispersión de la tendencia del descuento
  # y las ganancias
  geom_ribbon(
    data = nuevo,
    aes(
      x = Discount,
      ymin = li,
      ymax = ls
    ),
    inherit.aes = FALSE, # Para no heredar Profit de superstore
    alpha = 0.20,
    fill = "#2C7FB8"
  ) +
  # Línea de tendencia del descuento y las ganancias
  geom_line(
    data = nuevo,
    aes(
      x = Discount,
      y = media
    ),
    inherit.aes = FALSE, # Significa no heredar las estéticas de
                         # ggplot 
    linewidth = 1,
    color = "#2C7FB8"
  ) +
  labs(
    title = "Relación estimada entre descuentos y ganancias",
    x = "Descuento (%)",
    y = "Ganancia"
  )
  
# Con zoom
ggplot(superstore,
       aes(
         x = Discount,
         y = Profit
       )) +
  geom_point(
    alpha = 0.20,
    color = "gray50"
  ) +
  # Crear una cinta con la dispersión de la tendencia del descuento
  # y las ganancias
  geom_ribbon(
    data = nuevo,
    aes(
      x = Discount,
      ymin = li,
      ymax = ls
    ),
    inherit.aes = FALSE,
    alpha = 0.20,
    fill = "#2C7FB8"
  ) +
  # Línea de tendencia del descuento y las ganancias
  geom_line(
    data = nuevo,
    aes(
      x = Discount,
      y = media
    ),
    inherit.aes = FALSE,
    linewidth = 1,
    color = "#2C7FB8"
  ) +
  coord_cartesian(ylim = c(-150, 100)) +
  labs(
    title = "Relación estimada entre descuentos y ganancias",
    x = "Descuento (%)",
    y = "Ganancia"
  )

# 9 . Intervalo posterior del modelo del efecto del descuento sobre
# las ganancias
intervalos <- posterior_interval(
  modelos_profit,
  prob = 0.95
)
intervalos # produce:
#                  2.5%      97.5%
#(Intercept)   62.03286   73.10062
#Discount    -270.25999 -226.60977
#sigma        225.27095  231.69106

# 10. Gráfico de la distribución del modelo de disminución de las 
# ganancias según el descuento
mcmc_areas(
  as.matrix(modelo_profit),
  pars = "Discount",
  prob = 0.80, # Pinta el 80% central de la distribución
  prob_outer = 0.95 # Pinta otro intervalo hasta el 95% central 
)

# Devolver el modelo como si fuera una matriz
draws <- as.matrix(modelo_profit) # produce:
draws
#           parameters
#iterations (Intercept)  Discount    sigma
#      [1,]    66.21591 -232.4988 228.9274
#      [2,]    68.40164 -263.5152 227.4887
#      [3,]    66.67375 -258.4789 228.9514
#      [4,]    67.73225 -251.4958 228.1232
#      ...
#      [8,]    69.22587 -253.0093 228.8049
#      [9,]    71.09846 -267.5075 228.2164
# [ reached 'max' / getOption("max.print") -- omitted 991 rows ]

# 11. Tabla del Intercepto, el descuento y sigma
coeficientes <- as_draws_df(modelo_profit) |>
  summarise(
    Intercepto = median(`(Intercept)`),
    Descuento = median(Discount),
    Sigma = median(sigma)
  )
coeficientes # produce:
# Intercepto Descuento Sigma
#      <dbl>     <dbl> <dbl>
#1       67.7     -250.  228.

# Pasar los resultados del modelo a formato de tabla
as_draws_df(modelo_profit) # produce:
# A draws_df: 500 iterations, 2 chains, and 3 variables
#   (Intercept) Discount sigma
#1           66     -232   229
#2           68     -264   227
#3           67     -258   229
#4           68     -251   228
#...
#9           71     -268   228
#10          65     -230   229
# ... with 990 more draws
# ... hidden reserved variables {'.chain', '.iteration', '.draw'}

# Cambiar de ejes a los coeficientes
coeficientes <- coeficientes |>
  pivot_longer(
    everything(), # Usa todas las cols: Intercepto, Descuento y sigma
    names_to = "Parametro",
    values_to = "Mediana"
  )
coeficientes # produce:
# A tibble: 3 × 2
#  Parámetro  Mediana
#  <chr>        <dbl>
#1 Intercepto    67.7
#2 Descuento   -250. 
#3 Sigma        228. 

# 12. Poner a la tabla de resumen del modelo en formato bonito con 
# gt()
coeficientes |>
  gt() |>
  fmt_number(
    columns = Mediana,
    decimals = 2
  ) |>
  cols_label(
    Parametro = "Parámetro",
    Mediana = "Mediana posterior"
  )

############ Guardar los resultados en archivos ################

saveRDS(pred_media, "blog_ba/R/col2-2/pred_media.rds")
saveRDS(intervalos, "blog_ba/R/col2-2/intervalos.rds")
saveRDS(draws, "blog_ba/R/col2-2/draws.rds")
