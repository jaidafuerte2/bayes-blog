################################
##                            ##
## Colección 2 - Entrada 1    ##
##                            ##
################################

# 1. Cargar los datos
options(scipen = 999)
library(tidyverse)
library(ggplot2)
library(rstanarm)
library(posterior)
library(bayesplot)
library(skimr)

# Importar el conjunto de datos

superstore <- read_csv(
  "blog_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

# 2. Un vistazo a las variables principales

superstore |>
  select(Sales, Discount) |>
  skim()

# 3. Conocer cómo se distribuyen las ventas

ggplot(superstore,
       aes(x = Sales)) +
  geom_histogram(
    bins = 40,
    color = "white"
  ) +
  labs(
    title = "Distribución de las ventas",
    x = "Ventas",
    y = "numero de pedidos"
  )

# Conocer cómo se distribuyen las ventas con zoom
ggplot(superstore,
       aes(x = Sales)) +
  geom_histogram(
    bins = 40,
    color = "white"
  ) +
  coord_cartesian(ylim = c(0, 500)) +
  labs(
    title = "Distribución de las ventas",
    x = "Ventas",
    y = "Número de pedidos"
  )

# 4. ¿Qué descuentos aparecen con mayor frecuencia?

ggplot(superstore,
       aes(x = Discount)) +
  geom_histogram(
    bins = 20,
    color = "white"
  ) +
  labs(
    title = "Distribución de los descuentos",
    x = "Descuento",
    y = "Número de pedidos"
  )

# Con zoom
ggplot(superstore,
       aes(x = Discount)) +
  geom_histogram(
    bins = 20,
    color = "white"
  ) +
  coord_cartesian(ylim = c(0, 500)) +
  labs(
    title = "Distribución de los descuentos",
    x = "Descuento",
    y = "Número de pedidos"
  )

# 5. Conocer relación visible entre descuentos y ventas

ggplot(
  superstore,
  aes(
    x = Discount,
    y = Sales
  )
) +
  geom_point(alpha = 0.30) +
  labs(
    title = "Ventas frente a descuentos",
    x = "Descuento",
    y = "Ventas"
  )

# 6. Ajustando el modelo

modelo_sales <- stan_glm(
  Sales ~ Discount,
  data = superstore,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  seed = 1234,
  refresh = 0
) 
resumen <- print(modelo_sales, digits = 2)



#            Median MAD_SD
#(Intercept) 243.25   7.47
#Discount    -84.90  31.93
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 623.26   4.04

# 7. Conocer el intervalo posterior

intervalos <- posterior_interval(
  modelo_sales,
  prob = 0.95
) # produce:
#                 2.5%     97.5%
#(Intercept)  228.1053 258.51819
#Discount    -142.5592 -27.94353
#sigma        614.6660 631.86259

# 8. Conocer la incertidumbre del efecto del descuento sobre las 
# ventas

draws <- as.matrix(modelo_sales) # produce:
#    parameters
#iterations (Intercept)    Discount    sigma
#      [1,]    239.7658  -73.139692 623.1477
#      [2,]    243.8462  -85.106846 621.7437
#      [3,]    239.5412  -71.170475 625.2981
#      [4,]    237.9793  -67.608776 627.9122
#      [5,]    246.1767 -107.280282 627.1876
#      [6,]    246.3900 -112.837030 627.2603
#...
#      [331,]    252.0345 -113.812647 619.8824
#      [332,]    239.0272  -77.288160 616.3038
#      [333,]    241.0389  -60.126147 623.8257
#[ reached 'max' / getOption("max.print") -- omitted 667 rows ]

colnames(draws)

coeficientes <- tibble(
  Parametro = colnames(draws),
  Mediana   = apply(draws, 2, median),
  MAD_SD    = apply(draws, 2, mad)
)
coeficientes



# 8. Visualizar la distribución del intervalo posterior
mcmc_areas(
  as.matrix(modelo_sales),
  pars = "Discount",
  prob = 0.95
)

# 9. Dividir el intervalo de mínimo y máximo descuento en 100 cortes
# en intervalos iguales
newdata <- tibble(
  Discount = seq(
    min(superstore$Discount),
    max(superstore$Discount),
    length.out = 100
  )
)
newdata # produce:
# A tibble: 100 × 1
#   Discount
#    <dbl>
#1  0      
#2  0.00808
#3  0.0162 
#4  0.0242 
#....
#8  0.0566 
#9  0.0646 
# ℹ 90 more rows
# ℹ Use `print(n = ...)` to see more rows

# 10. Dados 100 cortes (columnas) de descuento , qué ventas (1000)
# filas se esperan para cada uno de los 100 niveles de descuento (100
# columnas o cortes).

predicciones <- posterior_epred(
  modelo_sales,
  newdata = newdata
)
predicciones # produce:
#iterations  1        2        3        4        5        6
#[1,] 239.7658 239.1748 238.5838 237.9928 237.4017 236.8107
#[2,] 243.8462 243.1585 242.4708 241.7830 241.0953 240.4076
#[3,] 239.5412 238.9661 238.3910 237.8158 237.2407 236.6656
#[4,] 237.9793 237.4330 236.8867 236.3403 235.7940 235.2477
#[5,] 246.1767 245.3098 244.4429 243.5760 242.7090 241.8421
#[6,] 246.3900 245.4782 244.5664 243.6546 242.7428 241.8310
#[7,] 232.5069 232.0454 231.5840 231.1225 230.6611 230.1996
#[8,] 247.0019 246.0069 245.0120 244.0170 243.0221 242.0272
#[9,] 238.2181 237.5568 236.8955 236.2342 235.5729 234.9116
#[10,] 238.9439 238.1978 237.4517 236.7056 235.9594 235.2133
#[ reached 'max' / getOption("max.print") -- omitted 990 rows, 94 cols ]

# 11. Dados 100 descuentos crear una variable con la media de las 
# ventas, otra variable con el límite inferior de las ventas y
# otra variable con el límite superior de las ventas

newdata <- newdata |>
  mutate(
    media = apply(predicciones, 2, mean),
    li = apply(predicciones, 2, quantile, probs = 0.025),
    ls = apply(predicciones, 2, quantile, probs = 0.975)
  )
newdata # produce:
# A tibble: 100 × 4
#  Discount media    li    ls
#    <dbl> <dbl> <dbl> <dbl>
#1  0        243.  228.  259.
#2  0.00808  242.  228.  258.
#3  0.0162   242.  227.  257.
#4  0.0242   241.  227.  256.
#5  0.0323   240.  226.  255.
#6  0.0404   240.  226.  254.
#7  0.0485   239.  226.  253.
#8  0.0566   238.  225.  252.
#9  0.0646   238.  225.  251.
#10  0.0727   237.  224.  250.
# ℹ 90 more rows
# ℹ Use `print(n = ...)` to see more rows

# 12. # Conocer la relación estimada entre descuentos y ventas
ggplot(superstore,
       aes(
         x = Discount,
         y = Sales
       )) +
  geom_point(alpha = 0.20) +
  geom_ribbon(
    data = newdata, # ya no uso suerstore sino la tabla newdata
    aes(
      x = Discount,
      ymin = li,
      ymax = ls
    ),
    inherit.aes = FALSE, # importante porque ya no uso superstore
                         # sino newdata
    alpha = 0.20
  ) +
  geom_line(
    data = newdata, # No uso superstore sino newdata
    aes(y = media),
    linewidth = 1
  ) +
  labs(
    title = "Relación estimada entre descuentos y ventas",
    x = "Descuentos",
    y = "Ventas"
  )

# Conocer la relación estimada entre descuentos y ventas con zoom
# en el eje y
ggplot(superstore,
       aes(
         x = Discount,
         y = Sales
       )) +
  geom_point(alpha = 0.20) +
  geom_ribbon(
    data = newdata,
    aes(
      x = Discount,
      ymin = li,
      ymax = ls
    ),
    inherit.aes = FALSE,
    alpha = 0.20
  ) +
  geom_line(
    data = newdata,
    aes(y = media),
    linewidth = 1
  ) +
  coord_cartesian(ylim = c(100, 300)) +
  labs(
    title = "Relación estimada entre descuentos y ventas",
    x = "Descuento",
    y = "Ventas"
  )

# Guardar todo
saveRDS(resumen, "resumen.rds")

saveRDS(intervalos, "intervalos.rds")

saveRDS(draws, "draws.rds")

saveRDS(newdata, "newdata.rds")

saveRDS(predicciones, "blog_ba/R/col2-1/predicciones.rds")

saveRDS(coeficientes, "blog_ba/R/col2-1/coeficientes.rds")
