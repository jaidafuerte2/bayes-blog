################################
##                            ##
## Colección 2 - Entrada 3    ##
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
library(dplyr)

# Cargar la tabla de superstores sales
superstore <- read_csv(
  "blog_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

# Distribución de las ganancias
ggplot(
  superstore,
  aes(x = Profit)
) +
  geom_histogram(
    bins = 40,
    fill = "#2C7FB8",
    color = "white"
  ) +
  labs(
    title = "Distribución de las ganancias",
    x = "Ganancia",
    y = "Número de pedidos"
  ) +
  theme_minimal()

# Con zoom
ggplot(
  superstore,
  aes(x = Profit)
) +
  geom_histogram(
    bins = 40,
    fill = "#2C7FB8",
    color = "white"
  ) +
  coord_cartesian(ylim = c(0, 500)) + 
  labs(
    title = "Distribución de las ganancias",
    x = "Ganancia",
    y = "Número de pedidos"
  ) +
  theme_minimal()

# Diagrama de dispersión de las ventas y las ganancias
ggplot(
  superstore,
  aes(
    x = Sales,
    y = Profit
  )
) +
  geom_point(
    alpha = 0.25,
    color = "#2C7FB8"
  ) +
  labs(
    title = "Ventas y ganancias",
    x = "Ventas",
    y = "Ganancias"
  ) + 
  theme_minimal()

# Con zoom
ggplot(
  superstore,
  aes(
    x = Sales,
    y = Profit
  )
) +
  geom_point(
    alpha = 0.25,
    color = "#2C7FB8"
  ) +
  coord_cartesian(ylim = c(-2000, 2000)) +
  labs(
    title = "Ventas y ganancias",
    x = "Ventas",
    y = "Ganancia"
  ) +
  theme_minimal()

# Diagrama del descuento y las ganancias
ggplot(
  superstore,
  aes(
    x = Discount,
    y = Profit
  ) 
) + geom_jitter(
  alpha = 0.20,
  width = 0.01,
  color = "#D95F02"
) +
  labs(
    title = "Descuentos y ganancias",
    x = "Descuento",
    y = "Ganancias"
  )

##################
##
## Modelo
##
##################

# Conocer el tipo de dato y los valores de la variable Category
class(superstore$Category) # produce: "character"
unique(superstore$Category) # produce:
#[1] "Furniture"       "Office Supplies" "Technology"  

# Cambiar el tipo de la variable de Category a superstore
superstore <- superstore |>
  mutate(
    Category = factor(Category)
  )

# Crear el modelo que indica como afectan a la ganancia las 
# variables: Discount, Sales, Quantity y Category 
modelo_profit <- stan_glm(
  Profit ~ Discount + Sales + Quantity + Category,
  data = superstore,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  seed = 1234,
  refresh = 0
)
modelo_profit # produce:
print_modelo <- print(modelo_profit)
#                        Median MAD_SD
#(Intercept)               -3.6    5.5
#Discount                -228.9    9.2
#Sales                      0.2    0.0
#Quantity                  -3.2    0.9
#CategoryOffice Supplies   50.4    5.0
#CategoryTechnology        41.4    6.4
#
#Auxiliary parameter(s):
#     Median MAD_SD
#sigma 198.7    1.3 


# Resumir el modelo
summary(modelo_profit) # Devuelve directamente una matriz

# Transformar la matriz del modelo a tabla
as.data.frame(summary(modelo_profit))

# Poner el modelo en formato de tabla, a la columna con los nombres
# de las variables le llamaremos "Variable" y renombramos a la
# columna con la media y la desviación estándar
coeficientes <- as.data.frame(summary(modelo_profit)) |>
  tibble::rownames_to_column("Variable") |>
  rename(
    Media = mean,
    SD = sd
  )
coeficientes # produce:
#                 Variable          Media          mcse          SD
#1             (Intercept)     -3.8075536 0.18370310310 5.504379380
#2                Discount   -229.3006302 0.21312667168 9.109250046
#3                   Sales      0.1845543 0.00008082798 0.003327364
#4                Quantity     -3.2265151 0.02318208147 0.887808894
#5 CategoryOffice Supplies     50.2496436 0.16804312504 5.066938847
#6      CategoryTechnology     41.3921266 0.20882182120 6.213761771
#7                   sigma    198.7415279 0.04813760392 1.350933854
#8                mean_PPD     28.5837278 0.13867099232 2.780219715
#9           log-posterior -67087.4561075 0.08224116493 1.746043962
#
#             10%            50%            90% n_eff      Rhat
#1    -10.7433722     -3.6300474      3.1086066   898 1.0019146
#2   -241.6651095   -228.9075213   -217.6592667  1827 0.9999457
#3      0.1805157      0.1844635      0.1887003  1695 0.9984839
#4     -4.3529654     -3.2253655     -2.0597582  1467 0.9994934
#5     43.6938894     50.3909238     56.4858768   909 0.9985513
#6     33.2258157     41.4396592     49.4405973   885 0.9993181
#7    196.9727590    198.7239961    200.5016392   788 0.9990975
#8     25.1077446     28.6117115     32.1307339   402 1.0068306
#9 -67089.7901369 -67087.1825323 -67085.5031607   451 1.0046394

# Poner el resumen del modelo en formato bonito
coeficientes |>
  gt() |>
  fmt_number(
    columns = c(Media, SD),
    decimals = 2
  ) |>
  cols_label(
    Variable = "Variable",
    Media = "Media posterior",
    SD = "Desv. estándar"
  )

# Intervalo posterior que indica los rangos de afectación sobre
# las ganancias de las variables: Discount, Sales, Quantity y 
# Category
intervalos <- posterior_interval(
  modelo_profit,
  prob = 0.95
)
intervalos # produce: (una matriz) 
#                                2.5%        97.5%
#(Intercept)              -14.5897287    6.9020438
#Discount                -247.0891211 -212.4246584
#Sales                      0.1781927    0.1912275
#Quantity                  -4.9466392   -1.4725048
#CategoryOffice Supplies   39.7109240   59.6649167
#CategoryTechnology        29.1705278   53.5340040
#sigma                    196.2609083  201.4362366

# Cambiar la matriz del intervalo posterior a tabla
intervalos <- as.data.frame(intervalos) |>
  tibble::rownames_to_column("Variable") |>
  rename(
    LI = `2.5%`,
    LS = `97.5%`
  )
intervalos # produce:
#                Variables           LI           LS
#1             (Intercept)  -14.5897287    6.9020438
#2                Discount -247.0891211 -212.4246584
#3                   Sales    0.1781927    0.1912275
#4                Quantity   -4.9466392   -1.4725048
#5 CategoryOffice Supplies   39.7109240   59.6649167
#6      CategoryTechnology   29.1705278   53.5340040
#7                   sigma  196.2609083  201.4362366

# Poner en formato bonito a la tabla delresumen del intervalo 
# posterior
intervalos |>
  gt() |>
  fmt_number(
    columns = c(LI, LS),
    decimals = 2
  ) |>
  cols_label(
    Variable = "Variable",
    LI = "Límite inferior",
    LS = "Límite superior"
  )

# Gráfico de los intervalos posteriores de las variables
draws <- as.matrix(modelo_profit) # produce:

bayesplot::mcmc_intervals(
  draws,
  pars = c(
    "(Intercept)",
    "Discount",
    "Sales",
    "Quantity",
    "CategoryOffice Supplies",
    "CategoryTechnology"
  )
)

# Dividir el intervalo del descuento en 100 conjuntamente con la
# mediana de las ventas y la mediana de la cantidad, y asignándole una 
# categoría
newdata <- tibble(
  Discount = seq(
    min(superstore$Discount),
    max(superstore$Discount),
    length.out = 100
  ),
  Sales = median(superstore$Sales),
  Quantity = median(superstore$Quantity),
  Category = factor(
    "Office Supplies",
    levels = levels(superstore$Category)
  )
)
newdata # produce:
# A tibble: 100 × 4
#  Discount Sales Quantity Category       
#     <dbl> <dbl>    <dbl> <fct>          
#1  0        54.5        3 Office Supplies
#2  0.00808  54.5        3 Office Supplies
#3  0.0162   54.5        3 Office Supplies
#4  0.0242   54.5        3 Office Supplies
#......
#8  0.0566   54.5        3 Office Supplies
#9  0.0646   54.5        3 Office Supplies
# ℹ 90 more rows
# ℹ Use `print(n = ...)` to see more rows

# A cada uno de los 100 descuentos asignarle 1000 predicciones
predicciones <-
  posterior_epred(
    modelo_profit,
    newdata = newdata
  )
predicciones # produce:
#iterations        1        2        3        4        5        6
#      [1,] 77.59456 75.82429 74.05401 72.28374 70.51347 68.74319
#      [2,] 81.93905 79.99197 78.04489 76.09781 74.15073 72.20364
#      [3,] 73.45402 71.72410 69.99418 68.26427 66.53435 64.80444
#      [4,] 81.17632 79.22127 77.26623 75.31118 73.35613 71.40109
#      [5,] 76.33629 74.59949 72.86268 71.12588 69.38907 67.65227
#      [6,] 77.22411 75.30357 73.38302 71.46247 69.54193 67.62138
#[ reached 'max' / getOption("max.print") -- omitted 994 rows, 94 cols ]

# A cada uno de los 100 descuentos calcular la media a partir de
# las 1000 predicciones
newdata$media <- apply(
  predicciones,
  2,
  median
)
newdata$media # produce:
#         1            2            3            4            5 
#76.6345276   74.7991191   72.9449673   71.0801707   69.2238627 
#         6            7            8            9           10 
#67.3900294   65.5408393   63.6946922   61.8444183   59.9740818 
#        11           12           13           14           15 
#58.1363313   56.3034571   54.4619941   52.5816322   50.7558821
# (faltan 85 prdecciones)

# A cada uno de los 100 descuentos calcular el cuantil 5 a partir de
# las 1000 predicciones
newdata$li <- apply(
  predicciones,
  2,
  quantile,
  probs = 0.05
)
newdata$li # produce:
#         1            2            3            4            5 
#71.7107357   69.9858666   68.2126718   66.4474070   64.6535245 
#         6            7            8            9           10 
#62.8071075   60.9860473   59.1627593   57.3645344   55.5250814 
#        11           12           13           14           15 
#53.6370211   51.8299928   49.9954035   48.1020202   46.2905838 
# (faltan 85 prdecciones)

# A cada uno de los 100 descuentos calcular el cuantil 95 a partir de
# las 1000 predicciones
newdata$ls <- apply(
  predicciones,
  2,
  quantile,
  probs = 0.95
)
newdata$ls # produce:
#          1            2            3            4            5 
#81.36308517  79.48961484  77.56240639  75.66639831  73.75689296 
#          6            7            8            9           10 
#71.86393580  69.98948501  68.03697527  66.15145190  64.24802681 
#         11           12           13           14           15 
#62.38002161  60.47335560  58.54573646  56.69526225  54.82025056
# (faltan 85 prdecciones)

# X - 6 
ggplot(
  newdata,
  aes(
    x = Discount,
    y = media
  )
) +
  geom_ribbon(
    aes(
      ymin = li,
      ymax = ls
    ),
    alpha = 0.20
  ) +
  geom_line(
    linewidth = 1
  ) +
  labs(
    title = "Ganancias esperadas según el descuento",
    x = "Descuento",
    y = "Ganancia esperada"
  ) +
  theme_minimal()

############ Guardar los resultados en archivos ################

saveRDS(coeficientes, "blog_ba/R/col2-3/coeficientes.rds")
saveRDS(intervalos, "blog_ba/R/col2-3/intervalos.rds")
saveRDS(draws, "blog_ba/R/col2-3/draws.rds")
saveRDS(predicciones, "blog_ba/R/col2-3/pedicciones.rds")



