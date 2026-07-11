# Apéndice — Código completo

# En esta sección reunimos todo el código utilizado durante el análisis en el mismo orden en que fue ejecutado. Los bloques utilizan `eval: false` para evitar una segunda ejecución durante el renderizado del documento, pero pueden copiarse y ejecutarse directamente en una sesión de R.

## 1. Cargar los paquetes

options(scipen = 999) # desactivar la notación científica

library(tidyverse)
library(ggplot2)
library(skimr)


## 2. Importar el conjunto de datos

sales <- read_csv(
  "blogs/blog_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

## 3. Explorar la estructura general del dataset


skim(sales)


## 4. Calcular indicadores descriptivos básicos

# Obtener un resumen de la cantidad de obseraciones, ventas, ganancias,
# cantidades vendidas y descuentos
sales %>%
  summarise(
    #Observaciones = n(),
    Ventas_totales = sum(Sales),
    Utilidad_total = sum(Profit),
    Productos_vendidos = sum(Quantity),
    Descuento_promedio = mean(Discount)
  ) # produce:
# A tibble: 1 × 5
#  Observaciones Ventas_totales Utilidad_total Productos_vendidos
#          <int>          <dbl>          <dbl>              <dbl>
#1          9994       2297201.        286397.              37873
# ℹ 1 more variable: Descuento_promedio <dbl>

## 5. Histograma de las ventas

ggplot(
  sales,
  aes(x = Sales)
) +
  geom_histogram(
    bins = 400,
    color = "white"
  ) + 
  coord_cartesian(xlim = c(0, 2500)) +
  #coord_cartesian(lim = c(0, 200)) +
  labs (
    title = "Dsitribución de las ventas",
    x = "Ventas",
    y = "Número de transacciones"
  ) +
  theme_minimal()


## 6. Boxplot de las ventas

# Diagrama de caja de las ventas
ggplot(
  sales,
  aes(y = Sales)
) +
  geom_boxplot() +
  labs(
    title = "Boxplot de las ventas",
    y = "Ventas"
  ) + 
  theme_minimal()

# Diagrama de caja de las ventas con zoom en el eje Y
ggplot(
  sales,
  aes(y = Sales)
) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 500)) +
  labs(
    title = "Boxplot de las ventas",
    y = "Ventas"
  ) +
  theme_minimal()

## 7. Histograma de la utilidad

# Sin zoom
ggplot(
  sales,
  aes(x = Profit)
) +
  geom_histogram(
    bins = 40,
    color = "white"
  ) +
  labs(
    title = "Distribución de la utilidad",
    x = "Utilidad",
    y = "Número de transacciones"
  )

# Con zoom en el eje x
ggplot(
  sales,
  aes(x = Profit)
) +
  geom_histogram(
    bins = 800,
    color = "white"
  ) +
  coord_cartesian(xlim = c(-300, 300)) +
  labs(
    title = "Distribución de la utilidad",
    x = "Utilidad",
    y = "Número de transacciones"
  ) +
  theme_minimal()

# Con zoom en el eje Y
ggplot(
  sales,
  aes(x = Profit)
) +
  geom_histogram(
    bins = 40,
    color = "white"
  ) +
  coord_cartesian(ylim = c(0, 300)) +
  labs(
    title = "Distribución de la utilidad",
    x = "Utilidad",
    y = "Número de transacciones"
  ) +
  theme_minimal()

## 8. Boxplot de la utilidad

ggplot(
  sales,
  aes(y = Profit)
) +
  geom_boxplot() +
  labs(
    title = "Distribución de la utilidad",
    y = "utilidad"
  ) +
  theme_minimal()

# Con zoom en el eje Y
ggplot(
  sales,
  aes(y = Profit)
) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-50, 50)) +
  labs(
    title = "Distribución de la utilidad",
    y = "Utilidad"
  ) +
  theme_minimal()

## 9. Distribución de la cantidad de productos vendidos

ggplot(
  sales,
  aes(x = Quantity)
) +
  geom_bar() +
  labs(
    title = "Cantidad de productos por pedido",
    x = "Cantidad",
    y = "Número de pedidos"
  )

## 10. Distribución de los descuentos

ggplot(
  sales,
  aes(x = Discount)
) +
  geom_bar() +
  labs(
    title = "Distribución de los descuentos",
    x = "Descuento",
    y = "Número de Transacciones"
  )

# Con zoom en el eje Y
ggplot(
  sales,
  aes(x = Discount)
) +
  geom_bar() +
  coord_cartesian(ylim = c(0, 500)) +
  labs(
    title = "Distribución de los descuentos",
    x = "Descuento",
    y = "Número de transacciones"
  ) +
  theme_minimal()

