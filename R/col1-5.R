################################
##                            ##
## Colección 1 - Entrada 5    ##
##                            ##
################################

library(tidyverse)
library(gt)

# Crear una tabla con las variables: Area, Variables y descripción
diccionario <- tibble(
  Area = c(
    "Ventas",
    "Rentabilidad",
    "Clientes",
    "Productos",
    "Ubicación",
    "Logística",
    "Tiempo"
  ),
  Variables = c(
    "Sales",
    "Profit, Discount",
    "Customer ID, Segment",
    "Category, Sub-Category",
    "Region, State, City",
    "Ship Mode",
    "Order Date, Ship Date"
  ),
  "¿Qué describe?" = c(
    "Valor económico de cada pedido",
    "Ganancias y descuentos aplicados",
    "Quién compra y a qué segmento pertenece",
    "Qué se vende",
    "Dónde ocurre la venta",
    "Cómo se entrega el pedido",
    "Cuándo ocurre cada operación"
  )
)

diccionario %>%
  gt() |>
  cols_label(
    Area = "Area del negocio",
    Variables = "Variables principales"
  )

library(gt)
library(tibble)

# Crear una tabla con el área y la pregunta principal
preguntas <- tribble(
  ~Área, ~Pregunta_principal,
  "Ventas", "¿Dónde se concentran las ventas?",
  "Rentabilidad", "¿Qué genera realmente beneficios?",
  "Clientes", "¿Quiénes aportan mayor valor?",
  "Productos", "¿Qué líneas conviene fortalecer?",
  "Descuentos", "¿Cuándo ayudan y cuándo perjudican?",
  "Regiones", "¿Dónde existen oportunidades de crecimiento?",
  "Logística", "¿Cómo mejorar las entregas?",
  "Tiempo", "¿Cómo evoluciona el negocio?"
)

preguntas %>%
  gt() %>%
  cols_label(
    Área = "Area del negocio", # Fijarse en la tilde
    Pregunta_principal = "Pregunta estratégica"
  )

