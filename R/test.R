# 1. Instalar causact (en Posit Cloud sí hay internet habilitado de fábrica)
install.packages("causact")

library(reticulate)

# 2. Instalar las dependencias de Python/NumPyro automáticamente
causact::install_causact_deps()
# Cuando te pregunte si deseas instalar Miniconda en la consola, escribe Y y presiona Enter.

library(causact)

graph <- dag_create() %>%
  dag_node("Normal RV", rhs = normal(0, 10))

graph %>% dag_render()
drawsDF <- graph %>% dag_numpyro()
drawsDF %>% dagp_plot(densityPlot = TRUE)





