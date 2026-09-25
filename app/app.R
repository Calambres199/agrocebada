library(shiny)

# leer los datos
datos = read.csv("ID_area_rendimiento_70_30_Reto_AgroCebada.csv")

# lo que se ve en la pantalla
ui = fluidPage(
  titlePanel("Reto AgroCebada"),
  # menu para escoger la parcela
  selectInput("parcela", "Elige una parcela:", choices = sort(datos$ID_POLIGONO)),
  # aqui sale la tabla
  tableOutput("info")
)

# lo que hace la app por detras
server = function(input, output) {
  output$info = renderTable({
    # agarro la parcela que se escogio
    fila = datos[datos$ID_POLIGONO == input$parcela, ]
    data.frame(
      Dato  = c("Parcela", "Área (ha)", "Conjunto", "Rendimiento (t/ha)"),
      Valor = c(fila$ID_POLIGONO,
                fila$AREA_HA,
                fila$CONJUNTO,
                # si no tiene rendimiento es de las que hay que predecir
                ifelse(is.na(fila$RENDIMIENTO_T_HA), "A predecir", fila$RENDIMIENTO_T_HA))
    )
  })
}

# correr la app
shinyApp(ui, server)