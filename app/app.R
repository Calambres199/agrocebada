########## APP RETO AGROCEBADA 2026
# app para consultar y predecir el rendimiento de cebada (t/ha) de las 197 parcelas
# el modelo de regresion se ajusta aqui mismo, dentro de la app

library(shiny)   # para hacer la app: la interfaz, los menus, las tablas y las graficas que se actualizan solas
library(bslib)   # para el diseño: la barra de pestañas, los cuadros de colores y los recuadros


# Leer las bases
# las dos bases que se usaran ya estan en una carpeta en mi lap
base         = read.csv("datos/base_modelo_rendimiento.csv")    # 197 parcelas con sus variables
predicciones = read.csv("datos/predicciones_rendimiento.csv")   # rendimiento predicho de las 59

# separo las parcelas que tienen rendimiento de las que hay que predecir
entrenamiento = base[base$CONJUNTO == "ENTRENAMIENTO", ]   # 138 parcelas con rendimiento
prediccion    = base[base$CONJUNTO == "PREDICCION", ]      # 59 parcelas sin rendimiento


# Haremos tal cual la regresion
# son las 5 variables que quedaron con step() en el modelo final
variables_modelo = c("prec_siembra", "tmin_ciclo", "tmax_llenado", "elevacion_m", "pendiente_grados")
formula_modelo = RENDIMIENTO_T_HA ~ prec_siembra + tmin_ciclo + tmax_llenado + elevacion_m + pendiente_grados

# lm ajusta la regresion lineal multiple con las 138 parcelas de entrenamiento
modelo = lm(formula_modelo, data = entrenamiento)

# predict usa el modelo para calcular el rendimiento de las 197 parcelas
# lo guardo en una columna nueva para poder compararlo con el real
base$RENDIMIENTO_MODELO = as.numeric(predict(modelo, newdata = base))

# validacion cruzada de 5 grupos, igual que en el analisis
set.seed(123)                                                       # para que los grupos siempre salgan iguales
grupo = sample(rep(1:5, length.out = nrow(entrenamiento)))          # a cada parcela le toca un grupo del 1 al 5 al azar
prediccion_cv = numeric(nrow(entrenamiento))                        # vector vacio donde guardo las predicciones
for (g in 1:5) {
  m = lm(formula_modelo, data = entrenamiento[grupo != g, ])        # ajusto el modelo sin el grupo g
  prediccion_cv[grupo == g] = predict(m, newdata = entrenamiento[grupo == g, ])   # y predigo el grupo g que deje fuera
}

# que tanto se equivoca el modelo con parcelas que no vio
real = entrenamiento$RENDIMIENTO_T_HA
rmse = sqrt(mean((real - prediccion_cv)^2))                                 # error promedio, castiga mas los errores grandes
mae  = mean(abs(real - prediccion_cv))                                      # error promedio en t/ha
r2   = 1 - sum((real - prediccion_cv)^2) / sum((real - mean(real))^2)      # que parte de las diferencias explica el modelo

# nombres para que en la app no salgan los nombres de las columnas
nombres = c(
  prec_siembra     = "Lluvia en la siembra, abril-junio (mm)",
  tmin_ciclo       = "Temperatura minima del ciclo, abril-octubre (°C)",
  tmax_llenado     = "Temperatura maxima en el llenado, agosto-septiembre (°C)",
  elevacion_m      = "Elevacion (m s.n.m.)",
  pendiente_grados = "Pendiente del terreno (grados)"
)

# lista de parcelas para el menu, que se vean asi: "AGC_001 - Calpulalpan, Tlaxcala"
# setNames hace que el menu muestre el texto largo pero por dentro guarde solo el ID
opciones_parcela = setNames(base$ID_POLIGONO,
                            paste0(base$ID_POLIGONO, " - ", base$Municipio, ", ", base$Estado))

# colores de la app (guinda y dorado de la poderosa UAEH)
colores = c(guinda = "#6E1428", dorado = "#BE9646", verde = "#2E7D32", gris = "#9E9E9E")


# Aqui estaremos haciendo la interfaz, osea lo que todo mundo vera
# page_navbar hace la pagina con la barra de pestañas arriba
ui = page_navbar(
  title = "AgroCebada 2026",
  # bs_theme pone los colores de la app
  theme = bs_theme(version = 5, primary = colores[["guinda"]], secondary = colores[["dorado"]]),
  # estas pestañas se ajustan al tamaño de la ventana por eso la pestaña "Modelo" no esta aqui porque es larga y se tiene que recorrer hacia abajo para que no se aplasten las graficas
  fillable = c("Consulta por parcela", "Predicciones", "Simulador", "Acerca de"),

  # En esta parte haremos las pestañas
  # nav_panel es una pestaña
  nav_panel("Consulta por parcela",
    # layout_sidebar divide la pestaña en un panel a la izquierda y el contenido a la derecha
    layout_sidebar(
      sidebar = sidebar(
        # selectInput es el menu desplegable; lo que se elija se guarda en input$parcela
        selectInput("parcela", "Elige una parcela:", choices = opciones_parcela),
        # helpText es un texto de ayuda en gris
        helpText("Las parcelas de entrenamiento tienen rendimiento real; las de prediccion solo el estimado por el modelo.")
      ),
      # layout_columns acomoda las cosas una al lado de otra
      layout_columns(
        # value_box es un cuadro de color con un numero grande
        # textOutput es un espacio vacio que se llena en el servidor
        value_box("Rendimiento real", textOutput("vb_real"), theme = "primary"),
        value_box("Rendimiento estimado por el modelo", textOutput("vb_modelo"), theme = "secondary"),
        value_box("Ubicacion", textOutput("vb_ubicacion"))
      ),
      layout_columns(
        # card es un recuadro con titulo (card_header)
        # tableOutput y plotOutput son los espacios vacios para la tabla y la grafica
        card(card_header("Datos de la parcela"), tableOutput("tabla_parcela")),
        card(card_header("Comparacion con su municipio"), plotOutput("grafica_parcela", height = "320px"))
      )
    )
  ),

  # En esta pestaña pondremos las predicciones
  nav_panel("Predicciones",
    layout_sidebar(
      sidebar = sidebar(
        # menu para filtrar por municipio, sort los pone en orden alfabetico y unique quita los repetidos
        selectInput("municipio", "Municipio:",
                    choices = c("Todos", sort(unique(prediccion$Municipio)))),
        # boton para descargar la tabla en csv
        downloadButton("descargar", "Descargar predicciones (CSV)")
      ),
      card(card_header("Rendimiento predicho de las 59 parcelas sin rendimiento"),
           tableOutput("tabla_predicciones"))
    )
  ),

  # En esta pestaña haremos el simulador
  nav_panel("Simulador",
    layout_sidebar(
      sidebar = sidebar(
        width = 350,   # ancho del panel para que quepan los nombres
        helpText("Mueve los valores y el modelo calcula el rendimiento esperado."),
        # sliderInput es una barrita para mover un valor
        # min y max son los valores mas chico y mas grande de las 197 parcelas que se vieron
        # value es donde empieza, la mediana de las parcelas
        sliderInput("s_prec", nombres[["prec_siembra"]], min = 290, max = 620,
                    value = round(median(base$prec_siembra)), step = 5),
        sliderInput("s_tmin", nombres[["tmin_ciclo"]], min = 11.4, max = 13.5,
                    value = round(median(base$tmin_ciclo), 1), step = 0.1),
        sliderInput("s_tmax", nombres[["tmax_llenado"]], min = 22.4, max = 23.6,
                    value = round(median(base$tmax_llenado), 1), step = 0.05),
        sliderInput("s_elev", nombres[["elevacion_m"]], min = 2490, max = 2910,
                    value = round(median(base$elevacion_m)), step = 10),
        sliderInput("s_pend", nombres[["pendiente_grados"]], min = 0, max = 9.5,
                    value = round(median(base$pendiente_grados), 1), step = 0.1)
      ),
      layout_columns(
        value_box("Rendimiento esperado", textOutput("sim_valor"), theme = "primary"),
        value_box("Intervalo de prediccion (95%)", textOutput("sim_intervalo"), theme = "secondary")
      ),
      card(card_header("Rendimiento simulado contra las parcelas reales"),
           plotOutput("sim_grafica", height = "320px")),
      helpText("Los rangos de los controles son los observados en las 197 parcelas; fuera de ellos el modelo no es confiable.")
    )
  ),

  # En esta pestaña se escribira el modelo
  nav_panel("Modelo",
    # las metricas de la validacion cruzada; round deja 3 decimales y paste le pega las unidades
    layout_columns(
      value_box("RMSE (validacion cruzada)", paste(round(rmse, 3), "t/ha"), theme = "primary"),
      value_box("MAE (validacion cruzada)", paste(round(mae, 3), "t/ha"), theme = "primary"),
      value_box("R² (validacion cruzada)", round(r2, 3), theme = "secondary")
    ),
    layout_columns(
      card(card_header("Coeficientes del modelo de regresion"), tableOutput("tabla_coeficientes"),
           helpText("Variables elegidas con seleccion paso a paso (AIC) a partir de 11 candidatas.")),
      card(card_header("Rendimiento real contra predicho (validacion cruzada)"),
           plotOutput("grafica_cv", height = "340px"))
    ),
    card(card_header("Diagnostico de residuos"), plotOutput("grafica_residuos", height = "380px"))
  ),

  # pestaña donde se habla acerca de la app y del resto
  nav_panel("Acerca de",
    card(
      card_header("Reto AgroCebada 2026 - FIRA"),
      # p es un parrafo de texto
      p("Esta aplicacion estima el rendimiento de cebada (t/ha) de 197 parcelas de Hidalgo, Tlaxcala y Puebla",
        "en el ciclo abril-octubre de 2025."),
      p("El analisis de series de tiempo (2022-2025) de la lluvia, la temperatura y los indices satelitales",
        "mostro que las variables siguen el ciclo de la temporada; por ello, las variables del modelo se construyeron",
        "por etapa fenologica del cultivo (escala de Zadoks)."),
      p("Con esas variables se ajusto una regresion lineal multiple con 138 parcelas de rendimiento conocido,",
        "se eligieron las variables con el AIC y el modelo se valido con validacion cruzada de 5 grupos."),
      p("Fuentes de datos: indices satelitales (Sentinel-2 y Planet), precipitacion (CHIRPS),",
        "temperatura (CHIRTS-ERA5) y topografia (INEGI, CEM 4.0)."),
      p("Universidad Autonoma del Estado de Hidalgo - Licenciatura en Matematicas Aplicadas."),
      # a es un enlace; target = "_blank" hace que se abra en otra pestaña
      p(a("Codigo del proyecto en GitHub", href = "https://github.com/Calambres199/agrocebada", target = "_blank"))
    )
  )
)



# Aqui se pondran todos los calculos que haran que se llenen cada cosa, cada interfaz
# input trae lo que la persona elige en la app (menus y sliders)
# output es donde guardo lo que se va a mostrar, con el mismo nombre que le puse en la interfaz
server = function(input, output, session) {

  # Aqui se consultara cada parcela
  # reactive se vuelve a calcular solo cada vez que cambia la parcela del menu
  # aqui saco la fila de la parcela que se eligio
  parcela = reactive(base[base$ID_POLIGONO == input$parcela, ])

  # renderText llena los textOutput
  output$vb_real = renderText({
    p = parcela()
    # si la parcela es de prediccion no tiene rendimiento real (is.na)
    if (is.na(p$RENDIMIENTO_T_HA)) "Sin dato (a predecir)" else paste(round(p$RENDIMIENTO_T_HA, 2), "t/ha")
  })
  output$vb_modelo = renderText(paste(round(parcela()$RENDIMIENTO_MODELO, 2), "t/ha"))
  output$vb_ubicacion = renderText(paste0(parcela()$Municipio, ", ", parcela()$Estado))   # paste0 pega sin espacio

  # renderTable llena los tableOutput
  # tabla con los datos de la parcela al lado del promedio de su municipio
  output$tabla_parcela = renderTable({
    p = parcela()
    mismo_municipio = base[base$Municipio == p$Municipio, ]   # todas las parcelas de su mismo municipio
    data.frame(
      Dato = c("Conjunto", "Area (ha)", nombres),
      Parcela = c(p$CONJUNTO, round(p$AREA_HA, 2),
                  round(unlist(p[variables_modelo]), 2)),                 # unlist pasa la fila a vector
      "Promedio del municipio" = c("", round(mean(mismo_municipio$AREA_HA), 2),
                                   round(colMeans(mismo_municipio[variables_modelo]), 2)),   # colMeans: promedio de cada columna
      check.names = FALSE   # para que deje los espacios en el nombre de la columna
    )
  })

  # renderPlot llena los plotOutput
  # grafica de barras, la parcela contra el promedio de su municipio y el promedio general
  output$grafica_parcela = renderPlot({
    p = parcela()
    mismo_municipio = entrenamiento[entrenamiento$Municipio == p$Municipio, ]
    valores = c("Esta parcela\n(modelo)" = p$RENDIMIENTO_MODELO,          # \n es salto de linea en el nombre
                "Esta parcela\n(real)" = p$RENDIMIENTO_T_HA,
                "Promedio real\ndel municipio" = if (nrow(mismo_municipio) > 0) mean(mismo_municipio$RENDIMIENTO_T_HA) else NA,
                "Promedio real\ngeneral" = mean(entrenamiento$RENDIMIENTO_T_HA))
    colores_barras = c(colores[["dorado"]], colores[["guinda"]], colores[["gris"]], colores[["gris"]])
    colores_barras = colores_barras[!is.na(valores)]   # si la parcela no tiene rendimiento real, se quita esa barra
    valores = valores[!is.na(valores)]
    par(mar = c(4, 4, 1, 1))                            # margenes de la grafica
    b = barplot(valores, col = colores_barras, ylim = c(0, 6), ylab = "Rendimiento (t/ha)", border = NA)
    text(b, valores + 0.25, round(valores, 2))          # escribe el valor arriba de cada barra
  })

  # Aqui se pone cada valor de las predicciones
  # tabla de las 59 predicciones; se recalcula cuando cambia el municipio del menu
  tabla_pred = reactive({
    # merge une las predicciones con el municipio, estado y area de cada parcela
    t = merge(predicciones, base[, c("ID_POLIGONO", "Municipio", "Estado", "AREA_HA")], by = "ID_POLIGONO")
    if (input$municipio != "Todos") t = t[t$Municipio == input$municipio, ]   # filtro si se eligio un municipio
    t = t[order(t$ID_POLIGONO), c("ID_POLIGONO", "Municipio", "Estado", "AREA_HA", "RENDIMIENTO_PREDICHO")]   # ordeno por ID
    t$RENDIMIENTO_PREDICHO = round(t$RENDIMIENTO_PREDICHO, 2)
    names(t) = c("Parcela", "Municipio", "Estado", "Area (ha)", "Rendimiento predicho (t/ha)")
    t
  })
  output$tabla_predicciones = renderTable(tabla_pred())
  # downloadHandler es lo que hace el boton de descarga: el nombre del archivo y que se guarda
  output$descargar = downloadHandler(
    filename = function() "predicciones_rendimiento_cebada.csv",
    content = function(file) write.csv(tabla_pred(), file, row.names = FALSE)
  )

  # Haremos que funciones el simulador
  # con los valores de los sliders armo una parcela nueva y el modelo predice su rendimiento
  simulacion = reactive({
    nuevo = data.frame(prec_siembra = input$s_prec, tmin_ciclo = input$s_tmin,
                       tmax_llenado = input$s_tmax, elevacion_m = input$s_elev,
                       pendiente_grados = input$s_pend)
    # interval = "prediction" da el rendimiento (fit) y el intervalo al 95% (lwr y upr)
    predict(modelo, newdata = nuevo, interval = "prediction", level = 0.95)
  })
  output$sim_valor = renderText(paste(round(simulacion()[1, "fit"], 2), "t/ha"))
  output$sim_intervalo = renderText(paste(round(simulacion()[1, "lwr"], 2), "a",
                                          round(simulacion()[1, "upr"], 2), "t/ha"))
  # histograma de los rendimientos reales con lineas donde cae la simulacion
  output$sim_grafica = renderPlot({
    par(mar = c(4, 4, 1, 1))
    hist(entrenamiento$RENDIMIENTO_T_HA, breaks = 15, col = "grey85", border = "white",
         main = "", xlab = "Rendimiento real de las 138 parcelas (t/ha)", ylab = "Parcelas", xlim = c(1.5, 6))
    abline(v = simulacion()[1, "fit"], col = colores[["guinda"]], lwd = 4)                  # linea del rendimiento simulado
    abline(v = simulacion()[1, c("lwr", "upr")], col = colores[["dorado"]], lwd = 2, lty = 2)   # lineas punteadas del intervalo
    legend("topleft", c("Rendimiento simulado", "Intervalo de prediccion"),
           col = c(colores[["guinda"]], colores[["dorado"]]), lwd = c(4, 2), lty = c(1, 2), bty = "n")
  })

  # Aqui va el modelo, esta parte es facil ya que se sacan solo los coeficientes que dio la regresion
  # tabla de coeficientes y p-valores sacada del summary del modelo
  output$tabla_coeficientes = renderTable({
    s = summary(modelo)$coefficients   # columna 1 es el coeficiente y columna 4 el p-valor
    data.frame(Variable = c("Intercepto", nombres[rownames(s)[-1]]),
               Coeficiente = format(signif(s[, 1], 4)),                   # format para que no se redondee a 0.00
               "p-valor" = format.pval(s[, 4], digits = 3, eps = 0.001),  # los p-valores muy chicos salen como <0.001
               check.names = FALSE)
  })
  # rendimiento real contra el predicho en la validacion cruzada
  # si el modelo fuera perfecto todos los puntos caerian sobre la linea
  output$grafica_cv = renderPlot({
    par(mar = c(4, 4, 1, 1))
    plot(real, prediccion_cv, pch = 19, col = colores[["guinda"]],
         xlab = "Rendimiento real (t/ha)", ylab = "Rendimiento predicho (t/ha)")
    abline(0, 1, col = colores[["dorado"]], lwd = 2)   # linea donde real = predicho
  })
  # residuos contra ajustados y grafica Q-Q de los residuos
  output$grafica_residuos = renderPlot({
    par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))   # mfrow pone las dos graficas una al lado de la otra
    plot(modelo, which = c(1, 2))
  })
}

# junta la interfaz y el servidor y abre la app
shinyApp(ui, server)

