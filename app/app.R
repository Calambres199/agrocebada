########## APP RETO AGROCEBADA 2026 - PREDICCION DEL RENDIMIENTO DE CEBADA
# La app lee la base del modelo, AJUSTA el modelo de regresion dentro de la app,
# lo valida con validacion cruzada y permite consultar y simular el rendimiento.
# Solo usa shiny y bslib (y R base), para que funcione con shinylive en GitHub Pages.

library(shiny)
library(bslib)

# =====================================================================
# 1. DATOS
# =====================================================================
base        = read.csv("datos/base_modelo_rendimiento.csv")
predicciones = read.csv("datos/predicciones_rendimiento.csv")

entrenamiento = base[base$CONJUNTO == "ENTRENAMIENTO", ]   # 138 parcelas con rendimiento
prediccion    = base[base$CONJUNTO == "PREDICCION", ]      # 59 parcelas a predecir

# =====================================================================
# 2. MODELO (el mismo que el final del analisis, elegido con step() y AIC)
# =====================================================================
variables_modelo = c("prec_siembra", "tmin_ciclo", "tmax_llenado", "elevacion_m", "pendiente_grados")
formula_modelo = RENDIMIENTO_T_HA ~ prec_siembra + tmin_ciclo + tmax_llenado + elevacion_m + pendiente_grados

modelo = lm(formula_modelo, data = entrenamiento)

# rendimiento estimado por el modelo para las 197 parcelas
base$RENDIMIENTO_MODELO = as.numeric(predict(modelo, newdata = base))

# ----- validacion cruzada de 5 grupos (igual que en el analisis) -----
set.seed(123)
grupo = sample(rep(1:5, length.out = nrow(entrenamiento)))
prediccion_cv = numeric(nrow(entrenamiento))
for (g in 1:5) {
  m = lm(formula_modelo, data = entrenamiento[grupo != g, ])
  prediccion_cv[grupo == g] = predict(m, newdata = entrenamiento[grupo == g, ])
}
real = entrenamiento$RENDIMIENTO_T_HA
rmse = sqrt(mean((real - prediccion_cv)^2))
mae  = mean(abs(real - prediccion_cv))
r2   = 1 - sum((real - prediccion_cv)^2) / sum((real - mean(real))^2)

# ----- nombres claros para mostrar las variables -----
nombres = c(
  prec_siembra     = "Lluvia en la siembra, abril-junio (mm)",
  tmin_ciclo       = "Temperatura minima del ciclo, abril-octubre (°C)",
  tmax_llenado     = "Temperatura maxima en el llenado, agosto-septiembre (°C)",
  elevacion_m      = "Elevacion (m s.n.m.)",
  pendiente_grados = "Pendiente del terreno (grados)"
)

# lista de parcelas para el menu: "AGC_001 - Calpulalpan, Tlaxcala"
opciones_parcela = setNames(base$ID_POLIGONO,
                            paste0(base$ID_POLIGONO, " - ", base$Municipio, ", ", base$Estado))

colores = c(guinda = "#6E1428", dorado = "#BE9646", verde = "#2E7D32", gris = "#9E9E9E")

# =====================================================================
# 3. INTERFAZ
# =====================================================================
ui = page_navbar(
  title = "AgroCebada 2026",
  theme = bs_theme(version = 5, primary = colores[["guinda"]], secondary = colores[["dorado"]]),
  # todas las pestañas se ajustan al tamaño de la ventana, menos "Modelo",
  # que es larga y se recorre hacia abajo para que sus graficas no se aplasten
  fillable = c("Consulta por parcela", "Predicciones", "Simulador", "Por municipio", "Acerca de"),

  # ----- PESTAÑA 1: CONSULTA POR PARCELA -----
  nav_panel("Consulta por parcela",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("parcela", "Elige una parcela:", choices = opciones_parcela),
        helpText("Las parcelas de entrenamiento tienen rendimiento real; las de prediccion solo el estimado por el modelo.")
      ),
      layout_columns(
        value_box("Rendimiento real", textOutput("vb_real"), theme = "primary"),
        value_box("Rendimiento estimado por el modelo", textOutput("vb_modelo"), theme = "secondary"),
        value_box("Ubicacion", textOutput("vb_ubicacion"))
      ),
      layout_columns(
        card(card_header("Datos de la parcela"), tableOutput("tabla_parcela")),
        card(card_header("Comparacion con su municipio"), plotOutput("grafica_parcela", height = "320px"))
      )
    )
  ),

  # ----- PESTAÑA 2: PREDICCIONES -----
  nav_panel("Predicciones",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("municipio", "Municipio:",
                    choices = c("Todos", sort(unique(prediccion$Municipio)))),
        downloadButton("descargar", "Descargar predicciones (CSV)")
      ),
      card(card_header("Rendimiento predicho de las 59 parcelas sin rendimiento"),
           tableOutput("tabla_predicciones"))
    )
  ),

  # ----- PESTAÑA 3: SIMULADOR -----
  nav_panel("Simulador",
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        helpText("Mueve los valores y el modelo calcula el rendimiento esperado."),
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

  # ----- PESTAÑA 4: POR MUNICIPIO -----
  nav_panel("Por municipio",
    layout_columns(
      card(card_header("Rendimiento promedio por municipio"), plotOutput("grafica_municipio", height = "380px")),
      card(card_header("Resumen por municipio"), tableOutput("tabla_municipio"))
    )
  ),

  # ----- PESTAÑA 5: MODELO -----
  nav_panel("Modelo",
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

  # ----- PESTAÑA 6: ACERCA DE -----
  nav_panel("Acerca de",
    card(
      card_header("Reto AgroCebada 2026 - FIRA"),
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
      p(a("Codigo del proyecto en GitHub", href = "https://github.com/Calambres199/agrocebada", target = "_blank"))
    )
  )
)

# =====================================================================
# 4. SERVIDOR
# =====================================================================
server = function(input, output, session) {

  # ----- PESTAÑA 1: CONSULTA POR PARCELA -----
  parcela = reactive(base[base$ID_POLIGONO == input$parcela, ])

  output$vb_real = renderText({
    p = parcela()
    if (is.na(p$RENDIMIENTO_T_HA)) "Sin dato (a predecir)" else paste(round(p$RENDIMIENTO_T_HA, 2), "t/ha")
  })
  output$vb_modelo = renderText(paste(round(parcela()$RENDIMIENTO_MODELO, 2), "t/ha"))
  output$vb_ubicacion = renderText(paste0(parcela()$Municipio, ", ", parcela()$Estado))

  output$tabla_parcela = renderTable({
    p = parcela()
    mismo_municipio = base[base$Municipio == p$Municipio, ]
    data.frame(
      Dato = c("Conjunto", "Area (ha)", nombres),
      Parcela = c(p$CONJUNTO, round(p$AREA_HA, 2),
                  round(unlist(p[variables_modelo]), 2)),
      "Promedio del municipio" = c("", round(mean(mismo_municipio$AREA_HA), 2),
                                   round(colMeans(mismo_municipio[variables_modelo]), 2)),
      check.names = FALSE
    )
  })

  output$grafica_parcela = renderPlot({
    p = parcela()
    mismo_municipio = entrenamiento[entrenamiento$Municipio == p$Municipio, ]
    valores = c("Esta parcela\n(modelo)" = p$RENDIMIENTO_MODELO,
                "Esta parcela\n(real)" = p$RENDIMIENTO_T_HA,
                "Promedio real\ndel municipio" = if (nrow(mismo_municipio) > 0) mean(mismo_municipio$RENDIMIENTO_T_HA) else NA,
                "Promedio real\ngeneral" = mean(entrenamiento$RENDIMIENTO_T_HA))
    colores_barras = c(colores[["dorado"]], colores[["guinda"]], colores[["gris"]], colores[["gris"]])
    colores_barras = colores_barras[!is.na(valores)]   # si la parcela no tiene rendimiento real, se quita esa barra
    valores = valores[!is.na(valores)]
    par(mar = c(4, 4, 1, 1))
    b = barplot(valores, col = colores_barras, ylim = c(0, 6), ylab = "Rendimiento (t/ha)", border = NA)
    text(b, valores + 0.25, round(valores, 2))
  })

  # ----- PESTAÑA 2: PREDICCIONES -----
  tabla_pred = reactive({
    t = merge(predicciones, base[, c("ID_POLIGONO", "Municipio", "Estado", "AREA_HA")], by = "ID_POLIGONO")
    if (input$municipio != "Todos") t = t[t$Municipio == input$municipio, ]
    t = t[order(t$ID_POLIGONO), c("ID_POLIGONO", "Municipio", "Estado", "AREA_HA", "RENDIMIENTO_PREDICHO")]
    t$RENDIMIENTO_PREDICHO = round(t$RENDIMIENTO_PREDICHO, 2)
    names(t) = c("Parcela", "Municipio", "Estado", "Area (ha)", "Rendimiento predicho (t/ha)")
    t
  })
  output$tabla_predicciones = renderTable(tabla_pred())
  output$descargar = downloadHandler(
    filename = function() "predicciones_rendimiento_cebada.csv",
    content = function(file) write.csv(tabla_pred(), file, row.names = FALSE)
  )

  # ----- PESTAÑA 3: SIMULADOR -----
  simulacion = reactive({
    nuevo = data.frame(prec_siembra = input$s_prec, tmin_ciclo = input$s_tmin,
                       tmax_llenado = input$s_tmax, elevacion_m = input$s_elev,
                       pendiente_grados = input$s_pend)
    predict(modelo, newdata = nuevo, interval = "prediction", level = 0.95)
  })
  output$sim_valor = renderText(paste(round(simulacion()[1, "fit"], 2), "t/ha"))
  output$sim_intervalo = renderText(paste(round(simulacion()[1, "lwr"], 2), "a",
                                          round(simulacion()[1, "upr"], 2), "t/ha"))
  output$sim_grafica = renderPlot({
    par(mar = c(4, 4, 1, 1))
    hist(entrenamiento$RENDIMIENTO_T_HA, breaks = 15, col = "grey85", border = "white",
         main = "", xlab = "Rendimiento real de las 138 parcelas (t/ha)", ylab = "Parcelas", xlim = c(1.5, 6))
    abline(v = simulacion()[1, "fit"], col = colores[["guinda"]], lwd = 4)
    abline(v = simulacion()[1, c("lwr", "upr")], col = colores[["dorado"]], lwd = 2, lty = 2)
    legend("topleft", c("Rendimiento simulado", "Intervalo de prediccion"),
           col = c(colores[["guinda"]], colores[["dorado"]]), lwd = c(4, 2), lty = c(1, 2), bty = "n")
  })

  # ----- PESTAÑA 4: POR MUNICIPIO -----
  resumen_municipio = reactive({
    municipios = sort(unique(base$Municipio))
    data.frame(
      Municipio = municipios,
      Estado = sapply(municipios, function(m) base$Estado[base$Municipio == m][1]),
      Parcelas = sapply(municipios, function(m) sum(base$Municipio == m)),
      "Con rendimiento" = sapply(municipios, function(m) sum(entrenamiento$Municipio == m)),
      "Rendimiento real promedio" = sapply(municipios, function(m) {
        x = entrenamiento$RENDIMIENTO_T_HA[entrenamiento$Municipio == m]
        if (length(x) == 0) NA else round(mean(x), 2)
      }),
      "Rendimiento predicho promedio" = sapply(municipios, function(m) {
        x = base$RENDIMIENTO_MODELO[base$Municipio == m & base$CONJUNTO == "PREDICCION"]
        if (length(x) == 0) NA else round(mean(x), 2)
      }),
      check.names = FALSE, row.names = NULL
    )
  })
  output$tabla_municipio = renderTable(resumen_municipio(), na = "-")
  output$grafica_municipio = renderPlot({
    r = resumen_municipio()
    r = r[order(r$`Rendimiento real promedio`), ]
    par(mar = c(4, 13, 1, 1))
    barplot(r$`Rendimiento real promedio`, names.arg = r$Municipio, horiz = TRUE, las = 1,
            col = colores[["guinda"]], border = NA, xlim = c(0, 6), xlab = "Rendimiento real promedio (t/ha)")
  })

  # ----- PESTAÑA 5: MODELO -----
  output$tabla_coeficientes = renderTable({
    s = summary(modelo)$coefficients
    data.frame(Variable = c("Intercepto", nombres[rownames(s)[-1]]),
               Coeficiente = format(signif(s[, 1], 4)),          # como texto para que no se redondee a 0.00
               "p-valor" = format.pval(s[, 4], digits = 3, eps = 0.001),     # p-valores muy chicos salen como < 0.001
               check.names = FALSE)
  })
  output$grafica_cv = renderPlot({
    par(mar = c(4, 4, 1, 1))
    plot(real, prediccion_cv, pch = 19, col = colores[["guinda"]],
         xlab = "Rendimiento real (t/ha)", ylab = "Rendimiento predicho (t/ha)")
    abline(0, 1, col = colores[["dorado"]], lwd = 2)
  })
  output$grafica_residuos = renderPlot({
    par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
    plot(modelo, which = c(1, 2))
  })
}

shinyApp(ui, server)
