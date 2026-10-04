# Explora como ha cambiado el clima en Sevilla
#
# App Shiny sobre los datos diarios de la estación AEMET 5783
# (SEVILLA AEROPUERTO), 1951-2025. Misma fuente que
# scripts/fetch_data.R y scripts/dirty_exploration.R.
#
# Arranca la app con Ctrl+Shift+A en RStudio, o desde la terminal
# (siempre desde la raíz del repositorio):
#   Rscript -e 'shiny::runApp()'
#
# ---------------------------------------------------------------------------
# ¿QUÉ ES UNA APP SHINY?
# ---------------------------------------------------------------------------
# Shiny se reduce a dos mitades, y solo dos:
#
#   ui     -> lo que se VE. Es HTML. Se ejecuta una única vez, cuando se abre
#             la página, en el navegador de quien la visita.
#   server -> lo que PASA cuando alguien toca un control (un filtro, un botón,
#             una tabla). Se ejecuta en el servidor, en una "sesión" distinta
#             por cada persona que abra la app.
#
# shinyApp(ui, server) las une y pone en marcha el servidor.
#
# Consecuencia práctica: lo que NO dependa de las decisiones de quien navega
# (leer el CSV, resumir por año, montar el gráfico) va aquí arriba, FUERA del
# server, para que se calcule una sola vez. Si lo metemos dentro de server, se
# recalcula para cada visitante y cada vez que alguien mueve un control.

library(shiny)

# ---------------------------------------------------------------------------
# 1. DATOS
# ---------------------------------------------------------------------------
# here resuelve la raíz del proyecto (fíjate en el .Rproj), de modo que la app
# encuentra el CSV tanto si la lanzas desde RStudio como desde otro directorio.
# No hay aleatoriedad en estos cálculos, así que no hace falta set.seed().

diario <- read.csv(here::here("data", "SevillaClimate.csv"))

diario$year <- as.numeric(diario$year)

# month llega como 1-12; lo fijamos a dos dígitos ("01".."12") para poder
# compararlo con las constantes de estaciones y meses que definimos más abajo.
diario$month <- sprintf("%02d", diario$month)

# Los dos contadores de días extremos se crean UNA vez aquí, no en el server:
# son columnas nuevas de la tabla y no dependen de lo que elija el visitante.
#
# OJO con los NA: ifelse(NA > 40, 1, 0) devuelve NA, no 0. Un día sin tmax no es
# un día fresco, es un día sin dato. Por eso abajo los sumamos con na.rm = TRUE,
# que cuenta los días que superaron el umbral e ignora los que no sabemos.
diario$a40 <- ifelse(diario$tmax > 40, 1, 0) # días de calor intenso
diario$a20 <- ifelse(diario$tmin > 20, 1, 0) # noches tropicales

# ---------------------------------------------------------------------------
# 2. QUÉ SE PUEDE EXPLORAR
# ---------------------------------------------------------------------------
# Cada variable necesita tres cosas que no se pueden adivinar después: cómo
# llamarla en el gráfico, en qué unidades está y cómo se agrega. Las guardamos
# juntas para que el menú, el eje y el cálculo lean siempre del mismo sitio, y
# no haya tres listas que se desincronicen.
#
#   regla = "media" -> se promedia sobre los días de la ventana
#   regla = "suma"  -> se suman los valores de la ventana
#
# Solo hay dos sumas, y por razones distintas: la precipitación se suma porque
# un total de mm es lo que significa "cuánto llovió", mientras que una media de
# precipitación diaria (mm/día) no dice nada que interese. Los contadores de
# días extremos se suman porque cada día aporta 0 o 1.

variables <- list(
  tmed = list(
    etiqueta = "Temperatura media diaria",
    unidad = "°C",
    regla = "media"
  ),
  tmin = list(
    etiqueta = "Temperatura mínima diaria",
    unidad = "°C",
    regla = "media"
  ),
  tmax = list(
    etiqueta = "Temperatura máxima diaria",
    unidad = "°C",
    regla = "media"
  ),
  prec = list(
    etiqueta = "Precipitación",
    unidad = "mm",
    regla = "suma"
  ),
  sol = list(
    etiqueta = "Insolación diaria",
    unidad = "h/día",
    regla = "media"
  ),
  a40 = list(
    etiqueta = "Días > 40 °C",
    unidad = "días",
    regla = "suma"
  ),
  a20 = list(
    etiqueta = "Noches > 20 °C",
    unidad = "días",
    regla = "suma"
  )
)

# El menú tiene que leer bien, así que en un vector con nombres los NOMBRES son
# lo que se ve y los VALORES son lo que viaja al server. Al revés, el desplegable
# pone "tmed" y el server recibe "Temperatura media diaria".
#
# Ojo con el fallo que produce: variables[["Temperatura media diaria"]] no da
# error, devuelve NULL en silencio, y el switch() de abajo revienta después con
# un "EXPR must be a length 1 vector" que no señala el origen.
opciones_variable <- setNames(
  names(variables),
  vapply(variables, function(v) v$etiqueta, character(1))
)

# ---------------------------------------------------------------------------
# 3. A QUÉ RESOLUCIÓN AGREGAMOS
# ---------------------------------------------------------------------------
NOMBRES_MES <- c(
  "Enero",
  "Febrero",
  "Marzo",
  "Abril",
  "Mayo",
  "Junio",
  "Julio",
  "Agosto",
  "Septiembre",
  "Octubre",
  "Noviembre",
  "Diciembre"
)
CODIGOS_MES <- sprintf("%02d", 1:12)

# Estación meteorológica, que en España no coincide con el trimestre civil:
# el invierno es diciembre-enero-febrero.
ESTACIONES <- list(
  primavera = c("03", "04", "05"),
  verano = c("06", "07", "08"),
  otono = c("09", "10", "11"),
  invierno = c("12", "01", "02")
)

# El menú se construye con nombres = lo que se ve, valores = el código interno.
# Ojo con setNames(x, y): y son los NOMBRES. Al revés, el menú mostraría "01" y
# el servidor recibiría "enero", que no casaría con ningún mes.
resoluciones <- c(
  "Año completo" = "anual",
  "Primavera" = "primavera",
  "Verano" = "verano",
  "Otoño" = "otono",
  "Invierno" = "invierno",
  setNames(CODIGOS_MES, tolower(NOMBRES_MES))
)

# Qué meses entran en cada resolución.
meses_de <- function(codigo) {
  if (codigo == "anual") {
    return(CODIGOS_MES)
  }
  if (codigo %in% names(ESTACIONES)) {
    return(ESTACIONES[[codigo]])
  }
  codigo # ya es un código de mes
}

# Cómo nombrar la ventana en el texto, para el título y el pie del gráfico.
periodo_texto <- function(codigo) {
  if (codigo == "anual") {
    return("año")
  }
  if (codigo %in% names(ESTACIONES)) {
    return(codigo)
  }
  tolower(NOMBRES_MES[match(codigo, CODIGOS_MES)])
}

# ---------------------------------------------------------------------------
# 4. LA AGREGACIÓN
# ---------------------------------------------------------------------------
# De 27.000 filas diarias a una fila por año dentro de la ventana elegida.
#
# Ojo con el invierno: aquí el "invierno de 1990" es enero-febrero + diciembre
# de 1990, no el invierno que va de 1990 a 1991. Es la simplificación habitual
# y para leer tendencias largas da igual; si algún día hace falta, se reordena
# el diciembre al año siguiente.

agregar_por_anio <- function(datos, variable, regla, meses) {
  # Cada regla se resuelve a una función; así el summarise no necesita ifelse.
  resumidor <- switch(
    regla,
    media = function(x) mean(x, na.rm = TRUE),
    suma = function(x) sum(x, na.rm = TRUE)
  )

  datos |>
    dplyr::filter(month %in% meses) |>
    dplyr::summarise(
      valor = resumidor(.data[[variable]]),
      # Cuántos díasylvan con dato real. No lo usamos para descartar nada
      # (todos los años y estaciones están completos), pero lo exponemos:
      # una media de 58 días no es comparable con una de 92.
      n_dias = sum(!is.na(.data[[variable]])),
      .by = year
    )
}

# Un tema compartido por los dos gráficos, para que no parezcan dos apps.
tema_app <- ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "#e8e8e8")
  )

# ---------------------------------------------------------------------------
# 5. FRANJAS DE CALENTAMIENTO (LA CABECERA)
# ---------------------------------------------------------------------------
# Este gráfico NO depende de los controles, así que se calcula una sola vez aquí
# arriba. Compáralo con el explorador de abajo, que sí es reactivo.
#
# ggstripes() viene del paquete climaemet; no existe ningún paquete llamado
# "ggstripes". |> es la tubería propia de R, no la de dplyr.

anual <- diario |>
  dplyr::group_by(year) |>
  dplyr::summarise(temp = mean(tmed, na.rm = TRUE), .groups = "drop")

# El rango sale de los datos, no escrito a mano: si mañana se amplía el CSV, el
# subtítulo seguirá diciendo la verdad. Ojo: son 75 años, 1951-2025, aunque el
# script de exploración rotule 1950-2025.
rango_anios <- paste0("(", min(anual$year), "-", max(anual$year), ")")

franjas <- climaemet::ggstripes(anual, plot_title = "Sevilla Airport") +
  ggplot2::labs(subtitle = rango_anios) +
  ggplot2::theme(
    # Sin fondo propio: el gráfico se apoya sobre el azul de la cabecera.
    plot.background = ggplot2::element_rect(fill = "#14213d", colour = NA),
    panel.background = ggplot2::element_rect(fill = "#14213d", colour = NA),
    legend.background = ggplot2::element_rect(fill = "#14213d", colour = NA),
    legend.key = ggplot2::element_rect(fill = "#14213d", colour = NA),
    text = ggplot2::element_text(colour = "#f2f2f2"),
    plot.title = ggplot2::element_text(colour = "#ffffff", face = "bold"),
    plot.subtitle = ggplot2::element_text(colour = "#cfd8e3"),
    plot.caption = ggplot2::element_text(colour = "#9fb0c4"),
    axis.text = ggplot2::element_text(colour = "#cfd8e3")
  )

# ---------------------------------------------------------------------------
# 6. INTERFAZ (ui)
# ---------------------------------------------------------------------------
# fluidPage() es la página básica de Shiny: una columna de ancho completo.
# Abajo la partimos en dos columnas con CSS grid, para dejar los controles a la
# derecha.

ui <- fluidPage(
  # CSS propio. tags$head() + tags$style() inyectan código en la página;
  # es la vía clásica para dar color y ancho a los bloques de Shiny.
  tags$head(
    tags$style(
      HTML(
        "
      .cabecera {
        background: #14213d;
        padding: 28px 24px 12px 24px;
        border-radius: 6px;
        margin-bottom: 24px;
      }
      .cabecera h1 { color: #ffffff; font-weight: 600; margin: 0 0 4px 0; }
      .cabecera .entradilla { color: #cfd8e3; margin: 0 0 14px 0; }

      /* Las dos columnas del explorador. La barra de controles va PRIMERA en el
         HTML, para que en móvil y para un lector de pantalla salga antes que el
         gráfico; grid-column es lo que la coloca a la derecha en pantalla
         ancha. Sin esto se quedaría a la izquierda. */
      .fila-explorador {
        display: grid;
        /* Primero el gráfico, que se estira; después la barra, de ancho fijo. */
        grid-template-columns: minmax(0, 1fr) minmax(260px, 330px);
        gap: 24px;
        align-items: start;
      }
      .fila-explorador > .panel-principal {
        grid-column: 1;
        grid-row: 1;
      }
      .fila-explorador > .barra-lateral {
        grid-column: 2;
        grid-row: 1;
      }
      .barra-lateral {
        background: #f7f7f7;
        border: 1px solid #e2e2e2;
        border-radius: 6px;
        padding: 16px;
      }
      /* min-width: 0 deja que el gráfico encoja en vez de desbordar la rejilla. */
      .panel-principal { min-width: 0; }
      /* Por debajo de 720px no cabe una barra al lado; la subimos arriba del
         todo y dejamos una sola columna. */
      @media (max-width: 720px) {
        .fila-explorador { grid-template-columns: 1fr; }
        .fila-explorador > .panel-principal { grid-column: 1; grid-row: 2; }
        .fila-explorador > .barra-lateral { grid-column: 1; grid-row: 1; }
      }

      .ayuda { color: #6c757d; font-size: 12px; margin-top: 14px; }
      "
      )
    )
  ),

  # ---- Cabecera: el título del proyecto y las franjas de calentamiento ----
  div(
    class = "cabecera",
    h1("Explora cómo ha cambiado el clima en Sevilla"),
    # plotOutput() reserva el hueco en el HTML; renderPlot() (más abajo) lo
    # rellena. Los dos van siempre emparejados por el mismo "id".
    plotOutput("franjas", height = "220px"),
    tags$p(
      class = "entradilla",
      "Temperatura media anual en el aeropuerto de Sevilla, ",
      "1951-2025. Datos de AEMET (estación 5783)."
    )
    ),

  # ---- Cuerpo: controles a la derecha, gráfico a la izquierda ----
  #
  # Montamos las dos columnas a mano en vez de usar sidebarLayout() a propósito:
  # en Shiny 1.9 sidebarLayout() solo produce un .row con .col-sm-4 y
  # .col-sm-8, sin ninguna clase propia que las identifique. Para ponerlo a la
  # derecha habría que adivinar algo como ".col-sm-4 { float: right }", que se
  # rompe con cualquier otra rejilla de la página. Con dos divs y un par de
  # reglas de CSS, el control es nuestro.
  div(
    class = "fila-explorador",
    div(
      class = "barra-lateral",
      # En el UI, un input es una función que pinta el control y cuyo primer
      # argumento es el "id". Ese id es el que luego lee el server como
      # input$<id>. Los dos nombres tienen que coincidir.
      selectInput(
        "variable",
        label = "Variable",
        choices = opciones_variable,
        selected = "tmed"
      ),

      selectInput(
        "resolucion",
        label = "Agregar por",
        choices = resoluciones,
        selected = "anual"
      ),

      sliderInput(
        "anios",
        label = "Rango de años",
        min = min(diario$year),
        max = max(diario$year),
        value = range(diario$year), # por defecto, toda la serie
        step = 1,
        sep = "",
        # Las marcas automáticas se solapan en una barra estrecha ("20202025").
        # ticks solo admite TRUE/FALSE, no un vector de posiciones, así que las
        # quitamos: los dos tiradores ya enseñan 1951 y 2025.
        ticks = FALSE
      ),

      tags$p(
        class = "ayuda",
        "Las medias (temperaturas, insolación) se calculan sobre los días ",
        "de la ventana elegida. La precipitación y los días de más de ",
        "40 °C y de más de 20 °C se suman."
      )
    ),

    div(
      class = "panel-principal",
      plotOutput("explorador", height = "480px")
    )
  )
)

# ---------------------------------------------------------------------------
# 7. SERVIDOR (server)
# ---------------------------------------------------------------------------
# function(input, output, session) { ... }
#   input   -> lo que la persona elige
#   output  -> los elementos con id que hay que rellenar
#   session -> la conversación con ese visitante concreto
#
# reactive() marca una función como reactiva: Shiny la guarda en memoria y solo
# vuelve a ejecutarla si algo de lo que ha leído ha cambiado. Eso convierte
# "filtrar y agregar" en una función que podemos llamar desde varios sitios
# (el gráfico, un pie, una tabla) sin repetir el cálculo ni una línea.

server <- function(input, output, session) {
  # Paso 1: filtrar por años y agregar a la resolución pedida.
  datos_agregados <- reactive({
    # req() = "todavía no tengo lo que necesito, no hagas nada y no pintes
    # nada". Hace falta porque al abrir la página Shiny lanza el primer render
    # antes de que el navegador haya mandado los valores de los controles, así
    # que input$variable llega NULL. Sin req(), variables[[NULL]] devuelve una
    # lista vacía, $regla es NULL y switch() revienta con "EXPR must be a
    # length 1 vector".
    req(input$variable, input$resolucion, input$anios)

    desde <- input$anios[1]
    hasta <- input$anios[2]

    agregar_por_anio(
      diario |> dplyr::filter(year >= desde, year <= hasta),
      variable = input$variable,
      regla = variables[[input$variable]]$regla,
      meses = meses_de(input$resolucion)
    )
  })

  # Paso 2: la recta de tendencia y lo que hay que decir sobre ella.
  # Con menos de tres años no hay recta que Estimate; devolvemos NULL y abajo
  # el pie se queda en blanco en vez de reventar.
  tendencia <- reactive({
    d <- datos_agregados()

    if (nrow(d) < 3 || stats::sd(d$valor) == 0) {
      return(NULL)
    }

    ajuste <- stats::lm(valor ~ year, data = d)
    resumen <- summary(ajuste)

    list(
      pendiente = unname(stats::coef(ajuste)["year"]),
      r2 = resumen$r.squared,
      p = unname(resumen$coefficients["year", 4]),
      n_dias = mean(d$n_dias),
      n_dias_min = min(d$n_dias)
    )
  })

  # Pie de figura: el número que convierte "una recta que sube" en "cuánto".
  #
  # Va en un reactive() y no en un output$ propio porque lo leemos desde dentro
  # de renderPlot(). Shiny no permite que un render lea otro output: le
  # devuelve el objeto de la función, no el texto, y labs() revienta con
  # "EXPR must be a length 1 vector". Un reactive() se puede leer desde
  # cuantos sitios haga falta.
  pie_texto <- reactive({
    t <- tendencia()

    if (is.null(t)) {
      return("Elige un rango de al menos tres años para ver la tendencia.")
    }

    # sprintf con %.Nf recorta a N decimales. Ojo: format(x, nsmall = 2) NO
    # hace eso, nsoft es un MÍNIMO de decimales, así que sacaría la
    # precisión entera del número (0.031155, 0.6022664). Y para el p-valor,
    # format.pval() cae en notación científica con p = 2.9e-16, así que lo
    # escribimos a mano.
    #
    # El fragmento entero del p-valor, incluido el signo, para no acabar
    # escribiendo "p = < 0.001".
    p_texto <- if (is.na(t$p)) {
      "p = n/d"
    } else if (t$p < 0.001) {
      "p < 0.001"
    } else {
      paste("p =", sprintf("%.3f", t$p))
    }

    # %+.3f pone el signo siempre, para que "+0.012" y "-0.012" no se confundan
    # al leerse. Y en dos líneas porque ggplot no ajusta el pie: con un solo
    # bloque se sale del gráfico cuando el panel es estrecho.
    paste0(
      "Tendencia: ",
      sprintf("%+.3f", t$pendiente),
      " por año · R² = ",
      sprintf("%.2f", t$r2),
      " · ",
      p_texto,
      "\nVentana: ",
      round(t$n_dias),
      " días de media (mínimo ",
      t$n_dias_min,
      ")"
    )
  })

  # Paso 3: el gráfico. Cada output$ debe existir en el ui con el mismo id.
  output$explorador <- renderPlot({
    d <- datos_agregados()
    meta <- variables[[input$variable]]
    periodo <- periodo_texto(input$resolucion)

    # Las sumas son magnitudes ACUMULADAS en la ventana, así que el eje lo dice
    # ("mm en julio", "días por julio"); si no, un 21 de mm no se distingue de
    # una media diaria. Las medias llevan su unidad tal cual ("°C").
    eje <- if (meta$regla == "suma") {
      if (input$resolucion == "anual") {
        meta$unidad
      } else {
        paste0(meta$unidad, " en ", periodo)
      }
    } else {
      meta$unidad
    }

    p <- ggplot2::ggplot(d, ggplot2::aes(x = year, y = valor)) +
      ggplot2::geom_point(colour = "#c1121f", size = 1.7, alpha = 0.85) +
      ggplot2::labs(
        title = paste0(meta$etiqueta, " por ", periodo),
        # En el anual el total ya va en el título y el eje; en el resto de
        # resoluciones el pie aclara que la suma es del periodo, no del año.
        subtitle = if (meta$regla == "suma" && input$resolucion != "anual") {
          paste0("Total acumulado del ", periodo)
        },
        x = "Año",
        y = eje,
        caption = pie_texto()
      ) +
      # El eje de los años va en años enteros y ajustado a los datos.
      #
      # Por defecto ggplot reparte el espacio con números "bonitos" (1982.5) y
      # además hincha el eje un 5% a cada lado, de modo que con dos años
      # marcados se ve 2000.95 y 2001.95, que no son años. Los dos detalles se
      # arreglan aquí: expand = 0 para que el eje acabe en los datos, y las
      # marcas se eligen a partir de d$year y no de los límites ya ensanchados.
      ggplot2::scale_x_continuous(
        breaks = function(limites) {
          paso <- max(1, ceiling(diff(range(d$year)) / 6))
          seq(min(d$year), max(d$year), by = paso)
        },
        expand = ggplot2::expansion(add = 0.6)
      ) +
      tema_app

    # geom_smooth() con menos de tres puntos no sabe qué recta pintar.
    if (nrow(d) >= 3) {
      p <- p +
        ggplot2::geom_smooth(
          method = "lm",
          formula = y ~ x,
          colour = "#14213d",
          fill = "#a8dadc",
          se = TRUE
        )
    }

    p
  })

  # El gráfico de franjas de la cabecera no depende de nada: se pinta una vez.
  output$franjas <- renderPlot({
    franjas
  })
}

# ---------------------------------------------------------------------------
# 8. ARRANCAR
# ---------------------------------------------------------------------------
# shinyApp() devuelve el objeto app; la última línea de app.R se evalúa al
# cargarlo, y shiny::runApp() se encarga de ponerlo en marcha.

shinyApp(ui = ui, server = server)
