# ==============================================================================
# Functions_agua.R - Funciones de Apoyo para EDA Calidad del Agua Ibagué (IBAL)
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,   # Manipulación de datos y visualización
  janitor,     # Limpieza automática de nombres de columnas
  lubridate,   # Manejo de fechas y componentes temporales
  scales,      # Formato de ejes y escalas
  patchwork,   # Combinación de gráficos
  corrplot,    # Visualización de matrices de correlación
  knitr,       # Tablas kable
  kableExtra,  # Estilo de tablas en Rmd
  RColorBrewer # Paletas de color
)

# ------------------------------------------------------------------------------
# 1. Definición de Paleta de Colores "Water Quality" y Tema
# ------------------------------------------------------------------------------
water_colors <- c(
  primary   = "#005F73",
  secondary = "#CA6702",
  accent1   = "#0A9396",
  accent2   = "#E9D8A6",
  accent3   = "#94D2BD",
  accent4   = "#BB3E03",
  light_bg  = "#F8F9FA",
  dark_txt  = "#1D2D44"
)

water_palette <- c("#005F73", "#0A9396", "#94D2BD", "#E9D8A6", "#EE9B00", "#CA6702", "#BB3E03", "#001219")

theme_water <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12, color = water_colors["primary"]),
      plot.subtitle = element_text(size = 9.5, color = "gray30"),
      plot.caption = element_text(size = 8, face = "italic", color = "gray50"),
      axis.title = element_text(face = "bold", size = 9.5, color = water_colors["dark_txt"]),
      axis.text = element_text(size = 8.5),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = water_colors["primary"], color = NA),
      strip.text = element_text(color = "white", face = "bold", size = 9.5)
    )
}

# Helper para limpiar texto numérico
clean_num <- function(x) {
  if (is.numeric(x)) return(x)
  
  x_clean <- trimws(as.character(x))
  x_clean <- gsub(",", "", x_clean)
  x_clean <- gsub("^<\\s*", "", x_clean)
  x_clean <- ifelse(x_clean %in% c("", "ND", "-", "N/A", "NA"), NA_character_, x_clean)
  
  suppressWarnings(as.numeric(x_clean))
}

# ------------------------------------------------------------------------------
# 2. Limpieza y Selección de Variables Reales
# ------------------------------------------------------------------------------
prepare_water_data <- function(df) {
  meses_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  
  df_clean <- df %>%
    janitor::clean_names() %>% 
    mutate(
      fecha_parsed = suppressWarnings(as.Date(substr(as.character(fecha_de_la_medicion), 1, 10))),
      anio         = factor(lubridate::year(fecha_parsed)),
      # Meses garantizados en español y ordenados cronológicamente
      mes          = factor(meses_es[lubridate::month(fecha_parsed)], levels = meses_es)
    ) %>%
    select(
      any_of(c(
        "fecha_parsed", "anio", "mes", "nuqfus",
        "ph", "temperatura", "turbiedad", "color_real", "conductividad", 
        "oxigeno_disuelto", "cloruros", "dureza", "coliformes_totales", "e_coli"
      ))
    ) %>%
    mutate(
      nuqfus_fct         = factor(gsub(",", "", as.character(nuqfus))),
      ph                 = clean_num(ph),
      temperatura        = clean_num(temperatura),
      turbiedad          = clean_num(turbiedad),
      color_real         = clean_num(color_real),
      conductividad      = clean_num(conductividad),
      oxigeno_disuelto   = clean_num(oxigeno_disuelto),
      cloruros           = clean_num(cloruros),
      dureza             = clean_num(dureza),
      coliformes_totales = clean_num(coliformes_totales),
      e_coli             = clean_num(e_coli)
    )
  
  return(df_clean)
}

# ------------------------------------------------------------------------------
# 3. Funciones para Análisis Univariado
# ------------------------------------------------------------------------------
plot_univariate_cuali <- function(df, var_name, title_label) {
  var_sym <- sym(var_name)
  
  df_summary <- df %>%
    filter(!is.na(!!var_sym)) %>%
    count(!!var_sym) %>%
    mutate(pct = n / sum(n) * 100)
  
  n_cats <- nrow(df_summary)
  colors_to_use <- rep(water_palette, length.out = n_cats)
  
  ggplot(df_summary, aes(x = reorder(!!var_sym, -n), y = n, fill = !!var_sym)) +
    geom_col(show.legend = FALSE, width = 0.7, color = "black", alpha = 0.85) +
    geom_text(aes(label = sprintf("%d (%.1f%%)", n, pct)), 
              vjust = -0.5, size = 3, fontface = "bold") +
    scale_fill_manual(values = colors_to_use) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = paste("Distribución de", title_label),
      x = title_label,
      y = "Frecuencia Absoluta",
      caption = "Fuente: Datos Abiertos Colombia - Alcaldía de Ibagué / IBAL"
    ) +
    theme_water() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

plot_univariate_cuanti <- function(df, var_name, title_label, log_scale = FALSE, unit = "") {
  var_sym <- sym(var_name)
  df_clean <- df %>% filter(!is.na(!!var_sym), !!var_sym >= 0)
  
  if (log_scale) {
    max_val <- max(df_clean[[var_name]], na.rm = TRUE)
    max_pow <- if (max_val > 0) floor(log10(max_val)) else 0
    dynamic_breaks <- c(0, 10^(0:max_pow))
    
    p_hist <- ggplot(df_clean, aes(x = !!var_sym)) +
      geom_histogram(bins = 30, fill = water_colors["primary"], color = "white", alpha = 0.75) +
      scale_x_continuous(trans = scales::pseudo_log_trans(base = 10), breaks = dynamic_breaks, labels = comma) +
      labs(
        title = paste("Distribución de", title_label),
        x = paste0(title_label, if(unit != "") paste0(" (", unit, ")") else ""),
        y = "Frecuencia"
      ) +
      theme_water()
    
    p_box <- ggplot(df_clean, aes(x = "", y = !!var_sym)) +
      geom_boxplot(fill = water_colors["accent2"], alpha = 0.7, outlier.color = water_colors["secondary"], outlier.size = 1) +
      scale_y_continuous(trans = scales::pseudo_log_trans(base = 10), breaks = dynamic_breaks, labels = comma) +
      coord_flip() +
      labs(x = "", y = paste0(title_label, if(unit != "") paste0(" (", unit, ")") else "")) +
      theme_water() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
  } else {
    p_hist <- ggplot(df_clean, aes(x = !!var_sym)) +
      geom_histogram(aes(y = after_stat(density)), bins = 35, fill = water_colors["primary"], color = "white", alpha = 0.75) +
      geom_density(color = water_colors["secondary"], linewidth = 0.8) +
      scale_x_continuous(labels = comma) +
      labs(
        title = paste("Distribución de", title_label),
        x = paste0(title_label, if(unit != "") paste0(" (", unit, ")") else ""),
        y = "Densidad"
      ) +
      theme_water()
    
    p_box <- ggplot(df_clean, aes(x = "", y = !!var_sym)) +
      geom_boxplot(fill = water_colors["accent2"], alpha = 0.7, outlier.color = water_colors["secondary"], outlier.size = 1) +
      scale_y_continuous(labels = comma) +
      coord_flip() +
      labs(x = "", y = paste0(title_label, if(unit != "") paste0(" (", unit, ")") else "")) +
      theme_water() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  
  p_hist / p_box + plot_layout(heights = c(3, 1))
}

# ------------------------------------------------------------------------------
# 4. Funciones Bivariadas
# ------------------------------------------------------------------------------
plot_cuali_cuali <- function(df, var1, var2, label1, label2, position = "fill") {
  sym1 <- sym(var1)
  sym2 <- sym(var2)
  
  df_filtered <- df %>% filter(!is.na(!!sym1), !is.na(!!sym2))
  n_cats_y <- length(unique(df_filtered[[var2]]))
  colors_to_use <- rep(water_palette, length.out = n_cats_y)
  
  y_lab <- if (position == "fill") "Proporción" else "Conteo"
  
  p <- ggplot(df_filtered, aes(x = !!sym1, fill = !!sym2)) +
    geom_bar(position = position, color = "black", alpha = 0.85, width = 0.7) +
    scale_fill_manual(values = colors_to_use) +
    labs(
      title = paste(label1, "vs.", label2),
      x = label1,
      y = y_lab,
      fill = label2
    ) +
    theme_water() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  
  if (position == "fill") p <- p + scale_y_continuous(labels = percent)
  return(p)
}

plot_cuali_cuanti <- function(df, var_cuali, var_cuanti, label_cuali, label_cuanti, log_scale = FALSE) {
  sym_cuali <- sym(var_cuali)
  sym_cuanti <- sym(var_cuanti)
  
  df_filtered <- df %>% filter(!is.na(!!sym_cuali), !is.na(!!sym_cuanti), !!sym_cuanti >= 0)
  
  # Reordenar por mediana solo para variables NO temporales
  if (!var_cuali %in% c("mes", "anio", "fecha_parsed")) {
    df_filtered <- df_filtered %>%
      mutate(!!sym_cuali := reorder(!!sym_cuali, !!sym_cuanti, FUN = median))
  }
  
  n_cats <- length(unique(df_filtered[[var_cuali]]))
  colors_to_use <- rep(water_palette, length.out = n_cats)
  
  p <- ggplot(df_filtered, aes(x = !!sym_cuali, y = !!sym_cuanti, fill = !!sym_cuali)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.4, outlier.size = 1) +
    scale_fill_manual(values = colors_to_use) +
    labs(
      title = paste(label_cuanti, "según", label_cuali),
      x = label_cuali,
      y = label_cuanti
    ) +
    theme_water() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 35, hjust = 1))
  
  if (log_scale) {
    max_val <- max(df_filtered[[var_cuanti]], na.rm = TRUE)
    max_pow <- if (max_val > 0) floor(log10(max_val)) else 0
    dynamic_breaks <- c(0, 10^(0:max_pow))
    
    p <- p + scale_y_continuous(
      trans  = scales::pseudo_log_trans(base = 10),
      breaks = dynamic_breaks,
      labels = comma
    )
  } else {
    p <- p + scale_y_continuous(labels = comma)
  }
  
  return(p)
}

plot_cuanti_cuanti <- function(df, var1, var2, label1, label2, log_x = FALSE, log_y = FALSE, color_var = NULL) {
  sym1 <- sym(var1)
  sym2 <- sym(var2)
  
  df_filtered <- df %>% filter(!is.na(!!sym1), !is.na(!!sym2), !!sym1 >= 0, !!sym2 >= 0)
  
  if (!is.null(color_var)) {
    sym_col <- sym(color_var)
    n_cats <- length(unique(df_filtered[[color_var]]))
    colors_to_use <- rep(water_palette, length.out = n_cats)
    
    p <- ggplot(df_filtered, aes(x = !!sym1, y = !!sym2, color = !!sym_col)) +
      geom_point(alpha = 0.6, size = 1.6) +
      scale_color_manual(values = colors_to_use)
  } else {
    p <- ggplot(df_filtered, aes(x = !!sym1, y = !!sym2)) +
      geom_point(alpha = 0.5, color = water_colors["primary"], size = 1.6)
  }
  
  p <- p +
    geom_smooth(method = "lm", color = water_colors["secondary"], se = TRUE, linetype = "dashed") +
    labs(
      title = paste("Relación entre", label1, "y", label2),
      x = label1,
      y = label2
    ) +
    theme_water()
  
  if (log_x) {
    max_x <- max(df_filtered[[var1]], na.rm = TRUE)
    max_pow_x <- if (max_x > 0) floor(log10(max_x)) else 0
    p <- p + scale_x_continuous(
      trans  = scales::pseudo_log_trans(base = 10),
      breaks = c(0, 10^(0:max_pow_x)),
      labels = comma
    )
  }
  
  if (log_y) {
    max_y <- max(df_filtered[[var2]], na.rm = TRUE)
    max_pow_y <- if (max_y > 0) floor(log10(max_y)) else 0
    p <- p + scale_y_continuous(
      trans  = scales::pseudo_log_trans(base = 10),
      breaks = c(0, 10^(0:max_pow_y)),
      labels = comma
    )
  }
  
  return(p)
}

# ------------------------------------------------------------------------------
# 5. Correlación de Pearson
# ------------------------------------------------------------------------------
plot_pearson_correlation <- function(df, vars_cuanti = NULL, use_log = FALSE) {
  if (is.null(vars_cuanti)) {
    vars_cuanti <- c("ph", "temperatura", "turbiedad", "color_real", "conductividad", "oxigeno_disuelto", "cloruros", "dureza")
  }
  
  df_num <- df %>% 
    select(any_of(vars_cuanti)) %>% 
    mutate(across(everything(), as.numeric))
  
  if (use_log) {
    df_num <- df_num %>% mutate(across(everything(), ~ ifelse(.x > 0, log10(.x), NA_real_)))
  }
  
  cor_matrix <- cor(df_num, use = "pairwise.complete.obs", method = "pearson")
  
  corrplot::corrplot(
    cor_matrix,
    method = "color",
    type = "upper",
    order = "hclust",
    addCoef.col = "black",
    number.cex = 0.75,
    tl.col = water_colors["dark_txt"],
    tl.srt = 45,
    col = colorRampPalette(c(water_colors["primary"], "white", water_colors["secondary"]))(200),
    title = paste0("Matriz de Correlación de Pearson", if(use_log) " (Escala Log10)" else ""),
    mar = c(0, 0, 2, 0)
  )
  
  return(cor_matrix)
}

# ------------------------------------------------------------------------------
# 6. Funciones de Resumen Estadístico para Tablas (Kable)
# ------------------------------------------------------------------------------
summary_cuanti_table <- function(df, var_name, label_name = NULL) {
  if (is.null(label_name)) label_name <- var_name
  var_sym <- sym(var_name)
  
  df %>%
    filter(!is.na(!!var_sym)) %>%
    summarise(
      Variable = label_name,
      `N Válidos` = n(),
      Media = round(mean(!!var_sym, na.rm = TRUE), 2),
      `Desv. Est.` = round(sd(!!var_sym, na.rm = TRUE), 2),
      Mínimo = round(min(!!var_sym, na.rm = TRUE), 2),
      `Q1 (25%)` = round(quantile(!!var_sym, 0.25, na.rm = TRUE), 2),
      Mediana = round(median(!!var_sym, na.rm = TRUE), 2),
      `Q3 (75%)` = round(quantile(!!var_sym, 0.75, na.rm = TRUE), 2),
      Máximo = round(max(!!var_sym, na.rm = TRUE), 2)
    )
}

summary_cuali_table <- function(df, var_name, label_name = NULL) {
  if (is.null(label_name)) label_name <- var_name
  var_sym <- sym(var_name)
  
  df %>%
    filter(!is.na(!!var_sym)) %>%
    count(Categoría = !!var_sym, name = "Frecuencia") %>%
    mutate(
      Variable = label_name,
      `Porcentaje (%)` = round((Frecuencia / sum(Frecuencia)) * 100, 2)
    ) %>%
    select(Variable, Categoría, Frecuencia, `Porcentaje (%)`)
}