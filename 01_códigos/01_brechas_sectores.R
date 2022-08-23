#------------------------------------------------------------------------------#
# Proyecto:                 Brechas de género 
# Objetivo:                 Brecha de ingreso promedio por sectores
#
# Encargadas:               Regina Isabel Medina
# Correos:                  regina@mexicocomovamos.mx
# 
# Fecha de creación:        08 de marzo de 2022
# Última actualización:     08 de marzo de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar paquetería 
require(pacman)
p_load(readr, readxl, inegiR, tidyverse, dplyr, lubridate, ggalt, beepr)

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
rm(list=ls())

# Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

# Funciones de importación y exportación
paste_out <- function(x){paste0("04_infobites/", x)}

# 1. Registrar datos -----------------------------------------------------------

# Ingreso promedio 
df_brecha <- dplyr::tribble(
    ~ sector     ,            ~ general, ~ hombres, ~ mujeres,
    # "Agricultura",                 3970,	3964,	4014,
    "Comercio",                    6483,	7628,	5403,
    "Construcción",                8036,	9783,	7981,
    "Gobierno",                   10892,   11314,  10270,
    "Industria extractiva",       12312,   13078,  12165,
    "Industria manufacturera",     7436,	8310,	6004,
    "Restaurantes y alojamientos", 8404,	8850,	7839,
    "Servicios diversos",          5654,	7114,	4461,
    "Servicios profesionales",     6025,	7533,	4821,
    "Servicios sociales",         10179,   10749,	9240,
    "Transportes",                10290,   11423,	9647
    ) %>% 
    mutate(brecha = hombres - mujeres,
           brecha_abs = abs(hombres-mujeres)) %>% 
    arrange(brecha) %>% 
    #mutate(sector = forcats::fct_inorder(sector)) %>% 
    glimpse


# 2. Infobite ------------------------------------------------------------------

v_colors <- c("#04b187", "#6950d8")

ggplot(df_brecha, 
        aes(y = reorder(sector, brecha_abs), x = hombres, xend = mujeres, group = sector)) +
        # Geoms       
        geom_dumbbell(size = 3, color = mcv_blacks[3],
            colour_x = v_colors[1], colour_xend = v_colors[2],
            dot_guide_size = 30
            ) +
        geom_text(aes(label = scales::dollar(hombres)), nudge_y = 0.3, color = "#777777",  family = "Ubuntu", size = 7) +
        geom_text(aes(label = scales::dollar(mujeres), x = mujeres), nudge_y = -0.3, color = "#777777",  family = "Ubuntu", size = 7) +
        annotate("text", x = 5000, y = 11.5, label = "Mujeres", color = mcv_discrete[1], size = 8, family = "Ubuntu", fontface = "bold") +
        annotate("text", x = 7500, y = 11.5, label = "Hombres", color = mcv_discrete[3], size = 8, family = "Ubuntu", fontface = "bold") +
        # Etiquetas
        labs(
            title = "Brecha salarial entre hombres y mujeres", 
            subtitle = "Por sector, según el ingreso promedio mensual", 
            x = "", 
            y = "") +
        scale_x_continuous(labels = scales::dollar_format(), limits = c(2000,15000), breaks = seq(4000,14000,2000)) +
        expand_limits(y = c(1, 12)) +
        # Tema 
        theme_minimal() +
        theme(plot.title    = element_text(size = 35, face = "bold", colour = "#6950D8"),
            plot.subtitle     = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
            plot.margin       = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
            plot.caption      = element_text(size = 20),
            strip.text.x      = element_text(size = 15),
            panel.grid.minor  = element_blank(),
            panel.background  = element_rect(fill = "transparent",colour = NA),
            text              = element_text(family = "Ubuntu"),
            axis.title.x      = element_blank(),
            axis.title.y      = element_text(size = 25),
            axis.text.x       = element_text(size = 25),
            axis.text.y       = element_text(size = 25),
            legend.text       = element_text(size = 25),
            legend.position   = "none") 


ggsave(filename = "~/desktop/99_brechas_sectores.png", width = 15, height = 20, dpi = 100, bg= "transparent")

