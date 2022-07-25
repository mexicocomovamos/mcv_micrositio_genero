#------------------------------------------------------------------------------#
# Proyecto:                 Brechas de género 
# Objetivo:                 Brecha de ingreso promedio por sectores 
#                           (construcción de serie de tiempo)
#
# Encargadas:               Regina Isabel Medina
# Correos:                  regina@mexicocomovamos.mx
# 
# Fecha de creación:        25 de junio de 2022
# Última actualización:     25 de junio de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar paquetería 
require(pacman)
p_load(
    readxl, googledrive, googlesheets4, tidyverse, dplyr, lubridate, ggalt, beepr)

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
paste_inp <- function(x){paste0("02_datos_crudos/", x)}
paste_fig <- function(x){paste0("04_infobites/", x)}

# Activar las credenciales de google
v_usuaria <- "regina"
# v_usuaria <- "katia"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))



# 1. Cargar datos --------------------------------------------------------------

# 1.1. Línea de pobreza --------------------------------------------------------

df_lp <- read_excel(paste_inp("lineas_de_pobreza_por_ingresos_11_7_2022.xlsx"))

# 1.2. Ingresos imputados ------------------------------------------------------

#---- Importar ids de archivos de la carpeta de drive (última parte del URL)
df_files    <- drive_ls(as_id("1Hgkbx62E9KAHPK29fUx3h4iNI2OQQy84"))
df_files # Todos los archivos deben ser extensión .RDS
temp        <- tempfile(fileext = ".rds") # Crear archivo temporal
dl          <- drive_download(as_id(df_files$id[1]), path = temp, overwrite = T)
df_ingresos <- readRDS(dl$local_path)

#---- Bucle 
df_ingresos_imputados <- data.frame()

# for(i in 1:length(df_files$id)){
#     dl <- drive_download(as_id(df_files$id[i]), path = temp, overwrite = T)
#     df_ingresos <- readRDS(dl$local_path)
#     
#     df_ingresos_imputados %>% df_ingresos_imputados %>% 
#         bind_rows(df_ingresos)
# }


# 2. Ensayo ENOE ---------------------------------------------------------------

# Último periodo 
i = "22"
x = "1"

## 2.1. Cargar datos -----------------------------------------------------------

#---- Importar ids de archivos de la carpeta de drive (última parte del URL)
df_files    <- drive_ls(as_id("15lBZZB0mOCkRqO9vkZpg2LUFv4EQF6Wl"))
df_files # Todos los archivos deben ser extensión .dbf
temp        <- tempfile(fileext = ".dbf")

#---- Descargar la base COE1, desde el drive, al archivo temporal
dl          <- drive_download(as_id(df_files$id[str_detect(df_files$name, "COE1")]), path = temp, overwrite = T)
df_coe1_raw <- foreign::read.dbf(dl$local_path)

#---- Descargar la base COE2, desde el drive, al archivo temporal
dl          <- drive_download(as_id(df_files$id[str_detect(df_files$name, "COE2")]), path = temp, overwrite = T)
df_coe2_raw <- foreign::read.dbf(dl$local_path)

#---- Descargar la base SDEM, desde el drive, al archivo temporal
dl          <- drive_download(as_id(df_files$id[str_detect(df_files$name, "SDEM")]), path = temp, overwrite = T)
df_sdem_raw <- foreign::read.dbf(dl$local_path)


## 2.2. Pegar bases-------------------------------------------------------------

# Con llaves viejas

d_coe1 <- df_coe1_raw   %>% 
    rename_all(tolower) %>% 
    mutate(
        llave = paste0(
            cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
            # cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, tipo, ca, mes_cal
        )
    )%>% 
    mutate_at(
        vars(-starts_with("llave")),
        as.numeric
    ) 

d_coe2 <- df_coe2_raw %>% 
    rename_all(tolower) %>% 
    mutate(
        llave = paste0(
            cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
            # cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, tipo, ca, mes_cal
        )
    ) %>% 
    mutate_at(
        vars(-starts_with("llave")),
        as.numeric
    ) 

d_sdem <- df_sdem_raw %>% 
    rename_all(tolower) %>% 
    mutate(
        llaveh = paste0(
            cd_a, ent, con, v_sel, n_hog, h_mud
            # cd_a, ent, con, v_sel, n_hog, h_mud, tipo, ca, mes_cal
        ),
        llave = paste0(
            cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
            # cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, tipo, ca, mes_cal
        )
    ) %>% 
    mutate_at(
        vars(-c(starts_with("llave"),r_def,c_res)),
        as.numeric
    ) %>% 
    filter(
        as.character(r_def)=="00" & (as.character(c_res)=="1" | as.character(c_res)=="3")
    ) 

tempo <- d_sdem %>%
    rename(fac = fac_tri, t_loc = t_loc_tri) %>% 
    select(-c(cd_a, con, upm, d_sem, n_pro_viv, v_sel,fac_men,
              n_hog, h_mud, n_ent, per, ur)) %>% 
    # left_join(d_coe1 %>% select(-c(fac, ent)), by = c("llave")) %>%
    left_join(d_coe1 %>% select(-c(fac_tri, fac_men, ent)), by = c("llave")) %>%
    # left_join(d_coe2 %>% select(-c(fac, ent)), by = c("llave")) %>%
    left_join(d_coe2 %>% select(-c(fac_tri, fac_men, ent)), by = c("llave")) %>%
    select(-c(ends_with(".y"),ends_with(".x"))) %>% 
    filter(eda>14) %>% 
    select(llaveh, llave, n_ren, everything())%>% 
    mutate(sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)))

tempo_todo <- d_sdem %>%
    rename(fac = fac_tri, t_loc = t_loc_tri) %>% 
    select(-c(cd_a, con, upm, d_sem, n_pro_viv, v_sel,fac_men,
              n_hog, h_mud, n_ent, per, n_ren, ur)) %>%
    # left_join(d_coe1 %>% select(-c(fac, ent)), by = c("llave")) %>%
    left_join(d_coe1 %>% select(-c(fac_tri, fac_men, ent)), by = c("llave")) %>%
    # left_join(d_coe2 %>% select(-c(fac, ent)), by = c("llave")) %>%
    left_join(d_coe2 %>% select(-c(fac_tri, fac_men, ent)), by = c("llave")) %>%
    select(-c(ends_with(".y"),ends_with(".x")))%>% 
    mutate(sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)))




# 3. Bucle ENOE ----------------------------------------------------------------
# 4. Limpieza ------------------------------------------------------------------
# 5. Guardar datos -------------------------------------------------------------
# FIN. -------------------------------------------------------------------------