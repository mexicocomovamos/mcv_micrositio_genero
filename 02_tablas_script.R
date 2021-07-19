Sys.setlocale("LC_TIME", "es_ES")

# Paquetes ----
if(!require("lubridate")) install.packages("lubridate") & require("lubridate")
if(!require("hot.deck")) install.packages("hot.deck") & require("hot.deck")
if(!require("zoo")) install.packages("zoo") & require("zoo")
if(!require("stringi")) install.packages("stringi") & require("stringi")
if(!require("gganimate")) install.packages("gganimate") & require("gganimate")
if(!require("gridExtra")) install.packages("gridExtra") & require("gridExtra")
if(!require("ggthemes")) install.packages("ggthemes") & require("ggthemes")
if(!require("hrbrthemes")) install.packages("hrbrthemes") & require("hrbrthemes")
if(!require("magick")) install.packages("magick") & require("magick")
if(!require("scales")) install.packages("scales") & require("scales")
if(!require("RColorBrewer")) install.packages("RColorBrewer") & require("RColorBrewer")
if(!require("foreign")) install.packages("foreign") & require("foreign")
if(!require("srvyr")) install.packages("srvyr") & require("srvyr")
if(!require("openxlsx")) install.packages("openxlsx") & require("openxlsx")

# Tidyverse <3
require(tidyverse)



# Desactiva notación científica
options(scipen=999)

# Colores MCV -----
mcv_discrete <- c(
  "#6950d8", "#3CEAFA", "#00b783", 
  "#ff6260", "#ffaf84", "#ffbd41"
)

mcv_semaforo <- c(
  "#00b783", # verde
  "#E8D92E", # amarillo
  "#ffbd41", # naranja
  "#ff6260" # rojo
)


mcv_blacks <- c("black", "#D2D0CD", "#777777")

# Directorios ----
paste_inp <- function(x){paste0("mcv/enoe/01_datos/", x)}
paste_out <- function(x){paste0("mcv/mcv_micrositio_genero/03_productos/", x)}

# Línea de pobreza ----
lps <- tribble(
  ~rururb,~year_vector,~trim_vector,~lp,
  0,"05","1",712.54,
  1,"05","1",492.78,
  0,"05","2",741.49,
  1,"05","2",520.05,
  0,"05","3",740.37,
  1,"05","3",516.84,
  0,"05","4",736.48,
  1,"05","4",510.03,
  0,"06","1",754.24,
  1,"06","1",526.53,
  0,"06","2",748.55,
  1,"06","2",518.10,
  0,"06","3",764.24,
  1,"06","3",533.35,
  0,"06","4",797.76,
  1,"06","4",565.69,
  0,"07","1",814.70,
  1,"07","1",576.48,
  0,"07","2",803.92,
  1,"07","2",563.70,
  0,"07","3",810.79,
  1,"07","3",567.70,
  0,"07","4",830.52,
  1,"07","4",582.76,
  0,"08","1",839.18,
  1,"08","1",585.91,
  0,"08","2",858.24,
  1,"08","2",601.28,
  0,"08","3",875.52,
  1,"08","3",614.67,
  0,"08","4",906.75,
  1,"08","4",640.53,
  0,"09","1",922.00,
  1,"09","1",649.04,
  0,"09","2",949.84,
  1,"09","2",672.97,
  0,"09","3",967.60,
  1,"09","3",686.43,
  0,"09","4",968.90,
  1,"09","4",686.09,
  0,"10","1",992.68,
  1,"10","1",705.17,
  0,"10","2",987.38,
  1,"10","2",695.86,
  0,"10","3",979.48,
  1,"10","3",684.99,
  0,"10","4",1003.59,
  1,"10","4",705.71,
  0,"11","1",1021.50,
  1,"11","1",717.40,
  0,"11","2",1022.29,
  1,"11","2",717.83,
  0,"11","3",1023.17,
  1,"11","3",716.93,
  0,"11","4",1049.22,
  1,"11","4",740.43,
  0,"12","1",1079.48,
  1,"12","1",765.39,
  0,"12","2",1089.02,
  1,"12","2",770.93,
  0,"12","3",1130.09,
  1,"12","3",805.72,
  0,"12","4",1151.75,
  1,"12","4",820.38,
  0,"13","1",1166.22,
  1,"13","1",828.61,
  0,"13","2",1177.4,
  1,"13","2",837.18,
  0,"13","3",1177.99,
  1,"13","3",833.37,
  0,"13","4",1201.99,
  1,"13","4",853.72,
  0,"14","1",1234.8,
  1,"14","1",870.81,
  0,"14","2",1223.42,
  1,"14","2",854.08,
  0,"14","3",1243.83,
  1,"14","3",869.82,
  0,"14","4",1276.56,
  1,"14","4",899.27,
  0,"15","1",1264.53,
  1,"15","1",896.17,
  0,"15","2",1268.44,
  1,"15","2",901.34,
  0,"15","3",1282.51,
  1,"15","3",911.38,
  0,"15","4",1302.59,
  1,"15","4",926.32,
  0,"16","1",1338.64,
  1,"16","1",959.7,
  0,"16","2",1329.36,
  1,"16","2",947.13,
  0,"16","3",1323.88,
  1,"16","3",942.81,
  0,"16","4",1357.24,
  1,"16","4",970.61,
  0,"17","1",1376.88,
  1,"17","1",975.82,
  0,"17","2",1410.21,
  1,"17","2",1003.88,
  0,"17","3",1469.65,
  1,"17","3",1053.26,
  0,"17","4",1479.05,
  1,"17","4",1054.84,
  0,"18","1",1482.13,
  1,"18","1",1052.56,
  0,"18","2",1477.31,
  1,"18","2",1046.25,
  0,"18","3",1510.45,
  1,"18","3",1068.21,
  0,"18","4",1533.46,
  1,"18","4",1091.92,
  0,"19","1",1561.64,
  1,"19","1",1111.31,
  0,"19","2",1561.34,
  1,"19","2",1109.07,
  0,"19","3",1563.49,
  1,"19","3",1109.45,
  0,"19","4",1578.72,
  1,"19","4",1119.41,
  0,"20","1",1625.16,
  1,"20","1",1158.80,
  0,"20","3",1660.28,
  1,"20","3",1191.07,
  0,"20","4",1674.65,
  1,"20","4",1204.29,
  0,"21","1",1684.84,
  1,"21","1",1205.15
)

# Ingresos imputados de última ENOE_n
ingresos_imputados <- readRDS("mcv/enoe/01_datos/00_ingresos_imputados/01_ingresos_laborales_imputados/2021T1_ingresos_imputados.rds") 

# Procesamiento de última ENOE_n ----
# Últimos dígitos del año
i = "21"
# Trimestre
x = "1"

d_coe1 <- read.dbf(
  paste_inp(
    # ENOE
    # paste0("20", i, "trim", x, "_dbf/COE1T", x, i, ".dbf")
    # ENOE_N
    paste0("enoe_n_20", i, "_trim", x, "_dbf/ENOEN_COE1T", x, i, ".dbf")
  ), as.is = T
) %>% 
  rename_all(tolower) %>% 
  mutate(
    llave = paste0(
      # cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
      cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, tipo, ca, mes_cal
    )
  )%>% 
  mutate_at(
    vars(-starts_with("llave")),
    as.numeric
  ) 

d_coe2 <- read.dbf(
  paste_inp(
    # paste0("20", i, "trim", x, "_dbf/COE2T", x, i, ".dbf")
    paste0("enoe_n_20", i, "_trim", x, "_dbf/ENOEN_COE2T", x, i, ".dbf")
  ), as.is = T
) %>% 
  rename_all(tolower) %>% 
  mutate(
    llave = paste0(
      # cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
      cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, tipo, ca, mes_cal
    )
  ) %>% 
  mutate_at(
    vars(-starts_with("llave")),
    as.numeric
  ) 

d_sdem <- read.dbf(
  paste_inp(
    # paste0("20", i, "trim", x, "_dbf/SDEMT", x, i, ".dbf")
    paste0("enoe_n_20", i, "_trim", x, "_dbf/ENOEN_SDEMT", x, i, ".dbf")
  ), as.is = T
) %>% 
  rename_all(tolower) %>% 
  mutate(
    llaveh = paste0(
      # cd_a, ent, con, v_sel, n_hog, h_mud
      cd_a, ent, con, v_sel, n_hog, h_mud, tipo, ca, mes_cal
    ),
    llave = paste0(
      # cd_a, ent, con, v_sel, n_hog, h_mud, n_ren
      cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, tipo, ca, mes_cal
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
  select(llaveh, llave, n_ren, everything())

tempo_todo <- d_sdem %>%
  rename(fac = fac_tri, t_loc = t_loc_tri) %>% 
  select(-c(cd_a, con, upm, d_sem, n_pro_viv, v_sel,fac_men,
            n_hog, h_mud, n_ent, per, n_ren, ur)) %>% 
  # left_join(d_coe1 %>% select(-c(fac, ent)), by = c("llave")) %>% 
  left_join(d_coe1 %>% select(-c(fac_tri, fac_men, ent)), by = c("llave")) %>% 
  # left_join(d_coe2 %>% select(-c(fac, ent)), by = c("llave")) %>% 
  left_join(d_coe2 %>% select(-c(fac_tri, fac_men, ent)), by = c("llave")) %>% 
  select(-c(ends_with(".y"),ends_with(".x")))


# General ----

# 1. PEA Y PNEA ----
test_gen <- tempo %>% 
  mutate(
    tipo = case_when(
      clase1 == 1 ~ "Población económicamente activa",
      clase1 == 2 ~ "Población no económicamente activa",
      T ~ NA_character_
    )
  ) %>% 
  as_survey_design(weights = fac) %>% 
  group_by(tipo) %>% 
  srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
  left_join(
    tempo %>% 
      mutate(
        tipo = case_when(
          clase1 == 1 ~ "Población económicamente activa",
          clase1 == 2 ~ "Población no económicamente activa",
          T ~ NA_character_
        )
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(tipo) %>% 
      srvyr::summarise(prop = survey_mean(na.rm = T))
  ) %>% 
  select(-ends_with("se")) %>% 
  mutate(ord = "1") %>% 
  # 2. Población ocupada y desocupada ----
  bind_rows(
    tempo %>% 
      filter(clase1==1) %>% 
      mutate(
        tipo = case_when(
          clase2 == 1 ~ "Ocupada",
          clase2 == 2 ~ "Desocupada",
          T ~ NA_character_
        )
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(tipo) %>% 
      srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
      left_join(
        tempo %>% 
          filter(clase1==1) %>% 
          mutate(
            tipo = case_when(
              clase2 == 1 ~ "Ocupada",
              clase2 == 2 ~ "Desocupada",
              T ~ NA_character_
            )
          ) %>% 
          as_survey_design(weights = fac) %>% 
          group_by(tipo) %>% 
          srvyr::summarise(prop = survey_mean(na.rm = T))
      ) %>% 
      select(-ends_with("se")) %>% 
      mutate(ord = "2")
  ) %>% 
  # 3. Población disponible y no disponible ----
  bind_rows(
    
    tempo %>% 
      filter(clase1==2) %>% 
      mutate(
        tipo = case_when(
          clase2 == 3 ~ "Disponibles",
          clase2 == 4 ~ "No disponibles",
          T ~ NA_character_
        )
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(tipo) %>% 
      srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
      left_join(
        tempo %>% 
          filter(clase1==2) %>% 
          mutate(
            tipo = case_when(
              clase2 == 3 ~ "Disponibles",
              clase2 == 4 ~ "No disponibles",
              T ~ NA_character_
            )
          ) %>% 
          as_survey_design(weights = fac) %>% 
          group_by(tipo) %>% 
          srvyr::summarise(prop = survey_mean(na.rm = T))
      ) %>% 
      select(-ends_with("se")) %>% 
      mutate(ord = "3")
    
    
  ) %>% 
  # 4. Población subocupada ----
  bind_rows(
  
    tempo %>% 
      filter(clase1 == 1) %>% 
      mutate(
        tipo = case_when(
          sub_o == 1 ~ "Población subocupada",
          sub_o == 2 ~ "Población no subocupada",
          T ~ NA_character_
        )
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(tipo) %>% 
      srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
      left_join(
        tempo %>%
          filter(clase1 == 1) %>% 
          mutate(
            tipo = case_when(
              sub_o == 1 ~ "Población subocupada",
              sub_o == 2 ~ "Población no subocupada",
              T ~ NA_character_
            )
          ) %>% 
          as_survey_design(weights = fac) %>% 
          group_by(tipo) %>% 
          srvyr::summarise(prop = survey_mean(na.rm = T))
      ) %>% 
      select(-ends_with("se")) %>% 
      drop_na(tipo) %>% 
      mutate(ord = "4")
      
  ) %>% 
  # 5. Tipo de empleo (formal e informal) ----
  bind_rows(
    
    tempo %>% 
      filter(clase1 == 1 & clase2 == 1) %>% 
      mutate(
        tipo = case_when(
          emp_ppal == 1 ~ "Empleo informal",
          emp_ppal == 2 ~ "Empleo formal",
          T ~ NA_character_
        )
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(tipo) %>% 
      srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
      left_join(
        tempo %>%
          filter(clase1 == 1 & clase2 == 1) %>% 
          mutate(
            tipo = case_when(
              emp_ppal == 1 ~ "Empleo informal",
              emp_ppal == 2 ~ "Empleo formal",
              T ~ NA_character_
            )
          ) %>% 
          as_survey_design(weights = fac) %>% 
          group_by(tipo) %>% 
          srvyr::summarise(prop = survey_mean(na.rm = T))
      ) %>% 
      select(-ends_with("se")) %>% 
      drop_na(tipo) %>% 
      mutate(ord = "5")
    
  ) %>% 
  # 6. Pobreza laboral ----
  bind_rows(
    
    
    tempo_todo %>% 
      mutate(
        ocupado = ifelse(clase1 == 1 & clase2 == 1, 1, 0),
        ingreso = case_when(
          ocupado == 0 | (is.na(p6b2) & (p6_9==9 | p6a3==3)) ~ 0,
          is.na(p6b2) & p6c == 1 ~ salario*0.5,
          is.na(p6b2) & p6c == 2 ~ salario*1,
          is.na(p6b2) & p6c == 3 ~ salario*1.5,
          is.na(p6b2) & p6c == 4 ~ salario*2.5,
          is.na(p6b2) & p6c == 5 ~ salario*4,
          is.na(p6b2) & p6c == 6 ~ salario*7.5,
          is.na(p6b2) & p6c == 7 ~ salario*10,
          T ~ p6b2
        ),
        tamh = 1,
        rururb = if_else((t_loc>=1 & t_loc<=3), 0, 1),
        mv = if_else(is.na(ingreso) & ocupado == 1, 1, 0)
      ) %>% 
      select(
        llaveh, tamh, ingreso, mv, ocupado, rururb, factor = fac, ent, mun
      ) %>% 
      group_by(llaveh) %>% 
      summarise(
        tamh = sum(tamh, na.rm = T),
        ingreso = sum(ingreso, na.rm = T),
        mv = sum(mv, na.rm = T),
        ocupado = sum(ocupado, na.rm = T),
        rururb = mean(rururb, na.rm = T),
        factor = mean(factor, na.rm = T),
        ent = mean(ent, na.rm = T),
        mun=first(mun)
      ) %>% 
      select(llaveh, tamh, ingreso, mv, ocupado, rururb, factor, ent, mun) %>%
      mutate(
        mv = if_else(!is.na(mv) & mv > 0, 1, 0),
        year_vector = as.character(str_pad(i,2,"l","0")),
        trim_vector = as.character(x)
      ) %>% 
      filter(
        mv != 1
      ) %>% 
      ungroup() %>% 
      left_join(
        lps
      ) %>% 
      select(-c(year_vector, trim_vector)) %>% 
      mutate(
        factorp = factor*tamh,
        ingreso_per_cap = ingreso/tamh,
        pob_lab = case_when(
          ingreso_per_cap<lp ~ "1",
          T ~ "0"
        )
      ) %>% 
      as_survey_design(weights = factorp) %>% 
      srvyr::group_by(pob_lab) %>% 
      srvyr::summarise(tlp = survey_total(na.rm = T)) %>% 
      select(-contains("se")) %>% 
      filter(pob_lab == "1") %>% 
      mutate(tipo="Pobreza laboral") %>% 
      select(tipo, tot = tlp) %>% 
      left_join(
        
        tempo_todo %>% 
          mutate(
            ocupado = ifelse(clase1 == 1 & clase2 == 1, 1, 0),
            ingreso = case_when(
              ocupado == 0 | (is.na(p6b2) & (p6_9==9 | p6a3==3)) ~ 0,
              is.na(p6b2) & p6c == 1 ~ salario*0.5,
              is.na(p6b2) & p6c == 2 ~ salario*1,
              is.na(p6b2) & p6c == 3 ~ salario*1.5,
              is.na(p6b2) & p6c == 4 ~ salario*2.5,
              is.na(p6b2) & p6c == 5 ~ salario*4,
              is.na(p6b2) & p6c == 6 ~ salario*7.5,
              is.na(p6b2) & p6c == 7 ~ salario*10,
              T ~ p6b2
            ),
            tamh = 1,
            rururb = if_else((t_loc>=1 & t_loc<=3), 0, 1),
            mv = if_else(is.na(ingreso) & ocupado == 1, 1, 0)
          ) %>% 
          select(
            llaveh, tamh, ingreso, mv, ocupado, rururb, factor = fac, ent, mun
          ) %>% 
          group_by(llaveh) %>% 
          summarise(
            tamh = sum(tamh, na.rm = T),
            ingreso = sum(ingreso, na.rm = T),
            mv = sum(mv, na.rm = T),
            ocupado = sum(ocupado, na.rm = T),
            rururb = mean(rururb, na.rm = T),
            factor = mean(factor, na.rm = T),
            ent = mean(ent, na.rm = T),
            mun=first(mun)
          ) %>% 
          select(llaveh, tamh, ingreso, mv, ocupado, rururb, factor, ent, mun) %>%
          mutate(
            mv = if_else(!is.na(mv) & mv > 0, 1, 0),
            year_vector = as.character(str_pad(i,2,"l","0")),
            trim_vector = as.character(x)
          ) %>% 
          filter(
            mv != 1
          ) %>% 
          ungroup() %>% 
          left_join(
            lps
          ) %>% 
          select(-c(year_vector, trim_vector)) %>% 
          mutate(
            factorp = factor*tamh,
            ingreso_per_cap = ingreso/tamh,
            pob_lab = case_when(
              ingreso_per_cap<lp ~ "1",
              T ~ "0"
            )
          ) %>% 
          as_survey_design(weights = factorp) %>% 
          srvyr::group_by(pob_lab) %>% 
          srvyr::summarise(tlp = survey_mean(na.rm = T)) %>% 
          select(-contains("se")) %>% 
          filter(pob_lab == "1") %>% 
          mutate(tipo="Pobreza laboral") %>% 
          select(tipo, prop = tlp) 
        
        
      )%>% 
      mutate(ord = "6")
    
    
    
  ) %>% 
  # 7. Ingresos promedio ----
  bind_rows(
    
    ingresos_imputados %>% 
      as_survey_design(weights = factor) %>% 
      summarise(
        tot = survey_mean(ingreso_sm, na.rm = T)
      ) %>% 
      select(-ends_with("se")) %>% 
      mutate(tipo="Ingreso laboral promedio") %>% 
      select(tipo, tot)%>% 
      bind_rows(
        ingresos_imputados %>% 
          mutate(
            tipo = tipo_formalidad
          ) %>% 
          as_survey_design(weights = factor) %>% 
          group_by(tipo) %>% 
          summarise(
            tot = survey_mean(ingreso_sm, na.rm = T)
          ) %>% 
          select(-ends_with("se")) %>% 
          mutate(tipo=paste0("Ingreso laboral promedio (",tipo,")")) %>% 
          select(tipo, tot)
        
      ) %>% 
      mutate(ord = "7")
    
  ) %>% 
  # 8. Horas trabajadas a la semana ----
  bind_rows(
    
    tempo %>% 
      filter(clase1==1) %>% 
      as_survey_design(weights = fac) %>% 
      summarise(
        tot = survey_mean(hrsocup, na.rm = T)
      ) %>% 
      mutate(tipo = "Horas trabajadas a la semana (promedio)") %>% 
      select(-contains("_se")) %>% 
      mutate(ord = "8")
    
  ) %>% 
  # 9. Horas trabajadas en el tareas del hogar ----
  # El nombre de las variables cambia entre trimestres 1 y el resto
  bind_rows(
    tempo %>% 
      mutate(
        p11_h2 = ifelse(p11_h2>97,NA,p11_h2),
        horas_semana_cuidados = p11_h2 + (p11_m2/60),
        p11_h7 = ifelse(p11_h7>97,NA,p11_h7),
        horas_semana_quehaceres = p11_h7 + (p11_m7/60),
        horas_semana_quehaceres_cuidados = horas_semana_cuidados+horas_semana_quehaceres
      ) %>% 
      as_survey_design(weights = fac) %>% 
      summarise(tot = survey_mean(horas_semana_quehaceres_cuidados, na.rm = T)) %>% 
      select(-ends_with("_se")) %>% 
      mutate(tipo = "Horas trabajadas en tareas del hogar y de cuidados (no remunerado)",
             ord = "9")
  ) %>% 
  mutate(sexo="General") 



# Sexo ----
# 1. PEA Y PNEA ----
test_sex <- tempo %>% 
  mutate(
    tipo = case_when(
      clase1 == 1 ~ "Población económicamente activa",
      clase1 == 2 ~ "Población no económicamente activa",
      T ~ NA_character_
    ),
    sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))
  ) %>% 
  as_survey_design(weights = fac) %>% 
  group_by(sexo,tipo) %>% 
  srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
  left_join(
    tempo %>% 
      mutate(
        tipo = case_when(
          clase1 == 1 ~ "Población económicamente activa",
          clase1 == 2 ~ "Población no económicamente activa",
          T ~ NA_character_
        ),
        sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(sexo,tipo) %>% 
      srvyr::summarise(prop = survey_mean(na.rm = T))
  ) %>% 
  select(-ends_with("se")) %>% 
  mutate(ord="1") %>% 
  # 2. Población ocupada y desocupada ----
  bind_rows(
    tempo %>% 
      filter(clase1 == 1) %>% 
      mutate(
        tipo = case_when(
          clase2 == 1 ~ "Ocupada",
          clase2 == 2 ~ "Desocupada",
          T ~ NA_character_
        ),
        sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(sexo,tipo) %>% 
      srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
      left_join(
        tempo %>% 
          filter(clase1 == 1) %>% 
          mutate(
            tipo = case_when(
              clase2 == 1 ~ "Ocupada",
              clase2 == 2 ~ "Desocupada",
              T ~ NA_character_
            ),
            sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))
          ) %>% 
          as_survey_design(weights = fac) %>% 
          group_by(sexo,tipo) %>% 
          srvyr::summarise(prop = survey_mean(na.rm = T))
      ) %>% 
      select(-ends_with("se"))   %>% 
      mutate(ord="2") 
  )%>% 
  # 3. Población disponible y no disponible ----
  bind_rows(
    
    tempo %>% 
      filter(clase1 == 2) %>% 
      mutate(
        tipo = case_when(
          clase2 == 3 ~ "Disponibles",
          clase2 == 4 ~ "No disponibles",
          T ~ NA_character_
        ),
        sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(sexo,tipo) %>% 
      srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
      left_join(
        tempo %>% 
          filter(clase1 == 2) %>% 
          mutate(
            tipo = case_when(
              clase2 == 3 ~ "Disponibles",
              clase2 == 4 ~ "No disponibles",
              T ~ NA_character_
            ),
            sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))
          ) %>% 
          as_survey_design(weights = fac) %>% 
          group_by(sexo,tipo) %>% 
          srvyr::summarise(prop = survey_mean(na.rm = T))
      ) %>% 
      select(-ends_with("se"))  %>% 
      mutate(ord="3")
    
  ) %>% 
  # 4. Población subocupada ----
  bind_rows(
    
    tempo %>% 
      filter(clase1 == 1) %>% 
      mutate(
        tipo = case_when(
          sub_o == 1 ~ "Población subocupada",
          sub_o == 2 ~ "Población no subocupada",
          T ~ NA_character_
        ),
        sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(sexo,tipo) %>% 
      srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
      left_join(
        tempo %>%
          filter(clase1 == 1) %>% 
          mutate(
            tipo = case_when(
              sub_o == 1 ~ "Población subocupada",
              sub_o == 2 ~ "Población no subocupada",
              T ~ NA_character_
            ),
            sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))
          ) %>% 
          as_survey_design(weights = fac) %>% 
          group_by(sexo,tipo) %>% 
          srvyr::summarise(prop = survey_mean(na.rm = T))
      ) %>% 
      select(-ends_with("se")) %>% 
      drop_na(tipo) %>% 
      mutate(ord="4") 
    
  ) %>% 
  # 5. Tipo de empleo (formal e informal) ----
  bind_rows(
    
    tempo %>% 
      filter(clase1 == 1 & clase2 == 1) %>% 
      mutate(
        tipo = case_when(
          emp_ppal == 1 ~ "Empleo informal",
          emp_ppal == 2 ~ "Empleo formal",
          T ~ NA_character_
        ),
        sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(sexo,tipo) %>% 
      srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
      left_join(
        tempo %>% 
        filter(clase1 == 1 & clase2 == 1) %>% 
          mutate(
            tipo = case_when(
              emp_ppal == 1 ~ "Empleo informal",
              emp_ppal == 2 ~ "Empleo formal",
              T ~ NA_character_
            ),
            sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))
          ) %>% 
          as_survey_design(weights = fac) %>% 
          group_by(sexo,tipo) %>% 
          srvyr::summarise(prop = survey_mean(na.rm = T))
      ) %>% 
      select(-ends_with("se"))  %>% 
      mutate(ord="5") 
    
  )%>% 
  # 6. Pobreza laboral ----
  bind_rows(
    
    tempo_todo %>% 
      mutate(
        ocupado = ifelse(clase1 == 1 & clase2 == 1, 1, 0),
        ingreso = case_when(
          ocupado == 0 | (is.na(p6b2) & (p6_9==9 | p6a3==3)) ~ 0,
          is.na(p6b2) & p6c == 1 ~ salario*0.5,
          is.na(p6b2) & p6c == 2 ~ salario*1,
          is.na(p6b2) & p6c == 3 ~ salario*1.5,
          is.na(p6b2) & p6c == 4 ~ salario*2.5,
          is.na(p6b2) & p6c == 5 ~ salario*4,
          is.na(p6b2) & p6c == 6 ~ salario*7.5,
          is.na(p6b2) & p6c == 7 ~ salario*10,
          T ~ p6b2
        ),
        tamh = 1,
        rururb = if_else((t_loc>=1 & t_loc<=3), 0, 1),
        mv = if_else(is.na(ingreso) & ocupado == 1, 1, 0)
      ) %>% 
      select(
        llaveh, tamh, ingreso, mv, ocupado, rururb, factor = fac, ent, mun
      ) %>% 
      group_by(llaveh) %>% 
      summarise(
        tamh = sum(tamh, na.rm = T),
        ingreso = sum(ingreso, na.rm = T),
        mv = sum(mv, na.rm = T),
        ocupado = sum(ocupado, na.rm = T),
        rururb = mean(rururb, na.rm = T),
        factor = mean(factor, na.rm = T),
        ent = mean(ent, na.rm = T),
        mun=first(mun)
      ) %>% 
      select(llaveh, tamh, ingreso, mv, ocupado, rururb, factor, ent, mun) %>%
      mutate(
        mv = if_else(!is.na(mv) & mv > 0, 1, 0),
        year_vector = as.character(str_pad(i,2,"l","0")),
        trim_vector = as.character(x)
      ) %>% 
      filter(
        mv != 1
      ) %>% 
      ungroup() %>% 
      left_join(
        lps
      ) %>% 
      select(-c(year_vector, trim_vector)) %>% 
      mutate(
        factorp = factor*tamh,
        ingreso_per_cap = ingreso/tamh,
        pob_lab = case_when(
          ingreso_per_cap<lp ~ "1",
          T ~ "0"
        )
      ) %>% 
      left_join(
        tempo_todo %>% 
          mutate(sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))) %>% 
          select(llaveh, sexo, fac_original = fac)
      ) %>% 
      as_survey_design(weights = fac_original) %>% 
      srvyr::group_by(sexo, pob_lab) %>% 
      srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
      select(-contains("_se"))  %>% 
      filter(pob_lab == "1") %>% 
      mutate(tipo="Pobreza laboral") %>% 
      select(sexo, tipo, tot) %>% 
      left_join(
        
        tempo_todo %>% 
          mutate(
            ocupado = ifelse(clase1 == 1 & clase2 == 1, 1, 0),
            ingreso = case_when(
              ocupado == 0 | (is.na(p6b2) & (p6_9==9 | p6a3==3)) ~ 0,
              is.na(p6b2) & p6c == 1 ~ salario*0.5,
              is.na(p6b2) & p6c == 2 ~ salario*1,
              is.na(p6b2) & p6c == 3 ~ salario*1.5,
              is.na(p6b2) & p6c == 4 ~ salario*2.5,
              is.na(p6b2) & p6c == 5 ~ salario*4,
              is.na(p6b2) & p6c == 6 ~ salario*7.5,
              is.na(p6b2) & p6c == 7 ~ salario*10,
              T ~ p6b2
            ),
            tamh = 1,
            rururb = if_else((t_loc>=1 & t_loc<=3), 0, 1),
            mv = if_else(is.na(ingreso) & ocupado == 1, 1, 0)
          ) %>% 
          select(
            llaveh, tamh, ingreso, mv, ocupado, rururb, factor = fac, ent, mun
          ) %>% 
          group_by(llaveh) %>% 
          summarise(
            tamh = sum(tamh, na.rm = T),
            ingreso = sum(ingreso, na.rm = T),
            mv = sum(mv, na.rm = T),
            ocupado = sum(ocupado, na.rm = T),
            rururb = mean(rururb, na.rm = T),
            factor = mean(factor, na.rm = T),
            ent = mean(ent, na.rm = T),
            mun=first(mun)
          ) %>% 
          select(llaveh, tamh, ingreso, mv, ocupado, rururb, factor, ent, mun) %>%
          mutate(
            mv = if_else(!is.na(mv) & mv > 0, 1, 0),
            year_vector = as.character(str_pad(i,2,"l","0")),
            trim_vector = as.character(x)
          ) %>% 
          filter(
            mv != 1
          ) %>% 
          ungroup() %>% 
          left_join(
            lps
          ) %>% 
          select(-c(year_vector, trim_vector)) %>% 
          mutate(
            factorp = factor*tamh,
            ingreso_per_cap = ingreso/tamh,
            pob_lab = case_when(
              ingreso_per_cap<lp ~ "1",
              T ~ "0"
            )
          ) %>% 
          left_join(
            tempo_todo %>% 
              mutate(sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))) %>% 
              select(llaveh, sexo, fac_original = fac)
          ) %>% 
          as_survey_design(weights = fac_original) %>% 
          srvyr::group_by(sexo, pob_lab) %>% 
          srvyr::summarise(prop = survey_mean(na.rm = T)) %>% 
          select(-contains("_se"))  %>% 
          filter(pob_lab == "1") %>% 
          mutate(tipo="Pobreza laboral") %>% 
          select(sexo, tipo, prop)
        
        
      )  %>% 
      mutate(ord="6") 
    
    
    
  )%>% 
  # 7. Ingresos promedio ----
  bind_rows(
    
    ingresos_imputados %>% 
      mutate(sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))) %>% 
      as_survey_design(weights = factor) %>% 
      group_by(sexo) %>% 
      summarise(
        tot = survey_mean(ingreso_sm, na.rm = T)
      ) %>% 
      select(-ends_with("se")) %>% 
      mutate(tipo="Ingreso laboral promedio") %>% 
      select(sexo,tipo, tot)%>% 
      bind_rows(
        ingresos_imputados %>% 
          mutate(
            tipo = tipo_formalidad,
            sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))
          ) %>% 
          as_survey_design(weights = factor) %>% 
          group_by(sexo,tipo) %>% 
          summarise(
            tot = survey_mean(ingreso_sm, na.rm = T)
          ) %>% 
          select(-ends_with("se")) %>% 
          mutate(tipo=paste0("Ingreso laboral promedio (",tipo,")")) %>% 
          select(sexo,tipo, tot)
        
      )  %>% 
      mutate(ord="7")
    
  ) %>% 
  # 8. Horas trabajadas a la semana ----
  bind_rows(
    
    tempo %>% 
      filter(clase1==1) %>% 
      mutate(sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(sexo) %>% 
      summarise(
        tot = survey_mean(hrsocup, na.rm = T)
      ) %>% 
      mutate(tipo = "Horas trabajadas a la semana (promedio)") %>% 
      select(-contains("_se")) %>% 
      mutate(ord="8") 
    
    
  ) %>% 
  # 9. Horas trabajadas en tareas del hogar ----
  bind_rows(
    tempo %>% 
      mutate(
        p11_h2 = ifelse(p11_h2>97,NA,p11_h2),
        horas_semana_cuidados = p11_h2 + (p11_m2/60),
        p11_h7 = ifelse(p11_h7>97,NA,p11_h7),
        horas_semana_quehaceres = p11_h7 + (p11_m7/60),
        horas_semana_quehaceres_cuidados = horas_semana_cuidados+horas_semana_quehaceres,
        sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(sexo) %>% 
      summarise(tot = survey_mean(horas_semana_quehaceres_cuidados, na.rm = T)) %>% 
      select(-ends_with("_se")) %>% 
      mutate(tipo = "Horas trabajadas en tareas del hogar y de cuidados (no remunerado)") %>% 
      mutate(ord="9")
  )

# Tabla para documento ----
test_gen %>% 
  rename_at(vars(tot:prop),~paste0(.,"_general")) %>% 
  select(-sexo) %>% 
  left_join(
    test_sex %>% 
      pivot_wider(
        names_from = "sexo",
        values_from = c(tot,prop)
      )
  ) %>%
  select(ord, tipo, contains("gen"), contains("mbr"), contains("uje")) %>% 
  mutate_at(
    vars(contains("prop")),
    ~round(.*100,1)
  ) %>% 
  mutate_at(
    vars(contains("tot")),
    ~round(.)
  ) %>% 
  janitor::clean_names() %>% 
  mutate(
    
    General = ifelse(
      !is.na(prop_general),
      paste0(
        prettyNum(tot_general,big.mark = ","),
        "<br/>[ ", prop_general,"% ]"
      ),
      prettyNum(tot_general,big.mark = ",")
    ),
    
    Hombres = ifelse(
      !is.na(prop_hombres),
      paste0(
        prettyNum(tot_hombres,big.mark = ","),
        "<br/>[ ", prop_hombres,"% ]"
      ),
      prettyNum(tot_hombres,big.mark = ",")
    ),
    
    Mujeres = ifelse(
      !is.na(prop_mujeres),
      paste0(
        prettyNum(tot_mujeres,big.mark = ","),
        "<br/>[ ", prop_mujeres,"% ]"
      ),
      prettyNum(tot_mujeres,big.mark = ",")
    )
    
  ) %>% 
  select(ord, tipo, General:Mujeres) 




# Por sectores ----

# General ----
# 1. Población ocupada por sector ----
test_sector <- 
  tempo %>% 
  filter(clase2==1) %>% 
  mutate(
    sector = case_when(
      rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
      rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
      rama_est2 == 3 ~ "Industria manufacturera",
      rama_est2 == 4 ~ "Construcción",
      rama_est2 == 5 ~ "Comercio",
      rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
      rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
      rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
      rama_est2 == 9 ~ "Servicios sociales",
      rama_est2 == 10 ~ "Servicios diversos",
      rama_est2 == 11 ~ "Gobierno y organismos internacionales",
      T ~ "99_No especificado"
    )
  ) %>% 
  as_survey_design(weights = fac) %>% 
  group_by(sector) %>% 
  srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
  left_join(
    tempo %>% 
      filter(clase2==1) %>% 
      mutate(
        sector = case_when(
          rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
          rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
          rama_est2 == 3 ~ "Industria manufacturera",
          rama_est2 == 4 ~ "Construcción",
          rama_est2 == 5 ~ "Comercio",
          rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
          rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
          rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
          rama_est2 == 9 ~ "Servicios sociales",
          rama_est2 == 10 ~ "Servicios diversos",
          rama_est2 == 11 ~ "Gobierno y organismos internacionales",
          T ~ "99_No especificado"
        )
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(sector) %>% 
      srvyr::summarise(prop = survey_mean(na.rm = T))
  ) %>% 
  select(-ends_with("se")) %>% 
  mutate(
    tipo = "Población ocupada",
    ord = "1"
  ) %>% 
  # 2. Población subocupada ----
bind_rows(
  
  tempo %>% 
    filter(clase1 == 1) %>% 
    mutate(
      sector = case_when(
        rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
        rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
        rama_est2 == 3 ~ "Industria manufacturera",
        rama_est2 == 4 ~ "Construcción",
        rama_est2 == 5 ~ "Comercio",
        rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
        rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
        rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
        rama_est2 == 9 ~ "Servicios sociales",
        rama_est2 == 10 ~ "Servicios diversos",
        rama_est2 == 11 ~ "Gobierno y organismos internacionales",
        T ~ "99_No especificado"
      ),
      tipo = case_when(
        sub_o == 1 ~ "Población subocupada",
        sub_o == 2 ~ "Población no subocupada",
        T ~ NA_character_
      )
    ) %>% 
    as_survey_design(weights = fac) %>% 
    group_by(sector, tipo) %>% 
    srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
    left_join(
      tempo %>%
        filter(clase1 == 1) %>% 
        mutate(
          sector = case_when(
            rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
            rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
            rama_est2 == 3 ~ "Industria manufacturera",
            rama_est2 == 4 ~ "Construcción",
            rama_est2 == 5 ~ "Comercio",
            rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
            rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
            rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
            rama_est2 == 9 ~ "Servicios sociales",
            rama_est2 == 10 ~ "Servicios diversos",
            rama_est2 == 11 ~ "Gobierno y organismos internacionales",
            T ~ "99_No especificado"
          ),
          tipo = case_when(
            sub_o == 1 ~ "Población subocupada",
            sub_o == 2 ~ "Población no subocupada",
            T ~ NA_character_
          )
        ) %>% 
        as_survey_design(weights = fac) %>% 
        group_by(sector, tipo) %>% 
        srvyr::summarise(prop = survey_mean(na.rm = T))
    ) %>% 
    select(-ends_with("se")) %>% 
    drop_na(tipo) %>% 
    mutate(ord = "2")
  
) %>% 
  # 3. Tipo de empleo (formal e informal) ----
bind_rows(
  
  tempo %>% 
    filter(clase1 == 1 & clase2 == 1) %>% 
    mutate(
      sector = case_when(
        rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
        rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
        rama_est2 == 3 ~ "Industria manufacturera",
        rama_est2 == 4 ~ "Construcción",
        rama_est2 == 5 ~ "Comercio",
        rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
        rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
        rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
        rama_est2 == 9 ~ "Servicios sociales",
        rama_est2 == 10 ~ "Servicios diversos",
        rama_est2 == 11 ~ "Gobierno y organismos internacionales",
        T ~ "99_No especificado"
      ),
      tipo = case_when(
        emp_ppal == 1 ~ "Empleo informal",
        emp_ppal == 2 ~ "Empleo formal",
        T ~ NA_character_
      )
    ) %>% 
    as_survey_design(weights = fac) %>% 
    group_by(sector,tipo) %>% 
    srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
    left_join(
      tempo %>%
        filter(clase1 == 1 & clase2 == 1) %>% 
        mutate(
          sector = case_when(
            rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
            rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
            rama_est2 == 3 ~ "Industria manufacturera",
            rama_est2 == 4 ~ "Construcción",
            rama_est2 == 5 ~ "Comercio",
            rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
            rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
            rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
            rama_est2 == 9 ~ "Servicios sociales",
            rama_est2 == 10 ~ "Servicios diversos",
            rama_est2 == 11 ~ "Gobierno y organismos internacionales",
            T ~ "99_No especificado"
          ),
          tipo = case_when(
            emp_ppal == 1 ~ "Empleo informal",
            emp_ppal == 2 ~ "Empleo formal",
            T ~ NA_character_
          )
        ) %>% 
        as_survey_design(weights = fac) %>% 
        group_by(sector,tipo) %>% 
        srvyr::summarise(prop = survey_mean(na.rm = T))
    ) %>% 
    select(-ends_with("se")) %>% 
    drop_na(tipo) %>% 
    mutate(ord = "3")
  
) %>% 
  # 4. Ingresos promedio ----
bind_rows(
  
  ingresos_imputados %>% 
    as_survey_design(weights = factor) %>% 
    group_by(sector) %>% 
    summarise(
      tot = survey_mean(ingreso_sm, na.rm = T)
    ) %>% 
    select(-ends_with("se")) %>% 
    mutate(tipo="Ingreso laboral promedio",
           sector=ifelse(is.na(sector),"99_No especificado",sector)) %>% 
    select(sector,tipo, tot)%>% 
    bind_rows(
      ingresos_imputados %>% 
        mutate(
          tipo = tipo_formalidad
        ) %>% 
        as_survey_design(weights = factor) %>% 
        group_by(sector,tipo) %>% 
        summarise(
          tot = survey_mean(ingreso_sm, na.rm = T)
        ) %>% 
        select(-ends_with("se")) %>% 
        mutate(tipo=paste0("Ingreso laboral promedio (",tipo,")"),
               sector=ifelse(is.na(sector),"99_No especificado",sector)) %>% 
        select(sector,tipo, tot)
      
    ) %>% 
    mutate(ord = "4")
  
) %>% 
  # 5. Horas trabajadas a la semana ----
bind_rows(
  
  tempo %>% 
    filter(clase1==1) %>% 
    mutate(
      sector = case_when(
        rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
        rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
        rama_est2 == 3 ~ "Industria manufacturera",
        rama_est2 == 4 ~ "Construcción",
        rama_est2 == 5 ~ "Comercio",
        rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
        rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
        rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
        rama_est2 == 9 ~ "Servicios sociales",
        rama_est2 == 10 ~ "Servicios diversos",
        rama_est2 == 11 ~ "Gobierno y organismos internacionales",
        T ~ "99_No especificado"
      )
    ) %>% 
    as_survey_design(weights = fac) %>% 
    group_by(sector) %>% 
    summarise(
      tot = survey_mean(hrsocup, na.rm = T)
    ) %>% 
    mutate(tipo = "Horas trabajadas a la semana (promedio)") %>% 
    select(-contains("_se")) %>% 
    mutate(ord = "5")
  
) 


# Sexo ----
# 1. Población ocupada por sector ----
test_sector_sex <- 
  tempo %>% 
  filter(clase2==1) %>% 
  mutate(
    sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)),
    sector = case_when(
      rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
      rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
      rama_est2 == 3 ~ "Industria manufacturera",
      rama_est2 == 4 ~ "Construcción",
      rama_est2 == 5 ~ "Comercio",
      rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
      rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
      rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
      rama_est2 == 9 ~ "Servicios sociales",
      rama_est2 == 10 ~ "Servicios diversos",
      rama_est2 == 11 ~ "Gobierno y organismos internacionales",
      T ~ "99_No especificado"
    )
  ) %>% 
  as_survey_design(weights = fac) %>% 
  group_by(sexo, sector) %>% 
  srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
  left_join(
    tempo %>% 
      filter(clase2==1) %>% 
      mutate(
        sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)),
        sector = case_when(
          rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
          rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
          rama_est2 == 3 ~ "Industria manufacturera",
          rama_est2 == 4 ~ "Construcción",
          rama_est2 == 5 ~ "Comercio",
          rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
          rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
          rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
          rama_est2 == 9 ~ "Servicios sociales",
          rama_est2 == 10 ~ "Servicios diversos",
          rama_est2 == 11 ~ "Gobierno y organismos internacionales",
          T ~ "99_No especificado"
        )
      ) %>% 
      as_survey_design(weights = fac) %>% 
      group_by(sexo, sector) %>% 
      srvyr::summarise(prop = survey_mean(na.rm = T))
  ) %>% 
  select(-ends_with("se")) %>% 
  mutate(
    tipo = "Población ocupada",
    ord = "1"
  ) %>% 
  # 2. Población subocupada ----
bind_rows(
  
  tempo %>% 
    filter(clase1 == 1) %>% 
    mutate(
      sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)),
      sector = case_when(
        rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
        rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
        rama_est2 == 3 ~ "Industria manufacturera",
        rama_est2 == 4 ~ "Construcción",
        rama_est2 == 5 ~ "Comercio",
        rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
        rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
        rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
        rama_est2 == 9 ~ "Servicios sociales",
        rama_est2 == 10 ~ "Servicios diversos",
        rama_est2 == 11 ~ "Gobierno y organismos internacionales",
        T ~ "99_No especificado"
      ),
      tipo = case_when(
        sub_o == 1 ~ "Población subocupada",
        sub_o == 2 ~ "Población no subocupada",
        T ~ NA_character_
      )
    ) %>% 
    as_survey_design(weights = fac) %>% 
    group_by(sexo,sector, tipo) %>% 
    srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
    left_join(
      tempo %>%
        filter(clase1 == 1) %>% 
        mutate(
          sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)),
          sector = case_when(
            rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
            rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
            rama_est2 == 3 ~ "Industria manufacturera",
            rama_est2 == 4 ~ "Construcción",
            rama_est2 == 5 ~ "Comercio",
            rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
            rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
            rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
            rama_est2 == 9 ~ "Servicios sociales",
            rama_est2 == 10 ~ "Servicios diversos",
            rama_est2 == 11 ~ "Gobierno y organismos internacionales",
            T ~ "99_No especificado"
          ),
          tipo = case_when(
            sub_o == 1 ~ "Población subocupada",
            sub_o == 2 ~ "Población no subocupada",
            T ~ NA_character_
          )
        ) %>% 
        as_survey_design(weights = fac) %>% 
        group_by(sexo,sector, tipo) %>% 
        srvyr::summarise(prop = survey_mean(na.rm = T))
    ) %>% 
    select(-ends_with("se")) %>% 
    drop_na(tipo) %>% 
    mutate(ord = "2")
  
) %>% 
  # 3. Tipo de empleo (formal e informal) ----
bind_rows(
  
  tempo %>% 
    filter(clase1 == 1 & clase2 == 1) %>% 
    mutate(
      sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)),
      sector = case_when(
        rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
        rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
        rama_est2 == 3 ~ "Industria manufacturera",
        rama_est2 == 4 ~ "Construcción",
        rama_est2 == 5 ~ "Comercio",
        rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
        rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
        rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
        rama_est2 == 9 ~ "Servicios sociales",
        rama_est2 == 10 ~ "Servicios diversos",
        rama_est2 == 11 ~ "Gobierno y organismos internacionales",
        T ~ "99_No especificado"
      ),
      tipo = case_when(
        emp_ppal == 1 ~ "Empleo informal",
        emp_ppal == 2 ~ "Empleo formal",
        T ~ NA_character_
      )
    ) %>% 
    as_survey_design(weights = fac) %>% 
    group_by(sexo,sector,tipo) %>% 
    srvyr::summarise(tot = survey_total(na.rm = T)) %>% 
    left_join(
      tempo %>%
        filter(clase1 == 1 & clase2 == 1) %>% 
        mutate(
          sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)),
          sector = case_when(
            rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
            rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
            rama_est2 == 3 ~ "Industria manufacturera",
            rama_est2 == 4 ~ "Construcción",
            rama_est2 == 5 ~ "Comercio",
            rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
            rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
            rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
            rama_est2 == 9 ~ "Servicios sociales",
            rama_est2 == 10 ~ "Servicios diversos",
            rama_est2 == 11 ~ "Gobierno y organismos internacionales",
            T ~ "99_No especificado"
          ),
          tipo = case_when(
            emp_ppal == 1 ~ "Empleo informal",
            emp_ppal == 2 ~ "Empleo formal",
            T ~ NA_character_
          )
        ) %>% 
        as_survey_design(weights = fac) %>% 
        group_by(sexo,sector,tipo) %>% 
        srvyr::summarise(prop = survey_mean(na.rm = T))
    ) %>% 
    select(-ends_with("se")) %>% 
    drop_na(tipo) %>% 
    mutate(ord = "3")
  
) %>% 
  # 4. Ingresos promedio ----
bind_rows(
  
  ingresos_imputados %>% 
    mutate(sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA))) %>% 
    as_survey_design(weights = factor) %>% 
    group_by(sexo,sector) %>% 
    summarise(
      tot = survey_mean(ingreso_sm, na.rm = T)
    ) %>% 
    select(-ends_with("se")) %>% 
    mutate(tipo="Ingreso laboral promedio",
           sector=ifelse(is.na(sector),"99_No especificado",sector)) %>% 
    select(sector,tipo, tot)%>% 
    bind_rows(
      ingresos_imputados %>% 
        mutate(
          sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)),
          tipo = tipo_formalidad
        ) %>% 
        as_survey_design(weights = factor) %>% 
        group_by(sexo,sector,tipo) %>% 
        summarise(
          tot = survey_mean(ingreso_sm, na.rm = T)
        ) %>% 
        select(-ends_with("se")) %>% 
        mutate(tipo=paste0("Ingreso laboral promedio (",tipo,")"),
               sector=ifelse(is.na(sector),"99_No especificado",sector)) %>% 
        select(sector,tipo, tot)
      
    ) %>% 
    mutate(ord = "4")
  
) %>% 
  # 5. Horas trabajadas a la semana ----
bind_rows(
  
  tempo %>% 
    filter(clase1==1) %>% 
    mutate(
      sexo = ifelse(sex==1,"Hombres",ifelse(sex==2,"Mujeres",NA)),
      sector = case_when(
        rama_est2 == 1 ~ "Agricultura, ganadería, silvicultura, caza y pesca",
        rama_est2 == 2 ~ "Industria extractiva y de la electricidad",
        rama_est2 == 3 ~ "Industria manufacturera",
        rama_est2 == 4 ~ "Construcción",
        rama_est2 == 5 ~ "Comercio",
        rama_est2 == 6 ~ "Restaurantes y servicios de alojamiento",
        rama_est2 == 7 ~ "Transportes, comunicaciones, correo y almacenamiento",
        rama_est2 == 8 ~ "Servicios profesionales, financieros y corporativos",
        rama_est2 == 9 ~ "Servicios sociales",
        rama_est2 == 10 ~ "Servicios diversos",
        rama_est2 == 11 ~ "Gobierno y organismos internacionales",
        T ~ "99_No especificado"
      )
    ) %>% 
    as_survey_design(weights = fac) %>% 
    group_by(sexo,sector) %>% 
    summarise(
      tot = survey_mean(hrsocup, na.rm = T)
    ) %>% 
    mutate(tipo = "Horas trabajadas a la semana (promedio)") %>% 
    select(-contains("_se")) %>% 
    mutate(ord = "5")
  
) 


# Tabla para micrositio sectores ----
sector <- bind_rows(
  test_sector %>% mutate(sexo="General"), test_sector_sex
) %>% 
  filter(!str_starts(sector, "99"))

sector_tot <- sector %>% 
  select(sector, tipo, sexo, tot) %>% 
  pivot_wider(names_from = "sexo", values_from = "tot") %>% 
  group_split(sector)


sector_prop <- sector %>% 
  select(sector, tipo, sexo, prop) %>% 
  pivot_wider(names_from = "sexo", values_from = "prop") %>% 
  group_split(sector)

wb <- loadWorkbook(paste_out("01_sectores_tot.xlsx"))

for(i in 1:length(sector_tot)){
  
  writeData(
    
    wb = wb,
    sheet = i,
    x = sector_tot[[i]],
    colNames = T,
    rowNames = F
    
  )
  
}
saveWorkbook(wb,paste_out("01_sectores_tot.xlsx"), overwrite = T)



wb <- loadWorkbook(paste_out("02_sectores_prop.xlsx"))

for(i in 1:length(sector_prop)){
  
  writeData(
    
    wb = wb,
    sheet = i,
    x = sector_prop[[i]],
    colNames = T,
    rowNames = F
    
  )
  
}
saveWorkbook(wb,paste_out("02_sectores_prop.xlsx"), overwrite = T)

# Descargables ----
write.xlsx(
  test_gen %>% 
    bind_rows(test_sex) %>% 
    select(tipo, sexo, tot, prop) %>% 
    pivot_wider(names_from = "sexo", values_from = c("tot","prop")) %>% 
    janitor::clean_names() %>% 
    select(tipo, contains("general"), contains("hombre"), contains("muj")),
  paste_out("01_descargable_general.xlsx")
)

write.xlsx(
  sector %>% 
    select(sector, tipo, sexo, tot, prop) %>% 
    pivot_wider(names_from = "sexo", values_from = c("tot","prop")) %>% 
    janitor::clean_names() %>% 
    select(sector, tipo, contains("general"), contains("hombre"), contains("muj")) %>% 
    arrange(sector),
  paste_out("02_descargable_sectores.xlsx")
)
