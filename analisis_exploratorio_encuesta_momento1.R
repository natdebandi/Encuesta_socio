library(tidyverse)
library(readr)
# Importar el CSV
encuesta <- read_csv("Trayectorias laborales vf kobo weightvec v3.csv", locale = locale(encoding = "UTF-8"))

# Verificación rápida
glimpse(encuesta)
nrow(encuesta)  # cantidad de respuestas
ncol(encuesta)  # cantidad de columnas

# Se limpian las columnas innecesarias según es detallado en el documento de decisiones de limpieza.
encuesta_clean <- encuesta %>%
  select(
    # Eliminar columnas viejas de q40 (versión anterior del formulario, sin datos)
    -`q40_estudios/s__estudios_de_nivel_primario`,
    -`q40_estudios/s__estudios_de_nivel_secundario`,
    -`q40_estudios/s__estudios_de_nivel_terciario__institut`,
    -`q40_estudios/s__estudios_universitarios__carrera_de_g`,
    -`q40_estudios/s__estudios_de_postgrado`,
    -`q40_estudios/s__capacitaciones_laborales_profesionale`,
    -`q40_estudios/no__no_estudi__desde_que_llegu__a_argent`,
    # Eliminar metadatos de Kobo irrelevantes para el análisis
    -`...1`, -`__version__`, -`_tags`, -`_index`,
    -start, -end
    # Se conservan: _uuid, _id, q0_consentimiento (por trazabilidad)
  )

cat("Columnas después de la limpieza:", ncol(encuesta_clean))

# Descargamos la librería survey para incorporar el trabajo con pesos muestrales.
if (!require(survey)) install.packages("survey")
library(survey)

library(survey)

# Declarar diseño
diseno <- svydesign(ids = ~1, weights = ~weightvec, data = encuesta_clean)

# Perfil sociodemográfico ponderado
# TABLAS DE DISTRIBUCIONES
# Nacionalidad
svytable(~nac, diseno) %>%
  as.data.frame() %>%
  mutate(porcentaje = round(Freq / sum(Freq) * 100, 1)) %>%
  arrange(desc(porcentaje))

# Sexo
svytable(~sexo, diseno) %>%
  as.data.frame() %>%
  mutate(porcentaje = round(Freq / sum(Freq) * 100, 1)) %>%
  arrange(desc(porcentaje))

# Edad agrupada — acá no ordenamos por porcentaje sino que mantenemos orden natural de la variable Edad
svytable(~edad_agrupada, diseno) %>%
  as.data.frame() %>%
  mutate(porcentaje = round(Freq / sum(Freq) * 100, 1))

# Zona del AMBA - Agrupación detallada en el doc de limpieza y calibración.
svytable(~zona, diseno) %>%
  as.data.frame() %>%
  mutate(porcentaje = round(Freq / sum(Freq) * 100, 1)) %>%
  arrange(desc(porcentaje))

# Estudios 
svytable(~q7_estudios, diseno) %>%
  as.data.frame() %>%
  mutate(
    porcentaje = round(Freq / sum(Freq) * 100, 1),
    q7_estudios = recode(q7_estudios,
                         "sin_estudios"             = "Sin estudios",
                         "primario_incompleto"      = "Primario incompleto",
                         "primario_completo"        = "Primario completo",
                         "secundario_incompleto"    = "Secundario incompleto",
                         "secundario_completo"      = "Secundario completo",
                         "terciario_incompleto"     = "Terciario incompleto",
                         "terciario_completo"       = "Terciario completo",
                         "universitario_incompleto" = "Universitario incompleto",
                         "universitario_completo"   = "Universitario completo",
                         "posgrado_incompleto"      = "Posgrado incompleto",
                         "posgrado_completo"        = "Posgrado completo",
                         "prefiero_no_responder"    = "Prefiero no responder"
                         
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Nivel educativo` = q7_estudios, `%` = porcentaje)

# Creamos una nueva variable, agrupando por estudios. (A revisar metodológicamente)
encuesta_clean <- encuesta_clean %>%
  mutate(
    q7_estudios_grupo = case_when(
      q7_estudios %in% c("sin_estudios",
                         "primario_incompleto",
                         "primario_completo")       ~ "Hasta primario completo",
      q7_estudios %in% c("secundario_incompleto",
                         "secundario_completo")     ~ "Hasta secundario completo",
      q7_estudios %in% c("terciario_incompleto",
                         "terciario_completo")      ~ "Hasta terciario completo",
      q7_estudios %in% c("universitario_incompleto",
                         "universitario_completo",
                         "posgrado_incompleto",
                         "posgrado_completo")       ~ "Universitario o posgrado (Incluye Univ. incompleto",
      q7_estudios == "prefiero_no_responder"        ~ "Prefiero no responder"
    )
  )

# Q7 - Refrescamos el df
diseno <- svydesign(ids = ~1, weights = ~weightvec, data = encuesta_clean)

svytable(~q7_estudios_grupo, diseno) %>%
  as.data.frame() %>%
  mutate(porcentaje = round(Freq / sum(Freq) * 100, 1)) %>%
  arrange(desc(porcentaje)) %>% 
  select(`Nivel educativo` = q7_estudios_grupo, `%` = porcentaje)


# Q8 - Convivencia en pareja
svytable(~q8_hogar_convivencia, diseno) %>%
  as.data.frame() %>%
  mutate(
    porcentaje = round(Freq / sum(Freq) * 100, 1),
    q8_hogar_convivencia = recode(q8_hogar_convivencia,
                                  "si_nacida_argentina"   = "Sí, con persona nacida en Argentina",
                                  "si_nacida_extranjero"   = "Sí, con persona nacida en otro país",
                                  "sin_conyugue"              = "No tiene cónyuge o pareja",
                                  "prefiero_no_responder" = "Prefiero no responder"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(Convivencia = q8_hogar_convivencia, `%` = porcentaje)

# Q9 -¿Tiene hijos menores?
svytable(~q9_hijos_menores, diseno) %>%
  as.data.frame() %>%
  mutate(
    porcentaje = round(Freq / sum(Freq) * 100, 1),
    q9_hijos_menores = recode(q9_hijos_menores,
                              "si" = "Sí",
                              "no" = "No"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Hijos menores de 18` = q9_hijos_menores, `%` = porcentaje)

# Q10 - Residencia de hijos menores (solo aplica a quienes respondieron Sí en q9)
svytable(~q10_hijos_menores_residencia, diseno) %>%
  as.data.frame() %>%
  filter(q10_hijos_menores_residencia != "") %>%  # excluir casos no aplicables
  mutate(
    porcentaje = round(Freq / sum(Freq) * 100, 1),
    q10_hijos_menores_residencia = recode(q10_hijos_menores_residencia,
                                          "argentina"          = "En Argentina",
                                          "otro_pais"          = "En otro país",
                                          "argentina_y_otro"   = "En Argentina y en otro país"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Residencia de hijos menores` = q10_hijos_menores_residencia, `%` = porcentaje)

# Q11 - Motivos de salida — Pregunta de selección múltiple (% sobre total de casos)
tibble(
  Motivo = c("Para estudiar u obtener nuevas experiencias",
             "Para conseguir mejor trabajo / mejorar ingresos",
             "Por violencia o persecución",
             "Para reagruparse con familia",
             "Vino con su familia siendo niño/a",
             "Otro"),
  `%` = c(
    svymean(~`q11_motivos_salida/estudio`,            diseno, na.rm = TRUE)[1] * 100,
    svymean(~`q11_motivos_salida/trabajo`,            diseno, na.rm = TRUE)[1] * 100,
    svymean(~`q11_motivos_salida/violencias`,         diseno, na.rm = TRUE)[1] * 100,
    svymean(~`q11_motivos_salida/reagrupación`,       diseno, na.rm = TRUE)[1] * 100,
    svymean(~`q11_motivos_salida/migracion_familiar`, diseno, na.rm = TRUE)[1] * 100,
    svymean(~`q11_motivos_salida/otro`,               diseno, na.rm = TRUE)[1] * 100
  )
) %>%
  mutate(`%` = round(`%`, 1)) %>%
  arrange(desc(`%`))


# MOMENTO 1: PAÍS DE ORIGEN

# Q12 - ¿En qué año salió de su país de origen?
unique(encuesta_clean$q12_anio_salida) # Se encontraron años con errores de carga. 
# Ver cuántos casos tienen valores problemáticos
# Exploración de los errores:
encuesta_clean %>%
  filter(q12_anio_salida < 1950 | q12_anio_salida > 2025) %>%
  select(q12_anio_salida, q19_anio_arribo, nac, edad) %>%
  arrange(q12_anio_salida)

# Corrección de valores inconsistentes en q12_anio_salida
# Se identificaron 11 casos con valores imposibles o inverosímiles.
# Criterio general: se intenta recuperar el año real usando edad, año de arribo
# y coherencia temporal. Se convierte a NA cuando no es posible inferir.

encuesta_clean <- encuesta_clean %>%
  mutate(q12_anio_salida = case_when(
    
    # RECUPERABLES
    # 219 → 2019: venezolano, 71 años, arribo 2019. Error tipográfico evidente.
    q12_anio_salida == 219  ~ 2019,
    
    # 2995 → 1995: colombiano, 59 años, arribo 1994.
    q12_anio_salida == 2995 ~ 1995,
    
    # 28 → 2018: venezolano, 37 años, arribo 2018
    q12_anio_salida == 28   ~ 2018,
    
    # 210 → 2010: 44 años, arribo 2010. Error tipográfico evidente.
    q12_anio_salida == 210  ~ 2010,
    
    # 19 → 2006: paraguayo, 38 años, arribo 2006. El encuestado registró su edad
    # al salir (19 años) en lugar del año. 2025 - 19 = 2006, coincide con arribo. Inferimos
    # que salió en 2006.
    q12_anio_salida == 19   ~ 2006,
    
    # 20 → 2006: peruano, 41 años, arribó en 2006. Misma hipótesis: registró edad
    # al salir (20 años). 2025 - 20 = 2005 ≈ 2006, coherente con arribo.
    q12_anio_salida == 20   ~ 2006,
    
    # 1949: persona de 76 años, arribo también 1949. Aunque inusual, está dentro
    # del universo de posibilidades. Se conserva el valor original.
    q12_anio_salida == 1949 ~ 1949,
    
    # IRRECUPERABLES → NA
    # 1: sin contexto suficiente para inferir año.
    # 18: arribo también es 18, ambos campos corruptos.
    # 22: hipótesis de edad al salir daría ~2003, no coincide con arribo 2009.
    # 1918: venezolano de 64 años, año imposible.
    q12_anio_salida %in% c(1, 18, 22, 1918) ~ NA_real_,
    
    # Todos los demás valores se conservan sin cambios
    TRUE ~ q12_anio_salida
  ))


# Refrescar diseño tras correcciones en q12
diseno <- svydesign(ids = ~1, weights = ~weightvec, data = encuesta_clean)

# Q13 - ¿Tenía trabajo antes de salir de su país de origen?
svytable(~q13_trabajo_origen, diseno) %>%
  as.data.frame() %>%
  mutate(
    porcentaje = round(Freq / sum(Freq) * 100, 1),
    q13_trabajo_origen = recode(q13_trabajo_origen,
                                "si"            = "Sí, trabajaba",
                                "no_desempleado" = "No, estaba desempleado/buscando trabajo",
                                "no_ninio"      = "No, era niño/a o adolescente",
                                "no_estudio"    = "No, solo estudiaba",
                                "no_jubilado"   = "No, ya estaba jubilado/retirado"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Situación laboral en origen` = q13_trabajo_origen, `%` = porcentaje)

# Creamos un subconjunto del diseño: solo quienes trabajaban en origen
diseno_trabajo_origen <- subset(diseno, q13_trabajo_origen == "si")

# Q14 - Situación ocupacional en origen
svytable(~q14_situacion_ocupacional_orig, diseno_trabajo_origen) %>%
  as.data.frame() %>%
  mutate(
    porcentaje = round(Freq / sum(Freq) * 100, 1),
    q14_situacion_ocupacional_orig = recode(q14_situacion_ocupacional_orig,
                                            "salario_fijo"  = "Trabajo con salario fijo",
                                            "cuenta_propia" = "Trabajo por cuenta propia",
                                            "changas"       = "Trabajos esporádicos / changas / subempleo",
                                            "trabajo_sin_remuneracion" = "Trabajo sin remuneración",
                                            "otro"          = "Otra"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Situación ocupacional en origen` = q14_situacion_ocupacional_orig, `%` = porcentaje)

# Subconjunto del diseño: solo salario fijo o cuenta propia en Q14
diseno_formalidad_origen <- subset(diseno, 
                                   q13_trabajo_origen == "si" & 
                                     q14_situacion_ocupacional_orig %in% c("salario_fijo", "cuenta_propia")
)

# Q15 - Formalidad del trabajo en origen
svytable(~q15_formalidad_trabajo, diseno_formalidad_origen) %>%
  as.data.frame() %>%
  mutate(
    porcentaje = round(Freq / sum(Freq) * 100, 1),
    q15_formalidad_trabajo = recode(q15_formalidad_trabajo,
                                    "formal"   = "Formal",
                                    "informal" = "Informal"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Formalidad del trabajo en origen` = q15_formalidad_trabajo, `%` = porcentaje)

# Para pasar a la Q15 hay que hacer un subconjunto: cuenta propia o changas en Q14
diseno_actividad_origen <- subset(diseno,
                                  q13_trabajo_origen == "si" &
                                    q14_situacion_ocupacional_orig %in% c("cuenta_propia", "changas")
)

# Q16 - Actividad laboral en origen
diseno_actividad_origen <- subset(diseno,
                                  q13_trabajo_origen == "si" &
                                    q14_situacion_ocupacional_orig %in% c("cuenta_propia", "changas")
)

svytable(~q16_actividad_origen, diseno_actividad_origen) %>%
  as.data.frame() %>%
  mutate(
    porcentaje = round(Freq / sum(Freq) * 100, 1),
    q16_actividad_origen = recode(q16_actividad_origen,
                                  "patron"              = "Patrón de un negocio (empleador)",
                                  "profesional"         = "Profesional independiente",
                                  "servicio_oficio"     = "Trabajo de servicio u oficio",
                                  "emprendimiento"      = "Emprendimiento / negocio familiar",
                                  "venta_ambulante"     = "Venta ambulante",
                                  "plataforma"          = "Trabajo en plataformas",
                                  "trabajo_estacional"  = "Trabajo estacional o por períodos",
                                  "otro"                = "Otra"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Actividad laboral en origen` = q16_actividad_origen, `%` = porcentaje)


# Q17 - Ocupación en origen (texto libre) — Trabajamos con nube de palabras

if (!require(wordcloud2)) install.packages("wordcloud2")
if (!require(tidytext)) install.packages("tidytext")
if (!require(SnowballC)) install.packages("SnowballC")

library(wordcloud2)
library(tidytext)
library(SnowballC)

stopwords_es <- c(
  # Artículos, preposiciones y conectores
  "de", "en", "la", "el", "los", "las", "un", "una", "y", "a", "por",
  "con", "del", "al", "se", "su", "que", "o", "para", "pero", "como",
  "más", "este", "esta", "esto", "entre", "hasta", "desde",
  # Términos poco informativos en este contexto
  "trabajo", "trabajaba", "trabajé", "sector", "área", "empresa", "famili")

palabras_q17 <- encuesta_clean %>%
  filter(!is.na(q17_ocupacion_origen)) %>%
  select(q17_ocupacion_origen) %>%
  unnest_tokens(word, q17_ocupacion_origen) %>%
  filter(!word %in% stopwords_es) %>%
  filter(nchar(word) > 3) %>%
  count(word, sort = TRUE)

# Nube de palabras — sin stemming, sin agrupación
wordcloud2(palabras_q17 %>% slice_max(n, n = 50),
           color = "random-dark",
           backgroundColor = "white",
           size = 0.8)


# Q18 - En qué medida el trabajo cubría necesidades básicas en origen
svytable(~q18_cubre_necesidades, diseno_trabajo_origen) %>%
  as.data.frame() %>%
  mutate(
    porcentaje = round(Freq / sum(Freq) * 100, 1),
    q18_cubre_necesidades = recode(q18_cubre_necesidades,
                                   "completamente"  = "Completamente",
                                   "bastante_bien"  = "Bastante bien",
                                   "justo"          = "Justo",
                                   "insuficiente"   = "Insuficiente",
                                   "muy_deficiente" = "Muy deficiente"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Cobertura de necesidades básicas` = q18_cubre_necesidades, `%` = porcentaje)

# Q19 - Año de arribo a Argentina (toda la muestra)
encuesta_clean %>%
  filter(!is.na(q19_anio_arribo)) %>%
  count(q19_anio_arribo) %>%
  arrange(q19_anio_arribo)

# Identificar valores problemáticos en q19_anio_arribo
encuesta_clean %>%
  filter(q19_anio_arribo < 1945 | q19_anio_arribo > 2025) %>%
  select(q19_anio_arribo, q12_anio_salida, nac, edad) %>%
  arrange(q19_anio_arribo)

# Corrección de valores inconsistentes en q19_anio_arribo
encuesta_clean <- encuesta_clean %>%
  mutate(q19_anio_arribo = case_when(
    
    # 21 → 2004: 41 años, salió en 2005. Hipótesis edad al llegar (2025 - 21 = 2004),
    # coherente con q12_anio_salida = 2005.
    q19_anio_arribo == 21   ~ 2004,
    
    # IRRECUPERABLES → NA
    # 18: paraguayo, 48 años, q12 también corrupto. Sin referencia para inferir.
    # 1918: venezolano, 64 años, año imposible (¿Quizás 2018?). q12 también corrupto.
    q19_anio_arribo %in% c(18, 1918) ~ NA_real_,
    
    TRUE ~ q19_anio_arribo
  ))

# Verificación
encuesta_clean %>%
  filter(q19_anio_arribo < 1945 | q19_anio_arribo > 2025 | is.na(q19_anio_arribo)) %>%
  select(q19_anio_arribo, q12_anio_salida, nac, edad) %>%
  arrange(q19_anio_arribo)