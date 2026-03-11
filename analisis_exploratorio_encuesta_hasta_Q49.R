library(tidyverse)
library(readr)
# Importar el CSV
encuesta <- read_csv("data/Trayectorias laborales vf kobo weightvec v3.csv", locale = locale(encoding = "UTF-8"))

# Verificación rápida
glimpse(encuesta)

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

# AGRUPACIÓN DE PAÍSES — basada en clasificación de Camila
encuesta_clean <- encuesta_clean %>%
  mutate(
    # Migrante regional (latinoamericano + Caribe)
    inregion_migrant = case_when(
      nacionalidad %in% c("bolivia", "brasil", "chile", "colombia", "ecuador",
                          "haiti", "paraguay", "per", "rep_blica_dominicana",
                          "uruguay", "venezuela") ~ 1,
      nacionalidad %in% c("Haiti", "Venezuela", "Mexico", "México", "Cuba",
                          "Guatemala", "Panamá", "Costa Rica", "Puerto Rico",
                          "Trinidad y Tobago") ~ 1,
      TRUE ~ 0
    ),
    
    # Migrante del sur global (agrega Senegal y Guinea Bissau)
    south_migrant = case_when(
      inregion_migrant == 1 ~ 1,
      nacionalidad == "senegal" ~ 1,
      otro_pais %in% c("Senegal", "Guinea Bissau") ~ 1,
      TRUE ~ 0
    ),
    
    # Países principales del estudio
    big5_country = case_when(
      nacionalidad %in% c("bolivia", "colombia", "paraguay", "per", "venezuela") ~ 1,
      TRUE ~ 0
    )
  )
# EXPLORACIÓN SIN PONDERAR — cantidad de casos por nacionalidad
encuesta_clean %>%
  filter(!is.na(nacionalidad)) %>%
  count(nacionalidad, sort = TRUE) %>%
  mutate(porcentaje_muestra = round(n / sum(n) * 100, 1)) %>%
  select(Nacionalidad = nacionalidad, `N (sin ponderar)` = n, `% muestra` = porcentaje_muestra)


# Perfil sociodemográfico ponderado
# TABLAS DE DISTRIBUCIONES
# NACIONALIDAD PONDERADA
encuesta_clean %>%
  group_by(nac) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(porcentaje)) %>%
  select(Nacionalidad = nac, `%` = porcentaje)

# COMPARACIÓN — distribución ponderada vs sin ponderar
encuesta_clean %>%
  filter(!is.na(nacionalidad)) %>%
  group_by(nacionalidad) %>%
  summarise(
    n_sin_ponderar = n(),
    n_ponderado    = round(sum(weightvec), 1)
  ) %>%
  mutate(
    pct_sin_ponderar = round(n_sin_ponderar / sum(n_sin_ponderar) * 100, 1),
    pct_ponderado    = round(n_ponderado    / sum(n_ponderado)    * 100, 1)
  ) %>%
  arrange(desc(n_sin_ponderar)) %>%
  select(Nacionalidad = nacionalidad,
         `N`          = n_sin_ponderar,
         `% sin pond` = pct_sin_ponderar,
         `% ponderado`= pct_ponderado)


# SEXO
encuesta_clean %>%
  group_by(sexo) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(porcentaje)) %>%
  select(Sexo = sexo, `%` = porcentaje)

# EDAD AGRUPADA — orden cronológico
encuesta_clean %>%
  group_by(edad_agrupada) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  select(`Grupo etario` = edad_agrupada, `%` = porcentaje)

# ZONA
encuesta_clean %>%
  group_by(zona) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(porcentaje)) %>%
  select(Zona = zona, `%` = porcentaje)

# DATOS EDUCATIVOS Y CONVIVENCIALES

# Q7 NIVEL EDUCATIVO DETALLE
encuesta_clean %>%
  group_by(q7_estudios) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
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

# NIVEL EDUCATIVO AGRUPADO (A revisar metodológicamente)
encuesta_clean %>%
  group_by(q7_estudios_grupo) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(porcentaje)) %>%
  select(`Nivel educativo (agrupado)` = q7_estudios_grupo, `%` = porcentaje)

# Q8 CONVIVENCIA EN PAREJA
encuesta_clean %>%
  filter(!is.na(q8_hogar_convivencia)) %>%
  group_by(q8_hogar_convivencia) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q8_hogar_convivencia = recode(q8_hogar_convivencia,
                                  "si_nacida_argentina"   = "Sí, con persona nacida en Argentina",
                                  "si_nacida_extranjero"  = "Sí, con persona nacida en otro país",
                                  "sin_conyugue"          = "No tiene cónyuge o pareja",
                                  "prefiero_no_responder" = "Prefiero no responder"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(Convivencia = q8_hogar_convivencia, `%` = porcentaje)

# Q9 HIJOS MENORES
encuesta_clean %>%
  group_by(q9_hijos_menores) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q9_hijos_menores = recode(q9_hijos_menores,
                              "si" = "Sí",
                              "no" = "No"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Hijos menores de 18` = q9_hijos_menores, `%` = porcentaje)

# RESIDENCIA DE HIJOS MENORES
# Q10 - Solo aplica a quienes respondieron Sí en q9 — filtramos antes de calcular
encuesta_clean %>%
  filter(q9_hijos_menores == "si") %>%
  filter(!is.na(q10_hijos_menores_residencia)) %>%
  group_by(q10_hijos_menores_residencia) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q10_hijos_menores_residencia = recode(q10_hijos_menores_residencia,
                                          "argentina"        = "En Argentina",
                                          "otro_pais"        = "En otro país",
                                          "argentina_y_otro" = "En Argentina y en otro país"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Residencia de hijos menores` = q10_hijos_menores_residencia, `%` = porcentaje)


# Q11 - MOTIVOS DE SALIDA (selección múltiple)
# El porcentaje se calcula sobre el total de la muestra — la suma supera 100%

total_pesos <- sum(encuesta_clean$weightvec)

tibble(
  Motivo = c(
    "Para conseguir mejor trabajo / mejorar ingresos",
    "Para estudiar u obtener nuevas experiencias",
    "Vino con su familia siendo niño/a",
    "Para reagruparse con familia",
    "Por violencia o persecución",
    "Otro"
  ),
  `%` = c(
    round(sum(encuesta_clean$`q11_motivos_salida/trabajo`            * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1),
    round(sum(encuesta_clean$`q11_motivos_salida/estudio`            * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1),
    round(sum(encuesta_clean$`q11_motivos_salida/migracion_familiar` * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1),
    round(sum(encuesta_clean$`q11_motivos_salida/reagrupación`       * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1),
    round(sum(encuesta_clean$`q11_motivos_salida/violencias`         * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1),
    round(sum(encuesta_clean$`q11_motivos_salida/otro`               * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1)
  )
) %>%
  arrange(desc(`%`))

##### MOMENTO 1: PAÍS DE ORIGEN

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


# Q13 - SITUACIÓN LABORAL ANTES DE MIGRAR
encuesta_clean %>%
  filter(!is.na(q13_trabajo_origen)) %>%
  group_by(q13_trabajo_origen) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q13_trabajo_origen = recode(q13_trabajo_origen,
                                "si"             = "Sí, trabajaba",
                                "no_desempleado" = "No, estaba desempleado/buscando trabajo",
                                "no_ninio"       = "No, era niño/a o adolescente",
                                "no_estudio"     = "No, solo estudiaba",
                                "no_jubilado"    = "No, ya estaba jubilado/retirado"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Situación laboral en origen` = q13_trabajo_origen, `%` = porcentaje)

# Q14 - TIPO DE OCUPACIÓN EN ORIGEN
# Filtreamos a quienes trabajaban en origen (Q13 = "si")
encuesta_clean %>%
  filter(q13_trabajo_origen == "si") %>%
  filter(!is.na(q14_situacion_ocupacional_orig)) %>%
  group_by(q14_situacion_ocupacional_orig) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q14_situacion_ocupacional_orig = recode(q14_situacion_ocupacional_orig,
                                            "salario_fijo"             = "Trabajo con salario fijo",
                                            "cuenta_propia"            = "Trabajo por cuenta propia",
                                            "changas"                  = "Trabajos esporádicos / changas / subempleo",
                                            "trabajo_sin_remuneracion" = "Trabajo sin remuneración",
                                            "otro"                     = "Otra"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Situación ocupacional en origen` = q14_situacion_ocupacional_orig, `%` = porcentaje)



# Q15 - FORMALIDAD DEL TRABAJO EN ORIGEN
# Solo quienes tenían salario fijo o cuenta propia en origen (Q14)
encuesta_clean %>%
  filter(q13_trabajo_origen == "si") %>%
  filter(q14_situacion_ocupacional_orig %in% c("salario_fijo", "cuenta_propia")) %>%
  filter(!is.na(q15_formalidad_trabajo)) %>%
  group_by(q15_formalidad_trabajo) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q15_formalidad_trabajo = recode(q15_formalidad_trabajo,
                                    "formal"   = "Formal",
                                    "informal" = "Informal"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Formalidad del trabajo en origen` = q15_formalidad_trabajo, `%` = porcentaje)

# Q16 - ACTIVIDAD LABORAL EN ORIGEN
# Solo quienes trabajaban por cuenta propia o changas en origen (Q14)
encuesta_clean %>%
  filter(q13_trabajo_origen == "si") %>%
  filter(q14_situacion_ocupacional_orig %in% c("cuenta_propia", "changas")) %>%
  filter(!is.na(q16_actividad_origen)) %>%
  group_by(q16_actividad_origen) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q16_actividad_origen = recode(q16_actividad_origen,
                                  "patron"             = "Patrón de un negocio (empleador)",
                                  "profesional"        = "Profesional independiente",
                                  "servicio_oficio"    = "Trabajo de servicio u oficio",
                                  "emprendimiento"     = "Emprendimiento / negocio familiar",
                                  "venta_ambulante"    = "Venta ambulante",
                                  "plataforma"         = "Trabajo en plataformas",
                                  "trabajo_estacional" = "Trabajo estacional o por períodos",
                                  "otro"               = "Otra"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Actividad laboral en origen` = q16_actividad_origen, `%` = porcentaje)

# Q17 - Ocupación en origen (texto libre) — 
encuesta_clean <- encuesta_clean %>%
  mutate(
    q17_lower = str_to_lower(q17_ocupacion_origen),
    
    ocupacion_origen = case_when(
      
      # 1 - Trabajo doméstico y de cuidados
      str_detect(q17_lower, "limpieza|camarera|niñ|cuidado|cuidad|mesera|doméstic|domestic|empleada del hogar|ama de casa|casa de familia|empleada de casa|empleada en casa") ~ 1,
      
      # 2 - Profesional y técnico
      str_detect(q17_lower, "ingenier|abog|medic|médic|odont|bioquim|bioquím|contador|auditor|psicol|psicól|psicopedag|farmac|nutricion|veterinar|citotecn|trabajador social|trabajadora social|lic\\.|licenciad|enferm|salud|químic|quimic|dermatoc|cosmetol|architect|arquitect|programad|informát|informatic|desarroll|web|software|sociolog|sociólog|antropol|economista|paralegal|jurídic|juridic|aduaner") ~ 2,
      
      # 3 - Administrativo, financiero y oficina
      str_detect(q17_lower, "data entry|administr|adm |rrhh|banco|empleado|oficina|secretar|recepcion|contabil|analista|asistente|impuesto|recursos humanos|calidad|operacion|operación|soporte|marketing") ~ 3,
      
      # 4 - Ama de casa / trabajo no remunerado
      str_detect(q17_lower, "ama de casa|hogar") ~ 4,
      
      # 5 - Agrícola y rural
      str_detect(q17_lower, "agric|algodón|algodon|verdul|campo|cosecha|cosechero|ganadería|ganaderia|chacra|rural|minero|miner|madera|^agrícola$|^agricola$") ~ 5,
      
      # 6 - Oficios manuales y técnicos
      str_detect(q17_lower, "mantenimiento|herrero|soldad|electric|metal|fabr|tecnico|técnico|mecanic|mecán|plomer|carpinter|albañil|albanil|construcc|costrucc|c0nstructor|pintor|costur|peluquer|peñuquer|estilista|chapa|tejido|textil|operari|operaría|joyería|joyeria|ropa|maletero|cargador|changarin|changas|seguridad|guardia|auxiliar de carga|designer de joyas|tránsito aéreo|transito aereo") ~ 6,
      
      # 7 - Ventas, comercio y logística
      str_detect(q17_lower, "venta|vendedor|vendedora|negocio|distribu|almacén|almacen|supermercado|comerci|logistic|depósito|deposito|tienda|librer|cajero|cajera|taxista|chofer|chófer|camionero|transport|despacho|mercadeo|consumo masivo|vetas") ~ 7,
      
      # 8 - Atención al cliente y servicios
      str_detect(q17_lower, "atención|atencion|teleoperador|bartender|delivery|mozo|gastrono|cociner|chef|call center|turismo|hotelería|hoteleria|masoterap|estética|estetica|entrenador|musculac|ayudante de cocina|cantina") ~ 8,
      
      # 9 - Consultoría y gerencia
      str_detect(q17_lower, "consult|project manager|freelancer|asesor|gerente|manager|coordinad|supervisor|jefe|jefa|director|directora|team leader|mando medio|gestión|gestion") ~ 9,
      
      # 10 - Educación, cultura y medios
      str_detect(q17_lower, "maestra|maestr|profesor|universitario|docente|docencia|educac|educador|periodis|comunicac|comunicad|diseñ|diseñador|arte|artista|música|musica|radialis|locutor|audiovisual|fotografo|fotógraf|operadora cultural|apoyo escolar|ayuda escolar|actriz") ~ 10,
      
      # 11 - Emprendimiento e independiente
      str_detect(q17_lower, "emprend|enprendim|taller|independiente|propio|propietari|empresari|dueño|start up") ~ 11,
      
      # 12 - Sector público
      str_detect(q17_lower, "public|municipal|consulado|empresa estatal|estado|gobierno|funcionario|empleada pública|empleado público|empleada publica|empleado publico") ~ 12,
      
      # 13 - Investigación y academia
      str_detect(q17_lower, "investig|proyectos sociales|medio ambientales|académic|academico|becari|doctoral") ~ 13,
      
      # 0 - Sin clasificar (incluye irrecuperables: "of", "Ñabiadi", "Era una playa", etc.)
      !is.na(q17_lower) ~ 0,
      TRUE ~ NA_real_
    )
  )

# TABLA COMPARATIVA — ponderada vs sin ponderar
encuesta_clean %>%
  filter(!is.na(ocupacion_origen)) %>%
  mutate(
    label_ocupacion = recode(as.character(ocupacion_origen),
                             "0"  = "Sin clasificar",
                             "1"  = "Trabajo doméstico y de cuidados",
                             "2"  = "Profesional y técnico",
                             "3"  = "Administrativo y financiero",
                             "4"  = "Ama de casa / no remunerado",
                             "5"  = "Agrícola y rural",
                             "6"  = "Oficios manuales y técnicos",
                             "7"  = "Ventas y comercio",
                             "8"  = "Atención al cliente y servicios",
                             "9"  = "Consultoría y gerencia",
                             "10" = "Educación, cultura y medios",
                             "11" = "Emprendimiento e independiente",
                             "12" = "Sector público",
                             "13" = "Investigación y academia"
    )
  ) %>%
  group_by(label_ocupacion) %>%
  summarise(
    n_sin_ponderar = n(),
    n_ponderado    = sum(weightvec)
  ) %>%
  mutate(
    pct_sin_ponderar = round(n_sin_ponderar / sum(n_sin_ponderar) * 100, 1),
    pct_ponderado    = round(n_ponderado    / sum(n_ponderado)    * 100, 1)
  ) %>%
  arrange(desc(n_sin_ponderar)) %>%
  select(
    `Ocupación en origen`  = label_ocupacion,
    `N`                    = n_sin_ponderar,
    `% sin ponderar`       = pct_sin_ponderar,
    `% ponderado`          = pct_ponderado
  )

# Verificación de los casos que quedaron "Sin clasificar"
encuesta_clean %>%
  filter(ocupacion_origen == 0) %>%
  select(q17_ocupacion_origen) %>%
  print(n = Inf)

#Trabajamos con nube de palabras

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


# Q18 - COBERTURA DE NECESIDADES BÁSICAS EN ORIGEN
# Solo quienes trabajaban en origen (Q13 = "si")
encuesta_clean %>%
  filter(q13_trabajo_origen == "si") %>%
  filter(!is.na(q18_cubre_necesidades)) %>%
  group_by(q18_cubre_necesidades) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
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

##### MOMENTO 2 — solo quienes llegaron antes de 2024
# Quienes llegaron en 2024 o 2025 se saltan este bloque
encuesta_momento2 <- encuesta_clean %>%
  filter(q19_anio_arribo < 2024)

# Verificación: 1037 Registros 
nrow(encuesta_momento2)

# Q20 - DNI durante el primer año
encuesta_momento2 %>%
  filter(!is.na(q20_DNI_primer_anio)) %>%
  group_by(q20_DNI_primer_anio) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q20_DNI_primer_anio = recode(q20_DNI_primer_anio,
                                 "si"                    = "Sí",
                                 "no"                    = "No",
                                 "prefiero_no_responder" = "Prefiero no responder / No sé"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Obtuvo DNI en el primer año` = q20_DNI_primer_anio, `%` = porcentaje)


# Q21 - Situación laboral durante el primer año
encuesta_momento2 %>%
  filter(!is.na(q21_trabajo_primer_anio)) %>%
  group_by(q21_trabajo_primer_anio) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q21_trabajo_primer_anio = recode(q21_trabajo_primer_anio,
                                     "si"             = "Sí, trabajó",
                                     "no_jubilado"    = "No, ya estaba jubilado/retirado",
                                     "no_estudio"     = "No, solo estudió",
                                     "no_desempleado" = "No, estuvo desempleado/buscando trabajo",
                                     "no_ninio"       = "No, era niño/a o adolescente"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Situación laboral primer año` = q21_trabajo_primer_anio, `%` = porcentaje)

# Q22 - Situación ocupacional durante el primer año
# Solo quienes trabajaron en el primer año (Q21 = "si")
encuesta_momento2 %>%
  filter(q21_trabajo_primer_anio == "si") %>%
  filter(!is.na(q22_ocupacion_primer_anio)) %>%
  group_by(q22_ocupacion_primer_anio) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q22_ocupacion_primer_anio = recode(q22_ocupacion_primer_anio,
                                       "salario_fijo"             = "Trabajo con salario fijo",
                                       "cuenta_propia"            = "Trabajo por cuenta propia",
                                       "changas"                  = "Trabajos esporádicos / changas / subempleo",
                                       "trabajo_sin_remuneracion" = "Trabajo sin remuneración",
                                       "otro"                     = "Otra"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Situación ocupacional primer año` = q22_ocupacion_primer_anio, `%` = porcentaje)

# Exploración pregunta 22 "Otra"
encuesta_momento2 %>%
  filter(q21_trabajo_primer_anio == "si") %>%
  filter(q22_ocupacion_primer_anio == "otro") %>%
  filter(!is.na(q22_otra_ocupacion_primer_anio)) %>%
  select(q22_otra_ocupacion_primer_anio) %>%
  print(n = Inf)
# Se observan 31 casos con distintas actividades, varias describen informalidad y quizás 
#podrían ser recodificadas en "changas/subempleo".(A verificar metodológicamente)

# Q23 - Formalidad del trabajo en el primer año
# Responden solo quienes tenían salario fijo o cuenta propia en Q22
encuesta_momento2 %>%
  filter(q21_trabajo_primer_anio == "si") %>%
  filter(q22_ocupacion_primer_anio %in% c("salario_fijo", "cuenta_propia")) %>%
  filter(!is.na(q23_formalidad_trabajo)) %>%
  group_by(q23_formalidad_trabajo) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q23_formalidad_trabajo = recode(q23_formalidad_trabajo,
                                    "formal"   = "Formal",
                                    "informal" = "Informal"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Formalidad del trabajo en primer año` = q23_formalidad_trabajo, `%` = porcentaje)


# Q24 - Actividad laboral en el primer año
# Solo quienes trabajaban por cuenta propia o changas en Q22
encuesta_momento2 %>%
  filter(q21_trabajo_primer_anio == "si") %>%
  filter(q22_ocupacion_primer_anio %in% c("cuenta_propia", "changas")) %>%
  filter(!is.na(q24_actividad_primer_anio)) %>%
  group_by(q24_actividad_primer_anio) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q24_actividad_primer_anio = recode(q24_actividad_primer_anio,
                                       "patron"             = "Patrón de un negocio (empleador)",
                                       "profesional"        = "Profesional independiente",
                                       "servicio_oficio"    = "Trabajo de servicio u oficio",
                                       "emprendimiento"     = "Emprendimiento",
                                       "venta_ambulante"    = "Venta ambulante",
                                       "plataforma"        = "Trabajo en plataformas",
                                       "trabajo_estacional" = "Trabajo estacional o por períodos",
                                       "otro"               = "Otra"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Actividad laboral primer año` = q24_actividad_primer_anio, `%` = porcentaje)

# Exploración de respuestas en la Q24 "Otra"
encuesta_momento2 %>%
  filter(q21_trabajo_primer_anio == "si") %>%
  filter(q22_ocupacion_primer_anio %in% c("cuenta_propia", "changas")) %>%
  filter(q24_actividad_primer_anio == "otro") %>%
  filter(!is.na(q24_otra_actividad_primer_anio)) %>%
  select(q24_otra_actividad_primer_anio) %>%
  print(n = Inf)
# Hay 39 registros. Varios inferibles, ej: trabajos de cuidados recodificables en: servicio_oficio   
# Tomar decisiones de recodificación 

# Q25 - Ocupación laboral primer año (texto libre) — nube de palabras
stopwords_es <- c(
  "de", "en", "la", "el", "los", "las", "un", "una", "y", "a", "por",
  "con", "del", "al", "se", "su", "que", "o", "para", "pero", "como",
  "más", "este", "esta", "esto", "entre", "hasta", "desde",
  "trabajo", "trabajaba", "trabajé", "sector", "área", "empresa"
)

palabras_q25 <- encuesta_momento2 %>%
  filter(q21_trabajo_primer_anio == "si") %>%
  filter(!is.na(q25_ocupacion_primer_anio)) %>%
  select(q25_ocupacion_primer_anio) %>%
  unnest_tokens(word, q25_ocupacion_primer_anio) %>%
  filter(!word %in% stopwords_es) %>%
  filter(nchar(word) > 3) %>%
  count(word, sort = TRUE)

wordcloud2(palabras_q25 %>% slice_max(n, n = 50),
           color = "random-dark",
           backgroundColor = "white",
           size = 0.8)

# Q26 - Cómo obtuvo el trabajo en el primer año
encuesta_momento2 %>%
  filter(q21_trabajo_primer_anio == "si") %>%
  filter(!is.na(q26_forma_obtencion)) %>%
  group_by(q26_forma_obtencion) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q26_forma_obtencion = recode(q26_forma_obtencion,
                                 "vino_con_trabajo"         = "Vine con trabajo desde mi país de origen",
                                 "anuncios_redes"     = "A través de anuncios, plataformas y redes",
                                 "dejo_cv"                  = "Dejé mi curriculum y me llamaron",
                                 "recomendación_familiar"   = "Por recomendación de conocido, compatriota o familiar",
                                 "agencia_empleo"           = "A través de una oficina o agencia de empleo",
                                 "otro"                     = "Otra"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Cómo obtuvo el trabajo` = q26_forma_obtencion, `%` = porcentaje)
# Exploración "Otro" - 37 Resultados. Algunos pueden ser recodificados.
encuesta_momento2 %>%
  filter(q26_forma_obtencion == "otro") %>%
  filter(!is.na(Especifique_otra_for_ue_obtuvo_su_trabajo)) %>%
  select(Especifique_otra_for_ue_obtuvo_su_trabajo) %>%
  print(n = Inf)

# Q27 - Envío de dinero durante el primer año
# Toda la muestra del Momento 2 (no filtra por Q21)
encuesta_momento2 %>% count(is.na(q27_remesas)) # 217 NA / 820 Valores
encuesta_momento2 %>%
  filter(!is.na(q27_remesas)) %>%
  group_by(q27_remesas) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q27_remesas = recode(q27_remesas,
                         "regularmente"          = "Sí, regularmente",
                         "a_veces"               = "Sí, de vez en cuando",
                         "no"                    = "No",
                         "prefiero_no_responder" = "Prefiero no responder"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Envío de remesas primer año` = q27_remesas, `%` = porcentaje)

# Q28 - Dificultades para acceder a trabajo en el primer año
# Aplica a quienes trabajaron, estuvieron desempleados o trabajaron por cuenta propia
# (Q21 = "si", "no_desempleado", "no_estudio" — excluye jubilados y niños)
encuesta_momento2 %>%
  filter(q21_trabajo_primer_anio %in% c("si", "no_desempleado", "no_estudio")) %>%
  filter(!is.na(q28_dificultades_acceso)) %>%
  group_by(q28_dificultades_acceso) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q28_dificultades_acceso = recode(q28_dificultades_acceso,
                                     "si"                    = "Sí",
                                     "no"                    = "No",
                                     "prefiero_no_responder" = "Prefiero no responder"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Dificultades para acceder a trabajo` = q28_dificultades_acceso, `%` = porcentaje)


# Q29 - Principal dificultad para acceder a trabajo en el primer año
# Solo quienes respondieron Sí en Q28
encuesta_momento2 %>%
  filter(q28_dificultades_acceso == "si") %>%
  filter(!is.na(q29_tipo_dificultades_primer_a)) %>%
  group_by(q29_tipo_dificultades_primer_a) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q29_tipo_dificultades_primer_a = recode(q29_tipo_dificultades_primer_a,
                                            "homologacion_titulos"     = "Dificultades con la homologación/reconocimiento de títulos",
                                            "discriminacion"           = "Discriminación por condición de extranjero/a",
                                            "documentacion"      = "Falta de documentación (DNI u otros)",
                                            "idioma"                   = "Dificultades con el idioma",
                                            "experiencia"          = "Falta de experiencia previa",
                                            "poca_oferta"              = "Poca oferta en el rubro buscado",
                                            "desinformacion"          = "No sabía dónde buscar trabajo",
                                            "prefiero_no_responder"    = "Prefiero no responder",
                                            "otra"                     = "Otra"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Principal dificultad de acceso` = q29_tipo_dificultades_primer_a, `%` = porcentaje)

# Exploración "Otra"
# 40 resultados, destaca fuertemente la edad como razón.
encuesta_momento2 %>%
  filter(q29_tipo_dificultades_primer_a == "otra") %>%
  filter(!is.na(q29_otras_dificultades_primer_)) %>%
  select(q29_otras_dificultades_primer_) %>%
  print(n = Inf)
# Tabla de frecuencias de las razones aducidas. 19 nombran la Edad. 
palabras_q29 %>%
  slice_max(n, n = 15) %>%
  select(Término = word, Frecuencia = n)

# Q30 - Adecuación de la actividad laboral al área de conocimiento
# Solo quienes trabajaron en el primer año (Q21 = "si")
encuesta_momento2 %>%
  filter(q21_trabajo_primer_anio == "si") %>%
  filter(!is.na(q30_actividad_adecuada)) %>%
  group_by(q30_actividad_adecuada) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q30_actividad_adecuada = recode(q30_actividad_adecuada,
                                    "si"                    = "Sí",
                                    "no"                    = "No",
                                    "parcialmente"          = "Parcialmente",
                                    "prefiero_no_responder" = "Prefiero no responder"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Actividad adecuada a conocimientos` = q30_actividad_adecuada, `%` = porcentaje)

# Q31 - Cobertura de necesidades básicas durante el primer año
# Solo quienes trabajaron en el primer año (Q21 = "si")
encuesta_momento2 %>%
  filter(q21_trabajo_primer_anio == "si") %>%
  filter(!is.na(q31_necesidades_basicas)) %>%
  group_by(q31_necesidades_basicas) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q31_necesidades_basicas = recode(q31_necesidades_basicas,
                                     "completamente"  = "Completamente",
                                     "bastante_bien"  = "Bastante bien",
                                     "justo"          = "Justo",
                                     "insuficiente"   = "Insuficiente",
                                     "muy_deficiente" = "Muy deficiente"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Cobertura de necesidades básicas primer año` = q31_necesidades_basicas, `%` = porcentaje)

# Q32 - Pregunta si se recibieron subsidios o programas sociales durante el primer año
# Responde toda la muestra del Momento 2
encuesta_momento2 %>%
  filter(!is.na(q32_subsidios)) %>%
  group_by(q32_subsidios) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q32_subsidios = recode(q32_subsidios,
                           "si"                    = "Sí",
                           "no"                    = "No",
                           "prefiero_no_responder" = "Prefiero no responder / No sé"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Recibió ayuda o programa social` = q32_subsidios, `%` = porcentaje)

##### Momento 3
# Responde toda la muestra
encuesta_clean %>% count(is.na(q33_trabajo_actualidad))
# Verificamos que respondieron los 1104 entrevistados. 

# Q33 - Situación laboral actual
encuesta_clean %>%
  filter(!is.na(q33_trabajo_actualidad)) %>%
  group_by(q33_trabajo_actualidad) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q33_trabajo_actualidad = recode(q33_trabajo_actualidad,
                                    "si"             = "Sí, trabaja",
                                    "no_jubilado"    = "No, jubilado/retirado",
                                    "no_estudio"     = "No, solo estudia",
                                    "no_desempleado" = "No, desempleado/buscando trabajo"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Situación laboral actual` = q33_trabajo_actualidad, `%` = porcentaje)

# Q34 - Cantidad de trabajos actuales
# Solo quienes trabajan actualmente
encuesta_clean %>%
  filter(q33_trabajo_actualidad == "si") %>%
  filter(!is.na(q34_trabajos_cantidad)) %>%
  group_by(q34_trabajos_cantidad) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  arrange(q34_trabajos_cantidad) %>%
  select(`Cantidad de trabajos` = q34_trabajos_cantidad, `%` = porcentaje)
# Tratamiento de outliers
encuesta_clean %>%
  filter(q33_trabajo_actualidad == "si") %>%
  filter(!is.na(q34_trabajos_cantidad)) %>%
  filter(q34_trabajos_cantidad >= 5) %>%
  select(q34_trabajos_cantidad, q35_ocupacion_actual_ingreso, edad, nac) %>%
  arrange(desc(q34_trabajos_cantidad))

encuesta_clean <- encuesta_clean %>%
  mutate(q34_trabajos_cantidad = case_when(
    # Valores >= 5 parecen errores de carga (posiblemente confunden con edad o años de antigüedad).
    # Se convierten a NA. Decisión de imputación pendiente de validación con el equipo.
    q34_trabajos_cantidad >= 5 | q34_trabajos_cantidad < 1 ~ NA_real_,
    TRUE                       ~ q34_trabajos_cantidad
  ))

# Distribución corregida
encuesta_clean %>%
  filter(q33_trabajo_actualidad == "si") %>%
  filter(!is.na(q34_trabajos_cantidad)) %>%
  group_by(q34_trabajos_cantidad) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  arrange(q34_trabajos_cantidad) %>%
  select(`Cantidad de trabajos` = q34_trabajos_cantidad, `%` = porcentaje)

# Q35 - Principal fuente de ingresos actual
# Solo quienes trabajan actualmente (Q33 = "si")
encuesta_clean %>%
  filter(q33_trabajo_actualidad == "si") %>%
  filter(!is.na(q35_ocupacion_actual_ingreso)) %>%
  group_by(q35_ocupacion_actual_ingreso) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q35_ocupacion_actual_ingreso = recode(q35_ocupacion_actual_ingreso,
                                          "salario_fijo"             = "Trabajo con salario fijo",
                                          "cuenta_propia"            = "Trabajo por cuenta propia",
                                          "changas"                  = "Trabajos esporádicos / changas / subempleo",
                                          "trabajo_sin_remuneracion" = "Trabajo sin remuneración",
                                          "otro"                     = "Otra"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Situación ocupacional actual` = q35_ocupacion_actual_ingreso, `%` = porcentaje)

#Exploramos los "Otra".
encuesta_clean %>%
  filter(!is.na(Especifique_otra_fuente_de_ingresos)) %>%
  select(Especifique_otra_fuente_de_ingresos) %>%
  print(n = Inf)
# Se repite una confusión común, varios entrevistados entienden que "Salario Fijo" 
# no incliuye al trabajo informal ("en negro")



# Q36 - Formalidad del trabajo actual
# Solo quienes tienen salario fijo o cuenta propia (Q35)
encuesta_clean %>%
  filter(q35_ocupacion_actual_ingreso %in% c("salario_fijo", "cuenta_propia")) %>%
  filter(!is.na(q36_formalidad_trabajo)) %>%
  group_by(q36_formalidad_trabajo) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q36_formalidad_trabajo = recode(q36_formalidad_trabajo,
                                    "formal"   = "Formal",
                                    "informal" = "Informal"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Formalidad del trabajo actual` = q36_formalidad_trabajo, `%` = porcentaje)

# Q37 - Actividad laboral actual
# Solo quienes trabajan por cuenta propia o changas (Q35)
encuesta_clean %>%
  filter(q35_ocupacion_actual_ingreso %in% c("cuenta_propia", "changas")) %>%
  filter(!is.na(q37_trabajo_actual)) %>%
  group_by(q37_trabajo_actual) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q37_trabajo_actual = recode(q37_trabajo_actual,
                                "patron"             = "Patrón de un negocio (empleador)",
                                "profesional"        = "Profesional independiente",
                                "servicio_oficio"    = "Trabajo de servicio u oficio",
                                "emprendimiento"     = "Emprendimiento",
                                "venta_ambulante"    = "Venta ambulante",
                                "plataforma"         = "Trabajo en plataformas",
                                "trabajo_temporal" = "Trabajo estacional o por períodos",
                                "otro"               = "Otra"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Actividad laboral actual` = q37_trabajo_actual, `%` = porcentaje)

# Exploración "Otra"
encuesta_clean %>%
  filter(q37_trabajo_actual == "otro") %>%
  filter(!is.na(q37_otra_actividad_actualidad)) %>%
  select(q37_otra_actividad_actualidad) %>%
  print(n = Inf)
# 38 respuestas en "Otra", algunas opciones de recodificación a verificar con el equipo.



# Q38 - Ocupación laboral actual (texto libre) — nube de palabras
palabras_q38 <- encuesta_clean %>%
  filter(!is.na(q38_ocupacion_actual)) %>%
  select(q38_ocupacion_actual) %>%
  unnest_tokens(word, q38_ocupacion_actual) %>%
  filter(!word %in% stopwords_es) %>%
  filter(nchar(word) > 3) %>%
  count(word, sort = TRUE)

wordcloud2(palabras_q38 %>% slice_max(n, n = 60),
           color = "random-dark",
           backgroundColor = "white",
           size = 0.8)


# Q39 - Cómo obtuvo el trabajo actual
# Solo quienes trabajan actualmente (Q33 = "si")
encuesta_clean %>%
  filter(!is.na(q39_obtencion_trabajo)) %>%
  group_by(q39_obtencion_trabajo) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q39_obtencion_trabajo = recode(q39_obtencion_trabajo,
                                          "vino_con_trabajo"       = "Vine con trabajo desde mi país de origen",
                                          "anuncios_redes"   = "A través de anuncios, plataformas y redes",
                                          "dejo_cv"                = "Dejé mi curriculum y me llamaron",
                                          "recomendación_familiar" = "Por recomendación de conocido, compatriota o familiar",
                                          "agencia_empleo"         = "A través de una oficina o agencia de empleo",
                                          "otro"                   = "Otra"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Cómo obtuvo el trabajo actual` = q39_obtencion_trabajo, `%` = porcentaje)

# Exploración variable "Otra"
palabras_q39 <- encuesta_clean %>%
  filter(q39_obtencion_trabajo == "otro") %>%
  filter(!is.na(q39_otra_obtencion_trabajo)) %>%
  select(q39_otra_obtencion_trabajo) %>%
  unnest_tokens(word, q39_otra_obtencion_trabajo) %>%
  filter(!word %in% stopwords_es) %>%
  filter(nchar(word) > 3) %>%
  count(word, sort = TRUE)

# Tabla
palabras_q39
# Destacan emprendimientos, cuenta propia, 

# Q40 - Estudios realizados desde que llegó a Argentina (selección múltiple), la suma supera el 100%
total_pesos <- sum(encuesta_clean$weightvec)

tibble(
  Formación = c(
    "Estudios de nivel primario",
    "Estudios de nivel secundario",
    "Estudios de nivel terciario",
    "Estudios universitarios (grado)",
    "Estudios de posgrado",
    "Capacitaciones laborales / cursos de idioma",
    "No estudió desde que llegó"
  ),
  `%` = c(
    round(sum(encuesta_clean$`q40_estudios/si_primaria`        * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1),
    round(sum(encuesta_clean$`q40_estudios/si_secundaria`      * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1),
    round(sum(encuesta_clean$`q40_estudios/si_terciario`       * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1),
    round(sum(encuesta_clean$`q40_estudios/si_universidad`   * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1),
    round(sum(encuesta_clean$`q40_estudios/si_postgrado`        * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1),
    round(sum(encuesta_clean$`q40_estudios/si_capacitaciones`  * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1),
    round(sum(encuesta_clean$`q40_estudios/no`      * encuesta_clean$weightvec, na.rm = TRUE) / total_pesos * 100, 1)
  )
) %>%
  arrange(desc(`%`))

# Q41 - Envío de remesas actualmente
encuesta_clean %>%
  filter(!is.na(q41_remesas_actualidad)) %>%
  group_by(q41_remesas_actualidad) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q41_remesas_actualidad = recode(q41_remesas_actualidad,
                                "regularmente"          = "Sí, regularmente",
                                "a_veces"               = "Sí, de vez en cuando",
                                "no"                    = "No",
                                "prefiero_no_responder" = "Prefiero no responder"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Envío de remesas actual` = q41_remesas_actualidad, `%` = porcentaje)

# Q42 - Medios de envío de remesas
# Solo quienes envían remesas actualmente (Q41 = "regularmente" o "a_veces")
encuesta_clean %>%
  filter(q41_remesas_actualidad %in% c("regularmente", "a_veces")) %>%
  filter(!is.na(q42_envio_remesas)) %>%
  group_by(q42_envio_remesas) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q42_envio_remesas = recode(q42_envio_remesas,
                                "transferencia_bancaria" = "Transferencia bancaria",
                                "envio_empresa"         = "Empresas de envío de dinero (Western Union, MoneyGram, etc.)",
                                "crypto"                = "Criptomonedas",
                                "familiares_amigos"      = "A través de familiares o amigos",
                                "otra"                   = "Otra"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Medio de envío de remesas` = q42_envio_remesas, `%` = porcentaje)

# Exploración "Otra"
encuesta_clean %>%
  filter(q42_envio_remesas == "otra") %>%
  filter(!is.na(q42_otra_modalidad_envio_diner)) %>%
  select(q42_otra_modalidad_envio_diner) %>%
  print(n = Inf)
#Solo 15 resultados, no tiene mucho sentido recodificar.


# Q43 - Dificultades para acceder al trabajo en los últimos dos años
encuesta_clean %>%
  filter(!is.na(q43_dificultad_trabajo)) %>%
  group_by(q43_dificultad_trabajo) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q43_dificultad_trabajo = recode(q43_dificultad_trabajo,
                                            "si"                    = "Sí",
                                            "no"                    = "No",
                                            "prefiero_no_responder" = "Prefiero no responder"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Dificultades para acceder al trabajo` = q43_dificultad_trabajo, `%` = porcentaje)

# Q44 - Principal dificultad para acceder al trabajo (selección simple)
# Solo quienes tuvieron dificultades (Q43 = "si")
encuesta_clean %>%
  filter(q43_dificultad_trabajo == "si") %>%
  filter(!is.na(q44_tipo_dificultades_trabajo_)) %>%
  group_by(q44_tipo_dificultades_trabajo_) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q44_tipo_dificultades_trabajo_ = recode(q44_tipo_dificultades_trabajo_,
                                            "dificultades_y_demoras_con_el_reconocimi"  = "Homologación/reconocimiento de títulos",
                                            "discriminaci_n_por_mi_condici_n_de_extra"        = "Discriminación por condición de extranjero/a",
                                            "falta_de_documentaci_n__dni_u_otros_docu"   = "Falta de documentación (DNI u otros)",
                                            "dificultades_con_el_idioma"                = "Dificultades con el idioma",
                                            "falta_de_experiencia_previa"       = "Falta de experiencia previa",
                                            "hay_poca_oferta_de_trabajo_en_el_rubro_l"           = "Poca oferta en el rubro buscado",
                                            "no_sab_a_d_nde_buscar_trabajo"       = "No sabía dónde buscar trabajo",
                                            "prefiero_no_responder" = "Prefiero no responder",
                                            "otras"                  = "Otras"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Principal dificultad de acceso` = q44_tipo_dificultades_trabajo_, `%` = porcentaje)

# Otras en este caso no es procesable, no se creó un campo para desarrollar.

# Q45 - Adecuación de la actividad laboral actual al área de conocimiento
# Solo quienes trabajan actualmente (Q33 = "si")
encuesta_clean %>%
  filter(!is.na(q45_trabajo_experiencia)) %>%
  group_by(q45_trabajo_experiencia) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q45_trabajo_experiencia = recode(q45_trabajo_experiencia,
                                     "si"                    = "Sí",
                                     "no"                    = "No",
                                     "parcialmente"          = "Parcialmente",
                                     "prefiero_no_responder" = "Prefiero no responder"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Actividad adecuada a conocimientos` = q45_trabajo_experiencia, `%` = porcentaje)

# Q46 - Ayuda o programa social estatal actual
encuesta_clean %>%
  filter(!is.na(q46_ayuda_estatal_inclusion)) %>%
  group_by(q46_ayuda_estatal_inclusion) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q46_ayuda_estatal_inclusion = recode(q46_ayuda_estatal_inclusion,
                                         "si"                    = "Sí",
                                         "no"                    = "No",
                                         "prefiero_no_responder" = "Prefiero no responder / No sé"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Recibe ayuda o programa social` = q46_ayuda_estatal_inclusion, `%` = porcentaje)

# Exploración quienes respondieron Sí — ¿qué programas mencionan?
encuesta_clean %>%
  filter(q46_ayuda_estatal_inclusion == "si") %>%
  filter(!is.na(q46_nombre_programa_estatal)) %>%
  select(q46_nombre_programa_estatal) %>%
  print(n = Inf)
# Destacan AUH , Volver al Trabajo / Ex Potenciar Trabajo y  Jubilación - 
# ¿Posible nube de palabras?

# Q47 - Cobertura de necesidades básicas con trabajo actual
# Solo quienes trabajan actualmente (Q33 = "si")
encuesta_clean %>%
  filter(!is.na(q47_necesidades_basicas)) %>%
  group_by(q47_necesidades_basicas) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q47_necesidades_basicas = recode(q47_necesidades_basicas,
                                     "completamente"  = "Completamente",
                                     "bastante_bien"  = "Bastante bien",
                                     "justo"          = "Justo",
                                     "insuficiente"   = "Insuficiente",
                                     "muy_deficiente" = "Muy deficiente"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Cobertura de necesidades básicas actual` = q47_necesidades_basicas, `%` = porcentaje)

# Q48 - Mejora de situación laboral respecto al país de origen
encuesta_clean %>%
  filter(!is.na(q48_mejora_origen)) %>%
  group_by(q48_mejora_origen) %>%
  summarise(n = sum(weightvec)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    q48_mejora_origen = recode(q48_mejora_origen,
                               "si"                    = "Sí",
                               "no"                    = "No",
                               "parcialmente"          = "Parcialmente",
                               "no_aplica"             = "No aplica / No sé",
                               "prefiero_no_responder" = "Prefiero no responder"
    )
  ) %>%
  arrange(desc(porcentaje)) %>%
  select(`Mejora respecto al país de origen` = q48_mejora_origen, `%` = porcentaje)

# Q49 - Comentarios libres — nube de palabras
encuesta_clean %>%
  filter(!is.na(q49_comentarios)) %>%
  select(q49_comentarios) %>%
  print(n = Inf)
