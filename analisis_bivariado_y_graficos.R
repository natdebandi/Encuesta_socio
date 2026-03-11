install.packages("treemapify")
library(treemapify)
library(ggplot2)
library(paletteer)
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



# Preparación de datos
treemap_data <- encuesta_clean %>%
  filter(!is.na(nac)) %>%
  group_by(nac) %>%
  summarise(
    n_sin_ponderar = n(),
    n_ponderado    = sum(weightvec)
  ) %>%
  mutate(
    pct_sin_ponderar = round(n_sin_ponderar / sum(n_sin_ponderar) * 100, 1),
    pct_ponderado    = round(n_ponderado    / sum(n_ponderado)    * 100, 1)
  )

# Paleta
colores <- paletteer_c("ggthemes::Orange-Gold", nrow(treemap_data))

# TREEMAP SIN PONDERAR
ggplot(treemap_data,
       aes(area = n_sin_ponderar, fill = pct_sin_ponderar,
           label = paste0(nac, "\n", pct_sin_ponderar, "%"))) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(color = "white", fontface = "bold",
                    place = "centre", reflow = TRUE) +
  scale_fill_gradientn(colors = colores) +
  labs(title = "Distribución por nacionalidad — Sin ponderar",
       fill = "%") +
  theme_minimal()

# TREEMAP PONDERADO
ggplot(treemap_data,
       aes(area = n_ponderado, fill = pct_ponderado,
           label = paste0(nac, "\n", pct_ponderado, "%"))) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(color = "white", fontface = "bold",
                    place = "centre", reflow = TRUE) +
  scale_fill_gradientn(colors = colores) +
  labs(title = "Distribución por nacionalidad — ponderado (Censo 2022)",
       fill = "%") +
  theme_minimal()


encuesta_clean %>%
  filter(!is.na(nac)) %>%
  ggplot(aes(x = reorder(nac, weightvec, FUN = median), 
             y = weightvec, 
             fill = reorder(nac, weightvec, FUN = median))) +
  geom_boxplot(show.legend = FALSE, alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = paletteer_c("ggthemes::Orange-Gold", 
                                         length(unique(encuesta_clean$nac[!is.na(encuesta_clean$nac)])))) +
  coord_flip() +
  labs(
    title = "Distribución de pesos muestrales por nacionalidad",
    subtitle = "Línea punteada = peso = 1 (sin corrección)",
    x = NULL,
    y = "Peso muestral (weightvec)",
    caption = "Pesos calibrados según Censo 2022"
  ) +
  theme_minimal(base_size = 12)

encuesta_clean %>% filter(weightvec> 5) %>% select(nacionalidad, weightvec, sexo, edad_agrupada, zona) %>%
  arrange(desc(weightvec))


### Distribución de Grupos etarios por Nacionalidad
encuesta_clean %>%
  filter(!is.na(nac), !is.na(edad_agrupada)) %>%
  group_by(nac, edad_agrupada) %>%
  summarise(n = sum(weightvec), .groups = "drop") %>%
  group_by(nac) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  mutate(edad_agrupada = factor(edad_agrupada,
                                levels = rev(c("18-24", "25-35", "36-55", "56-64", "65-100")))) %>%
  ggplot(aes(x = reorder(nac, porcentaje),
             y = porcentaje,
             fill = edad_agrupada)) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = rev(paletteer_c("ggthemes::Orange-Gold", 5)),
    guide  = guide_legend(reverse = TRUE)
  ) +
  coord_flip() +
  labs(
    title   = "Distribución de grupos etarios por nacionalidad",
    x       = NULL,
    y       = "%",
    fill    = "Grupo etario",
    caption = "Datos ponderados. Encuesta trayectorias sociolaborales AMBA.\nNota: Venezuela y Uruguay no presentan casos en el \nrango de 18 a 25 años."
  ) +
  theme_minimal(base_size = 12)


### Distribución de zonas por nacionalidad
encuesta_clean %>%
  filter(!is.na(nac), !is.na(zona)) %>%
  group_by(nac, zona) %>%
  summarise(n = sum(weightvec), .groups = "drop") %>%
  group_by(nac) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = zona, values_from = porcentaje) %>%
  select(Nacionalidad = nac, CABA = zona_caba, Norte = zona_norte, Oeste = zona_oeste, Sur = zona_sur)

encuesta_clean %>%
  filter(!is.na(nac), !is.na(zona)) %>%
  group_by(nac, zona) %>%
  summarise(n = sum(weightvec), .groups = "drop") %>%
  group_by(nac) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  mutate(zona = factor(zona,
                                     levels = c("zona_caba", "zona_norte", "zona_oeste", "zona_sur"))) %>%
  ggplot(aes(x = reorder(nac, porcentaje),
             y = porcentaje,
             fill = zona)) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = c(
      "zona_caba"  = "#F5C242",
      "zona_norte" = "#E8873A",
      "zona_oeste" = "#C94F1A",
      "zona_sur"   = "#7B1F0A"
    ),
    labels = c("CABA", "Zona Norte", "Zona Oeste", "Zona Sur"),
    guide  = guide_legend(reverse = TRUE)
  ) +
  coord_flip() +
  labs(
    title   = "Distribución territorial por nacionalidad",
    x       = NULL,
    y       = "%",
    fill    = "Zona",
    caption = "Datos ponderados. Encuesta trayectorias sociolaborales AMBA."
  ) +
  theme_minimal(base_size = 12)
