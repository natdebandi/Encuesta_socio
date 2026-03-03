library(tidyverse)
library(readr)

encuesta <- read_csv(
  "data/Trayectorias laborales vf kobo weightvec v3.csv",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# Correcciones mínimas necesarias
encuesta_clean <- encuesta %>%
  select(-`q40_estudios/s__estudios_de_nivel_primario`,
         -`q40_estudios/s__estudios_de_nivel_secundario`,
         -`q40_estudios/s__estudios_de_nivel_terciario__institut`,
         -`q40_estudios/s__estudios_universitarios__carrera_de_g`,
         -`q40_estudios/s__estudios_de_postgrado`,
         -`q40_estudios/s__capacitaciones_laborales_profesionale`,
         -`q40_estudios/no__no_estudi__desde_que_llegu__a_argent`,
         -`...1`, -`__version__`, -`_tags`, -`_index`,
         -start, -end) %>%
  mutate(q19_anio_arribo = case_when(
    q19_anio_arribo == 21            ~ 2004,
    q19_anio_arribo %in% c(18, 1918) ~ NA_real_,
    TRUE                             ~ q19_anio_arribo
  ))

total_pesos <- sum(encuesta_clean$weightvec)

# Formalidad ponderada en cada momento
# Solo casos que aplican al filtro de cada momento
formalidad_momentos <- tibble(
  Momento = c("País de origen", "Primer año en Argentina", "Actualidad"),
  Formal = c(
    # Momento 1: salario fijo o cuenta propia en origen
    encuesta_clean %>%
      filter(q14_situacion_ocupacional_orig %in% c("salario_fijo", "cuenta_propia"),
             !is.na(q15_formalidad_trabajo)) %>%
      summarise(p = sum(weightvec[q15_formalidad_trabajo == "formal"]) /
                  sum(weightvec) * 100) %>% pull(p),
    
    # Momento 2: primer año
    encuesta_clean %>%
      filter(q19_anio_arribo < 2024,
             q22_ocupacion_primer_anio %in% c("salario_fijo", "cuenta_propia"),
             !is.na(q23_formalidad_trabajo)) %>%
      summarise(p = sum(weightvec[q23_formalidad_trabajo == "formal"]) /
                  sum(weightvec) * 100) %>% pull(p),
    
    # Momento 3: actualidad
    encuesta_clean %>%
      filter(q35_ocupacion_actual_ingreso %in% c("salario_fijo", "cuenta_propia"),
             !is.na(q36_formalidad_trabajo)) %>%
      summarise(p = sum(weightvec[q36_formalidad_trabajo == "formal"]) /
                  sum(weightvec) * 100) %>% pull(p)
  )
) %>%
  mutate(
    Informal = 100 - Formal,
    Formal   = round(Formal, 1),
    Informal = round(Informal, 1),
    Momento  = factor(Momento, levels = c("País de origen",
                                          "Primer año en Argentina",
                                          "Actualidad"))
  )

# Gráfico
formalidad_momentos %>%
  pivot_longer(cols = c(Formal, Informal),
               names_to = "Condición",
               values_to = "Porcentaje") %>%
  ggplot(aes(x = Momento, y = Porcentaje, fill = Condición)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Formal" = "#2C7BB6", "Informal" = "#D7191C")) +
  labs(
    title = "Evolución de la formalidad laboral a lo largo de la trayectoria migratoria",
    subtitle = "Sobre quienes tenían salario fijo o trabajo por cuenta propia en cada momento",
    x = NULL, y = "Porcentaje (%)",
    fill = NULL,
    caption = "Datos ponderados. Encuesta trayectorias sociolaborales AMBA."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")