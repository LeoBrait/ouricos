library(tidyverse)
library(vegan)
library(cowplot)



df <- read.csv("ouriquinho.csv")


df_long <- df %>%
  pivot_longer(
    cols = c("satelite", "ourico", "ofiuridio", "pepino"),
    names_to = "Especie",
    values_to = "Abundancia"
  ) %>%
  mutate(
    estrato = fct_relevel(
      estrato,
      c("praia_superior", "meio", "oceano_inferior")
    ),
    Especie = fct_relevel(
      Especie,
      c("ourico", "ofiuridio", "pepino", "satelite" )
    ),
    eixo = interaction(estrato, ponto, replica, sep = "_"),
    ponto = fct_relevel(
      ponto,
      c("direita", "esquerda")
    )
  ) %>%
  mutate(
    eixo = factor(eixo, levels = unique(eixo)
    )
  ) %>% 
  mutate(
    estrato = fct_recode(
      estrato,
      "Superior" = "praia_superior",
      "Intermediário" = "meio",
      "Inferior" = "oceano_inferior",
    ),
    ponto = fct_recode(
      ponto,
      "Leste" = "direita",
      "Oeste" = "esquerda"
    ),
    Especie = fct_recode(
      Especie,
      "Eucidaris tribuloides" = "satelite",
      "Echinometra lucunter" = "ourico",
      "Desc. em Ophiuroidea" = "ofiuridio",
      "Holothuria grisea" = "pepino"
    ),
    label = interaction(ponto, replica, sep = " ")
  )

df_riqueza <- df_long %>%
  group_by(estrato, ponto, eixo) %>%
  summarise(Riqueza = sum(Abundancia > 0))

df_diversidade <- df_long %>%
  group_by(estrato, ponto, eixo) %>%
  summarise(
    Shannon = diversity(Abundancia, index = "shannon")
  ) %>%
  mutate( Simpson = 1 - Simpson)

df_pielou <- df_diversidade %>%
  left_join(df_riqueza, by = c("estrato", "ponto", "eixo")) %>%
  mutate(
    Pielou = Shannon / log(Riqueza)
  ) %>%
  filter(is.finite(Pielou))

df_abundancia <- df_long %>%
  group_by(estrato, ponto, replica) %>%
  summarise(Abundancia = sum(Abundancia))

# comprar riqueza entre estratos com anova
anova_riqueza <- aov(Riqueza ~ estrato, data = df_riqueza)
summary(anova_riqueza)
TukeyHSD(anova_riqueza)

anova_diversidade_shannon <- aov(Shannon ~ estrato, data = df_diversidade)
summary(anova_diversidade_shannon)
TukeyHSD(anova_diversidade_shannon)

anova_evenness_pielou <- aov(Pielou ~ estrato, data = df_pielou)
summary(anova_evenness_pielou)
TukeyHSD(anova_evenness_pielou)

anova_abundancia <- aov(Abundancia ~ estrato, data = df_abundancia)
summary(anova_abundancia)
TukeyHSD(anova_abundancia)




# boxplot de riqueza por estrato
plot_riqueza <- 
  ggplot(
    df_riqueza, 
    aes(
      x = estrato, 
      y = Riqueza, 
      fill = estrato
    )
  ) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 2) +
  ylab("Riqueza de espécies") +
  xlab("Estrato") +
  theme_classic() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  annotate(
    "text", 
    x = 2, 
    y = max(df_riqueza$Riqueza) + 0.5, 
    label = paste0("ANOVA: p = ", 
                   round(summary(anova_riqueza)[[1]][["Pr(>F)"]][1], 4)),
    size = 6
  )

plot_diversidade_shannon <- 
  ggplot(
    df_diversidade, 
    aes(
      x = estrato, 
      y = Shannon, 
      fill = estrato
    )
  ) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 2) +
  ylab("Índice de Shannon") +
  xlab("Estrato") +
  theme_classic() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  annotate(
    "text", 
    x = 2, 
    y = max(df_diversidade$Shannon) + 0.5, 
    label = paste0("ANOVA: p = ", 
                   round(summary(anova_diversidade_shannon)[[1]][["Pr(>F)"]][1], 4)),
    size = 6
  )

plot_abundancia <- 
  ggplot(
    df_abundancia, 
    aes(
      x = estrato, 
      y = Abundancia, 
      fill = estrato
    )
  ) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 2) +
  ylab("Abundância total") +
  xlab("Estrato") +
  theme_classic() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  annotate(
    "text", 
    x = 2, 
    y = max(df_abundancia$Abundancia) + 5, 
    label = paste0("ANOVA: p = ", 
                   round(summary(anova_abundancia)[[1]][["Pr(>F)"]][1], 4)),
    size = 6
  )

plot_evenness_pielou <-
    ggplot(
        df_pielou, 
        aes(
        x = estrato, 
        y = Pielou, 
        fill = estrato
        )
    ) +
    geom_boxplot() +
    geom_jitter(width = 0.2, size = 2) +
    ylab("Equitabilidade de Pielou") +
    xlab("Estrato") +
    theme_classic() +
    theme(
        text = element_text(size = 16),
        axis.title = element_text(face = "bold"),
        legend.position = "none"
    ) +
    annotate(
        "text", 
        x = 2, 
        y = max(df_pielou$Pielou) + 0.05, 
        label = paste0("ANOVA: p = ", 
                     round(summary(anova_evenness_pielou)[[1]][["Pr(>F)"]][1], 4)),
        size = 6
    )

painel <- plot_grid(
  plot_riqueza,
  plot_abundancia,
  plot_diversidade_shannon,
  plot_evenness_pielou,
  ncol = 2,
  labels = c("A", "B", "C", "D"),
  align = "v"
)

ggsave( 
  "figuras/boxplot_riqueza_estratos.png", 
  painel, 
  width = 10, 
  height = 10
)
