# Adriano Sanches Melo O que ganhamos confundindo riqueza de especies
# serie de Hill
library(vegan)
library(ggplot2)
library(forcats)
library(iNEXT)
library(cowplot)

df <- read.csv("ouriquinho.csv")
df_numeric <- df[5:8]
acumula <- specaccum(df_numeric, method = "rarefaction")


plot_data <- data.frame(
  "Locais" = c(0, acumula$sites),
  "Riqueza" = c(0, acumula$richness),
  "lower" = c(0, acumula$richness - acumula$sd),
  "upper" = c(0, acumula$richness + acumula$sd)
)



plot_total <- 
  ggplot(plot_data, aes(x = Locais, y = Riqueza)) +
  geom_point(color = "blue", size = 4) +
  geom_line(color = "blue", lwd = 2) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper), 
    linetype = 2, alpha = 0.3, fill = "yellow"
  ) +
  ylab("Riqueza acumulada") +
  theme_classic() +
  theme(text = element_text(size = 16))





df_sem_pinauna <- df[-6]
df_sem_pinauna_numeric <- df_sem_pinauna[5:7]
acumula_sem_pinauna <- specaccum(df_sem_pinauna_numeric, method = "rarefaction")


plot_data_sem_pinauna <- data.frame(
  "Locais" = c(0, acumula_sem_pinauna $sites),
  "Riqueza" = c(0, acumula_sem_pinauna $richness),
  "lower" = c(0, acumula_sem_pinauna $richness - acumula$sd),
  "upper" = c(0, acumula_sem_pinauna $richness + acumula$sd)
)

plot_sem_pinauna <- 
  ggplot(plot_data_sem_pinauna, aes(x = Locais, y = Riqueza)) +
  geom_point(color = "blue", size = 4) +
  geom_line(color = "blue", lwd = 2) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper), 
    linetype = 2, alpha = 0.3, fill = "yellow"
  ) +
  ylab("Riqueza acumulada") +
  theme_classic() +
  theme(text = element_text(size = 16))




# Plot indivíduos
plot_data_individuos <- 
  data.frame(
    "Individuals" = c(0, acumula$individuals),
    "Riqueza" = c(0, acumula$richness),
    "lower" = c(0, acumula$richness - acumula$sd),
    "upper" = c(0, acumula$richness + acumula$sd)
  )


individuos_plot <- 
  ggplot(
    plot_data_individuos, 
    aes(x = Individuals, y = Riqueza)
  ) +
  geom_point(color = "blue", size = 4) +
  geom_line(color = "blue", lwd = 2) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper), 
    linetype=2, alpha=0.3, fill = "yellow"
  ) +
  ylab("Riqueza acumulada") +
  theme_classic() +
  theme(text = element_text(size = 16))



plot_data_individuos_sem_pinauna <-
    data.frame(
    "Individuals" = c(0, acumula_sem_pinauna$individuals),
    "Riqueza" = c(0, acumula_sem_pinauna$richness),
    "lower" = c(0, acumula_sem_pinauna$richness - acumula_sem_pinauna$sd),
    "upper" = c(0, acumula_sem_pinauna$richness + acumula_sem_pinauna$sd)
  )


individuos_plot_sem_pinauna <- 
  ggplot(
    plot_data_individuos_sem_pinauna, 
    aes(x = Individuals, y = Riqueza)
  ) +
  geom_point(color = "blue", size = 4) +
  geom_line(color = "blue", lwd = 2) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper), 
    linetype=2, alpha=0.3, fill = "yellow"
  ) +
  ylab("Riqueza acumulada") +
  theme_classic() +
  theme(text = element_text(size = 16))

## Criar Painel com titulos e labels (A, B, C, D)
painel <- plot_grid(
  plot_total,
  plot_sem_pinauna,
  labels = c("A", "B")
)

ggsave("ouricos_acumulacao_locais.png", painel, width = 12, height = 6)

