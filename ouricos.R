# Adriano Sanches Melo O que ganhamos confundindo riqueza de especies
# serie de Hill
library(vegan)
library(ggplot2)
library(forcats)
library(iNEXT)

df <- read.csv("ouriquinho.csv")

df_numeric <- df[5:8]
acumula <- specaccum(df_numeric, method = "rarefaction")
plot(acumula)


plot_data <- data.frame("Locais" = c(0, acumula$sites),
                        "Riqueza" = c(0, acumula$richness),
                        "lower" = c(0, acumula$richness - acumula$sd),
                        "upper" = c(0, acumula$richness + acumula$sd))
g <- ggplot(plot_data, aes(x = Locais, y = Riqueza)) +
  geom_point(color = "blue", size = 4) +
  geom_line(color = "blue", lwd = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              linetype=2, alpha=0.3, fill = "yellow") +
  ylab("Riqueza acumulada") +
  theme_classic() +
  theme(text = element_text(size = 16))
g



# Plot indivíduos
plot_data <- data.frame("Individuals" = c(0, acumula$individuals),
                        "Riqueza" = c(0, acumula$richness),
                        "lower" = c(0, acumula$richness - acumula$sd),
                        "upper" = c(0, acumula$richness + acumula$sd))
g <- ggplot(plot_data, aes(x = Individuals, y = Riqueza)) +
  geom_point(color = "blue", size = 4) +
  geom_line(color = "blue", lwd = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              linetype=2, alpha=0.3, fill = "yellow") +
  ylab("Riqueza acumulada") +
  theme_classic() +
  theme(text = element_text(size = 16))
g
