library(tidyverse)

df <- read.csv("ouriquinho.csv")
df_long <- df %>%
  pivot_longer(cols = c("satelite", "ourico", "ofiuridio", "pepino"),
               names_to = "Especie",
               values_to = "Abundancia") %>%
  mutate(
    estrato = fct_relevel(
        estrato,
        c("praia_superior", "meio", "oceano_inferior")
    ))

ggplot(df_long, aes(x = interaction(estrato,ponto,replica), y = Abundancia, fill = Especie)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~estrato, scales = "free_x")
