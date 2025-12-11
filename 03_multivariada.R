library(vegan)
library(tidyverse)




df <- read.csv("ouriquinho.csv")


df <- df %>%
  mutate(
    estrato = fct_relevel(
      estrato,
      c("praia_superior", "meio", "oceano_inferior")
    ),
    ponto = fct_relevel(
      ponto,
      c("direita", "esquerda")
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
    label = interaction(ponto, replica, sep = " "),
    eixo = interaction(estrato, ponto, replica, sep = " ")
  )





comm <- df[, c("satelite", "ourico", "pepino", "ofiuridio")]

# remover amostras sem organismos
comm2 <- comm[rowSums(comm) > 0, ]
estrato2 <- df$estrato[rowSums(comm) > 0]
eixo <- df$eixo[rowSums(comm) > 0]

# criar um vetor com 

# distância e clustering
dist_bc <- vegdist(comm2, method = "bray")
clust <- hclust(dist_bc, method = "average")
perm <- adonis2(dist_bc ~ estrato2)

R2 <- round(perm$R2[1], 3)
pval <- perm$`Pr(>F)`[1]

# Texto para colocar no título
titulo <- paste0(
  "Cluster (Bray-Curtis) — PERMANOVA: R² = ", R2,
  ", p = ", format(pval, scientific = TRUE, digits = 3)
)
plot(
  clust,
  labels = eixo,
  main = titulo,
  xlab = "Amostras",
  ylab = "Distância",
  sub = ""
)
anosim(dist_bc, estrato2)


plot(clust, labels = eixo, main = "Cluster das amostras - Distância de Bray-Curtis",
     xlab = "Amostras", ylab = "Distância", sub = "")





# ANOTAR RESULTADOS DO R2 E P-VALUE DA PERMANOVA NO GRÁFICO
boxplot(dist_bc ~ estrato2)
