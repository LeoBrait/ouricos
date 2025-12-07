library(vegan)

df <- read.csv("ouriquinho.csv")
df$FatorCombinado <- interaction(df$estrato, df$ponto, sep = "_")

comm <- df[, c("satelite", "ourico", "pepino", "ofiuridio")]

# remover amostras sem organismos
comm2 <- comm[rowSums(comm) > 0, ]
estrato2 <- df$estrato[rowSums(comm) > 0]

# distância e clustering
dist_bc <- vegdist(comm2, method = "bray")
clust <- hclust(dist_bc, method = "average")

plot(clust, labels = estrato2)


adonis2(dist_bc ~ estrato2)
anosim(dist_bc, estrato2)

anosim_interacao <- anosim(dist_bc, df$FatorCombinado[rowSums(comm) > 0], permutations = 9999)
anosim_interacao
