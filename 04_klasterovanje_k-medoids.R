# ZADATAK 4 - klasterovanje k-medoids ==========================================
# Dataset: Arcadis.xlsx
library(cluster)
library(readxl)
set.seed(123)

df <- as.data.frame(read_excel(file.choose(), sheet = 1)) # odmah konvertuj u dataframe
df
vars <- c("health", "affordability", "Energy", "Easeofdoingbusiness", "Connectivity")
df.use <- subset(df, select=vars)
acredis_mat <- as.matrix(df.use)

# klasterovati na osnovu: health, affordability, Energy, Easeofdoingbusiness, Connectivity
# metoda PAM, broj klastera 3
k <- 3

pamcluster <- pam(df.use, k, metric="euclidean")
summary(pamcluster)

# Ovako bi izgledalo da koristimo matricu rastojanja
# d_manh <- dist(df.use, method = "manhattan")
# pam_manh <- pam(d_manh, k = 3, diss = TRUE)

# nazivi gradova ova 3 klastera
idx <- pamcluster$id.med
df[pamcluster$id.med,c("City")]

pamcluster$silinfo

# proseci promenljivih po klasterima (df se ne menja)
d <- df
d$cluster <- pamcluster$clustering

res <- data.frame()
for (klaster in 1:k) {
  grad    <- d$City[pamcluster$id.med[klaster]]
  podskup <- d[d$cluster == klaster, ]
  res <- rbind(res, data.frame(
    Cluster = klaster,
    City = grad,
    health = mean(podskup$health),
    affordability = mean(podskup$affordability),
    Energy = mean(podskup$Energy),
    Easeofdoingbusiness = mean(podskup$Easeofdoingbusiness),
    Connectivity = mean(podskup$Connectivity)
  ))
}
res
# KRACE
#res <- aggregate(cbind(health, affordability, Energy, Easeofdoingbusiness, Connectivity) ~ cluster,
#                 data = df, FUN = mean)
#res$City <- df$City[pamcluster$id.med][res$cluster]


# Kreiramo podskup sa kriterijumima za turizam i kriminal i analiziramo zahteve
maska <- df$Tourism > median(df$Tourism) & df$crime < median(df$crime)
podskup <- df[maska, ]
X <- podskup[, vars]

k <- 3
pamcluster <- pam(X, k, metric = "euclidean")
podskup$cluster <- pamcluster$clustering


D <- as.matrix(dist(X, method = "euclidean"))

res <- data.frame()
for (i in 1:k) {
  idx <- which(podskup$cluster == i)      # redovi gradova u klasteru i
  med <- pamcluster$id.med[i]             # red medoida klastera i
  d   <- D[idx, med]                      # rastojanja gradova do medoida
  far <- idx[which.max(d)]                # najudaljeniji grad
  
  res <- rbind(res, data.frame(
    Cluster  = i,
    Medoid   = podskup$City[med],
    Farthest = podskup$City[far],
    Distance = max(d),
    N        = length(idx)
  ))
}
res

# Filtirrati za health i grupisati za kontinente
# - koji klaster ima max count za taj kontinent
# - udeo tog klastera unutar kontinenta
# - ukupno gradova u kontinentu
maska <- df$health > median(df$health)
podskup <- df[maska,]
X <- podskup[,vars] # X je promenljiva nad kojom trazimo cluster vektor
pamcluster <- pam(X, k, metric="euclidean")
podskup$cluster <- pamcluster$clustering

# Tabela 1: kontinent x klaster
tab <- table(podskup$Continent, podskup$cluster)
rownames(tab) <- c("Europe", "North America", "Asia")[as.integer(rownames(tab))]
colnames(tab) <- paste0("Klaster_", colnames(tab))
tab

# Tabela 2: dominantni klaster, udeo i ukupan broj
dom <- data.frame()
for (i in 1:nrow(tab)) {
  n_total  <- sum(tab[i, ])
  dom_klas <- which.max(tab[i, ])
  dom <- rbind(dom, data.frame(
    Continent = rownames(tab)[i],
    Dominant  = dom_klas,
    Share     = round(tab[i, dom_klas] / n_total, 3),
    n_total   = n_total
  ))
}
dom

# 2 razlicite metrike, strana 9


X <- df[,vars] # X je promenljiva nad kojom trazimo cluster vektor
X
k <- 3
pam_euc <- pam(X, k, metric="euclidean")

D <- dist(X, method="manhattan")
pam_manh <- pam(D, k, diss=TRUE)

summary(pam_euc)
summary(pam_manh)

pam_euc$silinfo$avg.width
pam_manh$silinfo$avg.width

modeli <- list(Euclidean=pam_euc, Manhattan=pam_manh)
res <- data.frame()
for(ime in names(modeli)){
  m <- modeli[[ime]]
  res <- rbind(res, data.frame(
    model = ime,
    avg_sill_score = m$silinfo$avg.width,
    objective = m$objective[["swap"]]
  ))
}
res

# bolja metrika (veci avg_sill_score) i medoidi tog modela
best_model <- as.character(res$model[which.max(res$avg_sill_score)])
best_model
df[modeli[[best_model]]$id.med, "City"]