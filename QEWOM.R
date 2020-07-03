load("/home/juan/Documents/ComentariosFrescosDomicilios/DomiciliosComments.RData")
rm(list=setdiff(ls(), "df"))
library(dplyr)
df$Rankings <- gsub("[[:punct:]]", "", df$Rankings) %>% as.numeric(df$Rankings)
df$comments <- as.character(df$Comments)
df$TotalComments <- as.numeric(gsub("[^0-9.-]", "", df$Total_comments))
deliveries <- data.frame(table(df$Deliveries))
df$Deliveries <- as.numeric(gsub("[^0-9.-]", "", df$Deliveries))
deliveries2 <- data.frame(table(df$Deliveries))

library(dplyr)
df <- mutate(df, DeliveryTime =
         ifelse(grepl("30-45 mins", Times), "30-45",
         ifelse(grepl("45-60 mins", Times), "45-60",
         "60-90")))

Restaurants <- data.frame(table(df$Name))
# Based on the number of comments, we select 
# the restaurants with comments above 100
SelectedRestaurants <- filter(Restaurants, Freq > 200)
# We merged the comments of different restaurants that
# share the same commercial brand (e.g., KFC) and 
# have different branch or point-of-sale.

DefinitiveSample <- filter(
  df, grepl("Arroz y Pasta al Wok", Name) |
      grepl("Bogota Food Company", Name) |
      grepl("Casa Del Sushi", Name) |
      grepl("Charlies Roastbeef", Name) |  
      grepl("China Tao", Name) |
      grepl("Colombia and Pizza", Name) |
      grepl("Comidas Rapidas", Name) |
      grepl("Donde Beto", Name) |
      grepl("Donde Lucho", Name) |
      grepl("Donde Pele", Name) |
      grepl("El Señor del Pollo", Name) |
      grepl("Frisby", Name) |
      grepl("Hamburguesas Santa Fe", Name) |
      grepl("Hot Delivery Principal", Name) |
      grepl("Juancho", Name) |
      grepl("JYS", Name) |
      grepl("KFC", Name) |
      grepl("La Sazón", Name) |
      grepl("Manyares", Name) |
      grepl("Napoli", Name) |
      grepl("Perros", Name) |
      grepl("Pizza del Barrio", Name) |
      grepl("Presto", Name) |
      grepl("Sr Wok", Name))

pave <- data.frame(table(DefinitiveSample$Name))
library(quanteda)
my_corpus <- corpus(DefinitiveSample$comments)
mycorpus <- data.frame(summary(my_corpus, n = nrow(DefinitiveSample)))
head(summary(my_corpus))
docvars(my_corpus, "Name") <- DefinitiveSample$Name
docvars(my_corpus, "ShipmentCost") <- DefinitiveSample$Shippings
docvars(my_corpus, "Rating") <- DefinitiveSample$Rankings
docvars(my_corpus, "MinimumOrder") <- DefinitiveSample$Deliveries
head(summary(my_corpus))
# Let's first create a list of stopwords
spanishstopwords <- c("q", stopwords("spanish"))

CustomersDFM <- dfm(
  my_corpus, 
  remove_numbers = TRUE, 
  remove = spanishstopwords, 
  stem = TRUE, 
  remove_punct = TRUE)

CustomersDFM[,1:5]


# Because some of these restaurants are located in
# different places of the city, we will merge the comments
# in just one single corpus for each restaurant
ArrozyPasta <- dfm(corpus_subset(my_corpus, grepl("Arroz y Pasta al Wok", Name)))
ArrozyPastaSIM <- textstat_simil(ArrozyPasta, margin = "documents", method = "jaccard")
a <- data.frame(as.matrix(ArrozyPastaSIM))
library(mclust)
fit <- Mclust(a)
summary(fit)
clasificados <- data.frame(fit$classification)
names(clasificados)[1] <- "classification"
clasificados$Category <- "Arroz y Pasta al Wok"

BogotaFoodCompany <- dfm(corpus_subset(my_corpus, grepl("Bogota Food Company", Name)))
BogotaFoodCompanySIM <- textstat_simil(BogotaFoodCompany, margin = "documents", method = "jaccard")
b <- data.frame(as.matrix(BogotaFoodCompanySIM))
b[is.na(b)] <- 0
fit <- Mclust(b)
summary(fit)
clasificados2 <- data.frame(fit$classification)
names(clasificados2)[1] <- "classification"
clasificados2$Category <- "Bogota Food Company"


CasadelSushi <- dfm(corpus_subset(my_corpus, grepl("Casa Del Sushi", Name)))
CasadelSushiSIM <- textstat_simil(CasadelSushi, margin = "documents", method = "jaccard")
c <- data.frame(as.matrix(CasadelSushiSIM))
c[is.na(c)] <- 0
fit <- Mclust(c)
summary(fit)
clasificados3 <- data.frame(fit$classification)
names(clasificados3)[1] <- "classification"
clasificados3$Category <- "Casa del Sushi"

CharliesRoastbeef <- dfm(corpus_subset(my_corpus, grepl("Charlies Roastbeef", Name)))
CharliesSIM <- textstat_simil(CharliesRoastbeef, margin = "documents", method = "jaccard")
d <- data.frame(as.matrix(CasadelSushiSIM))
d[is.na(d)] <- 0
fit <- Mclust(d)
summary(fit)
clasificados4 <- data.frame(fit$classification)
names(clasificados4)[1] <- "classification"
clasificados4$Category <- "Charlies Roastbeef"

ChinaTao <- dfm(corpus_subset(my_corpus, grepl("China", Name)))
ChinaTaoSIM <- textstat_simil(ChinaTao, margin = "documents", method = "jaccard")
e <- data.frame(as.matrix(ChinaTaoSIM))
e[is.na(e)] <- 0
fit <- Mclust(e)
summary(fit)
clasificados5 <- data.frame(fit$classification)
names(clasificados5)[1] <- "classification"
clasificados5$Category <- "China Tao"



ColombiaAndPizza <- dfm(corpus_subset(my_corpus, grepl("Colombia and Pizza", Name)))
ColombiaAndPizzaSIM <- textstat_simil(ColombiaAndPizza, margin = "documents", method = "jaccard")
f <- data.frame(as.matrix(ColombiaAndPizzaSIM))
f[is.na(f)] <- 0
fit <- Mclust(f)
summary(fit)
clasificados6 <- data.frame(fit$classification)
names(clasificados6)[1] <- "classification"
clasificados6$Category <- "Colombia and Pizza"


ComidasRapidas <- dfm(corpus_subset(my_corpus, grepl("Comidas Rapidas", Name)))
ComidasRapidasSIM <- textstat_simil(ComidasRapidas, margin = "documents", method = "jaccard")
g <- data.frame(as.matrix(ComidasRapidasSIM))
g[is.na(g)] <- 0
fit <- Mclust(g)
summary(fit)
clasificados6 <- data.frame(fit$classification)
names(clasificados6)[1] <- "classification"
clasificados6$Category <- "Colombia and Pizza"


DondeBeto <- dfm(corpus_subset(my_corpus, grepl("Donde Beto", Name)))
DondeBetoSIM <- textstat_simil(DondeBeto, margin = "documents", method = "jaccard")
h <- data.frame(as.matrix(DondeBetoSIM))
h[is.na(h)] <- 0
fit <- Mclust(g)
summary(fit)
clasificados7 <- data.frame(fit$classification)
names(clasificados7)[1] <- "classification"
clasificados7$Category <- "Donde Beto"


DondeLucho <- dfm(corpus_subset(my_corpus, grepl("Donde Lucho", Name)))
DondeLuchoSIM <- textstat_simil(DondeLucho, margin = "documents", method = "jaccard")
i <- data.frame(as.matrix(DondeLuchoSIM))
i[is.na(i)] <- 0
fit <- Mclust(i)
summary(fit)
clasificados8 <- data.frame(fit$classification)
names(clasificados8)[1] <- "classification"
clasificados8$Category <- "Donde Lucho"


DondePele <- dfm(corpus_subset(my_corpus, grepl("Donde Pele", Name)))
DondePeleDFM <- textstat_simil(DondePele, margin = "documents", method = "jaccard")
j <- data.frame(as.matrix(DondeBetoSIM))
j[is.na(j)] <- 0
fit <- Mclust(j)
summary(fit)
clasificados7 <- data.frame(fit$classification)
names(clasificados7)[1] <- "classification"
clasificados7$Category <- "Donde Beto"

Elsenor <- corpus_subset(my_corpus, grepl("El Señor del Pollo", Name))
ElsenorDFM <- dfm(Elsenor)
j <- convert(ElsenorDFM, to = "data.frame")

Frisby <- corpus_subset(my_corpus, grepl("Frisby", Name))
FrisbyDFM <- dfm(Frisby)
k <- convert(FrisbyDFM, to = "data.frame")

Hamburguesas <- corpus_subset(my_corpus, grepl("Hamburguesas Santa Fe", Name))
HamburguesasDFM <- dfm(Hamburguesas)
l <- convert(HamburguesasDFM, to = "data.frame")


HotDelivery <- corpus_subset(my_corpus, grepl("Hot Delivery", Name))
HotDeliveryDFM <- dfm(HotDelivery)
m <- convert(HotDeliveryDFM, to = "data.frame")


JYS <- corpus_subset(my_corpus, grepl("JYS", Name))
JYSDFM <- dfm(JYS)
n <- convert(JYSDFM, to = "data.frame")

KFC <- corpus_subset(my_corpus, grepl("KFC ", Name))
KFCDFM <- dfm(KFC)
o <- convert(KFCDFM, to = "data.frame")

LaSazon <- corpus_subset(my_corpus, grepl("La Sazón ", Name))
LaSazondfm <- dfm(LaSazon)
p <- convert(LaSazondfm, to = "data.frame")

Manyares <- corpus_subset(my_corpus, grepl("Manyares", Name))
ManyaresDFM <- dfm(Manyares)
q <- convert(ManyaresDFM, to = "data.frame")

NapoliBurger <- corpus_subset(my_corpus, grepl("Napoli", Name))
NapoliDFM <- dfm(NapoliBurger)
r <- convert(NapoliDFM, to = "data.frame")

PerrosJ <- corpus_subset(my_corpus, grepl("Perros J&R", Name))
PerrosDFM <- dfm(PerrosJ)
s <- convert(PerrosDFM, to = "data.frame")

PizzadelBarrio <- corpus_subset(my_corpus, grepl("Pizza del Barrio", Name))
PizzadelBDFM <- dfm(PizzadelBarrio)
t <- convert(PizzadelBDFM, to = "data.frame")

Presto <- corpus_subset(my_corpus, grepl("Presto", Name))
PrestoDFM <- dfm(Presto)
u <- convert(PrestoDFM, to = "data.frame")

SrWok <- corpus_subset(my_corpus, grepl("Sr Wok", Name))
SrWokDFM <- dfm(SrWok)
v <- convert(SrWokDFM, to = "data.frame")

# Finally, we end up with the following
# 22 incidence matrices
rm(list=setdiff(ls(), c("a", "b", "c", "d", "e", "f", "g", "h", 
                "i", "j", "k", "l", "m", "n", "o", "p",
                "q", "r", "s", "t", "u", "v")))