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

library(quanteda)
my_corpus <- corpus(df$comments)
mycorpus <- data.frame(summary(my_corpus, n = nrow(df)))
head(summary(my_corpus))
docvars(my_corpus, "Name") <- df$Name
docvars(my_corpus, "ShipmentCost") <- df$Shippings
docvars(my_corpus, "Rating") <- df$Rankings
docvars(my_corpus, "MinimumOrder") <- df$Deliveries
head(summary(my_corpus))

CustomersDFM <- dfm(my_corpus)
CustomersDFM[,1:5]


# Based on the number of comments, we select 
# the restaurants with comments above 100
SelectedRestaurants <- filter(Restaurants, Freq > 200)
# Because some of these restaurants are located in
# different places of the city, we will merge the comments
# in just one single corpus for each restaurant
ArrozyPasta <- corpus_subset(my_corpus, grepl("Arroz y Pasta al Wok", Name))
ArrozyPastaDFM <- dfm(ArrozyPasta)
convert(ArrozyPastaDFM, to = "data.frame")

BogotaFoodCompany <- corpus_subset(my_corpus, grepl("Bogota Food Company", Name))
BogotaFoodCompanyDFM <- dfm(BogotaFoodCompany)
a <- convert(BogotaFoodCompanyDFM, to = "data.frame")

CasadelSushi <- corpus_subset(my_corpus, grepl("Casa Del Sushi", Name))
CasadelSushiDFM <- dfm(CasadelSushi)
b <- convert(CasadelSushiDFM, to = "data.frame")

CharliesRoastbeef <- corpus_subset(my_corpus, grepl("Charlies Roastbeef", Name))
CharliesRoastbeefDFM <- dfm(CharliesRoastbeef)
c <- convert(CharliesRoastbeefDFM, to = "data.frame")

ChinaTaoNorte <- corpus_subset(my_corpus, grepl("China", Name))
ChinaTaoNorteDFM <- dfm(ChinaTaoNorte)
d <- convert(ChinaTaoNorteDFM, to = "data.frame")

ColombiaAndPizza <- corpus_subset(my_corpus, grepl("Colombia and Pizza", Name))
ColombiaAndPizzaDFM <- dfm(ColombiaAndPizza)
e <- convert(ColombiaAndPizzaDFM, to = "data.frame")

ComidasRapidas <- corpus_subset(my_corpus, grepl("Comidas Rapidas", Name))
ComidasRapidasDFM <- dfm(ComidasRapidas)
f <- convert(ComidasRapidasDFM, to = "data.frame")

DondeBeto <- corpus_subset(my_corpus, grepl("Donde Beto", Name))
DondeBetoDFM <- dfm(DondeBeto)
g <- convert(DondeBetoDFM, to = "data.frame")


DondeLucho <- corpus_subset(my_corpus, grepl("Donde Lucho", Name))
DondeLuchoDFM <- dfm(DondeLucho)
h <- convert(DondeLuchoDFM, to = "data.frame")

DondePele <- corpus_subset(my_corpus, grepl("Donde Pele", Name))
DondePeleDFM <- dfm(DondePele)
i <- convert(DondePeleDFM, to = "data.frame")

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

