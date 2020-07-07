load("/home/juan/Documents/ComentariosFrescosDomicilios/DomiciliosComments.RData")
rm(list=setdiff(ls(), "df"))
library(dplyr) 
df$Rankings <- gsub("[[:punct:]]", "", df$Rankings) %>% as.numeric(df$Rankings)
df$Comments <- as.character(df$Comments)
df$Total_comments <- as.numeric(gsub("[^0-9.-]", "", df$Total_comments))
deliveries <- data.frame(table(df$Deliveries))
df$Deliveries <- as.numeric(gsub("[^0-9.-]", "", df$Deliveries))

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

ToyDB <- filter(
    df, grepl("Arroz y Pasta al Wok", Name) |
      grepl("Bogota Food Company", Name) |
      grepl("Bogotá Food Company Centro", Name))

library(quanteda)
my_corpus <- corpus(ToyDB$Comments)
mycorpus <- data.frame(summary(my_corpus, n = nrow(ToyDB)))
head(summary(my_corpus))
docvars(my_corpus, "Name") <- ToyDB$Name
docvars(my_corpus, "ShipmentCost") <- ToyDB$Shippings
docvars(my_corpus, "Rating") <- ToyDB$Rankings
docvars(my_corpus, "MinimumOrder") <- ToyDB$Deliveries
head(summary(my_corpus))
overviewcorpus <- data.frame(summary(my_corpus, n = nrow(ToyDB)))

#ToyDB <- cbind(ToyDB, overviewcorpus, pave2)


# Let's first create a list of stopwords
spanishstopwords <- c("q", stopwords("spanish"))

CustomersDFM <- dfm(
  my_corpus, 
  remove_numbers = TRUE, 
  remove = spanishstopwords, 
  stem = TRUE, 
  remove_punct = TRUE)

ArrozyPasta <- dfm(corpus_subset(my_corpus, grepl("Arroz y Pasta al Wok", Name)))
ArrozyPastaSIM <- textstat_simil(ArrozyPasta, margin = "documents", method = "jaccard")
a <- data.frame(as.matrix(ArrozyPastaSIM))
library(mclust)
fit <- Mclust(a)
summary(fit)
clasificados <- data.frame(fit$classification)
names(clasificados)[1] <- "classification"
clasificados$Category <- "Arroz y Pasta al Wok"

BogotaFoodCompany <- dfm(corpus_subset(my_corpus, grepl("Bogota Food Company", Name) |  grepl("Bogotá Food Company Centro", Name)))
BogotaFoodCompanySIM <- textstat_simil(BogotaFoodCompany, margin = "documents", method = "jaccard")
b <- data.frame(as.matrix(BogotaFoodCompanySIM))
b[is.na(b)] <- 0
fit2 <- Mclust(b)
summary(fit2)
clasificados2 <- data.frame(fit2$classification)
names(clasificados2)[1] <- "classification"
clasificados2$Category <- "Bogota Food Company"

#DefinitiveSample <- filter(
#  df, grepl("Arroz y Pasta al Wok", Name) |
#      grepl("Bogota Food Company", Name) |
#      grepl("Casa Del Sushi", Name) |
#      grepl("Charlies Roastbeef", Name) |  
#      grepl("China Tao", Name) |
#      grepl("Colombia and Pizza", Name) |
#      grepl("Comidas Rapidas", Name) |
#      grepl("Donde Beto", Name) |
#      grepl("Donde Lucho", Name) |
#      grepl("Donde Pele", Name) |
#      grepl("El Señor del Pollo", Name) |
#      grepl("Frisby", Name) |
#      grepl("Hamburguesas Santa Fe", Name) |
#      grepl("Hot Delivery Principal", Name) |
#      grepl("Juancho", Name) |
#      grepl("JYS", Name) |
#      grepl("KFC", Name) |
#      grepl("La Sazón", Name) |
#      grepl("Manyares", Name) |
#      grepl("Napoli", Name) |
#      grepl("Perros", Name) |
#      grepl("Pizza del Barrio", Name) |
#      grepl("Presto", Name) |
#      grepl("Sr Wok", Name))

pave <- list(clasificados, clasificados2)
pave2 <- do.call(rbind.data.frame, pave)



#DefinitiveSample <- DefinitiveSample[c(1,8:11)]
#rm(list=setdiff(ls(), "DefinitiveSample"))
#write.csv(DefinitiveSample, file = "/home/juan/Documents/ComentariosFrescosDomicilios/df.csv")
#library(quanteda)
#my_corpus <- corpus(DefinitiveSample$comments)
#mycorpus <- data.frame(summary(my_corpus, n = nrow(DefinitiveSample)))
#head(summary(my_corpus))
#docvars(my_corpus, "Name") <- DefinitiveSample$Name
#docvars(my_corpus, "ShipmentCost") <- DefinitiveSample$Shippings
#docvars(my_corpus, "Rating") <- DefinitiveSample$Rankings
#docvars(my_corpus, "MinimumOrder") <- DefinitiveSample$Deliveries
#head(summary(my_corpus))
# Let's first create a list of stopwords
#spanishstopwords <- c("q", stopwords("spanish"))

#CustomersDFM <- dfm(
#  my_corpus, 
#  remove_numbers = TRUE, 
#  remove = spanishstopwords, 
#  stem = TRUE, 
#  remove_punct = TRUE)

# Here, we can check the document-term frequency
CustomersDFM[,1:5]


# Finally, we end up with the following
# 22 incidence matrices
#rm(list=setdiff(ls(), c("a", "b", "c", "d", "e", "f", "g", "h", 
#                "i", "j", "k", "l", "m", "n", "o", "p",
#                "q", "r", "s", "t", "u", "v")))