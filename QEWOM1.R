load("~/Documents/GitHub/QEWOM/QEWOM/QEWOM1.RData")

ToyDB <- filter(
  df, grepl("Arroz y Pasta al Wok", Name))



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