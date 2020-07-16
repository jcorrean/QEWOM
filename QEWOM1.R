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

# Let's first create a list of stopwords
spanishstopwords <- c("q", stopwords("spanish"))

CustomersDFM <- dfm(
  my_corpus, 
  remove_numbers = TRUE, 
  remove = spanishstopwords, 
  stem = TRUE, 
  remove_punct = TRUE)

Restaurant1 <- dfm(corpus_subset(my_corpus, grepl("Arroz y Pasta al Wok", Name)))
Restaurant1SIM <- textstat_simil(Restaurant1, margin = "documents", method = "jaccard")
a <- data.frame(as.matrix(Restaurant1SIM))
library(mclust)
fit <- Mclust(a)
summary(fit)
classification <- data.frame(fit$classification)
names(classification)[1] <- "classification"
classification$Restaurant <- "Arroz y Pasta al Wok"
