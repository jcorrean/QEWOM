load("~/Documents/GitHub/QEWOM/QEWOM/QEWOM2.RData")

ToyDB <- filter(
  df, grepl("Bogota Food Company", Name) |
    grepl("Food Company Centro", Name) |
    grepl("Food Company Suba", Name) )

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

Restaurant2 <- dfm(corpus_subset(my_corpus, grepl("Food Company", Name)))
Restaurant2SIM <- textstat_simil(Restaurant2, margin = "documents", method = "jaccard")
b <- data.frame(as.matrix(Restaurant2SIM))
b[is.na(b)] <- 0
library(mclust)
fit <- Mclust(b)
summary(fit)
classification <- data.frame(fit$classification)
names(classification)[1] <- "classification"
classification$Restaurant <- "Bogota Food Company"
