load("~/Documents/Corpus.RData")

ArrozyPasta <- dfm(corpus_subset(my_corpus, grepl("Arroz y Pasta al Wok", Name)))
ArrozyPastaSIM <- textstat_simil(ArrozyPasta, margin = "documents", method = "jaccard")
a <- data.frame(as.matrix(ArrozyPastaSIM))
library(mclust)
fit <- Mclust(a)
summary(fit)
clasificados <- data.frame(fit$classification)
names(clasificados)[1] <- "classification"
clasificados$Category <- "Arroz y Pasta al Wok"