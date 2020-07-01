ArrozyPasta <- dfm(corpus_subset(my_corpus, grepl("Arroz y Pasta al Wok", Name)))
ArrozyPastaSIM <- textstat_simil(ArrozyPasta, margin = "documents", method = "jaccard")
adf <- data.frame(as.matrix(ArrozyPastaSIM))


library(mclust)
fit <- Mclust(adf)
summary(fit)
clasificados <- data.frame(fit$classification)
names(clasificados)[1] <- "classification"
clasificados$Category <- "Arroz y Pasta al Wok"
######


ArrozyPastaDFM <- dfm(ArrozyPasta)
convert(ArrozyPastaDFM, to = "data.frame")
