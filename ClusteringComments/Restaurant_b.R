load("~/Documents/Corpus.RData")

BogotaFoodCompany <- dfm(corpus_subset(my_corpus, grepl("Bogota Food Company", Name)))
BogotaFoodCompanySIM <- textstat_simil(BogotaFoodCompany, margin = "documents", method = "jaccard")
b <- data.frame(as.matrix(BogotaFoodCompanySIM))
b[is.na(b)] <- 0
fit2 <- Mclust(b)
summary(fit2)
clasificados2 <- data.frame(fit2$classification)
names(clasificados2)[1] <- "classification"
clasificados2$Category <- "Bogota Food Company"
