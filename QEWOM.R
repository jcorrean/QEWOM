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

Provider <- data.frame(table(df$Name))

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

ArrozyPasta <- corpus_subset(my_corpus, grepl("Arroz y Pasta al Wok", Name))
ArrozyPastaDFM <- dfm(ArrozyPasta)
BogotaFoodCompany <- corpus_subset(my_corpus, grepl("Bogota Food Company", Name))
BogotaFoodCompanyDFM <- dfm(BogotaFoodCompany)
