load("/home/juan/Documents/ComentariosFrescosDomicilios/DomiciliosComments.RData")
rm(list=setdiff(ls(), "df"))
df$comments <- as.character(df$Comments)
df$TotalComments <- as.numeric(gsub("[^0-9.-]", "", df$Total_comments))
deliveries <- data.frame(table(df$Deliveries))
df$Deliveries <- as.numeric(gsub("[^0-9.-]", "", df$Deliveries))
deliveries2 <- data.frame(table(df$Deliveries))
table(df$Times)[3]

library(dplyr)
df <- mutate(df, DeliveryTime =
         ifelse(grepl("30-45 mins", Times), "30-45",
         ifelse(grepl("45-60 mins", Times), "45-60",
         "60-90")))

table(df$DeliveryTime)

library(quanteda)
my_corpus <- corpus(df$comments)
mycorpus <- data.frame(summary(my_corpus, n = nrow(df)))
head(summary(my_corpus))
docvars(my_corpus, "Name") <- df$Name
docvars(my_corpus, "Provider") <- UserComments$Provider
docvars(my_corpus, "Rating") <- UserComments$Rating