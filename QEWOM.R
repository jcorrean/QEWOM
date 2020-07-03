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
# Based on the number of comments, we select 
# the restaurants with comments above 100
SelectedRestaurants <- filter(Restaurants, Freq > 200)
# We merged the comments of different restaurants that
# share the same commercial brand (e.g., KFC) and 
# have different branch or point-of-sale.

DefinitiveSample <- filter(
  df, grepl("Arroz y Pasta al Wok", Name) |
      grepl("Bogota Food Company", Name) |
      grepl("Casa Del Sushi", Name) |
      grepl("Charlies Roastbeef", Name) |  
      grepl("China Tao", Name) |
      grepl("Colombia and Pizza", Name) |
      grepl("Comidas Rapidas", Name) |
      grepl("Donde Beto", Name) |
      grepl("Donde Lucho", Name) |
      grepl("Donde Pele", Name) |
      grepl("El Señor del Pollo", Name) |
      grepl("Frisby", Name) |
      grepl("Hamburguesas Santa Fe", Name) |
      grepl("Hot Delivery Principal", Name) |
      grepl("Juancho", Name) |
      grepl("JYS", Name) |
      grepl("KFC", Name) |
      grepl("La Sazón", Name) |
      grepl("Manyares", Name) |
      grepl("Napoli", Name) |
      grepl("Perros", Name) |
      grepl("Pizza del Barrio", Name) |
      grepl("Presto", Name) |
      grepl("Sr Wok", Name))

DefinitiveSample <- DefinitiveSample[c(1,8:11)]

library(quanteda)
my_corpus <- corpus(DefinitiveSample$comments)
mycorpus <- data.frame(summary(my_corpus, n = nrow(DefinitiveSample)))
head(summary(my_corpus))
docvars(my_corpus, "Name") <- DefinitiveSample$Name
docvars(my_corpus, "ShipmentCost") <- DefinitiveSample$Shippings
docvars(my_corpus, "Rating") <- DefinitiveSample$Rankings
docvars(my_corpus, "MinimumOrder") <- DefinitiveSample$Deliveries
head(summary(my_corpus))
# Let's first create a list of stopwords
spanishstopwords <- c("q", stopwords("spanish"))

CustomersDFM <- dfm(
  my_corpus, 
  remove_numbers = TRUE, 
  remove = spanishstopwords, 
  stem = TRUE, 
  remove_punct = TRUE)

# Here, we can check the document-term frequency
CustomersDFM[,1:5]

# Let's see if this document-term frequency is clusterable
# or not, by calculating the Hopkins statistics.

library(clustertend)
hopkins(as.matrix(CustomersDFM), n = nrow(DefinitiveSample)/4)


# Finally, we end up with the following
# 22 incidence matrices
rm(list=setdiff(ls(), c("a", "b", "c", "d", "e", "f", "g", "h", 
                "i", "j", "k", "l", "m", "n", "o", "p",
                "q", "r", "s", "t", "u", "v")))