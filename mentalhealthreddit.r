data <- read.csv(file.choose(), header = TRUE)
str(data)

install.packages("syuzhet")
install.packages("lubridate")

library(syuzhet)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(dplyr)
#Create a vector containing only the text
#text <- data$text
text <- data$post_name
postname <- iconv(data$post_name, to = "utf-8")
snt <- get_nrc_sentiment(postname)
head(snt)
postname[3]
get_nrc_sentiment('argument')
data$post_username[3]
barplot(colSums(snt),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = "Sentiment scores for r/mentalhealth posts")
#data[data$post_username=="Fun_Distribution_143",]

# Create a corpus  
docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, random.order = FALSE, 
          colors=brewer.pal(8, "Dark2"), max.words=200, rot.per=0.35)

barplot(data$post_comments,names.arg=data$post_flair,xlab="Flair",ylab="Comments",col="blue",
        main="Flairs based on number of comments",border="red")

data_grp_flair = data %>% group_by(post_flair)  %>%
  summarise(total_upvotes = sum(post_upvotes),
            .groups = 'drop')

View(data_grp_flair)
library(plyr)
data$post_upvotes <- revalue(data$post_upvotes, c("•" = "0"))
#transform(data, post_upvotes = as.numeric(post_upvotes))
as.numeric(as.character(data$post_upvotes))
str(data)
print(typeof(data$post_upvotes))
write.csv(data, "C:\\Users\\DELL\\Downloads\\subreddit1.csv", row.names=FALSE)
data["post_flair"][data["post_flair"] == ''] <- NA

library(ggplot2)
library(dplyr)
data %>% 
  arrange(desc(post_upvotes)) %>%
  slice(1:402) %>%
  ggplot(., aes(x=post_flair, y=post_upvotes))+
  geom_bar(stat='identity')

data %>% 
  arrange(desc(post_comments)) %>%
  slice(1:402) %>%
  ggplot(., aes(x=post_flair, y=post_comments))+
  geom_bar(stat='identity')


data[data$post_flair == 'Venting',]
nrow(data[data$post_flair == 'Venting' & data$post_flair != 'NA', ])
nrow(data[data$post_flair == 'NA', ])
nrow(data[data$post_flair == 'Resources', ])
sum(data$post_flair == "Venting")
length(which(data$post_flair == "Venting"))
length(which(data$post_flair == "Resources"))
length(which(data$post_flair == "Research Study"))
length(which(data$post_flair == "Sadness / Grief"))
length(which(data$post_flair == "Opinion / Thoughts"))
length(which(data$post_flair == "Member Poll"))
length(which(data$post_flair == "Good News / Happy"))

plot(data$post_comments,data$post_upvotes,
     main ="Scatterplot",
     xlab ="Number of upvotes",
     ylab =" Number of comments ", pch = 19)
