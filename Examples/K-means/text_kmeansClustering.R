
library(data.table)
library(quanteda)
library(RColorBrewer)
library(cluster)
my_data <- read.csv("~/Data/healthdat.csv")

my_data <- data.table(my_data)
dat <- copy(my_data)
#create a text column
dat[, num := 1:length(dat$X)]
dat[, ID := paste0("text",num)]
#clean data
dat <- dat[!(is.na(Text) | Text == ""), ]
#set all text to lower case
dat$Text <- as.character(dat$Text)
dat$Text <- quanteda::char_tolower(dat$Text)
#tokenise text and remove puntuation
toks <- quanteda::tokens(dat$Text, remove_punct = TRUE, remove_symbols = TRUE,
                         remove_twitter = TRUE, remove_numbers = TRUE)
#remove stopwords
stopwordsPL <- c("thu", "tue", "dec", "rt", "mon", "jun", "sat", "sun", "rss",
                 "sep", "feb", "nov", "may", "says", "jan", "fri", "aug", "apr",
                 "oct", "jul", "can", "http", "wed", "amp", "reut.rs")
toks <- tokens_remove(toks, c(quanteda::stopwords("english"), as.character(stopwordsPL)))
dfm <- dfm(toks)

head(dfm, nf = 10) 

topfeatures(dfm, 100)
textplot_wordcloud(dfm, max.words = 30, colors = brewer.pal(8, "Set2"), scale = c(5,1))
dfm2 <- dfm_trim(dfm, min_docfreq = 500)
dfm2
head(dfm2, nf = 10) 

# k-means clustering
set.seed(123)
clusterk3 <- kmeans(tf(dfm2, "prop"), 15)
docs <- split(docnames(dfm2), clusterk3$cluster)

# Render the cluster plot
clusplot(as.matrix(dfm2), clusterk3$cluster, color=TRUE, shade=TRUE, labels=15, lines=0, main = "K-means clustering")
clust1 <- docs$`1`
#chack clusters
#the first cluster is latintimes
cluster_1 <- dat[dat$ID %in% clust1]
cluster_1 <- table(cluster_1$Page)

#the first cluster is reuters
clust2 <- docs$`2`
cluster_2 <- dat[dat$ID %in% clust2]
cluster_2 <- table(cluster_2$Page)

t1 <- rbind(cluster_1, cluster_2)

clust3 <- docs$`3`
cluster_3 <- dat[dat$ID %in% clust3]
cluster_3 <- table(cluster_3$Page)

clust4 <- docs$`4`
cluster_4 <- dat[dat$ID %in% clust4]
cluster_4 <- table(cluster_4$Page)

#nu surprise latin times is the most unique cluster

clust5 <- docs$`5`
cluster_5 <- dat[dat$ID %in% clust5]
cluster_5 <- table(cluster_5$Page)

clust6 <- docs$`6`
cluster_6 <- dat[dat$ID %in% clust6]
cluster_6 <- table(cluster_6$Page)

clust7 <- docs$`7`
cluster_7 <- dat[dat$ID %in% clust7]
cluster_7 <- table(cluster_7$Page)

clust8 <- docs$`8`
cluster_8 <- dat[dat$ID %in% clust8]
cluster_8 <- table(cluster_8$Page)

clust9 <- docs$`9`
cluster_9 <- dat[dat$ID %in% clust9]
cluster_9 <- table(cluster_9$Page)

clust10 <- docs$`10`
cluster_10 <- dat[dat$ID %in% clust10]
cluster_10 <- table(cluster_10$Page)

clust11 <- docs$`11`
cluster_11 <- dat[dat$ID %in% clust11]
cluster_11 <- table(cluster_11$Page)

clust12 <- docs$`12`
cluster_12 <- dat[dat$ID %in% clust12]
cluster_12 <- table(cluster_12$Page)

clust13 <- docs$`13`
cluster_13 <- dat[dat$ID %in% clust13]
cluster_13 <- table(cluster_13$Page)

clust14 <- docs$`14`
cluster_14 <- dat[dat$ID %in% clust14]
cluster_14 <- table(cluster_14$Page)

clust15 <- docs$`15`
cluster_15 <- dat[dat$ID %in% clust15]
cluster_15 <- table(cluster_15$Page)


t1 <- rbind(cluster_1, cluster_2)
t2 <- rbind(t1, cluster_3)
t3 <- rbind(t2, cluster_4)
t4 <- rbind(t3, cluster_5)
t5 <- rbind(t4, cluster_6)
t6 <- rbind(t5, cluster_7)
t7 <- rbind(t6, cluster_8)
t8 <- rbind(t7, cluster_9)
t9 <- rbind(t8, cluster_10)

t10 <- rbind(t9, cluster_11)
t11 <- rbind(t10, cluster_12)
t12 <- rbind(t11, cluster_13)
t13 <- rbind(t12, cluster_14)
t14 <- rbind(t13, cluster_15)
t14

heatmap(t14, Colv = NA, Rowv = NA)
