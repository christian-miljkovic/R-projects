#load all of the libaries that you will be using
library(tm)
library(wordcloud)
library(topicmodels)


###Part 1####

#read the data into R
news_article_csv = read.csv("/Users/christianmiljkovic/Downloads/NewsArticles.csv")

news_article_csv = news_article_csv[1:1000,]

#convert the news articles to a corpus so that you can clean the text and mine it
corp.original <- VCorpus(VectorSource(news_article_csv$content))

corp_clean = tm_map(corp.original, removePunctuation)
corp_clean = tm_map(corp_clean, removeNumbers)
corp_clean = tm_map(corp_clean, content_transformer(removeWords), stopwords("SMART"), lazy = T)
corp_clean = tm_map(corp_clean, content_transformer(tolower), lazy = T)
corp_clean = tm_map(corp_clean, content_transformer(stemDocument), lazy = T)
corp_clean = tm_map(corp_clean, stripWhitespace)

#convert it to a dtm so that you can analyze the data

dtm = DocumentTermMatrix(corp_clean)
dtm = removeSparseTerms(dtm[1:1000,], .995)

dtm_matrix = as.matrix(dtm)

#get rid of anything with just zeros
terms = rowSums(dtm_matrix) != 0

dtm_matrix = dtm_matrix[terms,]

#fit an LDA model to prduce topics
ldaOut <-LDA(dtm_matrix, 10, method="Gibbs")

terms(ldaOut,10)

#ldaOut topic names starting from topic 1 to topic 10
lda_topics = c("Everyday News","Housing Market","Business","Media",
               "Health","Financial","Companies","Company Measures",
               "Markets","Presidential")

###Part 2###

#load in libararies
library(rvest)
library(stringr)
library(XML)


#load in the data from the website you are scraping 
#then clean the data so that you just have website links
cnbc1 = read_html("http://www.cnbc.com/us-news/")

cnbc1 = html_nodes(cnbc1, "#pipeline_assetlist_0 .headline a")

cnbc1 = html_attr(cnbc1,"href")

cnbc1 = cnbc1[!is.na(cnbc1)]


#make them a single valid string
cnbc1 = paste("http://www.cnbc.com",cnbc1[1:16],sep = "")

#collect and clean the first link you collected
first_link = read_html(cnbc1[1])

first_link = html_nodes(first_link, "p")

first_link = html_text(first_link)

first_link = paste(first_link,collapse = "")

first_link = gsub("\"","",first_link)

first_link = gsub("  ","",first_link)

first_link = gsub("[[:punct:]]", "", first_link)

#add it to a data frame so you can maintain each document that you clean
news_pages_frame = as.data.frame(first_link)

#repeat the above steps for all of the other articles
for(i in c(2:16)) {

  #collect and clean the first link you collected
  other_link = read_html(cnbc1[i])
  
  other_link = html_nodes(other_link, "p")
  
  other_link = html_text(other_link)
  
  other_link = paste(other_link,collapse = "")
  
  other_link = gsub("\"","",other_link)
  
  other_link = gsub("  ","",other_link)
  
  other_link = gsub("[[:punct:]]", "", other_link)
  
  news_pages_frame = cbind(news_pages_frame,other_link)
}

###Step 3###

#determine what words are going to be examined
dic = Terms(dtm)

#create a vector that will hold the category for every individual article
topic_prob_vect = c(NULL)

#loop through each article and apply the topics developed from part 1 to determine
#each articles categorization
for(i in c(1:ncol(news_pages_frame))) {
  
  corp.first <- VCorpus(VectorSource(news_pages_frame[1,i]))

  corp.first.clean = tm_map(corp.first, removePunctuation)
  corp.first.clean = tm_map(corp.first.clean, removeNumbers)
  corp.first.clean = tm_map(corp.first.clean, content_transformer(tolower), lazy = T)
  corp.first.clean = tm_map(corp.first.clean, stripWhitespace)

  #get the probabilities of each cateogry for each document
  new_dtm = DocumentTermMatrix(corp.first.clean, control=list(dictionary = dic))
  new_dtm = new_dtm[rowSums(as.matrix(new_dtm))!=0,]
  topic_probabilities = posterior(ldaOut, new_dtm)
  
  #create a vector of all the probabilities for each category of every individual article
  all_probabilities = topic_probabilities$topics
  
  colnames(all_probabilities) = lda_topics
  
  #find the max value and its index in order to place the categorization in the vector topic_prob_vect
  topic_prob_vect = append(topic_prob_vect,
                           colnames(all_probabilities)[which.max(all_probabilities)])

}


#rename the columns of the data frame holding the entire text of each article
colnames(news_pages_frame) = topic_prob_vect

#print out the article and its categorization
news_pages_frame[1:10]
