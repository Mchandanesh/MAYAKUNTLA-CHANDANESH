

if (!require("rvest")) install.packages("rvest", quiet=TRUE) ; require("rvest")
if (!require("XML")) install.packages("XML", quiet=TRUE) ; require("XML")
if (!require("stringr")) install.packages("stringr", quiet=TRUE) ; require("stringr")
if (!require("tidyverse")) install.packages("tidyverse", quiet=TRUE) ; require("tidyverse")
setwd(“D:\webscraping project\code”)




# Function to retrieve Review Score ######
getReviewScore <- function(x){
  stars_html <- html_nodes(x,'.ratings-imdb-rating')
  stars <- html_text(stars_html)
  stars <- str_extract(stars[1], "\\d\\.\\d")
  return(stars)
}


# Initialize dataframe to store the movie id ######
all_movies <- data.frame(titles=character(),
                         imdb_ratings=integer(),
                         movie_id=character(),
                         stringsAsFactors=FALSE)









# scrape movie infos, return to all_movies dataframe ######
# output column names: titles	imdb_ratings	movie_id
# loop through 144 pages
for (i in 1:144){
  url <- paste0("https://www.imdb.com/search/title/?title_type=feature&year=2023-01-01,2023-12-31 ")
  url <- URLdecode(url)
  webpage <- read_html(url)
  # get a list of all the films in this page (list of 100)
  films_html <- html_nodes(webpage,'.mode-advanced')
  
  # set sleep time 5 seconds for each page.
  Sys.sleep(5)
  
  
  #loop for all pages
  for (k in 1:length(films_html)){
    
    # extract movie title
    titles_html <- html_nodes(films_html[[k]],'.lister-item-header a')
    titles <- html_text(titles_html)
    
    
    # extract movie average rating (stars)
    stars_html <- html_nodes(films_html[[k]],'.ratings-imdb-rating')
    stars <- html_text(stars_html)
    imdb_ratings <- str_extract(stars[1], "\\d\\.\\d")
    
    
    # extract IMDb movie id
    href_html <- html_nodes(films_html[[k]],'a')%>% html_attr('href')
    movie_id <- strsplit(href_html[[1]],"/")[[1]][3]
    
    # append to dataframe
    this_movie <- as.data.frame(cbind(titles,imdb_ratings,movie_id))
    all_movies <- rbind(all_movies,this_movie)
  }
  # # periodically save the file every 1000 entries
  if(nrow(all_movies)%%1000==0){write.csv(all_movies,'all_movies.csv')}
}
# export to csv
write.csv(all_movies,'all_movies1.csv')

# read in all_movies.csv file
library(readr)
all_movies <- read_csv("all_movies1.csv",header=TRUE,sep=",")





# scrape movie review using movie id as a key, return data to all_reviews dataframe ######
# output names: id	comment_titles	ratings	comments_body
all_reviews <- data.frame(id=character(),
                          comment_titles=character(),
                          ratings=integer(),
                          comments_body=character(), 
                          stringsAsFactors=FALSE)


# get all films reviews, return to all_reviews dataframe
all_ids <- as.character(all_movies$movie_id)
# loop through all movie id
for (id in all_ids){
  # this url is sorted by 'helpfulness of the review', 25 reviews per movie.
  url <- paste0("https://www.imdb.com/title/tt10954600/reviews/?ref_=tt_ql_2 ")
  url <- URLdecode(url)
  webpage <- read_html(url)
  # some movies do not have any review, check for 0 Reviews
  check_review<- html_nodes(webpage,'.article')[[1]]%>%html_text()
  zero_review<- str_detect(check_review,"0 Reviews")
  if(zero_review==TRUE)
    # set sleep time 5 seconds for each page
  {Sys.sleep(5)}
  else{
    films_html <- html_nodes(webpage,'.lister-item-content')
    for (k in 1:length(films_html)){
      # extract review title
      comment_titles_html <- html_nodes(films_html[[k]],'.title')
      comment_titles <- html_text(comment_titles_html)
      comment_titles <-str_trim(gsub("\r?\n|\r", " ", comment_titles))
      
      # extract comment rating
      ratings_html <- html_nodes(films_html[[k]],'.ipl-ratings-bar')
      ratings <- html_text(ratings_html)
      ratings<- str_extract(ratings, "(\\d)+")
      if(identical(ratings, character(0))){ratings<-0} #replace missing rating with 0
      
      # extract review content
      comments_body_html <- html_nodes(films_html[[k]],'.show-more__control ')
      comments_body <- html_text(comments_body_html)
      comments_body <-str_trim(gsub("\r?\n|\r", " ", comments_body))
      
      # combine into dataframe and append
      this_review <- as.data.frame(cbind(id,comment_titles,ratings,comments_body))
      all_reviews <- rbind(all_reviews,this_review)
      # keep track of movie id, in case the script crashes
      write.csv(c(id,match(id,all_ids)),'last_id.csv')
    }
  }
  
  
  # periodically save the file every 500 reviews
  if(nrow(all_reviews)%%500==0){write.csv(all_reviews,'all_reviews.csv')}
}
# export to csv
write.csv(all_reviews,'all_reviews_movie_0-1866.csv')




#load required packages##################################################
library(dplyr)
library(SnowballC)
library(slam)
library(Matrix)
library(rvest)
library(XML)
library(stringr)
library(stringi)
library(tidyverse)
library(stringr)
library(tm)
library(udpipe)
library(text2vec)
library(glmnet)
library(caret)
library(caTools)
library(tidytext)
library(widyr)
library(ggraph)
library(ggforce)
library(igraph)
library(syuzhet)
library(igraph)
library(shinycssloaders)
library(wordcloud2)
library(shiny)
library(shinythemes)
#load required packages##################################################
# packages_needed <- c('dplyr','SnowballC','slam','tm',
#                      'RWeka','Matrix','rvest',
#                      'XML','stringr','stringi',
#                      'tidyverse','stringr', 
#                      'udpipe','text2vec','glmnet',
#                      'caret', 'caTools','tidytext',
#                      'widyr','ggraph', 'ggforce',
#                      'igraph','syuzhet','igraph', 
#                      'shinycssloaders', 'wordcloud2',
#                      'shiny', 'shinythemes')
# for (i in packages_needed){
#   if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org", quiet=TRUE)
#   require(i, character.only=TRUE)
# }



#chunk_into_sentences######################################################
# input: review column
# output: sublists of review, split in to sentences .!?,<>
chunk_into_sentences <- function(text) {
  break_points <- c(1, as.numeric(gregexpr('[[:alnum:]][.!?,<>()]', text)[[1]]) + 1)
  sentences <- NULL
  for(i in 1:length(break_points)) {
    res <- substr(text, break_points[i], break_points[i+1]) 
    if(i>1) { sentences[i] <- sub('.', '', res) } else { sentences[i] <- res }
  }
  sentences <- sentences[sentences=!is.na(sentences)]
  if(length(break_points)==2) {sentences<-text}
  return(sentences)
}
# https://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences


#cleanup_function_before######################################################
# input: Splitted review, list of items to delete the sentence before the keyword (however, but)
# output: Review list without the sentences before the keyword
cleanup_function_before <- function(this_review_splited, list_delete_before){
  text <- tolower(this_review_splited)
  text <- str_replace(text, "[.!?,]", "")
  logical_list<-rep(0, length(text))
  for (i in 1:length(list_delete_before)){
    m <- grepl(list_delete_before[i], text)
    logical_list<-logical_list|m
  }
  return_review <-this_review_splited[!lead(logical_list,default = FALSE)]
  # return_review <- paste(return_review, collapse = ' ') # combine sentences list into one review
  return(return_review)
}


#cleanup_function_after######################################################
# input: Splitted review, list of items to delete the sentence after the keyword (despite, in spite of)
# output: Review list without the sentences after the keyword
cleanup_function_after <- function(this_review_splited, list_delete_after){
  text <- tolower(this_review_splited)
  text <- str_replace(text, "[.!?,]", "")
  logical_list<-rep(0, length(text))
  for (i in 1:length(list_delete_after)){
    m <- grepl(list_delete_after[i], text)
    logical_list<-logical_list|m
  }
  return_review <-this_review_splited[!lag(logical_list,default = FALSE)]
  # return_review <- paste(return_review, collapse = ' ')# combine sentences list into one review
  return(return_review)
}

#cleanup_function_this_sentence######################################################
# input: Splitted review, list of items to delete the sentence if the keyword occurs (while)
# output: Review list without the sentences if the keyword occurs in the sentence
cleanup_function_this_sentence <- function(this_review_splited, list_delete_this_sentence){
  text <- tolower(this_review_splited)
  text <- str_replace(text, "[.!?,]", "")
  logical_list<-rep(0, length(text))
  for (i in 1:length(list_delete_this_sentence)){
    m <- grepl(list_delete_this_sentence[i], text)
    logical_list<-logical_list|m
  }
  return_review <-this_review_splited[!logical_list]
  # return_review <- paste(return_review, collapse = ' ')# combine sentences list into one review
  return(return_review)
}


#func_replace_emoji######################################################
# input: Splitted review, table to replace the emoticons with the emotion description
# output: Review list with emotion description instead of emoticons
func_replace_emoji<- function(text, emoji_table){
  clean_text <-stri_replace_all_fixed(text,
                                      pattern = emoji_table$emoji.chars,
                                      replacement = paste(emoji_table$emoji.descriptions," "),
                                      vectorize_all=FALSE)
  return(clean_text)
}


#elongated_words2######################################################
# input: Single word
# output: Boolean value if a word is elongated or not (Eg: Aweeesomeeee)
# Rule: A character coming together more than twice
elongated_words2 <- function(my_str){
  temp <- strsplit(my_str, "")[[1]]
  
  for(i in 1:length(temp)){
    count = 1
    if(i+1 <= length(temp)){
      for(j in (i+1):length(temp)){
        if(temp[i] == temp[j]){
          count = count + 1
          if(count>=3){
            return(T)
          }
        } else{
          break
        }
      }
    }
    
  }
  return(F)
  
}

#clean_elongated_words2######################################################
# input: Single word
# output: Clean elongated word (Eg: Aweeeeesome will become Aweesome)
# Rule: Reduce elongations with 2 character repetitions together only
clean_elongated_words2 <- function(my_str){
  temp <- strsplit(my_str, "")[[1]]
  my_word <- NA
  for(i in 1:length(temp)){
    if(is.na(my_word)){
      my_word <- temp[i]
    }
    if((i+1) <= length(temp)){
      if(temp[i] != temp[i+1]){
        my_word <- paste(c(my_word, temp[i+1]), collapse = "")
      } else if((i-1) >= 1){
        if(temp[i] != temp[i-1]){
          my_word <- paste(c(my_word, temp[i+1]), collapse = "")
        }
      }
      
    }
    
  }
  return(my_word)
  
}


#correct######################################################
# input: Single word
# output: Correct the spelling from dictionary (Eg: Aweesome will become Awesome)
# Rule: Closest distant word
correct <- function(word) {
  
  word = tolower(word)
  edit_dist <- adist(word, wordlist)
  c(wordlist[edit_dist <= min(edit_dist,2)],word)[1]
}


#clean_words######################################################
# input: Words in the review
# output: Rating value written in review comment on 10 (Eg: 3/5 will return 6)
clean_words <- function(words){
  
  num1 <- numeric(length(words))
  num2 <- numeric(length(words))
  
  for(j in 1:length(words)){
    words[j] <- gsub("[a-z]|[A-Z]", "" ,words[j])
    words[j] <- gsub("\\,", "." ,words[j])
    
    while((!grepl("^[0-9]",words[j])) | (!grepl("[0-9]$",words[j]))){
      words[j] <- gsub("^[[:punct:]]", "" ,words[j])
      words[j] <- gsub("[[:punct:]]$", "" ,words[j])
    }
    
    temp1 <- unlist(strsplit(words[j],'/'))[1]
    temp2 <- unlist(strsplit(words[j],'/'))[2]
    
    
    if(suppressWarnings(!is.na(as.numeric(temp1)))){
      num1[j] <- as.numeric(temp1)
    } else{
      
      temp1 <- unlist(strsplit(temp1,'&'))
      temp1 <- unlist(strsplit(temp1,'-'))
      temp1 <- unlist(strsplit(temp1,'$'))
      temp1 <- unlist(strsplit(temp1,'@'))
      temp1 <- unlist(strsplit(temp1,'!'))
      temp1 <- unlist(strsplit(temp1,'%'))
      temp1 <- unlist(strsplit(temp1,'\\('))
      temp1 <- unlist(strsplit(temp1,'\\+'))
      temp1 <- unlist(strsplit(temp1,'\\)'))
      temp1 <- unlist(strsplit(temp1,'\\*'))
      num1[j] <- mean(as.numeric(temp1))
    }
    
    if(suppressWarnings(!is.na(as.numeric(temp2)))){
      num2[j] <- as.numeric(temp2)
    } else{
      
      temp2 <- substr(temp2, 1, 2)
      num2[j] <- ifelse(temp2 != "10", as.numeric(substr(temp2,1,1)), as.numeric(temp2))
    }
  }
  
  res <- sum(num1)*10/sum(num2)
  
  if(res > 10 | is.nan(res)){
    return(NA)
  } else{
    return(res)
  }
  
}

#IsWordCap######################################################
# input: Single word
# output: Boolean value for capital words (Eg: COOL will return True)
IsWordCap<-function(word){
  if (word==toupper(word) & is.na(as.numeric(word)) & nchar(word)>=2 )
  {
    out<-TRUE
  }
  else {out<-FALSE}
  return(out)
}

#CountWordsCap######################################################
# input: Whole review
# output: Count all capital words (Eg: The movie was COOL and FABULOUS will return 2)
CountWordsCap<-function(review){
  review_clean<- str_replace_all(review, "[^[:alnum:]]", " ")
  review_split <- str_split(review_clean, boundary("word"))
  tf_vector<-unlist(sapply(unlist(review_split), IsWordCap))
  n<-sum(tf_vector)
  return(n)
}

#LoadToEnvironment######################################################
# input: RData to be loaded, in the new environment
# output: env with Rdata
LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}


#clean_reviews######################################################
# input: Review data with text column for reviews
# output: Multiple features added to the data as follow:
# elongated_words_freq - Elongated word frequency
# rating_words_value - Rating of movie written in the review comment
# elongated_sentiment - Average intensified valence for the elongated words
# exclamed_sentiment - Average intensified valence for the exclaimed words
# is_elongated - Boolean (If the review has elongated words)
# is_exclaimed - Boolean (If the review has exclaimed words)
# capital_freq - Capital words frequency
# is_capital - Boolean (If the review has capital words)
clean_reviews <- function(data, wordlist){
  
  mycorpus <- VCorpus(VectorSource(data$text))
  corpus_frame <- data.frame(text=unlist(sapply(mycorpus, `[`, "content")), stringsAsFactors=F)
  
  list_delete_before <- c("\\<however\\>","\\<but\\>")
  
  list_delete_after <- c("\\<despite\\>","\\<in spite of\\>")
  
  list_delete_this_sentence <- c("\\<while\\>")
  
  # replace emoji characters with their descriptions
  # https://unicode.org/Public/emoji/12.0/emoji-test.txt
  emoji_table<- read.csv("./IMDb_Sentiment_Analysis/Data/emoji_table.csv", header = TRUE)
  
  #run functions################################################################
  raw_review <- corpus_frame[,1]
  corpus_frame_split <- lapply(raw_review,chunk_into_sentences)
  corpus_frame_split <- lapply(corpus_frame_split,cleanup_function_before, list_delete_before)
  corpus_frame_split <- lapply(corpus_frame_split,cleanup_function_after, list_delete_after)
  corpus_frame_split <- lapply(corpus_frame_split,cleanup_function_this_sentence, list_delete_this_sentence)
  #combine sublists (sentences) back into one element per review
  clean_review <- lapply(corpus_frame_split, paste, collapse = ' ')
  #replace emoji
  clean_review <- lapply(clean_review, func_replace_emoji, emoji_table)
  #save output file
  mycorpus_clean <- VCorpus(VectorSource(clean_review))
  corpus_frame_clean <- data.frame(text=unlist(sapply(mycorpus_clean, `[`, "content")), stringsAsFactors=F)
  
  corpus_frame_clean$ratings <- data$ratings
  #corpus_frame_clean$sentiment <- data$count_pos
  #corpus_frame_clean$sentiment <- NULL
  
  corpus_frame_clean$elongated_words <- NA
  corpus_frame_clean$cleaned_elongated_words <- NA
  corpus_frame_clean$elongated_words_freq <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    words <- unlist(strsplit(corpus_frame_clean$text[i],' '))
    
    # Removes punctuations
    words = gsub("[[:punct:]]", " ", words)
    # Removes numbers
    words = gsub("[[:digit:]]", " ", words)
    
    words = gsub("\\s+"," ",words)
    
    words <- unlist(strsplit(words,' '))
    
    words <- sapply(words, elongated_words2)
    words <- names(words)[words == T]
    
    words <- tolower(words)
    
    cleaned_elongated_words <- sapply(words, clean_elongated_words2)
    
    #Concatenate back to a string
    corpus_frame_clean$elongated_words[i] <-
      paste(words, collapse=" ")
    
    corpus_frame_clean$cleaned_elongated_words[i] <-
      paste(cleaned_elongated_words, collapse=" ")
    
    corpus_frame_clean$elongated_words_freq[i] <-
      length(words)
  }
  
  corpus_frame_clean$Spelled_elongated_words <- NA
  
  
  for (i in 1:length(corpus_frame_clean$cleaned_elongated_words)){
    words <- unlist(strsplit(corpus_frame_clean$cleaned_elongated_words[i],' '))
    words <- as.character(sapply(words,correct))
    
    #Concatenate back to a string
    corpus_frame_clean$Spelled_elongated_words[i] <-
      paste(words, collapse=" ")
  }
  
  
  corpus_frame_clean$rating_words <- NA
  corpus_frame_clean$rating_words_value <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    
    if(grepl("+[0-9]/[0-9]+", corpus_frame_clean$text[i])){
      words <- unlist(strsplit(corpus_frame_clean$text[i],' '))
      words <- words[grepl("*[0-9]/[0-9]*", words)]
      
      corpus_frame_clean$rating_words_value[i] <- clean_words(words)
      
      corpus_frame_clean$rating_words[i] <-
        paste(words, collapse=" ")
      
    }
    
    if(grepl("+[0-9] out of [0-9]+", corpus_frame_clean$text[i]
             ,ignore.case = T)){
      
      body_comment <- gsub(" out of ","/", corpus_frame_clean$text[i])
      
      words <- unlist(strsplit(body_comment,' '))
      words <- words[grepl("*[0-9]/[0-9]*", words)]
      
      corpus_frame_clean$rating_words_value[i] <- clean_words(words)
      
      corpus_frame_clean$rating_words[i] <-
        paste(words, collapse=" ")
      
    }
    
    
    if(grepl("+(two|three|four|five|six|seven|eight|nine|ten) out of (two|three|four|five|six|seven|eight|nine|ten)+", corpus_frame_clean$text[i]
             ,ignore.case = T)){
      
      body_comment <- gsub(" out of ","/", corpus_frame_clean$text[i])
      body_comment <- gsub("one","1", body_comment, ignore.case = T)
      body_comment <- gsub("two","2", body_comment, ignore.case = T)
      body_comment <- gsub("three","3", body_comment, ignore.case = T)
      body_comment <- gsub("four","4", body_comment, ignore.case = T)
      body_comment <- gsub("five","5", body_comment, ignore.case = T)
      body_comment <- gsub("six","6", body_comment, ignore.case = T)
      body_comment <- gsub("seven","7", body_comment, ignore.case = T)
      body_comment <- gsub("eight","8", body_comment, ignore.case = T)
      body_comment <- gsub("nine","9", body_comment, ignore.case = T)
      body_comment <- gsub("ten","10", body_comment, ignore.case = T)
      
      words <- unlist(strsplit(body_comment,' '))
      words <- words[grepl("*[0-9]/[0-9]*", words)]
      
      corpus_frame_clean$rating_words_value[i] <- clean_words(words)
      
      corpus_frame_clean$rating_words[i] <-
        paste(words, collapse=" ")
      
    }
    
    
  }
  
  
  corpus_frame_clean$exclamated_sentences <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    
    sentences <- unlist(strsplit(corpus_frame_clean$text[i],'\\.'))
    
    sentences <- gsub("([!\\s])\\1+", "\\1", sentences, perl=TRUE)
    
    sentences <- unlist(strsplit(sentences,'(?<=[!])', perl = T))
    
    sentences <- sentences[grepl("+\\!+",sentences)]
    
    corpus_frame_clean$exclamated_sentences[i] <-
      paste(sentences, collapse=" .")
    
  }
  
  CommentsText <- sapply(corpus_frame_clean$exclamated_sentences
                         ,function(x) iconv(x, 'utf8', 'ascii',""))
  
  text <- tolower(CommentsText)
  
  ud_model <- udpipe_load_model("./IMDb_Sentiment_Analysis/Data/english-ewt-ud-2.3-181115.udpipe")
  
  pos_tokens <- udpipe_annotate(ud_model, x =text)
  pos_tokens <- as.data.frame(pos_tokens)
  pos_tokens <- subset(pos_tokens, pos_tokens$upos %in% c("ADJ", "VERB"))
  
  pos_tokens$doc_id <- gsub("doc","", pos_tokens$doc_id)
  pos_tokens$doc_id <- as.numeric(pos_tokens$doc_id)
  
  corpus_frame_clean$exclamated_words <- ""
  
  for(i in unique(pos_tokens$doc_id)){
    
    words <- subset(pos_tokens$lemma, pos_tokens$doc_id == i)
    
    corpus_frame_clean$exclamated_words[i] <-
      paste(words, collapse=" ")
  }
  
  
  
  
  dictionary <- read.csv("./IMDb_Sentiment_Analysis/Data/SentimentDictionary.csv")
  dictionary[,2:4] <- sapply(dictionary[,2:4],function(x) x-5)
  
  corpus_frame_clean$elongated_sentiment <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    
    if(corpus_frame_clean$Spelled_elongated_words[i] != ""){
      
      split <- strsplit(corpus_frame_clean$Spelled_elongated_words[i],split=" ")[[1]] 
      
      m <- match(split, dictionary$Word)
      
      present <- !is.na(m)
      
      wordvalences <- dictionary$VALENCE[m[present]] * 2
      
      corpus_frame_clean$elongated_sentiment[i] <- mean(wordvalences, na.rm=TRUE)
      
      if (is.na(corpus_frame_clean$elongated_sentiment[i])) 
        corpus_frame_clean$elongated_sentiment[i] <- 0 
      else corpus_frame_clean$elongated_sentiment[i] <- corpus_frame_clean$elongated_sentiment[i]
    }
  }
  
  
  corpus_frame_clean$exclamed_sentiment <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    
    if(corpus_frame_clean$exclamated_words[i] != ""){
      
      split <- strsplit(corpus_frame_clean$exclamated_words[i],split=" ")[[1]] 
      
      m <- match(split, dictionary$Word)
      
      present <- !is.na(m)
      
      wordvalences <- dictionary$VALENCE[m[present]] * 2
      
      corpus_frame_clean$exclamed_sentiment[i] <- mean(wordvalences, na.rm=TRUE)
      
      if (is.na(corpus_frame_clean$exclamed_sentiment[i])) 
        corpus_frame_clean$exclamed_sentiment[i] <- 0 
      else corpus_frame_clean$exclamed_sentiment[i] <- corpus_frame_clean$exclamed_sentiment[i]
    }
  }
  
  
  corpus_frame_clean$elongated_words <- NULL
  corpus_frame_clean$cleaned_elongated_words <- NULL
  corpus_frame_clean$Spelled_elongated_words <- NULL
  corpus_frame_clean$rating_words <- NULL
  corpus_frame_clean$exclamated_sentences <- NULL
  corpus_frame_clean$exclamated_words <- NULL
  corpus_frame_clean$ratings <- NULL
  
  corpus_frame_clean$is_elongated <- ifelse(corpus_frame_clean$elongated_words_freq > 0, 1, 0)
  #corpus_frame_clean$is_rated <- ifelse(!is.na(corpus_frame_clean$rating_words_value),1, 0)
  corpus_frame_clean$is_exclaimed <- ifelse(!is.na(corpus_frame_clean$exclamed_sentiment),1, 0)
  
  corpus_frame_clean[is.na(corpus_frame_clean)] <- 0
  
  corpus_frame_clean <- corpus_frame_clean[!is.na(corpus_frame_clean$text),]
  
  review_comments<-corpus_frame_clean$text
  FreqCapWords<-sapply(review_comments,CountWordsCap)
  
  corpus_frame_clean$capital_freq <- as.vector(FreqCapWords)
  corpus_frame_clean$is_capital <- ifelse(corpus_frame_clean$capital_freq > 0, 1, 0)
  
  return(corpus_frame_clean)
  
}

raw_data <- read.csv("./IMDb_Sentiment_Analysis/Data/all_reviews_movie_0-1866.csv", header = TRUE, stringsAsFactors = F)

data <- data.frame(text = raw_data$comments_body)

load("./IMDb_Sentiment_Analysis/Data/wordListSpelling.Rdata")

data <- clean_reviews(data, wordlist)

data <- cbind(data, sentiment = raw_data$count_pos)

write.csv(data, file = "./IMDb_Sentiment_Analysis/Data/corpus_preprocessed.csv", row.names = F)






#
# This script is to run the baseline model.
# input: all_reviews.csv
# output: trained Random Forest model with AUC score
########################################################################


#load required packages##################################################
setwd("D:/webscraping project/code")
packages_needed <- c('SnowballC','slam','tm',
                     'RWeka','Matrix','rvest',
                     'XML','stringr','stringi',
                     'dplyr','tidyverse')


for (i in packages_needed){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org", quiet=TRUE)
  require(i, character.only=TRUE)
}
### Step 1:load input data ######
data = read.csv("all_reviews_movie_0-1866.csv", header = TRUE)


# First we will delete non-recognizable characters, otherwise the tm package will get in trouble later. 
CommentsText <- sapply(data$comments_body,function(x) iconv(x, 'utf8', 'ascii',""))
comments <- VCorpus(VectorSource(CommentsText))


### Step 2: Pre-processing ######


# We can apply these transformer functions to the entire corpus using a 'mapper', tm_map


comments <- tm_map(comments, removePunctuation)
comments <- tm_map(comments, removeNumbers)
comments <- tm_map(comments, stripWhitespace)


# Create new transformation functions
# we can create new tranformation functions using the content_transformer wrapper


comments <- tm_map(comments,content_transformer(tolower))


#remove stopwords
forremoval <- stopwords('english')


# remove stop words but keep ("no","not","nor")
comments <- tm_map(comments, removeWords,c(forremoval[!forremoval %in% c("no","not","nor")])) 


comments_frame <- data.frame(text=unlist(sapply(comments, `[`, "content")), stringsAsFactors=F)


### Step 3: Structure and apply ML model ######


# 1. create a complete data set
SentimentReal<-data$count_pos
head(SentimentReal) 


# 2. Create a training and test set


set.seed(2) # Set a seed to have the same subsets every time 


# Set sample (stratified)
# Make our dependent variable dichotomous
y <- as.factor(SentimentReal)


levels(y)


# Define proportion to be in training set 
p <- 0.5


# Define observations to be in training set
class1_train <- sample(which(y==as.integer(levels(y)[1])), floor(p*table(y)[1]),replace=FALSE)
class2_train <- sample(which(y==as.integer(levels(y)[2])), floor(p*table(y)[2]),replace=FALSE)


training_locations <- c(class1_train,class2_train) 


# Create the training and test set now
# Store them in a list for easiness


txt_l <- list()
txt_l[[2]] <- list()


txt_l[[1]]<- comments_frame[sort(training_locations),1]
txt_l[[2]]<- comments_frame[-sort(training_locations),1]




# Make our training and test set corpora
for (i in 1:2){
  txt_l[[i]] <- VCorpus(VectorSource((txt_l[[i]])))
}




# 3. Create the term-document matrices
# Remember that we create two different matrices
# the variables of the test matrix should be the same as for the training matrix. 


# create a function that allows to make the training and test set correctly, with the n-grams specified
Ngram <- function(inputset1,inputset2,mindegree,maxdegree){
  # inputset1 = training dataset
  # inputset2 = test dataset
  # mindegree = minimum n-gram
  # maxdegree = maximum n-gram
  
  outputlist <- list()
  
  # training
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = mindegree, max = maxdegree))
  tr <- DocumentTermMatrix(inputset1, control = list(tokenize = Tokenizer,
                                                     weighting = function(x) weightTf(x),
                                                     RemoveNumbers=TRUE,
                                                     removePunctuation=TRUE,
                                                     stripWhitespace= TRUE))
  # test
  test <- DocumentTermMatrix(inputset2, control = list(tokenize = Tokenizer,
                                                       weighting = function(x) weightTf(x),
                                                       RemoveNumbers=TRUE,
                                                       removePunctuation=TRUE,
                                                       stripWhitespace= TRUE))
  
  # Apply sparseness reduction 
  # also reducing the number of documents (respondents) because there will be rows which will not have values anymore
  
  #tr <- removeSparseTerms(tr,0.9999)
  
  # Reform the test DTM to have the same terms as the training case 
  # Remember that, because we will use a model, that our test set should contain the same elements as our training dataset
  Intersect <- test[,intersect(colnames(test), colnames(tr))]
  diffCol <- tr[,setdiff(colnames(tr),colnames(test))]
  newCols <- as.simple_triplet_matrix(matrix(0,nrow=test$nrow,ncol=diffCol$ncol))
  newCols$dimnames <- diffCol$dimnames
  testNew<-cbind(Intersect,newCols)
  testNew<- testNew[,colnames(tr)]
  
  ## Convert term document matrices to common sparse matrices to apply efficient SVD algorithm
  
  dtm.to.sm <- function(dtm) {sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))}
  
  outputlist<- list(train=dtm.to.sm(tr),test=dtm.to.sm(testNew))
  
  return(outputlist)
}


# apply our function
# we store this in a new list, unigram


unigram <-Ngram(txt_l[[1]],txt_l[[2]],1,1)


# 4. Apply Singular Value Decomposition
# SVD will help to reduce this to a selected number of terms
# Note that we implemented an approximation with the package irlba, since the 'normal' svd gets stuck with very large datasets


if (!require("irlba")) install.packages("irlba", quiet=TRUE) ; require("irlba")


SVD_all <- function(inputset,k){
  outputlist <- list()
  
  outputlist[[i]]<-list()
  
  ### specify 'k' concepts, apply to training dataset
  trainer <- irlba(t(inputset[[1]]), nu=k, nv=k)
  
  ### apply the same training concepts to testing dataset
  tester <- as.data.frame(as.matrix(inputset[[2]] %*% trainer$u %*%  solve(diag(trainer$d))))
  
  outputlist<- list(train = as.data.frame(trainer$v), test= tester)
  
  return(outputlist)
}


# select k=20 for SVD
svdUnigram <- SVD_all(unigram,20)


# 5. Prediction models


# Create datasets to use: append our dependent variable to our dataset 


train  <- cbind(y[sort(training_locations)],svdUnigram[[1]])
test <- cbind(y[-sort(training_locations)],svdUnigram[[2]])


### Apply Random Forest model ######


if (!require("randomForest")) install.packages("randomForest", quiet=TRUE) ; require("randomForest")


### use ntree 1001 (not even number) so it can make decision 1 or 0
RF_model_train <- randomForest(x=train[,2:dim(train)[[2]]],y=train[,1],importance=TRUE,ntree=1001)
RF_predict <- predict(RF_model_train,test[,2:dim(test)[[2]]],type = "prob")[,2]
# This returns the probabilities, which is more useful for the evaluation measures


# Calculate auc
if (!require("ROCR")) install.packages("ROCR", quiet=TRUE) ; require("ROCR")


predML <- prediction(RF_predict,test[,1])


# ROC curve
perfML <- performance(predML,"tpr","fpr")
plot(perfML)
abline(0,1)


## auc
auc.perfML = performance(predML, measure = "auc")
auc.perfML@y.values


if (!require("caret")) install.packages("caret", quiet=TRUE) ; require("caret")
if (!require("e1071")) install.packages("e1071", quiet=TRUE) ; require("e1071")
pred <- as.factor(ifelse(RF_predict>0.5,1,0))
confusionMatrix(pred, test[,1])




