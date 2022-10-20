##########################################
## A3 Assignment
## UFP Observation 
## Takahiro Yamada
## 5.Dec.2021
##########################################

#install.packages("igraph")
#install.packages("ggraph")
#install.packages("pdftools")
library(pdftools) 
library(tidytuesdayR)
library(dplyr)
library(tidytext)
library(tidyr)
library(stringr)
library(scales)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)
library(tidyverse)
library(topicmodels)


##########################################
## Data massage
##########################################
ufo <- read.csv("/Users/takahiroyamada/Desktop/MBAN/20211116 Text Analysis/A3 Assingment/scrubbed.csv") %>%
  rename(text=comments) 
colnames(ufo)[8] <- "text"
ufo$datetime <-strptime(ufo$datetime, "%m/%d/%Y %H:%M")
ufo <- ufo %>% separate(datetime, c("date", "time"), sep=" ") 
ufo <- ufo %>% separate(date, c("year", "month", "day"), sep="-") 
ufo$year <- as.numeric(ufo$year)
ufo$longitude <- as.numeric(ufo$longitude)
ufo$latitude <- as.numeric(ufo$latitude)

hist(ufo$year,
     main = "UFO Observation",
     xlim=c(1940,2020),
     breaks = 20)

sapply(ufo, function(x) sum(is.na(x)))
table(ufo$country)
table(ufo$state)
table(ufo$shape)

stop_words <- get_stopwords()
custom_lex <- data_frame(word=c("44","uk","u.k"), lexicon=rep("SMART", each=1))
stop_words <- rbind(stop_words, custom_lex)

city_name <- ufo %>% separate(city, c("city1", "city2"), sep = " ") %>% select(city1,city2)
custom_lex_city_name1 <- data_frame(word=city_name$city1, lexicon=rep("SMART", each=80332))
custom_lex_city_name2 <- data_frame(word=city_name$city2, lexicon=rep("SMART", each=80332))
custom_lex_state <- data_frame(word=ufo$state, lexicon=rep("SMART", each=80332))
stop_words_city_name <- rbind(stop_words, custom_lex, custom_lex_city_name1, custom_lex_city_name2, custom_lex_state)


##########################################
## Overview World / US
##########################################
ufo_w <- ufo %>% 
  select(country) %>% 
  count(country) %>% 
  mutate(country = reorder(country ,n ))

ggplot(ufo_w ,aes(country, n))+
  geom_col()+
  coord_flip()

ufo_us <- ufo %>% 
  select(state) %>% 
  count(state) %>% 
  mutate(state = reorder(state ,n )) %>% 
  filter(n>500)

ggplot(ufo_us ,aes(state, n))+
  geom_col()+
  coord_flip()

##########################################
## Tidy countries
##########################################
tidy_us <- ufo %>%
  filter(country== "us") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_gb <- ufo %>%
  filter(country== "gb") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_ca <- ufo %>%
  filter(country== "ca") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_au <- ufo %>%
  filter(country== "au") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


##########################################
## Frequent words world / us/ uk/ gb/ ca/ au
##########################################
tidy_ufo <- ufo %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>% 
  filter(n>5000) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  ggtitle("Frequent Word World UFO Report")+
  xlab(NULL)+
  coord_flip()
print(tidy_ufo)

tidy_us_fr <- tidy_us %>% 
  count(word, sort = T) %>% 
  filter(n>5000) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  ggtitle("Frequent Word U.S UFO Report")+
  xlab(NULL)+
  coord_flip()
print(tidy_us_fr)

tidy_gb_fr <- tidy_gb %>% 
  count(word, sort = T) %>% 
  filter(n>150) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  ggtitle("Frequent Word U.K UFO Report")+
  xlab(NULL)+
  coord_flip()
print(tidy_gb_fr)

tidy_ca_fr <- tidy_ca %>%   
  count(word, sort = T) %>% 
  filter(n>200) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  ggtitle("Frequent Word Canada UFO Report")+
  xlab(NULL)+
  coord_flip()
print(tidy_ca_fr)

tidy_au_fr <- tidy_au %>% 
  count(word, sort = T) %>% 
  filter(n>40) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  ggtitle("Frequent Word Australia UFO Report")+
  xlab(NULL)+
  coord_flip()
print(tidy_au_fr)

##########################################
## Country correlation
##########################################
frequency <- bind_rows(mutate(tidy_us, country="US"),
                       mutate(tidy_gb, country="UK"),
                       mutate(tidy_ca, country="Canada"),
                       mutate(tidy_au, country="Australia"))%>%
  mutate(word=str_extract(word, "[a-z']+")) %>% 
  count(country, word) %>%
  group_by(country) %>%
  mutate(proportion = n/sum(n))%>% 
  select(-n) %>% 
  spread(country, proportion) %>% 
  gather(country, proportion, `UK`, `Canada`, `Australia`)


ggplot(frequency, aes(x=proportion, y=`US`, 
                      color = abs(`US`- proportion)))+ 
  geom_abline(color="grey40", lty=2)+ 
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+ 
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) + 
  scale_x_log10(labels = percent_format())+ 
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~country, ncol=2)+ 
  theme(legend.position = "none")+ 
  labs(y= "US", x=NULL) 


cor.test(data=frequency[frequency$country == "UK",],
         ~proportion + `US`)

cor.test(data=frequency[frequency$country == "Canada",],
         ~proportion + `US`)

cor.test(data=frequency[frequency$country == "Australia",],
         ~proportion + `US`)

##########################################
## Sentiments
##########################################
nrc <- get_sentiments("nrc")

us_senti <- tidy_us %>% 
  inner_join(nrc) %>% 
  count(sentiment, sort = T) %>% 
  filter(n>100) %>% # we need this to eliminate all the low count words
  mutate(sentiment = reorder(sentiment,n )) %>%
  ggplot(aes(sentiment, n,))+
  geom_col()+
  ggtitle("US Senti")+
  xlab(NULL)+
  coord_flip()
print(us_senti)

gb_senti <- tidy_gb %>% 
  inner_join(nrc) %>% 
  count(sentiment, sort = T) %>% 
  filter(n>100) %>% # we need this to eliminate all the low count words
  mutate(sentiment = reorder(sentiment,n )) %>%
  ggplot(aes(sentiment, n))+
  geom_col()+
  ggtitle("UK Senti")+
  xlab(NULL)+
  coord_flip()
print(gb_senti)

ca_senti <- tidy_ca %>% 
  inner_join(nrc) %>% 
  count(sentiment, sort = T) %>% 
  filter(n>100) %>% # we need this to eliminate all the low count words
  mutate(sentiment = reorder(sentiment,n )) %>%
  ggplot(aes(sentiment, n))+
  geom_col()+
  ggtitle("Canada Senti")+
  xlab(NULL)+
  coord_flip()
print(ca_senti)

au_senti <- tidy_au %>% 
  inner_join(nrc) %>% 
  count(sentiment, sort = T) %>% 
  filter(n>100) %>% # we need this to eliminate all the low count words
  mutate(sentiment = reorder(sentiment,n )) %>%
  ggplot(aes(sentiment, n))+
  geom_col()+
  ggtitle("Australia Senti")+
  xlab(NULL)+
  coord_flip()
print(au_senti)

##########################################
## TF_IDF
##########################################
ufo_token <- ufo %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words_city_name) %>% 
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words <- ufo_token %>%
  group_by(country) 

ufo_words <- left_join(ufo_token, total_words)%>%
  filter(country %in% c("us", "gb", "ca", "au"))

country_words <- ufo_words %>%
  bind_tf_idf(word, country, n)

country_words # we get all the zeors because we are looking at stop words ... too common

uniqueness <- country_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical approach:
country_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(7) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()

##########################################
## n-gram
##########################################

ufo_quadgrams <- ufo %>%
  unnest_tokens(quadgram, text, token = "ngrams", n=4)

ufo_quadgrams #We want to see the bigrams (words that appear together, "pairs")

ufo_quadgrams %>%
  count(quadgram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
quadgrams_separated <- ufo_quadgrams %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")

quadgrams_filtered <- quadgrams_separated %>%
  filter(!word1 %in% stop_words_city_name$word) %>% # ! means "not" all without !
  filter(!word2 %in% stop_words_city_name$word) %>% 
  filter(!word3 %in% stop_words_city_name$word) %>% 
  filter(!word4 %in% stop_words_city_name$word)

#creating the new bigram, "no-stop-words":
quadgrams_counts <- quadgrams_filtered %>%
  count(word1, word2, word3, word4, sort = TRUE)
#want to see the new bigrams
quadgrams_counts

###
quadgrams_graph <- quadgrams_counts %>%
  filter(n>4) %>%
  graph_from_data_frame()

quadgrams_graph

ggraph(quadgrams_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=.5)


##########################################
## word correlation
##########################################

my_tidy_df <- ufo %>%
  filter(country == "us") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words_city_name$word)

word_cors <- my_tidy_df %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, city, sort=TRUE)

##########################################
## Creating barcharts for correlatoins
##########################################
word_cors %>%
  filter(item1 %in% c("color", "japanese", "note", "shape")) %>%
  group_by(item1) %>%
  top_n(7) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

##########################################
## creating a correlation network
##########################################
#this will take some time to run, we will need to wait for the result
# feel free to adjust the geom_node_point to somehting smaller

word_cors %>%
  filter(correlation >.6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()




####################################################################################
####################################################################################
## Roswell Report
####################################################################################
####################################################################################
setwd("/Users/takahiroyamada/Desktop/MBAN/20211116 Text Analysis/A3 Assingment/Roswell")
nm <- list.files(path="/Users/takahiroyamada/Desktop/MBAN/20211116 Text Analysis/A3 Assingment/Roswell")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x))) %>% t() 
colnames(my_pdf_text) <- c("text")
my_pdf <- data.frame(line=1:994, text=my_pdf_text)

custom_lex_ros <- data_frame(word=c("a","b","c","d","e","f","g","h","i","j","k","l",
                                "m","n","o","p","q","r","s","t","u","v",
                                "w","x","y","z"), lexicon=rep("SMART", each=26))
custom_lex_custom_r <- data_frame(word=my_pdf$line, lexicon=rep("SMART", each=994))
stop_words_custom_r <- rbind(stop_words,  my_pdf$line, custom_lex_ros, custom_lex_custom_r)


tidy_ros <- my_pdf %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words_custom_r)


##########################################
## Topic Analysis
##########################################
ros_dtm <- tidy_ros %>%
  count(line, word) %>%
  cast_dtm(line, word, n)

ros_dtm

#calling the Latent Dirichlet Allocation algorithm
ros_lda <- LDA(ros_dtm, k=2, control=list(seed=123))  ## k=2 - only 2 topics
ros_lda

#now we are looking for the per topic per word probabilities aka. beta
#beta - what is the probability that "this term" will be generated by "this topic"

ros_topics <- tidy(ros_lda, matrix="beta")
ros_topics

ros_top_terms <- ros_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ros_top_terms

#lets plot the term frequencies by topic
ros_top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#lets calculate the relative difference between the betas for words in topic 1
#and words in topic 2

ros_beta_spread <- ros_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>.001 | topic2 >.001) %>%
  mutate(log_rate = log2(topic2/topic1))   ## log rate shows the big diff between topic 1 and topic 2

ros_beta_spread


##########################################
## Word Frequency
##########################################
tidy_ros_fr <- tidy_ros %>% 
  count(word, sort = T) %>% 
  filter(n>200) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(tidy_ros_fr)


##########################################
## n-gram
##########################################
ros_quadgrams <- my_pdf %>%
  unnest_tokens(quadgram, text, token = "ngrams", n=4)

ros_quadgrams #We want to see the bigrams (words that appear together, "pairs")

ros_quadgrams %>%
  count(quadgram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
ros_quadgrams_separated <- ros_quadgrams %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")

ros_quadgrams_filtered <- ros_quadgrams_separated %>%
  filter(!word1 %in% stop_words_custom_r$word) %>% # ! means "not" all without !
  filter(!word2 %in% stop_words_custom_r$word) %>% 
  filter(!word3 %in% stop_words_custom_r$word) %>% 
  filter(!word4 %in% stop_words_custom_r$word)

#creating the new bigram, "no-stop-words":
ros_quadgrams_counts <- ros_quadgrams_filtered %>%
  count(word1, word2, word3, word4, sort = TRUE)
#want to see the new bigrams
ros_quadgrams_counts

#
ros_quadgrams_graph <- ros_quadgrams_counts %>%
  filter(n>3) %>%
  graph_from_data_frame()

ros_quadgrams_graph

ggraph(ros_quadgrams_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=.5)

##########################################
## Report vs Country correlation
##########################################
ros_frequency <- bind_rows(mutate(tidy_ros, country="Roswell"),
                           mutate(tidy_us, country="US"),
                           mutate(tidy_gb, country="UK"),
                           mutate(tidy_ca, country="Canada"),
                           mutate(tidy_au, country="Australia"))%>%
  mutate(word=str_extract(word, "[a-z']+")) %>% 
  count(country, word) %>% 
  group_by(country) %>%　
  mutate(proportion = n/sum(n))%>% 
  select(-n) %>%
  spread(country, proportion) %>% 
  gather(country, proportion, `US`, `UK`, `Canada`, `Australia`)


ggplot(ros_frequency, aes(x=proportion, y=`Roswell`, 
                          color = abs(`Roswell`- proportion)))+ 
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+ 
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) + 
  scale_x_log10(labels = percent_format())+ 
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+ 
  facet_wrap(~country, ncol=2)+ 
  theme(legend.position = "none")+ 
  labs(y= "Roswel", x=NULL) 

cor.test(data=ros_frequency[ros_frequency$country == "US",],
         ~proportion + `Roswell`)

cor.test(data=ros_frequency[ros_frequency$country == "UK",],
         ~proportion + `Roswell`)

cor.test(data=ros_frequency[ros_frequency$country == "Canada",],
         ~proportion + `Roswell`)

cor.test(data=ros_frequency[ros_frequency$country == "Australia",],
         ~proportion + `Roswell`)



