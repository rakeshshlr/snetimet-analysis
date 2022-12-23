library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(e1071)
library(mlbench)
library(ggplot2)
library(gridExtra)

#Text mining packages
library(tm)
library(SnowballC)
library("wordcloud")
library("RColorBrewer")

getwd()
setwd("C:/Users/rakes/Downloads")
King <- read.csv("King_Lear_words_and_players_only.csv")
str(King)
Lear_king%>%head(30)
str(King)
names(King)
nrow(King)
King <- tibble::rowid_to_column(King, "Line ID")
King%>%head(10)

custom_stop_words <- bind_rows(tibble(word = c("thou","thy","thee","tis"),  # add your own extra stop words
                                      lexicon = c("custom")), 
                               stop_words)

Lear_king<-King%>%
  unnest_tokens(word,text)%>%
  anti_join(custom_stop_words,"word")
Lear_king%>%head(10)

Lear_top_player<-Lear_king%>%count(player, sort = TRUE)%>%
  head(10)%>%
  ggplot(aes(x=reorder(player,n), y=n)) +
  geom_bar(stat="identity", show.legend=FALSE)+
  geom_text(aes(label=n), hjust=0.5,vjust=1, size=3, color="white")+
  scale_x_discrete(guide =  guide_axis(angle = 30))

lear_top_word<-Lear_king%>%count(word, sort = TRUE)%>%
  head(10)%>%
  ggplot(aes(x=reorder(word,n), y=n)) +
  geom_bar(stat="identity", show.legend=FALSE)+
  geom_text(aes(label=n), hjust=0.5,vjust=1, size=3, color="white")+
  scale_x_discrete(guide =  guide_axis(angle = 90))

Words_used<-Lear_king%>%
  filter(player==c("LEAR","EDGAR","KENT","GLOUCESTER","EDMUND"))%>%
  count(word,player, sort = TRUE)%>%
  head(20)%>%
  ggplot(aes(x=reorder(word,n), y=n)) +
  geom_bar(stat="identity",aes(fill=player), show.legend=TRUE)+
  geom_text(aes(label=n), hjust=0.5,vjust=-1, size=3, color="white")+
  scale_x_discrete(guide =  guide_axis(angle = 90))

grid.arrange(Lear_top_player,lear_top_word,Words_used, nrow=2)

Lear_king%>%
  filter(player==c("LEAR","EDGAR","KENT","GLOUCESTER","EDMUND"))%>%
  count(player, sort = TRUE)%>%
  mutate(player_per=(n/sum(n))*100)
####################################NRC######################################################4

get_sentiments("nrc")
  

lear_nrc<-King%>%
  unnest_tokens(word,text)%>%
  inner_join(get_sentiments("nrc"),"word")

lear_nrc%>%head(10)

lear_nrc%>%
  filter(player==c("LEAR","EDGAR","KENT","GLOUCESTER","EDMUND"))%>%
  count(sentiment,player, sort = TRUE)%>%
  ggplot(aes(x=reorder(sentiment,n), y=n)) +
  geom_bar(stat="identity",aes(fill=player), show.legend=TRUE)+
  scale_x_discrete(guide =  guide_axis(angle = 90))

novels_anno <-lear_nrc  %>%
  filter(player==c("LEAR","EDGAR","KENT","GLOUCESTER","EDMUND"))%>%
  dplyr::group_by(player) %>%
  dplyr::mutate(words = n()) %>%
  dplyr::left_join(tidytext::get_sentiments("nrc")) %>%
  dplyr::mutate(novel = factor(player),
                sentiment = factor(sentiment))
novels_anno
novels <- novels_anno  %>%
  dplyr::group_by(player) %>%
  dplyr::group_by(player, sentiment) %>%
  dplyr::summarise(sentiment = unique(sentiment),
                   sentiment_freq = n(),
                   words = unique(words)) %>%
  dplyr::filter(is.na(sentiment) == F) %>%
  dplyr::mutate(percentage = round(sentiment_freq/words*100, 1))
Player_NRc<-novels %>%
  ggplot(aes(player, percentage, fill = sentiment)) +    
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette = "RdBu") +
  theme_bw() +
  theme(legend.position = "right") +
  coord_flip()
sentiments <- King_untok %>% 
  inner_join(get_sentiments("nrc"), "word") %>%
  count(word, sentiment,player, sort=TRUE)

A3<-sentiments %>% filter(sentiment==c("positive","negative","trust","fear"))%>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Frequency", x="Terms") +
  coord_flip() +
  theme_bw() 
A3
grid.arrange(Player_NRc,A3, nrow=TRUE)
King_untok %>%
  inner_join(get_sentiments("nrc"), "word") %>%
  count(sentiment, sort=TRUE)%>%
  mutate(sentiment_percent=(n/sum(n))*100)

King_untok%>%
  inner_join(get_sentiments("nrc"), "word") %>%
  filter(sentiment=="positive")%>%
  count(word, sort=TRUE)%>%
  head(10)%>%
  mutate(word_percent=(n/sum(n))*100)


Afin_value<-King_untok%>% 
  filter(!word=="fool")%>%
  inner_join(get_sentiments("afinn"), "word") %>%
  count(`Line ID`,value, sort = TRUE)

R<-aggregate(Afin_value$value, by=list(Category=Afin_value$`Line ID`), FUN=sum, sort=TRUE)
mean(R$x)

King_untok%>%
  inner_join(get_sentiments("afinn"), "word")%>%
  filter(!word=="fool")%>%
  group_by(`Line ID`)%>%
  ggplot(aes(x=`Line ID`, y=value))+
  geom_line(stat="identity", col="blue") +
  geom_smooth(col="red") + 
  labs(title="Sentiment over entire play")

mean(R$x)

R%>%
  filter(Category>1000&Category<2000)%>%
  mutate(sum_line=mean(x))

R%>%
  filter(Category>2000&Category<3000)%>%
  mutate(sum_line=mean(x))
R%>%
  filter(Category<1000)%>%
  mutate(sum_line=mean(x))
R%>%
  filter(Category>3000)%>%
  mutate(sum_line=mean(x))


R%>%
  filter(Category<2000)%>%
  mutate(sum_line=mean(x))

p1 <- King_untok %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(!word=="fool")%>%
  filter(player==c("LEAR","EDGAR","KENT","EDGAR","GLOUCESTER","EDMUND"))%>%
  ggplot(aes(player, fill = sentiment)) +
  geom_bar(position = "fill")
p1



p3 <- King_untok %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(!word=="fool")%>%
  filter(player=="GLOUCESTER")%>%
  group_by(player, `Line ID`, sentiment) %>%
  count() %>%
  spread(sentiment, n, fill = 0) %>%
  group_by(player, `Line ID`) %>%
  summarise(neg = sum(negative),
            pos = sum(positive)) %>%
  arrange(`Line ID`) %>%
  mutate(frac_neg = neg/(neg + pos)) %>%
  ggplot(aes(frac_neg, fill = player)) +
  geom_density(bw = .2, alpha = 0.3) +
  theme(legend.position = "right") +
  labs(x = "Fraction of negative words per sentence")
p3
p4 <- King_untok %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(!word=="fool")%>%
  filter(player=="LEAR")%>%
  group_by(player, `Line ID`, sentiment) %>%
  count() %>%
  spread(sentiment, n, fill = 0) %>%
  group_by(player, `Line ID`) %>%
  summarise(neg = sum(negative),
            pos = sum(positive)) %>%
  arrange(`Line ID`) %>%
  mutate(frac_neg = neg/(neg + pos)) %>%
  ggplot(aes(frac_neg, fill = player)) +
  geom_density(bw = .2, alpha = 0.3) +
  theme(legend.position = "right") +
  labs(x = "Fraction of negative words per sentence")
p4
p5 <- King_untok %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(!word=="fool")%>%
  filter(player=="KENT")%>%
  group_by(player, `Line ID`, sentiment) %>%
  count() %>%
  spread(sentiment, n, fill = 0) %>%
  group_by(player, `Line ID`) %>%
  summarise(neg = sum(negative),
            pos = sum(positive)) %>%
  arrange(`Line ID`) %>%
  mutate(frac_neg = neg/(neg + pos)) %>%
  ggplot(aes(frac_neg, fill = player)) +
  geom_density(bw = .2, alpha = 0.3) +
  theme(legend.position = "right") +
  labs(x = "Fraction of negative words per sentence")
p5

p6 <- King_untok %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(!word=="fool")%>%
  filter(player=="EDGAR")%>%
  group_by(player, `Line ID`, sentiment) %>%
  count() %>%
  spread(sentiment, n, fill = 0) %>%
  group_by(player, `Line ID`) %>%
  summarise(neg = sum(negative),
            pos = sum(positive)) %>%
  arrange(`Line ID`) %>%
  mutate(frac_neg = neg/(neg + pos)) %>%
  ggplot(aes(frac_neg, fill = player)) +
  geom_density(bw = .2, alpha = 0.3) +
  theme(legend.position = "right") +
  labs(x = "Fraction of negative words per sentence")
p6
p7 <- King_untok %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(!word=="fool")%>%
  filter(player=="EDMUND")%>%
  group_by(player, `Line ID`, sentiment) %>%
  count() %>%
  spread(sentiment, n, fill = 0) %>%
  group_by(player, `Line ID`) %>%
  summarise(neg = sum(negative),
            pos = sum(positive)) %>%
  arrange(`Line ID`) %>%
  mutate(frac_neg = neg/(neg + pos)) %>%
  ggplot(aes(frac_neg, fill = player)) +
  geom_density(bw = .2, alpha = 0.3) +
  theme(legend.position = "right") +
  labs(x = "Fraction of negative words per sentence")
p7

p8 <- King_untok %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(!word=="fool")%>%
  group_by(player, `Line ID`, sentiment) %>%
  count() %>%
  spread(sentiment, n, fill = 0) %>%
  group_by(player, `Line ID`) %>%
  summarise(neg = sum(negative),
            pos = sum(positive)) %>%
  arrange(`Line ID`) %>%
  mutate(frac_neg = neg/(neg + pos)) %>%
  ggplot(aes(frac_neg)) +
  geom_density(bw = .2, alpha = 0.3) +
  labs(x = "Fraction of negative words per sentence")
p8

grid.arrange(p3,p4,p5,p6,p7,nrow=2)

