library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)


consumer_key <-"ISXlzWCr8XdlTu4UIuhjrIFD"
consumer_secret<-"2mNHlZ2jQ37BKgZhuhbiCHc34549GCyEgqbgLZ2B2dfPffGA7k"
access_token<-"gAH079KYYvVh5E2k10DFRVOmxh2HwmzlYRyazErE"
access_secret<-"A5nxpy0NXa3rw9pGf1uYA5O4EyhCxQzzj6Eqdm3oS2XzB"


setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)


 candidates <- c("Hilary Clinton","Ted Cruz","Donald Trump")

 tweetsdf <- data.frame(tweets = character(),candidate=character())

 for(i in seq(candidates)) { 
       
    notweets <- 10000
		print(candidates[i])
	
			tweets <-	searchTwitter(candidates[i], n=notweets )
			tweets.text <- lapply(tweets, function(t) t$getText() )
			
		candidatetw <-data.frame(tweets = matrix(unlist(tweets.text), nrow=notweets , byrow=T))
		candidatetw$candidate <-  candidates[i]	
			tweetsdf <- rbind(tweetsdf,candidatetw)
					
				}
hu.liu.pos = scan('positive-words.txt',what='character', comment.char=';')

hu.liu.neg = scan('negative-words.txt',what='character', comment.char=';')
pos.words = c(hu.liu.pos, 'upgrade')

neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting',
'epicfail', 'mechanical')


score = function(sentence,pos.words,neg.words)
{

sentence = gsub('[[:punct:]]', '', sentence)
sentence = gsub('[[:cntrl:]]', '', sentence)
sentence = gsub('\\d+', '', sentence)
sentence  = iconv(sentence , "latin1", "ASCII", sub="")
# and convert to lower case:
sentence = tolower(sentence)


# split into words. str_split is in the stringr package
word.list = str_split(sentence, '\\s+')
# sometimes a list() is one level of hierarchy too much
words = unlist(word.list)

# compare our words to the dictionaries of positive & negative terms
pos.matches = match(words, pos.words)
neg.matches = match(words, neg.words)

# match() returns the position of the matched term or NA
# we just want a TRUE/FALSE:
pos.matches = !is.na(pos.matches)
neg.matches = !is.na(neg.matches)

# and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
score = sum(pos.matches) - sum(neg.matches)

return(score)




}

tweetsdf$score <- lapply(tweetsdf$tweets,score,pos.words,neg.words)

g = ggplot(data=tweetsdf, mapping=aes(x=score, fill=candidate) )
g = g + geom_histogram(binwidth=1)
g = g + facet_grid(candidate~.)
g = g + theme_bw() + scale_fill_brewer()



