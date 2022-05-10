library(plyr)
library(stringr)


readAndflattenSentiWS <- function(filename) { 
  words = readLines(filename, encoding="UTF-8")
  words <- sub("\\|[A-Z]+\t[0-9.-]+\t?", ",", words)
  words <- unlist(strsplit(words, ","))
  words <- tolower(words)
  return(words)
}
pos.words <- c(scan("positive-words.txt",what='character', comment.char=';', quiet=T), 
               readAndflattenSentiWS("positive-words.txt"))
neg.words <- c(scan("negative-words.txt",what='character', comment.char=';', quiet=T), 
               readAndflattenSentiWS("negative-words.txt"))

score.sentiment = function(sentences, pos.words, neg.words, .progress='none') {
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) 
  {
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
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
    # I don't just want a TRUE/FALSE! How can I do this?
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, 
  pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores)
  return(scores.df)
}
tweet=read.csv("tweets.csv", header = False, sep = ",", quote = "\"",dec = ".", fill = TRUE)
Scores=dlply(tweet,-1,score.sentiment,pos.words,neg.words)
dput(Scores, file = "Scores.txt") 
