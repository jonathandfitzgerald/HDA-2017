#Working with Wright Data

#read in one Wright text

text = scan("data/wright-txt/VAC5884.txt",sep="\n",what="raw")

#tokenization (words per line)
text %>% 
  strsplit("[^A-Za-z]")

#unlist
words = text %>% 
  strsplit("[^A-Za-z]") %>% 
  unlist 

#make dataframe
wrightWords = data.frame(word=words,stringsAsFactors = "false")

#filter out blank cells
wrightWords = wrightWords %>% filter(word != "")


#word counts

wordcounts = wrightWords %>% group_by(word) %>% summarize(count=n()) %>% arrange(-count)
wordcounts

#concordances

#show how it works
numbers = c(1,2,3,4,5)
lag(numbers,1)
lead(numbers,1)

wrightWords %>% mutate(word2 = lead(word,1)) %>% head

#create concordance

multiColumn = wrightWords %>% mutate(word2 = lead(word,1),word3=lead(word,2),word4=lead(word,3))
multiColumn %>% head

#see a particular word

multiColumn %>% filter(word3=="sea")


#READ IN MULTIPLE

#make a list of all files
WRIGHT = list.files("data/wright-txt",full.names = TRUE)

#function to read them in
readWRIGHTtext = function(file) {
  message(file)
  text = paste(scan(file, sep="\n",what="raw",strip.white = TRUE),collapse = "/n")
  WRIGHT = data.frame(filename=file,text=text,stringsAsFactors = FALSE)  
  return(WRIGHT)
}

readWRIGHTwords = function(file) {
  message(file)
  text = paste(scan(file, sep="\n",what="raw",strip.white = TRUE),collapse = "/n")
  words = text %>% 
    strsplit("[^A-Za-z]") %>% 
    unlist
  SOTU = data.frame(word=words,filename=gsub("data/wright-txt/","",file),stringsAsFactors = FALSE)  
  return(SOTU)
}

#Working with allWRIGHTtext -- remove everything above Full Text
#run the function
allWRIGHTtext = data.frame(filename=WRIGHT,stringsAsFactors=FALSE) %>% 
  group_by(filename) %>% 
  do(readWRIGHTtext(.$filename))


#USE THIS FOR ASSIGNMENT
allWRIGHT$text = gsub("/n", "", allWRIGHT$text)
#this removes line breaks
allWRIGHT$text = gsub("\r?\n|\r", "\\s", allWRIGHT$text)
allWRIGHT$text = gsub(".*?----FULL TEXT----", " ", allWRIGHT$text)



#Working with allWRIGHTwords
allWRIGHTwords = data.frame(filename=WRIGHT,stringsAsFactors=FALSE) %>% 
  group_by(filename) %>% 
  do(readWRIGHTwords(.$filename))

allWRIGHTwords = allWRIGHTwords %>% filter(word != "")

#unique words
unique = allWRIGHTwords %>% 
  mutate(word=tolower(word)) %>%
  distinct(word) %>% 
  group_by(word) %>% filter(n()==1)


#probabilities

#use the concordance practice from above

allWRIGHTwords = allWRIGHTwords %>% mutate(word2=lead(word,1),word3=lead(word,2))

allWRIGHTwords = allWRIGHTwords[,c("filename","word","word2","word3")]

transitions = allWRIGHTwords %>% 
  group_by(word) %>% 
  mutate(word1Count=n()) %>% 
  group_by(word,word2) %>% 
  summarize(chance = n()/word1Count[1])

findNextWord = function(current) {
  subset = transitions %>% filter(word==current)
  nextWord = sample(subset$word2,1,prob = subset$chance)
}

#random walk (not working)

word = "I"
while(TRUE) {
  message(word)
  word = findNextWord(word)
}



#random walk to twitter
autoGenerate = function(previous,current) {
  previous = "It"
  current = "Was"
  wordlist = c()
  while(length(wordlist) < 3) {  
    message(previous)
    nextWord = findNextWord(previous,current)
    previous = current
    current = nextWord
    wordlist = c(wordlist,previous)
  }
}

wordlist


current = "I"
wordlist = c()
while(length(wordlist) < 20) {  
  nextWord = findNextWord(previous,current)
  previous = current
  current = nextWord
  wordlist = c(wordlist,previous)  
}

tweettext = paste(paste(wordlist, collapse=" "),"... #automaticDickens", collapse=" ")
tweettext


#tokenize Viral Vignettes & random walk

vtWords = vtClusters %>% mutate(words = text %>% strsplit("[^A-Za-z]") %>% unlist )

words = as.character(vtClusters$text) %>% 
  strsplit("[^A-Za-z]") %>% 
  unlist 

vtWords = data.frame(word=words,stringsAsFactors = "false")

vtWords = vtWords %>% filter(word != "")

vtWords = vtWords %>% mutate(word2=lead(word,1),word3=lead(word,2))

VTtransitions = vtWords %>% 
  group_by(word) %>% 
  mutate(word1Count=n()) %>% 
  group_by(word,word2) %>% 
  summarize(chance = n()/word1Count[1])

VTfindNextWord = function(current) {
  subset = VTtransitions %>% filter(word==current)
  nextWord = sample(subset$word2,1,prob = subset$chance)
}

word = "I"
while(TRUE) {
  message(word)
  word = VTfindNextWord(word)
}
