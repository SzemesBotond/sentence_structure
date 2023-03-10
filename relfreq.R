#Load corpus - without roman numbers and \" signs
input.dir <- "YOUR DIR"
files.v <- dir(input.dir, "\\.txt$")
make.file.l <- function(files.v, input.dir){
  text.l <- list()
  for(i in 1:length(files.v)){
    text.v <- scan(paste(input.dir, files.v[i], sep="/"),
                   what="character", sep="\f", quote = "", encoding = "UTF-8")
    text.l[[files.v[i]]] <- text.v
  }
  return(text.l)
}
my.corpus.l <- make.file.l(files.v, input.dir)

#Clear text (e.g. page number, abbriviation)
novels <- sapply(my.corpus.l, unlist,recursive = TRUE, use.names = TRUE)
novels <-  sapply(novels,function(x) gsub("([0-9]+)([A-zöüóőúéáűí])", "\\2",as.character(x)))
novels <-  sapply(novels,function(x) gsub("(– )([A-ZÖÜÓŐÚÉÁŰÍ])", "\\2",as.character(x)))
novels <-  sapply(novels,function(x) gsub("(- )([A-ZÖÜÓŐÚÉÁŰÍ])", "\\2",as.character(x)))
novels <-  sapply(novels,function(x) gsub("(\\.\\.\\.)( [A-ZÖÜÓŐÚÉÁŰÍ])", "\\.\\2",as.character(x)))
novels <-  sapply(novels,function(x) gsub("([A-zzöüóőúéáűí])(-)", "\\1",as.character(x)))
novels <-  sapply(novels,function(x) gsub("([[:punct:]])([A-zzöüóőúéáűí])", "\\2",as.character(x)))
novels <-  sapply(novels,function(x) gsub("Dr\\. ", "Dr ",as.character(x)))
novels <-  sapply(novels,function(x) gsub("stb\\. ", "stb ",as.character(x)))
novels <-  sapply(novels,function(x) gsub("Özv\\. ", "Özv ",as.character(x)))
novels <-  sapply(novels,function(x) gsub("ifj\\. ", "ifj ",as.character(x)))
novels <-  sapply(novels,function(x) gsub("ún\\. ", "ún ",as.character(x)))
novels <-  sapply(novels,function(x) gsub("St\\. ", "st ",as.character(x)))
novels <-  sapply(novels,function(x) gsub("( [A-zzöüóőúéáűí])(\\.)", "\\1", as.character(x)))
novels <-  sapply(novels,function(x) gsub("([.?!])( [a-zöüóőúéáűí])", "\\2", as.character(x)))
novels <-  sapply(novels,function(x) gsub("([.?!])( [a-zöüóőúéáűí])", "\\2", as.character(x)))
novels <-  sapply(novels,function(x) gsub("([.?!])([\\)] [a-zöüóőúéáűí])", "\\2", as.character(x)))

#Tokenize sentences and words
library(tokenizers)
token_sent <- sapply(novels, tokenize_sentences)
token_sent2 <- sapply(token_sent, unlist, recursive = TRUE, use.names = TRUE)
sentence_words <- sapply(token_sent2, tokenize_words)

# Clear chapters ("fejezet") and chapter numbers
sw <- list()
for (i in 1:length(sentence_words)) {
  sw[[i]] <- sentence_words[[i]][which(sentence_words[[i]] != "fejezet")]
  sw[[i]] <- sw[[i]][which(!grepl("^[0-9][0-9]", sw[[i]]))]
  sw[[i]] <- sw[[i]][which(!grepl("^[0-9]", sw[[i]]))]
}
# Sentence Length and clear 0 values
sentence_length <- list ()
for (i in 1:length(sw)) {
  sentence_length [[i]] <- sapply(sw[[i]], length)
  sentence_length [[i]] <- sentence_length[[i]][which(sentence_length[[i]] !=0)]
}

num_of_words <- lapply(sentence_length, sum)


# Clase Realations
# Regex
relations <- c("([,;:\\-\\–](( (és |sőt |s |valamint|majd |aztán|azután))|( (([a-zíéáűúőóüö]+ ){0,2})(ráadásul))))|(((egyrészt )(.*[,;] )(másrészt))|((hol )(.*[,;] )(hol )))",
                 "[,;:\\-\\–] ((([a-zíéáűúőóüö ]+|)(azonban|ellenben|viszont |ámde |csakhogy |ellenkezőleg|mindazonáltal|ugyananakkor |márpedig |mégis |mégse|csak hát))|(de |pedig|ám ))",
                 "[,;:\\-\\–] (vagy |avagy )",
                 "[,;:\\-\\–]( (([a-zíéáűúőóüö ]+|)(azaz |ugyanis |hiszen |azazhogy |vagyishogy |tudniillik |mármint|mégpedig|elvégre |egyszóval|utóvégre|illetve))|((( mert | de | és | s )| )(hisz |pontosabban)))",
                 "[,;:\\-\\–]( (([a-zíéáűúőóüö ]+|)(tehát |ennélfogva |eszerint |úgyhogy|következésképpen))|(( és | s | )(ezért|szóval)))|([,;:]( és | s | )így )",
                 "(((^(?=.*[,;:]))|([,;:\\-\\–]([a-zíéáűúőóüö ]+|) ))(bár |(ha )(([a-zíéáűúőóüö]+ )+)(is )|habár |igaz, hogy |jóllehet |még ha |mégha |noha |ugyan |ámbár))|([,;:\\-\\–] (holott))",
                 "([,;:\\-\\–] ((([a-zíéáűúőóüö]+ ){0,1}((éppen|)mert |merthogy |minthogy |mivel |nehogy))|különben))|((amiatt |avégett |avégre |azért)(([a-zíéáűúőóüö ]+|)(([a-zíéáűúőóüö]+)|))[,;]( hogy))|((^(?=.*[,;:]))(([a-zíéáűúőóüö]+ ){0,1})(minthogy |mivel ))",
                 "((^(?=.*[,;:]))|([,;:\\-\\–]([a-zíéáűúőóüö ]+|) ))(ha |hacsak |amennyiben |hogyha|a mennyiben)")

simile <- "(^|([,;:\\-\\–] ))(([a-zíéáűúőóüö]+ ){0,1})(mint |mintha |akár |akárha |akárcsak |minthogyha)"
space <- "(((^(?=.*[,;:]))|([,;:\\-\\–]([a-zíéáűúőóüö ]+|) ))(ahol |ahonnan |ahov|ameddig |amerr))|([,;:\\-\\–] (a |)(hol |honnan |hová |hova |meddig |merről |merre))"
time <- "(((^(?=.*[,;:]))|([,;:\\-\\–]([a-zíéáűúőóüö ]+|) ))(alighogy |ameddig |amíg |amikor|amióta |attól (fogva|kezdve), hogy |amint |mígnem |amidőn |midőn |mielőtt |míg |mialatt |mi alatt |mihelyt |mikor|mi kor|mióta|mi óta |miután|mi után|miközben|mi közben))|([,;:\\-\\–] (közben|mire))"
relative_clause <- "(((^(?=.*[,;:]))|([,;:\\-\\–]([a-zíéáűúőóüö ]+|) ))((ami|aki|amely|mely)))|[,;\\-\\–]((( és | s | de | a | )(ki |a mit |kit |mik |kik |miben |kiben |mibe |kibe |a mire |kire |a miről |kiről |mitől |kitől |miből |kiből |kinél  |kivel |a mivel |minek |kinek |min |kin |mihez |kihez |miket |kiket |mikben |kikben |mikbe |kikbe |mikre |kikre |mikről |kikről |miktől |kiktől |mikből |kikből |miknél |kiknél |mikkel |kikkel |miknek |kiknek |miken |kiken |mikhez |kikhez))|( mi ))"

# function to relfreq
relfreq <- function(relation, coprus) {
  all <- list ()
  num <- list ()
  freq <- list()
  for (i in 1:length(coprus)) {
    all[[i]] <- gregexpr(relation, coprus[[i]], perl = TRUE, ignore.case =TRUE)
    all [[i]] <- unlist(all[[i]])
    all [[i]] <- all[[i]][which(all[[i]] != -1)]
    num [[i]] <- length(all[[i]])
    freq[[i]] <- num[[i]]/num_of_words[[i]]*1000 
  }
  return(freq)
}

# count all types
type <- list ()
for (i in 1:length(relations)) {
  type[[i]] <- relfreq(relations[[i]], token_sent2)
}

# some preparation for other types
mental_verb <- "((^| )(elárul|eldönt|elképzel|ért|érdekel|érdekl|tud|kérde|kérdi|megkérd|képzel|kitalál|megállapít|megért|megmutat|sejt))|(ni akar)"
mental_verb2 <- "(ni akar)|((^| )(elárul|eldönt|elképzel|érdekel|érdekl|tud|kérde|kérdi|megkréd|képzel|kitalál|megállapít|megért|megmutat|sejt))"
library(stringr)
simile_red <- lapply(token_sent2, str_remove_all,"((akár|Akár) (.*?)akár )|( a mint | nem mint )")
rel_cluase_red <- lapply(token_sent2, str_remove_all, "amint|(A|a)mikor|amiként|amiképp|amiatt|amidőn|amióta|amialatt|amielőtt|amiután|amíg ")
rel_cluase_red <- lapply(rel_cluase_red, function(x) gsub("(^| )a melyik", " amelyik", ignore.case=T, as.character(x)))
rel_cluase_red <- lapply(rel_cluase_red, str_remove_all, "( mint (aki|ami|ki))|( melyik)")
rel_cluase_red <- lapply(rel_cluase_red, function(x) gsub(paste(ment_ig,"([a-zíéáűúőóüö ]+|)(\\, )(mi|mely)", sep=""), "\\1\\2\\3\\4", ignore.case=T, as.character(x)))
rel_cluase_red <- lapply(rel_cluase_red, function(x) gsub(paste(ment_ig, "([a-zíéáűúőóüö]+|)(\\, )(ki)", sep=""), "\\1\\2\\3", as.character(x)))
time_red <- lapply(token_sent2, function(x) gsub(paste(ment_ig2,"([a-zíéáűúőóüö]+|)(\\, )(mikor|mióta)", sep=""),"\\1\\2\\3", ignore.case=T, as.character(x)))
time_red <- lapply(token_sent2, function(x) gsub("(\\, hogy)([a-zíűáéúőóüö ]+|) (mikor|mi kor|mióta|mi óta )", "\\1\\2 ", ignore.case=T, as.character(x)))
time_red  <- lapply(time_red, str_remove_all, " mint (amikor|amióta |mióta |amidőn |midőn |mikor|mi kor)")
sapce_red <- lapply(token_sent2, function(x) gsub(paste(ment_ig2,"([a-zíéáűúőóüö]+|)(\\, )(hol |hon|hov|merr|meddig)", sep=""),"\\1\\2\\3", ignore.case=T, as.character(x)))
sapce_red <- lapply(token_sent2, function(x) gsub("(\\, hogy)([a-zíűáéúőóüö ]+|) (hol |honnan |hová |hova)", "\\1\\2 ", ignore.case=T, as.character(x)))

# counting relfreq for the other types
sim_rel <- relfreq(simile, simile_red)
space_rel <- relfreq(space, sapce_red)
time_rel <- relfreq(time, time_red)
rel_rel <- relfreq(relative_clause, rel_cluase_red)


#combine all to one dataframe
type[9] <- list(sim_rel)
type[10] <- list(space_rel)
type[11] <- list(time_rel)
type[12] <- list(rel_rel)
type  <-  as.data.frame(matrix(unlist(type), nrow=length(unlist(type[1]))))
names <- c("Copulative",
           "Adversative",
           "Disjuctive",
           "Explicatory",
           "Inferential",
           "Concessive",
           "Logical",
           "Conditional",
           "Simile",
           "Space",
           "Time",
           "Relative"
)
colnames(type) <- names
row.names(type) <- files.v

#Write results
library("writexl")
write_xlsx(type,"YOUR DIR")
