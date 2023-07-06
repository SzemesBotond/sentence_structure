# regex from "relreq.R' in 'realtions'

# function for counting proportions
conjunction_num <- function(relation, corpus) {
  all <- list ()
  number <- list ()
  for (i in 1:length(corpus)) {
    all[[i]] <- gregexpr(relation, corpus[[i]], perl = TRUE, ignore.case =TRUE)
    all [[i]] <- unlist(all[[i]])
    all [[i]] <- all[[i]][which(all[[i]] != -1)]
    number [[i]] <- length(all[[i]])
  }
  return(number)
}

# counting numbers
type_num <- list ()
for (i in 1:length(relations)) {
  type_num[[i]] <- conjunction_num(relations[[i]], token_sent2)
}
# counting other types using values from relfreq.R
simile_num <- conjunction_num(simile, sim_red)
space_num <- conjunction_num(space, space_red)
time_num <- conjunction_num(time, time_red)
rel_cl_num <- conjunction_num(relative_clause, rel_clause_red)

type_num[9] <- list(simile_num)
type_num[10] <- list(space_num)
type_num[11] <- list(time_num)
type_num[12] <- list(rel_cl_num)
type_num  <-  as.data.frame(matrix(unlist(type_num), nrow=length(unlist(type_num[1]))))
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
colnames(type_num) <- names
library(stringr)
files.v <- sapply(files.v, str_remove_all, ".txt")
files.v <- sapply(files.v,str_remove_all, ".*?\\_")
row.names(type_num) <- files.v

# rotate rows and cols
all_num <- t(type_num) 

#mixed categories
explic_infer <- unlist(lapply(all_num[4,]+ all_num[5,], sum))
advers_disjuct <- unlist(lapply(all_num[2,]+ all_num[3,], sum))
space_time <-  unlist(lapply(all_num[10,]+ all_num[11,], sum))

all_num<- rbind(all_num,explic_infer, advers_disjuct, space_time)
rownames(all_num)[rownames(all_num) == "explic_infer"] <- "Explicatory/Inferentail"
rownames(all_num)[rownames(all_num) == "advers_disjuct"] <- "Adversative/Disjunctive"
rownames(all_num)[rownames(all_num) == "space_time"] <- "Space/Time"
all_num <- all_num[-c(1,2,3,4,5,10,11),]
proportion_num <- as.data.frame(all_num)

# Counting Proportion
for (i in 1:ncol(all_num)) {
  for (j in 1:nrow(all_num)){
    proportion_num[j,i] <- all_num[j,i]/sum(all_num[,i])*100 
  }
} 
# Different format for visualization
all_proportion <- data.matrix(proportion_num)
all_proportion <- as.table(all_proportion)
all_proportion <- as.data.frame(all_proportion)
colnames(all_proportion) <- c("Relation", "Novel", "Proportion")

# Some examples
library(tidyverse)
ex_nov <- all_proportion %>%
  filter(Novel %in% c("AzÉrsekL", "Sinistra","Magyarorszag1514","AFaluJegyzője",  "Kaddis", "Sortalansag", "ATrafik", "Atleta",  "Saulus", "Aranysárkány", "EgyCsaladregeny","Emlekiratok") )
library(ggplot2)
ggplot(ex_nov, aes("", Proportion, fill = Relation)) +
  geom_bar(stat = "identity", position = "fill", width = .5) +
  ggtitle("Proportion of Relations") +
  xlab ("Novels") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap (~ Regény, ncol=6)+
  theme(strip.text.x = element_text(size = 7, colour = "black"))




