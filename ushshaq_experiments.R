library(stylo)
library(tidyverse)
library(tidytext)
library(paletteer)
library(rethinking)

#### Preparations, exploratory ####

files = list.files("data/corpus_full/", full.names = T)

## document lengths

summary = tibble(title = files, text = sapply(files, read_file)) %>% 
  mutate(title = str_replace_all(title, "data/corpus_full//(.*?).txt", "\\1")) %>% 
  group_by(title) %>% 
  unnest_tokens(input=text, output=word, token="words") %>% 
  count(title)

summary


## min length = 5861 words


## exploratory experiment for hafif vs. hazadzh meters only (Iraqi's diwan excluded)

set.seed(189)
res = stylo(gui=F,
            corpus.dir = "data/corpus/",
            analyzed.features = "w",
            ngram.size = 1,
            mfw.min=400,
            mfw.max=400, 
            distance.measure="delta",
            culling.min=0,
            culling.max=0,
            sampling="no.sampling",
            sample.size=5000,
            analysis.type = "CA",
            corpus.lang="Other")

## let's check the features that drive meter-based clustering
features = res$table.with.all.zscores %>% as.data.frame()

rownames(features)
classes = c(2,1,2,1,1,1,2) %>% as.factor()
fin = cbind(as.data.frame(classes), features)


## association of categories with features
description = FactoMineR::catdes(fin,num.var=1)

signif(description$quanti.var[1:10,], digits = 2)

hafif = description$quanti[[1]] %>% as_tibble(rownames = "word")
hazadzh = description$quanti[[2]] %>% as_tibble(rownames = "word")

## features that drive hafif / hazadzh difference
haf = hafif %>% filter(v.test > 0) %>% select(word, v.test) %>% mutate(meter = "hafif")
haz = hazadzh %>% filter(v.test > 0) %>% select(word, v.test) %>% mutate(meter ="hazadzh")


## plot features with p > 0.05

haf %>% bind_rows(haz) %>% mutate(v.test = ifelse(meter == "hafif", -v.test, v.test)) %>% ggplot(aes(x=v.test, y=reorder(word,-v.test),fill=meter)) + geom_col() + theme_minimal() + labs(y = "", x="") + scale_fill_paletteer_d("wesanderson::Rushmore1")



#### Ishqname verification (Main) ####

## how many times take a 5k random sample
n_samples = 100
list_of_results = vector(mode="list",length=n_samples)
list_of_distances  = vector(mode="list",length=n_samples)

## make a variable to get "meter-associated" words (for later filtering)
meter_words = c(haf$word,haz$word) 
distances = c("wurzburg", "manhattan", "minmax")

for (i in 1:n_samples) {

# use stylo for quick random sampling  
sample_5k = stylo(gui=F,
                    corpus.dir = "data/corpus_full/",
                    analyzed.features = "w",
                    ngram.size = 1,
                    mfw.min=200,
                    mfw.max=200, 
                    distance.measure="delta",
                    culling.min=0,
                    culling.max=0,
                    sampling="random.sampling",
                    sample.size=5000,
                    analysis.type = "CA",
                    corpus.lang="Other",
                    use.existing.freq.tables = F,
                    use.existing.wordlist = F)
  
  # retrieve table with z-scores
  freqs = sample_5k$table.with.all.zscores[,1:500]
  
  
  # remove meter words
  freqs = freqs[,!colnames(freqs) %in% meter_words]
  
  #quick dendro
  #freqs %>% dist(method="method") %>%  hclust(method="ward.D2") %>% ggdendro::ggdendrogram()
  
  
  # test text
  
  no_ishq = rownames(freqs) %>% str_detect("Ishqname") %>% which()
  
  # run impostors verification, 1000 iterations
  
  dist_i = sample(distances,1)
  sample_imposters = imposters(reference.set = freqs[-no_ishq,],
            test = freqs[no_ishq,],
            iterations = 1000,
            features = sample(seq(0.5,0.9, by=0.1), 1),
            imposters = 1,
            distance=dist_i)
  
  # save results
  
  list_of_results[[i]] = sample_imposters
  
  # continue
}



saveRDS(list_of_results, file="data/100_test_fin.rds")

list_of_results = readRDS("data/100_test_fin.rds")

# boxplots

bind_rows(list_of_results) %>%
  pivot_longer(c(1:5), names_to="impostor", values_to="threshold") %>%
  group_by(impostor) %>%
  mutate(mean_coef = mean(threshold)) %>%
  ggplot(aes(reorder(impostor, -mean_coef), threshold,group=impostor)) + geom_boxplot()

#### Verification (with known authors) ####





results_verify = vector(mode="list", length=20)
for (i in 1:20) {

sample_5k = stylo(gui=F,
                  corpus.dir = "data/corpus_no_dubia//",
                  analyzed.features = "w",
                  ngram.size = 1,
                  mfw.min=200,
                  mfw.max=200, 
                  distance.measure="delta",
                  culling.min=0,
                  culling.max=0,
                  sampling="random.sampling",
                  sample.size=5000,
                  analysis.type = "CA",
                  corpus.lang="Other",
                  use.existing.freq.tables = F,
                  use.existing.wordlist = F)


freqs = #read.table(file = "table_with_frequencies.txt")[1:400,] %>% t() %>% scale() 
  sample_5k$table.with.all.zscores[,1:500]

# remove meter words
freqs = freqs[,!colnames(freqs) %in% meter_words]

rownames(freqs)

target = rownames(freqs) %>% str_detect("Sanayi") %>% which() %>% sample(1)

sample_imposters = imposters(reference.set = freqs[-target,],
                             test = freqs[target,],
                             iterations = 1000,
                             features = sample(seq(0.5, 0.9, by=0.1),1),
                             imposters = 1,
                             distance="wurzburg")


results_verify[[i]] = sample_imposters


}


saveRDS(results_verify,"data/iraqi_verify.rds")
saveRDS(results_verify, "data/sanayi_verify.rds")

iraqi_v = readRDS("data/iraqi_verify.rds")
sanayi_v = readRDS("data/sanayi_verify.rds")

bind_rows(iraqi_v) %>% pivot_longer(c(1:5), names_to="impostor", values_to="threshold") %>% group_by(impostor) %>% mutate(mean_coef = mean(threshold)) %>%  ggplot(aes(reorder(impostor, -mean_coef), threshold,group=impostor)) + geom_boxplot() + labs(x="", y="Уверенность", title="Верификация подлинного Санаи (20 выборок по 5000 слов х 1000 итераций)")


#### Unmasking manual ####


mfw_list = seq(50, 500, by=50)
samples = 100
unmask_mfw = c()

for (mfw in mfw_list) {
  
  
  for (i in 1:samples) {
  
  # take sample  
  res = stylo(gui=F,
              corpus.dir="data/manual_AA/",
              mfw.min=mfw, 
              mfw.max=mfw,
              distance.measure="wurzburg", 
              sampling="random.sampling", 
              sample.size=5000,
              corpus.lang="Other")
  
  # remove metrical words & calculate distance
  freqs = res$table.with.all.freqs
  freqs = freqs[,!colnames(freqs) %in% meter_words]
  freqs = freqs[,1:mfw]
  distances = dist.wurzburg(freqs) %>% as.matrix()
  
  
  
  sanai_a = str_detect(rownames(distances), "Sanayi") %>% which() %>% sample(1)
  
  # find neighbors of Sanayi A
  dist_to_a = as.matrix(distances[sanai_a, -sanai_a]) %>% as_tibble(rownames = "target") %>%
    rename(distance = V1) %>% 
    mutate(target = str_replace(target, "Ibn Imad Shirazi_.*", "Ibn Imad Shirazi"),
           target = str_replace(target, "Iraqi_Ush.*", "Iraqi_hafif"),
           target = str_replace(target, "Iraqi_Diwan.*", "Iraqi_Diwan"),
           target = str_replace(target, "Ishqname_.*", "Ishqname"),
           target = str_replace(target, "Sanayi_.*", "Sanayi"),
           target = str_replace(target, "Ubayd Zakkani_.*", "Ubayd Zakkani"),
           target = str_replace(target, "Marageyi_.*", "Marageyi"),
           mfw = mfw,
           iteration = i) 
  
  unmask_mfw = rbind(unmask_mfw, dist_to_a)
  
  }
  
}


saveRDS(unmask_mfw, file="data/unmasking_manual.rds")

unmask_mfw %>% group_by(target,mfw) %>% mutate(type = case_when(target == "Ishqname" ~ "Ishqname",target == "Sanayi" ~ "same author",T ~ "other")) %>% mutate(pi_lower = PI(distance)[1], pi_upper = PI(distance)[2]) %>%
#  filter(target != "Iraqi_hafif") %>%
  group_by(target,mfw, pi_lower, pi_upper,type) %>%
  summarise(mean_dist = mean(distance)) %>%
  ggplot(aes(x=mfw, y=mean_dist,group=target,color=type)) + geom_line(aes(linetype=type)) + geom_ribbon(data = . %>% filter(target %in% c("Sanayi", "Ishqname")),aes(ymin=pi_lower,ymax=pi_upper,group=target,color=NULL),alpha=0.1)

