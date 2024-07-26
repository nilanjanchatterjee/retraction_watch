library(readxl)
library(tidyverse)
library(lubridate)
library(cowplot)

setwd("./..")
rtr_db <-read_xlsx("./Retraction/Data_life science_SB_12092023.xlsx")
head(rtr_db)
summary(rtr_db)

country_db <-mosaic::tally(~ Reason, rtr_db)

################################################################################################
###### Pattern across countries

cntry <-capture.output(cat(rtr_db$Country)) ###concatenate all country name together
cntry1 <- gsub(";", " ", cntry)
cntry1 <-gsub("United ", "United-", cntry1)
cntry1 <-gsub("Hong ", "Hong-", cntry1)
cntry1 <-gsub("South ", "South-", cntry1)
cntry1 <-gsub("North ", "North-", cntry1)
cntry1 <-gsub("Sri ", "Sri-", cntry1)
cntry1 <-gsub("Saudi ", "Saudi-", cntry1)
cntry1 <-gsub("New ", "New-", cntry1)
cntry1 <-gsub(" Emirates", "-Emirates", cntry1)
cntry1 <-gsub(" & ", "-&-", cntry1)
cntry1 <-gsub("Puerto ", "Puerto-", cntry1)
cntry1 <-gsub("Costa ", "Costa-", cntry1)
cntry1 <-gsub("Czech Republic", "Czech-Republic", cntry1)

freq_x <- sort(table(unlist(strsplit(cntry1, " "))),      # Create frequency table
               decreasing = TRUE)
freq_x

freq_x1 <- freq_x[freq_x>25] ## Can use other cutoffs also 10/40/50

###### Countrywise retraction pattern
cntry_freq <-ggplot(as.data.frame(freq_x1), aes(y=Var1, x= Freq, fill = Var1)) + 
  geom_col()+
  theme_bw()+ labs(y = "Country", x = "Number of Studies")+ 
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")
cntry_freq
ggsave(cntry_freq, file = "Country_frequency_new.jpeg", width = 9, height = 6, units = "in", dpi=200)

################################################################################################################
######## Country culture and retraction relation
country_cltr <- read_xlsx("Country_list_updated 07112023.xlsx") |> drop_na(Individualism)
head(country_cltr)
colnames(country_cltr) <- gsub(" \r\n", "_", colnames(country_cltr))

cltr_pca <- prcomp(country_cltr[,5:10])
cor.test(country_cltr$Freq, cltr_pca$x[,3]) ###only significant
country_cltr$pca3 <- cltr_pca$x[,3]

pca_plot <- ggplot(country_cltr,aes(x= pca3, y= log(Freq)))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw() + labs(y = "Log(Number of retractions)", x= "PCA axis 3")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")

lng_ornt_plot <- ggplot(country_cltr,aes(x= Longterm_orientation, y= log(Freq)))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw() + labs(y = "Log(Number of retractions)", x= "Longterm_orientation")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")

ggsave(lng_ornt_plot, filename = "Freq_wlongornt.jpeg", width = 6, height = 4, units = "in", dpi=300)

plot_grid(pca_plot, lng_ornt_plot, ncol = 2, labels = c("A", "B"), label_size = 20)
ggsave( filename = "Freq_wculture.jpeg", width = 8, height = 4, units = "in", dpi=300)
#############################################################################################################
##### Counting frequency of the reasons

rsn_db <-capture.output(cat(rtr_db$Reason))
rsn_db <-gsub(" +", "+", rsn_db)

freq_rsn <- as.data.frame(sort(table(unlist(strsplit(rsn_db, ";"))),      # Create frequency table
               decreasing = TRUE))
head(freq_rsn)
freq_rsn$Var1 <- gsub(" \\+", "\\+", freq_rsn$Var1)
freq_rsn_new <- aggregate(Freq ~ Var1, FUN = sum, freq_rsn)
freq_rsn_new <- freq_rsn_new[order(freq_rsn_new$Freq, decreasing = T),]
freq_rsn_new$Var1 <-factor(freq_rsn_new$Var1, levels = freq_rsn_new$Var1)
freq_rsn_new <-subset(freq_rsn_new, Freq >=10)
head(freq_rsn_new)

reason_freq <-ggplot(freq_rsn_new, aes(y=Var1, x= Freq, fill = Var1)) + 
  geom_col()+
  theme_bw()+ labs(y = "Reasons", x= "Numbers of reasons")+
  theme(legend.position = "none")
reason_freq

ggsave(reason_freq, file = "./Documents/Retraction/Reason_frequency.jpeg", width = 12, height = 8, units = "in", dpi=200)


###### Aggregated subject list summary
rsn_freq_new<-read_xlsx("Reason_frequency_26122023_SB.xlsx") |>drop_na(`New label`) |>arrange(desc(Freq))
  head(rsn_freq_new)
 
rsn_sum <- rsn_freq_new |> group_by(`New label`) |> summarise(total=sum(Freq)) |>arrange(total)

#Then turn it back into a factor with the levels in the correct order
rsn_sum$`New label` <- factor(rsn_sum$`New label`, levels=unique(rsn_sum$`New label`))

#agg_sub_list |> #filter(Freq >5)|> order_by(`new category`)
rsn_sum |>
  ggplot( aes(x= total, y= `New label`))+
  geom_col(aes(fill= `New label`))+
  theme_bw()+
  labs(y = "Reasons for retraction", x= "Number of papers")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")

ggsave("Reason_area_freq_new.jpeg", width = 7, height=5, units= "in", dpi=300)

####################################################################################################
###### Number of Authors and reasons

rtr_db$num_reason <-str_count(rtr_db$Reason, ";")
 
num_rsn <- rtr_db |> count(num_reason) |>
  ggplot( aes(x= as.factor(num_reason), y = n))+
  geom_col(fill= "blue")+
  theme_bw()+
  labs(x= "Number of reasons", y= "Number of papers")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=12))

ggsave(num_rsn, file = "./Documents/Retraction/Num_reason.jpeg", width = 12, height = 8, units = "in", dpi=200)  

rtr_db$num_authr <- str_count(rtr_db$Author, ";") +1
xtabs(~num_authr, rtr_db)

num_atr <- rtr_db |> count(num_authr) |>
  ggplot( aes(x= as.factor(num_authr), y = n))+
  geom_col(fill= "blue")+
  theme_bw()+
  labs(x= "Number of Authors", y= "Number of papers")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=12))

ggsave(num_atr, file = "./Documents/Retraction/Num_authors.jpeg", width = 12, height = 8, units = "in", dpi=200)  

authr_db <-capture.output(cat(rtr_db$Author))

#rsn_db <-gsub(" +", "+", rsn_db)

freq_athr <- as.data.frame(sort(table(unlist( strsplit(authr_db, ";"))),      # Create frequency table
                               decreasing = TRUE))
head(freq_athr)
freq_athr1 <- freq_athr |> filter(Freq >10) |>
  ggplot(aes(x= Freq, y= Var1, fill = Var1))+
  geom_col() +
  theme_bw() + theme(legend.position = "none")

freq_athr1
write.csv(freq_athr, "./Documents/Retraction/Author-list.csv")

#####################################################################################################################
#### create networks of the authors
library(statnet)
rtr_db_filatr <- rtr_db |> filter(num_authr >1)
rtr.coauthors = sapply(as.character(rtr_db_filatr$Author), strsplit, ";")
#rtr.coauthors <- cbind(rtr_db$`Record ID`, (strsplit(rtr_db$Author, ";"))[[1]])
rtr.coauthors <- cbind(rtr_db$`Record ID`, unlist(sapply(as.character(rtr_db$Author), strsplit, ";")))
kellogg.coauthors = lapply(rtr.coauthors, trimws)

kellogg.coauthors.unique = unique(unlist(kellogg.coauthors))[order(unique(unlist(kellogg.coauthors)))]

freq_athr1 <- freq_athr |> filter(Freq >5)
kellogg.bipartite.edges = lapply(kellogg.coauthors, function(x) {freq_athr1$Var1 %in% x})
kellogg.bipartite.edges = do.call("cbind", kellogg.bipartite.edges) # dimension is number of authors x number of papers
rownames(kellogg.bipartite.edges) = freq_athr1$Var1 #kellogg.coauthors.unique

#new_mat <- subset(kellogg.bipartite.edges, colSums(kellogg.bipartite.edges)>1)
kellogg.mat = kellogg.bipartite.edges %*% t(kellogg.bipartite.edges) #bipartite to unimode
mat = kellogg.mat[order(rownames(kellogg.mat)), order(rownames(kellogg.mat))]

kellogg.statnet = as.network(kellogg.mat, directed = FALSE, names.eval = "edge.lwd", ignore.eval = FALSE)
kellogg.statnet ##view network summary
plot.network(kellogg.statnet, edge.col = "gray", edge.lwd = "edge.lwd", label = " ", 
             label.cex = .5, label.pad = 0, label.pos = 1, jitter = TRUE)

head(kellogg.statnet$)
jpeg(filename = "./Documents/Retraction/Social_network_rtr.jpeg", width = 12, height = 9, units = "in", res = 300)
plot.network(kellogg.statnet, edge.col = "gray", edge.lwd = "edge.lwd", label = " ", label.cex = .5, label.pad = 0, label.pos = 1)
dev.off()

####################################################################################################
###### Publication Time of the studies

rtr_db$pubyear <- year(rtr_db$OriginalPaperDate)
rtr_db$rtryear <- year(rtr_db$RetractionDate)

rtr_db$timediff <- as.numeric(rtr_db$RetractionDate - rtr_db$OriginalPaperDate, units = "weeks")
rtr_pubyr <- rtr_db |> count(year = pubyear)
rtr_rtryr <- rtr_db |> count(year = rtryear)

rtr_year_ptrn <- rtr_rtryr |>   filter( year >=1975)|>
  ggplot(aes(x= year))+ 
  geom_line(aes(y= n), lwd=1, col ="brown") + 
  #geom_line(aes(y=rollmean(n, 5, na.pad=TRUE)), linetype= "dashed") +
  geom_smooth(aes(x=year, y= n), method = "gam", linetype= "dashed")+
  #geom_line(aes(x= year, y= n.y), col ="blue", lwd=1, lty = 2) + 
  labs(x= "Year" , y= "Number of publication", col = " ", lty= " ")+
  theme_bw()+coord_cartesian(ylim = c(0,2100))+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=12),
        legend.position = c(0.1,0.8),
        legend.text = element_text(size=18))

ggsave(rtr_year_ptrn, file = "Yearwise_pattern_wgam.jpeg", width = 9, height = 6, units = "in", dpi=300)

rtr_yr <- full_join(rtr_pubyr, rtr_rtryr, by = "year") |> 
  pivot_longer(cols = c(n.x, n.y),
               names_to = c("type"),
               names_prefix = "n.")
head(rtr_yr)

year_pattern <- rtr_yr |>   filter( year >=1975)|>
  ggplot()+ 
  geom_line(aes(x= year, y= value, col = type, lty = type), lwd=1) + 
  #geom_line(aes(x= year, y= n.y), col ="blue", lwd=1, lty = 2) + 
  labs(x= "Year" , y= "Number of publication", col = " ", lty= " ")+
  theme_bw()
  scale_color_discrete(labels = c("Published", "Retracted"))+
  scale_linetype_discrete(labels = c("Published", "Retracted"))+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=12),
        legend.position = c(0.1,0.8),
        legend.text = element_text(size=18))

ggsave(year_pattern, file = "./Documents/Retraction/Yearwise_pattern.jpeg", width = 12, height = 8, units = "in", dpi=200)
 
diff_pattern <- rtr_db |>   filter( year >=1975) |>
  ggplot()+geom_point(aes(x = OriginalPaperDate, y= timediff/48))+
  theme_bw()

diff_pattern

#################################################################################################
#################Jounals and subject area

rtr_jrnl <-rtr_db |> count(Journal)

write.csv(rtr_jrnl, "Journal_list.csv")

head(rtr_db$Subject)

rtr_subject <-capture.output(cat(rtr_db$Subject))

freq_subject <- as.data.frame(sort(table(unlist(strsplit(rtr_subject, ";"))),      # Create frequency table
                               decreasing = FALSE))
head(freq_subject)

freq_subject |> filter(Freq >10)|>
  ggplot( aes(x= Freq, y= Var1))+
  geom_col(aes(fill= Var1))+
  theme_bw()+
  labs(y = "Subject Area", x= "Number of papers")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")

ggsave("./Documents/Retraction/Subject_area_freq.jpeg", width = 10, height=15, units= "in", dpi=200)
write.csv(freq_subject, "./Documents/Retraction/subject-list.csv" )

###### Aggregated subject list summary
agg_sub_list <- read_xlsx("subject-list_SB_27th December 2023.xlsx") |> 
  drop_na(`new category`) |>arrange(desc(Freq))
agg_sub_list$`new category` <- as.factor(agg_sub_list$`new category`)
head(agg_sub_list)

agg_sum <- agg_sub_list |> group_by(`new category`) |> summarise(total=sum(Freq)) |>arrange(total)

#Then turn it back into a factor with the levels in the correct order
agg_sum$`new category` <- factor(agg_sum$`new category`, levels=unique(agg_sum$`new category`))

#agg_sub_list |> #filter(Freq >5)|> order_by(`new category`)
  agg_sum |>
  ggplot( aes(x= total, y= `new category`))+
  geom_col(aes(fill= `new category`))+
  theme_bw()+
  labs(y = "Subject Area", x= "Number of papers")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")
  
  ggsave("Subject_area_freq_new.jpeg", width = 7, height=5, units= "in", dpi=300)
  
  
########################################################################################
  ### Relationship between the Journal IF and number of retractions
  
 jif_num <- read.csv("Journal_impact_factor_retraction_all.csv")
 jif_num <- jif_num |> drop_na(X2022.JIF) ###drop columns with NO IF information
  
  summary(lm(log(n) ~ log(X2022.JIF) -1, data = jif_num)) ##Fitting the regression
  
  jif_num |> drop_na(X2022.JIF) |> #plot the regression 
    ggplot(aes(x= log(X2022.JIF), y = log(n)))+
    geom_point(alpha = 0.7) +
    geom_smooth( method = "gam", formula = y ~ (x)-1) + 
    coord_cartesian(ylim = c(0,6.5))+
    labs(x= "2022 Impact Factor", y= "Number of retractions") + 
    annotate("text", x = -0.85, y=5.5 , label = bquote("R^2 == 0.343"), parse = TRUE)+
    annotate("text", x = -0.85, y=5.2 , label = bquote("p <0.001"), parse = TRUE)+
    scale_x_continuous(breaks = c(-1.386,0, 1.61, 2.996,  3.912, 5.011),
                       labels = c( "0.25",  "1", "5", "20", "50", "150"))+
    scale_y_continuous(breaks = c(0, 1.61, 2.996,  4.61, 5.991),
                       labels = c("1", "5", "20", "100", "400")) +
    theme_bw() + 
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size=12)) 

ggsave("JIF_freq_plot.jpeg", width = 8, height=5.5, units= "in", dpi=600)
