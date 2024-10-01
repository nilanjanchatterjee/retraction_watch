library(readxl)
library(tidyverse)
library(lubridate)
library(cowplot)
library(igraph)

#setwd("./retraction_watch/")
rtr_db <-read_xlsx("./Retraction/Retraction Watch Data_ Bio Science.xlsx")
head(rtr_db)
summary(rtr_db)

country_db <-mosaic::tally(~ Reason, rtr_db)

####################################################################################################
###### Publication Time of the studies

rtr_db$pubyear <- year(rtr_db$OriginalPaperDate)
rtr_db$rtryear <- year(rtr_db$RetractionDate)

rtr_db$timediff <- as.numeric(rtr_db$RetractionDate - rtr_db$OriginalPaperDate, units = "weeks")
rtr_pubyr <- rtr_db |> count(year = pubyear)
rtr_rtryr <- rtr_db |> count(year = rtryear)

#### Figure 1A Year wise pattern of publication ----
rtr_year_ptrn <- rtr_rtryr |>   filter( year >=1975)|>
  ggplot(aes(x= year))+ 
  geom_line(aes(y= n), lwd=1, col ="brown") + 
  #geom_line(aes(y=rollmean(n, 5, na.pad=TRUE)), linetype= "dashed") +
  geom_smooth(aes(x=year, y= n), method = "gam", linetype= "dashed")+
  #geom_line(aes(x= year, y= n.y), col ="blue", lwd=1, lty = 2) + 
  labs(x= "Publication Year" , y= "Number of publication", col = " ", lty= " ")+
  theme_bw()+coord_cartesian(ylim = c(0,2100))+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=12),
        legend.position = c(0.1,0.8),
        legend.text = element_text(size=18))

#ggsave(rtr_year_ptrn, file = "Yearwise_pattern_wgam.jpeg", width = 9, height = 6, units = "in", dpi=300)

#### Figure 1B Year wise pattern of time difference ----
rtr_year_diff <- rtr_db |> filter( pubyear >=1975)|>
  ggplot() + 
  geom_boxplot(aes(x= (pubyear), y= timediff/4, group =pubyear),
                   fill = "lightblue", outlier.size = 0.4) +
  labs(x = "Publication Year", 
       y = "Time between publication and retraction (months)") +
  scale_fill_discrete(breaks = c("1980", "1990", "2000", "2010", "2020"))+
  theme_bw() + theme(axis.text = element_text(size=10),
                     axis.title.x = element_text(size=16))

cowplot::plot_grid(rtr_year_ptrn, rtr_year_diff,
                   labels = c("A", "B"))
ggsave("Final_figure_1.jpeg", width = 9, height = 4, dpi = 300, units = "in")
# rtr_yr <- full_join(rtr_pubyr, rtr_rtryr, by = "year") |> 
#   pivot_longer(cols = c(n.x, n.y),
#                names_to = c("type"),
#                names_prefix = "n.")
# head(rtr_yr)

# year_pattern <- rtr_yr |>   filter( year >=1975)|>
#   ggplot()+ 
#   geom_line(aes(x= year, y= value, col = type, lty = type), lwd=1) + 
#   #geom_line(aes(x= year, y= n.y), col ="blue", lwd=1, lty = 2) + 
#   labs(x= "Year" , y= "Number of publication", col = " ", lty= " ")+
#   theme_bw()
# scale_color_discrete(labels = c("Published", "Retracted"))+
#   scale_linetype_discrete(labels = c("Published", "Retracted"))+
#   theme(axis.title = element_text(size=16),
#         axis.text  = element_text(size=12),
#         legend.position = c(0.1,0.8),
#         legend.text = element_text(size=18))
# 
# ggsave(year_pattern, file = "./Documents/Retraction/Yearwise_pattern.jpeg", width = 12, height = 8, units = "in", dpi=200)

# diff_pattern <- rtr_db |>   filter( year >=1975) |>
#   ggplot()+geom_point(aes(x = OriginalPaperDate, y= timediff/48))+
#   theme_bw()
# 
# diff_pattern

################################################################################################
###### Retraction Pattern across countries

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

###### Figure 2A Country wise retraction pattern -----
cntry_freq <-ggplot(as.data.frame(freq_x1), aes(y=Var1, x= Freq, fill = Var1)) + 
  geom_col()+
  theme_bw()+ labs(y = "Country", x = "Number of Studies")+ 
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")
cntry_freq
ggsave(cntry_freq, file = "Country_frequency_new.jpeg", width = 8, height = 6, units = "in", dpi=200)

###############################################################
###### Aggregated subject list summary
agg_sub_list <- read_xlsx("subject-list_SB_27th December 2023.xlsx") |> 
  drop_na(`new category`) |>arrange(desc(Freq))
agg_sub_list$`new category` <- as.factor(agg_sub_list$`new category`)
head(agg_sub_list)

agg_sum <- agg_sub_list |> group_by(`new category`) |> summarise(total=sum(Freq)) |>arrange(total)

#Then turn it back into a factor with the levels in the correct order
agg_sum$`new category` <- factor(agg_sum$`new category`, levels=unique(agg_sum$`new category`))

#### Figure 2B Subject area plot ----
agg_sum_plt <- agg_sum |>
  ggplot( aes(x= total, y= `new category`))+
  geom_col(aes(fill= `new category`))+
  theme_bw()+
  labs(y = "Subject Area", x= "Number of papers")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")

#ggsave("Subject_area_freq_new.jpeg", width = 7, height=5, units= "in", dpi=300)

#####################################################################
###### Aggregated list of reasons -----
rsn_freq_new<-read_xlsx("./Retraction/Reason_frequency_26122023_SB.xlsx") |>
  drop_na(`New label`) |>arrange(desc(Freq))
head(rsn_freq_new)

rsn_sum <- rsn_freq_new |> group_by(`New label`) |> summarise(total=sum(Freq)) |>arrange(total)

#Then turn it back into a factor with the levels in the correct order
rsn_sum$`New label` <- factor(rsn_sum$`New label`, levels=unique(rsn_sum$`New label`))

#agg_sub_list |> #filter(Freq >5)|> order_by(`new category`)
#### Figure 2C Reason frequency----- 
rsn_db_plt <- rsn_sum |>
  ggplot( aes(x= total, y= `New label`))+
  geom_col(aes(fill= `New label`))+
  theme_bw()+
  labs(y = "Reasons for retraction", x= "Number of papers")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")

ggsave(rsn_db_plt, filename = "Reason_area_freq_new.jpeg", width = 7, height=5, units= "in", dpi=300)



#######################################################################
#### Number of Authors
#### Figure 2D Number of Authors----
rtr_db$num_authr <- str_count(rtr_db$Author, ";") +1
xtabs(~num_authr, rtr_db)

num_atr <- rtr_db |> count(num_authr) |> filter(!is.na(num_authr)) |>
  ggplot( aes(x= as.factor(num_authr), y = n))+
  geom_col(fill= "blue")+
  theme_bw()+
  labs(x= "Number of Authors", y= "Number of papers")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=12))
num_atr
ggsave(num_atr, file = "./Documents/Retraction/Num_authors.jpeg", width = 12, height = 8, units = "in", dpi=200)  

#### Final Figure 2 ----
cowplot::plot_grid(cntry_freq, agg_sum_plt, rsn_db_plt, num_atr, 
                   labels = c("A", "B", "C", "D"), ncol =2)

ggsave("Final_multipanel_plot.jpeg", width=14, height =9, units = "in", dpi=300)

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

#### Figure 5 Culture plot with different countries -----
plot_grid(pca_plot, lng_ornt_plot, ncol = 2, labels = c("A", "B"), label_size = 20)
ggsave(filename = "Freq_wculture.jpeg", width = 8, height = 4, units = "in", dpi=300)
#############################################################################################################
##### Counting frequency of the reasons

# rsn_db <-capture.output(cat(rtr_db$Reason))
# rsn_db <-gsub(" +", "+", rsn_db)
# 
# freq_rsn <- as.data.frame(sort(table(unlist(strsplit(rsn_db, ";"))),      # Create frequency table
#                decreasing = TRUE))
# head(freq_rsn)
# freq_rsn$Var1 <- gsub(" \\+", "\\+", freq_rsn$Var1)
# freq_rsn_new <- aggregate(Freq ~ Var1, FUN = sum, freq_rsn)
# freq_rsn_new <- freq_rsn_new[order(freq_rsn_new$Freq, decreasing = T),]
# freq_rsn_new$Var1 <-factor(freq_rsn_new$Var1, levels = freq_rsn_new$Var1)
# freq_rsn_new <-subset(freq_rsn_new, Freq >=10)
# head(freq_rsn_new)
# 
# reason_freq <-ggplot(freq_rsn_new, aes(y=Var1, x= Freq, fill = Var1)) + 
#   geom_col()+
#   theme_bw()+ labs(y = "Reasons", x= "Numbers of reasons")+
#   theme(legend.position = "none")
# reason_freq
# 
# ggsave(reason_freq, file = "./Documents/Retraction/Reason_frequency.jpeg", width = 12, height = 8, units = "in", dpi=200)

####################################################################################################
###### Number of Authors and reasons

rtr_db$num_reason <-str_count(rtr_db$Reason, ";")

#### Supplementary figure S1 ----
num_rsn <- rtr_db |> count(num_reason) |>
  ggplot( aes(x= as.factor(num_reason), y = n))+
  geom_col(fill= "blue")+
  theme_bw()+
  labs(x= "Number of reasons", y= "Number of papers")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=12))

ggsave(num_rsn, file = "./Documents/Retraction/Num_reason.jpeg", width = 12, height = 8, units = "in", dpi=200)  


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
authr_db <-capture.output(cat(rtr_db$Author))
freq_athr <- as.data.frame(sort(table(unlist( strsplit(authr_db, ";"))),      # Create frequency table
                                decreasing = TRUE))

rtr_db$num_authr <- str_count(rtr_db$Author, ";") +1
rtr_db_filatr <- rtr_db |> filter(num_authr >1)
rtr.coauthors = sapply(as.character(rtr_db_filatr$Author), strsplit, ";")
#rtr.coauthors <- cbind(rtr_db$`Record ID`, (strsplit(rtr_db$Author, ";"))[[1]])
rtr.coauthors <- cbind(rtr_db$`Record ID`, unlist(sapply(as.character(rtr_db$Author), strsplit, ";")))
rtr.coauthors <-  sapply(as.character(rtr_db$Author), strsplit, ";")
kellogg.coauthors = lapply(rtr.coauthors, trimws)

kellogg.coauthors.unique = unique(unlist(kellogg.coauthors))[order(unique(unlist(kellogg.coauthors)))]

freq_athr1 <- freq_athr |> filter(Freq >5)
kellogg.bipartite.edges = lapply(kellogg.coauthors, function(x) {freq_athr1$Var1 %in% x})
kellogg.bipartite.edges = do.call("cbind", kellogg.bipartite.edges) # dimension is number of authors x number of papers
rownames(kellogg.bipartite.edges) = freq_athr1$Var1 #kellogg.coauthors.unique

#new_mat <- subset(kellogg.bipartite.edges, colSums(kellogg.bipartite.edges)>1)
kellogg.mat = kellogg.bipartite.edges %*% t(kellogg.bipartite.edges) #bipartite to unimode
mat = kellogg.mat[order(rownames(kellogg.mat)), order(rownames(kellogg.mat))]

kellogg.statnet = as.network(author.mat, directed = FALSE, names.eval = "edge.lwd", ignore.eval = FALSE)
kellogg.statnet ##view network summary
plot.network(kellogg.statnet, edge.col = "gray", edge.lwd = "edge.lwd", 
             label = " ", 
             label.cex = .5, label.pad = 0, label.pos = 1, jitter = TRUE)

#head(kellogg.statnet$)
jpeg(filename = "Social_network_rtr.jpeg", width = 12, height = 9, units = "in", res = 300)
plot.network(kellogg.statnet, edge.col = "gray", edge.lwd = "edge.lwd", label = " ", label.cex = .5, label.pad = 0, label.pos = 1)
dev.off()

authors.ig1 <- graph_from_adjacency_matrix(mat, mode = "upper", diag = FALSE, 
                                          weighted = TRUE)

mod_athr <-modularity(authors.ig, membership = athr_grp)
deg_cntr <-centr_degree(authors.ig1, mode = "all")
deg_cntr <- deg_cntr$res

bw.tbnet1 <- betweenness(mat) 
V(authors.ig1)$betweenness <- bw.tbnet1 
summary(bw.tbnet1)
l.tbnet <- layout.kamada.kawai(authors.ig1) 
# l.tbnet2 <- layout.kamada.kawai(tbnet2) 
l.tbnet2 <- layout_as_star(authors.ig1) 
                          #weight.edge.lengths = edge_density(mat)/1000)

par(cex.main=2)
plot(groups1, authors.ig1,vertex.label='', layout=l.tbnet, 
     mark.groups = NULL,  #vertex.size = 9, # edge.color = memb.tbnet2, 
     edge.width = 0.8, vertex.size=1+2*sqrt(bw.tbnet1/500))

###### simulate modularity for graphs to compare 
groups1 <-cluster_fast_greedy(authors.ig1)
athr_grp <- groups1$membership
# Initialize vector to store values
sims <- 1000
mod_vals <- rep(0,sims)

# Loop through simulations
for (i in c(1:sims))
{
  # Simulate a configuration model
  config <- sample_degseq(deg_cntr, method = "simple.no.multiple")
  
  # Compute the modularity of the network w/ respect to ZKC groups
  mod_score <- modularity(config, membership = athr_grp)
  
  # Store the modularity value in our vector
  mod_vals[i] <- mod_score
  
}

ggplot(data = as.data.frame(mod_vals), aes(x = mod_vals)) + 
  geom_histogram(binwidth = 0.005110, color = 'black', fill = 'lightblue') +
  geom_vline(xintercept = 0.245, color = 'purple')+
  coord_cartesian(xlim = c(-0.15,0.35))+
  theme_bw()

layout <- create_layout(authors.ig1, layout = 'igraph', algorithm = 'kk')
layout$betweenness <- bw.tbnet1
layout$degree <- deg_cntr

lyt <- attributes(layout)
df <-data.frame(lyt$graph)

layout |> #filter(betweenness >0) |> 
  ggraph(layout = "focus") + 
  geom_edge_link0(colour = "grey70", alpha = 0.3) + 
  geom_node_point( aes(col = log(betweenness/100 + 0.01),alpha = (1/1-0.1*degree),
                      size = 0.1+2*sqrt(betweenness/500)),
                   show.legend = FALSE) +
  # geom_node_label(aes(label = 1:960), size = 0.001,
  #                 repel = TRUE, show.legend = FALSE)+
  scale_color_continuous(type = "viridis")+
  theme_graph() +  ylim(-20, 20) +
  xlim(-20, 20) 

ggsave("Author_network2.jpeg", width = 10, height = 10, units = "in", dpi=300)


#################################################################################################
#################Jounals and subject area

# rtr_jrnl <-rtr_db |> count(Journal)
# 
# write.csv(rtr_jrnl, "Journal_list.csv")
# 
# head(rtr_db$Subject)
# 
# rtr_subject <-capture.output(cat(rtr_db$Subject))
# 
# freq_subject <- as.data.frame(sort(table(unlist(strsplit(rtr_subject, ";"))),      # Create frequency table
#                                decreasing = FALSE))
# head(freq_subject)
# 
# freq_subject |> filter(Freq >10)|>
#   ggplot( aes(x= Freq, y= Var1))+
#   geom_col(aes(fill= Var1))+
#   theme_bw()+
#   labs(y = "Subject Area", x= "Number of papers")+
#   theme(axis.title = element_text(size=16),
#         axis.text  = element_text(size=10),
#         legend.position = "none")
# 
# ggsave("./Documents/Retraction/Subject_area_freq.jpeg", width = 10, height=15, units= "in", dpi=200)
# write.csv(freq_subject, "./Documents/Retraction/subject-list.csv" )

  ########################################################################################
  ### Figure 3 Relationship between Journal IF and number of retractions ----
  
 jif_num <- read.csv("Journal_impact_factor_retraction_all.csv")
 jif_num_fltr <- jif_num |> drop_na(X2022.JIF) ###drop columns with NO IF information
  
  summary(lm(log(n) ~ log(X2022.JIF) -1, data = jif_num_fltr)) ##Fitting the regression
  
  jif_num_fltr |> drop_na(X2022.JIF) |> #plot the regression 
    ggplot(aes(x= log(X2022.JIF), y = log(n)))+
    geom_point(alpha = 0.7) +
    geom_smooth( method = "gam", formula = y ~ (x)-1) + 
    coord_cartesian(ylim = c(0,6.5))+
    labs(x= "2022 Impact Factor", y= "Number of retractions") + 
    annotate("text", x = -0.85, y=5.5 , label = bquote("R^2 == 0.424"), parse = TRUE)+
    annotate("text", x = -0.85, y=5.2 , label = bquote("p <0.001"), parse = TRUE)+
    scale_x_continuous(breaks = c(-1.386,0, 1.61, 2.996,  3.912, 5.011),
                       labels = c( "0.25",  "1", "5", "20", "50", "150"))+
    scale_y_continuous(breaks = c(0, 1.61, 2.996,  4.61, 5.991),
                       labels = c("1", "5", "20", "100", "400")) +
    theme_bw() + 
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size=12)) 

ggsave("JIF_freq_plot.jpeg", width = 8, height=5.5, units= "in", dpi=600)
