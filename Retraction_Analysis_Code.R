library(readxl)
library(tidyverse)
library(lubridate)
library(cowplot)
library(igraph)
library(statnet)
library(ggraph)

### Load the data downloaded from the retraction watch database  
rtr_db <-read_csv("./Retraction/Retraction Watch Data_ Bio Science.csv")
head(rtr_db)
summary(rtr_db)
glimpse(rtr_db)

###### Publication Time of the retracted studies ----

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
  coord_cartesian(ylim =c(0,550))+
  theme_bw() + theme(axis.text = element_text(size=12),
                     axis.title.x = element_text(size=16))

cowplot::plot_grid(rtr_year_ptrn, rtr_year_diff,     labels = c("A", "  B"))
ggsave("./Figure/Final_figure_1.jpeg", width = 9, height = 4, dpi = 300, units = "in")

## Retraction Pattern across countries ----

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
#ggsave(cntry_freq, file = "Country_frequency_new.jpeg", width = 8, height = 6, units = "in", dpi=200)

###### Aggregated subject list summary -----
agg_sub_list <- read_xlsx("./Retraction/subject_list_fig2B.xlsx") |> 
  drop_na(`new category`) |> 
  arrange(desc(Freq))

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

## Aggregated list of reasons -----
rsn_freq_new<-read_xlsx("./Retraction/Reason_frequency_fig2C.xlsx") |>
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

#ggsave(rsn_db_plt, filename = "Reason_area_freq_new.jpeg", width = 7, height=5, units= "in", dpi=300)

### Number of Authors across the studies ----
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
#ggsave(num_atr, file = "./Documents/Retraction/Num_authors.jpeg", width = 12, height = 8, units = "in", dpi=200)  

#### Final Figure 2 ----
cowplot::plot_grid(cntry_freq, agg_sum_plt, rsn_db_plt, num_atr, 
                   labels = c("A", "B", "C", "D"), ncol =2)

ggsave("./Figure/Final_figure_2.jpeg", width=14, height =9, units = "in", dpi=300)

## Relationship between Journal IF and number of retractions ----

jif_num <- read.csv("./Retraction/JIF_fig3.csv")
jif_num_fltr <- jif_num |> drop_na(X2022.JIF) ###drop columns with NO IF information

summary(lm(log(n) ~ log(X2022.JIF) -1, data = jif_num_fltr)) ##Fitting the regression

### Figure 3 final plot ----
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

ggsave("./Figure/Final_figure_3.jpeg", width = 8, height=5.5, units= "in", dpi=600)

## Network of authors of the retracted articles ----

authr_db <-capture.output(cat(rtr_db$Author)) ###create the author database
freq_athr <- as.data.frame(sort(table(unlist( strsplit(authr_db, ";"))),      # Create frequency table
                                decreasing = TRUE))

rtr_db$num_authr <- str_count(rtr_db$Author, ";") +1 ##calculate the number of authors for each article
rtr_db_filatr <- rtr_db |> filter(num_authr >1) ##filter articles with only one author
rtr.coauthors = sapply(as.character(rtr_db_filatr$Author), strsplit, ";")
#create record_id wise author list
rtr.coauthors <- cbind(rtr_db$`Record ID`, unlist(sapply(as.character(rtr_db$Author), strsplit, ";")))
rtr.coauthors <-  sapply(as.character(rtr_db$Author), strsplit, ";")
coauthors = lapply(rtr.coauthors, trimws)

###create unique oauthor lit
coauthors.unique = unique(unlist(coauthors))[order(unique(unlist(coauthors)))]

freq_athr1 <- freq_athr |> filter(Freq >5) ##filter author with less than 5 retractions
##We have also done the same analysis with less than 3 authors
bipartite.edges = lapply(coauthors, function(x) {freq_athr1$Var1 %in% x})
bipartite.edges = do.call("cbind", bipartite.edges) # dimension is number of authors x number of papers
rownames(bipartite.edges) = freq_athr1$Var1 #coauthors.unique

#new_mat <- subset(kellogg.bipartite.edges, colSums(kellogg.bipartite.edges)>1)
author.mat = bipartite.edges %*% t(bipartite.edges) #bipartite to unimode
##create the author matrix of the retracted articles with the order from highest to lowest 
mat = author.mat[order(rownames(author.mat)), order(rownames(author.mat))]

### Convert the matrix into igraph format
authors.ig1 <- graph_from_adjacency_matrix(mat, mode = "upper", diag = FALSE, 
                                           weighted = TRUE)

#mod_athr <-modularity(authors.ig, membership = athr_grp)
deg_cntr <-centr_degree(authors.ig1, mode = "all")
deg_cntr <- deg_cntr$res

### Calculate the betweenness value of the graph
bw.tbnet1 <- betweenness(mat) 
V(authors.ig1)$betweenness <- bw.tbnet1 
summary(bw.tbnet1)

### Figure 4 network plot ----
###Create layout for the plot and final plot 
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

ggsave("./Figure/Final_figure_4.jpeg", width = 10, height = 10, units = "in", dpi=300)

## Relation between retraction and cultures ----
country_cltr <- read_csv("./Retraction/Country_culture_fig5.csv") |> drop_na(Individualism)
head(country_cltr)
#colnames(country_cltr) <- gsub(" \r\n", "_", colnames(country_cltr))

### Individual plot with different variables
lng_ornt_plot <- ggplot(country_cltr,aes(x= Longterm_orientation, y= log(Freq)))+
  geom_point()+
  geom_smooth(method = "lm")+
  annotate("text", x = 25, y=8.5 , label = bquote("R^2 == 0.19"), parse = TRUE)+
  annotate("text", x = 25, y= 8 , label = bquote("p <0.001"), parse = TRUE)+
  theme_bw() + labs(y = "Log(Number of retractions)", x= "Longterm orientation")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")

indvlsm_plot <- ggplot(country_cltr,aes(x= Individualism, y= log(Freq)))+
  geom_point()+
  geom_smooth(method = "lm")+
  annotate("text", x = 25, y=8.5 , label = bquote("R^2 == 0.05"), parse = TRUE)+
  annotate("text", x = 25, y=8 , label = bquote("p == 0.087"), parse = TRUE)+
  theme_bw() + labs(y = "Log(Number of retractions)", x= "Individualism")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")

pow_dist_plot <- ggplot(country_cltr,aes(x= `Power distance`, y= log(Freq)))+
  geom_point()+
  geom_smooth(method = "lm")+
  annotate("text", x = 25, y=8.5 , label = bquote("R^2 == 0.014"), parse = TRUE)+
  annotate("text", x = 25, y=8 , label = bquote("p == 0.36"), parse = TRUE)+
  theme_bw() + labs(y = "Log(Number of retractions)", x= "Power distance")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")

mtvn_suc_plot <- ggplot(country_cltr,aes(x= `Motovation_towards achivement and sucess`, y= log(Freq)))+
  geom_point()+
  geom_smooth(method = "lm")+
  annotate("text", x = 25, y=8.5 , label = bquote("R^2 == 0.006"), parse = TRUE)+
  annotate("text", x = 25, y=8 , label = bquote("p == 0.54"), parse = TRUE)+
  theme_bw() + labs(y = "Log(Number of retractions)", x= "Motivation towards achivement and sucess")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=10),
        legend.position = "none")

#### Final figure 5 Culture plot with different countries -----
plot_grid( lng_ornt_plot, indvlsm_plot,pow_dist_plot, mtvn_suc_plot,
           ncol = 2, labels = c("A", "B", "C", "D"), label_size = 20)
ggsave(filename = "./Figure/Final_figure_5.jpeg", width = 12, height = 8, units = "in", dpi=300)

###### Number of reasons for retraction ----

rtr_db$num_reason <-str_count(rtr_db$Reason, ";")

#### Supplementary figure S1 ----
num_rsn <- rtr_db |> count(num_reason) |>
  ggplot( aes(x= as.factor(num_reason), y = n))+
  geom_col(fill= "blue")+
  theme_bw()+
  labs(x= "Number of reasons", y= "Number of papers")+
  theme(axis.title = element_text(size=16),
        axis.text  = element_text(size=12))

ggsave(num_rsn, file = "./Figure/Final_figure_S1.jpeg", width = 12, height = 8, units = "in", dpi=200)  
