library(readxl)
library(tidyverse)
library(lubridate)
library(cowplot)
library(igraph)

### Load the data from the retraction watch database  
rtr_db <-read_xlsx("./Retraction/Data_life science_final.xlsx")
head(rtr_db)
summary(rtr_db)

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

###### Retraction Pattern across countries ----

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
agg_sub_list <- read_xlsx("./Retraction/subject_list_final.xlsx") |> 
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

###### Aggregated list of reasons -----
rsn_freq_new<-read_xlsx("./Retraction/Reason_frequency_final.xlsx") |>
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

#### Number of Authors across the studies ----
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

### Figure 3 Relationship between Journal IF and number of retractions ----

jif_num <- read.csv("./Retraction/Journal_impact_factor_retraction_all.csv")
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

ggsave("./Figure/Final_figure_3.jpeg", width = 8, height=5.5, units= "in", dpi=600)

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
