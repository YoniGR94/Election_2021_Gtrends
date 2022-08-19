
#############
##libraries##
#############

library(tidyverse)    #for dplayer
library(RColorBrewer)
library(gtrendsR)
library(treemap)      #block map
#library(hrbrthemes)   #maybe delete
library(maps)
library(usmap)
library(streamgraph)
#devtools::install_github("hrbrmstr/streamgraph")

###################################
##Import from Google with gtrends##
###################################

election<-gtrends(c("Naftali Bennett", "Yair Lapid", "Benjamin Netanyahu"), gprop = "web", time = "2021-04-10 2022-08-18")

#google.trends = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
#rownames(google.trends) = google.trends$date
#google.trends$date = NULL

#####################
#Gov interest by day in the world
#####################

Gov<- election$interest_over_time %>%
  select(-gprop,-category) %>% 
  mutate(hits= ifelse(hits== "<1", 0.5, as.numeric(hits)),
  hits= hits/100)

# the world

x_continuous<- as.Date(unique(Gov$date)[seq(1,71,10)],"%Y-%m-%d",tz= '')
x_continuous

world_gg<- ggplot(Gov, aes(x=date, y= hits,colour= keyword) )+
  geom_line(alpha= 0.5,size= 0.8)+
  theme_classic()+
  labs (x= "date", y= "Rate",
        caption = "data: Google Trends, 10/4/2021 - 18/8/2022",
        title= "Interest in Israel Government Worldwide")+
  theme(panel.grid =  element_blank(),
        axis.text.y =  element_text(angle = 0),
        axis.text.x =  element_text(angle = 90),
        panel.grid.major.y = element_line(colour = "grey40",size = 0.5),
        panel.grid.major.x = element_line(colour = "grey40",size= 0.25),
        panel.grid.minor.x = element_line(colour = "grey70", size = 0.15),
        axis.line = element_blank(),
        legend.position="bottom",
        panel.background = element_rect(fill = "lightblue",
                                              colour = "lightblue",
                                              size = 0.5, linetype = "solid")
  )+
  scale_colour_discrete(name="")+
  scale_y_continuous(labels=scales::percent_format(accuracy = 5L), breaks = seq(0,1, by= 0.2))+
  geom_vline(xintercept=as.Date("2021-06-13" ), colour= "darkblue")+
  scale_fill_brewer(palette="Dark2")


world_gg
ggsave(world_gg,device = "png", filename = "Interest in Israel Government Worldwide")

#as.Date("2021-06-07 17:00:00 GMT","%Y-%m-%d", tz= '')

by_share <- Gov  %>%
  group_by(date, keyword) %>%
  summarise(n = sum(hits)) %>%
  mutate(percentage = n / sum(n))


ggplot(by_share, aes(x=date, y= percentage,fill= keyword )) +
  labs(title="Relative Search Popularity by Candidate",
       caption = "relative search popularity by candidate. from Google Trends, 10/4/2021 - 18/8/2022")+
  geom_area()+
  scale_y_continuous(labels=scales::percent)+
  theme(legend.position="bottom",
        legend.title = element_text())+
  scale_fill_discrete(name="")+ylab("")+
  scale_fill_brewer(palette="Set2")

  


#############
##box world##
#############

election$interest_by_country %>%
  select(location,hits,keyword) %>%
  filter(keyword ==  "Naftali Bennett" ) %>%
  mutate(hits= ifelse(hits== "<1", 0, as.numeric(hits)/100)) %>% 
  filter( hits >= 0.05) %>% 
  treemap(title = "search for Bennet by Country",
        index="location",
        vSize="hits",
        type="index")

election_streamgraph<- Gov %>% 
  streamgraph(key="keyword", value="hits", date="date",order = "inside-out",
            offset="silhouette", width="600px", height="350px") %>%
  sg_legend(show=TRUE, label="Choose Continent: ") 

election_streamgraph

ggsave(election_streamgraph,device = "png", filename = "election_streamgraph as png")


###########
# the US
###########

election_US<- gtrends(c("Naftali Bennett", "Yair Lapid", "Benjamin Netanyahu"), gprop = "web",
                      time = "2021-04-10 2022-08-18", geo = "US")


Gov_US<- election_US$interest_over_time %>%
  #mutate(n_date= seq(1,7,1/(N-1)) ) %>%
  mutate(hits= ifelse(hits== "<1", 0.5, as.numeric(hits))) %>%
  mutate(hits= hits/100)

US_gg<- ggplot(Gov_US, aes(x=date, y= hits,colour= keyword ) )+
  geom_line(alpha= 0.5,size= 0.8)+
  theme_gray()+
  labs (x= "date", y= "Rate",
        caption = "data: Google Trends, 10/4/2021 - 18/8/2022",
        title= "Interest in Israel Government in the US")+
  theme(panel.grid =  element_blank(),
        axis.text.y =  element_text(angle = 0),
        axis.text.x =  element_text(angle = 90),
        panel.grid.major.y = element_line(colour = "grey40",size = 0.5),
        panel.grid.major.x = element_line(colour = "grey40",size= 0.25),
        panel.grid.minor.x = element_line(colour = "grey70", size = 0.15),
        axis.line = element_blank(),
        legend.position="bottom",
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid")
  )+
  scale_colour_discrete(name="")+
  scale_y_continuous(labels=scales::percent_format(accuracy = 5L), breaks = seq(0,1, by= 0.2))+
  #scale_x_continuous(breaks = seq.Date(as.Date("2021-06-08"),as.Date("2021-06-14"),by = 14))
  geom_vline(xintercept =  as.Date("2021-06-13 17:00:00 GMT","%Y-%m-%d"), colour= "darkblue")

US_gg

# the US map

states <- us_map("state")

state_hit<- election_US$interest_by_region %>% 
  filter(keyword== "Naftali Bennett",location !='District of Columbia' ) %>% 
  mutate(hits= ifelse(is.na(hits),0,hits)) %>% 
  rename("state"= 1, "Bennett"= 2)


#state_hit<- subset(state_hit, state != "District of Columbia")

state_hit_map<- state_hit %>%
  mutate(state = tolower(state)) %>%
  mutate(Bennett= Bennett/max(Bennett)) %>%
  select(state, "Bennett") %>% # -> my_df
  as.data.frame()

plot_usmap(data = state_hit_map, values = "Bennett", color = "black") + 
  scale_fill_continuous(
    low = "gray90", high = "red", name = "Bennett", label = scales::percent_format( breaks = seq(0,1, by= 0.2)))+
  theme(legend.position = "right",
        legend.box.background = element_rect(colour = "black"))+
  labs(title= "Interest in Naftali Bennett in the US by State",
       caption = "data: Google Trends, 10/4/2021 - 18/8/2022")


