library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways


####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('data',full.names=T)

#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')


ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')

# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))

##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

full_wide <- spread(full_long,key="data",value="value") %>%
  filter_if(is.numeric,all_vars(!is.na(.))) %>%
  mutate(month = month(DateTime),
         year = year(DateTime))

summer_only <- dplyr::filter(full_wide, month %in% c(6,7,8,9))

ggplot(summer_only,aes(x=ndmi,y=ndvi,color=site))+ # how does moisture affect greenness?
  geom_point() + 
  theme_few()+ # remove gray background
  theme(legend.position = c(0.8,0.8))


## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 

# find summer NDVI by year and site
summer_ndvi <- dplyr::filter(full_wide, month %in% c(6,7,8,9)) %>%
  select(ndvi,year,site) %>%
  group_by(year) %>%
  summarize(ann_ndvi=mean(ndvi))

# find winter NDSI by year and site
winter_ndvi <- dplyr::filter(full_wide, month %in% c(1,2,3,4)) %>% # filter by season
  select(ndsi,year,site) %>% 
  group_by(year) %>%
  summarize(ann_ndsi=mean(ndsi)) # mean annual value by site

ndsi2ndvi_ann <- dplyr::full_join(summer_ndvi,winter_ndsi,by=c("year","site"))

# plot of ndvi and ndsi by year and site
ggplot(ndsi2ndvi_ann, aes(x=ann_ndsi,y=ann_ndvi))+ # how does moisture affect greenness?
  geom_point() + 
  theme_few()+ # remove gray background
  theme(legend.position = c(0.8,0.2))+
  labs(x="Mean Annual NDSI",y="Mean Annual NDVI",
       title="Vegetation as a function of snow cover")

## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

# add treatment (pre-burn & post-burn)
ndsi2ndvi_pre_post <- ndsi2ndvi_ann %>%
  mutate(treatment=cut(year,breaks=c(0,2003,2020),
                       labels=c("pre-burn","post-burn")))

# plot of ndvi and ndsi by site and treatment
ggplot(ndsi2ndvi_pre_post, aes(x=ann_ndsi,y=ann_ndvi,color=treatment))+ # how does moisture affect greenness?
  geom_point() + 
  theme_few()+ # remove gray background
  labs(x="Mean Annual NDSI",y="Mean Annual NDVI")+
  facet_wrap("site")

## End code for question 3 


###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 

# DF containing ndvi, month, site, and treatment
full_ndvi <- full_wide %>%
  select(ndvi,year,month,site) %>%
  mutate(treatment=cut(year,breaks=c(0,2003,2020),
                       labels=c("pre-burn","post-burn")))

# DF of only pre-burn times
preburn_ndvi <- full_ndvi %>%
  filter(treatment=="pre-burn") %>%
  group_by(month) %>%
  summarize(mean_ndvi = mean(ndvi))

# DF of only burned area post-burn
postburn_ndvi <- full_ndvi %>%
  filter(treatment=="post-burn",site=="burned") %>%
  group_by(month) %>%
  summarize(mean_ndvi = mean(ndvi))

# plot ndvi pre-burn and post-burn burned area together by month
ggplot(preburn_ndvi, aes(x=month,y=mean_ndvi))+
  geom_point(aes(color="All Sites Pre-Burn"))+
  geom_point(data=postburn_ndvi,
             mapping=aes(x=month,y=mean_ndvi,color="Burned Site Post-Burn"))+
  theme_few()+
  scale_x_continuous(breaks =1:12)+
  labs(x="Month",y="Mean NDVI",title="Greenness by Month",colour="Treatment and Sites")

# August is the greenest month at both sites pre and post-burn

## End code for question 4 -----------------

##### Question 5 #####

#What month is the snowiest on average?

# create DF containing ndsi & month
full_ndsi <- full_wide %>%
  select(ndsi,month) %>%
  group_by(month) %>%
  summarise(mean_ndsi=mean(ndsi))

snowy_mon <- top_n(full_ndsi,1,mean_ndsi)
print(paste("The snowiest month is",snowy_mon$month),sep="")

# plot NDSI by month
ggplot(full_ndsi,aes(x=month,y=mean_ndsi))+
  geom_point()+
  theme_few()+
  scale_x_continuous(breaks =1:12)+
  labs(x="month",y="Mean NDSI")

## End code for question 5 -----------------

