#setwd("~/nealsmith/")

rm(list=ls(all=TRUE))

library(tidyverse)

gwcodes2017 <- readr::read_csv("./groundwater/data-raw/waterquality/2017/nealsmithgroundwatercodes2017.csv", skip = 3,
                               col_names = c("year", "month", "site", "catch", "pos", "source", "date", "sampleid")) %>%
  select(-source, -date) %>%
  mutate(pos = ifelse(pos == "Sum", "sum", pos),
         pos = ifelse(pos == "Toe", "toe", pos),
         pos = ifelse(pos == "Toe Flume", "toeflume", pos),
         pos = ifelse(pos == "Toe Top", "toetop", pos),
         pos = ifelse(pos == "toe flume", "toeflume", pos),
         pos = ifelse(pos == "toe top", "toetop", pos))

gwdata2017 <- readr::read_csv("./groundwater/data-raw/waterquality/2017/nealsmithgroundwater2017.csv", skip=3, 
                              col_names=c("sampleid", "no3date", "no3mgl", "drpdate", "drpmgl")) %>%
  select(-no3date, -drpdate) %>%
  left_join(gwcodes2017) %>%
  filter(site!="NA") %>%
  mutate(trt = "allcrop",
         site = ifelse(site=="i", "I", site),
         site = ifelse(site=="b", "B", site),
         site = ifelse(site=="w", "W", site),
         sitecatch = paste(site, catch, sep = "")) %>%
  filter(pos != "toeflume") %>%                         #getting rid of odd wells and leaving only sum and toe pos
  filter(pos != "toetop")

gwdata2017$trt[gwdata2017$sitecatch=="B1" | gwdata2017$sitecatch=="W1" | gwdata2017$sitecatch=="I2"] <- "tentoe"
gwdata2017$trt[gwdata2017$sitecatch=="I1" | gwdata2017$sitecatch=="B2" | gwdata2017$sitecatch=="B5"] <- "tenstrip"
gwdata2017$trt[gwdata2017$sitecatch=="W2" | gwdata2017$sitecatch=="B3" | gwdata2017$sitecatch=="B4"] <- "twentystrip"

gwdata2017 <- gwdata2017 %>%
  group_by(trt, month) %>%
  summarise_at(c("no3mgl", "drpmgl"), mean, na.rm=T) %>%
  mutate(no3mgl = format(round(no3mgl, 2), nsmall = 2),
         drpmgl = format(round(drpmgl, 2), nsmall = 2)) 

no3trt <- gwdata2017 %>%
  select(-drpmgl) %>%
  spread(trt, no3mgl) %>%
  select(month, allcrop, tentoe, tenstrip, twentystrip)


drptrt <- gwdata2017 %>%
  select(-no3mgl) %>%
  spread(trt, drpmgl) %>%
  select(month, allcrop, tentoe, tenstrip, twentystrip)

write.csv(no3trt, "./groundwater/no3trt.csv")
write.csv(drptrt, "./groundwater/drptrt.csv")

#this is a test