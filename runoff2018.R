# setwd("~/nealsmith/")

rm(list=ls(all=TRUE))

library("dplyr")
library("tidyr")
#library("ggplot2")

# Rain

rain <- readr::read_csv("./rain/nealsmithrain2018.csv", skip = 8, col_names = c("site", "date", "rainin")) %>%
  mutate(date = gsub(" CST", "", date)) %>%
  select(-site) %>%
  filter(date>="05/01/2018 00:00")%>%
  filter(date<="11/01/2018 00:00")

rain <- rain %>%
  mutate(raininadj = rain$rainin-4.91) %>%
  separate(date, c("date", "time"), sep = "\\ ") %>%
  arrange(date) %>%
  mutate(rainlasthr = raininadj - lag(raininadj, default=first(raininadj)),
         rainlast24 = zoo::rollapply(rainlasthr, 24, sum, align = "right", fill = 0)) %>%
  separate(time, c("hour"))

rain$date <- as.POSIXct(rain$date, format = "%m/%d/%Y")
rain <- rain %>%
  mutate(date = as.character(date))

# 
# hrandr = hrandr %>%
#   group_by(property, treatment, year) %>%
#   arrange(property, treatment, year, month, day) %>%
#   mutate(rain_last_24 = zoo::rollapply(rain_hourly, 24, sum, align = "right", fill = 0))
# ```

# Flow
############################THIS IS ALTERED FROM JARAD'S RUNOFF-READ FILE
setwd("runoff")

#IMPORTING RUNOFF DATA
my_runoff_csv = function(f, into) {
  readr::read_csv(f, 
                  skip = 11, 
                  col_types = "ccnnn") %>%
    mutate(file = f) %>%
    tidyr::separate(file, into)
}

read_dir = function(path, pattern, into) {
  files = list.files(path = path,
                     pattern = pattern,
                     recursive = TRUE,
                     full.names = TRUE)
  plyr::ldply(files, my_runoff_csv, into = into)
}


runoff <- read_dir(path="data-raw",
                   pattern = "*.csv",
                   into = c("data", "raw", "year", "site", "csv")) %>%
  
  mutate(Date = lubridate::parse_date_time(Date,
                                                orders = c("mdY"))) %>%
  
  select(Date, Time, LEVEL, TEMPERATURE, site) %>%
  arrange(site, Date, Time) %>%
  rename(date = Date, time = Time, levelm = LEVEL, temp = TEMPERATURE) %>%
  filter(date>= "2018-04-01") %>%
  filter(date <= "2018-11-01")
  
runoff$time <- as.POSIXct(runoff$time, format = "%I:%M:%S %p")
  
runoff <- runoff %>%
  separate(time, c("fakedate", "time"), sep = " ") %>%
  select(-fakedate) %>%
  arrange(site, date, time) %>%
  separate(time, c("hour", "min", "sec"), sep = ":")

###END OF jARAD'S RUNOFF-READ SCRIPT

#NEED TO ADJUST LEVELM VALUES BY THE FIRST FEW VALUES OF EACH SITE... I FILLED THE STILLING WELL WHEN DEPLOYING THE PROBES
runoff1 <- runoff %>%
  group_by(site) %>%
  filter(date<"2018-05-02", hour>"15") %>%
  summarise_at(c("levelm"), mean, na.rm=F) %>%
  rename(leveladj = levelm)

runoff <- runoff %>%
  left_join(runoff1) %>%
  mutate(levelm = levelm-leveladj)

#CHANGING NEGATIVE LEVELM'S TO 0
runoff$levelm[runoff$levelm<0] <- 0

#####THIS IS THE CONVERSION EQUATION
# FOR 2.0 H FLUME, LPS = 0.022285358-0.55496382*(LEVELM^0.5)+125.5275778*(LEVELM^1.5)+939.5717311*(LEVELM^2.5)
# FOR 2.5 H FLUME, LPS = 0.042446953-0.90725263*(levelm^0.4)+108.676075*(levelm^1.4)+937.5943603*(levelm^2.5)

runoff <- runoff %>%
  mutate(lps = ifelse(site=="i2" | site=="w3",
                      0.042446953-0.90725263*(levelm^0.4)+108.676075*(levelm^1.4)+937.5943603*(levelm^2.5), 
                      0.022285358-0.55496382*(levelm^0.5)+125.5275778*(levelm^1.5)+939.5717311*(levelm^2.5)))

#AT VERY LOW LEVELS, THERE CAN BE NEGATIVE VALUES FROM THE CONVERSION EQUATIONS
runoff$lps[runoff$lps<0] <- 0

#CONVERTING LPS TO GALLONS PER 5 MINUTES
runoff <- runoff %>%
  mutate(gp5m = 5*(lps*.264172)*60)

runoff$gp5m <- format(round(runoff$gp5m, 2), nsmall = 2)

runoff$gp5m[runoff$gp5m == "NaN"] <- 0
runoff$gp5m <- as.numeric(runoff$gp5m)

runoffhr <- aggregate(runoff$gp5m, by=list(runoff$site, runoff$date, runoff$hour), FUN=sum, na.rm=FALSE) %>%
  rename(site = Group.1, date = Group.2, hour = Group.3, gphr = x) %>%
  mutate(date = as.character(date))%>%
  merge(rain, by=c("date", "hour"), all=TRUE, sort = TRUE) %>%
  arrange(site, date, hour)

runoffhr$gphrclipped <- runoffhr$gphr
runoffhr$gphrclipped[runoffhr$rainlast24 == 0] <- 0
  
runoffhr <- runoffhr %>%
  left_join(readr::read_csv("../sitenamesandwatershedsizes.csv")) %>%
  group_by(site) %>%
  mutate(
    cumulative_flowin = cumsum(gphrclipped) * 231 / # convert gpm to in^3
           (acres * 6.273e6) )  # normalize by watershed area
                                    # after converting acres to square inches

runoffhr$cumulative_flowin <- format(round(runoffhr$cumulative_flowin, digits = 2))
runoffhr <- filter(runoffhr, site!='NA' & date<='2018-10-31')
runoffhr$cumulative_flowin <- as.numeric(runoffhr$cumulative_flowin)


runday <- runoffhr %>%
  group_by(site, date) %>%
  summarise_at(c("cumulative_flowin"), sum, na.rm=T)

#NOT SURE WHAT TO DO AFTER HERE...RUNDAY END TOTALS ARE STILL HIGH FOR AT LEAST SEVERAL SITES 


#SINCE sites b6 and i3 so high, we are throwing them out for the trtrunday calc's
trtrunday <- runday %>%
  filter(site!="b6") %>%
  filter(site!="i3") %>%
  mutate(trt = "allcrop")

trtrunday$trt[trtrunday$site=="b1" | trtrunday$site=="w1" | trtrunday$site=="i2"] <- "tentoe"

trtrunday <- trtrunday %>%
  group_by(trt, date) %>%
  summarise_at(c("cumulative_flowin"), mean, na.rm=T) %>%
  mutate(cumulative_flowin = format(round(cumulative_flowin, 2), nsmall = 2)) %>%
  mutate(cumulative_flowin = as.numeric(cumulative_flowin),
         cumulative_cm = cumulative_flowin*2.54) %>%
  select(-cumulative_flowin) %>%
  spread(trt, cumulative_cm) %>%
  select(date, allcrop, tentoe)


#THIS IS FOR SEEING THE FINAL RUNOFF INCHES BY SITE
runtotal <- runday %>%
  filter(date=="2018-09-17")
# write.csv(test, file= "./2018runofftotalinchesbysite.csv")

rainsummary <- readr::read_csv("../rain/nealsmithrain2018.csv", skip = 8, col_names = c("site", "date", "rainin")) %>%
  mutate(date = gsub(" CST", "", date)) %>%
  select(-site) %>%
  separate(date, c("date", "time"), sep = "\\ ") %>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) %>%
  filter(date>="2018-05-01") %>%
  filter(date<"2018-09-17") %>%
  filter(time=="23:48") %>%
  mutate(raincm = rainin*2.54)
# 
# write.csv(trtrunday, file= "./trtrunoffsummary.csv" )
# write.csv(rainsummary, file="./raindaysummary.csv")


# Gio's script for graphs -------------------------------------------------


library(tidyverse)
library(lubridate)

runoffhr %>%
  filter(site == "b6") %>%    # b1, b6, i2, i3, w1, w3
  ungroup() %>%
  select(date, hour, raininadj, rainlasthr, rainlast24, gphr, gphrclipped) %>%
  mutate(timestamp = ymd_h(paste(date, hour), tz = "UTC")) %>%
  filter(month(timestamp) < 11) %>%
  fill(rainlast24) %>%
  mutate(background = ifelse(rainlast24 > 0, timestamp, NA),
         background = as_datetime(background)) %>%
  mutate(xstrat = background,
         xend = lead(background),
         xstrat = ifelse(is.na(xend), NA, xstrat) %>% as_datetime(),
         background = NULL) %>%
  ggplot(aes(x = timestamp)) +
  geom_rect(aes(xmin = xstrat, xmax = xend, ymin = 0, ymax = 1000), alpha = 0.25, fill = "grey50") +
  geom_step(aes(y = 1000 - rainlasthr * 700), colour = 'dodgerblue2') + 
  geom_hline(yintercept = 1000, colour = "grey90") +
  geom_line(aes(y = gphr), colour = "grey20", alpha = 0.75, size = 1) +
  geom_line(aes(y = gphrclipped), colour = "orange", size = 1) +
  theme_light() +
  labs(title = "SITE b6", x = NULL, y = "Surface Runoff (gphr)\nRainfall (not to scale)") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

test1 <- runoff %>% 
  filter(site=="b6", temp>5, date>"2017-08-23") %>%
  filter(date<"2017-08-24")

qplot(test1$temp, test1$levelm, test1)

