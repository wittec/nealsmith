# setwd("~/nealsmith/")

rm(list=ls(all=TRUE))

library("dplyr")
library("tidyr")
#library("ggplot2")

# Rain

rain <- readr::read_csv("./rain/nealsmithrain2017.csv", skip = 8, col_names = c("site", "date", "rainin")) %>%
  mutate(date = gsub(" CST", "", date)) %>%
  select(-site) %>%
  filter(date>="07/10/2017 00:00")%>%
  filter(date<="11/01/2017 00:00")

rain <- rain %>%
  mutate(raininadj = rain$rainin-10.34) %>%
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
                   into = c("data", "raw", "site", "csv")) %>%
  
  mutate(Date = lubridate::parse_date_time(Date,
                                                orders = c("mdY"))) %>%
  
  select(Date, Time, LEVEL, TEMPERATURE, site) %>%
  arrange(site, Date, Time) %>%
  rename(date = Date, time = Time, levelm = LEVEL, temp = TEMPERATURE) %>%
  filter(date>= "2017-07-10") %>%
  filter(date <= "2017-11-02")
  
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
  filter(date<"2017-07-11") %>%
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
runoffhr <- filter(runoffhr, site!='NA' & date<='2017-10-31')
runoffhr$cumulative_flowin <- as.numeric(runoffhr$cumulative_flowin)


runday <- runoffhr %>%
  group_by(site, date) %>%
  summarise_at(c("cumulative_flowin"), sum, na.rm=T)

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
  filter(date=="2017-10-31")
write.csv(test, file= "./runofftotalinchesbysite.csv")

rainsummary <- readr::read_csv("../rain/nealsmithrain2017.csv", skip = 8, col_names = c("site", "date", "rainin")) %>%
  mutate(date = gsub(" CST", "", date)) %>%
  select(-site) %>%
  separate(date, c("date", "time"), sep = "\\ ") %>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) %>%
  filter(date>="2017-04-01") %>%
  filter(date<"2017-11-01") %>%
  filter(time=="23:48") %>%
  mutate(raincm = rainin*2.54)

write.csv(trtrunday, file= "./trtrunoffsummary.csv" )
write.csv(rainsummary, file="./raindaysummary.csv")


#Gio's script for graphs

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



#########################BELOW IS NOT USED
























ggplot(flow, aes(x = date_time, y = cumulative_flow, 
              group = watershed, linetype = treatment)) +
  geom_line(size=3) + 
  facet_grid(site~year, scales='free') + 
  theme_bw()
  
# ggplot(flow, aes(x = date_time, y = cumulative_flow, 
#               group = watershed, linetype = treatment)) +
#   geom_line(size=3) + 
#   facet_grid(site ~ year, scales='free') + 
#   theme_bw()

# Combine flow and rain
rain <- rain %>%
  mutate(treatment = "rain",
         watershed = paste(site,"_rain", sep=""),
         y = cumulative_rain)

flow <- flow %>%
  mutate(y = cumulative_flow)

wnames <- data.frame(site = c("arm","eia","marsh","mcnay","rhodes","spirit","white","worle"),
                     full = c("Armstrong","E. IA Airport","Marshaltown","McNay",
                              "Rhodes","Spirit Lake ","Whiterock","Worle"))

wnames <- wnames %>%
  mutate(site = as.character(site))


d <- bind_rows(flow,rain) %>%
  mutate(treatment = factor(treatment, 
                            levels = c("rain","control","treatment"))) %>%
  left_join(wnames)



# Customized colors
colorscale <- c(rain = "blue", 
                control = "black",
                treatment = "seagreen")
linescale <- c(rain = "dotted",
               control = "solid",
               treatment = "dashed")




g <- ggplot(d, aes(x = date_time, 
                   y = y, 
                   group = watershed, 
                   linetype = treatment,
                   color = treatment)) +
  geom_line() + 
  facet_grid(full~year, scales='free_x') + 
  labs(x = '',  
       y = 'Cumulative rainfall and runoff (inches)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/runoff.jpg", plot=g, width = 6, height=8)

# 
# # Multiply flow by 10
# g <- ggplot(d %>% left_join(wnames), aes(x = date_time, 
#                                          y = y*ifelse(treatment != "rain", 10, 1), 
#                                          group = watershed, 
#                                          linetype = treatment,
#                                          color = treatment)) +
#   geom_line() + 
#   facet_grid(full~year, scales='free_x') + 
#   labs(x = '',  
#        y = 'Cumulative runoff (x10) and rainfall in inches') + 
#   scale_color_manual(values = colorscale) +
#   theme_bw() +
#   theme(legend.position = "bottom",
#         legend.title    = element_blank())
# 
# ggsave(filename = "runoff_x10.jpg", plot=g, width = 6, height=8)
# 
# 


###################################################
# Sediment
###################################################
sed <- STRIPS2Helmers::runoff %>%
  
  mutate(treatment = ifelse(grepl("ctl", watershed), "control", "treatment"),
         site = gsub("ctl", "", watershed),
         site = gsub("trt", "", site),
         year = lubridate::year(date_time),
         watershed = as.character(watershed),
         sampleID = as.character(sampleID)) %>%
  
  left_join(myrain, by=c("date_time", "site", "year")) %>%
  # filter(!is.na(flow), !is.na(rain_m)) %>%
  group_by(watershed,year) %>%
  do(HelmersLab::clip_flow(.))  # should we be using do() here?

# mutate(sampleID = as.numeric(sampleID)) %>%
# group_by(watershed,year) %>%

# Remove any watershed-year where no sampleIDs exist
# mutate(anySampleID = any(!is.na(sampleID))) %>%
# filter(anySampleID) %>%

sed2 <- sed %>%
  group_by(watershed,year) %>%
  #HelmersLab::spread_sampleID() # should we be using do() here?

  do(HelmersLab::spread_sampleID(.)) %>% 
  filter(!is.na(sampleID), !is.na(flow)) %>%
  left_join(STRIPS2Helmers::water_quality, by = "sampleID") %>%
  tidyr::gather(analyte, value,
                `Nitrate + nitrite (mg N/L)`,
                `Orthophosphate (mg P/L)`,
                `TSS (mg/L)`) %>%
  left_join(readr::read_csv("../data-raw/sitenamesandwatershedsizes.csv")) %>%
  mutate(valueload = ((value*flow*5)/453592.37)/acres) %>%  #this converts the mg/L units of values into lbs/acre
  group_by(watershed, year, analyte) %>%
  filter(!is.na(value)) %>%
  mutate(cumulative = cumsum(valueload)) %>%
  ##########################################
#ADDING THIS STUFF IN


#############################################
  left_join(wnames)



########################################
#testing stuff below

test1 <- sed2 %>%
  group_by(watershed, analyte, year)%>%
  full_join(test7)%>%
  arrange(watershed, analyte, date_time)%>%
  na.locf(, fromlast = T)
  
  #complete(watershed, date_time)


test2 <- as.data.frame(unique(sed2$date_time)) %>%
  rename(date_time = 'unique(sed2$date_time)')

test8 <- as.data.frame(seq(start(sed2$date_time, end(sed2$date_time, "5 min"))))


az <- as.data.frame(zoo(1:6))

bz <- as.data.frame(zoo(c(2,NA,1,4,5,2)))
bz1 <- na.locf(bz)
bz2 <- na.locf(bz, fromLast = TRUE)

z <- as.data.frame(zoo(c(0.007306621, 0.007659046, 0.007681013,
           0.007817548, 0.007847579, 0.007867313),
         as.Date(c("1993-01-01", "1993-01-09", "1993-01-16",
                   "1993-01-23", "1993-01-30", "1993-02-06"))))
g <-  seq(start(z), end(z), "day")
na.locf(z, xout = g)




# test2 <- as.data.frame(seq(ISOdate(2016,4,26), ISOdate(2017,6,19), by = "5 min")) %>%
#   rename(date_time = "seq(ISOdate(2016, 4, 26), ISOdate(2017, 6, 19), by = \"5 min\")")
#   
#   # 
# test3 <- merge(test2, test1, by = "date_time", all.x = T) %>%
#   arrange(watershed, year, date_time)
#   
#   colnames(date)
#   
# test4 <- sed2 %>%
#   mutate(date_time = as.factor(date_time),
#          analyte = as.factor(analyte),
#          watershed = as.factor(watershed)) %>%
#   complete(analyte, watershed, date_time)
# 
# df <- tibble(
#   group = c(1:2, 1),
#   item_id = c(1:2, 2),
#   item_name = c("a", "b", "b"),
#   value1 = 1:3,
#   value2 = 4:6
# )
# 
# test5 <- df %>%
#   complete(group, nesting(item_id, item_name))

test7 <- expand.grid(test6$watershed, test2$date_time) %>%
  rename(watershed = 'Var1', date_time = 'Var2')

test6<- as.data.frame(unique(test1$watershed)) %>%
  rename(watershed = `unique(test1$watershed)`)


no3graph <- ggplot(sed2 %>% 
         filter(analyte == "Nitrate + nitrite (mg N/L)"), 
       aes(x = date_time, 
           y = cumulative,
           group = treatment,
           color = treatment,
           linetype = treatment)) + 
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Dissolved Nitrogen (lbs/ac)') + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/no3.jpg", plot=no3graph, width = 6, height=8)


#stuff between ### is only stuff I am messing with
##############################################################################

orthopgraph <- ggplot(sed2 %>% 
         filter(analyte == "Orthophosphate (mg P/L)"), 
       aes(x = date_time, 
           y = cumulative,
           group = treatment,
           color = treatment,
           linetype = treatment)) + 
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Dissolved Phosphorus (lbs/ac)') + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/orthop.jpg", plot=orthopgraph, width = 6, height=8)


tssgraph <- ggplot(sed2 %>% 
         filter(analyte == "TSS (mg/L)"), 
       aes(x = date_time, 
           y = cumulative,
           group = treatment,
           color = treatment,
           linetype = treatment)) + 
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Total Suspended Solids (lbs/ac)') + 
  theme_bw() + 
  theme(legend.position = "bottom",
      legend.title    = element_blank(),
      axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/tss.jpg", plot=tssgraph, width = 6, height=8)

#############################################################################
#THIS IS FOR lISA TO SEND TO EIA, edited to give to hoien at spirit lake

spirit <- d %>%
  filter(site=="spirit")

spiritrainrunplot <- ggplot(spirit, aes(x = date_time, 
                   y = y, 
                   group = watershed, 
                   linetype = treatment,
                   color = treatment)) +
  ggtitle("Rain and Surface Runoff") +
  geom_line() + 
  facet_grid(full~year, scales='free_x') + 
  labs(x = '',  
       y = 'Cumulative rainfall and runoff (inches)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "spiritrunoff.jpg", plot=spiritrainrunplot, width = 6, height=8)


spiritsed2 <- sed2%>%
  filter(site=="spirit")

spiritorthopgraph <- ggplot(spiritsed2 %>% 
                        filter(analyte == "Orthophosphate (mg P/L)"), 
                      aes(x = date_time, 
                          y = cumulative,
                          group = treatment,
                          color = treatment,
                          linetype = treatment)) + 
  ggtitle("Surface Runoff Orthophosphate") +
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Cumulative Orthophosphate (lbs/ac)') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "spiritorthop.jpg", plot=spiritorthopgraph, width = 6, height=8)


spirittssgraph <- ggplot(spiritsed2 %>% 
                     filter(analyte == "TSS (mg/L)"), 
                   aes(x = date_time, 
                       y = cumulative,
                       group = treatment,
                       color = treatment,
                       linetype = treatment)) + 
  ggtitle("Surface Runoff Sediment") +
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Cumulative Total Suspended Solids (lbs/ac)') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "spirittss.jpg", plot=spirittssgraph, width = 6, height=8)


