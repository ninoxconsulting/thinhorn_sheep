# HMM example 
#https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-example.pdf
#https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-starting-values.pdf

#install.packages("moveHMM")
library(dplyr)
library(sf)
library(fs)
library(readxl)
library(lubridate)
library(hms)
library(ggplot2)
library(moveHMM)

# read in the summary data 

clean_dir <- fs::path("01_clean_data")
out_dir <- fs::path("02_draft_outputs/01_lamb_figures")


allpts <- read.csv(fs::path("01_clean_data", "location_steps_all_raw.csv")) 

# remove unwated cols 
pts <- allpts |> 
  select(-X.1, -CollarSerialNumber , -Hdop,-NumSats, -FixTime, -Year , -Hour, -Minute, -X2D.3D, 
         -Date, -Time.Zone, -tag_id.x, -tag_id.y, -capture_date, -time,-date_pst.x,
         -Recorder, -Capture_GPS_Zone_NAD_83., -Northing, -Westing,-x,-y,
         -Animal_WLH, -Eartag, -End_Date, -Comments, - date_time,
         -lat_prior, -long_prior  , -time_prior , -cos_turn, -gps_spike     
  ) |> 
  mutate(date_time_pst = ymd_hms(date_time_pst)) |> 
  mutate(date_pst = as_date(date_pst.y)) 

# get list of ewes
ewes <- pts |> 
  filter(sheep_class == "ewe") |> 
  filter(date_pst >= ymd("2024-05-01") & date_pst <= ymd("2024-06-10")) |> 
  select(tag_idn, X, Y, Activity) |> 
  rename("x" = X, "y" = Y) 

#unique(ewes$tag_idn)


ei <- ewes |> filter(tag_idn == 55702) 
data <-prepData(ei, type ="UTM")
hist(data$angle, breaks = seq(-pi, pi, length = 15), xlab = "turning angle")
hist(data$step, xlab = "step length")

plot(data, animals=c(1), ask=F)

# select parameters for the two states (ie a review of the steplenth shows
# bulk close to 1 and highest 20000, so selected a low step length state1
# and a high step length state2, 
# check for zero step length and 

stepMean0 <- c(1, 200)
stepSD0 <- c(1, 200)

# check zero movemnt 
whichzero <- which(data$step == 0)
zeromass1 <- c(0.1, 0.1)

stepPar0 <- c(stepMean0, stepSD0,zeromass1) 

angleMean0 <- c(pi, 0) # initial means (one for each state) 
angleCon0 <- c(1, 10) #
anglePar0 <- c(angleMean0,angleCon0)

mod1 <- fitHMM(data = data, 
               nbStates = 2, 
               stepPar0 = stepPar0, 
               anglePar0 = anglePar0) 

mod1


################################################################

set.seed(12345) 
#Numberoftrieswithdifferentstartingvalues 
niter<-25 
#Savelistoffittedmodels 
allm <-list() 
for(i in 1:niter) { 
  #Step lengthmean 
  stepMean0<-runif(2, 
                   min=c(0.5, 6), 
                   max=c(5,200)) 
  
  #Steplengthstandarddeviation 
  stepSD0<-runif(2, 
                 min=c(0.5,6), 
                 max=c(5,200)) 
  
  #Turninganglemean 
  angleMean0<-c(0,0) 
  #Turningangleconcentration 
  angleCon0<-runif(2, 
                   min=c(0.5, 5), 
                   max=c(2,15)) 
  
  #Fitmodel 
  zeromass1 <- c(0.1, 0.1)
  
  stepPar0<-c(stepMean0,stepSD0,zeromass1) 
  
  
  anglePar0<-c(angleMean0,angleCon0) 
  allm[[i]]<-fitHMM(data=data,
                    nbStates=2,
                    stepPar0=stepPar0, 
                    anglePar0=anglePar0) 
  } 

allm

#Extractlikelihoodsoffittedmodels 
allnllk<-unlist(lapply(allm,function(m)m$mod$minimum)) 
allnllk


whichbest<-which.min(allnllk)
#Bestfittingmodel 
mbest<-allm[[whichbest]] 
mbest

plot(mbest, ask = FALSE, animals = 1)


data$state <- factor(viterbi(mbest))

e <- factor(viterbi(mod1)) 
# Plot tracks coloured by state 
ggplot(data, aes(x, y, col = state, group = ID)) + 
  geom_path() +
  coord_equal() 

# Plot step lengths coloured by state 
ggplot(data, aes(x = 1:nrow(data), y = step, col = state, group = ID)) + 
  geom_point(size = 0.8)

# 
# sp <- stateProbs(m = mod1)
# data$sp1 <- sp[,1] 
# ggplot(data, aes(x, y, col = sp1, group = ID)) + 
#   geom_path() + 
#   coord_equal() + 
#   labs(col = "Pr(S = 1)")
# 
# mod2 <- fitHMM(data = data, 
#                nbStates = 2, 
#                stepPar0 = stepPar0, 
#                anglePar0 = anglePar0, 
#                formula = ~ Activity)
# 
# 
# mod2
# plot(mod2,ask = FALSE, plotTracks= FALSE, plotCI = TRUE)
# plotStationary(mod2, plotCI = TRUE)
# 
# AIC(mbest, mod2)
# 
# 
# plotPR(mod2)
# 
# 
# plotData1<-getPlotData(m =mod2, type= "tpm",format ="long")
# 
# ggplot(plotData1$Activity,aes(Activity,mle)) + facet_wrap("prob")+ geom_line()+
#   facet_wrap("prob")+
#   geom_line()+
#   geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.3) +
#   labs(y = "transition probability")
# 
# 
# 
# 
# 
# plotData2 <- getPlotData(m = mod2, type = "stat", format = "long")
# ggplot(plotData2$Activity, aes(Activity, mle, col = factor(state))) + 
#   geom_line() + 
#   geom_ribbon(aes(ymin = lci, ymax = uci, col = NULL, fill = factor(state)), alpha = 0.3) + 
#   labs(fill = "state", col = "state", y = "stationary state probabilities")
# 











hist(allnllk, breaks=20)

head(pts)

tt <- pts |> 
  select(tag_id, X, Y, Activity) |>
  filter(tag_id == 556691)
  group_by(tag_id) |> 
  summarise(n = n()) |> 
  arrange(n)
