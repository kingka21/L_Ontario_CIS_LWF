#### cisco #### 

#### load libraries #### 
options(java.parameters = "-Xmx6000m")
library(sf)
library(dplyr)
library(raster)
library(dismo) #for MAXENT 
library(rJava)

#### read in CORHIST data that was published #### 

corhist_cisco<-read.csv("data/points_ont_cis_historical.csv")

#get the lat long data  
x <- corhist_cisco$LON_DD
y <- corhist_cisco$LAT_DD

#make a dataframe of the coordinates and project
lake.ll <- SpatialPointsDataFrame(data.frame(x=x,y=y), data=corhist_cisco, proj4string=CRS("+proj=longlat +datum=NAD83"))   
prjnew <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
ont_cis_aea <- spTransform(lake.ll, prjnew )


####read in environmental variables  ####
clim_list <- list.files("data/ont_rasters", pattern = ".asc", 
                        full.names = T)  # 
clim <- raster::stack(clim_list)

#plot points and layer to make sure they line up 
plot(clim[[1]], col="gray")  # to the first layer of the clim layers as a reference
plot(ont_cis_aea, add = TRUE)  # plot 


# MAXENT 
#this code was modeled after the following Github page
#https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md
# A function that implements Maxent parameters using the general R manner
prepPara <- function(userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "P")
                     responsecurves=TRUE,
                     jackknife=TRUE,      
                     outputformat="logistic",
                     outputfiletype="asc", 
                     projectionlayers=NULL,
                     randomseed=FALSE,
                     removeduplicates=TRUE,
                     betamultiplier=NULL,
                     biasfile=NULL,
                     testsamplesfile=NULL,
                     replicates=1,
                     replicatetype="crossvalidate",
                     writeplotdata=TRUE,
                     extrapolate=TRUE,
                     doclamp=TRUE,
                     beta_threshold=NULL,
                     beta_categorical=NULL,
                     beta_lqp=NULL,
                     beta_hinge=NULL,
                     applythresholdrule=NULL
){
  #20 & 29-33 features, default is autofeature
  if(is.null(userfeatures)){
    args_out <- c("autofeature")
  } else {
    args_out <- c("noautofeature")
    if(grepl("L",userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
    if(grepl("Q",userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
    if(grepl("H",userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
    if(grepl("P",userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
    if(grepl("T",userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
  }
  
  #1 
  if(responsecurves) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
  #2
  #if(picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
  #3
  if(jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
  #4
  args_out <- c(args_out,paste0("outputformat=",outputformat))
  #5
  args_out <- c(args_out,paste0("outputfiletype=",outputfiletype))
  #7
  if(!is.null(projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",projectionlayers))
  #10
  if(randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
  #16
  if(removeduplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
  #20 & 53-56
  # check if negative
  betas <- c( betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
  if(! is.null(betas) ){
    for(i in 1:length(betas)){
      if(betas[i] <0) stop("betamultiplier has to be positive")
    }
  }
  if (  !is.null(betamultiplier)  ){
    args_out <- c(args_out,paste0("betamultiplier=",betamultiplier))
  } else {
    if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
    if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
    if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
    if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
  }
  #22
  if(!is.null(biasfile))    args_out <- c(args_out,paste0("biasfile=",biasfile))
  #23
  if(!is.null(testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",testsamplesfile))
  #24&25
  replicates <- as.integer(replicates)
  if(replicates>1 ){
    args_out <- c(args_out,
                  paste0("replicates=",replicates),
                  paste0("replicatetype=",replicatetype) )
  }
  #37
  if(writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
  #39
  if(extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
  #42
  if(doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
  #60
  if(!is.null(applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",applythresholdrule))
  
  return(args_out)
}

#### pull background points and occurrence points ####
#select background points from study area #
set.seed(1) 
bg <- sampleRandom(x=clim,
                   size=20000, 
                   na.rm=T, #removes the 'Not Applicable' points  
                   sp=T) # return spatial points 


#using all of the data at the moment  
occ_train<-ont_cis_aea

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
p <- raster::extract(clim, occ_train, df=TRUE) %>% 
  dplyr::select(-c(ID)) %>% 
  mutate(substrate_hist = as.factor(substrate_hist))

# extracting env conditions for background
a <- raster::extract(clim, bg, df=TRUE)%>% 
  dplyr::select(-c(ID))%>% 
  mutate(substrate_hist = as.factor(substrate_hist))

#maxent needs 0s and 1s for pres/abs
#repeat the number 1 as many numbers as the number of rows in pres
#repeat 0 as the rows of background points (abs)
pa <- c(rep(1, nrow(p)), rep(0, nrow(a)))

#environmental attributes dataframe
pder <- as.data.frame(rbind(p, a))

#### cross validation #### 
## the names of the projectionlayers (excluding the name extension) must match the names 
mod_cross <- maxent(x=pder[c( "bathy_extrap","fetch","substrate_hist")], 
                    p=pa, 
                    path="maxent_ONT_cisco_hist", 
                    args=prepPara(userfeatures=NULL, #could be any combination of LQHP
                                  betamultiplier=1,
                                  doclamp = TRUE,
                                  projectionlayers="data/ont_rasters",
                                  replicates=5, ## 5 replicates
                                  replicatetype="crossvalidate") )


#### pull marginal plots #### 
files <- list.files(path = "maxent_ONT_cisco_hist/plots" , pattern = "\\.dat$", full.names = TRUE)

response_data <- bind_rows(
  lapply(files, function(f) {
    # Read as CSV since they’re comma-separated
    df <- read.csv(f)
    # Add the variable name (strip the extension)
    df$variable <- gsub("\\.dat$", "", basename(f))
    
    df
  })
)

#write.csv(response_data, "data/ont_cis_marginal_hist.csv", row.names = FALSE)


#### pull out all threshold values from each fold ####
# Read CSV file of maxent results (generated by maxent earlier)
results_df <- read.csv("maxent_ONT_cisco_hist/maxentResults.csv", header=TRUE) 

cols<-grep("(Logistic\\.threshold$)", 
           names(results_df), 
           value = TRUE)

results_subset<-results_df[,c("Species", cols)]

#compare MTP, or 10th percentile or MaxSSS 
thresholds<-results_subset %>% 
  dplyr::select( Species, Minimum.training.presence.Logistic.threshold, X10.percentile.training.presence.Logistic.threshold, Maximum.training.sensitivity.plus.specificity.Logistic.threshold)


####Boyce Index ###### 
library(ecospat)
cis_hist_raster<-raster("maxent_ONT_cisco_hist/species_ont_rasters_avg.asc") 

#extract predicted values from presence and background points 
pres_prediction <- raster::extract(cis_hist_raster, occ_train, df=TRUE) %>%
  dplyr::select(-c(ID)) %>% 
  drop_na()

bg_prediction <- raster::extract(cis_hist_raster, bg, df=TRUE) %>%
  dplyr::select(-c(ID))

boyce<-ecospat.boyce(obs = pres_prediction, 
                     fit = bg_prediction)

boyce$cor

