## Demo script for spaced plant images
## This script will demonstrate:
#1) seperating foreground (plant) from background
#2) measure crown rust
## Date: 2.25.19
## Author: Garett Heineck

## Required packages - may need installation first
## PLEASE UPDATE R (if you have not done so recently)
library(tidyverse)
require(readxl)
library(dplyr)
library(jpeg)
library(EBImage) # needs to be downloaded from (https://www.bioconductor.org/packages/release/bioc/html/EBImage.html)
library(randomForest)
library(stringr)
library(ggplot2)
library(cowplot)
##############
##############
##############


##############
##############
##############
## Required file paths
#parent directory folder nomed "grass.plant_rust.detection_2.25.19"***
img_dir_plant.demo_2.25.19<- "/Users/heine237/Documents/GitHub/grass.plant_rust.detection_2.25.19" # NOTE: change to your own file path 
#************************#
## Creating folders to store all image output
#NOTE: the folder "original_img" is where the images you want to process need to be***
folders <- c("results", "original_img", "S1_crop_img", "S2_foreground_classify", "S3_foreground_EBImage", "S4_CR_classify",  "S5_CR_EBImage") #adding in the correct folder list*** 
for (i in 1:length(folders))  { 
  dir.create(paste(img_dir_plant.demo_2.25.19,folders[i], sep="/")) 
} 
#make sure the "original_img" folder has images in it***
##############
##############
##############


##############
##############
##############
## Read in field data sheet.
## The field data contains plant information about each image captured in the field.
## Field data also gives us median visual ratings for crown rust.
## We can compare these visual scores to our computer generated ratings.
#************************#
#read in field data***
#we will use this later in the image analysis process***
plant_field.dat<-  read_excel(paste(img_dir_plant.demo_2.25.19, "results", "plant.demo_field.data_2.25.19.xlsx", sep = "/"), skip = 5, na = ".")
summary(plant_field.dat)
##############
##############
##############


##############
##############
##############
## This step crops the original images.
## Cropping your images is important to reduce processing time. 
#************************#
#file paths for the original images***
folder_original_img<- paste(img_dir_plant.demo_2.25.19, "original_img",sep = "/")
#file path for the cropped images***
folder_crop_img<-  (paste(img_dir_plant.demo_2.25.19,"S1_crop_img",sep = "/"))
#************************#
#this is one way to determine how much of an image to remove***
#look at your image and guess what proportion should be removed***
#5% is take from the short length***
#15% is taken from the long length***
Top.Bot<- 0.05
Lef.Rig<- 0.15
#************************#
paths_original_img<- list.files(path=folder_original_img,full.names = TRUE) 
names_original_img<- list.files(path=folder_original_img,full.names = FALSE) 
for (i in 1:length(paths_original_img)){
  temp=readJPEG(paths_original_img[i])
  temp1=temp[1:dim(temp)[1], 1:dim(temp)[2],] # standardizing the rotation of each image***
  tempdim<- round(dim(temp1))
  top<- round(tempdim[1] * Top.Bot)
  bottom<- tempdim[1] - (tempdim[1] * Top.Bot) 
  left<- round(tempdim[2] * Lef.Rig)
  right<- round(tempdim[2] - (tempdim[2] * Lef.Rig))
  temp2<-temp1[top:bottom, left:right,]
  order<- plant_field.dat$plant_num[i] # this adds the field ID names to each image***
  writeJPEG(temp2, paste(folder_crop_img, "/", order, "_", names_original_img[i], sep = ""), quality = 1)
}
##############
##############
##############


##############
##############
##############
## We now need to load the training data.
## Information on how to create training data can be found in the TRAINING DATA HELP GUIDE.
## Collectively the training mixes are called a palette in the training palette folder.
## The palette has many mixes, each help in predicting different features within the image.
#************************#
palette_directory_plant<- paste(img_dir_plant.demo_2.25.19, "training data_2.25.19",sep = "/") #file path where mixes are saved***
#************************#
mixes_names<- list.files(path=palette_directory_plant,pattern="*.csv",full.names = FALSE) #name directory for what is in the palette folder***
mixes_path<- list.files(path=palette_directory_plant, pattern="*.csv", full.names = TRUE) #path directory for what is in the palette folder***
training.palette_plant<- data.frame()
#this for() loop will systematically re arrange and condense each mix file in the training palette folder***
#the reason I am doing this is to allow the script to update itself upon adding additional mixes***
for (i in 1:length(mixes_path)){
  temp_mix<- read.csv(mixes_path[i])
  temp_mix$band<- NA
  temp_mix$band[1:which(temp_mix$Label == "Red")] <- "Red"
  temp_mix$band[(which(temp_mix$Label == "Red")+1):which(temp_mix$Label == "Green")] <- "Green"
  temp_mix$band[(which(temp_mix$Label == "Green")+1):which(temp_mix$Label == "Blue")] <- "Blue"
  temp<- split(temp_mix, temp_mix$band)
  temp2<- do.call("cbind", split(temp_mix, temp_mix$band))
  image<- temp2$Blue.Label[1]
  mix<- mixes_names[i]
  temp3<- data.frame(mix, image, x=temp2[5]$Blue.X, y=temp2[6]$Blue.Y, red=temp2[18]$Red.Mean, green=temp2[11]$Green.Mean, blue=temp2[4]$Blue.Mean)
  training.palette_plant<- rbind(training.palette_plant, temp3) 
}
summary(training.palette_plant) #summarizing the training palette***
count(training.palette_plant, mix) %>% View #counting observations in each mix of the training palette*** 
##############
##############
##############


##############
##############
##############
## We will now make the random forest models to detect different features in the cropped images.
## A different random forest model will be needed for each feature.
## Here were are detecting two features: 
# 1) the foreground (biological related pixels) from the background
# 2) crown rust from healthy plant tissue
#************************#
#model to seperate foreground (biological related pixels)***
palette_selection_bio<- training.palette_plant
palette_selection_bio$classification<- c(rep(1, len=1450),rep(0, len=800)) #selecting the mixes (1=foreground)***
palette_selection_bio %>% group_by(mix) %>% summarise(avg=mean(classification)) 
rfm_bio_plant.demo_2.25.19<- randomForest(classification~(red+green+blue),data=palette_selection_bio, ntree=100,mtry = 1,importance=TRUE)
print(rfm_bio_plant.demo_2.25.19)
plot(rfm_bio_plant.demo_2.25.19) #ntree is set to 100, is looks like it could be set to about 50***
importance(rfm_bio_plant.demo_2.25.19) #green and blue bands are the most important***
#************************#
#model for crown rust***
palette_selection_CR<- training.palette_plant
palette_selection_CR$classification<- c(rep(1, len=650),rep(0, len=1600)) 
palette_selection_CR %>% group_by(mix) %>% summarise(avg=mean(classification))  #check to make sure CR (crown rust related mixes) have a 1***
rfm_CR_plant.demo_2.25.19<- randomForest(classification~(red+green+blue),data=palette_selection_CR, ntree=50,mtry = 1,importance=TRUE) #there is far lower variation in this training data***
print(rfm_CR_plant.demo_2.25.19)
plot(rfm_CR_plant.demo_2.25.19)
importance(rfm_CR_plant.demo_2.25.19) #red and blue bands are the most important***
##############
##############
##############


##############
##############
##############
## Running the image processing loop.
## This is a really large loop that is broken up into 4 sections.
#1) seperating foreground from background
#2) seperating crown rust from healthy tissue
#3) conducting morphological operations for rust quantification with EBImage
#4) writing summary statisitics
#************************#
#each path is for an image***
folder_cropped_plant.demo_2.25.19<-  (paste(img_dir_plant.demo_2.25.19,"S1_crop_img",sep = "/"))
folder_classify_plant.demo_2.25.19<- (paste(img_dir_plant.demo_2.25.19,"S2_foreground_classify",sep = "/"))
folder_EBImage_plant.demo_2.25.19<-  (paste(img_dir_plant.demo_2.25.19,"S3_foreground_EBImage",sep = "/"))
folder_CR_classify_plant.demo_2.25.19<-  (paste(img_dir_plant.demo_2.25.19,"S4_CR_classify",sep = "/"))
folder_CR_EBImage_plant.demo_2.25.19<-  (paste(img_dir_plant.demo_2.25.19,"S5_CR_EBImage",sep = "/"))
#************************#
#check to make sure all the cropped image show up***
paths_cropped_plant<- list.files(path=folder_cropped_plant.demo_2.25.19,full.names = TRUE)
names_cropped_plant<- list.files(path=folder_cropped_plant.demo_2.25.19,full.names = FALSE) 
#create a data frome to collect numeric output from the analysis***
img.stats_plant.demo_2.25.19<- data.frame()

for (i in 1:length(paths_cropped_plant)) {
  img.01<- readJPEG(paths_cropped_plant[i])
  coor<- as.data.frame(as.table(img.01[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.01[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.01[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.01[,,3]))[3]
  img.dat.01<- cbind(coor, red, green, blue)
  colnames(img.dat.01)<- c("y","x","red","green","blue")
  img.dat.01$classify<- predict(rfm_bio_plant.demo_2.25.19, img.dat.01)
  img.dat.01$thresh<- ifelse(img.dat.01$classify>0.80, 1,0)  #Set threshold to 80%***
  img.02<- matrix(img.dat.01$thresh, nrow=nrow(img.01), ncol=ncol(img.01))
  writeJPEG(img.02, paste(folder_classify_plant.demo_2.25.19, "/", names_cropped_plant[i], sep = ""), quality = 1)
  paths_classify_plant.01<- list.files(path=folder_classify_plant.demo_2.25.19,full.names = TRUE)
  morph_op.01<- readImage(paths_classify_plant.01[i])
  overlay.01<-  readImage(paths_cropped_plant[i])
  kernal.01<- makeBrush(3, shape='box') #kernal sizes between 3 and 9 seem to work best***
  image_dilate.01<- thresh(dilate(morph_op.01, kernal.01), w=150, h=150, offset= 0.001) #here dilate is used, sometimes 'erode' works better***
  img.03 = stackObjects(image_dilate.01, overlay.01, combine = T, bg.col='black')
  writeImage(img.03, paste(folder_EBImage_plant.demo_2.25.19, "/", names_cropped_plant[i] ,sep=""), quality = 100)
  plant.featr.img.03<- bwlabel(image_dilate.01)
  plant.featr<- data.frame(computeFeatures.shape(plant.featr.img.03))
  
  paths_EBImage_plant<-list.files(path=folder_EBImage_plant.demo_2.25.19,full.names = TRUE)
  img.04<- readJPEG(paths_EBImage_plant[i])
  coor<- as.data.frame(as.table(img.04[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.04[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.04[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.04[,,3]))[3]
  img.dat.02<- cbind(coor, red, green, blue)
  colnames(img.dat.02)<- c("y","x","red","green","blue")
  img.dat.02$order<- seq(1:length(img.dat.02$x))
  img.dat.02$exclude<- img.dat.02$red+img.dat.02$blue+img.dat.02$green
  img.dat.02_rgb<- filter(img.dat.02, exclude > 0)
  img.dat.02_black<- filter(img.dat.02, exclude == 0)
  img.dat.02_rgb$classify<- predict(rfm_CR_plant.demo_2.25.19, img.dat.02_rgb)
  img.dat.02_black$classify<- rep(0, times=length(img.dat.02_black$red))
  img_combine<- rbind(img.dat.02_rgb,img.dat.02_black)
  img_combine<- arrange(img_combine, order)
  img_combine$thresh<- ifelse(img_combine$classify>0.80, 1,0) #Set threshold to 80%***
  img.05<- matrix(img_combine$thresh, nrow=nrow(img.04), ncol=ncol(img.04))
  writeJPEG(img.05, paste(folder_CR_classify_plant.demo_2.25.19, "/", names_cropped_plant[i] ,sep=""), quality= 1)
  
  paths_classify_CR<- list.files(path=folder_CR_classify_plant.demo_2.25.19,full.names = TRUE)
  morph_op.02<- readImage(paths_classify_CR[i])
  overlay.02<-  readImage(paths_cropped_plant[i])
  img.06<- thresh(morph_op.02, w=5, h=5, offset=0.05) #use a small window for crown rust***
  img.06<- opening(img.06, makeBrush(3, shape='disc')) #disk shape from 3-9 seems to work best***
  img.06<- fillHull(img.06)
  img.06<- bwlabel(img.06)
  writeImage(colorLabels(img.06), paste(folder_CR_EBImage_plant.demo_2.25.19, "/", names_cropped_plant[i], sep=""), quality = 100)
  CR.featr<- data.frame(computeFeatures.shape(img.06, overlay.02))
  
  write.stats<- data.frame(img.ID=              str_sub(names_cropped_plant[i]), #unique image ID***
                           comp.sum.img=        length(img.dat.01$thresh), #total pixels***
                           comp.sum.RF.bio=     sum(img.dat.01$thresh),  #total biological pixels***
                           comp.prop.RF.bio=    (sum(img.dat.01$thresh)/length(img.dat.01$thresh)), #proportion of biological pixels in relation to total pixels***
                           comp.sum.EBI.bio=    sum(plant.featr$s.area), #sum of biological pixels after EBImage processing***
                           comp.objID.EBI.01=   arrange(plant.featr, desc(s.area))[1,1], # weed ID helper***
                           comp.objID.EBI.02=   arrange(plant.featr, desc(s.area))[2,1],
                           comp.objID.EBI.03=   arrange(plant.featr, desc(s.area))[3,1],
                           comp.prop.EBI.bio=   sum(plant.featr$s.area)/length(img.dat.01$thresh), #proportion of biological pixels after EBImage***
                           comp.sum.RF.CR=      sum(img_combine$thresh), #sum all crown rust pixels***
                           comp.prop.RF.CR=     (sum(img_combine$thresh)/sum(plant.featr$s.area)), 
                           comp.pustct.EBI.CR=  length(CR.featr$s.area), #average pustule cluster count***
                           comp.avgpust.EBI.CR= mean(CR.featr$s.area), #average pusutle cluster size***
                           comp.sum.EBI.CR=     sum(CR.featr$s.area), 
                           comp.prop.EBI.CR=    (sum(CR.featr$s.area)/sum(plant.featr$s.area))) 
                            
  img.stats_plant.demo_2.25.19<-rbind(img.stats_plant.demo_2.25.19, write.stats) 
}
#writing the output statistics to the parent directory folder***
write.csv(img.stats_plant.demo_2.25.19, paste(img_dir_plant.demo_2.25.19, "results","img.stats_plant.demo_2.25.19.csv", sep = "/"))
##############
##############
##############


##############
##############
##############
## Now we can check to see how our computer and visual scores match up.
## We will use the random forest, EBImage output, and the orinal field data file to determine success of the proccess.
## Here are some things to note about how computer ratings differ from visual ratings on the Cobb scale:
#1) The Cobb scale is a 0-100 scale, but is NOT percent severity.
#2) You must multiply Cobb severity by 0.37 to get to percent severity.
#3) The last column in "img.stats_plant.demo_2.25.19" is the most conservative estimate of crown rust (comp.prop.EBI.CR).
#4) This is a proportion not a percent so multiply by 100 to standardize against visual ratings.
stats_plant.demo_2.25.19<- read.csv(paste(img_dir_plant.demo_2.25.19, "results", "img.stats_plant.demo_2.25.19.csv", sep = "/"))

plant.demo_2.25.19.output<- cbind(plant_field.dat,stats_plant.demo_2.25.19) %>%
  mutate(visual.percent = ratr.med.CR * 0.37) %>%
  mutate(spatial.limiting_computer.percent = comp.prop.EBI.CR * 100) %>%
  mutate(randomForest_computer.percent = comp.prop.RF.CR * 100)
summary(plant.demo_2.25.19.output)

#plotting data points***
ggplot(plant.demo_2.25.19.output, aes(x = visual.percent, y = spatial.limiting_computer.percent)) + 
  labs(x = "Visual rating % (Cobb scale * 0.37)", y = "Computer rating %") +
  geom_point()

## You can see that the computer is much more conservative than the visual ratings.
## This seems to be very typical, but could be manipulated by changing the thresholding paramerters.
## EBImage thresholding and dilation operations make a more conservative estimate, sometimes this is good (folder S5).
## We could also use the less conservative estimate that includes only the output form the randomForest and the global threshold (folder S4).

#plotting data points***
ggplot(plant.demo_2.25.19.output, aes(x = visual.percent, y = randomForest_computer.percent)) + 
  labs(x = "Visual rating % (Cobb scale * 0.37)", y = "Computer rating %") +
  geom_point()

## In this particular case estimates are a bit closer when the unadjusted values are used.
## The application dictates what value work best.
##############
##############
##############


##############
##############
##############
## There are some other output features that can be interesting 
## 1) the "comp.objID" columns can be useful if there are weeds or other misidentified objects
mis.ID<- plant.demo_2.25.19.output %>%
  select(plant_num,image_num, 
         comp.sum.EBI.bio, 
         comp.objID.EBI.01, 
         comp.objID.EBI.02, 
         comp.objID.EBI.03) %>%
  gather("Object", "pixel_num", 3:6)

ggplot(mis.ID, aes(x = Object, y = pixel_num, fill = image_num)) +
  geom_col()+
  facet_wrap(~image_num)+
  theme(axis.text.x = element_text(angle = 20))
#you can see that the sum is almost the same size of the largest pixel cluser***
#this inidicates that there is no misclassification***

## 2) You can also check the foreground (plant) compared to an ImageJ result
ggplot(plant.demo_2.25.19.output, aes(x = comp.sum.EBI.bio, y = ImJ.HSB.biopix)) +
  labs(x = "R pixel number prediction", y = "ImageJ pixel number prediction")+
  geom_point()
#the result looks to be near 1:1***
#we can also see that one of the plants is much larger than the other two***
#this makes sense if you look at the images***

## 3) Finally, you might be intersted in both the number of pustule clusters or the average size of rust pustule cluster.
avgpust<- ggplot(plant.demo_2.25.19.output, aes(x = image_num, y = comp.avgpust.EBI.CR))+
  labs(x = "Image ID", y = "Average pustule cluster size")+
  geom_col() + #plotting average pustule cluster size***
  theme(axis.title.y = element_text(size = 10))

pustct<- ggplot(plant.demo_2.25.19.output, aes(x = image_num, y = comp.pustct.EBI.CR))+
  labs(x = "Image ID", y = "Average pustule cluster #")+
  geom_col() + #plotting average pustule cluster number***
  theme(axis.title.y = element_text(size = 10))

percent.CR<- ggplot(plant.demo_2.25.19.output, aes(x = image_num, y = randomForest_computer.percent))+
  labs(x = "Image ID", y = "Crown rust severity")+
  geom_col() + #plotting severity***
  theme(axis.title.y = element_text(size = 10))


plot_grid(avgpust,pustct,percent.CR, ncol = 1) #requires the cowplot package***
#these results show us that the plant that has the largest clusters (7132) does not have the highest severity (7193)***


