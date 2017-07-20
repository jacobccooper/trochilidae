###ABOUT AND DISCLAIMER:

#'These codes are derived from the codes Jacob C. Cooper and Jorge Soberon used to model and
#'analyze stacked species distribution models in Trochilidae. These codes are specifically
#'written for use on our personal machines and, as such, possess many loop codes and 
#'batch edits that are specific for the directories that existed on our machines. To the 
#'best of our ability, we have simplified the and annotated the code; however, errors and 
#'unexplained portions of the code may still exist. These codes are presented *as is*, without
#'the guarantee that they will immediately work on another person's computer after download.
#'Please note there are several iterations of some codes as well; batch processing is not 
#'perfect, and small edits/reruns are necessary to properly run and model every scenario
#'and every species.
#'
#'If you have questions about the usage of this code and its applications for niche modeling and
#'SDM stacking, please contact Jacob C. Cooper <black.hawk.birder@gmail.com>.
#'
#'Thanks!
#'
#'Jacob C. Cooper and Jorge Soberon
#'July 2017
#'
#'
#################################################################################

filepath="set directory and folder of \\Hummingbirds"
setwd(filepath)
#Required packages for running these codes:
library(dismo)
library(rgbif)
library(maptools)
library(raster)
library(plyr)
library(rgdal)
library(shapefiles)
library(sp)
library(fossil)
library(devtools)
library(SDMTools)
library(rJava)

#Download GBIF data
##################################################################################################

#written for a table with one column of data; subsetted as ncol for columnwise vertical reading
#name of column MUST be "SCIENTIFICNAME", then must have IOC values for "CODE"
#remove ssp's to make the program faster and to prevent downloading "blanks"
setwd(filepath)
m=read.csv("IOC_hummers.csv")
#After reading, set to the working directory in which you want to save the files
setwd(paste0(filepath,"\\raw_GBIF",sep=""))
#strsplit will split values within that column and assign parts as "Genus and "Species"
for (i in 1:nrow(m)) {
  #splits out the genus and the species for downloading and for usage in the file download
  genus<-strsplit(as.character(m$SCIENTIFICNAME[i])," ")[[1]][1]
  species<-strsplit(as.character(m$SCIENTIFICNAME[i])," ")[[1]][2]
  code<-as.character(m$CODE[i],"")
  print("Downloading...")
  #Concept must be set to "TRUE" to download synonyms and, most importantly, ssp's
  Gbif=gbif(genus=genus, species=species,concept=T,geo=T,removeZeros=T)
  print(paste("saving :", genus, species))
  filename=paste("raw_", code, "_Gbif_", genus, "_", species,".csv", sep="" )
  write.csv(Gbif,file=filename)
  print(paste("species :", genus, species, "GBIF download", "and", filename, "file created   ...done."))
}

#Parse GBIF data for things from Cornell Lab of Ornithology (ie, eBird)
#subsetting the data - removing all eBird records
#collection == EBIRD
#following code successfully parses out all Cornell Records for non-blanks
#Must save to same directory!
csvfiles=list.files(path=paste0(filepath,"\\raw_GBIF",sep=""),pattern="*.csv")
for (i in 1:length(csvfiles)){
  print("Working...")
  m=read.csv(csvfiles[i])
  x<-as.character(csvfiles[i])
  #remove all Cornell records (aka, eBird)
  refine=subset(m,m$institution!="CLO")
  print(paste("saving :",x))
  filename=paste("non-eBird_",x,sep="")
  print(paste("File :",x,"eBird parse saving."))
  write.csv(refine,file=filename)
  print("file created   ...done.")
}

#set up to run again

#You now have your raw and non-eBird data together in the same directory. You are ready for ArcMap
##################################################################################################

#eBird data, being so massive, is being downloaded BY COUNTRY, and in USA, by STATE
##Code to combine species from the different country files:

#Load Clements Taxonomy
setwd(filepath)
#file is a list of all hummingbird taxa per clements. SCIENTIFIC.NAME is essential column
#REMOVE REGULAR USA SPECIES
#Download as separate files
m=read.csv("eBird_hummers.csv")
filepath=("set directory and folder of \\Hummingbirds\\raw_eBird\\test")
setwd(filepath)
#Loop code for extracting every value for species from large list of country files
#works for small list of "countries"
countries=list.files(path=filepath,pattern="*.txt")
for (i in 1:length(countries)){
  #reads country by country, parsing out each species individually
  x=read.delim(countries[i],header=T,sep="\t",quote="")
  kiribati=as.character(countries[i])
  print(paste0("Reading:",kiribati,sep=""))
  #during the first pass, must create a directory for the species records
  if (i==1){
    for (j in 1:nrow(m)){
      species=as.character(m$SCIENTIFIC.NAME[j])
      taxa=subset(x,x$SCIENTIFIC.NAME==species)
      keep<-c("TAXONOMIC.ORDER","SCIENTIFIC.NAME","SUBSPECIES.SCIENTIFIC.NAME","COUNTRY","LATITUDE","LONGITUDE","DURATION.MINUTES","EFFORT.DISTANCE.KM")
      taxa1=taxa[keep]
      print(paste0("Saving ",species,sep=""))
      filename=(paste("raw_eBird_",species,".csv",sep=""))
      #though 'write.table' saves as .csv file
      write.table(taxa1,file=filename,row.names=F,quote=FALSE,sep=",")
    }
  } else {
    #non first pass, saves to already existing file
    for (j in 1:nrow(m)){
      species=as.character(m$SCIENTIFIC.NAME[j])
      taxa=subset(x,x$SCIENTIFIC.NAME==species)
      keep<-c("TAXONOMIC.ORDER","SCIENTIFIC.NAME","SUBSPECIES.SCIENTIFIC.NAME","COUNTRY","LATITUDE","LONGITUDE","DURATION.MINUTES","EFFORT.DISTANCE.KM")
      taxa1=taxa[keep]
      filename=(paste("raw_eBird_",species,".csv",sep=""))
      setwd(filepath)
      prev=read.csv(filename)
      df=rbind(prev,taxa1)
      print(paste0("Saving ",species,sep=""))
      write.table(df,file=filename,row.names=F,quote=FALSE,sep=",")
    }
  }
  print(paste0("Status: good. Finished:",kiribati))
}

#US species
filepath="set directory and folder of \\Hummingbirds"
setwd(paste0(filepath,"\\raw_eBird","\\USA",sep=""))
#Read country database file list
countries=list.files(path=paste0(filepath,"\\raw_eBird","\\USA",sep=""),pattern="*.txt")
#Loop code for extracting every value for species from large list of country files
for (j in 1:length(countries)){
  #read eBird txt files from direct download
  x=read.delim(countries[j],header=T,sep="\t",quote="")
  #extract all records with the scientific name requested
  species=x$SCIENTIFIC.NAME[2]
  #Remove extraneous columns, specifically those that contain commas
  keep<-c("TAXONOMIC.ORDER","SCIENTIFIC.NAME","SUBSPECIES.SCIENTIFIC.NAME","COUNTRY","LATITUDE","LONGITUDE","DURATION.MINUTES","EFFORT.DISTANCE.KM")
  df1=x[keep]
  #save refined dataframe of hummingbird records
  print(paste("saving",species,"concatonated",sep=" "))
  filename=(paste("raw_eBird_",species,".csv",sep=""))
  #though 'write.table' saves as .csv file
  write.table(df1,file=filename,row.names=F,quote=FALSE,sep=",")
}

#Parse eBird data for large traveling counts
#works on same working directory as above; however only reads .csv files
csvfiles=list.files(path=paste0(filepath,"\\","raw_eBird",sep=""),pattern="*.csv")
for(i in 1:length(csvfiles)){
  m=read.csv(csvfiles[i])
  #extract scientific name from file name
  x<-strsplit(as.character(csvfiles[i]),"[.]")[[1]][1]
  y<-strsplit(x,"_")[[1]][3]
  #set all NA values to "1" to prevent their parsing
  m[is.na(m)]<-1
  #Any count that travels more that 10km is removed
  distance=subset(m,m$EFFORT.DISTANCE.KM<=10)
  #Any count over 900 minutes is removed
  time=subset(distance,distance$DURATION.MINUTES<=900)
  keep<-c("TAXONOMIC.ORDER","SCIENTIFIC.NAME","LATITUDE","LONGITUDE")
  df<-time[keep]
  ##INSERT UNIQUE
  df1<-unique(df)
  #save dataframe
  filename=paste("parse_",y,".csv",sep="")
  print(paste("File :",y,"eBird parse saving."))
  write.csv(df1,file=filename,row.names=F)
  print("file created   ...done.")
}

#eBird data is now ready for georeferencing
##################################################################################################

#Combine raw eBird and GBIF data, BEFORE moving into M's, etc.
#D:\Hummingbirds\raw_GBIF\non-ebird
#D:\Hummingbirds\raw_eBird\parsed

##I need to extract lat and long from each, and combine them into one file - MAKE SURE THEY ALIGN
##Species names are going to differ, so I need to write it where every species will be combined appropriately
#Working on combining shapefiles from here on out
#For raw eBird (parsed)
setwd(paste0(filepath,"\\raw_GBIF\\non-ebird",sep=""))
##refining GBIF data to just coordinates
csvfiles=list.files(path=paste0(filepath,"\\raw_GBIF\\non-ebird",sep=""),pattern="*.csv")
keep=c("species","lat","lon")
for (i in 1:length(csvfiles)){
  x<-strsplit(as.character(csvfiles[i]),"_")[[1]][4]
  y<-strsplit(as.character(csvfiles[i]),"_")[[1]][5]
  z<-strsplit(as.character(csvfiles[i]),"_")[[1]][6]
  print(paste("working ",y," ",z))
  #remove extraneous columns
  taxa=read.csv(csvfiles[i])
  taxa1=taxa[keep]
  #save as format "reduced_[number]_[genus]_[species].csv"
  filename=paste("reduced_",x,"_",y,"_",z,".csv",sep="")
  print(paste("File :",y," ",z," eBird parse saving."))
  write.csv(taxa1,file=filename,row.names=F)
  print("file created   ...done.")
}

##put desired files into same directory, and run the following to combine coordinates

setwd(paste0(filepath,"\\concatonated\\to concatonate",sep=""))

#combining NOW
filepath="\\Divisions\\Ornithology\\Cooper\\Hummingbirds\\rawcoord"
setwd(paste0(filepath,"\\parse"))
list.files()
#must rename directory 'reduced'
setwd(paste0(filepath,"\\reduced"))
csvfiles=list.files(path=paste0(filepath,"\\reduced"),pattern="*.csv")
for(i in 1:length(csvfiles)){
  genus=strsplit(as.character(csvfiles[i]),"_")[[1]][3]
  species=strsplit(as.character(csvfiles[i]),"_")[[1]][4]
  gbif="gbif"
  file.rename(csvfiles[i],paste(gbif,"_",genus," ",species,sep=""))
}

#rename both to the SAME NAME so files can be set together - ie, one is 'G' read identical next
##rename to the same name for each file
setwd(paste0(filepath,"\\parse",sep=""))
csvfiles=list.files(path=paste0(filepath,"\\parse",sep=""),pattern="*.csv")
for(i in 1: length(csvfiles)){
  x=strsplit(as.character(csvfiles[i]),"_")[[1]][2]
  y=strsplit(as.character(x),"[.]")[[1]][1]
  file.rename(csvfiles[i],paste(y,".csv",sep=""))
}

#for next dataset
setwd(paste0(filepath,"\\reduced",sep=""))
csvfiles=list.files(path=paste0(filepath,"\\reduced",sep=""),pattern="*.csv")
for(i in 1:length(csvfiles)){
  x=strsplit(as.character(csvfiles[i]),"_")[[1]][2]
  y=strsplit(as.character(x),"[.]")[[1]][1]
  file.rename(csvfiles[i],paste(y,".csv",sep=""))
}

setwd(filepath)
reduced=list.files(path=paste0(filepath,"\\reduced"),pattern="*.csv")
parse=list.files(path=paste0(filepath,"\\parse"),pattern="*.csv")

##concatonate
for (i in 1:length(parse)){
  setwd(paste0(filepath,"\\parse"))
  file=as.character(parse[i])
  x=read.csv(file)
  species=strsplit(as.character(file),"[.]")[[1]][1]
  print(paste("Working...",species,sep=""))
  LATITUDE<-x$LATITUDE
  LONGITUDE<-x$LONGITUDE
  setwd(paste0(filepath,"\\reduced"))
  y=read.csv(file)
  lat<-y$lat
  lon<-y$lon
  lat<-c(lat,LATITUDE)
  long<-c(lon,LONGITUDE)
  df<-cbind(species,long,lat)
  filename=paste("raw_",species,".csv",sep="")
  print(paste("File :",species,"concatonated coordinates saving."))
  setwd(paste0(filepath,"\\finished",sep=""))
  write.csv(df,file=filename,row.names=F)
}

####################################################################################################
#Remove duplicate points
csvfiles=list.files(path=paste0(filepath,"\\concatonated\\finishfix",sep=""),pattern="*.csv")
setwd(paste0(filepath,"\\concatonated\\finishfix"))
for(i in 1:length(csvfiles)){
  setwd(paste0(filepath,"\\concatonated\\finishfix",sep=""))
  m=read.csv(csvfiles[i])
  x<-unique(m)
  name<-as.character(csvfiles[i])
  filename=paste("unique_",name,sep="")
  setwd(paste0(filepath,"\\concatonated\\nodup",sep=""))
  write.csv(x,file=filename,row.names=F)
}

###################################################################################################

#Converting files to Shapefiles for manipulation and editing
#work on inserting split species
#in order for this to rename with appropriate number, must have all files present and same length!
setwd(filepath)
m=read.csv("IOC_hummers.csv")
setwd(paste0(filepath,"\\concatonated\\nodup",sep=""))
csvfiles=list.files(path=paste0(filepath,"\\concatonated\\nodup",sep=""),pattern="*.csv")
for (i in 1:length(csvfiles)){
  setwd(paste0(filepath,"\\concatonated\\nodup",sep=""))
  x<-read.csv(csvfiles[i])
  z<-strsplit(as.character(csvfiles[i]),"[.]")[[1]][1]
  y<-strsplit(as.character(z),"_")[[1]][3]
  #extracts IOC taxon ID (again)
  k=which(m$SCIENTIFICNAME==y)
  num=m$CODE[k]
  name=paste0(num,"_",y,sep="")
  print(paste0("Converting file ",y),sep="")
  #save coordinates as combined and separate frames
  spd <- SpatialPointsDataFrame(x[,2:3],data.frame(x[,2:3]))
  #classify the projection, set the data frames
  proj4string(spd)<-CRS("+proj=longlat +datum=WGS84")
  print(paste0("Writing shapefile ",name,sep=""))
  setwd(paste0(filepath,"\\coord_shp",sep=""))
  #write ArcGIS shapefile
  writeOGR(spd,dsn=name,layer=name,driver="ESRI Shapefile")
}

##File rename just to ensure the ones I did earlier are in the same format
setwd(paste0(filepath,"\\coord_shp_edit\\From Before"))
shamwow=list.files(setwd(paste0(filepath,"\\coord_shp_edit\\From Before")))
for (i in 1:length(shamwow)){
  x=shamwow[i]
  y=strsplit(as.character(x),"_")[[1]][1]
  z=strsplit(as.character(x),"_")[[1]][2]
  j=strsplit(as.character(x),"_")[[1]][3]
  nombre=paste(y,"_",z," ",j,sep="")
  file.rename(x,nombre)
}

##################################################################################################

##From here on is AFTER data has been used to create M's, etc.

##Convert species Shapefiles to .csv files for rarefy and Maxent
##IMPORTANT!!!
####Remove migrant species first;place separately for everything else

######I have to reformat the Phaethornithidae for having too many data columns... damn

folders=list.files(paste0(filepath,"\\coord_shp_edit\\resident",sep=""))
for (i in 1:length(folders)){
  setwd(paste0(filepath,"\\coord_shp_edit","\\resident\\",folders[i],sep=""))
  shapes=list.files(pattern="*.shp")
  x<-readShapeSpatial(shapes)
  data=x@data
  print(shapes)
  name=strsplit(as.character(shapes),"[.]")[[1]][1]
  nombre=strsplit(as.character(name),"_")[[1]][2]
  setwd(paste0(filepath,"\\occurrences\\resident\\raw",sep=""))
  if (ncol(data)!=2){
    ebird<-which((data$LATITUDE!=0),(data$LONGITUDE!=0))
    gbif<-which((data$lat!=0),(data$lon!=0))
    gcol<-c("lon","lat")
    ecol<-c("LONGITUDE","LATITUDE")
    ebird1=data[ebird,ecol]
    gbif1=data[gbif,gcol]
    ebird2=rename(ebird1,c("LONGITUDE"="long","LATITUDE"="lat"))
    gbif2=rename(gbif1,c("lon"="long","lat"="lat"))
    data=rbind(ebird2,gbif2)
    print(paste0("writing file:",nombre),sep=" ")
  }else{
    print(paste0("writing file:",nombre),sep=" ")
  }
  z=cbind(species=nombre,data)
  write.csv(z,file=paste0(name,".csv",sep=""),row.names=F)
}

#now for migrants

shapes=list.files("D:\\Hummingbirds\\coord_shp_edit\\migrant\\todos",pattern="*.shp")
for (i in 1:length(shapely)){
  print(shapes[i])
  setwd("D:\\Hummingbirds\\coord_shp_edit\\migrant\\todos")
  x<-readShapeSpatial(shapes[i])
  data=x@data
  name=strsplit(as.character(shapes[i]),"[.]")[[1]][1]
  nombre=strsplit(as.character(name),"_")[[1]][2]
  setwd(paste0(filepath,"\\occurrences\\migrant\\raw",sep=""))
  print(paste0("writing file:",nombre),sep=" ")
  z=cbind(species=nombre,data)
  write.csv(z,file=paste0(name,".csv",sep=""),row.names=F)  
}

###################################################################################################
#Time to rarefy points to the 10 km interpoint distance

rarefy3 <- function(file, distance.km) {
  
  require(fossil)
  
  file <- file
  dist <- as.numeric(distance.km)
  
  x <- read.csv(file)
  x <- unique(x)
  
  x.new <- c()
  reps <- nrow(x)
  reps.x <- reps
  
  
  while (reps > 999) {
    
    x1 <- x[1:1000,]
    x <- x[1001:nrow(x),]
    reps <- nrow(x)
    
    reps2 <- nrow(x1)
    while(reps2 >= 1) {
      reps33 <- reps2 + reps
      print(reps33)
      if(reps2 > 1) { 
        
        lat <- x1[1,3]
        long <- x1[1,2]
        x.samp <- c(as.character(x1[1,1]), x1[1,2], x1[1,3])
        x.new <- rbind(x.new, x.samp)
        x1 <- x1[2:nrow(x1),]
        
        dist.rep <- c()
        reps2 <- nrow(x1)
        
        for(a in 1:reps2) {
          
          d.rep <- deg.dist(x1[a,2], x1[a,3], long, lat)
          dist.rep <- c(dist.rep, d.rep)
          
        }
        
        x1 <- x1[dist.rep > dist, ]
        reps2 <- nrow(x1)
        
      }
      
      if(reps2 == 1) {
        
        x.samp <- c(as.character(x1[1,1]), x1[1,2], x1[1,3])
        x.new <- rbind(x.new, x.samp)
        reps2 <- 0 
        
      }
      
    }
    
  }
  
  while(reps >= 1) {
    print(reps)
    if(reps > 1) { 
      
      lat <- x[1,3]
      long <- x[1,2]
      x.samp <- c(as.character(x[1,1]), x[1,2], x[1,3])
      x.new <- rbind(x.new, x.samp)
      x <- x[2:nrow(x),]
      
      dist.rep <- c()
      reps <- nrow(x)
      
      for(a in 1:reps) {
        
        d.rep <- deg.dist(x[a,2], x[a,3], long, lat)
        dist.rep <- c(dist.rep, d.rep)
        
      }
      
      x <- x[dist.rep > dist, ]
      reps <- nrow(x)
      
    }
    
    if(reps == 1) {
      
      x.samp <- c(as.character(x[1,1]), x[1,2], x[1,3])
      x.new <- rbind(x.new, x.samp)
      reps <- 0 
      
    }
    
  }
  
  if(reps.x > 999) {
    x.new <- na.omit(x.new)
    x.new <- data.frame(ID = x.new[,1], Lat = as.numeric(x.new[,2]), Long = as.numeric(x.new[,3]))
    x <- x.new
    x.new <- c()
    reps <- nrow(x)
    
    while(reps >= 1) {
      print(reps)
      if(reps > 1) { 
        
        lat <- x[1,3]
        long <- x[1,2]
        x.samp <- c(as.character(x[1,1]), x[1,2], x[1,3])
        x.new <- rbind(x.new, x.samp)
        x <- x[2:nrow(x),]
        
        dist.rep <- c()
        reps <- nrow(x)
        
        for(a in 1:reps) {
          
          d.rep <- deg.dist(x[a,2], x[a,3], long, lat)
          dist.rep <- c(dist.rep, d.rep)
          
        }
        
        x <- x[dist.rep > dist, ]
        reps <- nrow(x)
        
      }
      
      if(reps == 1) {
        
        x.samp <- c(as.character(x[1,1]), x[1,2], x[1,3])
        x.new <- rbind(x.new, x.samp)
        reps <- 0 
        
      }
      
    }
    
  }
  
  
  
  n <- nchar(file)
  n <- n - 4
  name <- substr(file, 1, n)
  name <- paste(name, "_subset_", dist, "km.csv", sep="")
  
  x.new <- na.omit(x.new)
  x.df <- data.frame(ID = x.new[,1], Long = x.new[,2], Long = x.new[,3])
  
  write.csv(x.df, file = name, row.names = FALSE)
  
}
x="\\occurrences\\"
m="migrant\\raw"
r="resident\\raw"

#migrant
setwd(dest)
files=list.files(dest)
for(i in 1:length(files)){
  rarefy3(file=files[i],distance.km=10)
}


rarefy4 <- function(file, distance.km) {
  
  require(fossil)
  setwd(paste0(filepath,x,r,sep=""))
  file <- file
  dist <- as.numeric(distance.km)
  
  x <- read.csv(file)
  x <- unique(x)
  
  x.new <- c()
  reps <- nrow(x)
  reps.x <- reps
  
  
  while (reps > 999) {
    
    x1 <- x[1:1000,]
    x <- x[1001:nrow(x),]
    reps <- nrow(x)
    
    reps2 <- nrow(x1)
    while(reps2 >= 1) {
      reps33 <- reps2 + reps
      print(reps33)
      if(reps2 > 1) { 
        
        lat <- x1[1,3]
        long <- x1[1,2]
        x.samp <- c(as.character(x1[1,1]), x1[1,2], x1[1,3])
        x.new <- rbind(x.new, x.samp)
        x1 <- x1[2:nrow(x1),]
        
        dist.rep <- c()
        reps2 <- nrow(x1)
        
        for(a in 1:reps2) {
          
          d.rep <- deg.dist(x1[a,2], x1[a,3], long, lat)
          dist.rep <- c(dist.rep, d.rep)
          
        }
        
        x1 <- x1[dist.rep > dist, ]
        reps2 <- nrow(x1)
        
      }
      
      if(reps2 == 1) {
        
        x.samp <- c(as.character(x1[1,1]), x1[1,2], x1[1,3])
        x.new <- rbind(x.new, x.samp)
        reps2 <- 0 
        
      }
      
    }
    
  }
  
  while(reps >= 1) {
    print(reps)
    if(reps > 1) { 
      
      lat <- x[1,3]
      long <- x[1,2]
      x.samp <- c(as.character(x[1,1]), x[1,2], x[1,3])
      x.new <- rbind(x.new, x.samp)
      x <- x[2:nrow(x),]
      
      dist.rep <- c()
      reps <- nrow(x)
      
      for(a in 1:reps) {
        
        d.rep <- deg.dist(x[a,2], x[a,3], long, lat)
        dist.rep <- c(dist.rep, d.rep)
        
      }
      
      x <- x[dist.rep > dist, ]
      reps <- nrow(x)
      
    }
    
    if(reps == 1) {
      
      x.samp <- c(as.character(x[1,1]), x[1,2], x[1,3])
      x.new <- rbind(x.new, x.samp)
      reps <- 0 
      
    }
    
  }
  
  if(reps.x > 999) {
    x.new <- na.omit(x.new)
    x.new <- data.frame(ID = x.new[,1], Lat = as.numeric(x.new[,2]), Long = as.numeric(x.new[,3]))
    x <- x.new
    x.new <- c()
    reps <- nrow(x)
    
    while(reps >= 1) {
      print(reps)
      if(reps > 1) { 
        
        lat <- x[1,3]
        long <- x[1,2]
        x.samp <- c(as.character(x[1,1]), x[1,2], x[1,3])
        x.new <- rbind(x.new, x.samp)
        x <- x[2:nrow(x),]
        
        dist.rep <- c()
        reps <- nrow(x)
        
        for(a in 1:reps) {
          
          d.rep <- deg.dist(x[a,2], x[a,3], long, lat)
          dist.rep <- c(dist.rep, d.rep)
          
        }
        
        x <- x[dist.rep > dist, ]
        reps <- nrow(x)
        
      }
      
      if(reps == 1) {
        
        x.samp <- c(as.character(x[1,1]), x[1,2], x[1,3])
        x.new <- rbind(x.new, x.samp)
        reps <- 0 
        
      }
      
    }
    
  }
  
  
  
  n <- nchar(file)
  n <- n - 4
  name <- substr(file, 1, n)
  name <- paste(name, "_subset_", dist, "km.csv", sep="")
  
  x.new <- na.omit(x.new)
  x.df <- data.frame(ID = x.new[,1], Long = x.new[,2], Long = x.new[,3])
  setwd("D:\\Hummingbirds\\occurrences\\resident\\10km")
  write.csv(x.df, file = name, row.names = FALSE)
  
}

#resident
setwd(paste0(filepath,x,r,sep=""))
files=list.files(paste0(filepath,x,r,sep=""))
for(i in 1:length(files)){
  rarefy4(file=files[i],distance.km=10)
}

#################################################################################################
#Files have incorrect column names - long.1 should be lat
#need to change ID to species, Long to long, and Long.1 to lat

setwd(paste0(filepath,"\\occurrences\\migrant\\10km",sep=""))
filelist=list.files(pattern="*.csv")
for (i in 1:length(filelist)){
  m=read.csv(filelist[i])
  name=as.character(filelist[i])
  x<-c("species","long","lat")
  colnames(m)=x
  write.csv(m,file=name,row.names=F)
}

###################################################################################################
#Clip rasters to the Ms of each species

g=paste0(filepath,"\\bioclim\\raw\\present",sep="")
bioclim=list.files(g)
#rename bioclim files for use as .asc
setwd(g)
for(i in 1:length(bioclim)){
  name=as.character(bioclim[i])
  newname=strsplit(as.character(name),"[.]")[[1]][1]
  x=paste(newname,".asc",sep="")
  file.rename(bioclim[i],x)
}


#Set lists for shapefiles of M and for bioclim layers of datasets
m=list.files(paste0(filepath,"\\M",sep=""),pattern="*.shp")
bioclim=list.files(paste0(filepath,"\\bioclim30s\\raw\\present",sep=""))

#Set list of *.ascii to crop (they are originally TXT!)
files=bioclim
#Locate and list shapefile to use for the cropping
shapes=m

CropLoop<-function(filelist=NA,ShapeFile=NA,SaveDir)
{
  Shp1 = readShapePoly(ShapeFile)
  for (i in 1:length(files))
  {
    r1 = raster(filelist[i])
    cr1 = crop(r1,Shp1)
    cr2 = mask(cr1,Shp1)
    ### For file with extension. 
    #FileName = paste(substr(filelist[i],1,nchar(filelist[i])-4),ext1,substr(filelist[i],nchar(filelist[i])-3,nchar(filelist[i])),sep="")
    #####JCC is putting in new naming protocols HERE
    ### For files without extension
    j=strsplit(as.character(filelist[i]),"[\\]")[[1]][8] #Always check this - make it be the "bioX" term
    FileName=strsplit(as.character(j),"[.]")[[1]][1]
    #writeRaster(cr2,FileName, "ascii")
    writeRaster(cr2,paste(SaveDir,"\\",FileName,sep=""),"ascii")
    plot(cr2)
    print(FileName)
  }
}

#Shapefile loop that runs CropLoop function
for(i in 1:length(shapes)){
  k<-strsplit(as.character(shapes[i]),"[.]")[[1]][1]
  print(paste0("Starting species ",k))
  ext<-"\\bioclim\\clipped\\present\\"
  print("Creating directory...")
  dir.create(paste0(filepath,ext,k,sep=""),showWarnings=FALSE)
  newdir<-(paste0(filepath,ext,k,sep=""))
  print("Cropping ascii files...")
  CropLoop(filelist=paste0(filepath,"\\bioclim\\raw\\present\\",files,sep=""),ShapeFile=paste0(filepath,"\\M\\",shapes[i]),SaveDir=newdir)
  print(paste0("Crop completed for ",k))
}
####renaming shapefiles

#file.rename
setwd("D:\\Phaethornis\\M")
folders=list.files(path=(filepath"\\M")) #list of all folders for shapefiles
for (i in 1:length(folders)){
  k=folders[i] #save name of species for other use
  j=strsplit(as.character(k),"_")[[1]][1]
  f=strsplit(as.character(k),"_")[[1]][2]
  g=strsplit(as.character(k),"_")[[1]][3]
  name=paste0(j,"_",f," ",g,sep="")
  print(paste0("Reading file ",name,sep=""))
  file.rename(k,name)
}

###################################################################################################
#Renaming files to match
#original format is ####_genus species_subset_10km
#desired format is ####_genus_species
setwd(paste0(filepath,"\\occurrences\\resident\\10km"))
filelist=list.files(pattern="*.csv")
for(i in 1:length(filelist)){
  x=as.character(filelist[i])
  y=strsplit(x," ")[[1]][1]
  z=strsplit(x," ")[[1]][2]
  a=strsplit(z,"[.]")[[1]][1]
  b=strsplit(a,"_")[[1]][1]
  name=paste(y,"_",b,".csv",sep="")
  file.rename(x,name)
}
##################################################################################################

#rename migrant files

setwd(paste0(filepath,"\\occurrences\\migrant\\10km"))
csv=list.files(pattern="*.csv")
for (i in 1:length(csv)){
  name=as.character(csv[i])
  number=strsplit(name,"_")[[1]][1]
  species=strsplit(name,"_")[[1]][2]
  x=strsplit(species," ")[[1]][1]
  y=strsplit(species," ")[[1]][2]
  z=strsplit(species," ")[[1]][3]
  file=paste0(number,"_",x,"_",y,"_",z,".csv")
  file.rename(name,file)
}
#Run MAXENT model of species with Ms

#set directory
filepath="Directory\\Hummingbirds"
#set resident species occurrence points
all=paste0(filepath,"\\occurrences\\all\\10km",sep="")
resident=paste0(filepath,"\\occurrences\\resident\\10km",sep="")
migrant=paste0(filepath,"\\occurrences\\migrant\\10km",sep="")

#bioclim layer directory
bioclim=paste0(filepath,"\\bioclim2.5m\\clipped\\present",sep="")

#begin adjusting previous script
#list resident species
rocc=list.files(resident,pattern="*.csv")
#list migrant species
mocc=list.files(migrant,pattern="*.csv")

work=rocc[27:319]

RunMaxent<-function(filepath,filelist){
  setwd(filepath)
  for (i in 1:length(filelist)){
    ptm=proc.time()
    #set species string that is identical to folders
    name=filelist[i]
    species=strsplit(as.character(name),"[.]")[[1]][1]
    print(paste0("Working:",species))
    
    #setwd to bioclim clipped to each species
    setwd(paste0(filepath,"\\bioclim\\clipped\\present\\",species))
    #set layers - these should be identical for each species
    biofiles=(list.files(paste0(filepath,"\\bioclim\\clipped\\present\\",species),pattern="*.asc"))
    predictors=stack(biofiles) #stack ascii files
    
    #load occurrence points for each species
    occfile=paste0(filepath,"\\occurrences\\resident\\10km\\",species,".csv",sep="")
    occur=read.csv(occfile)
    occur <- occur[,-1]
    names(occur)<-c("lon","lat")
    
    #create directory for output
    dir.create(paste0(filepath,"\\output\\",species,sep=""))
    setwd(paste0(filepath,"\\output\\",species,sep=""))
    
    ###run maxent
    xm1=maxent(predictors,occur,args=c("nowarnings",
                                       "noprefixes",
                                       "responsecurves",
                                       "jackknife",
                                       "outputformat=raw",
                                       "randomseed",
                                       "replicates=5", #each is 500 runs, total 2500
                                       "replicatetype=bootstrap",
                                       "nodoclamp"))
    ###get the average of the calibration and save
    ModelPred=predict(xm1,predictors)
    ModAvg=mean(ModelPred)
    ModStd=calc(ModelPred,fun=sd)
    avgpathway=paste0(filepath,"\\output\\",species,"\\",species,"_avg.asc",sep="")
    writeRaster(ModAvg,avgpathway,overwrite=T)
    stdpathway=paste0(filepath,"\\output\\",species,"\\",species,"_stddev_avg.asc",sep="")
    writeRaster(ModStd,stdpathway,overwrite=T)
    
    print(proc.time()-ptm)
    
  }
}

###############################################################################################

#Creating MAXENT loop that does multiple conditions
#The following codes were adjustments on the above to run MAXENT models for 
#different raster resolutions. 2.5 Arcminutes was used in the paper.

#check to see if worked ok
filefile="Directory:\\Hummingbirds\\output"
x=list.files(filefile)
z=x[268:314]
for (i in 1:length(z)){
  name=z[i]
  print(paste("Working ",name,sep=""))
  setwd(paste(filefile,"\\",name,sep=""))
  y=raster(paste(name,"_avg.asc",sep=""))
  plot(y)
}

#set directory
filepath="Directory:\\Hummingbirds"
#set resident species occurrence points
resident=paste0(filepath,"\\occurrences\\resident\\10km",sep="")
migrant=paste0(filepath,"\\occurrences\\migrant\\10km",sep="")

#bioclim layer directory
bioclim=paste0(filepath,"\\bioclim\\clipped\\present",sep="")

#begin adjusting previous script
#list resident species
rocc=list.files(resident,pattern="*.csv")
#list migrant species
mocc=list.files(migrant,pattern="*.csv")

#Run MAXENT model of species with Ms
##specifically for running everything on the lab computers; including migrants in run

#set directory
filepath=
#set resident species occurrence points
resident=paste0(filepath,"\\occurrences\\resident\\10km",sep="")
migrant=paste0(filepath,"\\occurrences\\migrant\\10km",sep="")

#bioclim layer directory
bioclim=paste0(filepath,"\\bioclim\\clipped\\present",sep="")

#begin adjusting previous script
#list resident species
rocc=list.files(resident,pattern="*.csv")
#list migrant species
mocc=list.files(migrant,pattern="*.csv")

work=list.files(paste(filepath,"\\occurrences\\all\\10km",sep=""),pattern="*.csv")

#2.5m

RunMaxent2.5<-function(filepath,filelist){
  setwd(filepath)
  for (i in 1:length(filelist)){
    ptm=proc.time()
    #set species string that is identical to folders
    name=filelist[i]
    species=strsplit(as.character(name),"[.]")[[1]][1]
    print(paste0("Working:",species))
    
    #setwd to bioclim clipped to each species
    setwd(paste0(filepath,"\\bioclim\\clipped\\present\\",species,sep=""))
    #set layers - these should be identical for each species
    biofiles=(list.files(paste0(filepath,"\\bioclim\\clipped\\present\\",species,sep=""),pattern="*.asc"))
    predictors=stack(biofiles) #stack ascii files
    
    print("Biofiles stacked.")
    
    #load occurrence points for each species
    occfile=paste0(filepath,"\\occurrences\\all\\10km\\",species,".csv",sep="")
    occur=read.csv(occfile)
    occur <- occur[,-1]
    names(occur)<-c("lon","lat")
    print("occurrences loaded.")
    
    #create directory for output
    dir.create(paste0(filepath,"\\output\\Bioclim2.5m\\",species,sep=""))
    setwd(paste0(filepath,"\\output\\Bioclim2.5m\\",species,sep=""))
    print("Directory created.")
    
    ###run maxent
    xm1=maxent(predictors,occur,args=c("nowarnings",
                                       "noprefixes",
                                       "responsecurves",
                                       "jackknife",
                                       "outputformat=raw",
                                       "randomseed",
                                       "replicates=5", #each is 500 runs, total 2500
                                       "replicatetype=bootstrap",
                                       "nodoclamp"))
    ###get the average of the calibration and save
    print("MAXENT complete. Saving...")
    ModelPred=predict(xm1,predictors)
    ModAvg=mean(ModelPred)
    ModStd=calc(ModelPred,fun=sd)
    avgpathway=paste0(filepath,"\\output\\Bioclim2.5m\\",species,"\\",species,"_avg.asc",sep="")
    writeRaster(ModAvg,avgpathway,overwrite=T)
    stdpathway=paste0(filepath,"\\output\\Bioclim2.5m\\",species,"\\",species,"_stddev_avg.asc",sep="")
    writeRaster(ModStd,stdpathway,overwrite=T)
    plot(raster(paste0(filepath,"\\output\\Bioclim2.5m\\",species,"\\",species,"_avg.asc",sep="")))
    print(proc.time()-ptm)
    
  }
}

#time to run them

RunMaxent2.5(filepath=filepath,filelist=work)

##############################################################################################

#re-running MAXENT for chosen few species and for migratory species

j=work[50]
k=work[331]
l=work[350]

RunMaxent2.5(filepath=filepath,filelist=c(j,k,l))

#####################################################################################################
#Determine which species lack sufficient points
setwd(filepath)
list.files()
setwd(paste(filepath,"\\occurrences\\all\\10km",sep=""))
files=list.files(paste(filepath,"\\occurrences\\all\\10km",sep=""),pattern="*.csv")
name1="LIST OF SPECIES"
for(i in 1:length(files)){
  x=read.csv(files[i])
  y=length(x$species)
  if (y<=5){
    name=x$species[1]
    name1=c(name1,name)
    print(paste(name, "below threshold",sep=" "))
  } else {
    print(paste("Species OK"))
  }
}

#species below threshold:
#Aglaiocercus berlepschi
#Augustes lumachella
#Loddigesia mirabilis
#Chaetocercus astreans
#Chaetocercus berlepschi
#Selasphorus ardens
#Anopetia gounellei
#Phaethornis aethopyga
#Campylopterus phaenopeplus
#Lophornis gouldii
#Lophornis brachylophus
#Trochilus scitulus
#Chlorostilbon russatus
#Geothalsia bella
#Goldmania violiceps
#Lepidopyga lilliae
#Hylocharis humboldtii
#Amazilia castaneiventris
#Anthocephala floriceps
#Lampornis castaneoventris
#Heliodoxa gularis
#Aglaeactis aliciae
#Coeligena phalerata
#Coeligena orina
#Heliangelus micraster
#Eriocnemis nigrivestis
#Ramphomicron dorsale
#Metallura baroni
#Metallura odomae
#Taphrolesbia griseiventris
###30 total


###############################################################################################
#Checking plots before creating binary maps for species richness

filepath="Directory:\\Hummingbirds"
setwd(paste0(filepath,"\\output\\",sep=""))
list.files()

#setfilepaths
##this runs off of the DIVISIONS hard-drive
hummers="Directory\\Hummingbirds"

output=(paste0(hummers,"\\output",sep=""))
roccur=paste0(hummers,"\\occurrences\\all\\10km",sep="")
rich=paste0(hummers,"\\richness",sep="")

work1=list.files(paste0(output,"\\Bioclim2.5m",sep=""))
work2=list.files(paste0(output,"\\Bioclim30s",sep=""))

plotcheck<-function(filepath,resol,names){
  for (i in 1:length(names)){
    #Set species name so that the raster of the avg. asc. can be read, and then read the occ. file
    species=as.character(names[i])
    setwd(paste0(filepath,"\\output\\",resol,"\\",species,sep=""))
    print(paste0("Setting Directory for Reading: ",species,sep=""))
    x=raster(paste0(species,"_avg.asc",sep=""))
    plot(x)
    setwd(roccur)
    y=read.csv(paste0(species,".csv",sep=""))
    points(y)
  }
  #setwd(paste0(filepath,"\\richness\\",sep=""))
  #write.csv(avg,file="Average_Points_Used.csv",row.names=F)
}

plotcheck(filepath=hummers,resol="Bioclim2.5m",names=work)
plotcheck(filepath=hummers,resol="Bioclim30s",names=work)

migrant=c("9750_Eugenes_fulgens_sumall","9750_Eugenes_fulgens_winall",
          "9558_Amazilia_yucatanensis_sumall","9558_Amazilia_yucatanensis_winall",
          "9315_Anthracothorax_prevostii_sumall","9315_Anthracothorax_prevostii_winall",
          "10116_Selasphorus_calliope_sumall","10116_Selasphorus_calliope_winall",
          "10107_Selasphorus_sasin_sumall","10107_Selasphorus_sasin_winall",
          "10106_Selasphorus_rufus_sumall","10106_Selasphorus_rufus_winall",
          "10105_Selasphorus_platycercus_sumall","10105_Selasphorus_platycercus_winall",
          "10076_Calypte_costae_sumall","10076_Calypte_costae_winall",
          "10075_Calypte_anna_sumall","10075_Calypte_anna_winall",
          "10073_Archilochus_alexandri_sumall","10073_Archilochus_alexandri_winall",
          "10072_Archilochus_colubris_sumall","10072_Archilochus_colubris_winall",
          "10064_Calothorax_lucifer_sumall","10064_Calothorax_lucifer_winall")

##########################################################################################################

#Now to threshold based off of E space; as Town Peterson suggested, we are thresholding 
#based off of E space limitations
##Distributions will be analyzed to see which threshold best fits the species
###This will be done for all!

output=(paste0(hummers,"\\output",sep=""))
roccur=paste0(hummers,"\\occurrences\\all\\10km",sep="")
rich=paste0(hummers,"\\richness",sep="")

work1=list.files(paste0(output,"\\10m",sep=""))
work2=list.files(paste0(output,"\\Bioclim30s",sep=""))

#define which set of resolutions you are dealing with initially
#color map of different quantile levels
rasterEtotal<-function(filepath,resol,names){
  for (i in 1:length(names)){
    #Set species name so that the raster of the avg. asc. can be read, and then read the occ. file
    species=as.character(names[i])
    setwd(paste0(filepath,"\\output\\",resol,"\\",species,sep=""))
    print(paste0("Setting Directory for Reading: ",species,sep=""))
    x=raster(paste0(species,"_avg.asc",sep=""))
    #if (resol!="Bioclim30s"){plot(x)}    
    setwd(roccur)
    y=read.csv(paste0(species,".csv",sep=""))
    ext=extract(x=x,y=y[,2:3])
    ext=na.omit(ext)
    
    f=max(quantile(ext,0.01))
    g=max(quantile(ext,0.05))
    h=max(quantile(ext,0.1))
    j=max(quantile(ext,0.15))
    k=max(quantile(ext,0.2))
    
    sr=c(0,f,0,f,g,1,g,h,2,h,j,3,j,k,4,k,1,5)
    rclmat=matrix(sr,ncol=3,nrow=6,byrow=TRUE)
    bix=reclassify(x,rclmat)
    plot(bix,main=species,xlab="Longitude",ylab="Lattitude")
    points(x=y$long,y=y$lat,pch=20)
    
    pathway=paste0(filepath,"\\output\\binary\\",resol,"\\etotal\\",species,"_binary.asc",sep="")
    writeRaster(bix,pathway,overwrite=T)
  }
  #setwd(paste0(filepath,"\\richness\\",sep=""))
  #write.csv(avg,file="Average_Points_Used.csv",row.names=F)
}

#Variations of the below can be made for any threshold value.

rasterE90<-function(filepath,resol,names){
  for (i in 1:length(names)){
    #Set species name so that the raster of the avg. asc. can be read, and then read the occ. file
    species=as.character(names[i])
    setwd(paste0(filepath,"\\output\\",resol,"\\",species,sep=""))
    print(paste0("Setting Directory for Reading: ",species,sep=""))
    x=raster(paste0(species,"_avg.asc",sep=""))
    #if (resol!="Bioclim30s"){plot(x)}    
    setwd(roccur)
    y=read.csv(paste0(species,".csv",sep=""))
    ext=extract(x=x,y=y[,2:3])
    ext=na.omit(ext)
    
    #f=max(quantile(ext,0.01))
    #g=max(quantile(ext,0.05))
    j=max(quantile(ext,0.1))
    #j=max(quantile(ext,0.15))
    #k=max(quantile(ext,0.2))
    
    #sr=c(0,f,0,f,g,1,g,h,2,h,j,3,j,k,4,k,1,5)
    #rclmat=matrix(sr,ncol=3,nrow=6,byrow=TRUE)
    sr=c(0,j,0,j,1,1)
    rclmat=matrix(sr,ncol=3,nrow=2,byrow=TRUE)
    bix=reclassify(x,rclmat)
    #plot(bix,main=species,sub="E90",xlab="Longitude",ylab="Lattitude")
    #points(x=y$long,y=y$lat,pch=20)
    
    pathway=paste0(filepath,"\\output\\binary\\",resol,"\\90e\\",species,"_binary.asc",sep="")
    writeRaster(bix,pathway,overwrite=T)
  }
  #setwd(paste0(filepath,"\\richness\\",sep=""))
  #write.csv(avg,file="Average_Points_Used.csv",row.names=F)
}
rasterE95<-function(filepath,resol,names){
  for (i in 1:length(names)){
    #Set species name so that the raster of the avg. asc. can be read, and then read the occ. file
    species=as.character(names[i])
    setwd(paste0(filepath,"\\output\\",resol,"\\",species,sep=""))
    print(paste0("Setting Directory for Reading: ",species,sep=""))
    x=raster(paste0(species,"_avg.asc",sep=""))
    #if (resol!="Bioclim30s"){plot(x)}    
    setwd(roccur)
    y=read.csv(paste0(species,".csv",sep=""))
    ext=extract(x=x,y=y[,2:3])
    ext=na.omit(ext)
    
    #f=max(quantile(ext,0.01))
    j=max(quantile(ext,0.05))
    #h=max(quantile(ext,0.1))
    #j=max(quantile(ext,0.15))
    #k=max(quantile(ext,0.2))
    
    #sr=c(0,f,0,f,g,1,g,h,2,h,j,3,j,k,4,k,1,5)
    #rclmat=matrix(sr,ncol=3,nrow=6,byrow=TRUE)
    sr=c(0,j,0,j,1,1)
    rclmat=matrix(sr,ncol=3,nrow=2,byrow=TRUE)
    bix=reclassify(x,rclmat)
    #plot(bix,main=species,sub="E90",xlab="Longitude",ylab="Lattitude")
    #points(x=y$long,y=y$lat,pch=20)
    
    pathway=paste0(filepath,"\\output\\binary\\",resol,"\\95e\\",species,"_binary.asc",sep="")
    writeRaster(bix,pathway,overwrite=T)
  }
  #setwd(paste0(filepath,"\\richness\\",sep=""))
  #write.csv(avg,file="Average_Points_Used.csv",row.names=F)
}
rasterE99<-function(filepath,resol,names){
  for (i in 1:length(names)){
    #Set species name so that the raster of the avg. asc. can be read, and then read the occ. file
    species=as.character(names[i])
    setwd(paste0(filepath,"\\output\\",resol,"\\",species,sep=""))
    print(paste0("Setting Directory for Reading: ",species,sep=""))
    x=raster(paste0(species,"_avg.asc",sep=""))
    #if (resol!="Bioclim30s"){plot(x)}    
    setwd(roccur)
    y=read.csv(paste0(species,".csv",sep=""))
    ext=extract(x=x,y=y[,2:3])
    ext=na.omit(ext)
    
    j=max(quantile(ext,0.01))
    #g=max(quantile(ext,0.05))
    #h=max(quantile(ext,0.1))
    #j=max(quantile(ext,0.15))
    #k=max(quantile(ext,0.2))
    
    #sr=c(0,f,0,f,g,1,g,h,2,h,j,3,j,k,4,k,1,5)
    #rclmat=matrix(sr,ncol=3,nrow=6,byrow=TRUE)
    sr=c(0,j,0,j,1,1)
    rclmat=matrix(sr,ncol=3,nrow=2,byrow=TRUE)
    bix=reclassify(x,rclmat)
    #plot(bix,main=species,sub="E90",xlab="Longitude",ylab="Lattitude")
    #points(x=y$long,y=y$lat,pch=20)
    
    pathway=paste0(filepath,"\\output\\binary\\",resol,"\\99e\\",species,"_binary.asc",sep="")
    writeRaster(bix,pathway,overwrite=T)
  }
  #setwd(paste0(filepath,"\\richness\\",sep=""))
  #write.csv(avg,file="Average_Points_Used.csv",row.names=F)
}

#example of usage:
rasterE90(filepath=hummers,resol="Bioclim2.5",names=work2)
rasterE95(filepath=hummers,resol="Bioclim2.5",names=work2)
rasterE99(filepath=hummers,resol="Bioclim2.5",names=work2)
rasterEtotal(filepath=hummers,resol="Bioclim2.5",names=work2)

#The following checks points on the plots of the thresholded ENMs.
for (i in 1:length(g)){
  setwd(j)
  #species=strsplit(as.character(g[i]),"_binary")[[1]][1]
  species=as.character(g[i])
  x=raster(paste(species,"_binary.asc",sep=""))
  setwd(roccur)
  y=read.csv(paste0(species,".csv",sep=""))
  plot(x,main=species)
  points(x=y$long,y=y$lat,pch=20)
  readline(prompt="Press [enter] to continue")
}

##########################################################################################################
#The following compiles the richness rasters for extracting species' predictions
setwd(paste(filepath,"\\results\\10m\\raw\\",sep=""))
x=list.files(paste(filepath,"\\results\\95\\raw",sep=""),pattern="*.asc")

richer=function(filelist){
  #set up the initial "reference raster"
  a=filelist[1]
  a=raster(a)
  #f=extent(-82,-67,-19,0) #extent of specific country
  e=extent(-170,-34,-56,65) #xmin,xmax,ymin,ymax
  e=raster(e,nrows=1,ncols=1,crs=a@crs)
  res(e)=res(a)
  values(e)=0
  
  #declare functions
  for (j in 1:length(filelist)){
    #create objects
    k=raster(filelist[j])
    k[is.na(k)]<-0
    #crop(k,f) #this is making it a specific country; should now be all inclusive
    k=resample(k,e,method="ngb")
    print(filelist[j])
    #plot(k)
    k[is.na(k)]<-0
    
    pathway=paste0(filepath,"\\results\\10m\\reprojected\\",filelist[j],sep="")
    writeRaster(k,pathway,overwrite=T)
    
    print("Complete. Accessing Next: ")
  }
}


setwd(paste(filepath,"\\results\\10m\\reprojected",sep=""))
z=list.files(pattern="*.asc")

for(i in 1:length(z)){
  name=as.character(z[i])
  x=raster(z[i])
  print("Raster loaded.")
  #plot(x,main=name)
  x[is.na(x)]<-0
  #plot(x,main=paste(name,"converted.",sep=" "))
  print("Raster converted.")
  pathway=paste(filepath,"\\results\\95\\reprojected\\",name,sep="")
  print(paste("Writing raster",name,sep=" "))
  writeRaster(x,pathway,overwrite=T)
}

setwd(paste(filepath,"\\results\\10m\\reprojected",sep=""))
z=list.files(pattern="*.asc")
Q=stack(z)
richness=calc(Q,sum)
pathway=(paste(filepath,"\\results\\10m\\stacked\\global.asc",sep=""))
writeRaster(richness,pathway,overwrite=T)

plot(richness)
rm(c(x,y))

########################################################################################################

##Testing localities against the matrix, defined as q in this R session (see above; stacked rasters of "q")

#this format to extract by a file defined at the occurrence files
ext=extract(x=q,y=pts[,2:3])
qw=which(ext==1)
ext[,qw]

#######################################################################################################

#'The following takes each thresholded binary raster and reprojects it to an equal extent
#'so that the rasters may be properly stacked and summed.

filepath="//media//kupeornis//My Passport//DIVISIONS//Hummingbirds"

setwd(paste(filepath,"//results//locs_rem//raw//",sep=""))
x=list.files(paste(filepath,"//results//95//raw",sep=""),pattern="*.asc")

richer=function(filelist){
  #set up the initial "reference raster"
  a=filelist[1]
  a=raster(a)
  #f=extent(-82,-67,-19,0) #extent of specific country
  e=extent(-170,-34,-56,65) #xmin,xmax,ymin,ymax
  e=raster(e,nrows=1,ncols=1,crs=a@crs)
  res(e)=res(a)
  values(e)=0
  
  #declare functions
  for (j in 1:length(filelist)){
    #create objects
    k=raster(filelist[j])
    k[is.na(k)]<-0
    #crop(k,f) #this is making it a specific country; should now be all inclusive
    k=resample(k,e,method="bilinear")
    print(filelist[j])
    #plot(k)
    k[is.na(k)]<-0
    
    pathway=paste0(filepath,"//results//locs_rem//reprojected//",filelist[j],sep="")
    writeRaster(k,pathway,overwrite=T)
    
    print("Complete. Accessing Next: ")
  }
}

setwd(paste(filepath,"//results//locs_rem//reprojected",sep=""))
z=list.files(pattern="*.asc")

for(i in 1:length(z)){
  name=as.character(z[i])
  x=raster(z[i])
  print("Raster loaded.")
  #plot(x,main=name)
  x[is.na(x)]<-0
  #plot(x,main=paste(name,"converted.",sep=" "))
  print("Raster converted.")
  pathway=paste(filepath,"//results//locs_rem//reprojected//",name,sep="")
  print(paste("Writing raster",name,sep=" "))
  writeRaster(x,pathway,overwrite=T)
}

setwd(paste(filepath,"//results//locs_rem//reprojected",sep=""))
z=list.files(pattern="*.asc")
Q=stack(z)
richness=calc(Q,sum)
pathway=(paste(filepath,"//results//locs_rem//stacked//global.asc",sep=""))
writeRaster(richness,pathway,overwrite=T)

plot(richness)
rm(c(x,y))

#'###################################################################
#Code for extracting species names
#This portion of the code identifies the localities and creates lists of the 
#grid cells that constitute a locality. These can then be used to extract
#each locality's species list.

#Extraction code for the Masters Thesis
#Extract radius of 20 km to get the true locality
##working with locality data

setwd(filepath)
list.files(pattern="*.csv")
loc.new=read.csv("Localities_thesis.csv")
loc.new=loc.new[,2:4]
colnames(loc.new)=c("Loc","Long","Lat")

#The following has already been calculated and saved for the Masters Thesis; see folder entitled 'Extracts'.
reference=raster(z[1])
for(i in 1:nrow(loc.new)){
  val=extract(richness,y=loc.new[i,2:3],cellnumbers=TRUE)
  val=val[1]
  if(i==1){
    CellNum=val
  }else{
    CellNum=c(CellNum,val)
  }
}

loc.cell=cbind(loc.new,CellNum)
loc.cell2=loc.cell[,-2]
loc.cell3=unique(loc.cell2)
rows=which(unique(loc.cell1))
write.csv(loc.cell,file="Localities_thesis_cellnum.csv",row.names=FALSE)

#Again, all cells with centroids within 20 km of the point were used to represent the community composition

localities=(loc.cell[,2:3])
estlist=extract(Q,localities)
write.csv(estlist,file="Locality_estimate_20kbuffer.csv",row.names=FALSE)

cnum=read.csv("Localities_thesis_cellnum.csv")
xy=xyFromCell(Q,cnum[1,4],spatial=FALSE)
x=xy[1]
y=xy[2]
cells=cnum[,4]
for(i in 1:length(cells)){
  xy=xyFromCell(Q,cells[i],spatial=FALSE)
  x=xy[1]
  y=xy[2]
  if(i==1){
    X=x
    Y=y
  }else{
    X=c(X,x)
    Y=c(Y,y)
  }
}
new.cnum=cbind(cnum,X)
new.cnum=cbind(new.cnum,Y)
locs=new.cnum
#specify distance km
distance=20

#for identifying distances
#immediately adjacent cells
for(p in 1:nrow(locs)){
  cells=locs[p,4]
  a=adjacent(Q,cells[1],directions="bishop",pairs=TRUE,target=NULL,sorted=FALSE,include=FALSE,id=FALSE)
  b=a[1,2]
  c=a[2,2]
  d=a[3,2]
  e=a[4,2]
  b.a=adjacent(Q,b,directions=8,pairs=FALSE,target=NULL,sorted=TRUE,include=TRUE,id=FALSE)
  c.a=adjacent(Q,c,directions=8,pairs=FALSE,target=NULL,sorted=TRUE,include=TRUE,id=FALSE)
  d.a=adjacent(Q,d,directions=8,pairs=FALSE,target=NULL,sorted=TRUE,include=TRUE,id=FALSE)
  e.a=adjacent(Q,e,directions=8,pairs=FALSE,target=NULL,sorted=TRUE,include=TRUE,id=FALSE)
  all=c(b.a,c.a,d.a,e.a)
  all=unique(all)
  all=all[order(all)]
  b.2=all[1]
  c.2=all[5]
  d.2=all[21]
  e.2=all[25]
  b.2a=adjacent(Q,b.2,directions="bishop",pairs=FALSE,target=NULL,include=FALSE,id=FALSE)
  c.2a=adjacent(Q,c.2,directions="bishop",pairs=FALSE,target=NULL,include=FALSE,id=FALSE)
  d.2a=adjacent(Q,d.2,directions="bishop",pairs=FALSE,target=NULL,include=FALSE,id=FALSE)
  e.2a=adjacent(Q,e.2,directions="bishop",pairs=FALSE,target=NULL,include=FALSE,id=FALSE)
  list=c(b.2a,c.2a,d.2a,e.2a)
  for(j in 1:length(list)){
    cell.1=list[j]
    cell.2=adjacent(Q,cell.1,directions=8,pairs=FALSE,target=NULL,sorted=TRUE,include=TRUE,id=FALSE)
    if(j==1){
      ALL=cell.2
    }else{
      ALL=c(ALL,cell.2)
    }
  }
  ALL=ALL[order(ALL)]
  ALL=unique(ALL)
  x1=locs[p,2]
  y1=locs[p,3]
  Long=x1
  Lat=y1
  for(k in 1:length(ALL)){
    xy=xyFromCell(Q,ALL[k],spatial=F)
    x2=xy[1]
    y2=xy[2]
    f=deg.dist(x1,y2,x2,y2)
    if(f<=distance){
      Long=c(Long,x2)
      Lat=c(Lat,y2)
    }
  }
  Loc=as.character(locs[p,1])
  locs.site=cbind(Loc,Long,Lat)
  write.csv(locs.site,file=paste(Loc,"_coordinates_",distance,"_km.csv"),row.names=FALSE)
}

#Next step

cnames<-colnames(estlist)
length(cnames)
for(i in 1:length(cnames)){
  genus=strsplit(cnames[i],"_")[[1]][2]
  species=strsplit(cnames[i],"_")[[1]][3]
  name=paste(genus,species,sep=" ")
  if(i==1){
    newnames=name
  }else{
    newnames=c(newnames,name)
  }
}
colnames(estlist)<-newnames
estloc=cbind(localities,estlist)
estloc[1:3,1:3]

#write.csv(estloc,file="file.csv",row.names=FALSE)

#Estimate of the coordinates and species list.

setwd(paste(filepath,"//Extracts",sep=""))
locs.thes=read.csv("Localities_thesis.csv")
keep=locs.thes[,2:4]
colnames(keep)=c("Loc","Long","Lat")
for(i in 1:nrow(keep)){
  Loc=as.character(keep$Loc[i])
  Long=keep$Long[i]
  Lat=keep$Lat[i]
  x=cbind(Loc,Long,Lat)
  write.csv(x,paste(Loc,"_coordinates_0_km.csv",sep=""),row.names=F)
}


############################################################################################

###Bash script to extract species composition lists for new thresholds. 
###This will not repeat work that has already been done, and will only 
###look at new threshold values.

##This script will create functions for the aggregation of different threshold and will then execute them

#Set the list of thresholds for which lists will be compiled.

library(raster)

rarefaction=c("0.01_do-over_locs_rem","0.01_do-over_no_m","0.1_do-over_locs_rem","0.1_do-over_no_m")

#0.01 conforms to 99% threshold, 0.1 to 90% threshold.

sp.extract=function(rarefaction){
  filepath="//home//"
  
  setwd(paste(filepath,"//Extracts",sep=""))
  coord.files=list.files(pattern="*.csv")
  
  #Create the raster stack of all species
  
  for(k in 1:length(rarefaction)){
    x=rarefaction[k]
    setwd(paste0(filepath,"//results//reprojected//",x))
    z=list.files(pattern="*.asc")
    Q=stack(z)
    ##Extracting the species list.
    
    for(i in 1:length(coord.files)){
      setwd(paste(filepath,"//Extracts",sep=""))
      y=read.csv(coord.files[i])
      loc=strsplit(as.character(coord.files[i])," _coordinates")[[1]][1]
      #print(paste("reading",loc,sep=" "))
      for(j in 1:nrow(y)){
        y1=y[j,2:3]
        ext=extract(x=Q,y=y1)
        if(j==1){
          df=ext
        }else{
          df=rbind(df,ext)
        }
        #print(paste(j,"of",nrow(y),"complete",sep=" "))
      }
      
      dist=strsplit(as.character(coord.files[i]),"coordinates_ ")[[1]][2]
      dist=strsplit(as.character(dist)," _")[[1]][1]
      #df=cbind(loc,df)
      newd=paste0(filepath,"//Extracts//RESULTS//",x)
      dir.create(newd)
      setwd(newd)
      #print(paste("writing",loc,sep=" "))
      write.csv(df,file=paste0(newd,"//",loc,"_splist_",dist,"-km.csv"),row.names=F)
    }
  }
}

sp.extract(rarefaction=rarefaction)

############################################################################################

#Regression analysis of final Completeness numbers

library(devtools)
library(ggplot2)
library(extrafont)
#font_import("Times")

filepath="//home//kupeornis//Documents//Manuscripts_outside-work//GEB-Stacking-Models"
setwd(filepath)
x=read.csv("finaltally2.csv")

###########################################################################

#First, we will compare completeness indices of all scenarios

#No M
plot(y=x$Total.all,x=x$no.99,asp=1,pch=19,col='black',ylim=c(0,250),xlim=c(0,250))
points(y=x$Total.all,x=x$no.95,asp=1,pch=19,col='grey',ylim=c(0,250),xlim=c(0,250))
points(y=x$Total.all,x=x$no.90,asp=1,pch=19,col='violet',ylim=c(0,250),xlim=c(0,250))

#With M
points(y=x$Total.all,x=x$m.99,asp=1,pch=19,col='blue',ylim=c(0,250),xlim=c(0,250))
points(y=x$Total.all,x=x$m.95,asp=1,pch=19,col='red',ylim=c(0,250),xlim=c(0,250))
points(y=x$Total.all,x=x$m.90,asp=1,pch=19,col='green',ylim=c(0,250),xlim=c(0,250))
abline(b=1,a=0)

##Now lets look only at M
plot(y=x$Total.all,x=x$m.99,asp=1,pch=19,col='blue',ylim=c(0,100),xlim=c(0,100))
points(y=x$Total.all,x=x$m.95,asp=1,pch=19,col='red')
points(y=x$Total.all,x=x$m.90,asp=1,pch=19,col='green')
abline(b=1,a=0)

###########################################################################

CI=function(w){
  a=mean(w)
  b=sd(w)
  c=nrow(x)
  e9=qnorm(0.975)*b/sqrt(c)
  print(paste("Mean ",a," +/- ",e9,sep=""))
}

#Let's look at average values

w=x$C.95.m
CI(w)
w=x$C.95.no
CI(w)

y=colMeans(x[,-c(1:12)])
x2=x[,-c(1:12)]
mean(x2[,6]/x2[,4])

###########################################################################

##Test and see if these lines are statistically different from a line Y=X+0

#Are the models accurate at predicting species richness:
#with all?
#Make sure derived=X, as that is what is known at the outset

#Linear regression of M with 95%

cor(x$m.95,x$Total.all)
l.1=lm(x$m.95~x$Total.all+0)
summary(l.1)
coefficients(l.1)
confint(l.1,level=0.95)
e1=confint(l.1,level=0.95)[1]
e2=confint(l.1,level=0.95)[2]
e3=(e1+e2)/2
e3-e1

#Linear regression without M with 95%

cor(x$no.95,x$Total.all)
nl.1=lm(x$no.95~x$Total.all+0)
summary(nl.1)
coefficients(nl.1)
confint(nl.1,level=0.95)
ne1=confint(nl.1,level=0.95)[1]
ne2=confint(nl.1,level=0.95)[2]
ne3=(ne1+ne2)/2
ne3-ne1

plotset1=(cbind("M-constrained",x$m.95,x$Total.all))
plotset2=(cbind("Unconstrained",x$no.95,x$Total.all))

plotset1=as.data.frame(plotset1)
plotset2=as.data.frame(plotset2)
colnames(plotset1)=colnames(plotset2)=c("Scenario","Predicted","Known")

plotset=rbind(plotset1,plotset2)
plotset$Known=as.numeric(as.character(plotset$Known))
plotset$Predicted=as.numeric(as.character(plotset$Predicted))

p=ggplot(plotset,aes(x=Known,y=Predicted,color=Scenario))
q=geom_point(size=3)

r1=geom_abline(intercept=0,slope=e3,colour="red",size=1)
r2=geom_abline(intercept=0,slope=e1,colour="red")
r3=geom_abline(intercept=0,slope=e2,colour="red")
r4=geom_abline(intercept=0,slope=1,colour="black",size=1)

nr1=geom_abline(intercept=0,slope=ne3,colour="blue",size=1)
nr2=geom_abline(intercept=0,slope=ne1,colour="blue")
nr3=geom_abline(intercept=0,slope=ne2,colour="blue")

s=xlab("Number of Observed Species")
u=ylab("Number of Predicted Species")
v=ylim(0,240)
w=xlim(0,80)

w5=geom_point(size=3)
w6=theme_classic(base_family="Times")
w7=theme(text=element_text(size=25),
         axis.text.x=element_text(size=18),
         axis.text.y=element_text(size=18),
         axis.title.x=element_text(size=25),
         axis.title.y=element_text(size=25))

x25=p+r1+r2+r3+r4+nr1+nr2+nr3+s+u+v+w+q+w5+w6+w7

print(x25)

#ggsave(filename=paste0(filepath,"//all_loc_rich_regress.png"), 
       plot=x25,scale=1,width=8,height=8,dpi=500,limitsize=TRUE)

##########################################################################################

#Sorenson and Prediction Success Calculations
#Jacob C. Cooper & Jorge Soberon, 2017

#Total number of species modeled:
alln=293

filepath="//home//kupeornis//Documents//Manuscripts_outside-work//GEB-Stacking-Models//Locality-PAM"

setwd(filepath)
files=list.files(pattern="*.csv")

for(i in 1:length(files)){
  x=files[i]
  csv=read.csv(x)
  n=nrow(csv)
  csv2=csv[-n,]
  
  #Determine true presence from checklist
  actually.present.2013=which(csv2[,3]=="X")
  actually.present.2015=which(csv2[,4]=="X")
  actually.present=c(actually.present.2013,actually.present.2015)
  
  #Predicted Present, no M
  predicted.no.m=which(csv2[,6]=="X")
  
  #Predicted with M
  predicted.m=which(csv2[,9]=="X")
  
  #Stats for no M
  #True Presences
  n.tp=length(which(actually.present%in%predicted.no.m))
  #True Negatives
  n.tn=alln-length(unique(c(actually.present,predicted.no.m)))
  #False Negative
  n.fn=length(actually.present)-n.tp
  #False Presences
  n.fp=length(predicted.no.m)-n.tp
  
  #Stats for with M
  #True Presences
  m.tp=length(which(actually.present%in%predicted.m))
  #True Negatives
  m.tn=alln-length(unique(c(actually.present,predicted.m)))
  #False Negative
  m.fn=length(actually.present)-m.tp
  #False Presences
  m.fp=length(predicted.m)-m.tp
  
  #Create a string to add to a matrix to save
  newrow=cbind(x,n.tp,n.fp,n.tn,n.fn,m.tp,m.fp,m.tn,m.fn)
  colnames(newrow)[1]="Locality"
  if(i==1){
    xarray=as.data.frame(newrow)
  }else{
    xarray=rbind(xarray,newrow)
  }
}

for(i in 2:ncol(xarray)){
  xarray[,i]=as.numeric(as.character(xarray[,i]))
}

################################################################################################

#Calculate Sorensen and Chi-Square metrics

pred.success=function(tp,tn,sp){
  return((tp+tn)/sp)
}

Sorensen=function(tp,fn,fp){
  return((2*tp)/(2*tp+fn+fp))
}

for(i in 1:nrow(xarray)){
  #Calculate metrics for no M
  if(i==1){
    x2=pred.success(tp=xarray$n.tp[i],tn=xarray$n.tn[i],sp=alln)
    y2=Sorensen(tp=xarray$n.tp[i],fn=xarray$n.fn[i],fp=xarray$n.fp[i])
    
    xx2=pred.success(tp=xarray$m.tp[i],tn=xarray$m.tn[i],sp=alln)
    yy2=Sorensen(tp=xarray$m.tp[i],fn=xarray$m.fn[i],fp=xarray$m.fp[i])
  }else{
    x=pred.success(tp=xarray$n.tp[i],tn=xarray$n.tn[i],sp=alln)
    y=Sorensen(tp=xarray$n.tp[i],fn=xarray$n.fn[i],fp=xarray$n.fp[i])
    x2=c(x2,x)
    y2=c(y2,y)
    
    xx=pred.success(tp=xarray$m.tp[i],tn=xarray$m.tn[i],sp=alln)
    yy=Sorensen(tp=xarray$m.tp[i],fn=xarray$m.fn[i],fp=xarray$m.fp[i])
    xx2=c(xx2,xx)
    yy2=c(yy2,yy)
  }
}

Prediction.Success.NO=x2
Sorensen.NO=y2
Prediction.Success.M=xx2
Sorensen.M=yy2

xarray2=as.data.frame(cbind(xarray,Prediction.Success.NO,Sorensen.NO,Prediction.Success.M,Sorensen.M))

#write.csv(xarray2,paste0("//home//kupeornis//Documents//Manuscripts_outside-work//GEB-Stacking-Models//PAM.csv"),row.names=F,quote=F)

################################################################################################

#Create "Long Form" Array for Plotting and Testing

no=xarray2[,c(1,10,11)]
yes=xarray2[,c(1,12,13)]

x1=as.data.frame(cbind("Unconstrained",no))
x2=as.data.frame(cbind("Constrained",yes))

colnames(x1)=colnames(x2)=c("Scenario","Locality","Pred.Success","Sorensen")

X=rbind(x1,x2)

################################################################################################
#Wilcoxon Tests of Signifant Difference in Metrics

#Prediction Success

p.1=wilcox.test(Pred.Success~Scenario,data=X)
p.1
#Sorensen Index

s.1=wilcox.test(Sorensen~Scenario,data=X)
s.1

#Present data with Confidence Intervals

CI=function(w){
  a=mean(w)
  b=sd(w)
  c=nrow(xarray2)
  e9=qnorm(0.975)*b/sqrt(c)
  print(paste("Mean ",a," +/- ",e9,sep=""))
}

w=xarray2$Prediction.Success.NO
CI(w)
w=xarray2$Prediction.Success.M
CI(w)
w=xarray2$Sorensen.NO
CI(w)
w=xarray2$Sorensen.M
CI(w)

################################################################################################

#Create Figure

X1=X[,1:3]
X2=X[,-3]
X1=cbind("Prediction Success",X1)
X2=cbind("Sorensen Index",X2)
colnames(X1)=colnames(X2)=c("Test","Scenario","Locality","Value")

XX=rbind(X1,X2)

p=ggplot(XX,aes(y=Value,x=Test,fill=Scenario))
q=geom_boxplot()
w6=theme_classic(base_family="Times")
w7=theme(text=element_text(size=30),
         axis.text.x=element_text(size=25),
         axis.text.y=element_text(size=25),
         axis.title.x=element_text(size=30),
         axis.title.y=element_text(size=30))

x25=p+q+w6+w7

print(x25)

#ggsave(filename="//home//kupeornis//Documents//Manuscripts_outside-work//GEB-Stacking-Models//boxplot.png", 
       plot=x25,scale=1,width=10,height=10,dpi=500,limitsize=TRUE)

#The box shows the interquartile range, and outliers are those points which are
#more than 1.5 boxlengths from the box. The heave line is the median.
