#Script for save some data frames in csv format
#Data simplification for a quicker web.

library(openxlsx) #Neccesary for opening xlsx file
library(dplyr) #Neccesary for recoding variables. 
library(crosstalk)##Neccesary for activity
library(leaflet)
library(flexdashboard)
#The database is a complex Excel file. 
#These commands will open the file and convert in a only data frame. 
filename1 <-"data/IBERLID.xlsx"
sheets <- openxlsx::getSheetNames(filename1)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=filename1)
names(SheetList) <- sheets

ssheetlist=SheetList[-1] #Command to discard the firt sheet of Excel file, without analytical data. 

database=do.call(rbind.data.frame, ssheetlist)
row.names(database)=database$code
#New 2 values for a new variable (sample.type1) are created: Archaeological and Geological.
database$sample.type1 = database$sample.type %>%
    recode("Mineral" = "Geological",
           "WR" = "Geological",
           "Archaeological artefacts" = "Archaeological",
           "Archaeological mineral" = "Archaeological")
  
  archae= filter(database, database$sample.type1=="Archaeological") #New dataframe. 
  geol= filter(database, database$sample.type1=="Geological") #New dataframe.


geol.simp<-geol[c(1, 10,28,12,13,15,17,19,21,38,39, 40,41,42,7,8)]
geol.simp$p206.204 <- with(geol.simp,round(geol.simp$p206.204,3))
geol.simp$p207.204 <- with(geol.simp,round(geol.simp$p207.204,3))
geol.simp$p208.204 <- with(geol.simp,round(geol.simp$p208.204,3))
geol.simp$p207.206 <- with(geol.simp,round(geol.simp$p207.206,3))
geol.simp$p208.206 <- with(geol.simp,round(geol.simp$p208.206,3))
geol.simp$decimallatitude <- with(geol.simp,round(geol.simp$decimallatitude,6))
geol.simp$decimallongitude <- with(geol.simp,round(geol.simp$decimallongitude,6))
geol.simp$tmod <- with(geol.simp,round(geol.simp$tmod,0))
geol.simp$mu12 <- with(geol.simp,round(geol.simp$mu12,2))

write.csv(geol.simp,"iberlid_2/data/geol.simp.csv", row.names = FALSE)


archae.simp<-archae[c(1, 6,10,36,13,15,17,19,21,38,39, 40, 41,42,7,8)]
archae.simp$p206.204 <- with(archae.simp,round(archae.simp$p206.204,3))
archae.simp$p207.204 <- with(archae.simp,round(archae.simp$p207.204,3))
archae.simp$p208.204 <- with(archae.simp,round(archae.simp$p208.204,3))
archae.simp$p207.206 <- with(archae.simp,round(archae.simp$p207.206,3))
archae.simp$p208.206 <- with(archae.simp,round(archae.simp$p208.206,3))
archae.simp$decimallatitude <- with(archae.simp,round(archae.simp$decimallatitude,6))
archae.simp$decimallongitude <- with(archae.simp,round(archae.simp$decimallongitude,6))
archae.simp$tmod <- with(archae.simp,round(archae.simp$tmod,0))
archae.simp$mu12 <- with(archae.simp,round(archae.simp$mu12,2))


write.csv(archae.simp,"iberlid_2/data/archae.simp.csv", row.names = FALSE)

write.csv(database,"data/wholedatabase.csv")
write.csv(archae,"data/archaeological.samples.csv")
write.csv(geol,"data/geological.samples.csv")


#Whole database after filtering:
databasef=subset(database, laboratory!="U. Oviedo")
databasef=subset(databasef, code !="GDM.2042" & code !="GDM.0031" & code !="GDM.0057" & code !="GDM.0079"& code !="GDM.0020"& code !="GDM.2710"
                 & code !="GDM.2045"& code !="GDM.0015" &reference!="ENADIMSA, 1971" 
                 & reference!="Arias et al., 1996" & ref.code != "Ref.117" & 
                   code !="GDM.0023" & code !="GDM.2292" & code !="GDM.0477")

databasef$geol.zone = databasef$geological.zone %>%
  recode("BC" = "Betic Cordillera", "BCB" = "Basque Cantabrian Basin", "CCR" = "Catalonian Coastal Ranges",
         "NIM" = "Northern branch of the Iberian Massif", "OMZ" = "Ossa Morena Zone", "Py" = "Pyrenees", "SPZ" = "South Portuguese Zone")

databasef$sample.type1 = databasef$sample.type %>%
  recode("Mineral" = "Geological",
         "WR" = "Geological",
         "Archaeological artefacts" = "Archaeological",
         "Archaeological mineral" = "Archaeological")

write.csv(databasef,"data/wholedatabase_filtered.csv")


  
databasef.simp<-databasef[c(1, 10,28,12,13,15,17,19,21,38,39, 40,41,42,7,8, 46, 47)]
   databasef.simp$p206.204 <- with( databasef.simp,round( databasef.simp$p206.204,3))
   databasef.simp$p207.204 <- with( databasef.simp,round( databasef.simp$p207.204,3))
   databasef.simp$p208.204 <- with( databasef.simp,round( databasef.simp$p208.204,3))
   databasef.simp$p207.206 <- with( databasef.simp,round( databasef.simp$p207.206,3))
   databasef.simp$p208.206 <- with( databasef.simp,round( databasef.simp$p208.206,3))
   databasef.simp$decimallatitude <- with( databasef.simp,round( databasef.simp$decimallatitude,6))
   databasef.simp$decimallongitude <- with( databasef.simp,round( databasef.simp$decimallongitude,6))
   databasef.simp$tmod <- with( databasef.simp,round( databasef.simp$tmod,0))
   databasef.simp$mu12 <- with( databasef.simp,round( databasef.simp$mu12,2))

  write.csv( databasef.simp,"iberlid_2/data/databasef.simp.csv", row.names = FALSE)

