#code to clean UN-Methodology.csv
unm49=read.csv("unmerged_unraw_csv/UNSD - Methodology.csv")
sum(unm49$Least.Developed.Countries..LDC.!="")
#create binary variables for small economies
unm49$leastDeveloped=(1*(unm49$Least.Developed.Countries..LDC.!=""))
unm49$landLockedDeveloping=(1*(unm49$Land.Locked.Developing.Countries..LLDC.!=""))
unm49$smallIsDeveloping=(1*(unm49$Small.Island.Developing.States..SIDS.!=""))
unm49$Developing=(1*(unm49$Developed...Developing.Countries=="Developing"))
#drop original 3 columns
unm49=subset(unm49,select= -Least.Developed.Countries..LDC.)
unm49=subset(unm49,select= -Land.Locked.Developing.Countries..LLDC.)
unm49=subset(unm49,select= -Small.Island.Developing.States..SIDS.)
unm49=subset(unm49,select= -Developed...Developing.Countries)
#write in back
write.csv(unm49,"C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/unmerged_unraw_csv/unm49.csv", row.names = FALSE)
#remove all
rm(list=ls())
#Put ISO code into OECD tables
total_aid=read.csv("unmerged_unraw_csv/OECD_total_aid_1995_2019.csv",
                                fileEncoding="UTF-8-BOM")
iso=subset(read.csv("unmerged_unraw_csv/unm49.csv"),
           select=c(countries,iso3))
#right outer join
#https://www.datasciencemadesimple.com/join-in-r-merge-in-r/
right=merge(x=iso,y=total_aid,by="countries",all.y=TRUE)
#find countries that do not have iso code
diff=setdiff(total_aid$countries, iso$countries)
#exclude total and regional
#https://stackoverflow.com/a/49089598
#Remove first N countries if it's iso code is either blanked or filled
#https://stackoverflow.com/a/37770986
diff[!grepl("(Total|regional|unspecified|income)$", diff)][-(1:15)]
sum(right$countries=="Wallis and Futuna")
right[which(right$countries=="Moldova"),]$iso3=rep("MDA",23)
right[which(right$countries=="Côte d'Ivoire"),]$iso3=rep("CIV",25)
right[which(right$countries=="Tanzania"),]$iso3=rep("TZA",25)
right[which(right$countries=="Bolivia"),]$iso3=rep("BOL",25)
right[which(right$countries=="Venezuela"),]$iso3=rep("VEN",25)
right[which(right$countries=="China (People's Republic of)"),]$iso3=rep("CHN",25)
right[which(right$countries=="Hong Kong (China)"),]$iso3=rep("HKG",2)
right[which(right$countries=="Korea"),]$iso3=rep("KOR",5)
right[which(right$countries=="Macau (China)"),]$iso3=rep("MAC",5)
right[which(right$countries=="Iran"),]$iso3=rep("IRN",25)
right[which(right$countries=="Micronesia"),]$iso3=rep("FSM",25)
right[which(right$countries=="Wallis and Futuna"),]$iso3=rep("FSM",25)
write.csv(right,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/OECD_total_aid.csv", 
          row.names = FALSE)
#after finishing with oecd total aid, move on to oecd net loan
oecdLoan=read.csv("unmerged_unraw_csv/OECD_net_loan_1995_2019.csv",
                   fileEncoding="UTF-8-BOM")
rm(total_aid,diff)
oecdISO=unique(subset(right,select=c(countries,iso3)))
#OECD-generated data should be similar, so let's run a set difference
setdiff(oecdLoan$countries,oecdICO$countries)
oecdLoanRight=merge(x=oecdISO,y=oecdLoan,by="countries",all.y=TRUE)
write.csv(oecdLoanRight,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/OECD_net_loan.csv", 
          row.names = FALSE)
#after finishing with oecd net loan, move on to oecd grant
oecdGrant=read.csv("unmerged_unraw_csv/OECD_grant_1985_2019.csv",
                    fileEncoding="UTF-8-BOM")
#OECD-generated data should be similar, so let's run a set difference
setdiff(oecdGrant$countries,oecdISO$countries)
oecdGrantRight=merge(x=oecdISO,y=oecdGrant,by="countries",all.y=TRUE)
write.csv(oecdGrantRight,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/OECD_grant.csv", 
          row.names = FALSE)
#ico is now attached into all 3 oecd csv, so they can all be removed
rm(oecdLoan,oecdGrant,iso,right)
#remove rows with null iso since inner join factors in NA
rm(aidLoan)
oecdLoanRight=subset(oecdLoanRight,(!is.na(oecdLoanRight$iso3)))
sum(is.na(oecdLoanRight$iso3))
oecdTotalAidRight=subset(oecdTotalAidRight,(!is.na(oecdTotalAidRight$iso3)))
sum(is.na(oecdTotalAidRight$iso3))
oecdGrantRight=subset(oecdGrantRight,(!is.na(oecdGrantRight$iso3)))
sum(is.na(oecdGrantRight$iso3))
#write them back
write.csv(oecdTotalAidRight,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/OECD_total_aid.csv", 
          row.names = FALSE)
write.csv(oecdLoanRight,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/OECD_net_loan.csv", 
          row.names = FALSE)
write.csv(oecdGrantRight,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/OECD_grant.csv", 
          row.names = FALSE)
#now you can first full outer join total aid and net loan
aidLoan=merge(x=oecdTotalAidRight,y=oecdLoanRight,
      by=c("iso3","year"),all=TRUE)
#then you drop the countries.y first
aidLoan=subset(aidLoan, select=-countries.y)
#change column name of countries.x to countries
#https://stackoverflow.com/a/6081514
colnames(aidLoan)[3]="countries"
#now full outer join aidLoan with grant
fullOECD=merge(x=aidLoan,y=oecdGrantRight,
               by=c("iso3","year"),all=TRUE)
#then you drop the countries.y first
fullOECD=subset(fullOECD, select=-countries.y)
#change column name of countries.x to countries
#https://stackoverflow.com/a/6081514
colnames(fullOECD)[3]="countries"
#finally write it out
write.csv(fullOECD,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/OECD.csv", 
          row.names = FALSE)
#the OECD is a successful merger and acquisition
#To conclude, one should attach ISO using M49, that's it
unm49=read.csv("unmerged_unraw_csv/UNSD - Methodology.csv")
imf=read.csv("unmerged_unraw_csv/IMF_tax_to_gdp_1995_2019.csv",
             fileEncoding="UTF-8-BOM")
diff=setdiff(imf$countries, unm49$countries)
#first right join imf with existing iso first
nameCode=subset(unm49,select=c(iso3,countries))
imfRight=merge(x=nameCode,y=imf,by="countries",all.y=TRUE)
imfRight=subset(imfRight,select= -X)
#now, take on the set difference and add manually
imfRight[which(imfRight$countries==name),]$iso3=
  rep("YEM",sum(imfRight$countries==name))
index=1+index
name = diff[index]
rm(name,index,diff)
#get the rows with iso code
imfRightISO=subset(imfRight,(!is.na(imfRight$iso3)))
#write it back
write.csv(imfRightISO,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/IMF_tax_gdp.csv", 
          row.names = FALSE)
#now i am on world bank
bank=read.csv("unmerged_unraw_csv/World_Bank_ALL_1995_2019.csv",
                   fileEncoding="UTF-8-BOM")
unm49=read.csv("unmerged_unraw_csv/UNSD - Methodology.csv")
nameCode=subset(unm49,select=c(countries,iso3))
colnames(nameCode)=c("countries","countries_code")
rightBank=merge(x=nameCode,y=bank,by="countries_code",all.y=TRUE)
rightBank=subset(rightBank,!is.na(rightBank$countries.x))
setdiff=subset(rightBank,rightBank$countries.x!=rightBank$countries.y)
setdiff=subset(setdiff,select=c(countries_code,countries.x,countries.y))
setdiff=unique(setdiff)
setdiff[index,]
index=index+1
#the country code is right already in world bank data
#therefore, the last step will be cleansing those rows
bank[bank$countries=="",]
bank=subset(bank,bank$countries!="")
bank[bank$countries=="Data from database: World Development Indicators",]
bank=subset(bank,bank$countries!="Data from database: World Development Indicators")
#filled the '..' with NA instead
#https://stackoverflow.com/a/3357784
for(s in 5:9)
  bank[bank[,s]=="..",][,s]=rep(NA,NROW(bank[bank[,s]=="..",][,s]))
#write world bank data back
write.csv(bank,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/World_Bank.csv", 
          row.names = FALSE)
rm(list=ls())
#now move on to instrumental variables
ceppi=read.csv("unmerged_unraw_csv/dist_cepii.csv",fileEncoding="UTF-8-BOM")
bigdac=c("AUS", "CAN", "FRA", "DEU", "ITA", "JPN", "KOR", "NLD", "ESP", "GBR",
         "USA")
#get distance with only the 11 countries
#prove this table is just a cartesian product
lefthkg=ceppi[ceppi$iso_o=="HKG",]
righthkg=ceppi[ceppi$iso_d=="HKG",]
#colony link should be the same, we need colony link, don't care common
lefthkg=lefthkg[order(lefthkg$iso_d),]
righthkg=righthkg[order(righthkg$iso_o),]
#vector comparison
#https://stackoverflow.com/a/10374972
all(lefthkg$colony==righthkg$colony)
#Verdict: in terms of colonial link, this table is certainly cartesian
#how about dist?
all(lefthkg$dist==righthkg$dist)
for(s in 11:14)
  print(all(lefthkg[,s]==righthkg[,s]))
colnames(lefthkg)
#Verdict: in terms of dist and distcap, this table is certainly cartesian
#Verdict: in terms of distw and distwces, no, they aren't cartesian
#Since I need only colony link and dist, it should be fine 
distance=read.csv("unmerged_unraw_csv/dist_cepii.csv",fileEncoding="UTF-8-BOM")
#mutliple conditions for subsets
#https://stackoverflow.com/a/6244267
distance=subset(distance, (iso_d %in% bigdac))
#get only columns that matter: dist and colony link
distance=subset(distance,select=c(iso_o, iso_d, dist, colony))
#order by 2 columns: first countries then dac countries
#https://chartio.com/resources/tutorials/how-to-sort-a-data-frame-by-multiple-columns-in-r/
distance=distance[with(distance, order(iso_o, iso_d)),]
colnames(distance)=c("countries","dac","dist","colony")
#write this back
write.csv(distance,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/CEPII_dist_col.csv", 
          row.names = FALSE)
bigdac=c("AUS", "CAN", "FRA", "DEU", "ITA", "JPN", "KOR", "NLD", "ESP", "GBR",
         "USA")
#Get religion
religion=read.csv("unmerged_unraw_csv/main-religion-of-the-country-in.csv",fileEncoding="UTF-8-BOM")
#first get code and name
unm49=read.csv("unmerged_unraw_csv/UNSD - Methodology.csv")
nameCode=subset(unm49,select=c(iso3,countries))
rm(unm49)
#then get set difference of religion and not in unm
colnames(religion)=c("countries","iso3","year","name")
rightreligion=merge(x=nameCode,y=religion,by="iso3",all.y=TRUE)
#multiple conditions in subset
#https://stackoverflow.com/a/4935551
setdiff=subset(rightreligion,
               (rightreligion$countries.x!=rightreligion$countries.y) |
                 is.na(rightreligion$countries.x))
listToRemove=subset(rightreligion,is.na(rightreligion$countries.x))$countries.y
religion=subset(religion,!(religion$countries %in% listToRemove))
#done, now write back and remove
rm(rightreligion,setdiff,listToRemove)
write.csv(religion,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/OWID_religion.csv", 
          row.names = FALSE)
#now make sure the 11 dac countries do have religions
NROW(setdiff(bigdac,religion$iso3))
#Just run a cartesian product of non dac and dac countries
religion=subset(religion,select=c(iso3,countries,name))
dacReligion=subset(religion,religion$iso3 %in% bigdac)
dacReligionC=subset(religion,!(religion$iso3 %in% bigdac))
fullReligion=merge(x = dacReligion, y = dacReligionC, by = NULL)
fullReligion$same=1*(fullReligion$name.x==fullReligion$name.y)
#need to check whether same is true or not
distinct=unique(subset(fullReligion,select=c(name.x,name.y,same)))
#Since the distinct shows the same parity between religion pair and same
#I now declare the cartesian product is good to go
rm(dacReligion, dacReligionC, distinct)
#now write it back, 2 columns only
writeReligion=subset(fullReligion,select=c(iso3.x,iso3.y,same))
colnames(writeReligion)=c("dac","dacc","same")
write.csv(writeReligion,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/OWID_religion_same.csv", 
          row.names = FALSE)
rm(fullReligion,religion,writeReligion)
bigdac=c("AUS", "CAN", "FRA", "DEU", "ITA", "JPN", "NLD", "ESP", "GBR",
         "USA")
#read the aid outflow of big 11 in 1984
outflow=read.csv("unmerged_unraw_csv/OECD_net_oda.csv",
                           fileEncoding="UTF-8-BOM")
outflow=subset(outflow,select=c(LOCATION,Value))
colnames(outflow)=c("iso3","aid")
setdiff(bigdac,outflow$iso3)
outflow=subset(outflow,outflow$iso3!="DAC")
#now you get the flow, it's time to join the religion table to try
religion=read.csv("C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/OWID_religion_same.csv")
#remove KOR from all tables since KOR has no data in oecd dac in 1984
religion=subset(religion,religion$dac!="KOR")
write.csv(religion,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/OWID_religion.csv", 
          row.names = FALSE)
View(religion)
#write down outflow as record for 10 dac countries
write.csv(outflow,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/OECD_outflow.csv", 
          row.names = FALSE)
colnames(outflow)=c("dac", "aid")
newReligion=merge(x=religion,y=outflow,by="dac")
View(newReligion)
newReligion=newReligion[order(newReligion$dacc),]
newReligion=subset(newReligion,select=-dac)
newReligion$cross=newReligion$same*newReligion$aid
newReligion=subset(newReligion,select=c(dacc,aid))
#group by then aggregate, sql in R
#https://stackoverflow.com/a/34523783
newReligion=aggregate(cross~dacc,newReligion,sum)
#write down the aid_religion csv
write.csv(newReligion,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/Aid_religion.csv", 
          row.names = FALSE)
rm(bigdac,religion,newReligion)
#read the cepii distance and colony data
cepii=read.csv("C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/CEPII_dist_col.csv")
View(cepii)
#remove korean
cepii=subset(cepii,cepii$dac!="KOR")
#write it back
write.csv(cepii,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/CEPII_dist_col.csv", 
          row.names = FALSE)
#inner join with outflow
colnames(outflow); colnames(cepii)
newCepii=merge(x=cepii,y=outflow,by="dac")
View(newCepii)
newCepii=newCepii[order(newCepii$countries),]
#ditch dac for grouping
newCepii=subset(newCepii,select=-dac)
#create 2 columns: aid-dist and aid-col
newCepii$aid_dist=newCepii$aid/newCepii$dist
newCepii$aid_col=newCepii$aid*newCepii$colony
#keep only the cross products and iso
colnames(newCepii)
newCepii=subset(newCepii,select=c(countries,aid_dist,aid_col))
#group by country
newCepii=aggregate(.~countries,newCepii,sum)
#write it back
write.csv(newCepii,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/Aid_dist_col.csv", 
          row.names = FALSE)
#compile corruption
rm(list=ls())
cpi95=read.csv("unmerged_unraw_csv/CPI-Archive-1995.csv",
              fileEncoding="UTF-8-BOM")
cpi96=read.csv("unmerged_unraw_csv/CPI-Archive-1996.csv",
               fileEncoding="UTF-8-BOM")
colnames(cpi95);colnames(cpi96)
joined=merge(x=cpi95,y=cpi96,by="CustomerId")
cpi95=subset(cpi95,select=c(iso,score)); cpi96=subset(cpi96,select=c(iso,score))
cpi9596=merge(x=cpi95,y=cpi96,
                      by="iso",all=TRUE)
colnames(cpi9596)=c("iso", "X1995","X1996")
View(cpi9596)
cpi97=read.csv("unmerged_unraw_csv/CPI-Archive-1997.csv",
               fileEncoding="UTF-8-BOM")
colnames(cpi9596); colnames(cpi97)
cpi97=subset(cpi97,select=c(iso,score))
cpi95_97=merge(x=cpi9596,y=cpi97,by="iso",all=TRUE)
View(cpi95_97)
colnames(cpi95_97)=c("iso","X1995","X1996","X1997")
#done with 95,96,97, now remove irrelevanty stuff
rm(cpi9596,cpi97)
#now go to 1998~2015
cpi98_15=read.csv("unmerged_unraw_csv/CPI_1998_2015.csv",
                   fileEncoding="UTF-8-BOM")
View(cpi98_15)
#attach iso to cpi98_15
colnames(cpi98_15)
colnames(nameCode)=c("iso","Jurisdiction")
colnames(nameCode)
cpi98_15=merge(x=nameCode,y=cpi98_15,by="Jurisdiction",all.y=TRUE)
View(subset(cpi98_15,is.na(cpi98_15$iso)))
#loop through the list of null iso and add them manually
cpi98_15[is.na(cpi98_15$iso),][2,]$Jurisdiction
cpi98_15[is.na(cpi98_15$iso),][2,]$iso="COG"
cpi98_15[cpi98_15$Jurisdiction=="Bosnia & Herzegovina",]
cpi98_15[cpi98_15$Jurisdiction=="Bosnia and Herzgegovina",]
rm(list=ls())
#faulty data source, abort

#get the list of oda recipients in 2021
unm49=read.csv("isoed/unm49.csv")
unm49=unm49[unm49$oda==1,]
colnames(unm49)
unm49=subset(unm49,select=iso3)
#get oecd data and join
oecd=read.csv("isoed/OECD.csv")
colnames(oecd)
oecd=subset(oecd,select=-countries)
colnames(oecd)
full=oecd
sum(is.na(full$total_aid)); sum(is.na(full$loan)); sum(is.na(full$grant))
NROW(unique(subset(full,is.na(full$loan),select=iso3)))
rm(oecd)
#now read imf 
imf=read.csv("isoed/IMF_tax_gdp.csv")

NROW(unique(imf$iso3))

colnames(imf)
imf=subset(imf,select=-countries)
colnames(imf)
full=merge(x=full,y=imf,by=c("iso3","year"),all=TRUE)
#now read world bank
rm(imf)
bank=read.csv("isoed/World_Bank.csv")
colnames(bank)
bank=subset(bank,select=-countries)
colnames(bank)
colnames(bank)=c("iso3", colnames(bank)[-1])
colnames(bank)
full=merge(x=full,y=bank,by=c("iso3","year"),all=TRUE)
rm(bank)
#inner join with countries in oda 2021 dac list
colnames(unm49); colnames(full)
full=merge(x=full,y=unm49,by="iso3")
#time to omit
#https://stackoverflow.com/a/4862264
omit=na.omit(full)
#divide oecd data by gdp
colnames(full)
full$total_aid_gdp=full$total_aid/full$nomina_gdp
full=subset(full,select=-total_aid)
colnames(full)
full$loan_gdp=full$loan/full$nomina_gdp
sum(is.na(full$loan_gdp))
full=subset(full,select=-loan)
colnames(full)
full$grant_gdp=full$grant/full$nomina_gdp
sum(is.na(full$grant_gdp))
full=subset(full,select=-grant)
#since nominal gdp is in USD instead of million USD
#multiply each column with 10^6
full$total_aid_gdp=full$total_aid_gdp*(10^6)
full$loan_gdp=full$loan_gdp*(10^6)
full$grant_gdp=full$grant_gdp*(10^6)
View(full)
max(subset(full$loan_gdp,!is.na(full$loan_gdp)))
#turn total_aid, loan, grant into percentage
full$total_aid_gdp=full$total_aid_gdp*100
full$loan_gdp=full$loan_gdp*100
full$grant_gdp=full$grant_gdp*100
summary(lm(tax_to_gdp~total_aid_gdp+real_gdp_capita,data=full,
           na.action=na.omit))
#OLS does not yield good results, so let's try IV
#read dist_col
dist_col=read.csv("isoed/Aid_dist_col.csv")
colnames(dist_col)=c("iso3", "aid_dist", "aid_col")
religion=read.csv("isoed/Aid_religion.csv")
colnames(religion)
colnames(religion)=c("iso3","cross")
#inner join with full
full=merge(x=full,y=dist_col,by="iso3")
full=merge(x=full,y=religion,by="iso3")
View(full)

#recompile all data and secure a good buildling block
#In order to ensure security and continuing stability,
#the Republic will be reorganized into the first Galactic Empire, 
#for a safe and secure society
#drop countries column
grant=read.csv("isoed/OECD/OECD_grant.csv")
grant=subset(grant,select=-countries)
write.csv(grant,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/building_block/OECD/OECD_grant.csv",
          row.names=FALSE)
#done with grant, now with loan
loan=read.csv("isoed/OECD/OECD_net_loan.csv")
loan=subset(loan,select=-countries)
write.csv(loan,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/building_block/OECD/OECD_net_loan.csv",
          row.names=FALSE)
#done with loan, now with total aid
aid=read.csv("isoed/OECD/OECD_total_aid.csv")
aid=subset(aid,select=-countries)
write.csv(aid,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/building_block/OECD/OECD_total_aid.csv",
          row.names=FALSE)
#done with total aid, now with tax to gdp
tax=read.csv("unmerged_unraw_csv/OECD/OECD_tax_to_gdp.csv",
             fileEncoding="UTF-8-BOM")
tax=subset(tax,select=-countries)
write.csv(tax,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/building_block/OECD/OECD_tax_to_gdp.csv",
          row.names=FALSE)
#done with oecd, now with world bank
worldbank=read.csv("isoed/World_Bank/World_Bank.csv")
worldbank=subset(worldbank,select=-countries)
colnames(worldbank)[1]="iso3"
write.csv(worldbank,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/building_block/World_Bank.csv",
          row.names=FALSE)
#done with world bank, now go to religionreligion=subset(religion,select=-dac)
table(aggregate(.~dacc,religion,sum)$same)
#really that similar?
rm(religion)
religion=read.csv("unmerged_unraw_csv/IV/main-religion-of-the-country-in.csv")
nameList=oda$dac
nameList=as.data.frame(nameList)
colnames(nameList)[1]="Code"
rightJoin=merge(x=religion,y=nameList,by="Code")
rightJoin=subset(rightJoin,select=c(Code,Main.religion))
religion=subset(religion,select=c(Code,Main.religion))
fullReligion=merge(x=religion,y=rightJoin,by=NULL)
fullReligion=(fullReligion[order(fullReligion$Code.x),])
fullReligion=subset(fullReligion,!fullReligion$Code.x=="")
fullReligion=subset(fullReligion,select=-Code.y)
fullReligion$same = 1*(fullReligion$Main.religion.x==fullReligion$Main.religion.y)
aggre=subset(fullReligion,select=c(Code.x,same))
aggre=aggregate(.~Code.x,aggre,sum)
table(aggre$same)
#yes
rm(list=ls())
religion=read.csv("IV/OWID_religion.csv")
oda=read.csv("IV/OECD_outflow.csv")
colnames(oda)[1]="dac"
colnames(oda)
religion=merge(x=religion,y=oda,by="dac")
religion=religion[order(religion$dacc),]
religion$religion.aid=religion$same*religion$aid
sum(religion$same==0)==sum(religion$religion.aid==0)
religion=subset(religion,select=c(dacc,religion.aid))
religion=aggregate(.~dacc,religion,sum)
write.csv(religion,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/building_block/IV/aid_religion.csv",
          row.names=FALSE)
#done with religion, now with colony and distance
rm(religion)
coldist=read.csv("isoed/IV/CEPII_dist_col.csv")
colnames(coldist)[1]="iso3"
colnames(coldist)
coldist=merge(x=coldist,y=oda,by="dac")
#test aggregate
aggre=subset(coldist,select=c(iso3,colony))
aggre=aggregate(.~iso3,aggre,sum)
table(aggre$colony)
#continue the join
coldist=(coldist[order(coldist$iso3),])
coldist$col.aid=coldist$colony*coldist$aid
coldist$dist.aid=coldist$aid/coldist$dist
#aggregate iso3, col.aid, dist.aid
aggre=subset(coldist,select=c(iso3,col.aid,dist.aid))
aggre=aggregate(.~iso3,aggre,sum)
table(aggre$col.aid)
write.csv(aggre,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/building_block/IV/aid_col_dist.csv",
          row.names=FALSE)
#done with all building block, full merge now
rm(list=ls())
