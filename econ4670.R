#code to clean UN-Methodology.csv
unm49=read.csv("unmerged_unraw_csv/UNSD - Methodology.csv",fileEncoding="UTF-8-BOM")
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
for(s in 5:9)
  bank[bank[,s]=="..",][,s]=rep(NA,NROW(bank[bank[,s]=="..",][,s]))
#write world bank data back
write.csv(bank,
          "C:/Users/andes/Documents/HKUST/Academic/2021 Fall/ECON4274/ECON4670/isoed/World_Bank.csv", 
          row.names = FALSE)
