##########################################################################################
# Interactive Reading Data #
##########################################################################################

mydata<- read.csv(file.choose(),header=T,na.strings=c(""))

###########################################################################################
# Automated Checking and Installing Packages if not Present #
###########################################################################################

if("smbinning" %in% rownames(installed.packages()) == FALSE) 
{
  install.packages("smbinning")
  library(smbinning)
}
require("smbinning")

if("xda" %in% rownames(installed.packages()) == FALSE) 
{
  library(devtools)
  install_github("ujjwalkarn/xda")
  library(xda)
}
require("xda")

###########################################################################################
# Use of xda #
###########################################################################################

str(mydata)
mydata$subs_id = as.factor(mydata$subs_id)
mydata$TESTCONTROL = as.factor(mydata$TESTCONTROL)
mydata$TC_flag = as.factor(mydata$TC_flag)
mydata$Upgraded = as.factor(mydata$Upgraded)
mydata$Upgraded = as.numeric(as.character(mydata$Upgraded))

numSummary(mydata)
charSummary(mydata)
Plot(mydata,'Upgraded')

###########################################################################################
# Use of Smbinning #
###########################################################################################

result=smbinning(df=mydata,y="Upgraded",x="Avg_PSD",p=0.10)
# result$ivtable
smbinning.sql(result)

# Relevant plots (2x2 Page)
par(mfrow=c(2,2))
boxplot(mydata$Avg_PSD ~ mydata$Upgraded,
        horizontal = T, frame = T,main="Distribution")
mtext("Avg_PSD",3)
smbinning.plot(result,option="dist",sub="Avg_PSD")
smbinning.plot(result,option="badrate",sub="Avg_PSD")
smbinning.plot(result,option="WoE",sub="Avg_PSD")


##########################################################################################
# Creating new variables based on smbinning #
##########################################################################################

library(sqldf)
mydata_1 <- sqldf("select a.*,
                          case when DOLA_days <= 0 then '01: DOLA_days <= 0' 
                               when DOLA_days <= 10 then '02: DOLA_days 0 - 10' 
                               when DOLA_days > 10 then '03: DOLA_days > 10' 
                               when DOLA_days Is Null then 'DOLA_days Is Null' 
                               else '99: Error' end as DOLA_DAYS_BAND,

                          case when TENURE <= 68 then '01: TENURE <= 68'
                               when TENURE <= 140 then '02: TENURE 68 - 140' 
                               when TENURE > 140 then '03: TENURE > 140' 
                               when TENURE Is Null then 'TENURE Is Null' 
                               else '99: Error' end as TENURE_BAND,

                          case when Avg_Bill <= 45 then '01: Avg_Bill <= 45' 
                               when Avg_Bill <= 131.6 then '02: Avg_Bill 45 - 131.6' 
                               when Avg_Bill <= 201.8 then '03: Avg_Bill 131.6 - 201.8' 
                               when Avg_Bill <= 350.8 then '04: Avg_Bill 201.8 - 350.8' 
                               when Avg_Bill <= 739.3 then '05: Avg_Bill 350.8 - 739.3' 
                               when Avg_Bill <= 1349.8 then '06: Avg_Bill 739.3 - 1349.8' 
                               when Avg_Bill > 1349.8 then '07: Avg_Bill > 1349.8' 
                               when Avg_Bill Is Null then 'Avg_Bill Is Null' 
                               else '99: Error' end as AVG_BILL_BAND,

                          case when Avg_OOB <= 0.4 then '01: Avg_OOB <= 0.4' 
                               when Avg_OOB > 0.4 then '02: Avg_OOB > 0.4' 
                               when Avg_OOB Is Null then 'Avg_OOB Is Null' 
                               else '99: Error' end as AVG_OOB_BAND,

                          case when Avg_MOU <= 2.1 then '01: Avg_MOU <= 2.1' 
                               when Avg_MOU <= 87.7 then '02: Avg_MOU 2.1 - 87.7' 
                               when Avg_MOU <= 336.3 then '03: Avg_MOU 87.7 - 336.3' 
                               when Avg_MOU <= 733 then '04: Avg_MOU 336.3 - 733' 
                               when Avg_MOU > 733 then '05: Avg_MOU > 733' 
                               when Avg_MOU Is Null then 'Avg_MOU Is Null' 
                               else '99: Error' end as AVG_MOU_BAND,

                          case when Avg_PSD <= 4.1 then '01: Avg_PSD <= 4.1' 
                               when Avg_PSD > 4.1 then '02: Avg_PSD > 4.1' 
                               when Avg_PSD Is Null then 'Avg_PSD Is Null' 
                               else '99: Error' end as AVG_PSD_BAND

                          from mydata a")