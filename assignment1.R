setwd("C:/Fish/classes/summer_2015/Analytic_edge/assignments/assignment1")

mvt<-read.csv("mvtWeek1.csv")

str(mvt)

#How many rows of data (observations) are in this dataset?
           #191641 obs. of  11 variables

summary(mvt)  
           #Max.:9181151


# Using the "max" function, what is the maximum value of the variable "ID"?

max(mvt$ID)  # 9181151
min(mvt$Beat)  # 111

# How many observations have value TRUE in the Arrest 
# variable (this is the number of crimes for which an arrest was made)?

table(mvt$Arrest) #15536

sum(mvt$LocationDescription=="ALLEY")  # 2308



mvt$Date[1]  #12/31/12 23:15  Month/Day/Year Hour:Minute

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

summary(DateConvert)

summary(DateConvert)

# What is the month and year of the median date 
# in our dataset? Enter your answer as "Month Year",
# without the quotes. (Ex: if the answer was 2008-03-28, 
# you would give the answer "March 2008", without the quotes.)

#May 2006

#Now, let's extract the month and the day of the week, and add these variables to our data frame mvt. We can do this with two simple functions. Type the following commands in R:

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Month)

table(mvt$Month)   #February: 13511 

table(mvt$Weekday) # Friday 29284

# Each observation in the dataset represents a motor 
# vehicle theft, and the Arrest variable indicates 
# whether an arrest was later made for this theft.
# Which month has the largest number of motor vehicle 
# thefts for which an arrest was made?

table(mvt$Arrest,mvt$Month)
# January: 1435

jpeg('crimetrend.jpg')
hist(mvt$Date, breaks=100)
dev.off()

boxplot(mvt$Date~mvt$Arrest)

# For what proportion of motor 
# vehicle thefts in 2001 was an arrest made?

table(subset(mvt$Arrest,mvt$Year==2001))

# FALSE  TRUE 
# 18517  2152
# 2152/(2152+18517)=0.1041173

table(subset(mvt$Arrest,mvt$Year==2007))
# FALSE  TRUE 
# 13068  1212
# 1212/(1212+13068)=0.08487395

sort(table(mvt$LocationDescription),decreasing=TRUE)[1:5]

indices<-mvt$LocationDescription %in% c("STREET",
        "GAS STATION","PARKING LOT/GARAGE(NON.RESID.)",
        "ALLEY","DRIVEWAY - RESIDENTIAL")

Top5<-subset(mvt,indices==TRUE )

str(Top5) #177510

Top5$LocationDescription = factor(Top5$LocationDescription)

table(Top5$LocationDescription)

# On which day of the week do the most motor vehicle thefts
# at gas stations happen?

gas<-Top5[Top5$LocationDescription=='GAS STATION',]
table(gas$Weekday)


# On which day of the week do the fewest motor 
# vehicle thefts in residential driveways happen?

res<-Top5[Top5$LocationDescription=='DRIVEWAY - RESIDENTIAL',]
table(res$Weekday)


######################################
#########################################

    IBM<-read.csv("IBMStock.csv")
    GE<-read.csv("GEStock.csv")
    ProcterGamble<-read.csv("ProcterGambleStock.csv")
    CocaCola<-read.csv("CocaColaStock.csv")
    Boeing<-read.csv("BoeingStock.csv")

    IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
    
    
    
    GE$Date = as.Date(GE$Date, "%m/%d/%y")
    
    str(IBM)
    
    CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
    
    ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
    
    Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

    
    plot(CocaCola$Date,CocaCola$StockPrice,type="l",col='red')
    
    lines(ProcterGamble$Date, ProcterGamble$StockPrice,col='blue',lty=2)
    
    abline(v=as.Date(c("1980-06-01")))
    
    plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
    lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="blue")
    lines(GE$Date[301:432], GE$StockPrice[301:432], col="black")
    lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="darkblue")
    lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="purple")
    abline(v=as.Date(c("1997-11-01")))
    abline(v=as.Date(c("1997-9-01")))
    
    tapply(IBM$StockPrice, months(IBM$Date), mean)
    
    tapply(GE$StockPrice, months(GE$Date), mean)
    
    tapply(Boeing$StockPrice, months(Boeing$Date), mean)
    

    tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
    
    tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
    
    
    ##################
    
    CPS<-read.csv("CPSData.csv")
    
    sort(table(CPS$Industry),decreasing = TRUE)
    sort(table(CPS$State),decreasing = TRUE)
    
    table(CPS$Race,CPS$Hispanic)
    
    table(CPS$Region, is.na(CPS$Married))
    table(CPS$State, is.na(CPS$MetroAreaCode))
    
    MetroAreaMap<-read.csv("MetroAreaCodes.csv")
    
    CountryMap<-read.csv("CountryCodes.csv")
    
    sort(tapply((CPS$Education == "No high school diploma" & !is.na(CPS$Education)), CPS$MetroArea, mean))
    
    CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
    
    CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
    
    
    table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")
    