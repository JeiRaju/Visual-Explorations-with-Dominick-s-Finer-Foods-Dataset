# R code written by Belinda Chiera
# The purpose is to demonstrate some of the graphs that can be produced in R
# to visualise the Dominick data

# ######################################################################################
# ########################## load the required libraries ###########################
# ######################################################################################


library(ggplot2)  
library(beeswarm)
library(quantmod)
library(reshape2)
library(plyr)
library(scales)
library(streamgraph)
library(viridis)
library(zoo)


# ######################################################################################
# ################################### read the data ####################################
# ######################################################################################

# Normally I wouldn't read in all the data files to begin with but
# for this demo we're doing this to make sure everything is set up in case
# you want to cut & edit bits of code :)

# customer count
ccount <- read.csv("ccount.csv",header=T)
head(ccount)
ccount <- na.omit(ccount)


# demographics
demo <- read.csv("demo.csv",header=T)
head(demo)
demo <- na.omit(demo)


# This next set of data files are the result of aggregating the data to massage the original
# data into providing more information.  This might provide inspiration :)

# Beer_agg.csv is the result of aggregating across the beer data

beern <- read.csv("Beer_agg.csv",header=T,strip.white=TRUE)
head(beern)
beern <- na.omit(beern)

# STORE_ZONE_SUM_PT.csv is the result of aggregating across the data to capture
# store information across zones and price tiers.  This data needed some extra cleaning, 
# hence the extra lines of code below

SZSP <- read.csv("STORE_ZONE_SUM_PT.csv",header=T,strip.white=TRUE)
head(SZSP)
SZSP <- SZSP[,-1]
SZSP <- na.omit(SZSP)
SZSP$ZONE <- factor(SZSP$ZONE)
# check the data - there's a weird entry that is blank
table(SZSP$Price_Tier)

# Clean the blank entry
SZSP <- SZSP[SZSP$Price_Tier != "",]
SZSP$Price_Tier <- factor(SZSP$Price_Tier)

# check the data again - it should be fine now :)
table(SZSP$Price_Tier)   # and it is!

# Now do the same for this summary of beer data
df <- read.csv("Beer_MPPPTB.csv",header=T,strip.white=TRUE)
df <- df[,-1]
table(df$Price_Tier)
df <- df[df$Price_Tier != "",]
df$Price_Tier <- factor(df$Price_Tier)
table(df$Price_Tier)


# And do the same one more time for this file :)
# CHE_Summary.csv is the result of aggregating across the cheese data
ch <- read.csv("CHE_Summary.csv",header=T,strip.white=TRUE,stringsAsFactors=F)
head(ch)
ch <- ch[,c(-1,-2,-22)]
ch <- na.omit(ch)
table(ch$Price_Tier)
ch <- ch[ch$Price_Tier != "",]
ch$Price_Tier <- factor(ch$Price_Tier)
table(ch$Price_Tier)


# We also did some specific beer brand analysis
bm <- read.csv("Bud-vs-Miller-dates-GOOD.csv",header=TRUE)
head(bm)

# ######################################################################################
# ################################### Dotplots #########################################
# ######################################################################################

# Dotplot of cheese data - isolate a week of interest
idx <- which(ch$"WEEK"==9)
length(idx) 
dplot_df <- ch[idx,c(1,8)]

# cheese stores
STORES <- c(5,9,12,14,32,40,49,52,56,62,70,71,78,80,81,84,86,93,97,100,102,104,113,115,116, 118, 119, 124, 126, 130)
dx <- match(STORES,dplot_df$STORE)

idx <- which(dplot_df$Profit_Count!=0)
dplot_df <- dplot_df[idx,]

#isolate a store of interest
dplot_df$fact <- ifelse(dplot_df$STORE==94,'Y','N')
head(dplot_df)

# Plot Total Profit/Store in Week 9 and highlight store 94
# you will need to re-size the graph to see the labels :)
ggplot(dplot_df,aes(x=factor(STORE),fill=factor(fact),y=Profit_Count)) + geom_dotplot(binaxis = "y", stackdir = "center") + xlab("Store") + ylab("Total Profit ($USD)") + theme(legend.position="none",axis.title=element_text(size=14),
axis.text=element_text(size=12)) + scale_fill_manual(values=c("#8595E1","#E07B91")) + theme(text = element_text(size=20),axis.text.x = element_text(angle=90, hjust=1)) 


# ######################################################################################
# ########################## Boxplots (Original, Notched, Violin) ######################
# ######################################################################################

# Boxplot of Sum 
ggplot(SZSP, aes(x=ZONE, y=Sum, fill=ZONE)) + geom_boxplot() + theme(legend.position="none") + ylab("Sales Volume")

# Boxplot Notched (False/True)
ggplot(SZSP, aes(x=ZONE, y=Sum, fill=ZONE)) + geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE) + theme(axis.title=element_text(size=14),axis.text=element_text(size=12),legend.position="none") + ylab("Beer Sales (Volume)\n") + xlab("\nZone")

# Violin Plot
ggplot(SZSP, aes(x=ZONE, y=Sum, fill=ZONE)) + geom_violin() + theme(text = element_text(size=14),legend.position="none") + ylab("Beer Sales (Volume)\n") + xlab("\nZone")


# Boxplots by group
ggplot(aes(y = Sum, x = BRAND, fill = Price_Tier), data = bm) + geom_boxplot() + theme(panel.background = element_rect(fill ="white"), panel.grid.major = element_line(colour = "#BFC1C9"),axis.title=element_text(size=14),axis.text=element_text(size=12)) 	+ ylab("Number of units sold per week\n") + xlab("\nBrand") + scale_fill_manual(values=c("#ffffff","#cce8f8", "#000000","#E69F00"))


# Boxplot Sales by Zone, filled by Price Tier
ggplot(SZSP, aes(ZONE, Sum, fill = factor(Price_Tier))) + scale_y_continuous(breaks=seq(0,3500,by=500)) + theme(axis.text.x = element_text(size=14,hjust = 1,vjust = 1)) + xlab("Zone") + ylab("Beer Sales (Volume)") + guides(fill=FALSE) + geom_boxplot() + facet_grid(.~Price_Tier, scales="free")


# Boxplot Sales by Store, filled by Price Tier
ggplot(beern, aes(factor(STORE), Sum, fill = factor(Price_Tier))) + scale_y_continuous(breaks=seq(0,3500,by=500)) + theme(axis.text.x = element_text(size  = 6,angle = 90,hjust = 1,vjust = 1)) + xlab("Store") + ylab("Beer Sales (Volume)") + guides(fill=FALSE) + geom_boxplot() + facet_grid(.~Price_Tier, scales="free")



# ######################################################################################
# ############################# Rainfall Plot ##########################################
# ######################################################################################


ggplot(beern, aes(factor(STORE), Sum)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Store") + ylab("Beer Sales (Volume)\n")  + geom_point(color='#149EF3') + theme(text = element_text(size=14),legend.position="none")


ggplot(SZSP, aes(ZONE, Sum)) + theme(axis.text.x = element_text(hjust = 1)) + xlab("Zone") + ylab("Beer Sales (Volume)\n")  + geom_point(color='#DD3497') + theme(text = element_text(size=14),legend.position="none")


# ######################################################################################
# ############################# Bubble & Swarm Plots ###########################################
# ######################################################################################

# ########## Bubble Plots

df1 <- df
df2 <- as.data.frame(sapply(df,gsub,pattern="High",replacement="H"))
df2 <- as.data.frame(sapply(df2,gsub,pattern="Low",replacement="L"))
df2 <- as.data.frame(sapply(df2,gsub,pattern="Medium",replacement="M"))
df2 <- as.data.frame(sapply(df2,gsub,pattern="CubFighter",replacement="C"))

df1$Price_Tier <- df2$Price_Tier

# Profit by zone 
radius <- sqrt(df1$MOVE/ pi )
symbols(df1$PRICE, df1$PROFIT, circles=radius, inches=0.35, fg="white", bg="#E7298A", xlab="Price", ylab="Profit")
text(df1$PRICE, df1$PROFIT, df1$Price_Tier, cex=0.5)


cbbPalette <- c("#000000", "#56B4E9", "#D55E00",  "#CC79A7")
pt <- as.factor(df$Price_Tier)
ggplot(df,aes(x=df$PRICE, y=df$PROFIT, group=pt,shape=pt,size=radius,colour=df$Price_Tier)) + scale_shape_manual(values=c(15,16,17,18)) + scale_colour_manual(values=cbbPalette) + theme(panel.background = element_rect(fill ="white"), panel.grid.major = element_line(colour = "#BFC1C9")) + geom_point( alpha=0.8) +  guides(colour = FALSE, shape = guide_legend(order = 1)) + labs(shape = "Price Tier",size="Scale") + scale_x_continuous("Beer Price (volume)") + scale_y_continuous("Beer Profit (volume)")


ggplot(df, aes(df$PRICE, y=df$PROFIT, colour=df$Price_Tier, size=radius)) + geom_point( alpha=0.8) + scale_colour_brewer(palette="Spectral", type="qual",name="Price Tier") + guides(colour = guide_legend(override.aes = list(alpha = 1))) + scale_x_continuous("Cheese Price") + scale_y_continuous("Cheese Profit (gross margin)")

# ########## Swarm Plots

df1$col <- as.numeric(df1$Price_Tier)

# set colour palette
df1$col[df1$col ==1] = "#32AAB5"
df1$col[df1$col ==2] = "#60C4BC"
df1$col[df1$col ==3] = "#8CD9C0"
df1$col[df1$col ==4] = "#B3E7C5"
beeswarm(PRICE ~ Price_Tier, data = df1, method = 'hex', corral = "gutter", pch = 16, pwcol = col, xlab = '', ylab = 'Price',cex=1.5,cex.lab=1.8)



# ######################################################################################
# ########################### Calendar Heat Map ########################################
# ######################################################################################

# save the data as we're manipulating it a bit
safe <- ch

ch$Date <- as.Date(factor(ch$Date),format = "%d/%m/%Y")

# We will facet by year ~ month, and each subgraph will
# show week-of-month versus weekday
# the year is simple
ch$year<-as.numeric(as.POSIXlt(ch$Date,format = "%d/%m/%y")$year+1900)

# the month too 
ch$month<-as.numeric(as.POSIXlt(ch$Date,format = "%d/%m/%y")$mon+1)


# but turn months into ordered facors to control the appearance/ordering in the presentation
ch$monthf<-factor(ch$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)


# the day of week is again easily found
ch$weekday = as.POSIXlt(ch$Date,format = "%d/%m/%y")$wday


# again turn into factors to control appearance/abbreviation and ordering
# I use the reverse function rev here to order the week top down in the graph
# you can cut it out to reverse week order
ch$weekdayf<-factor(ch$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)


# the monthweek part is a bit trickier 
# first a factor which cuts the data into month chunks
ch$yearmonth<-as.yearmon(ch$Date)
ch$yearmonthf<-factor(ch$yearmonth)

# then find the "week of year" for each day
ch$week <- as.numeric(format(ch$Date,"%W"))

# and now for each monthblock we normalize the week to start at 1 
ch<-ddply(ch,.(yearmonthf),transform,monthweek=1+week-min(week))

# Prepare colour palette
library("colorspace") 
pal <-choose_palette()   # I chose the sequential, multiple hues, blue/green palette

colnames(ch)[8] <- "Profit"  # Makes legend nicer :)
# Now for the plot
ggplot(ch, aes(monthweek, weekdayf, fill = Profit)) + geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradientn(colours = rev(pal(10))) + theme(panel.background = element_rect(fill ="white"), panel.grid.major = element_line(colour = "#BFC1C9"),axis.title=element_text(size=14),axis.text=element_text(size=10))+ xlab("Week of Month") + ylab("") 

# restore the original data
ch <- safe

# ######################################################################################
# ############################## 100% Stacked Bar ######################################
# ######################################################################################


# cheese data 
# Count by Price Tier
p <- ggplot(aes(x=WEEK, weight=Count, fill=Price_Tier), data=ch)
p + geom_bar(position='fill') + scale_y_continuous(labels = percent) + scale_fill_brewer(palette="PuOr") + labs(x="X Label", y="Y Label", title="An Example Stacked Column Percentage Chart")

# Average Price by Price Tier
p <- ggplot(aes(x=WEEK, weight=Price_Mean, fill=Price_Tier), data=ch)
p + geom_bar(position='fill') + scale_y_continuous(labels = percent_format()) + scale_fill_brewer(palette="Spectral") + theme(axis.title=element_text(size=14),axis.text=element_text(size=10)) + labs(x="Weeks of the Year", y="Average Beer Price ($USD)")