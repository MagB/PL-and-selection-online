#This is the script for obtaining the results for Bartkowska and Johnston 2015 in JEB
#This script is organized into two parts. 
#Part 1 is the analysis conducted for the informal meta-anlaysis.
#Part 2 is the second part of the paper based on field collected data of Lobelia cardinalis

######################################################################################
######################################################################################
#PART 1
######################################################################################
######################################################################################

#load the data of the literature survey
data_for_pub=read.csv("part1_data_for_pub.csv")

str(data_for_pub)


#Table S1 is a summary list of the publications included in the literature survey

#Table S2 is a summary of the relationship between selection strength and pollen limitation for five trait types estimated from 
#a survey of all studies reporting selection and pollen limitation published prior to January 2014.

###########################################
#TABLE S2
###########################################

#The following section of code will recreate table S2

length(unique(data_for_pub$Species))
length(unique(data_for_pub$Author.s..of.Selection.study))

#We plot the absolute value of the selection differential and gradients against the estimate of pollen limitation.
data_for_pub$Diff.linear.abs=abs(data_for_pub$Diff.linear)
data_for_pub$Grad.linear.abs=abs(data_for_pub$Grad.linear)

#The analysis is partitioned according to trait type. We categorized traits into one of 5 trait types
unique(data_for_pub$Trait.type)

# The function diff_out calculates the linear regression for the relationship between the selection differentials and pollen limitation for a given trait type
diff_out=function(x){
        regress_diff<-lm(data_for_pub$Diff.linear.abs[data_for_pub$Trait.type==x]~data_for_pub$Best.PL[data_for_pub$Trait.type==x], data=data_for_pub)
        plot(data_for_pub$Best.PL,data_for_pub$Diff.linear.abs)
        
        return(summary(regress_diff))
}

diff_out("floral size")
diff_out("plant size")
diff_out("flower number")
diff_out("phenology")
diff_out("vegetative tissue area")
x="floral size"

# The function diff_out calculates the linear regression for the relationship between the selection gradients and pollen limitation for a given trait type

grad_out=function(x){
        regress_grad<-lm(data_for_pub$Grad.linear.abs[data_for_pub$Trait.type==x]~data_for_pub$Best.PL[data_for_pub$Trait.type==x])
        plot(data_for_pub$Best.PL,data_for_pub$Grad.linear.abs)
        abline(regress_grad)
        return(summary(regress_grad))
}

grad_out("floral size")
grad_out("plant size")
grad_out("flower number")
grad_out("phenology")
grad_out("vegetative tissue area")




######################################################################################
######################################################################################
#PART 2
######################################################################################
######################################################################################


mydata=read.csv("part2_all_years_data_for_pub.csv", sep=";")

summary(mydata)
str(mydata)

#Variable in data with description
#Year: the data is organized according to year collected: 2009, 2010 or 2011
#Pop: is either s1 for naturally pollinated plants or s2 for hand_last-pollinated plants
#ID: is the label given to each plant 
#total_seed: is total seed production for the plant
#total_fruit: is total fruits produced
#seed_flr: is the average number of seeds produced per flower
#seed_fruit: is the average number of seeds produced per fruit
#fruit_flr: is the average number of fruits produced per flower
#mid_wid: is the width of the bottom 3 petals
#and: is the anther-nectary distance
#len_last: is the lenth to the last flower
#flr_num: is the total number of flowers produced by the plant
#avg_display: is the average number of flowers open per day 
#med_flr_date: is the day the middle flower was first observed in female phase. The dates are in SAS date format.


#################################
#PRODUCE TABLES S3 to S5. 
#These are the tables of means and comparison of means between the natural and hand-pollinated plants.
#################################

#enter the Year you would like to explore :
YR="2009"

#partition the data based on year
year=mydata[mydata$Year==YR, ]

#Table S3 presents the means and standard deviations and the bootstrapped difference in means for each trait
set.seed=2
#R allows the user to define the number of bootstrap iterations desired
R=10000

mean_diffs(year, "seed_fruit", R)

mean_diffs <- function(data, trait, R){
        output=data.frame()
        if(!(is.data.frame(data))){stop("invalid dataset name")} 

        trait_column=c(which(colnames(data) %in% trait))
        trait_pop=""
        trait_pop=c(2,trait_column)
        data_for_boot=data[trait_pop]
        #print(data_for_boot)
        data_for_boot=na.omit( data_for_boot)
        set.seed=2
        #R allows the user to define the number of bootstrap iterations desired
        R=10000
        b5=numeric(R)

        for(i in 1:R){
                #this samples, with replacement, assumes all observations are drawn from the same population
                data_for_boot$Pop<-sample( data_for_boot$Pop, length( data_for_boot$Pop));
                pop_s2= data_for_boot[data_for_boot$Pop=="s2",][2]
                pop_s1= data_for_boot[data_for_boot$Pop=="s1",][2]

                b5[i]<-mean(pop_s2[[1]])-mean(pop_s1[[1]])
        }

        
        pop_s2=data[data$Pop=="s2",][trait_column]
        pop_s1=data[data$Pop=="s1",][trait_column]
        print(pop_s2)
        
        natural=mean(pop_s1[[1]], na.rm=T)
        hand_pollinated=mean(pop_s2[[1]], na.rm=T)
        diff=abs(natural-hand_pollinated)
        pval=mean(abs(b5)>diff)
        
        output=cbind( natural, hand_pollinated,diff,pval)
        print(output)
}
        
##########################################
#Table S6
#Comparison of pollen limitation across three years in a population of L. cardinalis. 
#For each year and between year comparison, 95% confidence intervals and P-values were 
#determined using 10,000 bootstrap iterations.
#########################################



year=mydata[mydata$Year=='2009', ]
year2=mydata[mydata$Year=='2010', ]
year_2011=mydata[mydata$Year=='2011', ]

year_s1=year[year$Pop=='s1',]
year_s2=year[year$Pop=='s2',]

year2_s1=year2[year2$Pop=='s1',]
year2_s2=year2[year2$Pop=='s2',]

year_2011_s1=year_2011[year_2011$Pop=='s1',]
year_2011_s2=year_2011[year_2011$Pop=='s2',]

#this gets rid of any rows with missing values of flower number.
year_s1=year_s1[complete.cases(year_s1$flr_num), ]
year_s2=year_s2[complete.cases(year_s2$flr_num), ]

year2_s1=year2_s1[complete.cases(year2_s1$flr_num), ]
year2_s2=year2_s2[complete.cases(year2_s2$flr_num), ]

year_2011_s1=year_2011_s1[complete.cases(year_2011_s1$flr_num), ]
year_2011_s2=year_2011_s2[complete.cases(year_2011_s2$flr_num), ]

year_2011_s1=year_2011_s1[which(year_2011_s1$flr_num!=0), ]
year_2011_s2=year_2011_s2[which(year_2011_s2$flr_num!=0), ]

#find raw estimate of PL year 1

year_mean_s1_raw=mean(year_s1$total_seed)
year_mean_s2_raw=mean(year_s2$total_seed)
year_PL_raw_seed<-1-(year_mean_s1_raw/year_mean_s2_raw)



#find raw estimate of PL 2010

year2_mean_s1_raw=mean(year2_s1$total_seed)
year2_mean_s2_raw=mean(year2_s2$total_seed)
year2_PL_raw_seed<-1-(year2_mean_s1_raw/year2_mean_s2_raw)



#find raw estimate of PL year 2011

year_2011_mean_s1_raw=mean(year_2011_s1$total_seed)
year_2011_mean_s2_raw=mean(year_2011_s2$total_seed)
year_2011_PL_raw_seed<-1-(year_2011_mean_s1_raw/year_2011_mean_s2_raw)

results<-data.frame()

PL_2009_ci=data.frame()
PL_2010_ci=data.frame()
PL_2011_ci=data.frame()
PL_2009_2010=data.frame()
PL_2009_2011=data.frame()
PL_2010_2011=data.frame()
set.seed(2)

#this section of code creates the bootstrapped datasets for the hand and naturally pollinated groups for each of the three years
for(i in c(1:10000)){
        #rlen_lastomly sampled with replacement observations for the naturally pollinated group in 2009
        mysample <- year_s1[sample(1:nrow(year_s1), nrow(year_s1), replace=TRUE),]
        mean_s1=mean(mysample$total_seed)
        
        #rlen_lastomly sampled with replacement observations for pop s2  (the hlen_last-pollinated group) in 2009
        mysample2 <- year_s2[sample(1:nrow(year_s2), nrow(year_s2), replace=TRUE),]
        mean_s2=mean(mysample2$total_seed)
        
        #this find the PL estimate for 2009 
        PL_2009<-1-(mean_s1/mean_s2)
        
        #rlen_lastomly sampled observations for naturally pollinated group in 2010
        mysample <- year2_s1[sample(1:nrow(year2_s1), nrow(year2_s1), replace=TRUE),]
        mean_s1=mean(mysample$total_seed)
        
        #rlen_lastomly sampled observations for hlen_last pollinated group in 2010
        mysample2 <- year2_s2[sample(1:nrow(year2_s2), nrow(year2_s2), replace=TRUE),]
        mean_s2=mean(mysample2$total_seed)
        
        #PL estimate for 2010
        PL_2010<-1-(mean_s1/mean_s2)
        
        #rlen_lastomly sampled observations for naturally pollinated group in 2011
        mysample <- year_2011_s1[sample(1:nrow(year_2011_s1), nrow(year_2011_s1), replace=TRUE),]
        mean_s1=mean(mysample$total_seed)
        
        #rlen_lastomly sampled observations for hlen_last pollinated group in 2011
        mysample2 <- year_2011_s2[sample(1:nrow(year_2011_s2), nrow(year_2011_s2), replace=TRUE),]
        mean_s2=mean(mysample2$total_seed)
        
        #PL estimate for 2011
        PL_2011<-1-(mean_s1/mean_s2)
        
        
        #for each run, the PL estimates are appended to a dataset for each year.
        PL_2009_ci=rbind(PL_2009_ci, PL_2009)
        PL_2010_ci=rbind(PL_2010_ci, PL_2010)
        PL_2011_ci=rbind(PL_2011_ci, PL_2011)
        
        #for each run, the difference in PL estimates between years is stored.
        PL_2009_2010=rbind(PL_2009_2010, PL_2009-PL_2010)
        PL_2009_2011=rbind(PL_2009_2011, PL_2009-PL_2011)
        PL_2010_2011=rbind(PL_2010_2011, PL_2010-PL_2011)
}

#the estimates for each dataset created in the preceding loop are named
names(PL_2009_ci)[1]="PL_2009_ci"
names(PL_2010_ci)[1]="PL_2010_ci"
names(PL_2011_ci)[1]="PL_2011_ci"

names(PL_2009_2010)[1]="PL_2009_2010"
names(PL_2009_2011)[1]="PL_2009_2011"
names(PL_2010_2011)[1]="PL_2010_2011"

#now the 95% CI len_last P values for each PL estimate len_last comparison among years is obtained

#CI and pvalue of PL estimates for 2009
quantile(PL_2009_ci$PL_2009_ci, c(0.025, 0.975))
((length(PL_2010_ci[(PL_2009_ci)[1]<=0])/length(PL_2009_ci))/10000)


#CI and pvalue of PL estimates for 2010
quantile(PL_2010_ci$PL_2010_ci, c(0.025, 0.975))
((length(PL_2010_ci[(PL_2010_ci)[1]<=0])/length(PL_2010_ci))/10000)

#CI and pvalue of PL estimates for 2011
quantile(PL_2011_ci$PL_2011_ci, c(0.025, 0.975))
((length(PL_2011_ci$PL_2011_ci[PL_2011_ci$PL_2011_ci<=0])/length(PL_2011_ci))/10000)

#CI and pvalue of PL estimates for difference of PL between 2009 and 2010
quantile(PL_2009_2010$PL_2009_2010, c(0.025, 0.975))
(1-(length(PL_2009_2010[(PL_2009_2010)[1]<=0])/length(PL_2009_2010))/10000)

#CI and pvalue of PL estimates for difference of PL between 2009 and 2011
quantile(PL_2009_2011$PL_2009_2011, c(0.025, 0.975))
(1-(length(PL_2009_2011[(PL_2009_2011)[1]<=0])/length(PL_2009_2011))/10000)

#CI and pvalue of PL estimates for difference of PL between 2010 and 2011
quantile(PL_2010_2011$PL_2010_2011, c(0.025, 0.975))
((length(PL_2010_2011$PL_2010_2011[PL_2010_2011$PL_2010_2011<=0])/length(PL_2010_2011))/10000)






#################################################################################
#Tables S7 to S12. Total selection and direct selection in 2009, 2010 and 2011
#################################################################################

#I split the data into the two treatments s1 for naturally pollinated plants len_last s2 for hlen_last-pollinated plants
#recall that the data is paritioned above by year as follows:

year=mydata[mydata$Year=='2009', ]
#year2=mydata[mydata$Year=='2010', ]
#year_2011=mydata[mydata$Year=='2011', ]
head(subdata)

subdata=year[year$Pop=='s1', ]
#or
subdata2=year[year$Pop=='s2', ]


########Selection differentials:

#this is the function that will calculate the selection differentials. You can choose the traits and data you would like estimates for in the 
#by calling this function with the dataset name and list of traits.
raw_regression_univariate <- function(data,trait=c("and","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date"),y="total_seed" ){

        if(!(is.data.frame(data))){stop("invalid dataset name")}  
        output_file=data.frame(traits_list=as.character(), intercept=numeric(0), estimate=numeric(0),r=numeric(0), p=numeric(0), stringsAsFactors=FALSE)
        colnames(output_file)=c("trait", "intercept","estimate", "R2", "pval")
        
        trait_columns=c(which(colnames(data) %in% trait))
        y_var_pos=which(colnames(data) %in% y)
        
        for (i in trait_columns) {
                
                myvars=c(y_var_pos, i)
                subdata=data[,myvars]
                subdata=subdata[complete.cases(subdata), ]

                #this stlen_lastardizes the traits of interest
                subdata$stlen_lastardized_trait=(subdata[,2]-mean(subdata[,2], na.rm=TRUE))/sd(subdata[,2], na.rm=TRUE)   
                #this relatives fitness
                subdata$rel_fitness=subdata[,1]/mean(subdata[,1])
                coefs2<-lm(subdata$rel_fitness~   subdata$stlen_lastardized_trait , data=subdata)
                t=paste("N=", nrow(subdata), "trait", colnames(data[i]))
                print(t)
                print(summary(coefs2))
                #return(output_file)  
}
}
raw_regression_univariate(subdata2,c("and","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date"))


##this is the function that will calculate the nonlinear selection differentials. These must be doubled. See Stinchcombe et al 2008

raw_regression_univariate_w_gamma <- function(data,trait=c("and","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date"),y="total_seed" ){
        
        if(!(is.data.frame(data))){stop("invalid dataset name")}  

        trait_columns=c(which(colnames(data) %in% trait))
        y_var_pos=which(colnames(data) %in% y)

        for (i in trait_columns) {
                myvars=c(y_var_pos, i)
           
                #print(myvars)
                subdata=data[,myvars]
                
                subdata=subdata[complete.cases(subdata), ]
        
                #this stlen_lastardizes the traits of interest
                subdata$stand_trait=(subdata[,2]-mean(subdata[,2], na.rm=TRUE))/sd(subdata[,2], na.rm=TRUE)   
                #this relatives fitness
                subdata$rel_fitness=subdata[,1]/mean(subdata[,1])
                coefs2<-lm(subdata$rel_fitness~   subdata$stand_trait +I(subdata$stand_trait^2) , data=subdata)
      
                t=paste("N=", nrow(subdata), "trait", colnames(data[i]))
                print(t)
                print(summary(coefs2))

        }
}
raw_regression_univariate_w_gamma(subdata,c("and","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date"))



###########Selection gradients:
#This function will calculate selection gradients. When calling the function choose either subdata, for the naturally pollinated plants, or subdata2 for hand pollinated group.


raw_regression_gradient <- function(data){
        print(colnames(data))
        trait=c("total_seed","and","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date")
        if(!(is.data.frame(data))){stop("invalid dataset name")} 
        
        trait_columns=c(which(colnames(data) %in% trait))
        myvars=c(trait_columns)
        data=data[,myvars]
  
       data=data[complete.cases(data), ]
                
        #this stlen_lastardizes the traits of interest
        data$and=(data$and-mean(data$and,na.rm=T))/sd(data$and,na.rm=T)
        data$mid_wid=(data$mid_wid-mean(data$mid_wid,na.rm=T))/sd(data$mid_wid,na.rm=T)
        data$len_last=(data$len_last-mean(data$len_last,na.rm=T))/sd(data$len_last,na.rm=T)
        data$flr_num=(data$flr_num-mean(data$flr_num,na.rm=T))/sd(data$flr_num,na.rm=T)
        data$avg_display=(data$avg_display-mean(data$avg_display,na.rm=T))/sd(data$avg_display,na.rm=T)
        data$med_flr_date=(data$med_flr_date-mean(data$med_flr_date,na.rm=T))/sd(data$med_flr_date,na.rm=T)
        
        #this relatives fitness
        data$rel_fitness=data[,1]/mean(data[,1])
        coefs2<-lm(data$rel_fitness ~ data$and + data$mid_wid + data$len_last + data$flr_num + data$avg_display + data$med_flr_date , na.action=na.exclude, data=data)
                
        t=paste("N=", nrow(data), "trait", colnames(data))
        print(t)
        print(summary(coefs2))
        #return(output_file)  
}
raw_regression_gradient(subdata2)

raw_regression_gradient_w_gamma <- function(data){
        print(colnames(data))
        trait=c("total_seed","and","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date")
        if(!(is.data.frame(data))){stop("invalid dataset name")} 
        
        trait_columns=c(which(colnames(data) %in% trait))
        myvars=c(trait_columns)
        data=data[,myvars]
        
        data=data[complete.cases(data), ]
        
        #this stlen_lastardizes the traits of interest
        data$mid_wid=(data$mid_wid-mean(data$mid_wid,na.rm=T))/sd(data$mid_wid,na.rm=T)
        data$and=(data$and-mean(data$and,na.rm=T))/sd(data$and,na.rm=T)
        data$len_last=(data$len_last-mean(data$len_last,na.rm=T))/sd(data$len_last,na.rm=T)
        data$flr_num=(data$flr_num-mean(data$flr_num,na.rm=T))/sd(data$flr_num,na.rm=T)
        data$avg_display=(data$avg_display-mean(data$avg_display,na.rm=T))/sd(data$avg_display,na.rm=T)
        data$med_flr_date=(data$med_flr_date-mean(data$med_flr_date,na.rm=T))/sd(data$med_flr_date,na.rm=T)
        
        #this relatives fitness
        data$rel_fitness=data[,1]/mean(data[,1])
        coefs2<-lm(data$rel_fitness~ data$and + I(data$and^2)+ data$mid_wid + I(data$mid_wid^2)+ data$len_last+I(data$len_last^2) + data$len_last+ I(data$len_last^2) + data$flr_num +I(data$flr_num^2) + data$avg_display+I(data$avg_display^2) +  data$med_flr_date +I(data$med_flr_date^2), data=data)
        print(coefs2)
        t=paste("N=", nrow(data), "trait", colnames(data))
        print(t)
        print(summary(coefs2))
        #return(output_file)  
}
raw_regression_gradient_w_gamma(subdata)



################Bootstrap estimates of CI and pvalues for the selection coefficients.

#note to self: the original work is in the folder called */Selection vs Pollen limitation paper/R ANCOVA
#len_last original script "univariate estimtes len_last CI feb 18 bootstrap.R

###THIS FUNCTION WILL FIND THE CI len_last p values FOR THE LINEAR UNIVARATE ESTIMATES (differentials)
set.seed(2)

set.seed(2)
boot_uni(subdata, subdata2,"and")

boot_uni=function(subdata, subdata2, trait=c("len_last","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date"), y="total_seed") {
        
        trait_columns=c(which(colnames(subdata) %in% trait))
        y_var_pos=which(colnames(subdata) %in% y)
        
        for (i in trait_columns){
                len_last_s1=data.frame()
                len_last_s2=data.frame()
                len_last_diff=data.frame()
                myvars=c(y_var_pos, i)
                for(i in c(1:10000)){
                        subdata_len_last=subdata[,myvars]
                        subdata_len_last=subdata_len_last[complete.cases(subdata_len_last), ]#remove missing data here because it makes stlen_lastardizing the trait values easier later
                        subdata_len_last <- subdata_len_last[sample(1:nrow(subdata_len_last), nrow(subdata_len_last), replace=TRUE),]#this samples s1 within year1
                        subdata_len_last$total_seed=subdata_len_last$total_seed/mean(subdata_len_last$total_seed) #finds relative fitness
                        subdata_len_last$stlen_lastardized_trait=(subdata_len_last[,2]-mean(subdata_len_last[,2], na.rm=TRUE))/sd(subdata_len_last[,2], na.rm=TRUE) #finds stlen_lastard trait
                        regress1<-lm(subdata_len_last$total_seed~subdata_len_last$stlen_lastardized_trait, data=subdata_len_last)$coef
                        
                        #repeat the same thing (getting regression estiamtes) but for pop2.
                        subdata2_len_last=subdata2[myvars]
                        subdata2_len_last=subdata2_len_last[complete.cases(subdata2_len_last), ]#remove missing data here because it makes stlen_lastardizing the trait values easier later
                        subdata2_len_last <- subdata2_len_last[sample(1:nrow(subdata2_len_last), nrow(subdata2_len_last), replace=TRUE),]#this samples s1 within year1
                        subdata2_len_last$total_seed=subdata2_len_last$total_seed/mean(subdata2_len_last$total_seed) #finds relative fitness
                        subdata2_len_last$stlen_lastardized_trait=(subdata2_len_last[,2]-mean(subdata2_len_last[,2], na.rm=TRUE))/sd(subdata2_len_last[,2], na.rm=TRUE) 
                        
                        regress2_1<-lm(subdata2_len_last$total_seed~ subdata2_len_last$stlen_lastardized_trait, data=subdata2_len_last)$coef
                        
                        
                        len_last_s1=rbind(len_last_s1, regress1[2])
                        len_last_s2=rbind(len_last_s2, regress2_1[2])
                        len_last_diff=rbind(len_last_diff, (regress1[2]-regress2_1[2]))
                        
                }
                #print(len_last_s1[,1])
                print("naturally pollinated")
                print(nrow(subdata_len_last))
                print(quantile(len_last_s1[,1], c(0.025, 0.975)))
                print("Pval")
                print((length(len_last_s1[1,][len_last_s1[,1]<=0])/length(len_last_s1))/10000)
                print("hlen_last pollinated")
                print(nrow(subdata2_len_last))
                print(quantile(len_last_s2[,1], c(0.025, 0.975)))
                print("Pval")
                print((length(len_last_s2[,1][len_last_s2[,1]<=0])/length(len_last_s2))/10000)
                print("Diff between hlen_last- len_last naturally pollinated")
                print(quantile(len_last_diff[,1], c(0.025, 0.975)))
                print("Pval")
                print((length(len_last_diff[,1][len_last_diff[,1]<=0])/length(len_last_diff))/10000)
        }
}


#########THIS WILL FIND THE BOOTSTRAPPED CI  and PVALS FOR GAMMAS FOR TOTAL SELECTION IN TABLES S7-S9
set.seed(2)
gammas_uni(subdata, subdata2,"len_last")

gammas_uni=function(subdata, subdata2, trait=c("len_last","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date"), y="total_seed") {

        trait_columns=c(which(colnames(subdata) %in% trait))
        y_var_pos=which(colnames(subdata) %in% y)
        
        for (i in trait_columns){
                len_last_s1=data.frame()
                len_last_s2=data.frame()
                len_last_diff=data.frame()
                myvars=c(y_var_pos, i)
                for(i in c(1:10000)){
                        subdata_len_last=subdata[,myvars]
                        subdata_len_last=subdata_len_last[complete.cases(subdata_len_last), ]#remove missing data here because it makes stlen_lastardizing the trait values easier later
                        subdata_len_last <- subdata_len_last[sample(1:nrow(subdata_len_last), nrow(subdata_len_last), replace=TRUE),]#this samples s1 within year1
                        subdata_len_last$total_seed=subdata_len_last$total_seed/mean(subdata_len_last$total_seed) #finds relative fitness
                        subdata_len_last$stlen_lastardized_trait=(subdata_len_last[,2]-mean(subdata_len_last[,2], na.rm=TRUE))/sd(subdata_len_last[,2], na.rm=TRUE) #finds stlen_lastard trait
                        regress1<-lm(subdata_len_last$total_seed~subdata_len_last$stlen_lastardized_trait +I(subdata_len_last$stlen_lastardized_trait^2), data=subdata_len_last)$coef
    
                        #repeat the same thing (getting regression estiamtes) but for pop2.
                        subdata2_len_last=subdata2[myvars]
                        subdata2_len_last=subdata2_len_last[complete.cases(subdata2_len_last), ]#remove missing data here because it makes stlen_lastardizing the trait values easier later
                        subdata2_len_last <- subdata2_len_last[sample(1:nrow(subdata2_len_last), nrow(subdata2_len_last), replace=TRUE),]#this samples s1 within year1
                        subdata2_len_last$total_seed=subdata2_len_last$total_seed/mean(subdata2_len_last$total_seed) #finds relative fitness
                        subdata2_len_last$stlen_lastardized_trait=(subdata2_len_last[,2]-mean(subdata2_len_last[,2], na.rm=TRUE))/sd(subdata2_len_last[,2], na.rm=TRUE) 
                        
                        regress2_1<-lm(subdata2_len_last$total_seed~ subdata2_len_last$stlen_lastardized_trait + I(subdata2_len_last$stlen_lastardized_trait^2), data=subdata2_len_last)$coef
        
        
                        len_last_s1=rbind(len_last_s1, regress1[3])
                        len_last_s2=rbind(len_last_s2, regress2_1[3])
                        len_last_diff=rbind(len_last_diff, (regress1[3]-regress2_1[3]))
        
                        }
        print(head(len_last_s1))
        print("naturally pollinated")
        print(quantile(len_last_s1[,1], c(0.025, 0.975)))
        print("Pval")
        print((length(len_last_s1[1,][len_last_s1[,1]<=0])/length(len_last_s1))/10000)
        print("hlen_last pollinated")
        print(quantile(len_last_s2[,1], c(0.025, 0.975)))
        print("Pval")
        print((length(len_last_s2[,1][len_last_s2[,1]<=0])/length(len_last_s2))/10000)
        print("Diff between hlen_last- len_last naturally pollinated")
        print(quantile(len_last_diff[,1], c(0.025, 0.975)))
        print("Pval")
        print((length(len_last_diff[,1][len_last_diff[,1]<=0])/length(len_last_diff))/10000)
        }
}


#########THIS WILL FIND THE BOOTSTRAPPED CI len_last PVALS FOR SELECTION GRADIENTS IN TABLES S10-S12


set.seed(2)

trait=c("total_seed","and","mid_wid", "len_last", "flr_num","avg_display", "med_flr_date")


and_s1 <- data.frame()
and_s2 <- data.frame()
and_diff <- data.frame()
mid_wid_s1 <- data.frame()
mid_wid_s2 <- data.frame()
mid_wid_diff <- data.frame()
len_last_s1=data.frame()
len_last_s2=data.frame()
len_last_diff=data.frame()
len_last_s1=data.frame()
len_last_s2=data.frame()
len_last_diff=data.frame()
flr_num_s1=data.frame()
flr_num_s2=data.frame()
flr_num_diff=data.frame()
avg_disp_s1=data.frame()
avg_disp_s2=data.frame()
avg_disp_diff=data.frame()
med_flr_date_s1=data.frame()
med_flr_date_s2=data.frame()
med_flr_date_diff=data.frame()

for(i in c(1:10000)){
        sub=subdata[trait]
        data=sub[complete.cases(sub), ]
        mysample <- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
        
        
        mysample$rel_fitness=mysample$total_seed/mean(mysample$total_seed, na.rm=T)
        mysample$and=(mysample$and-mean(mysample$and, na.rm=T))/sd(mysample$and, na.rm=T)
        mysample$mid_wid=(mysample$mid_wid-mean(mysample$mid_wid, na.rm=T))/sd(mysample$mid_wid, na.rm=T)
        mysample$len_last=(mysample$len_last-mean(mysample$len_last, na.rm=T))/sd(mysample$len_last, na.rm=T)
        mysample$flr_num=(mysample$flr_num-mean(mysample$flr_num, na.rm=T))/sd(mysample$flr_num, na.rm=T)
        mysample$avg_display=(mysample$avg_display-mean(mysample$avg_display, na.rm=T))/sd(mysample$avg_display, na.rm=T)
        mysample$med_flr_date=(mysample$med_flr_date-mean(mysample$med_flr_date, na.rm=T))/sd(mysample$med_flr_date, na.rm=T)
        
        sub2=subdata2[trait]
        sub2=sub2[complete.cases(sub2), ]
        mysample2 <- sub2[sample(1:nrow(sub2), nrow(sub2), replace=TRUE),]
        
        mysample2$rel_fitness=mysample2$total_seed/mean(mysample2$total_seed, na.rm=T)
        mysample2$and=(mysample2$and-mean(mysample2$and, na.rm=T))/sd(mysample2$and, na.rm=T)
        mysample2$mid_wid=(mysample2$mid_wid-mean(mysample2$mid_wid, na.rm=T))/sd(mysample2$mid_wid, na.rm=T)
        mysample2$len_last=(mysample2$len_last-mean(mysample2$len_last, na.rm=T))/sd(mysample2$len_last, na.rm=T)
        mysample2$flr_num=(mysample2$flr_num-mean(mysample2$flr_num, na.rm=T))/sd(mysample2$flr_num, na.rm=T)
        mysample2$avg_display=(mysample2$avg_display-mean(mysample2$avg_display, na.rm=T))/sd(mysample2$avg_display, na.rm=T)
        mysample2$med_flr_date=(mysample2$med_flr_date-mean(mysample2$med_flr_date, na.rm=T))/sd(mysample2$med_flr_date, na.rm=T)
        
        regress1<-lm(mysample$rel_fitness~  mysample$and + mysample$mid_wid +mysample$len_last+ mysample$flr_num + mysample$avg_display+  mysample$med_flr_date , data=mysample)$coef
        regress2<-lm(mysample2$rel_fitness~  mysample2$and + mysample2$mid_wid + mysample2$len_last+  mysample2$flr_num  + mysample2$avg_display +  mysample2$med_flr_date, data=mysample2)$coef
        
        and_s1=rbind(and_s1, regress1[2])
        and_s2=rbind(and_s2, (regress2[2]))
        and_diff=rbind(and_diff, (regress1[2]-regress2[2]))
        
        mid_wid_s1=rbind(mid_wid_s1, regress1[3])
        mid_wid_s2=rbind(mid_wid_s2, regress2[3])
        mid_wid_diff=rbind(mid_wid_diff, (regress1[3]-regress2[3]))
        
        
        
        len_last_s1=rbind(len_last_s1, regress1[4])
        len_last_s2=rbind(len_last_s2, (regress2[4]))
        len_last_diff=rbind(len_last_diff, (regress1[4]-regress2[4]))
        
        flr_num_s1=rbind(flr_num_s1, regress1[5])
        flr_num_s2=rbind(flr_num_s2, (regress2[5]))
        flr_num_diff=rbind(flr_num_diff, (regress1[5]-regress2[5]))
        
        avg_disp_s1=rbind(avg_disp_s1, regress1[6])
        avg_disp_s2=rbind(avg_disp_s2, (regress2[6]))
        avg_disp_diff=rbind(avg_disp_diff, (regress1[6]-regress2[6]))
        
        
        med_flr_date_s1=rbind(med_flr_date_s1, regress1[7])
        med_flr_date_s2=rbind(med_flr_date_s2, regress2[7])
        med_flr_date_diff=rbind(med_flr_date_diff, (regress1[7]-regress2[7]))
        
}



print("and")
print("naturally pollinated")
print(quantile(and_s1[,1], c(0.025, 0.975)))
print("Pval")
print((length(and_s1[1,][and_s1[,1]<=0])/length(and_s1))/10000)
print("hand pollinated")
print(quantile(and_s2[,1], c(0.025, 0.975)))
print("Pval")
print((length(and_s2[,1][and_s2[,1]<=0])/length(and_s2))/10000)
print("Diff between hand- and naturally pollinated")
print(quantile(and_diff[,1], c(0.025, 0.975)))
print("Pval")
print((length(and_diff[,1][and_diff[,1]<=0])/length(and_diff))/10000)


print("mid_wid")
print("naturally pollinated")
print(quantile(mid_wid_s1[,1], c(0.025, 0.975)))
print("Pval")
print((length(mid_wid_s1[1,][mid_wid_s1[,1]<=0])/length(mid_wid_s1))/10000)
print("hmid_wid pollinated")
print(quantile(mid_wid_s2[,1], c(0.025, 0.975)))
print("Pval")
print((length(mid_wid_s2[,1][mid_wid_s2[,1]<=0])/length(mid_wid_s2))/10000)
print("Diff between hmid_wid- mid_wid naturally pollinated")
print(quantile(mid_wid_diff[,1], c(0.025, 0.975)))
print("Pval")
print((length(mid_wid_diff[,1][mid_wid_diff[,1]<=0])/length(mid_wid_diff))/10000)

print("len_last")
print("naturally pollinated")
print(quantile(len_last_s1[,1], c(0.025, 0.975)))
print("Pval")
print((length(len_last_s1[1,][len_last_s1[,1]<=0])/length(len_last_s1))/10000)
print("len_last pollinated")
print(quantile(len_last_s2[,1], c(0.025, 0.975)))
print("Pval")
print((length(len_last_s2[,1][len_last_s2[,1]<=0])/length(len_last_s2))/10000)
print("Diff between hlen_last- len_last naturally pollinated")
print(quantile(len_last_diff[,1], c(0.025, 0.975)))
print("Pval")
print((length(len_last_diff[,1][len_last_diff[,1]<=0])/length(len_last_diff))/10000)

print("flr_num")
print("naturally pollinated")
print(quantile(flr_num_s1[,1], c(0.025, 0.975)))
print("Pval")
print((length(flr_num_s1[1,][flr_num_s1[,1]<=0])/length(flr_num_s1))/10000)
print("flr_num pollinated")
print(quantile(flr_num_s2[,1], c(0.025, 0.975)))
print("Pval")
print((length(flr_num_s2[,1][flr_num_s2[,1]<=0])/length(flr_num_s2))/10000)
print("Diff between flr_num- flr_num naturally pollinated")
print(quantile(flr_num_diff[,1], c(0.025, 0.975)))
print("Pval")
print((length(flr_num_diff[,1][flr_num_diff[,1]<=0])/length(flr_num_diff))/10000)


print("naturally pollinated")
print(quantile(avg_disp_s1[,1], c(0.025, 0.975)))
print("Pval")
print((length(avg_disp_s1[1,][avg_disp_s1[,1]<=0])/length(avg_disp_s1))/10000)
print("avg_disp pollinated")
print(quantile(avg_disp_s2[,1], c(0.025, 0.975)))
print("Pval")
print((length(avg_disp_s2[,1][avg_disp_s2[,1]<=0])/length(avg_disp_s2))/10000)
print("Diff between havg_disp- avg_disp naturally pollinated")
print(quantile(avg_disp_diff[,1], c(0.025, 0.975)))
print("Pval")
print(1-(length(avg_disp_diff[,1][avg_disp_diff[,1]<=0])/length(avg_disp_diff))/10000)

print("med_flr_date")
print("naturally pollinated")
print(quantile(med_flr_date_s1[,1], c(0.025, 0.975)))
print("Pval")
print((length(med_flr_date_s1[1,][med_flr_date_s1[,1]<=0])/length(med_flr_date_s1))/10000)
print("med_flr_date pollinated")
print(quantile(med_flr_date_s2[,1], c(0.025, 0.975)))
print("Pval")
print((length(med_flr_date_s2[,1][med_flr_date_s2[,1]<=0])/length(med_flr_date_s2))/10000)
print("Diff between hmed_flr_date- med_flr_date naturally pollinated")
print(quantile(med_flr_date_diff[,1], c(0.025, 0.975)))
print("Pval")
print((length(med_flr_date_diff[,1][med_flr_date_diff[,1]<=0])/length(med_flr_date_diff))/10000)



##########BOOTSTRAP THE NONLINEAR SELECTION GRADIENTS
set.seed(30)

myvars=c("total_seed", "and","mid_wid", "len_last", "flr_num","avg_display", "med_flr_date")

and_s1 <- data.frame()
and_s2 <- data.frame()
and_diff <- data.frame()
mid_wid_s1 <- data.frame()
mid_wid_s2 <- data.frame()
mid_wid_diff <- data.frame()
len_last_s1=data.frame()
len_last_s2=data.frame()
len_last_diff=data.frame()
len_last_s1=data.frame()
len_last_s2=data.frame()
len_last_diff=data.frame()
flr_num_s1=data.frame()
flr_num_s2=data.frame()
flr_num_diff=data.frame()
avg_disp_s1=data.frame()
avg_disp_s2=data.frame()
avg_disp_diff=data.frame()
med_flr_date_s1=data.frame()
med_flr_date_s2=data.frame()
med_flr_date_diff=data.frame()

for(i in c(1:10000)){
        sub=subdata[myvars] 
        
        data=sub[complete.cases(sub), ]
        mysample <- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
        
        
        mysample$rel_fitness=mysample$total_seed/mean(mysample$total_seed, na.rm=T)
        mysample$and=(mysample$and-mean(mysample$and, na.rm=T))/sd(mysample$and, na.rm=T)
        mysample$mid_wid=(mysample$mid_wid-mean(mysample$mid_wid, na.rm=T))/sd(mysample$mid_wid, na.rm=T)
        mysample$len_last=(mysample$len_last-mean(mysample$len_last, na.rm=T))/sd(mysample$len_last, na.rm=T)
        mysample$flr_num=(mysample$flr_num-mean(mysample$flr_num, na.rm=T))/sd(mysample$flr_num, na.rm=T)
        mysample$avg_display=(mysample$avg_display-mean(mysample$avg_display, na.rm=T))/sd(mysample$avg_display, na.rm=T)
        mysample$med_flr_date=(mysample$med_flr_date-mean(mysample$med_flr_date, na.rm=T))/sd(mysample$med_flr_date, na.rm=T)
        
        
        sub2=subdata2[myvars] 
        data2=sub2[complete.cases(sub2), ]
        mysample2 <- data2[sample(1:nrow(data2), nrow(data2), replace=TRUE),]
        
        mysample2 <- data2[sample(1:nrow(data2), nrow(data2), replace=TRUE),]
        mysample2=mysample2[complete.cases(mysample2), ]
        mysample2$rel_fitness=mysample2$total_seed/mean(mysample2$total_seed, na.rm=T)
        mysample2$and=(mysample2$and-mean(mysample2$and, na.rm=T))/sd(mysample2$and, na.rm=T)
        mysample2$mid_wid=(mysample2$mid_wid-mean(mysample2$mid_wid, na.rm=T))/sd(mysample2$mid_wid, na.rm=T)
        mysample2$len_last=(mysample2$len_last-mean(mysample2$len_last, na.rm=T))/sd(mysample2$len_last, na.rm=T)
        mysample2$flr_num=(mysample2$flr_num-mean(mysample2$flr_num, na.rm=T))/sd(mysample2$flr_num, na.rm=T)
        mysample2$avg_display=(mysample2$avg_display-mean(mysample2$avg_display, na.rm=T))/sd(mysample2$avg_display, na.rm=T)
        mysample2$med_flr_date=(mysample2$med_flr_date-mean(mysample2$med_flr_date, na.rm=T))/sd(mysample2$med_flr_date, na.rm=T)
        
        regress1<-lm(mysample$rel_fitness~  mysample$and +I(mysample$and^2) + mysample$mid_wid + I(mysample$mid_wid^2) + mysample$len_last + I(mysample$len_last^2)+ mysample$flr_num + I(mysample$flr_num^2) + mysample$avg_display + I(mysample$avg_display^2)+  mysample$med_flr_date + I(mysample$med_flr_date^2) , data=mysample)$coef
        regress2<-lm(mysample2$rel_fitness~  mysample2$and + I( mysample2$and^2) + mysample2$mid_wid + I(mysample2$mid_wid^2) + mysample2$len_last + I(mysample2$len_last^2)+  mysample2$flr_num + I(mysample2$flr_num^2) + mysample2$avg_display + I( mysample2$avg_display^2) +  mysample2$med_flr_date + I( mysample2$med_flr_date^2), data=mysample2)$coef
        
        and_s1=rbind(and_s1, regress1[3])
        and_s2=rbind(and_s2, (regress2[3]))
        and_diff=rbind(and_diff, (regress1[3]-regress2[3]))
        
        mid_wid_s1=rbind(mid_wid_s1, regress1[5])
        mid_wid_s2=rbind(mid_wid_s2, regress2[5])
        mid_wid_diff=rbind(mid_wid_diff, (regress1[5]-regress2[5]))
        
        
        
        len_last_s1=rbind(len_last_s1, regress1[7])
        len_last_s2=rbind(len_last_s2, (regress2[7]))
        len_last_diff=rbind(len_last_diff, (regress1[7]-regress2[7]))
        
        flr_num_s1=rbind(flr_num_s1, regress1[9])
        flr_num_s2=rbind(flr_num_s2, (regress2[9]))
        flr_num_diff=rbind(flr_num_diff, (regress1[9]-regress2[9]))
        
        avg_disp_s1=rbind(avg_disp_s1, regress1[11])
        avg_disp_s2=rbind(avg_disp_s2, (regress2[11]))
        avg_disp_diff=rbind(avg_disp_diff, (regress1[11]-regress2[11]))
        
        
        med_flr_date_s1=rbind(med_flr_date_s1, regress1[13])
        med_flr_date_s2=rbind(med_flr_date_s2, regress2[13])
        med_flr_date_diff=rbind(med_flr_date_diff, (regress1[13]-regress2[13]))
        
}

print("and")
print("naturally pollinated")
print(quantile(and_s1[,1], c(0.025, 0.975)))
print("Pval")
print((length(and_s1[1,][and_s1[,1]<=0])/length(and_s1))/10000)
print("hand pollinated")
print(quantile(and_s2[,1], c(0.025, 0.975)))
print("Pval")
print((length(and_s2[,1][and_s2[,1]<=0])/length(and_s2))/10000)
print("Diff between hand- and naturally pollinated")
print(quantile(and_diff[,1], c(0.025, 0.975)))
print("Pval")
print((length(and_diff[,1][and_diff[,1]<=0])/length(and_diff))/10000)


print("mid_wid")
print("naturally pollinated")
print(quantile(mid_wid_s1[,1], c(0.025, 0.975)))
print("Pval")
print((length(mid_wid_s1[1,][mid_wid_s1[,1]<=0])/length(mid_wid_s1))/10000)
print("hmid_wid pollinated")
print(quantile(mid_wid_s2[,1], c(0.025, 0.975)))
print("Pval")
print((length(mid_wid_s2[,1][mid_wid_s2[,1]<=0])/length(mid_wid_s2))/10000)
print("Diff between hmid_wid- mid_wid naturally pollinated")
print(quantile(mid_wid_diff[,1], c(0.025, 0.975)))
print("Pval")
print((length(mid_wid_diff[,1][mid_wid_diff[,1]<=0])/length(mid_wid_diff))/10000)

print("len_last")
print("naturally pollinated")
print(quantile(len_last_s1[,1], c(0.025, 0.1, 0.975)))
print("Pval")
print((length(len_last_s1[1,][len_last_s1[,1]<=0])/length(len_last_s1))/10000)
print("len_last pollinated")
print(quantile(len_last_s2[,1], c(0.025, 0.975)))
print("Pval")
print((length(len_last_s2[,1][len_last_s2[,1]<=0])/length(len_last_s2))/10000)
print("Diff between hlen_last- len_last naturally pollinated")
print(quantile(len_last_diff[,1], c(0.025, 0.975)))
print("Pval")
print((length(len_last_diff[,1][len_last_diff[,1]<=0])/length(len_last_diff))/10000)

print("flr_num")
print("naturally pollinated")
print(quantile(flr_num_s1[,1], c(0.025, 0.975)))
print("Pval")
print((length(flr_num_s1[1,][flr_num_s1[,1]<=0])/length(flr_num_s1))/10000)
print("flr_num pollinated")
print(quantile(flr_num_s2[,1], c(0.025, 0.975)))
print("Pval")
print((length(flr_num_s2[,1][flr_num_s2[,1]<=0])/length(flr_num_s2))/10000)
print("Diff between flr_num- flr_num naturally pollinated")
print(quantile(flr_num_diff[,1], c(0.025, 0.975)))
print("Pval")
print((length(flr_num_diff[,1][flr_num_diff[,1]<=0])/length(flr_num_diff))/10000)


print("naturally pollinated")
print(quantile(avg_disp_s1[,1], c(0.025, 0.975)))
print("Pval")
print((length(avg_disp_s1[1,][avg_disp_s1[,1]<=0])/length(avg_disp_s1))/10000)
print("avg_disp pollinated")
print(quantile(avg_disp_s2[,1], c(0.025, 0.975)))
print("Pval")
print((length(avg_disp_s2[,1][avg_disp_s2[,1]<=0])/length(avg_disp_s2))/10000)
print("Diff between havg_disp- avg_disp naturally pollinated")
print(quantile(avg_disp_diff[,1], c(0.025, 0.975)))
print("Pval")
print((length(avg_disp_diff[,1][avg_disp_diff[,1]<=0])/length(avg_disp_diff))/10000)

print("med_flr_date")
print("naturally pollinated")
print(quantile(med_flr_date_s1[,1], c(0.025, 0.975)))
print("Pval")
print((length(med_flr_date_s1[1,][med_flr_date_s1[,1]<=0])/length(med_flr_date_s1))/10000)
print("med_flr_date pollinated")
print(quantile(med_flr_date_s2[,1], c(0.025, 0.975)))
print("Pval")
print((length(med_flr_date_s2[,1][med_flr_date_s2[,1]<=0])/length(med_flr_date_s2))/10000)
print("Diff between hmed_flr_date- med_flr_date naturally pollinated")
print(quantile(med_flr_date_diff[,1], c(0.025, 0.975)))
print("Pval")
print((length(med_flr_date_diff[,1][med_flr_date_diff[,1]<=0])/length(med_flr_date_diff))/10000)


################################################################################
#TABLES S13 and S14. Differences among years in estimates of directional selection differentials (si), 
#and gradients (beta)for six traits in naturally pollinated and hand-pollinated plants in a population of L. cardinalis.
#For each trait, the difference in pollinated- mediated selection among years (∆spoll) is also presented. 
#For all estimates, associated 95%CI and P-values are based on 10,000 bootstrapped iterations.
################################################################################

#Choose which two years you would like to compare selection differentials
between_year_differential_comparison("2010", "2011", "and")
between_year_differential_comparison=function(year1, year2, trait){
        year=mydata[mydata$Year==year1, ]
        year2=mydata[mydata$Year==year2, ]
        
        
        year_s1=year[year$Pop=='s1',]
        year_s2=year[year$Pop=='s2',]
        
        year2_s1=year2[year2$Pop=='s1',]
        year2_s2=year2[year2$Pop=='s2',]
        
        trait_y1_y2_s1=data.frame()
        trait_y1_y2_s2=data.frame()
        trait_diff_diff=data.frame()
        
        set.seed(2)
        #trait ="and
        myvars=c("total_seed",trait)
        year_s1=year_s1[myvars]
        year_s2=year_s2[myvars]
        year2_s1=year2_s1[myvars]
        year2_s2=year2_s2[myvars]
        
                
        year_s1=year_s1[complete.cases(year_s1), ]
        year_s2=year_s2[complete.cases(year_s2), ]     
        year2_s1=year2_s1[complete.cases(year2_s1), ] 
        year2_s2=year2_s2[complete.cases(year2_s2), ] 
        set.seed(2)
        for(i in c(1:10000)){
                #setup regression for first year for s1 year_s1: need to relative fitness, and standardize trait values for each bootstrapped dataset
                mysample <- year_s1[sample(1:nrow(year_s1), nrow(year_s1), replace=TRUE),]
                mysample$rel_fitness=mysample[,1]/mean(mysample[,1])
                
                mysample$stand_trait=(mysample[,2]-mean(mysample[,2], na.rm=TRUE))/sd(mysample[,2], na.rm=TRUE)   
                
       
                #setup regression for first year pop s2 
                mysample2 <- year_s2[sample(1:nrow(year_s2), nrow(year_s2), replace=TRUE),]
                
                mysample2$rel_fitness=mysample2[,1]/mean(mysample2[,1])
                mysample2$stand_trait=(mysample2[,2]-mean(mysample2[,2], na.rm=TRUE))/sd(mysample2[,2], na.rm=TRUE)   
                
                #this runs the regression for first year s1 and first year s2
                regress1<-lm(mysample$rel_fitness~mysample$stand_trait, data=mysample)$coef
                regress2<-lm(mysample2$rel_fitness~ mysample2$stand_trait,)$coef
                
                
                #setup regression for second year for s1 year_s1
                mysample3 <- year2_s1[sample(1:nrow(year2_s1), nrow(year2_s1), replace=TRUE),]
                mysample3$rel_fitness=mysample3[,1]/mean(mysample3[,1])
                mysample3$stand_trait=(mysample3[,2]-mean(mysample3[,2], na.rm=TRUE))/sd(mysample3[,2], na.rm=TRUE)   
                
                #setup regression for second year s2 year_s2
                mysample4 <- year2_s2[sample(1:nrow(year2_s2), nrow(year2_s2), replace=TRUE),]
                mysample4$rel_fitness=mysample4[,1]/mean(mysample4[,1])
                mysample4$stand_trait=(mysample4[,2]-mean(mysample4[,2], na.rm=TRUE))/sd(mysample4[,2], na.rm=TRUE)   
                
                
                #this runs the regression for the second year population= s1 and second year population= s2
                regress3<-lm(mysample3$rel_fitness ~  mysample3$stand_trait, data=mysample3)$coef
                regress4<-lm(mysample4$rel_fitness~ mysample4$stand_trait, data=mysample4)$coef
                
                #saves output from difference in selection  between y1 and yr2 for s1
                trait_y1_y2_s1=rbind(trait_y1_y2_s1, (regress1[2]-regress3[2]))
                #saves output from differnce in selection between y1 and y2 for s2
                trait_y1_y2_s2=rbind(trait_y1_y2_s2, (regress2[2]-regress4[2]))
                trait_diff_diff=rbind(trait_diff_diff, (regress1[2]-regress2[2])-(regress3[2]-regress4[2]))
   
        }
        
       
        data_list=c(trait_y1_y2_s1, trait_y1_y2_s2, trait_diff_diff)
        data_names=c("trait_y1_y2_s1", "trait_y1_y2_s2", "trait_diff_diff")
        data_explanation=c("year 1 to year 2 for naturally pollinated plants", "year 1 to year 2 for hand-pollinated plants", "year 1 to year 2 difference in pollinator mediated selection")
        
        
        count=0
        
        for(i in data_list){
                count=count+1
                i=as.data.frame(i)
                names(i)[1]="colname"
                print(data_explanation[count])
                if ((quantile(i$colname, c(0.025, 0.975))[1]) >0 & abs(quantile(i$colname, c(0.025, 0.975))[2])>0){
                        print(c(trait,quantile(i$colname, c(0.025, 0.975)),(length(i$colname[i$colname<=0])/10000)))
                }
                else if ( ((length(i$colname[(i$colname)[1]<=0])/length(i$colname))/10000>0.5)){
                        print(c(trait,quantile(i$colname, c(0.025, 0.975)),(1-(length(i$colname[i$colname<=0])/10000))))
                        
                } else {
                        print(c(trait,quantile(i$colname, c(0.025, 0.975)),length(i$colname[i$colname<=0])/10000))
                }
        }
 
}

#Choose which two years you would like to compare selection gradients
between_year_grad_comparison("2010", "2011")
between_year_grad_comparison=function(year1, year2){
        year=mydata[mydata$Year==year1, ]
        year2=mydata[mydata$Year==year2, ]
        
        
        year_s1=year[year$Pop=='s1',]
        year_s2=year[year$Pop=='s2',]
        
        year2_s1=year2[year2$Pop=='s1',]
        year2_s2=year2[year2$Pop=='s2',]
        
        results<-data.frame()
        INT <- data.frame()
        mid_wid <- data.frame()
        and=data.frame()
        len_last=data.frame()
        flr_num=data.frame()
        avg_disp=data.frame()
        med_flr_date=data.frame()
        
        and_y1_y2_s1=data.frame()
        and_y1_y2_s2=data.frame()
        and_diff_diff=data.frame()
        
        mid_wid_y1_y2_s1=data.frame()
        mid_wid_y1_y2_s2=data.frame()
        mid_wid_diff_diff=data.frame()
        
        mid_wid_y1_y2_s1=data.frame()
        mid_wid_y1_y2_s2=data.frame()
        mid_wid_diff_diff=data.frame()
        
        len_last_y1_y2_s1=data.frame()
        len_last_y1_y2_s2=data.frame()
        len_last_diff_diff=data.frame()
        
        flr_num_y1_y2_s1=data.frame()
        flr_num_y1_y2_s2=data.frame()
        flr_num_diff_diff=data.frame()
        
        avg_disp_y1_y2_s1=data.frame()
        avg_disp_y1_y2_s2=data.frame()
        avg_disp_diff_diff=data.frame()
        
        med_flr_date_y1_y2_s1=data.frame()
        med_flr_date_y1_y2_s2=data.frame()
        med_flr_date_diff_diff=data.frame()
        
        set.seed(2)
        trait=c("total_seed","and","mid_wid", "len_last", "flr_num","avg_display", "med_flr_date")
        year_s1=year_s1[trait]
        year_s2=year_s2[trait]
        year2_s1=year2_s1[trait]
        year2_s2=year2_s2[trait]
        
        year_s1=year_s1[complete.cases(year_s1), ]
        year_s2=year_s2[complete.cases(year_s2), ]     
        year2_s1=year2_s1[complete.cases(year2_s1), ] 
        year2_s2=year2_s2[complete.cases(year2_s2), ] 
        
        for(i in c(1:10000)){
                #setup regression for first year for s1 year_s1: need to relative fitness, and standardize trait values for each bootstrapped dataset
                mysample <- year_s1[sample(1:nrow(year_s1), nrow(year_s1), replace=TRUE),]
                mysample$rel_seed=mysample$total_seed/mean(mysample$total_seed)
                mysample$mid_wid=(mysample$mid_wid-mean(mysample$mid_wid))/sd(mysample$mid_wid)
                mysample$and=(mysample$and-mean(mysample$and))/sd(mysample$and)
                mysample$len_last=(mysample$len_last-mean(mysample$len_last))/sd(mysample$len_last)
                mysample$flr_num=(mysample$flr_num-mean(mysample$flr_num))/sd(mysample$flr_num)
                mysample$avg_display=(mysample$avg_display-mean(mysample$avg_display))/sd(mysample$avg_display)
                mysample$med_flr_date=(mysample$med_flr_date-mean(mysample$med_flr_date))/sd(mysample$med_flr_date)
                
                #setup regression for first year pop s2 
                mysample2 <- year_s2[sample(1:nrow(year_s2), nrow(year_s2), replace=TRUE),]
                mysample2$rel_seed=mysample2$total_seed/mean(mysample2$total_seed)
                mysample2$mid_wid=(mysample2$mid_wid-mean(mysample2$mid_wid))/sd(mysample2$mid_wid)
                mysample2$and=(mysample2$and-mean(mysample2$and))/sd(mysample2$and)
                mysample2$len_last=(mysample2$len_last-mean(mysample2$len_last))/sd(mysample2$len_last)
                mysample2$flr_num=(mysample2$flr_num-mean(mysample2$flr_num))/sd(mysample2$flr_num)
                mysample2$avg_display=(mysample2$avg_display-mean(mysample2$avg_display))/sd(mysample2$avg_display)
                mysample2$med_flr_date=(mysample2$med_flr_date-mean(mysample2$med_flr_date))/sd(mysample2$med_flr_date)
                
                #this runs the regression for first year s1 and first year s2
                regress1<-lm(mysample$rel_seed~ mysample$mid_wid + mysample$and + mysample$len_last + mysample$flr_num + mysample$avg_display +  mysample$med_flr_date, data=mysample)$coef
                regress2<-lm(mysample2$rel_seed~ mysample2$mid_wid + mysample2$and + mysample2$len_last + mysample2$flr_num + mysample2$avg_display +  mysample2$med_flr_date, data=mysample2)$coef
                
                
                #setup regression for second year for s1 year_s1
                mysample3 <- year2_s1[sample(1:nrow(year2_s1), nrow(year2_s1), replace=TRUE),]
                mysample3$rel_seed=mysample3$total_seed/mean(mysample3$total_seed)
                mysample3$mid_wid=(mysample3$mid_wid-mean(mysample3$mid_wid))/sd(mysample3$mid_wid)
                mysample3$and=(mysample3$and-mean(mysample3$and))/sd(mysample3$and)
                mysample3$len_last=(mysample3$len_last-mean(mysample3$len_last))/sd(mysample3$len_last)
                mysample3$flr_num=(mysample3$flr_num-mean(mysample3$flr_num))/sd(mysample3$flr_num)
                mysample3$avg_display=(mysample3$avg_display-mean(mysample3$avg_display))/sd(mysample3$avg_display)
                mysample3$med_flr_date=(mysample3$med_flr_date-mean(mysample3$med_flr_date))/sd(mysample3$med_flr_date)
                
                #setup regression for second year s2 year_s2
                mysample4 <- year2_s2[sample(1:nrow(year2_s2), nrow(year2_s2), replace=TRUE),]
                mysample4$rel_seed=mysample4$total_seed/mean(mysample4$total_seed)
                mysample4$mid_wid=(mysample4$mid_wid-mean(mysample4$mid_wid))/sd(mysample4$mid_wid)
                mysample4$and=(mysample4$and-mean(mysample4$and))/sd(mysample4$and)
                mysample4$len_last=(mysample4$len_last-mean(mysample4$len_last))/sd(mysample4$len_last)
                mysample4$flr_num=(mysample4$flr_num-mean(mysample4$flr_num))/sd(mysample4$flr_num)
                mysample4$avg_display=(mysample4$avg_display-mean(mysample4$avg_display))/sd(mysample4$avg_display)
                mysample4$med_flr_date=(mysample4$med_flr_date-mean(mysample4$med_flr_date))/sd(mysample4$med_flr_date)
                
                #this runs the regression for the second year population= s1 and second year population= s2
                regress3<-lm(mysample3$rel_seed~ mysample3$mid_wid + mysample3$and + mysample3$len_last + mysample3$flr_num + mysample3$avg_display +  mysample3$med_flr_date, data=mysample3)$coef
                regress4<-lm(mysample4$rel_seed~ mysample4$mid_wid + mysample4$and + mysample4$len_last + mysample4$flr_num + mysample4$avg_display +  mysample4$med_flr_date, data=mysample4)$coef
                
                #saves output from difference in selection gradient between y1 and yr2 for s1
                mid_wid_y1_y2_s1=rbind(mid_wid_y1_y2_s1, (regress1[2]-regress3[2]))
                and_y1_y2_s1=rbind(and_y1_y2_s1, (regress1[3]-regress3[3]))
                len_last_y1_y2_s1=rbind(len_last_y1_y2_s1, (regress1[4]-regress3[4]))
                flr_num_y1_y2_s1=rbind(flr_num_y1_y2_s1, (regress1[5]-regress3[5]))
                avg_disp_y1_y2_s1=rbind(avg_disp_y1_y2_s1, (regress1[6]--regress3[6]))
                med_flr_date_y1_y2_s1=rbind(med_flr_date_y1_y2_s1, (regress1[7]--regress3[7]))
                
                #saves output from differnce in selection gradient between y1 and y2 for s2
                mid_wid_y1_y2_s2=rbind(mid_wid_y1_y2_s2, (regress2[2]-regress4[2]))
                and_y1_y2_s2=rbind(and_y1_y2_s2, (regress2[3]-regress4[3]))
                len_last_y1_y2_s2=rbind(len_last_y1_y2_s2, (regress2[4]-regress4[4]))
                flr_num_y1_y2_s2=rbind(flr_num_y1_y2_s2, (regress2[5]-regress4[5]))
                avg_disp_y1_y2_s2=rbind(avg_disp_y1_y2_s2, (regress2[6]-regress4[6]))
                med_flr_date_y1_y2_s2=rbind(med_flr_date_y1_y2_s2, (regress2[7]-regress4[7]))
                
                mid_wid_diff_diff=rbind(mid_wid_diff_diff, (regress1[2]-regress2[2])-(regress3[2]-regress4[2]))
                and_diff_diff=rbind(and_diff_diff, (regress1[3]-regress2[3])-(regress3[3]-regress4[3]))
                len_last_diff_diff=rbind(len_last_diff_diff, (regress1[4]-regress2[4])-(regress3[4]-regress4[4]))
                flr_num_diff_diff=rbind(flr_num_diff_diff, (regress1[5]-regress2[5])-(regress3[5]-regress4[5]))
                avg_disp_diff_diff=rbind(avg_disp_diff_diff, (regress1[6]-regress2[6])-(regress3[6]-regress4[6]))
                med_flr_date_diff_diff=rbind(med_flr_date_diff_diff, (regress1[7]-regress2[7])-(regress3[7]-regress4[7]))

               
                }
        
        data_list=c(and_y1_y2_s1, and_y1_y2_s2, and_diff_diff,mid_wid_y1_y2_s1, mid_wid_y1_y2_s2, mid_wid_diff_diff,len_last_y1_y2_s1, len_last_y1_y2_s2, len_last_diff_diff,flr_num_y1_y2_s1, flr_num_y1_y2_s2, flr_num_diff_diff,avg_disp_y1_y2_s1, avg_disp_y1_y2_s2, avg_disp_diff_diff,med_flr_date_y1_y2_s1, med_flr_date_y1_y2_s2, med_flr_date_diff_diff)
        data_names=c("and_y1_y2_s1","and_y1_y2_s2","and_diff_diff","mid_wid_y1_y2_s1","mid_wid_y1_y2_s2","mid_wid_diff_diff","len_last_y1_y2_s1","len_last_y1_y2_s2","len_last_diff_diff","flr_num_y1_y2_s1","flr_num_y1_y2_s2","flr_num_diff_diff","avg_disp_y1_y2_s1","avg_disp_y1_y2_s2","avg_disp_diff_diff","med_flr_date_y1_y2_s1","med_flr_date_y1_y2_s2","med_flr_date_diff_diff")
        count=0
        for(i in data_list){
                count=count+1
                i=as.data.frame(i)
                names(i)[1]="colname"
                if ((quantile(i$colname, c(0.025, 0.975))[1]) >0 & abs(quantile(i$colname, c(0.025, 0.975))[2])>0){
                        print(c("1",data_names[count],quantile(i$colname, c(0.025, 0.975)),((length(i$colname[(i$colname)[1]<=0])/length(i$colname))/10000)))
                }
                else if ( ((length(i$colname[(i$colname)[1]<=0])/length(i$colname))/10000>0.5)){
                        print(c("2",data_names[count],quantile(i$colname, c(0.025, 0.975)),(1-((length(i$colname[(i$colname)[1]<=0])/length(i$colname))/10000))))
                        
                } else {
                        print(c("3",data_names[count],quantile(i$colname, c(0.025, 0.975)),((length(i$colname[(i$colname)[1]<=0])/length(i$colname))/10000)))
                }
        }
}







