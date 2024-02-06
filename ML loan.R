library(DescTools)
library(dplyr)
library(moments)
library(ggplot2)
library(Hmisc)
library(rstatix)
library(dvmisc)
library(regclass)
library(purrr)
library(imputeMissings)
library(lm.beta)
library(caret)
library(naivebayes)
library(rpart.plot)
library(randomForest)

LoanDefaulters[duplicated(LoanDefaulters)]
map(LoanDefaulters,~sum(is.na(.)))
LoanDefaulters<-imputeMissings::impute(LoanDefaulters,method="median/mode")
L1<-as.data.frame(LoanDefaulters)
str(L1)

L1$Gender<-as.factor(L1$Gender)

L1[,c(2:8)]<-lapply(L1[,c(2:8)],factor)
str(L1)

L1[,c(17:19)]<-lapply(L1[,c(17:19)],factor)
L1$credit_type<-as.factor(L1$credit_type)
L1$age<-as.factor(L1$age)

str(L1)

boxplot(L1[c(9,10,11,12,14,16)])
skewness(L1[c(9,10,11,12,14,16)])
kurtosis(L1[c(9,10,11,12,14,16)])

out_loan<-boxplot(L1$loan_amount)$out
length(out_loan)

out_income<-boxplot(L1$income)$out
length(out_income)

out_prop<-boxplot(L1$property_value)$out
length(out_prop)

out_rate<-boxplot(L1$rate_of_interest)$out
length(out_rate)

out_ltv<-boxplot(L1$LTV)$out
length(out_ltv)

# Loan amount	Property value	Income
# -	          log	            removal of outliers

L2<-L1

L2<- L2[-which(L2$income %in% out_income),] #removal
L2$property_value<-log10(L2$property_value) #log

boxplot(L2[c(9,10,11,12,14,16)])
skewness(L2[c(9,10,11,12,14,16)])
kurtosis(L2[c(9,10,11,12,14,16)])

# Loan amount	Property value	Income
#sqrt	       log	             imputation

L3<-L1
L3[L3$income %in% out_income,"income"]= NA #imputation
map(L3,~sum(is.na(.)))

L3$property_value<-log10(L3$property_value)#log
L3$loan_amount<-sqrt(L3$loan_amount)#sqrt
L3<-impute(L3,method = 'median/mode')

boxplot(L3[c(9,10,11,12,14,16)])
skewness(L3[c(9,10,11,12,14,16)])
kurtosis(L3[c(9,10,11,12,14,16)])

#Loan amount	Property value	        Income
#-	          removal of outliers	     removal of outliers

L4<-L1

L4<- L4[-which(L4$income %in% out_income),] #removal
L4<- L4[-which(L4$property_value %in% out_prop),] #removal

boxplot(L4[c(9,10,11,12,14,16)])
skewness(L4[c(9,10,11,12,14,16)])
kurtosis(L4[c(9,10,11,12,14,16)])

#Loan amount	Property value	      Income
#sqrt	        removal of outliers	  imputation

L5<-L1
L5[L5$income %in% out_income,"income"]= NA #imputation
map(L5,~sum(is.na(.)))


L5<- L5[-which(L5$property_value %in% out_prop),]#removal
L5$loan_amount<-sqrt(L5$loan_amount) #sqrt
L5<-impute(L5,method = 'median/mode')

boxplot(L5[c(9,10,11,12,14,16)])
skewness(L5[c(9,10,11,12,14,16)])
kurtosis(L5[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income
#-	          imputation	     removal of outliers
L6<-L1

L6<- L6[-which(L6$income %in% out_income),] #removal

L6[L6$property_value %in% out_prop,"property_value"]= NA #imputation
map(L6,~sum(is.na(.)))
L6<-impute(L6,method = 'median/mode')

boxplot(L6[c(9,10,11,12,14,16)])
skewness(L6[c(9,10,11,12,14,16)])
kurtosis(L6[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income
#sqrt	        imputation	   imputation

L7<-L1
str(L7)
L7$loan_amount<-sqrt(L7$loan_amount) #sqrt 

L7[L7$property_value %in% out_prop,"property_value"]= NA #imputation
map(L7,~sum(is.na(.)))

L7[L7$income %in% out_income,"income"]= NA #imputation
map(L7,~sum(is.na(.)))
L7<-impute(L7,method = 'median/mode')

boxplot(L7[c(9,10,11,12,14,16)])
skewness(L7[c(9,10,11,12,14,16)])
kurtosis(L7[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income
#log	        log	            imputation

L8<-L1
str(L8)

L8$loan_amount<-log10(L8$loan_amount) #log transformation

L8$property_value<-log10(L8$property_value)  #log transformation

L8[L8$income %in% out_income,"income"]= NA #imputation
map(L8,~sum(is.na(.)))

L8<-impute(L8,method = 'median/mode')

boxplot(L8[c(9,10,11,12,14,16)])
skewness(L8[c(9,10,11,12,14,16)])
kurtosis(L8[c(9,10,11,12,14,16)])

#Loan amount	Property value	      Income
#log	        removal of outliers	  imputation

L9<-L1

L9$loan_amount<-log10(L9$loan_amount) #log transfomration

L9<- L9[-which(L9$property_value %in% out_prop),] #removal 

L9[L9$income %in% out_income,"income"]= NA #imputation
map(L9,~sum(is.na(.)))
L9<-impute(L9,method = 'median/mode')

boxplot(L9[c(9,10,11,12,14,16)])
skewness(L9[c(9,10,11,12,14,16)])
kurtosis(L9[c(9,10,11,12,14,16)])


#Loan amount	Property value	 Income
#log	        imputation	     imputation



L10<-L1

L10$loan_amount<-log10(L10$loan_amount) #log 

L10[L10$property_value %in% out_prop,"property_value"]= NA #imputation
map(L10,~sum(is.na(.)))


L10[L10$income %in% out_income,"income"]= NA #imputation
map(L10,~sum(is.na(.)))
L10<-impute(L10,method = 'median/mode')

boxplot(L10[c(9,10,11,12,14,16)])
skewness(L10[c(9,10,11,12,14,16)])
kurtosis(L10[c(9,10,11,12,14,16)])

#Property value	 Income	              LTV	        Rate of interest
#	log	           removal of outliers	imputation	imputation


L11<-L1

L11$property_value<-log10(L11$property_value) #log

L11<- L11[-which(L11$income %in% out_income),] #removal


L11[L11$LTV %in% out_ltv,"LTV"]= NA  #imputataion
map(L11,~sum(is.na(.)))


L11[L11$rate_of_interest %in% out_rate,"rate_of_interest"]= NA #imputation
map(L11,~sum(is.na(.)))
L11<-impute(L11,method = 'median/mode')

boxplot(L11[c(9,10,11,12,14,16)])
skewness(L11[c(9,10,11,12,14,16)])
kurtosis(L11[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income
#imputation	     log	           imputation of outliers (rest unchanged after imputation)

L12<-L1
L12[L12$loan_amount %in% out_loan,"loan_amount"] = NA #imputation of outliers

L12$property_value<-log10(L12$property_value) #log transformation

L12[L12$income %in% out_income,"income"]= NA  #imputation of outliers

map(L12,~sum(is.na(.)))

L12<-impute(L12,method='median/mode')

boxplot(L12[c(9,10,11,12,14,16)])
skewness(L12[c(9,10,11,12,14,16)])
kurtosis(L12[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income
#imputation	     removal	           removal of outliers (rest unchanged after imputation)

L13<-L1
L13[L13$loan_amount %in% out_loan,"loan_amount"] = NA #imputation of outliers

L13<-L13[-which(L13$property_value %in% out_prop),] #removal of outliers

L13<-L13[-which(L13$income %in% out_income),] #removal of outliers

map(L13,~sum(is.na(.)))

L13<-impute(L13,method='median/mode')

boxplot(L13[c(9,10,11,12,14,16)])
skewness(L13[c(9,10,11,12,14,16)])
kurtosis(L13[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income
#imputation	     removal	           imputation of outliers (rest unchanged after imputation)

L14<-L1
L14[L14$loan_amount %in% out_loan,"loan_amount"] = NA #imputation of outliers

L14<-L14[-which(L14$property_value %in% out_prop),] #removal of outliers

L14[L14$income %in% out_income,"income"]= NA  #imputation of outliers

map(L14,~sum(is.na(.)))

L14<-impute(L14,method='median/mode')

boxplot(L14[c(9,10,11,12,14,16)])
skewness(L14[c(9,10,11,12,14,16)])
kurtosis(L14[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income
#imputation	     imputation	           removal of outliers (rest unchanged after imputation)

L15<-L1
L15[L15$loan_amount %in% out_loan,"loan_amount"] = NA #imputation of outliers

L15[L15$property_value %in% out_prop,"property_value"]= NA  #imputation of outliers

L15<-L15[-which(L15$income %in% out_income),] #removal of outliers

map(L15,~sum(is.na(.)))

L15<-impute(L15,method='median/mode')

boxplot(L15[c(9,10,11,12,14,16)])
skewness(L15[c(9,10,11,12,14,16)])
kurtosis(L15[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income
#imputation	   imputation	    imputation


L16<-L1
L16[L16$loan_amount %in% out_loan,"loan_amount"] = NA #imputation of outliers

L16[L16$property_value %in% out_prop,"property_value"]= NA  #imputation of outliers

L16[L16$income %in% out_income,"income"]= NA  #imputation of outliers

map(L16,~sum(is.na(.)))

L16<-impute(L16,method='median/mode')

boxplot(L16[c(9,10,11,12,14,16)])
skewness(L16[c(9,10,11,12,14,16)])
kurtosis(L16[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income
#removal	     log	           removal of outliers (rest unchanged after imputation)

L17<-L1
L17<- L17[-which(L17$loan_amount %in% out_loan),] #removal of outliers

L17$property_value<-log10(L17$property_value) #log transformation

L17<-L17[-which(L17$income %in% out_income),] #removal of outliers

boxplot(L17[c(9,10,11,12,14,16)])
skewness(L17[c(9,10,11,12,14,16)])
kurtosis(L17[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income
#removal	       log	         imputation

L18<-L1
L18<- L18[-which(L18$loan_amount %in% out_loan),] #removal of outliers

L18$property_value<-log10(L18$property_value) #log transformation

L18[L18$income %in% out_income,"income"]= NA

map(L18,~sum(is.na(.)))

L18<-impute(L18,method='median/mode')

boxplot(L18[c(9,10,11,12,14,16)])
skewness(L18[c(9,10,11,12,14,16)])
kurtosis(L18[c(9,10,11,12,14,16)])

#Loan amount	Property value	     Income
#removal	    removal of outliers	 removal of outliers

L19<-L1
L19<- L19[-which(L19$loan_amount %in% out_loan),] #removal of outliers

L19<-L19[-which(L19$property_value %in% out_prop),] #removal of outliers

L19<-L19[-which(L19$income %in% out_income),] #removal of outliers

boxplot(L19[c(9,10,11,12,14,16)])
skewness(L19[c(9,10,11,12,14,16)])
kurtosis(L19[c(9,10,11,12,14,16)])

#Loan amount	Property value	     Income
#removal	    removal of outliers	 imputation

L20<-L1
L20<-L20[-which(L20$loan_amount %in% out_loan),] #removal of outliers

L20<-L20[-which(L20$property_value %in% out_prop),] #removal of outliers

L20[L20$income %in% out_income,"income"]= NA

map(L20,~sum(is.na(.)))

L20<-impute(L20,method = 'median/mode')

boxplot(L20[c(9,10,11,12,14,16)])
skewness(L20[c(9,10,11,12,14,16)])
kurtosis(L20[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income
#removal	    imputation	    removal of outliers

L21<-L1
L21<- L21[-which(L21$loan_amount %in% out_loan),] #removal of outliers

L21[L21$property_value %in% out_prop,"property_value"]=NA

map(L21,~sum(is.na(.)))

L21<-L21[-which(L21$income %in% out_income),] #removal of outliers

map(L21,~sum(is.na(.)))

L21<-impute(L21,method='median/mode')

boxplot(L21[c(9,10,11,12,14,16)])
skewness(L21[c(9,10,11,12,14,16)])
kurtosis(L21[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income
#removal	    imputation	    imputation

L22<-L1
L22<-L22[-which(L22$loan_amount %in% out_loan),] #removal of outliers

L22[L22$property_value %in% out_prop,"property_value"]=NA

map(L22,~sum(is.na(.)))

L22[L22$income %in% out_income,'income']=NA
map(L22,~sum(is.na(.)))

L22<-impute(L22,method = 'median/mode')

boxplot(L22[c(9,10,11,12,14,16)])
skewness(L22[c(9,10,11,12,14,16)])
kurtosis(L22[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income	    LTV	        Rate of interest
#-  	        log	            removal   	imputation	imputation

L23<-L1

L23[L23$rate_of_interest %in% out_rate,"rate_of_interest"]= NA #imputation
map(L23,~sum(is.na(.)))

L23[L23$LTV %in% out_ltv,"LTV"]= NA #imputation
map(L23,~sum(is.na(.)))

L23<- L23[-which(L23$income %in% out_income),] #removal of outliers

L23$property_value<-log10(L23$property_value) #log

L23<-impute(L23,method = 'median/mode')

boxplot(L23[c(9,10,11,12,14,16)])
skewness(L23[c(9,10,11,12,14,16)])
kurtosis(L23[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income	    LTV	        Rate of interest
#sqrt	        log	            imputation	imputation	imputation

L24<-L1

L24[L24$rate_of_interest %in% out_rate,"rate_of_interest"]= NA #imputation
map(L24,~sum(is.na(.)))

L24[L24$LTV %in% out_ltv,"LTV"]= NA #imputation
map(L24,~sum(is.na(.)))

L24[L24$income %in% out_income,"income"]= NA #imputation
map(L24,~sum(is.na(.)))

L24$loan_amount<-sqrt(L24$loan_amount) #sqrt
L24$property_value<-log10(L24$property_value) #log

L24<-impute(L24,method = 'median/mode')

boxplot(L24[c(9,10,11,12,14,16)])
skewness(L24[c(9,10,11,12,14,16)])
kurtosis(L24[c(9,10,11,12,14,16)])

#Loan amount	Property value	     Income	             LTV	      Rate of interest
#-	          removal of outliers	removal of outliers	imputation	imputation
L25<-L1

L25[L25$rate_of_interest %in% out_rate,"rate_of_interest"]= NA #Imputation
map(L25,~sum(is.na(.)))

L25[L25$LTV %in% out_ltv,"LTV"]= NA #imputation
map(L25,~sum(is.na(.)))

L25<-impute(L25,method = 'median/mode')
L25<- L25[-which(L25$income %in% out_income),]#removal

L25<- L25[-which(L25$property_value %in% out_prop),]#removal

boxplot(L25[c(9,10,11,12,14,16)])
skewness(L25[c(9,10,11,12,14,16)])
kurtosis(L25[c(9,10,11,12,14,16)])

#Loan amount	Property value	     Income	    LTV	        Rate of interest
#sqrt	        removal of outliers	imputation	imputation	imputation
L26<-L1

L26[L26$rate_of_interest %in% out_rate,"rate_of_interest"]= NA #imputation
map(L26,~sum(is.na(.)))

L26[L26$LTV %in% out_ltv,"LTV"]= NA #imputation

L26[L26$income %in% out_income,"income"]= NA #imputation
map(L26,~sum(is.na(.)))


L26<- L26[-which(L26$property_value %in% out_prop),] #removal
L26$loan_amount<-sqrt(L26$loan_amount)#sqrt

L26<-impute(L26,method = 'median/mode')
boxplot(L26[c(9,10,11,12,14,16)])
skewness(L26[c(9,10,11,12,14,16)])
kurtosis(L26[c(9,10,11,12,14,16)])

#Loan amount	Property value	  Income            	LTV	        Rate of interest
#-	          imputation	      removal of outliers	imputation	imputation
L27<-L1

L27[L27$rate_of_interest %in% out_rate,"rate_of_interest"]= NA #imputation
map(L27,~sum(is.na(.)))


L27[L27$LTV %in% out_ltv,"LTV"]= NA #imputation
map(L27,~sum(is.na(.)))

L27<- L27[-which(L27$income %in% out_income),] #removal

L27[L27$property_value %in% out_prop,"property_value"]= NA #imputation
map(L27,~sum(is.na(.)))

L27<-impute(L27,method = 'median/mode')

boxplot(L27[c(9,10,11,12,14,16)])
skewness(L27[c(9,10,11,12,14,16)])
kurtosis(L27[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income	    LTV     	Rate of interest
#sqrt	        imputation	    imputation	imputation	imputation

L28<-L1

L28$loan_amount<-sqrt(L28$loan_amount) #sqrt

L28[L28$property_value %in% out_prop,"property_value"]= NA #imputation of outliers
map(L28,~sum(is.na(.)))

L28[L28$income %in% out_income,"income"]= NA #imputation
map(L28,~sum(is.na(.)))

L28[L28$LTV %in% out_ltv,"LTV"]= NA  #imputation
map(L28,~sum(is.na(.)))

L28[L28$rate_of_interest %in% out_rate,"rate_of_interest"]= NA #imputation
map(L28,~sum(is.na(.)))

L28<-impute(L28,method = 'median/mode')

boxplot(L28[c(9,10,11,12,14,16)])
skewness(L28[c(9,10,11,12,14,16)])
kurtosis(L28[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income	    LTV     	Rate of interest
#log	        log	            imputation	imputation	imputation

L29<-L1

L29$loan_amount<-log10(L29$loan_amount) #log

L29$property_value<-log10(L29$property_value)#log

L29[L29$income %in% out_income,"income"]= NA #imputation
map(L29,~sum(is.na(.)))

L29[L29$LTV %in% out_ltv,"LTV"]= NA  #imputation
map(L29,~sum(is.na(.)))

L29[L29$rate_of_interest %in% out_rate,"rate_of_interest"]= NA #imputation
map(L29,~sum(is.na(.)))

L29<-impute(L29,method = 'median/mode')

boxplot(L29[c(9,10,11,12,14,16)])
skewness(L29[c(9,10,11,12,14,16)])
kurtosis(L29[c(9,10,11,12,14,16)])

#Loan amount	Property value	     Income	    LTV	        Rate of interest
#log	        removal of outliers	imputation	imputation	imputation


L30<-L1

L30$loan_amount<-log10(L30$loan_amount) #log

L30<- L30[-which(L30$property_value %in% out_prop),] #removal

L30[L30$income %in% out_income,"income"]= NA #imputation
map(L30,~sum(is.na(.)))


L30[L30$LTV %in% out_ltv,"LTV"]= NA #imputation
map(L30,~sum(is.na(.)))


L30[L30$rate_of_interest %in% out_rate,"rate_of_interest"]= NA #imputation
map(L30,~sum(is.na(.)))

L30<-impute(L30,method = 'median/mode')

boxplot(L30[c(9,10,11,12,14,16)])
skewness(L30[c(9,10,11,12,14,16)])
kurtosis(L30[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income	    LTV	        Rate of interest
#log	       imputation	      imputation	imputation	imputation

L31<-L1

L31$loan_amount<-log10(L31$loan_amount)#log

L31[L31$property_value %in% out_prop,"property_value"]= NA #imputartion
map(L31,~sum(is.na(.)))


L31[L31$rate_of_interest %in% out_rate,"rate_of_interest"]= NA  #imputation
map(L31,~sum(is.na(.)))

L31[L31$income %in% out_income,"income"]= NA #imputation
map(L31,~sum(is.na(.)))


L31[L31$LTV %in% out_ltv,"LTV"]= NA #imputation
map(L31,~sum(is.na(.)))

L31<-impute(L31,method = 'median/mode')

boxplot(L31[c(9,10,11,12,14,16)])
skewness(L31[c(9,10,11,12,14,16)])
kurtosis(L31[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income	               LTV	       Rate of interest
#imputation	  log	             removal of outliers	imputation	imputation

L32<-L1

L32[L32$loan_amount %in% out_loan,"loan_amount"]= NA #imputation
map(L32,~sum(is.na(.)))


L32$property_value<-log10(L32$property_value) #log

L32<- L32[-which(L32$income %in% out_income),] #removal

L32[L32$rate_of_interest %in% out_rate,"rate_of_interest"]= NA #imputation
map(L32,~sum(is.na(.)))


L32[L32$LTV %in% out_ltv,"LTV"]= NA #imputation
map(L32,~sum(is.na(.)))

L32<-impute(L32,method = 'median/mode')

boxplot(L32[c(9,10,11,12,14,16)])
skewness(L32[c(9,10,11,12,14,16)])
kurtosis(L32[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income                 ltv    rate of interest
#imputation	     log	       imputation of outliers  imputation  imputation

L33<-L12
L33[L33$LTV %in% out_ltv,"LTV"]=NA

L33[L33$rate_of_interest %in% out_rate,"rate_of_interest"]=NA

map(L33,~sum(is.na(.)))

L33<-impute(L33,method="median/mode")

boxplot(L33[c(9,10,11,12,14,16)])
skewness(L33[c(9,10,11,12,14,16)])
kurtosis(L33[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income                 ltv    rate of interest
#imputation	     removal	   removal of outliers  imputation  imputation

L34<-L13
L34[L34$LTV %in% out_ltv,"LTV"]=NA

L34[L34$rate_of_interest %in% out_rate,"rate_of_interest"]=NA

map(L34,~sum(is.na(.)))

L34<-impute(L34,method="median/mode")

boxplot(L34[c(9,10,11,12,14,16)])
skewness(L34[c(9,10,11,12,14,16)])
kurtosis(L34[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income                 ltv    rate of interest
#imputation	     removal	   imputation of outliers  imputation  imputation

L35<-L14
L35[L35$LTV %in% out_ltv,"LTV"]=NA

L35[L35$rate_of_interest %in% out_rate,"rate_of_interest"]=NA

map(L35,~sum(is.na(.)))

L35<-impute(L35,method="median/mode")

boxplot(L35[c(9,10,11,12,14,16)])
skewness(L35[c(9,10,11,12,14,16)])
kurtosis(L35[c(9,10,11,12,14,16)])


#Loan amount	Property value	Income                 ltv    rate of interest
#imputation	     imputation	  removal of outliers    imputation  imputation

L36<-L15
L36[L36$LTV %in% out_ltv,"LTV"]=NA

L36[L36$rate_of_interest %in% out_rate,"rate_of_interest"]=NA

map(L36,~sum(is.na(.)))

L36<-impute(L36,method="median/mode")

boxplot(L36[c(9,10,11,12,14,16)])
skewness(L36[c(9,10,11,12,14,16)])
kurtosis(L36[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income          ltv    rate of interest
#imputation	   imputation	    imputation      imputation  imputation

L37<-L16
L37[L37$LTV %in% out_ltv,"LTV"]=NA

L37[L37$rate_of_interest %in% out_rate,"rate_of_interest"]=NA

map(L37,~sum(is.na(.)))

L37<-impute(L37,method="median/mode")

boxplot(L37[c(9,10,11,12,14,16)])
skewness(L37[c(9,10,11,12,14,16)])
kurtosis(L37[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income              	 LTV	       Rate of interest
#removal	    log	             removal of outliers	 imputation	 imputation

L38<-L17
L38[L38$LTV %in% out_ltv,"LTV"]=NA

L38[L38$rate_of_interest %in% out_rate,"rate_of_interest"]=NA

map(L38,~sum(is.na(.)))

L38<-impute(L38,method="median/mode")

boxplot(L38[c(9,10,11,12,14,16)])
skewness(L38[c(9,10,11,12,14,16)])
kurtosis(L38[c(9,10,11,12,14,16)])

#Loan amount	Property value	Income              	 LTV	       Rate of interest
#removal	       log	        imputation	          imputation	imputation

L39<-L18
L39[L39$LTV %in% out_ltv,"LTV"]=NA

L39[L39$rate_of_interest %in% out_rate,"rate_of_interest"]=NA

map(L39,~sum(is.na(.)))

L39<-impute(L39,method="median/mode")

boxplot(L39[c(9,10,11,12,14,16)])
skewness(L39[c(9,10,11,12,14,16)])
kurtosis(L39[c(9,10,11,12,14,16)])

#Loan amount	Property value	    Income              	 LTV	       Rate of interest
#removal	   removal of outliers	removal of outliers	   imputation	 imputation

L40<-L19
L40[L40$LTV %in% out_ltv,"LTV"]=NA

L40[L40$rate_of_interest %in% out_rate,"rate_of_interest"]=NA

map(L40,~sum(is.na(.)))

L40<-impute(L40,method="median/mode")

boxplot(L40[c(9,10,11,12,14,16)])
skewness(L40[c(9,10,11,12,14,16)])
kurtosis(L40[c(9,10,11,12,14,16)])

#Loan amount	Property value	     Income              	 LTV	       Rate of interest
#removal	    removal of outliers	 imputation	            imputation	imputation

L41<-L20
L41[L41$LTV %in% out_ltv,"LTV"]=NA

L41[L41$rate_of_interest %in% out_rate,"rate_of_interest"]=NA

map(L41,~sum(is.na(.)))

L41<-impute(L41,method="median/mode")

boxplot(L41[c(9,10,11,12,14,16)])
skewness(L41[c(9,10,11,12,14,16)])
kurtosis(L41[c(9,10,11,12,14,16)])


#Loan amount	Property value	     Income              	 LTV	       Rate of interest
#removal	    imputation	         removal of outliers	 imputation	imputation

L42<-L21
L42[L42$LTV %in% out_ltv,"LTV"]=NA

L42[L42$rate_of_interest %in% out_rate,"rate_of_interest"]=NA

map(L42,~sum(is.na(.)))

L42<-impute(L42,method="median/mode")

boxplot(L42[c(9,10,11,12,14,16)])
skewness(L42[c(9,10,11,12,14,16)])
kurtosis(L42[c(9,10,11,12,14,16)])

#Loan amount	Property value	     Income              	 LTV	       Rate of interest
#removal	    imputation	         imputation	          imputation	imputation

L43<-L22
L43[L43$LTV %in% out_ltv,"LTV"]=NA

L43[L43$rate_of_interest %in% out_rate,"rate_of_interest"]=NA

map(L43,~sum(is.na(.)))

L43<-impute(L43,method="median/mode")

boxplot(L43[c(9,10,11,12,14,16)])
skewness(L43[c(9,10,11,12,14,16)])
kurtosis(L43[c(9,10,11,12,14,16)])

#MODELS

set.seed(100)
train1 <- createDataPartition(L1$Status, p=0.8, list=FALSE)
training1 <- L1[ train1, ]
testing1 <- L1[ -train1, ]

#decision tree Gini

Model1_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=training1, method="rpart")
p1_gini<-predict(Model1_gini, newdata = testing1)
confusionMatrix(p1_gini, testing1$Status,positive = "1")
rpart.plot(Model1_gini$finalModel)

#decision tree info

Model1_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=training1, method="rpart", parms = list(split = "information"))
p1_info<-predict(Model1_info, newdata = testing1)
confusionMatrix(p1_info, testing1$Status,positive = "1")
rpart.plot(Model1_info$finalModel)

#Random Forest

Model1_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                   data=training1, method="rf")
p1_rf<-predict(Model1_rf, newdata = testing1)
confusionMatrix(p1_rf, testing1$Status,positive = "1")
Model1_rf
Model1_rf$finalModel
varImp(Model1_rf)

set.seed(100)
train2 <- createDataPartition(L2$Status, p=0.8, list=FALSE)
training2 <- L2[ train2, ]
testing2 <- L2[ -train2, ]

xL2<-L2[,c(9,10,11,12,14,16)]
cor(xL2)
modelL2_glm<- train(data=training2,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness
                    +open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL2_glm)
pL2_glm<-predict(modelL2_glm, newdata = testing2)
confusionMatrix(pL2_glm, testing2$Status,positive = "1")#85.74 44.53 99.51

modelL2_glm1<- train(data=training2,Status~Gender+loan_purpose+
                       rate_of_interest+income+LTV, method="glm", family="binomial")
summary(modelL2_glm1)
pL2_glm1<-predict(modelL2_glm1, newdata = testing2)
confusionMatrix(pL2_glm1, testing2$Status,positive = "1")#75.32 2.18 99.75


set.seed(100)
train3 <- createDataPartition(L3$Status, p=0.8, list=FALSE)
training3 <- L3[ train3, ]
testing3 <- L3[ -train3, ]

xL3<-L3[,c(9,10,11,12,14,16)]
cor(xL3)
modelL3_glm<- train(data=training3,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness
                    +open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL3_glm)
pL3_glm<-predict(modelL3_glm, newdata = testing3)
confusionMatrix(pL3_glm, testing3$Status,positive = "1")#85.12 42.36 99.31


modelL3_glm1<- train(data=training3,Status~Gender+loan_type+rate_of_interest+property_value+income+credit_type, method="glm", family="binomial")
summary(modelL3_glm1)
pL3_glm1<-predict(modelL3_glm1, newdata = testing3)
confusionMatrix(pL3_glm1, testing3$Status,positive = "1")#85.64 42.36 100


set.seed(100)
train4 <- createDataPartition(L4$Status, p=0.8, list=FALSE)
training4 <- L4[ train4, ]
testing4 <- L4[ -train4, ]
xL4<-L4[,c(9,10,11,12,14,16)]
cor(xL4)
modelL4_glm<- train(data=training4,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness
                    +open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL4_glm)
pL4_glm<-predict(modelL4_glm, newdata = testing4)
confusionMatrix(pL4_glm, testing4$Status,positive = "1")#87.33 51.49 99.49


modelL4_glm1<- train(data=training4,Status~loan_purpose
                     +rate_of_interest+income++LTV, method="glm", family="binomial")
summary(modelL4_glm1)
pL4_glm1<-predict(modelL4_glm1, newdata = testing4)
confusionMatrix(pL4_glm1, testing4$Status,positive = "1")#75.05 1.49 100

set.seed(100)
train5 <- createDataPartition(L5$Status, p=0.8, list=FALSE)
training5 <- L5[ train5, ]
testing5 <- L5[ -train5, ]

xL5<-L5[,c(9,10,11,12,14,16)]
cor(xL5)
modelL5_glm<- train(data=training5,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness
                    +open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL5_glm)
pL5_glm<-predict(modelL5_glm, newdata = testing5)
confusionMatrix(pL5_glm, testing5$Status,positive = "1")#85.74 44.29 99.76


modelL5_glm1<- train(data=training5,Status~loan_type+loan_purpose
                     +rate_of_interest+income+age+LTV, method="glm", family="binomial")
summary(modelL5_glm1)
pL5_glm1<-predict(modelL5_glm1, newdata = testing5)
confusionMatrix(pL5_glm1, testing5$Status,positive = "1")#74.19 4.28 97.82

set.seed(100)
train6<- createDataPartition(L6$Status, p=0.8, list=FALSE)
training6 <- L6[ train6, ]
testing6 <- L6[ -train6, ]

xL6<-L6[,c(9,10,11,12,14,16)]
cor(xL6)
modelL6_glm<- train(data=training6,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness
                    +open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL6_glm)
pL6_glm<-predict(modelL6_glm, newdata = testing6)
confusionMatrix(pL6_glm, testing6$Status,positive = "1")#85.92 44.53 99.76


modelL6_glm1<- train(data=training6,Status~Gender+loan_purpose
                     +rate_of_interest+income+LTV, method="glm", family="binomial")
summary(modelL6_glm1)
pL6_glm1<-predict(modelL6_glm1, newdata = testing6)
confusionMatrix(pL6_glm1, testing6$Status,positive = "1")#75.32 2.18 99.75

set.seed(100)
train7<- createDataPartition(L7$Status, p=0.8, list=FALSE)
training7 <- L7[ train7, ]
testing7 <- L7[ -train7, ]

xL7<-L7[,c(9,10,11,12,14,16)]
cor(xL7)

modelL7_glm<- train(data=training7,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness
                    +open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL7_glm)
pL7_glm<-predict(modelL7_glm, newdata = testing7)
confusionMatrix(pL7_glm, testing7$Status,positive = "1")#85.12 42.36 99.31


modelL7_glm1<- train(data=training7,Status~Gender+loan_type+loan_purpose
                     +open_credit+rate_of_interest+property_value+income+credit_type+age+LTV, method="glm", family="binomial")
summary(modelL7_glm1)
pL7_glm1<-predict(modelL7_glm1, newdata = testing7)
confusionMatrix(pL7_glm1, testing7$Status,positive = "1")#85.47 42.36 99.77

set.seed(100)
train8<- createDataPartition(L8$Status, p=0.8, list=FALSE)
training8 <- L8[ train8, ]
testing8 <- L8[ -train8, ]

xL8<-L8[,c(9,10,11,12,14,16)]
cor(xL8)
modelL8_glm<- train(data=training8,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness
                    +open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL8_glm)
pL8_glm<-predict(modelL8_glm, newdata = testing8)
confusionMatrix(pL8_glm, testing8$Status,positive = "1")#85.47 43.06 99.54


modelL8_glm1<- train(data=training8,Status~Gender+loan_type+loan_purpose
                     +loan_amount+rate_of_interest+property_value+income+credit_type+age+LTV, method="glm", family="binomial")
summary(modelL8_glm1)
pL8_glm1<-predict(modelL8_glm1, newdata = testing8)
confusionMatrix(pL8_glm1, testing8$Status,positive = "1")#85.64 43.06 99.77

set.seed(100)
train9<- createDataPartition(L9$Status, p=0.8, list=FALSE)
training9 <- L9[ train9, ]
testing9 <- L9[ -train9, ]

xL9<-L9[,c(9,10,11,12,14,16)]
cor(xL9)
modelL9_glm<- train(data=training9,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness
                    +open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL9_glm)
pL9_glm<-predict(modelL9_glm, newdata = testing9)
confusionMatrix(pL9_glm, testing9$Status,positive = "1")#85.92 45 99.76


modelL9_glm1<- train(data=training9,Status~loan_type+loan_purpose
                     +rate_of_interest+income+age+LTV, method="glm", family="binomial")
summary(modelL9_glm1)
pL9_glm1<-predict(modelL9_glm1, newdata = testing9)
confusionMatrix(pL9_glm1, testing9$Status,positive = "1")#74.19 4.28 97.82

set.seed(100)
train10<- createDataPartition(L10$Status, p=0.8, list=FALSE)
training10 <- L10[ train10, ]
testing10 <- L10[ -train10, ]
xL10<-L10[,c(9,10,11,12,14,16)]
cor(xL10)
modelL10_glm<- train(data=training10,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness
                     +open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL10_glm)
pL10_glm<-predict(modelL10_glm, newdata = testing10)
confusionMatrix(pL10_glm, testing10$Status,positive = "1")#85.29 42.36 99.54


modelL10_glm1<- train(data=training10,Status~Gender+loan_type+loan_purpose
                      +open_credit+rate_of_interest+income+age+LTV, method="glm", family="binomial")
summary(modelL10_glm1)
pL10_glm1<-predict(modelL10_glm1, newdata = testing10)
confusionMatrix(pL10_glm1, testing10$Status,positive = "1")#75.09 2.08 99.3

set.seed(100)
train11<- createDataPartition(L11$Status, p=0.8, list=FALSE)
training11 <- L11[ train11, ]
testing11 <- L11[ -train11, ]
xL11<-L11[,c(9,10,11,12,14,16)]
cor(xL11)
modelL11_glm<- train(data=training11,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness
                     +open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL11_glm)
pL11_glm<-predict(modelL11_glm, newdata = testing11)
confusionMatrix(pL11_glm, testing11$Status,positive = "1")#85.37 43.80 99.27


modelL11_glm1<- train(data=training11,Status~Gender+rate_of_interest+income+LTV, method="glm", family="binomial")
summary(modelL11_glm1)
pL11_glm1<-predict(modelL11_glm1, newdata = testing11)
confusionMatrix(pL11_glm1, testing11$Status,positive = "1")#74.95 0 100

set.seed(100)
train12<- createDataPartition(L12$Status, p=0.8, list=FALSE)
training12 <- L12[ train12, ]
testing12 <- L12[ -train12, ]
xL12<-L12[,c(9,10,11,12,14,16)]
cor(xL12)
modelL12_glm<- train(data=training12,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness
                     +open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL12_glm)
pL12_glm<-predict(modelL12_glm, newdata = testing12)
confusionMatrix(pL12_glm, testing12$Status,positive = "1")#85.29 42.36 99.54


modelL12_glm1<- train(data=training12,Status~Gender+loan_type+loan_purpose
                      +open_credit+rate_of_interest+income+credit_type+Credit_Score+age+LTV, method="glm", family="binomial")
summary(modelL12_glm1)
pL12_glm1<-predict(modelL12_glm1, newdata = testing12)
confusionMatrix(pL12_glm1, testing12$Status,positive = "1")#85.47 42.36 99.77

set.seed(100)
train13<- createDataPartition(L13$Status, p=0.8, list=FALSE)
training13 <- L13[ train13, ]
testing13 <- L13[ -train13, ]
xL13<-L13[,c(9,10,11,12,14,16)]
cor(xL13)
modelL13_glm<- train(data=training13,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness
                     +open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL13_glm)
pL13_glm<-predict(modelL13_glm, newdata = testing13)
confusionMatrix(pL13_glm, testing13$Status,positive = "1")#86.96 51.49 98.99


modelL13_glm1<- train(data=training13,Status~loan_purpose
                      +rate_of_interest+income+LTV, method="glm", family="binomial")
summary(modelL13_glm1)
pL13_glm1<-predict(modelL13_glm1, newdata = testing13)
confusionMatrix(pL13_glm1, testing13$Status,positive = "1")#75.05 1.49 100


#naive baye's
modelL2_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training2, method="naive_bayes")
pL2_NBLA<-predict(modelL2_NBLA, newdata = testing2)
confusionMatrix(pL2_NBLA, testing2$Status,positive = "1")

modelL2_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training2, method="naive_bayes")
pL2_NBPV<-predict(modelL2_NBPV, newdata = testing2)
confusionMatrix(pL2_NBPV, testing2$Status,positive = "1")

modelL3_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training3, method="naive_bayes")
pL3_NBLA<-predict(modelL3_NBLA, newdata = testing3)
confusionMatrix(pL3_NBLA, testing3$Status,positive = "1")

modelL3_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training3, method="naive_bayes")
pL3_NBPV<-predict(modelL3_NBPV, newdata = testing3)
confusionMatrix(pL3_NBPV, testing3$Status,positive = "1")


modelL4_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training4, method="naive_bayes")
pL4_NBLA<-predict(modelL4_NBLA, newdata = testing4)
confusionMatrix(pL4_NBLA, testing4$Status,positive = "1")

modelL4_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training4, method="naive_bayes")
pL4_NBPV<-predict(modelL4_NBPV, newdata = testing4)
confusionMatrix(pL4_NBPV, testing4$Status,positive = "1")

modelL5_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training5, method="naive_bayes")
pL5_NBLA<-predict(modelL5_NBLA, newdata = testing5)
confusionMatrix(pL5_NBLA, testing5$Status,positive = "1")

modelL5_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training5, method="naive_bayes")
pL5_NBPV<-predict(modelL5_NBPV, newdata = testing5)
confusionMatrix(pL5_NBPV, testing5$Status,positive = "1")


modelL6_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training6, method="naive_bayes")
pL6_NBLA<-predict(modelL6_NBLA, newdata = testing6)
confusionMatrix(pL6_NBLA, testing6$Status,positive = "1")

modelL6_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training6, method="naive_bayes")
pL6_NBPV<-predict(modelL6_NBPV, newdata = testing6)
confusionMatrix(pL6_NBPV, testing6$Status,positive = "1")


modelL7_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training7, method="naive_bayes")
pL7_NBLA<-predict(modelL7_NBLA, newdata = testing7)
confusionMatrix(pL7_NBLA, testing7$Status,positive = "1")

modelL7_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training7, method="naive_bayes")
pL7_NBPV<-predict(modelL7_NBPV, newdata = testing7)
confusionMatrix(pL7_NBPV, testing7$Status,positive = "1")


modelL8_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training8, method="naive_bayes")
pL8_NBLA<-predict(modelL8_NBLA, newdata = testing8)
confusionMatrix(pL8_NBLA, testing8$Status,positive = "1")

modelL8_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training8, method="naive_bayes")
pL8_NBPV<-predict(modelL8_NBPV, newdata = testing8)
confusionMatrix(pL8_NBPV, testing8$Status,positive = "1")


modelL9_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training9, method="naive_bayes")
pL9_NBLA<-predict(modelL9_NBLA, newdata = testing9)
confusionMatrix(pL9_NBLA, testing9$Status,positive = "1")

modelL9_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training9, method="naive_bayes")
pL9_NBPV<-predict(modelL9_NBPV, newdata = testing9)
confusionMatrix(pL9_NBPV, testing9$Status,positive = "1")


modelL10_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training10, method="naive_bayes")
pL10_NBLA<-predict(modelL10_NBLA, newdata = testing10)
confusionMatrix(pL10_NBLA, testing10$Status,positive = "1")

modelL10_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training10, method="naive_bayes")
pL10_NBPV<-predict(modelL10_NBPV, newdata = testing10)
confusionMatrix(pL10_NBPV, testing10$Status,positive = "1")


modelL11_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training11, method="naive_bayes")
pL11_NBLA<-predict(modelL11_NBLA, newdata = testing11)
confusionMatrix(pL11_NBLA, testing11$Status,positive = "1")

modelL11_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training11, method="naive_bayes")
pL11_NBPV<-predict(modelL11_NBPV, newdata = testing11)
confusionMatrix(pL11_NBPV, testing11$Status,positive = "1")


modelL12_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training12, method="naive_bayes")
pL12_NBLA<-predict(modelL12_NBLA, newdata = testing12)
confusionMatrix(pL12_NBLA, testing12$Status,positive = "1")

modelL12_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training12, method="naive_bayes")
pL12_NBPV<-predict(modelL12_NBPV, newdata = testing12)
confusionMatrix(pL12_NBPV, testing12$Status,positive = "1")


modelL13_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training13, method="naive_bayes")
pL13_NBLA<-predict(modelL13_NBLA, newdata = testing13)
confusionMatrix(pL13_NBLA, testing13$Status,positive = "1")

modelL13_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training13, method="naive_bayes")
pL13_NBPV<-predict(modelL13_NBPV, newdata = testing13)
confusionMatrix(pL13_NBPV, testing13$Status,positive = "1")


#information gain


modelL2_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training2, method="rpart", parms = list(split = "information"))
pL2_info<-predict(modelL2_info, newdata = testing2)
confusionMatrix(pL2_info, testing2$Status,positive = "1")#89.58 100 86.10
rpart.plot(modelL2_info$finalModel)


modelL3_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training3, method="rpart", parms = list(split = "information"))
pL3_info<-predict(modelL3_info, newdata = testing3)
confusionMatrix(pL3_info, testing3$Status,positive = "1")#88.58 100 84.79
rpart.plot(modelL3_info$finalModel)


modelL4_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training4, method="rpart", parms = list(split = "information"))
pL4_info<-predict(modelL4_info, newdata = testing4)
confusionMatrix(pL4_info, testing4$Status,positive = "1")#87.71 100 83.54
rpart.plot(modelL4_info$finalModel)


modelL5_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training5, method="rpart", parms = list(split = "information"))
pL5_info<-predict(modelL5_info, newdata = testing5)
confusionMatrix(pL5_info, testing5$Status,positive = "1")#92.78 88.57 94.20
rpart.plot(modelL5_info$finalModel)


modelL6_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training6, method="rpart", parms = list(split = "information"))
pL6_info<-predict(modelL6_info, newdata = testing6)
confusionMatrix(pL6_info, testing6$Status,positive = "1")#89.58 100 86.10 
rpart.plot(modelL6_info$finalModel)

modelL7_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training7, method="rpart", parms = list(split = "information"))
pL7_info<-predict(modelL7_info, newdata = testing7)
confusionMatrix(pL7_info, testing7$Status,positive = "1")#88.58 100 84.79 
rpart.plot(modelL7_info$finalModel)


modelL8_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training8, method="rpart", parms = list(split = "information"))
pL8_info<-predict(modelL8_info, newdata = testing8)
confusionMatrix(pL8_info, testing8$Status,positive = "1")#88.58 100 84.79
rpart.plot(modelL8_info$finalModel)


modelL9_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training9, method="rpart", parms = list(split = "information"))
pL9_info<-predict(modelL9_info, newdata = testing9)
confusionMatrix(pL9_info, testing9$Status,positive = "1")#92.78 88.57 94.20
rpart.plot(modelL9_info$finalModel)

modelL10_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training10, method="rpart", parms = list(split = "information"))
pL10_info<-predict(modelL10_info, newdata = testing10)
confusionMatrix(pL10_info, testing10$Status,positive = "1")#88.58 100 84.79
rpart.plot(modelL10_info$finalModel)


modelL11_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training11, method="rpart", parms = list(split = "information"))
pL11_info<-predict(modelL11_info, newdata = testing11)
confusionMatrix(pL11_info, testing11$Status,positive = "1")#88.85 81.02 91.46
rpart.plot(modelL11_info$finalModel)

modelL12_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training12, method="rpart", parms = list(split = "information"))
pL12_info<-predict(modelL12_info, newdata = testing12)
confusionMatrix(pL12_info, testing12$Status,positive = "1")#88.58 100 84.79
rpart.plot(modelL12_info$finalModel)


modelL13_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training13, method="rpart", parms = list(split = "information"))
pL13_info<-predict(modelL13_info, newdata = testing13)
confusionMatrix(pL13_info, testing13$Status,positive = "1")#87.71 100 83.54
rpart.plot(modelL13_info$finalModel)

#gini index

modelL2_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training2, method="rpart")
pL2_gini<-predict(modelL2_gini, newdata = testing2)
confusionMatrix(pL2_gini, testing2$Status,positive = "1")#89.58 100 86.10
rpart.plot(modelL2_gini$finalModel)


modelL3_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training3, method="rpart")
pL3_gini<-predict(modelL3_gini, newdata = testing3)
confusionMatrix(pL3_gini, testing3$Status,positive = "1")#88.58 100 84.79
rpart.plot(modelL3_gini$finalModel)

modelL4_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training4, method="rpart")
pL4_gini<-predict(modelL4_gini, newdata = testing4)
confusionMatrix(pL4_gini, testing4$Status,positive = "1")#87.71 100 83.54
rpart.plot(modelL4_gini$finalModel)


modelL5_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training5, method="rpart")
pL5_gini<-predict(modelL5_gini, newdata = testing5)
confusionMatrix(pL5_gini, testing5$Status,positive = "1")#91.88 100 89.13
rpart.plot(modelL5_gini$finalModel)


modelL6_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training6, method="rpart")
pL6_gini<-predict(modelL6_gini, newdata = testing6)
confusionMatrix(pL6_gini, testing6$Status,positive = "1")#89.58 100 86.10
rpart.plot(modelL6_gini$finalModel)


modelL7_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training7, method="rpart")
pL7_gini<-predict(modelL7_gini, newdata = testing7)
confusionMatrix(pL7_gini, testing7$Status,positive = "1")#88.58 100 84.79
rpart.plot(modelL7_gini$finalModel)


modelL8_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training8, method="rpart")
pL8_gini<-predict(modelL8_gini, newdata = testing8)
confusionMatrix(pL8_gini, testing8$Status,positive = "1")#88.58 100 84.79
rpart.plot(modelL8_gini$finalModel)


modelL9_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training9, method="rpart")
pL9_gini<-predict(modelL9_gini, newdata = testing9)
confusionMatrix(pL9_gini, testing9$Status,positive = "1")#91.88 100 89.13
rpart.plot(modelL9_gini$finalModel)


modelL10_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training10, method="rpart")
pL10_gini<-predict(modelL10_gini, newdata = testing10)
confusionMatrix(pL10_gini, testing10$Status,positive = "1")#88.58 100 84.79
rpart.plot(modelL10_gini$finalModel)


modelL11_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training11, method="rpart")
pL11_gini<-predict(modelL11_gini, newdata = testing11)
confusionMatrix(pL11_gini, testing11$Status,positive = "1")#88.85 78.83 92.20 
rpart.plot(modelL11_gini$finalModel)


modelL12_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training12, method="rpart")
pL12_gini<-predict(modelL12_gini, newdata = testing12)
confusionMatrix(pL12_gini, testing12$Status,positive = "1")#88.58 100 84.79
rpart.plot(modelL12_gini$finalModel)


modelL13_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training13, method="rpart")
pL13_gini<-predict(modelL13_gini, newdata = testing13)
confusionMatrix(pL13_gini, testing13$Status,positive = "1")#87.71 100 83.54
rpart.plot(modelL13_gini$finalModel)


#random forest
modelL2_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training2, method="rf")
pL2_rf<-predict(modelL2_rf, newdata = testing2)
confusionMatrix(pL2_rf, testing2$Status,positive = "1")#92.5 87.59 94.15
modelL2_rf
modelL2_rf$finalModel
varImp(modelL2_rf)

modelL3_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training3, method="rf")
pL3_rf<-predict(modelL3_rf, newdata = testing3)
confusionMatrix(pL3_rf, testing3$Status,positive = "1")#88.06 82.64 89.86
modelL3_rf
modelL3_rf$finalModel
varImp(modelL3_rf)

modelL4_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training4, method="rf")
pL4_rf<-predict(modelL4_rf, newdata = testing4)
confusionMatrix(pL4_rf, testing4$Status,positive = "1")#90.17 86.57 91.39
modelL4_rf
modelL4_rf$finalModel
varImp(modelL4_rf)


modelL5_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training5, method="rf")
pL5_rf<-predict(modelL5_rf, newdata = testing5)
confusionMatrix(pL5_rf, testing5$Status,positive = "1")#92.06 87.86 93.48
modelL5_rf
modelL5_rf$finalModel
varImp(modelL5_rf)

modelL6_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training6, method="rf")
pL6_rf<-predict(modelL6_rf, newdata = testing6)
confusionMatrix(pL6_rf, testing6$Status,positive = "1")#92.87 87.59 94.63
modelL6_rf
modelL6_rf$finalModel
varImp(modelL6_rf)

modelL7_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training7, method="rf")
pL7_rf<-predict(modelL7_rf, newdata = testing7)
confusionMatrix(pL7_rf, testing7$Status,positive = "1")#87.89 82.64 89.63
modelL7_rf
modelL7_rf$finalModel
varImp(modelL7_rf)


modelL8_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training8, method="rf")
pL8_rf<-predict(modelL8_rf, newdata = testing8)
confusionMatrix(pL8_rf, testing8$Status,positive = "1")#88.06 82.64 89.86
modelL8_rf
modelL8_rf$finalModel
varImp(modelL8_rf)


modelL9_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training9, method="rf")
pL9_rf<-predict(modelL9_rf, newdata = testing9)
confusionMatrix(pL9_rf, testing9$Status,positive = "1")#92.42 86.43 94.44
modelL9_rf
modelL9_rf$finalModel
varImp(modelL9_rf)


modelL10_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=training10, method="rf")
pL10_rf<-predict(modelL10_rf, newdata = testing10)
confusionMatrix(pL10_rf, testing10$Status,positive = "1")#88.58 83.33 90.32
modelL10_rf
modelL10_rf$finalModel
varImp(modelL10_rf)


modelL11_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=training11, method="rf")
pL11_rf<-predict(modelL11_rf, newdata = testing11)
confusionMatrix(pL11_rf, testing11$Status,positive = "1")#89.95 78.10 93.90
modelL11_rf
modelL11_rf$finalModel
varImp(modelL11_rf)


modelL12_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=training12, method="rf")
pL12_rf<-predict(modelL12_rf, newdata = testing12)
confusionMatrix(pL12_rf, testing12$Status,positive = "1")#87.72 81.94 89.63
modelL12_rf
modelL12_rf$finalModel
varImp(modelL12_rf)

modelL13_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=training13, method="rf")
pL13_rf<-predict(modelL13_rf, newdata = testing13)
confusionMatrix(pL13_rf, testing13$Status,positive = "1")#89.41 85.07 90.89
modelL13_rf
modelL13_rf$finalModel
varImp(modelL13_rf)

set.seed(100)
trainL14<- createDataPartition(L14 $Status, p=0.8, list=FALSE)
trainingL14 <- L14 [ trainL14, ]
testingL14 <- L14[ -trainL14, ]


x14<-L14[,c(9,10,11,12,14,16)]
cor(x14) #no multi-collinearity

modelL14_glm<- train(data=trainingL14,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL14_glm)
pL14_glm<-predict(modelL14_glm, newdata = testingL14)
confusionMatrix(pL14_glm, testingL14$Status,positive = "1")#85.38 42.57 99.52

modelL14_glm1<- train(data=trainingL14,Status~loan_type+loan_purpose+rate_of_interest+income+age+LTV, method="glm", family="binomial")
summary(modelL14_glm1)
pL14_glm1<-predict(modelL14_glm1, newdata = testingL14)
confusionMatrix(pL14_glm1, testingL14$Status,positive = "1") #74.19  4.28  97.82

#NAIVE BAYES

modelL14_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL14, method="naive_bayes")
pL14_NBLA<-predict(modelL14_NBLA, newdata = testingL14)
confusionMatrix(pL14_NBLA, testingL14$Status,positive = "1")

modelL14_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL14, method="naive_bayes")
pL14_NBPV<-predict(modelL14_NBPV, newdata = testingL14)
confusionMatrix(pL14_NBPV, testingL14$Status,positive = "1")


#DECION TREE GINI
ModelL14_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL14, method="rpart")
pL14_gini<-predict(ModelL14_gini, newdata = testingL14)
confusionMatrix(pL14_gini, testingL14$Status,positive = "1") #91.88  100  89.13
rpart.plot(ModelL14_gini$finalModel)

#decision tree info

ModelL14_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL14, method="rpart", parms = list(split = "information"))
pL14_info<-predict(ModelL14_info, newdata = testingL14)
confusionMatrix(pL14_info, testingL14$Status,positive = "1")#92.78  88.57  94.20
rpart.plot(ModelL14_info$finalModel)


#Random Forest

ModelL14_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=trainingL14, method="rf")
pL14_rf<-predict(ModelL14_rf, newdata = testingL14)
confusionMatrix(pL14_rf, testingL14$Status,positive = "1")#92.24 87.86 93.72
ModelL14_rf
ModelL14_rf$finalModel
varImp(ModelL14_rf)


set.seed(100)
trainL15 <- createDataPartition(L15$Status, p=0.8, list=FALSE)
trainingL15 <- L15[ trainL15, ]
testingL15 <- L15[ -trainL15, ]

#logistic regression models

xL15<-L15[,c(9,10,11,12,14,16)]
cor(xL15) #no multi-collinearity

modelL15_glm<- train(data=trainingL15,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL15_glm)
pL15_glm<-predict(modelL15_glm, newdata = testingL15)
confusionMatrix(pL15_glm, testingL15$Status,positive = "1")#85.92 44.53 99.76

#Now use the variables which were significant

modelL15_glm1<- train(data=trainingL15,Status~Gender+loan_purpose+Credit_Worthiness+rate_of_interest+income+LTV, method="glm", family="binomial")
summary(modelL15_glm1)
pL15_glm1<-predict(modelL15_glm1, newdata = testingL15)
confusionMatrix(pL15_glm1, testingL15$Status,positive = "1") #75.14 2.91 99.26

#naive byes models

modelL15_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL15, method="naive_bayes")
pL15_NBLA<-predict(modelL15_NBLA, newdata = testingL15)
confusionMatrix(pL15_NBLA, testingL15$Status,positive = "1")

modelL15_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL15, method="naive_bayes")
pL15_NBPV<-predict(modelL15_NBPV, newdata = testingL15)
confusionMatrix(pL15_NBPV, testingL15$Status,positive = "1")

#decision tree Gini

ModelL15_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL15, method="rpart")
pL15_gini<-predict(ModelL15_gini, newdata = testingL15)
confusionMatrix(pL15_gini, testingL15$Status,positive = "1") #89.58 100 86.10
rpart.plot(ModelL15_gini$finalModel)

#decision tree info

ModelL15_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL15, method="rpart", parms = list(split = "information"))
pL15_info<-predict(ModelL15_info, newdata = testingL15)
confusionMatrix(pL15_info, testingL15$Status,positive = "1")#89.58 100 86.10
rpart.plot(ModelL15_info$finalModel)

#Random Forest

ModelL15_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=trainingL15, method="rf")
pL15_rf<-predict(ModelL15_rf, newdata = testingL15)
confusionMatrix(pL15_rf, testingL15$Status,positive = "1")#92.5 87.59 94.15
ModelL15_rf
ModelL15_rf$finalModel
varImp(ModelL15_rf)




set.seed(100)
trainL16 <- createDataPartition(L16$Status, p=0.8, list=FALSE)
trainingL16 <- L16[ trainL16, ]
testingL16 <- L16[ -trainL16, ]

#logistic regression models

xL16<-L16[,c(9,10,11,12,14,16)]
cor(xL16) #no multi-collinearity

modelL16_glm<- train(data=trainingL16,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL16_glm)
pL16_glm<-predict(modelL16_glm, newdata = testingL16)
confusionMatrix(pL16_glm, testingL16$Status,positive = "1")#85.29 42.36 99.54

`#Now use the variables which were significant

modelL16_glm1<- train(data=trainingL16,Status~Gender+loan_type+loan_purpose+open_credit+rate_of_interest+property_value+income+credit_type+age+LTV, method="glm", family="binomial")

summary(modelL16_glm1)
pL16_glm1<-predict(modelL16_glm1, newdata = testingL16)
confusionMatrix(pL16_glm1, testingL16$Status,positive = "1")#85.47 42.36 99.77

#naive byes models

modelL16_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL16, method="naive_bayes")
pL16_NBLA<-predict(modelL16_NBLA, newdata = testingL16)
confusionMatrix(pL16_NBLA, testingL16$Status,positive = "1")

modelL16_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL16, method="naive_bayes")
pL16_NBPV<-predict(modelL16_NBPV, newdata = testingL16)
confusionMatrix(pL16_NBPV, testingL16$Status,positive = "1")

#decision tree Gini

ModelL16_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL16, method="rpart")
pL16_gini<-predict(ModelL16_gini, newdata = testingL16)
confusionMatrix(pL16_gini, testingL16$Status,positive = "1")#88.68 100 84.79
rpart.plot(ModelL16_gini$finalModel)

#decision tree info

ModelL16_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL16, method="rpart", parms = list(split = "information"))
pL16_info<-predict(ModelL16_info, newdata = testingL16)
confusionMatrix(pL16_info, testingL16$Status,positive = "1")#88.58 100 84.79
rpart.plot(ModelL16_info$finalModel)

#Random Forest

ModelL16_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=trainingL16, method="rf")
pL16_rf<-predict(ModelL16_rf, newdata = testingL16)
confusionMatrix(pL16_rf, testingL16$Status,positive = "1")#88.06 83.33 89.63
ModelL16_rf
ModelL16_rf$finalModel
varImp(ModelL16_rf)  


set.seed(100)
trainL23 <- createDataPartition(L23$Status, p=0.8, list=FALSE)
trainingL23 <- L23[ trainL23, ]
testingL23<- L23[ -trainL23, ]

#logistic regression models

xL23<-L23[,c(9,10,11,12,14,16)]
cor(xL23) #no multi-collinearity

modelL23_glm<- train(data=trainingL23,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL23_glm)
pL23_glm<-predict(modelL23_glm, newdata = testingL23)
confusionMatrix(pL23_glm, testingL23$Status,positive = "1")#85.37 43.80 99.27

`#Now use the variables which were significant

modelL23_glm1<- train(data=trainingL23,Status~Gender+rate_of_interest+income+LTV, method="glm", family="binomial")
summary(modelL23_glm1)
pL23_glm1<-predict(modelL23_glm1, newdata = testingL23)
confusionMatrix(pL23_glm1, testingL23$Status,positive = "1")#74.95 0 100

#naive byes models

modelL23_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL23, method="naive_bayes")
pL23_NBLA<-predict(modelL23_NBLA, newdata = testingL23)
confusionMatrix(pL23_NBLA, testingL23$Status,positive = "1")

modelL23_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL23, method="naive_bayes")
pL23_NBPV<-predict(modelL23_NBPV, newdata = testingL23)
confusionMatrix(pL23_NBPV, testingL23$Status,positive = "1")

#decision tree Gini

ModelL23_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL23, method="rpart")
pL23_gini<-predict(ModelL23_gini, newdata = testingL23)
confusionMatrix(pL23_gini, testingL23$Status,positive = "1")#88.85 78.83 92.20
rpart.plot(ModelL23_gini$finalModel)

#decision tree info

ModelL23_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL23, method="rpart", parms = list(split = "information"))
pL23_info<-predict(ModelL23_info, newdata = testingL23)
confusionMatrix(pL23_info, testingL23$Status,positive = "1")#88.85 81.02 91.46
rpart.plot(ModelL23_info$finalModel)

#Random Forest

ModelL23_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=trainingL23, method="rf")
pL23_rf<-predict(ModelL23_rf, newdata = testingL23)
confusionMatrix(pL23_rf, testingL23$Status,positive = "1")#89.95 81.75 92.68
ModelL23_rf
ModelL23_rf$finalModel
varImp(ModelL23_rf)


set.seed(100)
trainL24 <- createDataPartition(L24$Status, p=0.8, list=FALSE)
trainingL24 <- L24[ trainL24, ]
testingL24 <- L24[ -trainL24, ]

#logistic regression models

xL24<-L24[,c(9,10,11,12,14,16)]
cor(xL24) #no multi-collinearity

modelL24_glm<- train(data=trainingL24,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL24_glm)
pL24_glm<-predict(modelL24_glm, newdata = testingL24)
confusionMatrix(pL24_glm, testingL24$Status,positive = "1")#85.29 43.06 99.31

#Now use the variables which were significant

modelL24_glm1<- train(data=trainingL24,Status~Gender+loan_type+loan_purpose+loan_amount+rate_of_interest+property_value+income+credit_type+age+LTV, method="glm", family="binomial")
summary(modelL24_glm1)
pL24_glm1<-predict(modelL24_glm1, newdata = testingL24)
confusionMatrix(pL24_glm1, testingL24$Status,positive = "1")#85.29 43.06 99.31

#naive byes models

modelL24_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL24, method="naive_bayes")
pL24_NBLA<-predict(modelL24_NBLA, newdata = testingL24)
confusionMatrix(pL24_NBLA, testingL24$Status,positive = "1")

modelL24_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL24, method="naive_bayes")
pL24_NBPV<-predict(modelL24_NBPV, newdata = testingL24)
confusionMatrix(pL24_NBPV, testingL24$Status,positive = "1")


#decision tree Gini

ModelL24_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL24, method="rpart")
pL24_gini<-predict(ModelL24_gini, newdata = testingL24)
confusionMatrix(pL24_gini, testingL24$Status,positive = "1")#86.33 53.47 97.24
rpart.plot(ModelL24_gini$finalModel)

#decision tree info

ModelL24_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL24, method="rpart", parms = list(split = "information"))
pL24_info<-predict(ModelL24_info, newdata = testingL24)
confusionMatrix(pL24_info, testingL24$Status,positive = "1")#86.33 53.47 97.24
rpart.plot(ModelL24_info$finalModel)

#Random Forest

ModelL24_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=trainingL24, method="rf")
pL24_rf<-predict(ModelL24_rf, newdata = testingL24)
confusionMatrix(pL24_rf, testingL24$Status,positive = "1")#88.06 75.69 92.17
ModelL24_rf
ModelL24_rf$finalModel
varImp(ModelL24_rf)




set.seed(100)
trainL25 <- createDataPartition(L25$Status, p=0.8, list=FALSE)
trainingL25 <- L25 [trainL25, ]
testingL25 <- L25[ -trainL25, ]

#logistic regression models

xL25<-L25[,c(9,10,11,12,14,16)]
cor(xL25) #no multi-collinearity

modelL25_glm<- train(data=trainingL25,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL25_glm)
pL25_glm<-predict(modelL25_glm, newdata = testingL25)
confusionMatrix(pL25_glm, testingL25$Status,positive = "1")#87.15 50.75 99.49

`#Now use the variables which were significant

modelL25_glm1<- train(data=trainingL25,Status~Gender+loan_purpose+rate_of_interest+income+LTV, method="glm", family="binomial")
summary(modelL25_glm1)
pL25_glm1<-predict(modelL25_glm1, newdata = testingL25)
confusionMatrix(pL25_glm1, testingL25$Status,positive = "1")#74.67 0.76 99.74

#naive byes models

modelL25_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL25, method="naive_bayes")
pL25_NBLA<-predict(modelL25_NBLA, newdata = testingL25)
confusionMatrix(pL25_NBLA, testingL25$Status,positive = "1")

modelL25_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL25, method="naive_bayes")
pL25_NBPV<-predict(modelL25_NBPV, newdata = testingL25)
confusionMatrix(pL25_NBPV, testingL25$Status,positive = "1")


#decision tree Gini

ModelL25_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL25, method="rpart")
pL25_gini<-predict(ModelL25_gini, newdata = testingL25)
confusionMatrix(pL25_gini, testingL25$Status,positive = "1")#88.09 76.87 91.90
rpart.plot(ModelL25_gini$finalModel)

#decision tree info

ModelL25_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL25, method="rpart", parms = list(split = "information"))
pL25_info<-predict(ModelL25_info, newdata = testingL25)
confusionMatrix(pL25_info, testingL25$Status,positive = "1")#88.09 76.87 91.90
rpart.plot(ModelL25_info$finalModel)

#Random Forest

ModelL25_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=trainingL25, method="rf")
pL25_rf<-predict(ModelL25_rf, newdata = testingL25)
confusionMatrix(pL25_rf, testingL25$Status,positive = "1")#88.47 82.09 90.63
ModelL25_rf
ModelL25_rf$finalModel
varImp(ModelL25_rf)



set.seed(100)
trainL26 <- createDataPartition(L26$Status, p=0.8, list=FALSE)
trainingL26 <- L26[ trainL26, ]
testingL26 <- L26[ -trainL26, ]

#logistic regression models

xL26<-L26[,c(9,10,11,12,14,16)]
cor(xL26) #no multi-collinearity

modelL26_glm<- train(data=trainingL26,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL26_glm)
pL26_glm<-predict(modelL26_glm, newdata = testingL26)
confusionMatrix(pL26_glm, testingL26$Status,positive = "1")#85.02 42.14 99.52

#Now use the variables which were significant

modelL26_glm1<- train(data=trainingL26,Status~loan_type+rate_of_interest+income+age+LTV, method="glm", family="binomial")
summary(modelL26_glm1)
pL26_glm1<-predict(modelL26_glm1, newdata = testingL26)
confusionMatrix(pL26_glm1, testingL26$Status,positive = "1")#74.19 2.14 98.55

#naive byes models

modelL26_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL26, method="naive_bayes")
pL26_NBLA<-predict(modelL26_NBLA, newdata = testingL26)
confusionMatrix(pL26_NBLA, testingL26$Status,positive = "1")

modelL26_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL26, method="naive_bayes")
pL26_NBPV<-predict(modelL26_NBPV, newdata = testingL26)
confusionMatrix(pL26_NBPV, testingL26$Status,positive = "1")

#decision tree Gini

ModelL26_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL26, method="rpart")
pL26_gini<-predict(ModelL26_gini, newdata = testingL26)
confusionMatrix(pL26_gini, testingL26$Status,positive = "1")#88.63 63.57 97.10
rpart.plot(ModelL26_gini$finalModel)

#decision tree info

ModelL26_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL26, method="rpart", parms = list(split = "information"))
pL26_info<-predict(ModelL26_info, newdata = testingL26)
confusionMatrix(pL26_info, testingL26$Status,positive = "1")#88.63 63.57 97.10
rpart.plot(ModelL26_info$finalModel)

#Random Forest

ModelL26_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                     data=trainingL26, method="rf")
pL26_rf<-predict(ModelL26_rf, newdata = testingL26)
confusionMatrix(pL26_rf, testingL26$Status,positive = "1")#91.52 82.14 94.69
ModelL26_rf
ModelL26_rf$finalModel
varImp(ModelL26_rf)



set.seed(100)
trainL27 <- createDataPartition(L27$Status, p=0.8, list=FALSE)
trainingL27 <- L27[ trainL27, ]
testingL27 <- L27[ -trainL27, ]

#logistic regression models

xL27<-L27[,c(9,10,11,12,14,16)]
cor(xL27) #no multi-collinearity

modelL27_glm<- train(data=trainingL27,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL27_glm)
pL27_glm<-predict(modelL27_glm, newdata = testingL27)
confusionMatrix(pL27_glm, testingL27$Status,positive = "1")#85.92 43.80 100

#Now use the variables which were significant

modelL27_glm1<- train(data=trainingL27,Status~Gender+rate_of_interest+property_value+income+LTV, method="glm", family="binomial")
summary(modelL27_glm1)
pL27_glm1<-predict(modelL27_glm1, newdata = testingL27)
confusionMatrix(pL27_glm1, testingL27$Status,positive = "1")#74.95 0 100

#naive byes models

modelL27_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL27, method="naive_bayes")
pL27_NBLA<-predict(modelL27_NBLA, newdata = testingL27)
confusionMatrix(pL27_NBLA, testingL27$Status,positive = "1")

modelL27_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL27, method="naive_bayes")
pL27_NBPV<-predict(modelL27_NBPV, newdata = testingL27)
confusionMatrix(pL27_NBPV, testingL27$Status,positive = "1")


#decision tree Gini

ModelL27_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL27, method="rpart")
pL27_gini<-predict(ModelL27_gini, newdata = testingL27)
confusionMatrix(pL27_gini, testingL27$Status,positive = "1")#88.85 78.83 92.20
rpart.plot(ModelL27_gini$finalModel)

#decision tree info

ModelL27_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=trainingL27, method="rpart", parms = list(split = "information"))
pL27_info<-predict(ModelL27_info, newdata = testingL27)
confusionMatrix(pL27_info, testingL27$Status,positive = "1")#88.85 81.02 91.46
rpart.plot(ModelL27_info$finalModel
#Random Forest
           
ModelL27_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=trainingL27, method="rf")
pL27_rf<-predict(ModelL27_rf, newdata = testingL27)
confusionMatrix(pL27_rf, testingL27$Status,positive = "1")#90.49 79.56 94.15
ModelL27_rf
ModelL27_rf$finalModel
varImp(ModelL27_rf)
           
           
           
set.seed(100)
trainL28 <- createDataPartition(L28$Status, p=0.8, list=FALSE)
trainingL28 <- L28[ trainL28, ]
testingL28 <- L28[ -trainL28, ]
           
#logistic regression models
           
xL28<-L28[,c(9,10,11,12,14,16)]
cor(xL28) #no multi-collinearity
           
modelL28_glm<- train(data=trainingL28,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL28_glm)
pL28_glm<-predict(modelL28_glm, newdata = testingL28)
confusionMatrix(pL28_glm, testingL28$Status,positive = "1")#85.12 42.36 99.31
           
#Now use the variables which were significant
           
modelL28_glm1<- train(data=trainingL28,Status~Gender+loan_type+loan_purpose+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV, method="glm", family="binomial")
summary(modelL28_glm1)
pL28_glm1<-predict(modelL28_glm1, newdata = testingL28)
confusionMatrix(pL28_glm1, testingL28$Status,positive = "1")#85.29 42.36 99.54
           
#naive byes models
           
modelL28_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                data=trainingL28, method="naive_bayes")
pL28_NBLA<-predict(modelL28_NBLA, newdata = testingL28)
confusionMatrix(pL28_NBLA, testingL28$Status,positive = "1")
           
modelL28_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                 data=trainingL28, method="naive_bayes")
pL28_NBPV<-predict(modelL28_NBPV, newdata = testingL28)
confusionMatrix(pL28_NBPV, testingL28$Status,positive = "1")
           
           
#decision tree Gini
           
ModelL28_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                  data=trainingL28, method="rpart")
pL28_gini<-predict(ModelL28_gini, newdata = testingL28)
confusionMatrix(pL28_gini, testingL28$Status,positive = "1")#86.33 53.47 97.24
rpart.plot(ModelL28_gini$finalModel)
           
#decision tree info
           
ModelL28_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                  data=trainingL28, method="rpart", parms = list(split = "information"))
pL28_info<-predict(ModelL28_info, newdata = testingL28)
confusionMatrix(pL28_info, testingL28$Status,positive = "1")#86.33 53.47 97.24
rpart.plot(ModelL28_info$finalModel)
           
#Random Forest
           
ModelL28_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
               data=trainingL28, method="rf")
pL28_rf<-predict(ModelL28_rf, newdata = testingL28)
confusionMatrix(pL28_rf, testingL28$Status,positive = "1")#87.37 72.92 92.17
ModelL28_rf
ModelL28_rf$finalModel
varImp(ModelL28_rf)
           
           
           
set.seed(100)
trainL29 <- createDataPartition(L29 $Status, p=0.8, list=FALSE)
trainingL29 <- L29[ trainL29, ]
testingL29 <- L29[ -trainL29, ]
           
#logistic regression models
           
xL29<-L29[,c(9,10,11,12,14,16)]
cor(xL29) #no multi-collinearity
           
modelL29_glm<- train(data=trainingL29,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL29_glm)
pL29_glm<-predict(modelL29_glm, newdata = testingL29)
confusionMatrix(pL29_glm, testingL29$Status,positive = "1")#85.47 43.06 99.54
           
#Now use the variables which were significant
           
modelL29_glm1<- train(data=trainingL29,Status~Gender+loan_type+loan_purpose+rate_of_interest+income+credit_type+age+LTV, method="glm", family="binomial")
summary(modelL29_glm1)
pL29_glm1<-predict(modelL29_glm1, newdata = testingL29)
confusionMatrix(pL29_glm1, testingL29$Status,positive = "1")#85.47 42.36 99.77
           
#naive byes models
           
modelL29_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                  data=trainingL29, method="naive_bayes")
pL29_NBLA<-predict(modelL29_NBLA, newdata = testingL29)
confusionMatrix(pL29_NBLA, testingL29$Status,positive = "1")
           
modelL29_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                 data=trainingL29, method="naive_bayes")
pL29_NBPV<-predict(modelL29_NBPV, newdata = testingL29)
confusionMatrix(pL29_NBPV, testingL29$Status,positive = "1")
           
#decision tree Gini
           
ModelL29_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                 data=trainingL29, method="rpart")
pL29_gini<-predict(ModelL29_gini, newdata = testingL29)
confusionMatrix(pL29_gini, testingL29$Status,positive = "1")#86.33 53.47 97.24
rpart.plot(ModelL29_gini$finalModel)
           
#decision tree info
           
ModelL29_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                  data=trainingL29, method="rpart", parms = list(split = "information"))
pL29_info<-predict(ModelL29_info, newdata = testingL29)
confusionMatrix(pL29_info, testingL29$Status,positive = "1")#86.33 53.47 97.24
rpart.plot(ModelL29_info$finalModel)
           
#Random Forest
           
ModelL29_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
              data=trainingL29, method="rf")
pL29_rf<-predict(ModelL29_rf, newdata = testingL29)
confusionMatrix(pL29_rf, testingL29$Status,positive = "1")#87.2 74.31 91.47
ModelL29_rf
ModelL29_rf$finalModel
varImp(ModelL29_rf)
           
           
           
set.seed(100)
trainL30 <- createDataPartition(L30$Status, p=0.8, list=FALSE)
trainingL30 <- L30[ trainL30, ]
testingL30 <- L30[ -trainL30, ]
           
#logistic regression models
           
xL30<-L30[,c(9,10,11,12,14,16)]
cor(xL30) #no multi-collinearity
           
modelL30_glm<- train(data=trainingL30,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL30_glm)
pL30_glm<-predict(modelL30_glm, newdata = testingL30)
confusionMatrix(pL30_glm, testingL30$Status,positive = "1")#85.02 42.86 99.28
           
#Now use the variables which were significant
           
modelL30_glm1<- train(data=trainingL30,Status~loan_type+rate_of_interest+income+age+LTV, method="glm", family="binomial")
summary(modelL30_glm1)
pL30_glm1<-predict(modelL30_glm1, newdata = testingL30)
confusionMatrix(pL30_glm1, testingL30$Status,positive = "1")#74.19 2.14 98.55
           
#naive byes models
           
modelL30_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                data=trainingL30, method="naive_bayes")
pL30_NBLA<-predict(modelL30_NBLA, newdata = testingL30)
confusionMatrix(pL30_NBLA, testingL30$Status,positive = "1")
           
modelL30_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                  data=trainingL30, method="naive_bayes")
pL30_NBPV<-predict(modelL30_NBPV, newdata = testingL30)
confusionMatrix(pL30_NBPV, testingL30$Status,positive = "1")
           
#decision tree Gini
           
ModelL30_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                 data=trainingL30, method="rpart")
pL30_gini<-predict(ModelL30_gini, newdata = testingL30)
confusionMatrix(pL30_gini, testingL30$Status,positive = "1")#88.63 63.57 97.10
rpart.plot(ModelL30_gini$finalModel)
           
#decision tree info
           
ModelL30_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                data=trainingL30, method="rpart", parms = list(split = "information"))
pL30_info<-predict(ModelL30_info, newdata = testingL30)
confusionMatrix(pL30_info, testingL30$Status,positive = "1")#88.63 63.57 97.10
rpart.plot(ModelL30_info$finalModel)
           
#Random Forest
           
ModelL30_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                data=trainingL30, method="rf")
pL30_rf<-predict(ModelL30_rf, newdata = testingL30)
confusionMatrix(pL30_rf, testingL30$Status,positive = "1")#90.16 78.57 94.69
ModelL30_rf
ModelL30_rf$finalModel
varImp(ModelL30_rf)
           
           
           
set.seed(100)
trainL31 <- createDataPartition(L31$Status, p=0.8, list=FALSE)
trainingL31 <-L31[ trainL31, ]
testingL31 <- L31[ -trainL31, ]
           
#logistic regression models
           
xL31<-L31[,c(9,10,11,12,14,16)]
cor(xL31) #no multi-collinearity
           
modelL31_glm<- train(data=trainingL31,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(modelL31_glm)
pL31_glm<-predict(modelL31_glm, newdata = testingL31)
confusionMatrix(pL31_glm, testingL31$Status,positive = "1")#85.29 42.36 99.54
           
#Now use the variables which were significant
           
modelL31_glm1<- train(data=trainingL31,Status~Gender+loan_type+loan_purpose+rate_of_interest+property_value+income+credit_type+age+LTV, method="glm", family="binomial")
summary(modelL31_glm1)
pL31_glm1<-predict(modelL31_glm1, newdata = testingL31)
confusionMatrix(pL31_glm1, testingL31$Status,positive = "1")#85.47 42.36 99.77
           
#naive byes models
           
modelL31_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                  data=trainingL31, method="naive_bayes")
pL31_NBLA<-predict(modelL31_NBLA, newdata = testingL31)
confusionMatrix(pL31_NBLA, testingL31$Status,positive = "1")
           
modelL31_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                  data=trainingL31, method="naive_bayes")
pL31_NBPV<-predict(modelL31_NBPV, newdata = testingL31)
confusionMatrix(pL31_NBPV, testingL31$Status,positive = "1")
           
#decision tree Gini
           
ModelL31_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                  data=trainingL31, method="rpart")
pL31_gini<-predict(ModelL31_gini, newdata = testingL31)
confusionMatrix(pL31_gini, testingL31$Status,positive = "1")#86.33 53.47 97.24
rpart.plot(ModelL31_gini$finalModel)
           
#decision tree info
           
ModelL31_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                  data=trainingL31, method="rpart", parms = list(split = "information"))
pL31_info<-predict(ModelL31_info, newdata = testingL31)
confusionMatrix(pL31_info, testingL31$Status,positive = "1")#86.33 53.47 97.24
rpart.plot(ModelL31_info$finalModel)
           
#Random Forest
           
ModelL31_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                data=trainingL31, method="rf")
pL31_rf<-predict(ModelL31_rf, newdata = testingL31)
confusionMatrix(pL31_rf, testingL31$Status,positive = "1")#87.02 75 91.01
ModelL31_rf
ModelL31_rf$finalModel
varImp(ModelL31_rf)

set.seed(100)
train32 <- createDataPartition(L32$Status, p=0.8, list=FALSE)
training32 <- L32[ train32, ]
testing32 <- L32[ -train32, ]

#logistic regression models

x32<-L32[,c(9,10,11,12,14,16)]
cor(x32) #no multi-collinearity

model32_glm<- train(data=training32,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model32_glm)
p32_glm<-predict(model32_glm, newdata = testing32)
confusionMatrix(p32_glm, testing32$Status,positive = "1")

`#Now use the variables which were significant

model32_glm1<- train(data=training32,Status~Gender+loan_type+loan_purpose+rate_of_interest+property_value+income+LTV, method="glm", family="binomial")
summary(model32_glm1)
p32_glm1<-predict(model32_glm1, newdata = testing32)
confusionMatrix(p32_glm1, testing32$Status,positive = "1")

#naive byes models

model32_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training32, method="naive_bayes")
p32_NBLA<-predict(model32_NBLA, newdata = testing32)
confusionMatrix(p32_NBLA, testing32$Status,positive = "1")

model32_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training32, method="naive_bayes")
p32_NBPV<-predict(model32_NBPV, newdata = testing32)
confusionMatrix(p32_NBPV, testing32$Status,positive = "1")

#decision tree Gini

Model32_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training32, method="rpart")
p32_gini<-predict(Model32_gini, newdata = testing32)
confusionMatrix(p32_gini, testing32$Status,positive = "1")
rpart.plot(Model32_gini$finalModel)

#decision tree info

Model32_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training32, method="rpart", parms = list(split = "information"))
p32_info<-predict(Model32_info, newdata = testing32)
confusionMatrix(p32_info, testing32$Status,positive = "1")
rpart.plot(Model32_info$finalModel)

#Random Forest

Model32_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training32, method="rf")
p32_rf<-predict(Model32_rf, newdata = testing32)
confusionMatrix(p32_rf, testing32$Status,positive = "1")
Model32_rf
Model32_rf$finalModel
varImp(Model32_rf)

#models 

set.seed(100)
train33 <- createDataPartition(L33$Status, p=0.8, list=FALSE)
training33 <- L33[ train33, ]
testing33 <- L33[ -train33, ]

#logistic regression models

x33<-L33[,c(9,10,11,12,14,16)]
cor(x33) #no multi-collinearity

model33_glm<- train(data=training33,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model33_glm)
p33_glm<-predict(model33_glm, newdata = testing33)
confusionMatrix(p33_glm, testing33$Status,positive = "1")

`#Now use the variables which were significant

model33_glm1<- train(data=training33,Status~Gender+loan_type+loan_purpose+rate_of_interest+property_value+income+credit_type+age+LTV, method="glm", family="binomial")
summary(model33_glm1)
p33_glm1<-predict(model33_glm1, newdata = testing33)
confusionMatrix(p33_glm1, testing33$Status,positive = "1")

#naive byes models

model33_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training33, method="naive_bayes")
p33_NBLA<-predict(model33_NBLA, newdata = testing33)
confusionMatrix(p33_NBLA, testing33$Status,positive = "1")

model33_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training33, method="naive_bayes")
p33_NBPV<-predict(model33_NBPV, newdata = testing33)
confusionMatrix(p33_NBPV, testing33$Status,positive = "1")

#decision tree Gini

Model33_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training33, method="rpart")
p33_gini<-predict(Model33_gini, newdata = testing33)
confusionMatrix(p33_gini, testing33$Status,positive = "1")
rpart.plot(Model33_gini$finalModel)

#decision tree info

Model33_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training33, method="rpart", parms = list(split = "information"))
p33_info<-predict(Model33_info, newdata = testing33)
confusionMatrix(p33_info, testing33$Status,positive = "1")
rpart.plot(Model33_info$finalModel)

#Random Forest

Model33_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training33, method="rf")
p33_rf<-predict(Model33_rf, newdata = testing33)
confusionMatrix(p33_rf, testing33$Status,positive = "1")
Model33_rf
Model33_rf$finalModel
varImp(Model33_rf)

#models

set.seed(100)
train34 <- createDataPartition(L34$Status, p=0.8, list=FALSE)
training34 <- L34[ train34, ]
testing34 <- L34[ -train34, ]

#logistic regression models

x34<-L34[,c(9,10,11,12,14,16)]
cor(x34) #no multi-collinearity

model34_glm<- train(data=training34,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model34_glm)
p34_glm<-predict(model34_glm, newdata = testing34)
confusionMatrix(p34_glm, testing34$Status,positive = "1")

`#Now use the variables which were significant

model34_glm1<- train(data=training34,Status~Gender+loan_type+rate_of_interest+income+LTV, method="glm", family="binomial")
summary(model34_glm1)
p34_glm1<-predict(model34_glm1, newdata = testing34)
confusionMatrix(p34_glm1, testing34$Status,positive = "1")

#naive byes models

model34_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training34, method="naive_bayes")
p34_NBLA<-predict(model34_NBLA, newdata = testing34)
confusionMatrix(p34_NBLA, testing34$Status,positive = "1")

model34_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training34, method="naive_bayes")
p34_NBPV<-predict(model34_NBPV, newdata = testing34)
confusionMatrix(p34_NBPV, testing34$Status,positive = "1")

#decision tree Gini

Model34_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training34, method="rpart")
p34_gini<-predict(Model34_gini, newdata = testing34)
confusionMatrix(p34_gini, testing34$Status,positive = "1")
rpart.plot(Model34_gini$finalModel)

#decision tree info

Model34_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training34, method="rpart", parms = list(split = "information"))
p34_info<-predict(Model34_info, newdata = testing34)
confusionMatrix(p34_info, testing34$Status,positive = "1")
rpart.plot(Model34_info$finalModel)

#Random Forest

Model34_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training34, method="rf")
p34_rf<-predict(Model34_rf, newdata = testing34)
confusionMatrix(p34_rf, testing34$Status,positive = "1")
Model34_rf
Model34_rf$finalModel
varImp(Model34_rf)

#models 

set.seed(100)
train35 <- createDataPartition(L35$Status, p=0.8, list=FALSE)
training35 <- L35[ train35, ]
testing35 <- L35[ -train35, ]

#logistic regression models

x35<-L35[,c(9,10,11,12,14,16)]
cor(x35) 
#no multi-collinearity

model35_glm<- train(data=training35,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model35_glm)
p35_glm<-predict(model35_glm, newdata = testing35)
confusionMatrix(p35_glm, testing35$Status,positive = "1")

#Now use the variables which were significant

model35_glm1<- train(data=training35,Status~loan_type+loan_purpose+rate_of_interest+income+age+LTV, method="glm", family="binomial")
summary(model35_glm1)
p35_glm1<-predict(model35_glm1, newdata = testing35)
confusionMatrix(p35_glm1, testing35$Status,positive = "1")

#naive byes models

model35_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training35, method="naive_bayes")
p35_NBLA<-predict(model35_NBLA, newdata = testing35)
confusionMatrix(p35_NBLA, testing35$Status,positive = "1")

model35_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training35, method="naive_bayes")
p35_NBPV<-predict(model35_NBPV, newdata = testing35)
confusionMatrix(p35_NBPV, testing35$Status,positive = "1")

#decision tree Gini

Model35_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training35, method="rpart")
p35_gini<-predict(Model35_gini, newdata = testing35)
confusionMatrix(p35_gini, testing35$Status,positive = "1")
rpart.plot(Model35_gini$finalModel)

#decision tree info

Model35_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training35, method="rpart", parms = list(split = "information"))
p35_info<-predict(Model35_info, newdata = testing35)
confusionMatrix(p35_info, testing35$Status,positive = "1")
rpart.plot(Model35_info$finalModel)

#Random Forest

Model35_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training35, method="rf")
p35_rf<-predict(Model35_rf, newdata = testing35)
confusionMatrix(p35_rf, testing35$Status,positive = "1")
Model35_rf
Model35_rf$finalModel
varImp(Model35_rf)

#models 

set.seed(100)
train36 <- createDataPartition(L36$Status, p=0.8, list=FALSE)
training36 <- L36[ train36, ]
testing36 <- L36[ -train36, ]

#logistic regression models

x36<-L36[,c(9,10,11,12,14,16)]
cor(x36) #no multi-collinearity

model36_glm<- train(data=training36,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model36_glm)
p36_glm<-predict(model36_glm, newdata = testing36)
confusionMatrix(p36_glm, testing36$Status,positive = "1")

`#Now use the variables which were significant

model36_glm1<- train(data=training36,Status~Gender+loan_type+loan_purpose+rate_of_interest+property_value+income+LTV, method="glm", family="binomial")
summary(model36_glm1)
p36_glm1<-predict(model36_glm1, newdata = testing36)
confusionMatrix(p36_glm1, testing36$Status,positive = "1")

#naive byes models

model36_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training36, method="naive_bayes")
p36_NBLA<-predict(model36_NBLA, newdata = testing36)
confusionMatrix(p36_NBLA, testing36$Status,positive = "1")

model36_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training36, method="naive_bayes")
p36_NBPV<-predict(model36_NBPV, newdata = testing36)
confusionMatrix(p36_NBPV, testing36$Status,positive = "1")

#decision tree Gini

Model36_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training36, method="rpart")
p36_gini<-predict(Model36_gini, newdata = testing36)
confusionMatrix(p36_gini, testing36$Status,positive = "1")
rpart.plot(Model36_gini$finalModel)

#decision tree info

Model36_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training36, method="rpart", parms = list(split = "information"))
p36_info<-predict(Model36_info, newdata = testing36)
confusionMatrix(p36_info, testing36$Status,positive = "1")
rpart.plot(Model36_info$finalModel)

#Random Forest

Model36_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training36, method="rf")
p36_rf<-predict(Model36_rf, newdata = testing36)
confusionMatrix(p36_rf, testing36$Status,positive = "1")
Model36_rf
Model36_rf$finalModel
varImp(Model36_rf)

#models 

set.seed(100)
train37 <- createDataPartition(L37$Status, p=0.8, list=FALSE)
training37 <- L37[ train37, ]
testing37 <- L37[ -train37, ]

#logistic regression models

x37<-L37[,c(9,10,11,12,14,16)]
cor(x37) #no multi-collinearity

model37_glm<- train(data=training37,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model37_glm)
p37_glm<-predict(model37_glm, newdata = testing37)
confusionMatrix(p37_glm, testing37$Status,positive = "1")

`#Now use the variables which were significant

model37_glm1<- train(data=training37,Status~Gender+loan_type+loan_purpose+rate_of_interest+property_value+income+credit_type+age+LTV, method="glm", family="binomial")
summary(model37_glm1)
p37_glm1<-predict(model37_glm1, newdata = testing37)
confusionMatrix(p37_glm1, testing37$Status,positive = "1")

#naive byes models

model37_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training37, method="naive_bayes")
p37_NBLA<-predict(model37_NBLA, newdata = testing37)
confusionMatrix(p37_NBLA, testing37$Status,positive = "1")

model37_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training37, method="naive_bayes")
p37_NBPV<-predict(model37_NBPV, newdata = testing37)
confusionMatrix(p37_NBPV, testing37$Status,positive = "1")

#decision tree Gini

Model37_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training37, method="rpart")
p37_gini<-predict(Model37_gini, newdata = testing37)
confusionMatrix(p37_gini, testing37$Status,positive = "1")
rpart.plot(Model37_gini$finalModel)

#decision tree info

Model37_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training37, method="rpart", parms = list(split = "information"))
p37_info<-predict(Model37_info, newdata = testing37)
confusionMatrix(p37_info, testing37$Status,positive = "1")
rpart.plot(Model37_info$finalModel)

#Random Forest

Model37_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training37, method="rf")
p37_rf<-predict(Model37_rf, newdata = testing37)
confusionMatrix(p37_rf, testing37$Status,positive = "1")
Model37_rf
Model37_rf$finalModel
varImp(Model37_rf)

set.seed(100)
train17 <- createDataPartition(L17$Status, p=0.8, list=FALSE)
training17 <- L17[ train17, ]
testing17 <- L17[ -train17, ]

set.seed(100)
train18 <- createDataPartition(L18$Status, p=0.8, list=FALSE)
training18 <- L18[ train18, ]
testing18 <- L18[ -train18, ]

set.seed(100)
train19 <- createDataPartition(L19$Status, p=0.8, list=FALSE)
training19 <- L19[ train19, ]
testing19 <- L19[ -train19, ]

set.seed(100)
train20 <- createDataPartition(L20$Status, p=0.8, list=FALSE)
training20 <- L20[ train20, ]
testing20 <- L20[ -train20, ]

set.seed(100)
train21 <- createDataPartition(L21$Status, p=0.8, list=FALSE)
training21 <- L21[ train21, ]
testing21 <- L21[ -train21, ]

set.seed(100)
train22 <- createDataPartition(L22$Status, p=0.8, list=FALSE)
training22 <- L22[ train22, ]
testing22 <- L22[ -train22, ]

set.seed(100)
train38 <- createDataPartition(L38$Status, p=0.8, list=FALSE)
training38 <- L38[ train38, ]
testing38 <- L38[ -train38, ]

set.seed(100)
train39 <- createDataPartition(L39$Status, p=0.8, list=FALSE)
training39 <- L39[ train39, ]
testing39 <- L39[ -train39, ]

set.seed(100)
train40 <- createDataPartition(L40$Status, p=0.8, list=FALSE)
training40 <- L40[ train40, ]
testing40 <- L40[ -train40, ]

set.seed(100)
train41 <- createDataPartition(L41$Status, p=0.8, list=FALSE)
training41 <- L41[ train41, ]
testing41 <- L41[ -train41, ]

set.seed(100)
train42 <- createDataPartition(L42$Status, p=0.8, list=FALSE)
training42 <- L42[ train42, ]
testing42 <- L42[ -train42, ]

set.seed(100)
train43 <- createDataPartition(L43$Status, p=0.8, list=FALSE)
training43 <- L43[ train43, ]
testing43 <- L43[ -train43, ]


#logistic regression models

x17<-L17[,c(9,10,11,12,14,16)]
cor(x17) #no multi-collinearity
rcorr(as.matrix(x17)) #credit score and LTV

x18<-L18[,c(9,10,11,12,14,16)]
cor(x18) #no multi-collinearity
rcorr(as.matrix(x18))#credit score and LTV

x19<-L19[,c(9,10,11,12,14,16)]
cor(x19) #no multi-collinearity
rcorr(as.matrix(x19)) #credit score and LTV

x20<-L20[,c(9,10,11,12,14,16)]
cor(x20) #no multi-collinearity
rcorr(as.matrix(x20)) #credit score and LTV

x21<-L21[,c(9,10,11,12,14,16)]
cor(x21) #no multi-collinearity
rcorr(as.matrix(x21)) #credit score and LTV

x22<-L22[,c(9,10,11,12,14,16)]
cor(x22) #no multi-collinearity
rcorr(as.matrix(x22)) #credit score and LTV

x38<-L38[,c(9,10,11,12,14,16)]
cor(x38) #no multi-collinearity
rcorr(as.matrix(x38)) #credit score and LTV

x39<-L39[,c(9,10,11,12,14,16)]
cor(x39) #no multi-collinearity
rcorr(as.matrix(x39)) #credit score and LTV

x40<-L40[,c(9,10,11,12,14,16)]
cor(x40) #no multi-collinearity
rcorr(as.matrix(x40)) #credit score and LTV

x41<-L41[,c(9,10,11,12,14,16)]
cor(x41) #no multi-collinearity
rcorr(as.matrix(x41)) #credit score and LTV

x42<-L42[,c(9,10,11,12,14,16)]
cor(x42) #no multi-collinearity
rcorr(as.matrix(x42)) #credit score and LTV

x43<-L43[,c(9,10,11,12,14,16)]
cor(x43) #no multi-collinearity
rcorr(as.matrix(x43)) #credit score and LTV


model17_glm<- train(data=training17,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model17_glm)
p17_glm<-predict(model17_glm, newdata = testing17)
confusionMatrix(p17_glm, testing17$Status,positive = "1")#84.56 37.78 100 

#Status~Gender+loan_type+loan_purpose+open_credit+rate_of_interest+income+LTV

model17_glm1<- train(data=training17,Status~Gender+loan_type+loan_purpose+rate_of_interest+income+LTV, method="glm", family="binomial")
summary(model17_glm1)
p17_glm1<-predict(model17_glm1, newdata = testing17)
confusionMatrix(p17_glm1, testing17$Status,positive = "1")#75.74 2.22 100

model18_glm<- train(data=training18,Status~Gender+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model18_glm)
p18_glm<-predict(model18_glm, newdata = testing18)
confusionMatrix(p18_glm, testing18$Status,positive = "1")#84.62 39.72 99.30


model18_glm1<- train(data=training18,Status~Gender+loan_type+loan_purpose+open_credit+rate_of_interest+property_value+income+credit_type+LTV, method="glm", family="binomial")
summary(model18_glm1)
p18_glm1<-predict(model18_glm1, newdata = testing18)
confusionMatrix(p18_glm1, testing18$Status,positive = "1")#84.97 39.71 99.76


model19_glm<- train(data=training19,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model19_glm)
p19_glm<-predict(model19_glm, newdata = testing19)
confusionMatrix(p19_glm, testing19$Status,positive = "1")#87.12 50.38 99.49

model19_glm1<- train(data=training19,Status~loan_purpose+Credit_Worthiness+loan_amount+rate_of_interest+income+LTV, method="glm", family="binomial")
summary(model19_glm1)
p19_glm1<-predict(model19_glm1, newdata = testing19)
confusionMatrix(p19_glm1, testing19$Status,positive = "1")#74.62 3.00 98.73


model20_glm<- train(data=training20,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model20_glm)
p20_glm<-predict(model20_glm, newdata = testing20)
confusionMatrix(p20_glm, testing20$Status,positive = "1")#86.41 47.83 99.28


model20_glm1<- train(data=training20,Status~loan_type+loan_purpose+rate_of_interest+income+age+LTV, method="glm", family="binomial")
summary(model20_glm1)
p20_glm1<-predict(model20_glm1, newdata = testing20)
confusionMatrix(p20_glm1, testing20$Status,positive = '1') #74.46 2.17  98.55


model21_glm<- train(data=training21,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model21_glm)
p21_glm<-predict(model21_glm, newdata = testing21)
confusionMatrix(p21_glm, testing21$Status,positive = "1")#84.56 37.77 100

model21_glm1<- train(data=training21,Status~Gender+loan_type+loan_purpose+rate_of_interest+income+LTV, method="glm", family="binomial")
summary(model21_glm1)
p21_glm1<-predict(model21_glm1, newdata = testing21)
confusionMatrix(p21_glm1, testing21$Status,positive = '1')#75.74  2.22 100

model22_glm<- train(data=training22,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model22_glm)
p22_glm<-predict(model22_glm, newdata = testing22)
confusionMatrix(p22_glm, testing22$Status,positive = "1")#84.97 40.42 99.53

model22_glm1<- train(data=training22,Status~Gender+loan_type+loan_purpose+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+LTV, method="glm", family="binomial")
summary(model22_glm1)
p22_glm1<-predict(model22_glm1, newdata = testing22)
confusionMatrix(p22_glm1, testing22$Status,positive = '1')#84.97 39.71  99.76


model38_glm<- train(data=training38,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model38_glm)
p38_glm<-predict(model38_glm, newdata = testing38)
confusionMatrix(p38_glm, testing38$Status,positive = "1")#84.01 37.77 99.26


model38_glm1<- train(data=training38,Status~Gender+rate_of_interest+income+LTV, method="glm", family="binomial")
summary(model38_glm1)
p38_glm1<-predict(model38_glm1, newdata = testing38)
confusionMatrix(p38_glm1, testing38$Status,positive = '1')#75.37 0.74 100

model39_glm<- train(data=training39,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model39_glm)
p39_glm<-predict(model39_glm, newdata = testing39)
confusionMatrix(p39_glm, testing39$Status,positive = "1")#84.44 39.00 99.30


model39_glm1<- train(data=training39,Status~Gender+loan_type+loan_purpose+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+LTV, method="glm", family="binomial")
summary(model39_glm1)
p39_glm1<-predict(model39_glm1, newdata = testing39)
confusionMatrix(p39_glm1, testing39$Status,positive = '1') #84.44 39.00 99.30

model40_glm<- train(data=training40,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model40_glm)
p40_glm<-predict(model40_glm, newdata = testing40)
confusionMatrix(p40_glm, testing40$Status,positive = "1")#87.12 50.38 99.49


model40_glm1<- train(data=training40,Status~Gender+loan_purpose+Credit_Worthiness+income+LTV, method="glm", family="binomial")
summary(model40_glm1)
p40_glm1<-predict(model40_glm1, newdata = testing40)
confusionMatrix(p40_glm1, testing40$Status,positive = '1') #74.05 1.50  98.48

model41_glm<- train(data=training41,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model41_glm)
p41_glm<-predict(model41_glm, newdata = testing41)
confusionMatrix(p41_glm, testing41$Status,positive = "1")#86.59 47.83 99.52


model41_glm1<- train(data=training41,Status~Gender+loan_type+rate_of_interest+income+age+LTV, method="glm", family="binomial")
summary(model41_glm1)
p41_glm1<-predict(model41_glm1, newdata = testing41)
confusionMatrix(p41_glm1, testing41$Status,positive = '1') #74.46 1.44  98.79

model42_glm<- train(data=training42,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model42_glm)
p42_glm<-predict(model42_glm, newdata = testing42)
confusionMatrix(p42_glm, testing42$Status,positive = "1")#83.82 37.03 99.26


model42_glm1<- train(data=training42,Status~Gender+rate_of_interest+income+LTV, method="glm", family="binomial")
summary(model42_glm1)
p42_glm1<-predict(model42_glm1, newdata = testing42)
confusionMatrix(p42_glm1, testing42$Status,positive = '1') #75.37 0.74 100

model43_glm<- train(data=training43,Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, method="glm", family="binomial")
summary(model43_glm)
p43_glm<-predict(model43_glm, newdata = testing43)
confusionMatrix(p43_glm, testing43$Status,positive = "1")#84.62 39.00 99.53


model43_glm1<- train(data=training43,Status~Gender+loan_type+loan_purpose+loan_amount+rate_of_interest+property_value+income+credit_type+LTV, method="glm", family="binomial")
summary(model43_glm1)
p43_glm1<-predict(model43_glm1, newdata = testing43)
confusionMatrix(p43_glm1, testing43$Status,positive = '1')#85.14 39.72 100


#naive byes models

modelL17_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training17, method="naive_bayes")
pL17_NBLA<-predict(modelL17_NBLA, newdata = testing17)
confusionMatrix(pL17_NBLA, testing17$Status,positive = "1")

modelL17_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training17, method="naive_bayes")
pL17_NBPV<-predict(modelL17_NBPV, newdata = testing17)
confusionMatrix(pL17_NBPV, testing17$Status,positive = "1")


modelL18_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training18, method="naive_bayes")
pL18_NBLA<-predict(modelL18_NBLA, newdata = testing18)
confusionMatrix(pL18_NBLA, testing18$Status,positive = "1")

modelL18_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training18, method="naive_bayes")
pL18_NBPV<-predict(modelL18_NBPV, newdata = testing18)
confusionMatrix(pL18_NBPV, testing18$Status,positive = "1")

modelL19_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training19, method="naive_bayes")
pL19_NBLA<-predict(modelL19_NBLA, newdata = testing19)
confusionMatrix(pL19_NBLA, testing19$Status,positive = "1")

modelL19_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training19, method="naive_bayes")
pL19_NBPV<-predict(modelL19_NBPV, newdata = testing19)
confusionMatrix(pL19_NBPV, testing19$Status,positive = "1")


modelL20_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training20, method="naive_bayes")
pL20_NBLA<-predict(modelL20_NBLA, newdata = testing20)
confusionMatrix(pL20_NBLA, testing20$Status,positive = "1")

modelL20_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training20, method="naive_bayes")
pL20_NBPV<-predict(modelL20_NBPV, newdata = testing20)
confusionMatrix(pL20_NBPV, testing20$Status,positive = "1")

modelL21_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training21, method="naive_bayes")
pL21_NBLA<-predict(modelL21_NBLA, newdata = testing21)
confusionMatrix(pL21_NBLA, testing21$Status,positive = "1")

modelL21_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training21, method="naive_bayes")
pL21_NBPV<-predict(modelL21_NBPV, newdata = testing21)
confusionMatrix(pL21_NBPV, testing21$Status,positive = "1")

modelL22_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training22, method="naive_bayes")
pL22_NBLA<-predict(modelL22_NBLA, newdata = testing22)
confusionMatrix(pL22_NBLA, testing22$Status,positive = "1")

modelL22_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                       data=training22, method="naive_bayes")
pL22_NBPV<-predict(modelL22_NBPV, newdata = testing22)
confusionMatrix(pL22_NBPV, testing22$Status,positive = "1")

model38_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training38, method="naive_bayes")
p38_NBLA<-predict(model38_NBLA, newdata = testing38)
confusionMatrix(p38_NBLA, testing38$Status,positive = "1")

model38_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training38, method="naive_bayes")
p38_NBPV<-predict(model38_NBPV, newdata = testing38)
confusionMatrix(p38_NBPV, testing38$Status,positive = "1")


model39_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training39, method="naive_bayes")
p39_NBLA<-predict(model39_NBLA, newdata = testing39)
confusionMatrix(p39_NBLA, testing39$Status,positive = "1")

model39_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training39, method="naive_bayes")
p39_NBPV<-predict(model39_NBPV, newdata = testing39)
confusionMatrix(p39_NBPV, testing39$Status,positive = "1")


model40_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training40, method="naive_bayes")
p40_NBLA<-predict(model40_NBLA, newdata = testing40)
confusionMatrix(p40_NBLA, testing40$Status,positive = "1")

model40_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training40, method="naive_bayes")
p40_NBPV<-predict(model40_NBPV, newdata = testing40)
confusionMatrix(p40_NBPV, testing40$Status,positive = "1")


model41_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training41, method="naive_bayes")
p41_NBLA<-predict(model41_NBLA, newdata = testing41)
confusionMatrix(p41_NBLA, testing41$Status,positive = "1")

model41_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training41, method="naive_bayes")
p41_NBPV<-predict(model41_NBPV, newdata = testing41)
confusionMatrix(p41_NBPV, testing41$Status,positive = "1")


model42_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training42, method="naive_bayes")
p42_NBLA<-predict(model42_NBLA, newdata = testing42)
confusionMatrix(p42_NBLA, testing42$Status,positive = "1")

model42_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training42, method="naive_bayes")
p42_NBPV<-predict(model42_NBPV, newdata = testing42)
confusionMatrix(p42_NBPV, testing42$Status,positive = "1")

model43_NBLA <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training43, method="naive_bayes")
p43_NBLA<-predict(model43_NBLA, newdata = testing43)
confusionMatrix(p43_NBLA, testing43$Status,positive = "1")

model43_NBPV <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training43, method="naive_bayes")
p43_NBPV<-predict(model43_NBPV, newdata = testing43)
confusionMatrix(p43_NBPV, testing43$Status,positive = "1")



#Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,


#decision tree Gini



Model17_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training17, method="rpart")
p17_gini<-predict(Model17_gini, newdata = testing17)
confusionMatrix(p17_gini, testing17$Status,positive = "1")#89.52 100 86.06
rpart.plot(Model17_gini$finalModel)


Model18_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training18, method="rpart")
p18_gini<-predict(Model18_gini, newdata = testing18)
confusionMatrix(p18_gini, testing18$Status,positive = "1")#87.59 100 83.53
rpart.plot(Model18_gini$finalModel)


Model19_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training19, method="rpart")
p19_gini<-predict(Model19_gini, newdata = testing19)
confusionMatrix(p19_gini, testing19$Status,positive = "1")#86.74 100 82.28
rpart.plot(Model19_gini$finalModel)


Model20_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training20, method="rpart")
p20_gini<-predict(Model20_gini, newdata = testing20)
confusionMatrix(p20_gini, testing20$Status,positive = "1")#89.67 100 86.23
rpart.plot(Model20_gini$finalModel)



Model21_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training21, method="rpart")
p21_gini<-predict(Model21_gini, newdata = testing21)
confusionMatrix(p21_gini, testing21$Status,positive = "1")#89.52 100 86.06
rpart.plot(Model21_gini$finalModel)



Model22_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training22, method="rpart")
p22_gini<-predict(Model22_gini, newdata = testing22)
confusionMatrix(p22_gini, testing22$Status,positive = "1")#87.59 100  83.53
rpart.plot(Model22_gini$finalModel)



Model38_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training38, method="rpart")
p38_gini<-predict(Model38_gini, newdata = testing38)
confusionMatrix(p38_gini, testing38$Status,positive = "1")#86.58 65.93 93.40
rpart.plot(Model38_gini$finalModel)


Model39_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training39, method="rpart")
p39_gini<-predict(Model39_gini, newdata = testing39)
confusionMatrix(p39_gini, testing39$Status,positive = "1")#86.89 68.09 93.04
rpart.plot(Model39_gini$finalModel)


Model40_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training40, method="rpart")
p40_gini<-predict(Model40_gini, newdata = testing40)
confusionMatrix(p40_gini, testing40$Status,positive = "1")#88.83 75.19 93.42
rpart.plot(Model40_gini$finalModel)


Model41_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training41, method="rpart")
p41_gini<-predict(Model41_gini, newdata = testing41)
confusionMatrix(p41_gini, testing41$Status,positive = "1")#89.13 79.71 92.27
rpart.plot(Model41_gini$finalModel)


Model42_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training42, method="rpart")
p42_gini<-predict(Model42_gini, newdata = testing42)
confusionMatrix(p42_gini, testing42$Status,positive = "1")#86.58 65.93 93.40
rpart.plot(Model42_gini$finalModel)


Model43_gini <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training43, method="rpart")
p43_gini<-predict(Model43_gini, newdata = testing43)
confusionMatrix(p43_gini, testing43$Status,positive = "1")#86.89 68.09 93.04
rpart.plot(Model43_gini$finalModel)



#decision tree info



Model17_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training17, method="rpart", parms = list(split = "information"))
p17_info<-predict(Model17_info, newdata = testing17)
confusionMatrix(p17_info, testing17$Status,positive = "1")#89.52 100 86.06
rpart.plot(Model17_info$finalModel)


Model18_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training18, method="rpart", parms = list(split = "information"))
p18_info<-predict(Model18_info, newdata = testing18)
confusionMatrix(p18_info, testing18$Status,positive = "1")#86.89 83.69 87.94
rpart.plot(Model18_info$finalModel)


Model19_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training19, method="rpart", parms = list(split = "information"))
p19_info<-predict(Model19_info, newdata = testing19)
confusionMatrix(p19_info, testing19$Status,positive = "1")#90.15 92.48 89.37
rpart.plot(Model19_info$finalModel)


Model20_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training20, method="rpart", parms = list(split = "information"))
p20_info<-predict(Model20_info, newdata = testing20)
confusionMatrix(p20_info, testing20$Status,positive = "1")#89.67 100 86.23
rpart.plot(Model20_info$finalModel)



Model21_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training21, method="rpart", parms = list(split = "information"))
p21_info<-predict(Model21_info, newdata = testing21)
confusionMatrix(p21_info, testing21$Status,positive = "1")#89.52 100  86.06
rpart.plot(Model21_info$finalModel)



Model22_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training22, method="rpart", parms = list(split = "information"))
p22_info<-predict(Model22_info, newdata = testing22)
confusionMatrix(p22_info, testing22$Status,positive = "1")#86.89 83.69 87.94
rpart.plot(Model22_info$finalModel)



Model38_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training38, method="rpart", parms = list(split = "information"))
p38_info<-predict(Model38_info, newdata = testing38)
confusionMatrix(p38_info, testing38$Status,positive = "1")#86.58 65.93 93.40
rpart.plot(Model38_info$finalModel)


Model39_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training39, method="rpart", parms = list(split = "information"))
p39_info<-predict(Model39_info, newdata = testing39)
confusionMatrix(p39_info, testing39$Status,positive = "1")#87.06 61.70 95.36
rpart.plot(Model39_info$finalModel)


Model40_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training40, method="rpart", parms = list(split = "information"))
p40_info<-predict(Model40_info, newdata = testing40)
confusionMatrix(p40_info, testing40$Status,positive = "1")#88.83 75.19 93.42
rpart.plot(Model40_info$finalModel)


Model41_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training41, method="rpart", parms = list(split = "information"))
p41_info<-predict(Model41_info, newdata = testing41)
confusionMatrix(p41_info, testing41$Status,positive = "1")#89.13 78.99 92.51
rpart.plot(Model41_info$finalModel)


Model42_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training42, method="rpart", parms = list(split = "information"))
p42_info<-predict(Model42_info, newdata = testing42)
confusionMatrix(p42_info, testing42$Status,positive = "1")#86.58 65.93   93.40
rpart.plot(Model42_info$finalModel)


Model43_info <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                      data=training43, method="rpart", parms = list(split = "information"))
p43_info<-predict(Model43_info, newdata = testing43)
confusionMatrix(p43_info, testing43$Status,positive = "1")#87.06 61.70 95.36
rpart.plot(Model43_info$finalModel)






#Random Forest



Model17_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training17, method="rf")
p17_rf<-predict(Model17_rf, newdata = testing17)
confusionMatrix(p17_rf, testing17$Status,positive = "1")#89.89 81.48 92.67
Model17_rf
Model17_rf$finalModel
varImp(Model17_rf)


Model18_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training18, method="rf")
p18_rf<-predict(Model18_rf, newdata = testing18)
confusionMatrix(p18_rf, testing18$Status,positive = "1")#88.81 80.85 91.42
Model18_rf
Model18_rf$finalModel
varImp(Model18_rf)


Model19_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training19, method="rf")
p19_rf<-predict(Model19_rf, newdata = testing19)
confusionMatrix(p19_rf, testing19$Status,positive = "1")#90.91 88.72 91.65
Model19_rf
Model19_rf$finalModel
varImp(Model19_rf)


Model20_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training20, method="rf")
p20_rf<-predict(Model20_rf, newdata = testing20)
confusionMatrix(p20_rf, testing20$Status,positive = "1")#91.67 87.68 93.00
Model20_rf
Model20_rf$finalModel
varImp(Model20_rf)



Model21_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training21, method="rf")
p21_rf<-predict(Model21_rf, newdata = testing21)
confusionMatrix(p21_rf, testing21$Status,positive = "1")#90.81 81.48 93.89
Model21_rf
Model21_rf$finalModel
varImp(Model21_rf)



Model22_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training22, method="rf")
p22_rf<-predict(Model22_rf, newdata = testing22)
confusionMatrix(p22_rf, testing22$Status,positive = "1")#89.51 81.56 92.11
Model22_rf
Model22_rf$finalModel
varImp(Model22_rf)



Model38_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training38, method="rf")
p38_rf<-predict(Model38_rf, newdata = testing38)
confusionMatrix(p38_rf, testing38$Status,positive = "1")#88.05 81.56  93.64
Model38_rf
Model38_rf$finalModel
varImp(Model38_rf)


Model39_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training39, method="rf")
p39_rf<-predict(Model39_rf, newdata = testing39)
confusionMatrix(p39_rf, testing39$Status,positive = "1")#88.11 74.47 92.58
Model39_rf
Model39_rf$finalModel
varImp(Model39_rf)


Model40_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training40, method="rf")
p40_rf<-predict(Model40_rf, newdata = testing40)
confusionMatrix(p40_rf, testing40$Status,positive = "1")#89.93 80.45 92.41
Model40_rf
Model40_rf$finalModel
varImp(Model40_rf)


Model41_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training41, method="rf")
p41_rf<-predict(Model41_rf, newdata = testing41)
confusionMatrix(p41_rf, testing41$Status,positive = "1")#91.3 84.78 93.48
Model41_rf
Model41_rf$finalModel
varImp(Mode41_rf)


Model42_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training42, method="rf")
p42_rf<-predict(Model42_rf, newdata = testing42)
confusionMatrix(p42_rf, testing42$Status,positive = "1")#88.6 71.85 94.13
Model42_rf
Model42_rf$finalModel
varImp(Model42_rf)


Model43_rf <- train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+business_or_commercial+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region,
                    data=training43, method="rf")
p43_rf<-predict(Model43_rf, newdata = testing43)
confusionMatrix(p43_rf, testing43$Status,positive = "1")#88.11 75.89 92.11
Model43_rf
Model43_rf$finalModel
varImp(Model43_rf)