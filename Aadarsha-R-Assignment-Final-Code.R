#Aadarsha Final Code for R assessment
#########################################################################################################
#########################################################################################################
#########################################################################################################
#Importing Datasets
#Here I am defining my X variables as ax and Y variables as by as provided for the module assesment
#X-Dataset
ax = as.matrix(read.csv(
  file = "E:/Softwarica/Masters/Introduction-to-Statistical-Methods/Assisnment/dataset-X.csv",header=F))
colnames(ax)<-c("aX1","aX2","aX3","aX4")
#########################################################################################################
#Y-dataset
by=as.matrix(read.csv(
  file="E:/Softwarica/Masters/Introduction-to-Statistical-Methods/Assisnment/dataset-y.csv",header = F))
colnames(by)<-c("bY")
#########################################################################################################
#Time-dataset
Time = read.csv(
  file="E:/Softwarica/Masters/Introduction-to-Statistical-Methods/Assisnment/dataset-time-series.csv", header = F, skip = 1)
Time = as.matrix(rbind(0, Time))
#########################################################################################################
#########################################################################################################
#########################################################################################################
#Task-1-1
#Defining the values for ax and by (i.e. X and Y) against time for time-series plot
ax.ts<-ts(ax,start = c(min(Time),max(Time)),frequency =1)
by.ts<-ts(by,start = c(min(Time),max(Time)),frequency =1)
#########################################################################################################
#Plotting timeseries graph
plot(ax.ts,main = "Time series plot of a-X Signal", xlab = "Time", ylab = "Input signal",col="darkred")
plot(by.ts,main = "Time series plot of b-Y Signal", xlab = "Time", ylab = "Output signal",col="limegreen")

#########################################################################################################
#########################################################################################################
#########################################################################################################
#Task-1
#Creating Density Plot of ax (i.e X) signal
dis_ax=density(ax)
plot(dis_ax,main = "Density plot of whole ax input signal",col="darkred")
hist(ax,freq = FALSE,main = "Histogram and Density Plot of ax-Signal",xlab="ax-Signal")
lines(dis_ax,lwd=3,col="limegreen")
rug(jitter(ax))
#########################################################################################################
#Creating Density Plot of aX1 (from X-dataset) signal
dis_ax1=density(ax[,"aX1"])
hist(ax[,"aX1"],freq = FALSE,main = "Histogram and density plot of aX1",xlab = "aX1 Signal")
lines(dis_ax1,lwd=3,col="limegreen")
rug(jitter(ax[,"aX1"]))
#########################################################################################################
#Creating Density Plot of aX2 (from X-dataset) signal
dis_ax2=density(ax[,"aX2"])
hist(ax[,"aX2"],freq = FALSE,main = "Histogram and density plot of aX2",xlab = "aX2 Signal")
lines(dis_ax2,lwd=3,col="limegreen")
rug(jitter(ax[,"aX2"]))
#########################################################################################################
#Creating Density Plot of aX3 (from X-dataset) signal
dis_ax3=density(ax[,"aX3"])
hist(ax[,"aX3"],freq = FALSE,main = "Histogram and density plot of aX3",xlab = "aX3 Signal")
lines(dis_ax3,lwd=3,col="limegreen")
rug(jitter(ax[,"aX3"]))
#########################################################################################################
#Creating Density Plot of aX4 (from X-dataset) signal
dis_ax4=density(ax[,"aX4"])
hist(ax[,"aX4"],freq = FALSE,main = "Histogram and density plot of aX4",xlab = "aX4 Signal")
lines(dis_ax4,lwd=3,col="limegreen")
rug(jitter(ax[,"aX4"]))
#########################################################################################################
#Creating Density Plot of by (i.e Y) signal
dis_by=density(by)
plot(dis_by,main = "Density plot of by",xlab = "Output Signal",col="darkred")
hist(by,freq = FALSE,main = "Histogram and density plot of by",xlab = "Output Signal")
lines(dis_by,lwd=3,col="limegreen")
rug(jitter(by))

###################################################################################################
##Scatter-Plots
#Plotting ax1 against by
plot(ax[,"aX1"],by,main = "Correlation betweeen aX1 and bY signal", xlab = "aX1 signal", ylab = "Output signal",col="limegreen" )
# Plotting ax2 against by
plot(ax[,"aX2"],by,main = "Correlation betweeen aX2 and bY signal", xlab = "aX2 signal", ylab = "Output signal",col="limegreen")
# Plotting ax3 against by
plot(ax[,"aX3"],by,main = "Correlation betweeen aX3 and bY signal", xlab = "aX3 signal", ylab = "Output signal",col="limegreen")
# Plotting ax4 against by
plot(ax[,"aX4"],by,main = "Correlation betweeen aX4 and bY signal", xlab = "aX4 signal", ylab = "Output signal",col="limegreen")
#########################################################################################################
#########################################################################################################
#########################################################################################################
#Task-2
# Calculating ones for binding the data
aones = matrix(1 , length(ax)/4,1)
aones
#Task-2.1
#Model - 1
#Binding data from equation of model 1.
ax_Model1<-cbind(aones,(ax[,"aX4"]),(ax[,"aX1"]^2),(ax[,"aX1"])^3,(ax[,"aX2"])^4,(ax[,"aX1"])^4)
ax_Model1
#Calculating thetahat of model 1
aModel1_thetahat=solve(t(ax_Model1) %*% ax_Model1) %*% t(ax_Model1) %*% by
aModel1_thetahat
#########################################################################################################
#Model-2
#Binding data from equation of model 2.
ax_Model2<-cbind(aones,(ax[,"aX4"]),(ax[,"aX1"])^3,(ax[,"aX3"])^4)
ax_Model2
#Calculating thetahat of Model 2
aModel2_thetahat=solve(t(ax_Model2) %*% ax_Model2) %*% t(ax_Model2) %*% by
aModel2_thetahat
#########################################################################################################
#Model-3
#Binding data from equation of model 3.
ax_Model3<-cbind(aones,(ax[,"aX3"])^3,(ax[,"aX3"])^4)
ax_Model3
#Calculating thetahat of Model 3
aModel3_thetahat=solve(t(ax_Model3) %*% ax_Model3) %*% t(ax_Model3) %*% by
aModel3_thetahat
#########################################################################################################
#Model-4
#Binding data from equation of model 4.
ax_Model4<-cbind(aones,ax[,"aX2"],ax[,"aX1"]^3,ax[,"aX3"]^4)
ax_Model4
#Calculating thetahat of Model 4
aModel4_thetahat=solve(t(ax_Model4) %*% ax_Model4) %*% t(ax_Model4) %*% by
aModel4_thetahat
#########################################################################################################
#Model-5
#Binding data from equation of model 5.
ax_Model5<-cbind(aones,ax[,"aX4"],ax[,"aX1"]^2,ax[,"aX1"]^3,ax[,"aX3"]^4)
ax_Model5
#Calculating thetahat of Model 5
aModel5_thetahat=solve(t(ax_Model5) %*% ax_Model5) %*% t(ax_Model5) %*% by
aModel5_thetahat
#########################################################################################################
#########################################################################################################
#Task-2.2
#Calculating Y-hat and RSS Model 1
by_hat_m1 = ax_Model1 %*% aModel1_thetahat
by_hat_m1
#Calculating RSS
ab_RSS_Model_1=sum((by-by_hat_m1)^2)
ab_RSS_Model_1
#########################################################################################################
#Calculating Y-hat and RSS of model 2
by_hat_m2 = ax_Model2 %*% aModel2_thetahat
by_hat_m2
#Calculating RSS
ab_RSS_Model_2=sum((by-by_hat_m2)^2)
ab_RSS_Model_2
#########################################################################################################
#Calculating Y-hat and RSS of model 3
by_hat_m3 = ax_Model3 %*% aModel3_thetahat
by_hat_m3
#Calculating RSS
ab_RSS_Model_3=sum((by-by_hat_m3)^2)
ab_RSS_Model_3
#########################################################################################################
#Calculating Y-hat and RSS of model 4
by_hat_m4 = ax_Model4 %*% aModel4_thetahat
by_hat_m4
#Calculating RSS
ab_RSS_Model_4=sum((by-by_hat_m4)^2)
ab_RSS_Model_4
#########################################################################################################
#Calculating Y-hat and RSS of model 5
by_hat_m5 = ax_Model5 %*% aModel5_thetahat
by_hat_m5
#Calculating RSS
ab_RSS_Model_5=sum((by-by_hat_m5)^2)
ab_RSS_Model_5
#########################################################################################################
#########################################################################################################
#Task-2.3
#Likelihood and Variance
N=length(by)
#Calculating the Variance of Model 1
ab_Variance_Model1=ab_RSS_Model_1/(N-1)
ab_Variance_Model1
#########################################################################################################
#Calculating the log-likelihood of Model 1
ab_Likelihood_Model_1=-(N/2)*(log(2*pi))-(N/2)*(log(ab_Variance_Model1))-(1/(2*ab_Variance_Model1))*ab_RSS_Model_1
ab_Likelihood_Model_1
#########################################################################################################
#Calculating the Variance of Model 2
ab_Variance_Model2=ab_RSS_Model_2/(N-1)
ab_Variance_Model2
#########################################################################################################
#Calculating the log-likelihood of Model 2
ab_Likelihood_Model_2=-(N/2)*(log(2*pi))-(N/2)*(log(ab_Variance_Model2))-(1/(2*ab_Variance_Model2))*ab_RSS_Model_2
ab_Likelihood_Model_2
#########################################################################################################
#Calculating the Variance of Model 3
ab_Variance_Model3=ab_RSS_Model_3/(N-1)
ab_Variance_Model3
#########################################################################################################
#Calculating the log-likelihood of Model 3
ab_Likelihood_Model_3= -(N/2)*(log(2*pi))-(N/2)*(log(ab_Variance_Model3))-(1/(2*ab_Variance_Model3))*ab_RSS_Model_3
ab_Likelihood_Model_3
#########################################################################################################
#Calculating the Variance of Model 4
ab_Variance_Model4=ab_RSS_Model_4/(N-1)
ab_Variance_Model4
#########################################################################################################
#Calculating the log-likelihood of Model 4
ab_Likelihood_Model_4= -(N/2)*(log(2*pi))-(N/2)*(log(ab_Variance_Model4))-(1/(2*ab_Variance_Model4))*ab_RSS_Model_4
ab_Likelihood_Model_4
#########################################################################################################
#Calculating the Variance of Model 5
ab_Variance_Model5=ab_RSS_Model_5/(N-1)
ab_Variance_Model5
#########################################################################################################
#Calculating the log-likelihood of Model 5
ab_Likelihood_Model_5= -(N/2)*(log(2*pi))-(N/2)*(log(ab_Variance_Model5))-(1/(2*ab_Variance_Model5))*ab_RSS_Model_5
ab_Likelihood_Model_5
#########################################################################################################
#########################################################################################################
#Task-2.4
#Calculating AIC and BIC for Model-1
ab_K_Model1<-length(aModel1_thetahat)
ab_K_Model1
ab_AIC_Model1=2*ab_K_Model1-2*ab_Likelihood_Model_1
ab_AIC_Model1
ab_BIC_Model1=ab_K_Model1*log(N)-2*ab_Likelihood_Model_1
ab_BIC_Model1
#########################################################################################################
#Calculating AIC and BIC for Model-2
ab_K_Model2<-length(aModel2_thetahat)
ab_K_Model2
ab_AIC_Model2=2*ab_K_Model2-2*ab_Likelihood_Model_2
ab_AIC_Model2
ab_BIC_Model2=ab_K_Model2*log(N)-2*ab_Likelihood_Model_2
ab_BIC_Model2
#########################################################################################################
#Calculating AIC and BIC for Model-3
ab_K_Model3<-length(aModel3_thetahat)
ab_K_Model3
ab_AIC_Model3=2*ab_K_Model3-2*ab_Likelihood_Model_3
ab_AIC_Model3
ab_BIC_Model3=ab_K_Model3*log(N)-2*ab_Likelihood_Model_3
ab_BIC_Model3
#########################################################################################################
#Calculating AIC and BIC for Model-4
ab_K_Model4<-length(aModel1_thetahat)
ab_K_Model4
ab_AIC_Model4=2*ab_K_Model4-2*ab_Likelihood_Model_4
ab_AIC_Model4
ab_BIC_Model4=ab_K_Model4*log(N)-2*ab_Likelihood_Model_4
ab_BIC_Model4
#########################################################################################################
#Calculating AIC and BIC for Model-5
ab_K_Model5<-length(aModel5_thetahat)
ab_K_Model5
ab_AIC_Model5=2*ab_K_Model5-2*ab_Likelihood_Model_5
ab_AIC_Model5
ab_BIC_Model5=ab_K_Model5*log(N)-2*ab_Likelihood_Model_5
ab_BIC_Model5
#########################################################################################################
#########################################################################################################
#Task-2.5
#Error of Model-1
ab_Model1_error <- by-by_hat_m1
qqnorm(ab_Model1_error, col = "darkgreen",main = "QQ plot for Model-1")
qqline(ab_Model1_error, col = "darkred",lwd=1)
#########################################################################################################
#Error of Model-2
ab_Model2_error <- by-by_hat_m2 
qqnorm(ab_Model2_error, col = "darkgreen",main = "QQ plot for Model-2")
qqline(ab_Model2_error, col = "darkred")
#########################################################################################################
#Error of Model-3
ab_Model3_error <- by-by_hat_m3
qqnorm(ab_Model3_error, col = "darkgreen",main = "QQ plot for Model-3")
qqline(ab_Model3_error, col = "darkred")
#########################################################################################################
#Error of Model-4
ab_Model4_error <- by-by_hat_m4
qqnorm(ab_Model4_error, col = "darkgreen",main = "QQ plot for Model-4")
qqline(ab_Model4_error, col = "darkred")
#########################################################################################################
#Error of Model-5
ab_Model5_error <- by-by_hat_m5
qqnorm(ab_Model5_error, col = "darkgreen",main = "QQ plot for Model-5")
qqline(ab_Model5_error, col = "darkred")
#########################################################################################################
#########################################################################################################
#Task-2.7
#install package tidymodels
#Split-y
ab_split_y<-initial_split(data = as.data.frame(by),prop=.7)
by_training_set<-training(ab_split_y)
by_training_set
by_testing_set<-as.matrix(testing(ab_split_y))
by_testing_set
by_training_data<-as.matrix(by_training_set)
by_training_data
#########################################################################################################
#Split-x
ab_split_x<-initial_split(data = as.data.frame(ax),prop=.7)
ax_training_set<-training(ab_split_x)
ax_training_set
ax_testing_set<-as.matrix(testing(ab_split_x))
ax_testing_set
ax_testing_data<-as.matrix(ax_testing_set)
ax_testing_data
ax_training_data<-as.matrix(ax_training_set)
ax_training_data
#########################################################################################################
ax_training_one=matrix(1 , length(ax_training_set$aX2),1)
ax_training_model<-cbind(ax_training_one,ax_training_set[,"aX2"],(ax_training_set[,"aX1"])^3,(ax_training_set[
  ,"aX3"])^4)
ab_training_thetahat=solve(t(ax_training_model) %*% ax_training_model) %*% t(ax_training_model) %*% by_training_data
#Model out/Prediction
by_testing_hat = ax_testing_data %*% ab_training_thetahat
by_testing_hat
ab_RSS_testing=sum((by_testing_set-by_testing_hat)^2)
ab_RSS_testing
t.test(by_training_data, mu=500, alternative="two.sided", conf.level=0.95)
ab_C_I1=-0.2049779
ab_C_I2=0.4383936
ab_p2 <- plot(density(by_training_data), col="blue", lwd=2, main="Distribution of Training Data")
abline(v=ab_C_I1,col="red", lty=2)
abline(v=ab_C_I2,col="red", lty=2)
abline(v=0.27,col="darkgreen", lty=2)
ab_thetaHat_training =solve(t(ax_training_data) %*% ax_training_data) %*% t(ax_training_data) %*%
  by_training_data
ab_thetaHat_training
length(ab_thetaHat_training)
ab_dis_test=density(by_training_data)
plot((ab_dis_test))
plot(ab_dis_test,main = "Density plot of b-Y Signal")
##################################################################################################
ab_z=1.96 
ab_error=((by_testing_set-by_testing_hat))
ab_error
ab_n_len=length(by_testing_hat)
ab_n_len
C_I_1= ab_z*sqrt((ab_error*(1-ab_error))/ab_n_len)
C_I_1
C_I_2= ab_z*sqrt((ab_error*(1+ab_error))/ab_n_len)
C_I_2
##################################################################################################
#Error-Bar
ab_plot_data = data.frame(
  ab_x_Val = ax_Model2,
  ab_y_Val = by,
  ab_SD = sqrt(ab_Variance_Model2)  
  )

ab_plot<-ggplot(ab_plot_data) +
  geom_bar( aes(x=ab_x_Val.2, y=by), stat="identity", fill="lightgreen", alpha=0.7) +
  geom_errorbar( aes(x=ab_x_Val.2, ymin=by-ab_SD, ymax=by+ab_SD), width=0.2, colour="red", alpha=0.5, linewidth=1)

print (ab_plot)
##################################################################################################
##################################################################################################
##################################################################################################
##Task-3
ab_arr_1=0
ab_arr_2=0
ab_f_value=0
ab_s_value=0
aModel2_thetahat
#values from thetahat
ab_thetebias <- 0.483065688 
ab_thetaone <- 0.143578928 
ab_thetatwo <- 0.010038614 
ab_thetathree <- 0.001912836
Epison <- ab_RSS_Model_2 * 2 # assigning fixed value of eplision
num <- 100 #number of iteration
#Calculating Y-hat for performing rejection ABC
counter <- 0
for (i in 1:num) {
  range1 <- runif(1,-0.448299550,0.448299550) # calculating the range
  range1
  range2 <- runif(1,-0.038109255,0.038109255)
  ab_New_thetahat <- matrix(c(range1,range2,ab_thetatwo,ab_thetathree))
  ab_New_Y_Hat <- ax_Model2 %*% ab_New_thetahat
  ab_new_RSS <- sum((by - ab_New_Y_Hat)^2)
  ab_new_RSS
  if (ab_new_RSS > Epison){
    ab_arr_1[i] <- range1
    ab_arr_2[i] <- range2
    counter = counter+1
    ab_f_value <- matrix(ab_arr_1)
    ab_s_value <- matrix(ab_arr_2)
  }
}
hist(ab_f_value)
hist(ab_s_value)

plot(ab_f_value,ab_s_value, col = c("red", "blue"), main = "Joint and Marginal Posterior Distribution")

