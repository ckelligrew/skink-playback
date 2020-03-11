##############FINAL SKINK CODE##############################################################

##Import skinkData from online database before proceeding
skinkData<- read.csv()

##############INSTALL AND LOAD REQUIRED PACKAGES######################################################

install.packages("Cairo")
install.packages("MuMIn")
install.packages("rsq")
install.packages("emmeans")
install.packages("ggplot2")
install.packages("sjstats")

library(Cairo)
library(MuMIn)
library(rsq)
library(emmeans)
library(ggplot2)
library(sjstats)

#############################################################################################
#####SECTION 1: TESTING FOR COLLINEARITY OF COVARIATES AND TREATMENT #########################
###Purpose: Make sure no covariates have significant correlation with treatment
###Ensure significant results are due to treatment, not other factors

########ANOVA############
####Test for correlation between one numerical and one categorical variable #####
SDAnova <- aov(SD ~ treatment, data=skinkData)
summary(SDAnova) #no significant correlation between treatment and SD
sizecmAnova<- aov(sizecm ~ treatment, data=skinkData)
summary(sizecmAnova) #no significant correlation between size and treatment
conspecificsAnova<- aov(conspecificsIn1m ~ treatment, data=skinkData)
summary(conspecificsAnova) #no significant correlation between size and treatment
foliageAnova<- aov(percentFoliage ~ treatment, data=skinkData)
summary(foliageAnova) #no significant correlation between foliage and treatment

##########CHI-SQUARED#######
###Test for correlation between two categorical variables ####
ObsXsq <- chisq.test(x=skinkData$observer, y=skinkData$treatment)
ObsXsq$p.value #no significant correlation between observer and treatment
ObsXsq$observed
ObsXsq$expected
ObsXsq$residuals
LocXsq <- chisq.test(x=skinkData$location, y=skinkData$treatment)
LocXsq$p.value #no significant correlation between location and treatment 
LocXsq$observed
LocXsq$expected
LocXsq$residuals
PosXsq<- chisq.test(x=skinkData$initialPosition, y=skinkData$treatment)
PosXsq$p.value #no significant correlation between position and treatment
PosXsq$observed
PosXsq$expected
PosXsq$residuals
WindXsq<- chisq.test(x=skinkData$windBeaufort, y=skinkData$treatment)
WindXsq$p.value #no significant correlation between wind and treatment
WindXsq$observed
WindXsq$expected
WindXsq$residuals
TailXsq<- chisq.test(x=skinkData$tailColor, y=skinkData$treatment)
TailXsq$p.value #no significant correlation between tail color and treatment
TailXsq$observed
TailXsq$expected
TailXsq$residuals

###############################################################################
#####SECTION 2: RUNNING LINEAR MODELS FOR BEHAVIOR ############################
###Purpose: See effects of predictor variables on behavior and FID
###Run with and without observer to see which behaviors have observer effects 
###Include observer as predictor in models where behavior have observer effect in order to control for it 
###Include location in each model to control for location effects

#Need to use log10+1 transformed locomotion differences to achieve normal distribution
skinkData$log10LocomotionRateDiff<-log10(skinkData$locomotionRateDiff+1)

#Model for difference between post15s and pre treatment with observer
lookObslm<- lm(lookRateDiff ~  treatment + observer + SD + location, data=skinkData)
locomotionObslm<- lm(log10LocomotionRateDiff ~  treatment + observer + SD + location, data=skinkData)
summary(lookObslm) #significant effects: white noise more looking, observer effect: CK less looking, pineapple plantation more looking
summary(locomotionObslm) #significant effect: white noise more locomotion

#Model for difference between post15s-pre treatment without observer
looklm<- lm(lookRateDiff ~  treatment + SD + location, data=skinkData)
locomotionlm<- lm(log10LocomotionRateDiff ~  treatment + SD + location, data=skinkData)
summary(looklm) #significant effects: white noise more looking, pineapple plantation and road more looking
summary(locomotionlm) #significant effect: white noise more locomotion

#############################################################################
#####SECTION 3: CHECKING BEHAVIOR RESIDUALS##################################
###Purpose: Check how well the model predicts observed data
###Make sure the observed behavior data is evenly distributed around the linear regression

#Residuals from looking model
looklm<- lm(lookRateDiff ~  treatment + observer + SD + location, data=skinkData)
lookResid<- residuals(looklm)
hist(lookResid) 
qqnorm(lookResid) 
fittedLook<- fitted(looklm)
plot(fittedLook, lookResid) 

#Residuals from locomotion model
#Need to use log10+1 transformed locomotion differences because normal distribution required 
locomotionlm<-lm(log10LocomotionRateDiff ~ treatment + SD + location, data=skinkData)
summary(locomotionlm)
locomotionResid<- residuals(locomotionlm)
hist(locomotionResid) 
qqnorm(locomotionResid)
fittedLocomotion<- fitted(locomotionlm)
plot(fittedLocomotion, locomotionResid) 

#############################################################################
#####SECTION 4: FINAL BEHAVIOR MODELS##################################
###Purpose: Easy accessibility

#Looking Model
looklm<- lm(lookRateDiff ~  treatment + observer + SD + location, data=skinkData)
summary(looklm)

#Locomotion Model 
#Need to use log10+1 transformed locomotion diffs because normal distribution required
skinkData$log10LocomotionRateDiff<-log10(skinkData$locomotionRateDiff+1)
locomotionlm<-lm(log10LocomotionRateDiff ~ treatment + SD + location, data=skinkData)
summary(locomotionlm)

######################################################################
##########SECTION 5: BEHAVIOR EMMEANS ################################
###Purpose: Compare estimated marginal means between silence and white noise for our behavior models 

#Looking emmeans comparison
lookEmmean<- emmeans(looklm, "treatment")
summary(lookEmmean)
pairLookEmmean<- pairs(lookEmmean) #pairwise comparison 
summary(pairLookEmmean) #significant difference between treatment emmeans 
eff_size(lookEmmean, sigma = sigma(looklm), edf = 114) #cohen's d size effect #medium to large size effect

#Locomotion emmeans comparison 
locomotionEmmean<- emmeans(locomotionlm, "treatment")
summary(locomotionEmmean)
pairLocomotionEmmean<- pairs(locomotionEmmean)
summary(pairLocomotionEmmean) #significant difference between treatment emmeans 
eff_size(locomotionEmmean, sigma=sigma(locomotionlm), edf=114) #cohen's d size effect #small to medium size effect 


####################################################################
#####SECTION 6: FID MODEL AND RESIDUALS########################
###Purpose: Explore predictor variable effects on FID 
###Explore how well model explains observed FID data 

#Check looking and locomotion residuals aren't correlated so we can add both to model
skinkData$lookResid<- lookResid
skinkData$locoResid<- locomotionResid
cor.test(skinkData$lookResid, skinkData$locoResid) #not correlated so they can both be in the model

#FID model
FIDlm<- lm(FID ~ lookResid + locoResid + treatment + SD + treatment*SD + location, data=skinkData)
summary(FIDlm) #significant effect look resid higher FID #significant effect SD higher FID

#FID residuals
FIDResid<- residuals(FIDlm)
summary(FIDResid)
hist(FIDResid) 
qqnorm(FIDResid)
fittedFID<- fitted(FIDlm)
plot(fittedFID, FIDResid)

###################################################################################
#####SECTION 7: R2 OF MODELS AND PARTIAL R2 OF PREDICTOR VARIABLES########
###Purpose: Find strength of models and predictors 

#Looking model R2 and partial R2 values
looklm<- lm(lookRateDiff ~  treatment + observer + SD + location, data=skinkData)
rsq(looklm) #model explains 22.59% observed variation 
rsq.partial(looklm)

#Locomotion model R2 and partial R2 values
locomotionlm<-lm(log10LocomotionRateDiff ~ treatment + SD + location, data=skinkData)
rsq(locomotionlm) #model explains 8.80% observed variation
rsq.partial(locomotionlm)

#FID model R2 and eta-squared values
FIDlm<- lm(FID ~ lookResid + locoResid + treatment + SD + treatment*SD + location, data=skinkData)
rsq(FIDlm) #model explains 28.75% observed variation
FIDlmAnova<- anova(FIDlm) #create anova to calculate eta-squared values
eta_sq(FIDlm)

#############################################################################################
#####SECTION 8: GRAPHS#######################################################################
##Using ggplot2 for bar graphs

#Create looking dataframe
lookData <- data.frame(
  treatment=c("Silent", "White noise"),
  difference=c(-0.0223, 0.1155),
  lookSE=c(0.0295,0.0293) #got SE from summary(lookEmmean)
  
)

#Looking barplot
lookPlot <- ggplot(lookData, aes(x=treatment, y=difference)) +
  geom_bar(stat = "identity",
           color="black",
           fill="white") + 
  theme_classic() +
  geom_errorbar(aes(x=treatment, ymin=difference-lookSE, ymax=difference+lookSE),
                width=0.04) +
  ylab("Difference in looking (emmeans \u00B1 SE)") +
  xlab("Treatment") +
  geom_hline(yintercept=0) + 
  scale_y_continuous(breaks=c(-0.05, 0, 0.05, 0.1, 0.15),
                     labels=c("-0.05", "0", "0.05", "0.10", "0.15")) +
  theme(axis.line.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=16, color="black"),
        axis.title.y = element_text(margin = margin(t=0, r=10, b=0, l=0)),
        axis.title.x = element_text(margin = margin(t=10, r=0, b=0, l=0))) 
lookPlot

#Create locomotion dataframe
locomotionData <- data.frame(
  treatment=c("Silent", "White noise"),
  difference=c(0.00353, 0.03247),
  locomotionSE=c(0.0085, 0.0084) #got SE from summary(locomotionEmmean)
  
)

#Locomotion barplot
locomotionPlot <- ggplot(locomotionData, aes(x=treatment, y=difference)) +
  geom_bar(stat = "identity",
           color="black",
           fill="white") + 
  theme_classic() +
  geom_errorbar(aes(x=treatment, ymin=difference-locomotionSE, ymax=difference+locomotionSE),
                width=0.04) +
  ylab("Difference in locomotion (emmeans \u00B1 SE)") +
  xlab("Treatment") +
  geom_hline(yintercept=0) +
  scale_y_continuous(breaks=c(0, 0.01, 0.02, 0.03, 0.04),
                     labels=c("0", " 0.01", "0.02", "0.03", "0.04")) +
  theme(axis.line.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=16, color="black"),
        axis.title.y = element_text(margin = margin(t=0, r=10, b=0, l=0)),
        axis.title.x = element_text(margin = margin(t=10, r=0, b=0, l=0)))
locomotionPlot


#FID vs SD Scatter Plot
FIDPlot <- ggplot(skinkData, aes(x=SD, y=FID, shape=treatment))+
  geom_point(size=2)+
  geom_smooth(method=lm,se=FALSE,aes(linetype=treatment),color="black")+
  theme_classic()+
  xlab("Starting distance (cm)") + 
  ylab("FID (cm)") +
  scale_shape_manual(name="Treatment", labels=c("Silent","White Noise"), values=c(3,16))+
  scale_linetype_discrete(name="Treatment",breaks=c("silent","white"), labels=c("Silent","White Noise"))+ 
  theme(axis.title=element_text(size=18),
        axis.text = element_text(size=16, color="black"),
        axis.title.y = element_text(margin = margin(t=0, r=10, b=0, l=0)),
        axis.title.x = element_text(margin = margin(t=10, r=0, b=0, l=0)),
        legend.position = "none")
FIDPlot

##Save high quality images -- open images in powerpoint then copy&paste to word as image
#Save looking plot as lookPlot
CairoSVG(file.path("C:/New Folder/lookPlot"), width = 5.53, height = 4.84)
print(lookPlot)
dev.off()

#Save locomotion plot as locomotionPlot
CairoSVG(file.path("C:/New Folder/locomotionPlot"), width = 5.53, height = 4.84)
print(locomotionPlot)
dev.off()

#Save FID plot as FIDPlot
CairoSVG(file.path("C:/New Folder/FIDPlot"), width = 5.53, height = 4.84)
print(FIDPlot)
dev.off()

#############################################################################################
#####SECTION 9: CITATIONS###############################################################

citation()
citation(package = "emmeans")
citation(package = "MuMIn")
citation(package = "rsq")
citation(package = "Cairo")
citation(package = "sjstats")
