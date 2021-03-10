#ANOVA&ACOVA analysis
##there are significant differences in bullying rates among different cultures
##while holding the income constant, there's still a significant effect of regions on bullying rates
rm( list=ls() )
setwd("C:/Users/zhang/Desktop/Study/767/1_Final")
library(readxl)
library(psych)
library( ggplot2 )
library( sjstats )
library(car)
library( foreign )
library( QuantPsyc )
library( ppcor )
library( multcomp )
library( lsmeans )
ybdata<-read_excel("Clean data.xlsx")
ybdata
describe(ybdata)
BR=ybdata$`Toal bullying rates`
Female =ybdata$`Female bullying rates`
Male=ybdata$`male bullying rates`
income=ybdata$`Income per capita ($)`
GINI=ybdata$`Gini index`
edu=ybdata$`Education spending (%)`

# anova analysis of regions
## plot the data to see the differences among groups and Homoscedasticity.
leveneTest(BR ~ Continent, data=ybdata)
ggplot( ybdata, aes( x=Continent, y=BR, fill=Continent ) ) +
  geom_boxplot( ) + theme_linedraw() + theme(legend.position = "none") +
  ylab( 'Bullying rates' ) + xlab( 'region' ) +
  ggtitle( 'Boxplot of bullying rates by region' )
##get different levels describe data
do.call( rbind, describeBy( BR, ybdata$Continent ) )[ , -1 ]
## anova&post hoc&ef
summary( Fit1 <- aov( BR ~ Continent, data=ybdata ) )
( Tukey <- TukeyHSD( Fit1 ) )
anova_stats( Fit1 )





#ANCOVA add income
# Test the assumption of parallel regression lines
cor.test(income, BR)
summary( mAOV <- aov( lm( BR ~ income*Continent, data=ybdata ) ) )

ggplot( ybdata, aes( x=income, y=BR, color=Continent ) ) +
  geom_smooth( method=lm, se=FALSE, fullrange=TRUE ) +
  geom_point()

# ANCOVA using regression, notice the order of the variables
summary( Fit2 <- lm( BR ~ income + Continent, data=ybdata ) )
anova( Fit2 )

( coeffs    <- coef( Fit2) )
b0.G1       <- coeffs[ 1 ]
b2.G2       <- coeffs[ 3 ] + b0.G1
b3.G3       <- coeffs[ 4 ] + b0.G1
b4.G4       <- coeffs[ 5 ] + b0.G1
b5.G5       <- coeffs[ 6 ] + b0.G1
slopeCommon <- coeffs[ 2 ]

ggplot( ybdata, aes( x=income, y=BR, color=Continent, shape=Continent ) ) + geom_point( size=2 ) +
  geom_abline( aes( intercept=b0.G1, slope=slopeCommon ), col='red'   ) +
  geom_abline( aes( intercept=b2.G2, slope=slopeCommon ), col='green' ) +
  geom_abline( aes( intercept=b3.G3, slope=slopeCommon ), col='blue'  ) +
  geom_abline( aes( intercept=b4.G4, slope=slopeCommon ), col='yellow') +
  geom_abline( aes( intercept=b5.G5, slope=slopeCommon ), col='pink'  ) +
  ggtitle( 'Data and group-wise regression lines' ) +
  labs( shape='Continent', color='Continent' )

  # ANCOVA using ANOVA, notice the order
  summary( fAOV <- aov( BR ~ income + Continent, data=ybdata ) )
  
  anova_stats( fAOV )
  
  #Adjusted Means
  ( Means.Adjusted <- lsmeans( Fit2, 'Continent' ) )
  plot( Means.Adjusted, xlab='Adjusted Means' )
  sumMA <- summary( Means.Adjusted )
  
  # What are these lsmeans
  ( gMean <- mean( ybdata$`Income per capita ($)` ) )
  
  mean( ybdata$`Income per capita ($)`[ ybdata$Continent == 'Africa' ] )
  ( zCoef <- coef( Fit2 ) )
  zCoef[1] + zCoef[ 2 ] * gMean
  
  # Compare adjusted/lsmeans with observed means
  cmFit <- lm( BR ~ Continent - 1, data=ybdata )
  obsMeans <- data.frame( Mean=coef( cmFit ), as.array( confint( cmFit ) ) )
  names( obsMeans )[ 2:3 ] <- c( 'LCL', 'UCL' )
  rownames( obsMeans ) <- paste( 'Continent', 1:3 )
  obsMeans
  
  cbind( obsMeans, LSMean=sumMA$lsmean, LCL=sumMA$lower.CL, UCL=sumMA$upper.CL )
  
  summary( TukeysHSD <- glht( Fit2, linfct = mcp( Continent = "Tukey") ) )
  confint( TukeysHSD )
  
  
  # Give the intercept meaning by mean centering
  ybdata$zincome <- ybdata$`Income per capita ($)` - mean( ybdata$`Income per capita ($)` )
  summary( Fit.3 <- lm( BR ~ zincome + Continent, data=ybdata  ) )
  anova( Fit.3 )
  
 #regression 
  summary( Fit5 <- lm( BR ~  income+ GINI + edu, data=ybdata ) )
  
  
# Factorial Aanlysis
##Factorial ANOVA shows that there's a significant interaction effect between two factors
  rm( list=ls() )
  setwd("C:/Users/zhang/Desktop/Study/767/1_Final")
  library(readxl)
  library(  dplyr )
  library( ggplot2 )
  library(sjstats)
  fdata<-read_excel("F analysis.xlsx")
  fdata$sex <- factor( fdata$sex, levels=0:1, labels=c( 'female', 'male' ) )
  fdata
  FBR=fdata$`bullying rate`
  
  (df_interactions <- fdata %>%
      group_by( format, sex )   %>%
      summarise(mean=mean(FBR )))
  
  
  
  ( m1 <- aov( FBR ~ format*sex, data=fdata ) )
  ( m2 <- lm(  FBR ~ format*sex, data=fdata ) )
  
  summary( m1 )
  summary( m2 )
  
  summary( aov( m2 ) )
  
  # Tukey's test on the interaction term
  ( Tukey <- TukeyHSD( m1, which='format:sex' ) )
  anova_stats(m1)
  
  ggplot( df_interactions, aes( x=size, y=mean, group=color, col=color ) ) +
    geom_point( shape=23, size=3 ) + geom_line() +
    xlab( 'Monitor Size' ) + ylab( 'Mean Satisfaction Rating' ) +
    scale_color_manual( 'Monitor Color', values=c( 'forestgreen','goldenrod3')) + theme_bw() +
    geom_point( data=df_monitor, aes( x=size, y=jitter( satisfaction ), group=color, col=color ) )
  

##T-test
  setwd("C:/Users/zhang/Desktop/Study/767/1_Final")
  library(readxl)
  library(psych)
  library( ggplot2 )
  library( sjstats )
  library(car)
  library( multcomp )
  library( lsmeans )
  tdata<-read_excel("t-test data.xlsx")
  describe(tdata)
  GBR=tdata$`bullying rates`
  tdata$sex<-factor(tdata$sex,levels=0:1,labels=c("female","male"))
  
  # t-test sex 
  t.test(GBR~sex,data=tdata,var.equal=T)
  summary(Fit4<-lm(GBR~sex,data=tdata))
  confint(Fit4)
###regression 
  #Multiple linear regression analysis was used to develop a model for predicting a country's bullying rates from some economic factors
  rm( list=ls() )
  setwd("C:/Users/zhang/Desktop/Study/767/1_Final")
  library(car)
  library( foreign )
  library( QuantPsyc )
  library( ppcor )
  library(readxl)
  library(psych)
  rdata<-read_excel("spcor.xlsx")
  describe(rdata)
  BR=rdata$`Toal bullying rates`
  income=rdata$`Income per capita ($)`
  GINI=rdata$`Gini index`
  edu=rdata$`Education spending (%)`
  
  #regression 
  summary( Fit5 <- lm( BR ~  income+ GINI + edu, data=rdata ) )
  
  
  ( zero_order <- cor( rdata ) )
  diag( zero_order ) <- NA
  zero_order[ upper.tri( zero_order ) ] <- NA
  zero_order[ 1, ] <- NA
  
  
  ( regression_table <- summary( Fit5 )$coefficients )
  ( CI               <- confint( Fit5 ) )
  colnames( CI ) <- c( 'LCL', 'UCL' )
  
  ( beta <- lm.beta( Fit5 ) )
  ( sr <- round(spcor(rdata)$estimate["income",-1] ))
  ( sr <- spcor( rdata )$estimate[,-1 ] )
  
  cbind( regression_table, CI, zero_order, beta,sr ) 
  (sp<-spcor.test(edu,GINI,income))
  
  vif( Fit5 )
  
  
  avPlots( Fit5 )
  
  
  summary( Fit_Q <- lm( GRE_Q ~ GRE_V + MAT + AR, data=df_gpa ) )
  
  cor( df_gpa$GPA, Fit_Q$residuals )
  
  with( df_gpa, plot( GPA ~ Fit_Q$residuals ) )
  abline( lm( df_gpa$GPA ~ Fit_Q$residuals ) )
  
  
  # In multiple regress, the coefficient of alienation of referred to as Tolerance
  Tolerance <- 1 - summary( Fit_Q )$r.squared
  
  # The VIF is the inverse of the Tolerance
  1 / Tolerance
  
  