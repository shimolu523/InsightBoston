# Truveris data challenge solutions
# by Molu Shi

# Load data for all challenge question below:

setwd('Documents/GitDocs/InsightBoston/Interviews/')
require(xlsx)
dfRaw <- read.xlsx("TruverisChallengeData.xlsx", sheetIndex = 1, header = TRUE)

# Inspect data before addressing challenge questions:
print( colnames(dfRaw) )
print( head(dfRaw) )

# Q1, plan to remove bias by resampling:

# Examine the data copay distribution against the national copay distribution

# Histogram of current copay distribution
hCopay <- hist(dfRaw$co_pay_amt, 
               breaks=c(0, seq(from = 10.001, to = 120.001, by = 10), 300),
               plot = FALSE)

copayDistr_orig <- hCopay$counts/nrow(dfRaw) # current copay distribution
copayDistr_natl <- c(56.57,6.9,8.29,5.87,8.18,4.63,1.34,1.50,0.29,3.72,2.10,0.24,0.35)/100

barplot( rbind(copayDistr_orig, copayDistr_natl), beside = TRUE,
         main = 'Current sample vs national copay distribution',
         ylab = 'Fraction of copay amount',
         names.arg = c('0-10','10-20','20-30','30-40','40-50','50-60',
                       '60-70','70-80','80-90','90-100','100-110','110-120','120+'),
         col = c('red','blue'),
         legend.text = c('current','national'))

# Plan to address challenge:

# To transform the data so that the copay resembles that of the national copay distribution,
# We plan to randomly resample the data with the weight derived from national distribution.
# More specifically, for each patient the frequency it can be resampled follows:
# freq = 1/n_i * P_i,
# where n_i is the number of patients in copay bucket i, in the CURRENT dataset.
# P_i is the population fraction in copay bucket i, in the NATIONAL distribution.
# We can index each patient with a unique number, resample each index according to P_i,
# and obtain the resampled patients in the data, according to the resampled index.
# The sample(x, size, replace=TRUE, prob=freq) function in R performs this task,
# where x is the index, size is the number of patients, and prob is freq, defined above.
# The sampling is done with replacement, to retain the same sample size.

# Q2, function that performs resampling:

# Resample data:

reSampleCopay <- function(dfTruveris){
  
  # Input: 
  # dfTruveris: data frame containing a column named co_pay_amt in dollar amount
  # Output:
  # dfResampled: redampled dfTruveris with co_pay_amount following national distribution
  
  df <- dfTruveris # copy input data frame
  
  # Get the column names to recall columns after resampling
  df.columns <- names(df)
  
  # national distribution
  copayDistr_natl <- c(56.57,6.9,8.29,5.87,8.18,4.63,1.34,1.50,0.29,3.72,2.10,0.24,0.35)/100
  
  # histogram of current data
  hCopay <- hist(df$co_pay_amt, 
                 breaks=c(0, seq(from = 10.001, to = 120.001, by = 10), 300),
                 plot = FALSE)

  # Find the resampling weight freq:
  freq <- sapply(df$co_pay_amt, function(x, distr) 
    # for histogram specified by distr, the bin number where a copay amount x falls in is:
      # bin = floor(x/10) + 1
    # The percentage value for this bin in the distribution: 
    if (x==0) return(distr[1])
    else if (x > 120) return(distr[13]) # for x > 120
    else return(distr[ceiling(x/10)]), # groups segmented by 10 (unit: copay dollars)
    distr = copayDistr_natl/hCopay$counts 
    )
  
  # Use frquency to resample data
  df[['freq']] <- freq
  df[['ind']] <- 1:nrow(df) # row index id as unique label for resampling
  
  # resampled row index with replacement
  indResamp <- sample(df$ind, size=nrow(df), replace = TRUE, prob = freq)
  
  dfResampled <- df[indResamp, df.columns] # resampled data frame
  
  return(dfResampled)
}

# To validate method, plot resampled distribution against national distribution
df_resamp <- reSampleCopay(dfRaw)

hCopay_resamp <- hist(df_resamp$co_pay_amt, 
                      breaks=c(0, seq(from = 10.001, to = 120.001, by = 10), 300),
                      plot = FALSE)

copayDistr_resamp <- hCopay_resamp$counts/nrow(df_resamp) # resampled copay distribution
copayDistr_natl <- c(56.57,6.9,8.29,5.87,8.18,4.63,1.34,1.50,0.29,3.72,2.10,0.24,0.35)/100

barplot( rbind(copayDistr_resamp, copayDistr_natl), beside = TRUE,
         main = 'Resampled data vs national copay distribution',
         ylab = 'Fraction of copay amount',
         names.arg = c('0-10','10-20','20-30','30-40','40-50','50-60',
                       '60-70','70-80','80-90','90-100','100-110','110-120','120+'),
         col = c('red','blue'),
         legend.text = c('resampled','national'))

# Q3, coupon redemption rate:
# The total redemption under the current coupon program can be found with the given data:

# Current redemption rate:
pRedeemCopay <- 1/100 * 
  c(0.167,6.117,16.399,39.335,47.429,55.102,59.19,60.167,64.027,64.027,64.027,64.027,64.027)

# Can find population fraction for different copay bucket
df_RedAssign <- dfRaw # copy original dataset and to work with
# Create index field, as keys to refer back(used later when sampling)
df_RedAssign[['ind']] <- 1:nrow(df_RedAssign) 

hCopay <- hist(df_RedAssign$co_pay_amt, 
               breaks=c(0, seq(from = 10.001, to = 120.001, by = 10), 300),
               plot = FALSE)

countCgroup <- hCopay$counts # population counts in copay bucket

# The total redemption fraction thus follows:
print( sum( pRedeemCopay * countCgroup/nrow(df_RedAssign) ) )

# For this challenge, we need to change the redemption distribution.
# Probability of redemption can be modeled using a logistic regression.
# For example, the current redemption fits very well with a logistic curve:

reNormConst <- 0.641 # renormalization const

copay <- seq(from = 5, to = 125, by = 10)

# fitting logistic model:
logiOrig <- glm(pRedeemCopay/reNormConst ~ copay,
                family=binomial(logit), 
                data = data.frame( cbind(pRedeemCopay, copay) )
                )
# plot fitting result, seems promising
plot(copay, pRedeemCopay,
     main = 'Current redemption rate for different copay groups', 
     xlab = 'Copay (unit: dollar)',
     ylab = 'Redemption rate')

lines(copay, logiOrig$fitted*reNormConst, type="l", col="red")

summary(logiOrig)

# Note that in the model we used a renormalization factor. 
# We can think of this as an awareness probability due to marketing efforts.
# People who redeem the coupons come from the fraction of total population aware of the coupons.

# We start with this model.
# By tuning the parameters, we can find a target redemption distribution for a total 33% redemption 
# We assume the coupon program gives the same reduction for all copay bucket.
# This is equivalent to shift the copay amount for patients.
# To first order approximation, this shift ONLY results in a change of beta_0 component.
# That is, the patient's threshold of willingness to take efforts to redeem a coupon.
# Therefore, we change beta_0 only in the following model tuning process:

totRedemp_targ <- NULL
beta_1 <- 0.097
beta_0 <- seq(from=-3.0, to=-0.5, by=0.1)

# logistic model for redemption rate based on copay amount:
pRedeemCopay_model <- function(copay=seq(from=5,to=125,by=10), 
                               beta_0=-0.55, beta_1=0.097, const = 0.641){
  return(const * 1/(1+exp(-(beta_0+beta_1*copay))))
}

plot(copay, pRedeemCopay,
     main = 'Current redemption rate and proposed target logistic model', 
     xlab = 'Copay (unit: dollar)',
     ylab = 'Redemption rate')

for (i in 1:length(beta_0)){
  pRedeemCopay_model.out <- pRedeemCopay_model(copay=seq(from=5,to=125,by=10), 
                                               beta_0= beta_0[i], 
                                               beta_1= beta_1, 
                                               const = reNormConst) 
  totRedemp_targ[i] <- sum( pRedeemCopay_model.out * countCgroup/nrow(df_RedAssign) )
  lines(copay, pRedeemCopay_model.out)
}

# Found that the solution beta_0 = -0.55 is a good target distribution:
# The corresponding redemption distribution is therefore

redempRate_targ <- pRedeemCopay_model(beta_0= -0.55)
barplot(redempRate_targ, main="Target redemption rate for for copay groups", 
        names.arg = c('0-10','10-20','20-30','30-40','40-50','50-60',
                      '60-70','70-80','80-90','90-100','100-110','110-120','120+'),
        xlab="copay amount")

# Next step: sample according to this target redemption distribution.

# For each copay bucket, sample randomly according to redempRate_targ.
# We also need to avoid marking redemption 'Y' for those Px is already reversed

# Probability P_i of redemption for one patient in copay group i, who has not refused Px:

#   P_i = p_i / (m_i/n_i) * m_i/sum_i(m_i) * / sum_i( p_i / (m_i/n_i) * m_i/sum_i(m_i) ),
#       = p_i * n_i/sum_i(m_i) * / sum_i( p_i * n_i/sum_i(m_i) ), simplified

# where p_i = redempRate_targ is the probability for redemption in copay group i.
# m_i/n_i is the fraction of filled Px in copay group i.
# m_i/sum_i(m_i) is the population fraction of copay group i over all filled Px.
# The last term sum_i( p_i / (m_i/n_i) * m_i/sum_i(m_i) ) is a renormalization factor

# Now calculate P_i:

# For m_i's:
df_fill <- df_RedAssign[dfRaw$reversal=='N',] # Filled Px in df_RedAssign

hCopay_fill <- hist(df_fill$co_pay_amt, 
                    breaks=c(0, seq(from = 10.001, to = 120.001, by = 10), 300),
                    plot = FALSE)

countCgroup_fill <- hCopay_fill$counts # this is the list of all m_i's

# P_i's found here:
distrRed_fill <- redempRate_targ * ( countCgroup/nrow(df_fill) ) / 
  sum(redempRate_targ * ( countCgroup/nrow(df_fill) ))

# Function to assign P_i's based on the copay amount x, and distribution distr
prob <- function(x, distr){
  # distr: 13-element function specifying probability distribution for each group
  if (x==0) return(distr[1])
  else if (x > 120) return(distr[13]) # for x > 120
  else return(distr[ceiling(x/10)]) # groups segmented by 10 (unit: copay dollars)
}

# For each patient, the probability (frequency) to be sampled is:  
freqRed_fill <- sapply(df_fill$co_pay_amt, prob,
                       distr = distrRed_fill/countCgroup_fill)

# Now sample without replacement:

# Mark all redemption with 'N' first, then change the sampled ones to 'Y'
df_RedAssign['redemption'] <- rep('N', nrow(df_RedAssign))

# Sampling the ones that are filled:

indRedempY <- sample(df_fill$ind, floor(nrow(df_RedAssign)/3), 
                     prob = freqRed_fill, replace = FALSE)

# check if replaced for sampling:

print( length(unique(indRedempY)) )

# change the sampled index to 'Y' in the original data set:

df_RedAssign$redemption[indRedempY] <- 'Y'

# check if close to targeted distribution:

hCopay_redemp <- hist(df_RedAssign$co_pay_amt[indRedempY], 
                      breaks=c(0, seq(from = 10.001, to = 120.001, by = 10), 300),
                      plot = FALSE)

countCgroup_redemp <-hCopay_redemp$counts # population counts marked redemption = 'Y'

# Difference in fraction of redeemed coupons over total population, for target and sampled:
plot( copay, 
      100*(countCgroup_redemp - redempRate_targ*countCgroup)/nrow(df_RedAssign), 
      main = 'Difference in redemption fraction, sampled vs target', 
      xlab = 'Copay (unit: dollar)',
      ylab = 'sampled - target (unit: percent)')



# Q4, modeling reversal likelyhood:

# The probability of reversal is P_rev = 1 - P_fill

# According to Bayes theorem, the probability a Px is filled is
# P_fill <- P_redemp + P_0 * (1-P_redemp),
# where P_0 is the probability of a patient to fill the Px without coupon
# We assume P_0 does NOT change with P_redemp, since it describes patient's willingness to pay without coupon

# For this challenge, we use the original data to find P_0 for each copay amount
# Then using P_0, and the new target redemption distribution found in Challenge 3,
# we can predict P_fill, and P_rev = 1 - P_fill

# P_fill = fillRate
fillRate_orig <- countCgroup_fill/countCgroup
fillRate_orig[is.nan(fillRate_orig)] <- 0

# P_redempt = redempRate
redempRate_orig <- pRedeemCopay

# now calculate P_0 = fillRate_noCoup
fillRate_noCoup <- (fillRate_orig -  redempRate_orig)/ (1 - redempRate_orig)

# for the new target redemption rate, we thus have:
fillRate_targ <- redempRate_targ + fillRate_noCoup*(1 - redempRate_targ) 

# for the reversal rate under the new target redemption:
revRate_targ <- 1 - fillRate_targ