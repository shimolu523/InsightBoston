setwd('Documents/GitDocs/InsightBoston/Interviews/')
require(xlsx)
dfRaw <- read.xlsx("TruverisChallengeData.xlsx", sheetIndex = 1, header = TRUE)
# Inspecting the data:
print(colnames(dfRaw))
print(head(dfRaw))
# We're mostly interested in distribution of copay, which is the co_pay_amt column
# Obtaining the current co_pay_amt distribution
print(summary(dfRaw$co_pay_amt))
hist(dfRaw$co_pay_amt, 
     main="PDF of current copay",     
     xlab="copay amount",     
     border="black",     
     col="blue",     
     xlim=c(0,130),  
     breaks=c( seq(from = 0, to = 120, by = 10), 290))
# Now plotting the distribution of the national copy distribution:
# The 130 below is a normalization constant
pdfNatCopay <- c(56.57,6.9,8.29,5.87,8.18,4.63,1.34,1.50,0.29,3.72,2.10,0.24,0.35)/100/130
barplot(pdfNatCopay, main="PDF of current copay", 
        xlab="copay amount")
# Q2:
# Resample data:
reSampleCopay <- function(dfTruveris, freqNationalCopay){
  # dfTruveris: data frame containing a column named co_pay_amt in dollar amount
  # freqNationalCopay: vector with entries specifying population percentage 
    # on copay dollar amount with a 10 dollar bin size
  
  df <- dfTruveris
  hCopay <- hist(dfTruveris$co_pay_amt, 
                 breaks=c( seq(from = 0, to = 120, by = 10), 290),
                 plot = FALSE)
  sampleBinSize <- hCopay$counts
  
  # Find the resampling weight freq:
  freq <- sapply(dfTruveris$co_pay_amt, function(x, distr) 
    # for histogram specified by distr, the bin number where a copay amount x falls in is:
      # bin = floor(x/10) + 1
    # The percentage value for this bin in the distribution: 
    distr[ floor(x/10)+1 ], 
    distr = freqNationalCopay/sampleBinSize
    )
  
  # Use frquency to resample data
  df[['freq']] <- freq
  df[['ind']] <- 1:nrow(df) # row index id as unique label for resampling
  # resampled row index with replacement
  indResamp <- sample(df$ind, replace = TRUE, prob = freq)
  return(df[indResamp,]) # resampled data frame
}


# Q3:

# Q4:
# Recode reversal as a dummy variable 0 or 1
dfRaw[['dummReversal']] <- sapply(dfRaw$reversal, function(x) if (x=='Y'){1} else 0)
# Plot reversal as a function of copay:
plot(dfRaw$co_pay_amt,dfRaw$dummReversal,
     main="Reversal vs copay", 
     xlim=c(0,130),  
     xlab="Copay dollar amount", 
     ylab="Reversal")

