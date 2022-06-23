#---------------------------------------------------#
# FACTORED REGRESSION WITH CROSS-LEVEL INTERACTIONS
#
# Pooling Multiple Imputation Analysis with
#  computing Rights & Sterba (2019) R-square effect
#  sizes for Joint Imputations
#
#---------------------------------------------------#

# Load mitml package for pooling
library(mitml)

# Load lme4 package for multilevel model fitting
library(lme4)

# Load r2mlm for r2 computaitons
library(r2mlm)

# Read Each Imputed Data Set
imps_prior1   <- read.csv('imps.prior1.csv')
imps_prior2   <- read.csv('imps.prior2.csv')
imps_priorsep <- read.csv('imps.priorsep.csv')

# Choose Sequential imputations and split by Imputation number
imps <- split(imps_priorsep, imps_priorsep$imp.)

# Convert to mitml list
imps <- as.mitml.list(imps)

# Setup centering of each imputation individually
imps_centered <- within(imps, {
    # CGM center climate
    climate.cgm   <- climate - mean(climate)
    # CWC lmx with latent mean
    lmx.cwc <- lmx - lmx.mean.team. 
    # CGM lmx latent mean
    lmx.meanj.cgm <- lmx.mean.team. - mean(lmx.mean.team.)
})

# Analyze Each Imputation
model <- ''
analysis <- with(imps_centered, lmer(
    empower ~ lmx.cwc + lmx.meanj.cgm + climate.cgm + lmx.cwc*climate.cgm + lmx.meanj.cgm*climate.cgm + male + cohesion + (1 + lmx.cwc | team),
    REML = T)
)

# Pool estimates including variances set df adjustment
testEstimates(analysis, extra.pars = T, df.com = 97)

# Pool Rights & Sterba (2019) R-square Effect Sizes
# Apply function to each imputation
r_sqs <- sapply(imps_centered, function(imp) {
    # Re-estimate each model with individual imputation
    ran.slope <- lmer(
        empower ~ lmx.cwc + lmx.meanj.cgm + climate.cgm + lmx.cwc*climate.cgm + lmx.meanj.cgm*climate.cgm + male + cohesion + (1 + lmx.cwc | team),
        data = imp, REML = T
    )
    # Compute R-squares
    output <- r2mlm(ran.slope, bargraph = FALSE)
    # Return only the r-square metrics (adding [,1] will maintain names)
    return(output$Decompositions[,1])
})

# Average across rows
results <- rowMeans(r_sqs)

## Order of R-squares
# 1: Fixed Effects
# 2: Random Slope
# 3: Random Intercept
# 4: Residual Variation

# Print rounded results
print(results, digits = 2)

