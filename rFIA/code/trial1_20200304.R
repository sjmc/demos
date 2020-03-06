
## experimenting with rFIA ###
#https://cran.r-project.org/web/packages/rFIA/readme/README.html

##################################################3
pdat <- function(x){
  print.data.frame(x)
}

library(dplyr)

##################################################
# rFIA: Unlocking the FIA Database in R
# CRAN status  Travis build status Lifecycle: maturing

# US Biomass
# The goal of rFIA is to increase the accessibility and use of the USFS Forest Inventory and Analysis (FIA) Database by providing a user-friendly, open source platform to easily query and analyze FIA Data. Designed to accommodate a wide range of potential user objectives, rFIA simplifies the estimation of forest variables from the FIA Database and allows all R users (experts and newcomers alike) to unlock the flexibility and potential inherent to the Enhanced FIA design.
# 
# Specifically, rFIA improves accessibility to the spatio-temporal estimation capacity of the FIA Database by producing space-time indexed summaries of forest variables within user-defined population boundaries. Direct integration with other popular R packages (e.g., dplyr, sp, and sf) facilitates efficient space-time query and data summary, and supports common data representations and API design. The package implements design-based estimation procedures outlined by Bechtold & Patterson (2005), and has been validated against estimates and sampling errors produced by EVALIDator. Current development is focused on the implementation of spatially-enabled model-assisted estimators to improve population, change, and ratio estimates.
# 
# 
# 
# Installation
# You can install the released version of rFIA from CRAN with:
#   
  install.packages("rFIA")
# Alternatively, you can install the development version from GitHub:
  
 # devtools::install_github('hunter-stanke/rFIA')


# Functionality
# rFIA Function	Description
# area	Estimate land area
# biomass	Estimate volume, biomass, & carbon stocks of standing trees
# clipFIA	Spatial & temporal queries
# diversity	Estimate species diversity
# dwm	Estimate volume, biomass, and carbon stocks of down woody material
# getFIA	Download FIA data, load into R, and optionally save to disk
# growMort	Estimate recruitment, mortality, and harvest rates
# invasive	Estimate areal coverage of invasive species
# plotFIA	Produce static & animated plots of spatial FIA summaries
# readFIA	Load FIA database into R environment
# seedling	Estimate seedling abundance (TPA)
# standStruct	Estimate forest structural stage distributions
# tpa	Estimate abundance of standing trees (TPA & BAA)
# vitalRates	Estimate live tree growth rates


# Example Usage
# Download FIA Data and Load into R
# The first step to using rFIA is to download subsets of the FIA Database. The easiest way to accomplish this is using getFIA. Using one line of code, you can download state subsets of the FIA Database, load data into your R environment, and optionally save those data to a local directory for future use!
#   
  ## Download the state subset or Connecticut (requires an internet connection)
  # All data acquired from FIA Datamart: https://apps.fs.usda.gov/fia/datamart/datamart.html

#ct <- getFIA(states = 'CT', dir = '/path/to/save/data')
ct <- getFIA(states = 'CT', dir = 'C:/Users/stellac/Documents/Local_Data/rFIA_trial')

# By default, getFIA only loads the portions of the database required to produce summaries with other rFIA functions (common = TRUE). This conserves memory on your machine and speeds download time. If you would like to download all available tables for a state, simple specify common = FALSE in the call to getFIA.

# But what if I want to load multiple states worth of FIA data into R? No problem! Simply specify mutiple state abbreviations in the states argument of getFIA (e.g. states = c('MI', 'IN', 'WI', 'IL')), and all state subsets will be downloaded and merged into a single FIA.Database object. This will allow you to use other rFIA functions to produce estimates within polygons which straddle state boundaries!
#   

# Note: given the massive size of the full FIA Database, users are cautioned to only download the subsets containing their region of interest.

# If you have previously downloaded FIA data would simply like to load into R from a local directory, use readFIA:
  
  ## Load FIA Data from a local directory
#  db <- readFIA('/path/to/your/directory/')
#  dir = 'C:/Users/stellac/Documents/Local_Data/rFIA_trial')
db <- readFIA('C:/Users/stellac/Documents/Local_Data/rFIA_trial/')

# Compute Estimates of Forest Variables
# Now that you have loaded your FIA data into R, it’s time to put it to work. Let’s explore the basic functionality of rFIA with tpa, a function to compute tree abundance estimates (TPA, BAA, & relative abundance) from FIA data, and fiaRI, a subset of the FIA Database for Rhode Island including inventories from 2013-2018.
# 
# Estimate the abundance of live trees in Rhode Island:
#   
library(rFIA)
## Load the Rhode Island subset of the FIADB (included w/ rFIA)
## NOTE: This object can be produced using getFIA and/or readFIA
data("fiaRI")

## Only estimates for the most recent inventory year
fiaRI_MR <- clipFIA(fiaRI, mostRecent = TRUE) ## subset the most recent data
tpaRI_MR <- tpa(fiaRI_MR)
head(tpaRI_MR)

### not only most recent 
fiaRI_all <- clipFIA(fiaRI, mostRecent = FALSE)

names(fiaRI_all)
# these are the tables within the db
# [1] "COND_DWM_CALC"          "COND"                   "INVASIVE_SUBPLOT_SPP"   "PLOT"                   "POP_ESTN_UNIT"          "POP_EVAL_GRP"          
# [7] "POP_EVAL_TYP"           "POP_EVAL"               "POP_PLOT_STRATUM_ASSGN" "POP_STRATUM"            "SEEDLING"               "SUBP_COND_CHNG_MTRX"   
# [13] "SUBP_COND"              "SUBPLOT"                "SURVEY"                 "TREE_GRM_BEGIN"         "TREE_GRM_COMPONENT"     "TREE_GRM_MIDPT"        
# [19] "TREE" 

tpa_fiaRI_all <- tpa(fiaRI_all)

tpa_fiaRI_all$YEAR





#> # A tibble: 1 x 11
#>    YEAR   TPA   BAA TPA_PERC BAA_PERC TPA_SE BAA_SE TPA_PERC_SE BAA_PERC_SE
#>   <int> <dbl> <dbl>    <dbl>    <dbl>  <dbl>  <dbl>       <dbl>       <dbl>
#> 1  2018  427.  122.     93.2     93.7   6.63   3.06        7.62        4.48
#> # … with 2 more variables: nPlots_TREE <dbl>, nPlots_AREA <dbl>

## All Inventory Years Available (i.e., returns a time series)
tpaRI <- tpa(fiaRI)

## Time Series plot
plotFIA(tpaRI, BAA, se = TRUE, plot.title = 'Basal area per acre in Rhode Island over time')

#doesn't work

###What if I want to group estimates by species? How about by size class?
  
  ## Group estimates by species
  tpaRI_species <- tpa(fiaRI_MR, bySpecies = TRUE)
head(tpaRI_species, n = 3)
#> # A tibble: 3 x 14
#>    YEAR  SPCD COMMON_NAME SCIENTIFIC_NAME    TPA    BAA TPA_PERC BAA_PERC
#>   <int> <int> <chr>       <chr>            <dbl>  <dbl>    <dbl>    <dbl>
#> 1  2018    12 balsam fir  Abies balsamea  0.0873 0.0295   0.0191   0.0226
#> 2  2018    43 Atlantic w… Chamaecyparis … 0.247  0.180    0.0539   0.138 
#> 3  2018    68 eastern re… Juniperus virg… 1.14   0.138    0.249    0.106 
#> # … with 6 more variables: TPA_SE <dbl>, BAA_SE <dbl>, TPA_PERC_SE <dbl>,
#> #   BAA_PERC_SE <dbl>, nPlots_TREE <dbl>, nPlots_AREA <dbl>

## Group estimates by size class
## NOTE: Default 2-inch size classes, but you can make your own using makeClasses()
tpaRI_sizeClass <- tpa(fiaRI_MR, bySizeClass = TRUE)
head(tpaRI_sizeClass, n = 3)
#> # A tibble: 3 x 12
#>    YEAR sizeClass   TPA   BAA TPA_PERC BAA_PERC TPA_SE BAA_SE TPA_PERC_SE
#>   <int>     <dbl> <dbl> <dbl>    <dbl>    <dbl>  <dbl>  <dbl>       <dbl>
#> 1  2018         1 188.   3.57     41.0     2.74  13.0   12.8         6.39
#> 2  2018         3  68.6  5.76     15.0     4.42  15.1   15.8         6.39
#> 3  2018         5  46.5  9.06     10.2     6.95   6.51   6.57        6.38
#> # … with 3 more variables: BAA_PERC_SE <dbl>, nPlots_TREE <dbl>,
#> #   nPlots_AREA <dbl>

## Group by species and size class, and plot the distribution 
##  for the most recent inventory year
tpaRI_spsc <- tpa(fiaRI_MR, bySpecies = TRUE, bySizeClass = TRUE)
plotFIA(tpaRI_spsc, BAA, grp = COMMON_NAME, x = sizeClass,
        plot.title = 'Size-class distributions of BAA by species', 
        x.lab = 'Size Class (inches)', text.size = .75,
        n.max = 5) # Only want the top 5 species, try n.max = -5 for bottom 5


# What if I want estimates for a specific type of tree (ex. greater than 12-inches DBH and in a canopy dominant or subdominant position) in specific area (ex. growing on mesic sites), and I want to group by estimates by some variable other than species or size class (ex. ownsership group)? Easy! Each of these specifications are described in the FIA Database, and all rFIA functions can leverage these data to easily implement complex queries!
#   
  ## grpBy specifies what to group estimates by (just like species and size class above)
  ## treeDomain describes the trees of interest, in terms of FIA variables 
  ## areaDomain, just like above,describes the land area of interest
  tpaRI_own <- tpa(fiaRI_MR, 
                   grpBy = OWNGRPCD, 
                   treeDomain = DIA > 12 & CCLCD %in% c(1,2),
                   areaDomain = PHYSCLCD %in% c(20:29))
head(tpaRI_own)
#> # A tibble: 2 x 12
#>    YEAR OWNGRPCD   TPA   BAA TPA_PERC BAA_PERC TPA_SE BAA_SE TPA_PERC_SE
#>   <int>    <int> <dbl> <dbl>    <dbl>    <dbl>  <dbl>  <dbl>       <dbl>
#> 1  2018       30 0.848  3.57     20.8     29.3   59.0   59.1        24.7
#> 2  2018       40 1.49   3.99     79.2     70.7   25.7   27.7        24.7
#> # … with 3 more variables: BAA_PERC_SE <dbl>, nPlots_TREE <dbl>,
#> #   nPlots_AREA <dbl>
# What if I want to produce estimates within my own population boundaries (within user-defined spatial zones/polygons)? This is where things get really exciting.

## Load the county boundaries for Rhode Island
data('countiesRI') ## Load your own spatial data from shapefiles using readOGR() (rgdal)

## polys specifies the polygons (zones) where you are interested in producing estimates
## returnSpatial = TRUE indicates that the resulting estimates will be joined with the 
##    polygons we specified, thus allowing us to visualize the estimates across space
tpaRI_counties <- tpa(fiaRI_MR, polys = countiesRI, returnSpatial = TRUE)

## NOTE: Any grey polygons below simply means no FIA data was available for that region
plotFIA(tpaRI_counties, BAA) # Plotting method for spatial FIA summaries, also try 'TPA' or 'TPA_PERC'

# 
# We produced a really cool time series earlier, how would I marry the spatial and temporal capacity of rFIA to produce estimates across user-defined polygons and through time? Easy! Just hand tpa the full FIA.Database object you produced with readFIA (not the most recent subset produced with clipFIA). For stunning space-time visualizations, hand the output of tpa to plotFIA. To save the animation as a .gif file, simpy specify fileName (name of output file) and savePath (directory to save file, combined with fileName).

## Using the full FIA dataset, all available inventories
tpaRI_st <- tpa(fiaRI, polys = countiesRI, returnSpatial = TRUE)

## Animate the output
plotFIA(tpaRI_st, TPA, animate = TRUE, legend.title = 'Abundance (TPA)', legend.height = .8)

## what years?
head(tpaRI)
tpaRI$YEAR
# 2014-2018
######################################################
library(rFIA)

# this downloads a fresh copy from the web to a local file
#ca_remote <- getFIA(states = 'CA', dir = 'C:/Users/stellac/Documents/Local_Data/rFIA_trial/CA')

# this is for when you've downloaded the data already to a local directory
ca <- readFIA('C:/Users/stellac/Documents/Local_Data/rFIA_trial/CA')
db <- readFIA('C:/Users/stellac/Documents/Local_Data/rFIA_trial/CA')
# this data only goes back to 2005.
# The public db goes back to 2001 (and actually 1994 in some plots).
# Why different, if it is calling from the web?
cond <- data.frame(ca$COND)
range(cond$INVYR)

### All years (should be since 2001...)
fiaCA_all <- clipFIA(ca, mostRecent = FALSE)
# appears to only go back to 2005. 

# extract the TPA table (no longer a FIA database object, but instead a table)
tpa_fiaCA_all <- tpa(fiaCA_all, nCores = 4)

## Only estimates for the most recent inventory year
fiaCA_MR <- clipFIA(ca, mostRecent = TRUE, nCores = 4) ## subset the most recent data
tpaCA_MR <- tpa(fiaCA_MR)

## what years?
tpa_fiaCA_all$YEAR
#2005-2018 cool.... but not all the data.

# Take a look
#plotFIA(tpa_fiaCA_all, BAA, se = TRUE, plot.title = 'Basal area per acre over time')
#doesn't work

###What if I want to group estimates by species? How about by size class?

## Group estimates by species
tpaCA_species <- tpa(fiaCA_all, bySpecies = TRUE)

# chokes on this a bit...
tpaCA_species_MR <- tpa(fiaCA_MR, bySpecies = TRUE, nCores = 4)

dim(tpaCA_species) # 442
dim(tpaCA_species_MR) #175
head(tpaCA_species_MR, n = 100)
pdat(tpaCA_species_MR[,1:5])
tpaCA_species$COMMON_NAME

pdat(arrange(tpaCA_species_MR[,1:6], TPA))
View(arrange(tpaCA_species_MR[,c(1:6,13)], TPA)) # this now lacks the wierd species, good.
View(arrange(tpaCA_species[,c(1:6,13)], TPA))


# try to subset and combine tpa estimates by panel (year)
tpaCA_species_fortyp <- tpa(fiaCA_all, 
                            treeType = 'live', 
                            areaDomain = FORTYPCD %in% 371, # 371 is CA mixed conifer
                            bySpecies = TRUE, nCores = 4)
View(tpaCA_species_fortyp)

tpaCA_species_fortyp_sum <- tpaCA_species_fortyp %>% 
  group_by(SPCD, COMMON_NAME, SCIENTIFIC_NAME) %>% 
  summarise(mean.TPA = mean(TPA), mean.BAA = mean(BAA), nPlots_TOT = sum(nPlots_TREE))
tpaCA_species_fortyp_sum
View(tpaCA_species_fortyp_sum)

## check if this basically makes sense; sort by reverse TPA
arrange(tpaCA_species_fortyp_sum, desc(mean.TPA))

# For filtered by type = 371, ca mixed conifer, we have abco and psme in the top 3, followed by the usual suspects in great abundance. Seems a bit strange that species with zero occurences are still carrying through into the calculation (at 0 TPA), but that is easy to ignore. 

# # Groups:   SPCD, COMMON_NAME [96]
# SPCD COMMON_NAME          SCIENTIFIC_NAME       mean.TPA mean.BAA nPlots_TOT
# <int> <chr>                <chr>                    <dbl>    <dbl>      <dbl>
# 1    15 white fir            Abies concolor            89.3   36.7         4518
# 2   202 Douglas-fir          Pseudotsuga menziesii     71.0   42.5         3716
# 3    81 incense-cedar        Calocedrus decurrens      63.2   24.9         3614
# 4   122 ponderosa pine       Pinus ponderosa           33.0   19.0         3407
# 5   805 canyon live oak      Quercus chrysolepis       17.2    2.81        1186
# 6   818 California black oak Quercus kelloggii         13.8    4.97        1921
# 7   117 sugar pine           Pinus lambertiana         13.1   12.6         3278
# 8    20 California red fir   Abies magnifica           12.7    8.20         760
# 9   492 Pacific dogwood      Cornus nuttallii          12.1    0.338        360
# 10  116 Jeffrey pine        Pinus jeffreyi            10.9    9.91        1660

# now let's try dead trees. 

tpaCA_species_fortyp_dead <- tpa(fiaCA_all, 
                            treeType = 'dead', 
                            areaDomain = FORTYPCD %in% 371, # 371 is CA mixed conifer
                            bySpecies = TRUE, nCores = 4)
View(tpaCA_species_fortyp_dead)

# average yearly panels
tpaCA_species_fortyp_dead_sum <- tpaCA_species_fortyp_dead %>% 
  group_by(SPCD, COMMON_NAME, SCIENTIFIC_NAME) %>% 
  summarise(mean.TPA = mean(TPA), mean.BAA = mean(BAA), nPlots_TOT = sum(nPlots_TREE))
tpaCA_species_fortyp_dead_sum

arrange(tpaCA_species_fortyp_dead_sum, desc(mean.TPA))
# # Groups:   SPCD, COMMON_NAME [96]
# SPCD COMMON_NAME          SCIENTIFIC_NAME       mean.TPA mean.BAA nPlots_TOT
# <int> <chr>                <chr>                    <dbl>    <dbl>      <dbl>
#   1    15 white fir            Abies concolor           7.21     7.67        2637
# 2   202 Douglas-fir          Pseudotsuga menziesii    4.00     4.78        1906
# 3    81 incense-cedar        Calocedrus decurrens     3.63     2.51        1559
# 4   122 ponderosa pine       Pinus ponderosa          1.95     1.78        1155
# 5   818 California black oak Quercus kelloggii        1.57     0.806        795
# 6   117 sugar pine           Pinus lambertiana        1.50     2.22        1117
# 7   805 canyon live oak      Quercus chrysolepis      1.23     0.288        336
# 8    20 California red fir   Abies magnifica          1.16     2.14         566
# 9   361 Pacific madrone      Arbutus menziesii        0.698    0.205        198
# 10   108 lodgepole pine       Pinus contorta           0.637    0.604        246

7.67+36.7
7.67/44.37
# Hmm. 20% of BAA is dead?

# Produces tree per acre (TPA) and basal area per acre (BAA) estimates from FIA data, along with
# population totals for each variable. Estimates can be produced for regions defined within the FIA
# Database (e.g. counties), at the plot level, or within user-defined areal units. Options to group
# estimates by species, size class, and other variables defined in the FIADB. If multiple reporting
# years (EVALIDs) are included in the data, estimates will be output as a time series. If multiple
# states are represented by the data, estimates will be output for the full region (all area combined),
# unless specified otherwise (e.g. grpBy = STATECD). Easy options to implement parallel processing.

# take a look at the cond table
ca
# interesting to note that tree table DOES start in 1994. Why is TPA cutting off at 2005?

findEVALID(db, mostRecent = FALSE, state = 'CA', year = 2015, type = NULL)
findEVALID(fiaRI)
findEVALID(fiaCA_all)


fiaCA_all$TREE
tpaCA_species$YEAR
tpaCA_species <- tpa(fiaCA_all, bySpecies = TRUE)

range(cond$INVYR) # 1994-2018
caPEU <- data.frame(ca$POP_ESTN_UNIT)
POPEVAL <- data.frame(ca$POP_EVAL)
head(POPEVAL)

dim(POPEVAL)
POPEVAL[,c(1,2,4,8,9,10,11,12,13)]


# > POPEVAL[,c(1,2,4,8,9,10,11,12,13)]
# CN  EVAL_GRP_CN EVALID                                              REPORT_YEAR_NM START_INVYR END_INVYR LAND_ONLY TIMBERLAND_ONLY GROWTH_ACCT
# 1  1.000000e+00 1.000000e+00  69401                                                        1994          NA        NA         Y               Y            
# 2  5.348154e+14 4.807004e+14  61000           2001;2002;2003;2004;2005;2006;2007;2008;2009;2010        2001      2010         N               N            
# 3  5.348154e+14 4.807004e+14  61001           2001;2002;2003;2004;2005;2006;2007;2008;2009;2010        2001      2010         N               N            
# 4  6.911675e+14 6.870121e+14  61500           2006;2007;2008;2009;2010;2011;2012;2013;2014;2015        2006      2015         N               N            
# 5  6.911675e+14 6.870121e+14  61501           2006;2007;2008;2009;2010;2011;2012;2013;2014;2015        2006      2015         N               N            
# 6  6.911675e+14 6.870121e+14  61503                                    2011;2012;2013;2014;2015        2011      2015         N               N           Y
# 7  6.911675e+14 6.870121e+14  61507                                    2011;2012;2013;2014;2015        2011      2015         N               N            
# 8  6.911675e+14 6.870121e+14  61600           2007;2008;2009;2010;2011;2012;2013;2014;2015;2016        2007      2016         N               N            
# 9  6.911675e+14 6.870121e+14  61603                               2011;2012;2013;2014;2015;2016        2011      2016         N               N           Y
# 10 6.911675e+14 6.870121e+14  61601           2007;2008;2009;2010;2011;2012;2013;2014;2015;2016        2007      2016         N               N            
# 11 6.911675e+14 6.870121e+14  61607                               2011;2012;2013;2014;2015;2016        2011      2016         N               N            
# 12 6.911675e+14 6.870121e+14  61700           2008;2009;2010;2011;2012;2013;2014;2015;2016;2017        2008      2017         N               N            
# 13 6.911675e+14 6.870121e+14  61701      2007;2008;2009;2010;2011;2012;2013;2014;2015;2016;2017        2007      2017         N               N            
# 14 6.911675e+14 6.870121e+14  61703                          2011;2012;2013;2014;2015;2016;2017        2011      2017         N               N           Y
# 15 6.911675e+14 6.870121e+14  61707                          2011;2012;2013;2014;2015;2016;2017        2011      2017         N               N            
# 16 6.911675e+14 6.870121e+14  61800           2009;2010;2011;2012;2013;2014;2015;2016;2017;2018        2009      2018         N               N            
# 17 6.911675e+14 6.870121e+14  61801 2007;2008;2009;2010;2011;2012;2013;2014;2015;2016;2017;2018        2007      2018         N               N            
# 18 6.911675e+14 6.870121e+14  61807                     2011;2012;2013;2014;2015;2016;2017;2018        2011      2018         N               N            

# can I estimate tpa for specific years? for example, 2001?
# looks like the END_INVYR is used for all EVALIDS and not the previous re-visits. To get the earlier revisits I think you could truncate the data?

## Group estimates by size class
## NOTE: Default 2-inch size classes, but you can make your own using makeClasses()
tpaCA_sizeClass <- tpa(fiaCA_MR, bySizeClass = TRUE, nCores = 4)
head(tpaCA_sizeClass, n = 20)

tpaCA_sizeClass_dead <- tpa(fiaCA_MR, treeType = 'dead', bySizeClass = TRUE, bySpecies = TRUE, nCores = 4)
head(tpaCA_sizeClass_dead, n = 20)

arrange(tpaCA_sizeClass_dead, desc(TPA))
unique(tpaCA_sizeClass_dead$COMMON_NAME)
splist <- c("white fir", "Douglas-fir", "incense-cedar", "ponderosa pine", "black oak", "sugar pine", "redwood", "California red fir", "lodgepole pine", "Jeffrey pine", "Tasmanian bluegum")
splist
tpaCA_sizeClass_dead_subset <- filter(tpaCA_sizeClass_dead, COMMON_NAME %in% splist)

tpaCA_sizeClass_dead_yearsum <-  tpaCA_sizeClass_dead_subset %>% 
  group_by(SPCD, COMMON_NAME, SCIENTIFIC_NAME) %>% 
  summarise(mean.TPA = mean(TPA), mean.BAA = mean(BAA), nPlots_TOT = sum(nPlots_TREE))
tpaCA_sizeClass_dead_yearsum

tpaCA_species_fortyp_dead_sum <- tpaCA_species_fortyp_dead %>% 
  group_by(SPCD, COMMON_NAME, SCIENTIFIC_NAME) %>% 
  summarise(mean.TPA = mean(TPA), mean.BAA = mean(BAA), nPlots_TOT = sum(nPlots_TREE))
View(tpaCA_species_fortyp_dead_sum)


## Group by species and size class, and plot the distribution 
##  for the most recent inventory year
tpaCA_spsc <- tpa(fiaCA_MR, bySpecies = TRUE, bySizeClass = TRUE)
plotFIA(tpaCA_spsc, BAA, grp = COMMON_NAME, x = sizeClass,
        plot.title = 'Size-class distCAbutions of BAA by species', 
        x.lab = 'Size Class (inches)', text.size = .75,
        n.max = 5) # Only want the top 5 species, try n.max = -5 for bottom 5

#######################
## Test the growth and mortality functions ##

ecolump <- read.csv("C:/Users/stellac/Dropbox/Work/Cal_Work/CA_mort_shared/Data/xwalk_sectionname_ecolump.csv")
ecolump

gM_eco <- growMort(ca, grpBy = ECOSUBCD, polys = NULL,
         returnSpatial = FALSE,
         bySpecies = FALSE, 
         bySizeClass = FALSE,
         landType = 'forest',
         treeType = 'all',
         method = 'TI',
         stateVar = 'BAA',
         treeDomain = NULL,
         areaDomain = NULL, 
         totals = FALSE,
         byPlot = FALSE, 
         nCores = 4)

gM_eco_save <- gM_eco
#gM_eco <- gM_eco_save

gM_eco$ECOSUBCD_full <- gM_eco$ECOSUBCD

head(gM_eco)
levels(as.factor(gM_eco$ECOSUBCD))
#levels(gM_eco$ECOSUBCD)

gM_eco$ECOSUBCD <- sprintf("% 5s", gM_eco$ECOSUBCD)
levels(as.factor(gM_eco$ECOSUBCD))


# Strip the final characters off the code (retained in ECOSUBCD_full)
gM_eco$ECOSUBCD <-   substr(gM_eco$ECOSUBCD_full, start = 1, stop = 5)

#do they match?

#levels(raw$a) <- trimws(levels(raw$a))
gM_eco$ECOSUBCD <- as.factor(gM_eco$ECOSUBCD)
levels(gM_eco$ECOSUBCD) <- trimws(levels(gM_eco$ECOSUBCD))

levels(as.factor(gM_eco$ECOSUBCD))
levels(ecolump$ECOSUBCD)

# Join plot to Section info

gM_ecoj <- full_join(gM_eco, ecolump)
View(gM_ecoj)
gM_ecoj_sel <- select(gM_ecoj, ECO_LUMP, YEAR, MORT_BAA_ACRE, MORT_PERC, SECTION_NAME)
gM_ecoj_sel

gM_ecoj_sum <- gM_ecoj %>% 
  group_by(ECO_LUMP, YEAR) %>% 
  summarise(mean.MORT_BAA_AC = mean(MORT_BAA_ACRE), mean.MORT_PERC = mean(MORT_PERC), mean.REMV.BAA.ACRE = mean(REMV_BAA_ACRE))
pdat(gM_ecoj_sum)

## looks reasonable, actually! can I get mort_tpa? and by species?

gM_spp <- growMort(ca, grpBy = NULL, polys = NULL,
                   returnSpatial = FALSE,
                   bySpecies = TRUE, 
                   bySizeClass = FALSE,
                   landType = 'forest',
                   treeType = 'all',
                   method = 'TI',
                   stateVar = 'TPA',
                   treeDomain = NULL,
                   areaDomain = NULL, 
                   totals = FALSE,
                   byPlot = FALSE, 
                   nCores = 4)

gM_spp

gM_spp_sum <- gM_spp %>% 
  group_by(COMMON_NAME, SPCD, SCIENTIFIC_NAME, YEAR) %>% 
  summarise(mean.MORT_TPA_AC = mean(MORT_TPA_ACRE), mean.MORT_PERC = mean(MORT_PERC), mean.REMV.TPA.ACRE = mean(REMV_TPA_ACRE))
pdat(gM_spp_sum)
View(gM_spp_sum)

gM_spp10_sum <- filter(gM_spp_sum, SPCD %in% c(15, 122, 81, 202, 20, 117, 116, 108, 62, 211))
pdat(gM_spp10_sum)


gM_sppBA <- growMort(ca, grpBy = NULL, polys = NULL,
                   returnSpatial = FALSE,
                   bySpecies = TRUE, 
                   bySizeClass = FALSE,
                   landType = 'forest',
                   treeType = 'all',
                   method = 'TI',
                   stateVar = 'BAA',
                   treeDomain = NULL,
                   areaDomain = NULL, 
                   totals = FALSE,
                   byPlot = FALSE, 
                   nCores = 4)


gM_sppBA_sum <- gM_sppBA %>% 
  group_by(COMMON_NAME, SPCD, SCIENTIFIC_NAME, YEAR) %>% 
  summarise(mean.MORT_BAA_AC = mean(MORT_BAA_ACRE), mean.MORT_PERC = mean(MORT_PERC), mean.REMV.BAA.ACRE = mean(REMV_BAA_ACRE))
pdat(gM_sppBA_sum)
View(gM_sppBA_sum)

gM_sppBA10_sum <- filter(gM_sppBA_sum, SPCD %in% c(15, 122, 81, 202, 20, 117, 116, 108, 62, 211))
pdat(gM_sppBA10_sum)

# COMMON_NAME SPCD       SCIENTIFIC_NAME YEAR mean.MORT_BAA_AC mean.MORT_PERC mean.REMV.BAA.ACRE
# 1  California juniper   62 Juniperus californica 2015      0.001253065      0.4768524        0.000000000
# 2  California juniper   62 Juniperus californica 2016      0.005071388      1.0218732        0.000000000
# 3  California juniper   62 Juniperus californica 2017      0.004493862      0.9259834        0.000000000
# 4  California red fir   20       Abies magnifica 2015      0.117084736      1.5267152        0.008656759
# 5  California red fir   20       Abies magnifica 2016      0.115239720      1.5093740        0.007824228
# 6  California red fir   20       Abies magnifica 2017      0.118149840      1.5119163        0.007746588
# 7         Douglas-fir  202 Pseudotsuga menziesii 2015      0.133413423      0.5870053        0.089393331
# 8         Douglas-fir  202 Pseudotsuga menziesii 2016      0.148352824      0.6349256        0.090799792
# 9         Douglas-fir  202 Pseudotsuga menziesii 2017      0.162514302      0.6995517        0.093520890
# 10      incense-cedar   81  Calocedrus decurrens 2015      0.068715409      0.8014695        0.055352319
# 11      incense-cedar   81  Calocedrus decurrens 2016      0.066845806      0.8237185        0.054988742
# 12      incense-cedar   81  Calocedrus decurrens 2017      0.076482348      0.9482959        0.051416411
# 13       Jeffrey pine  116        Pinus jeffreyi 2015      0.051166391      0.7462255        0.015308059
# 14       Jeffrey pine  116        Pinus jeffreyi 2016      0.051023929      0.7478106        0.021966612
# 15       Jeffrey pine  116        Pinus jeffreyi 2017      0.050117683      0.7559117        0.019090938
# 16     lodgepole pine  108        Pinus contorta 2015      0.038459552      0.7256012        0.005042189
# 17     lodgepole pine  108        Pinus contorta 2016      0.039460087      0.7135954        0.006948118
# 18     lodgepole pine  108        Pinus contorta 2017      0.038501240      0.7031664        0.006435589
# 19     ponderosa pine  122       Pinus ponderosa 2015      0.107457704      0.8904872        0.070764505
# 20     ponderosa pine  122       Pinus ponderosa 2016      0.106732705      0.9111796        0.075510595
# 21     ponderosa pine  122       Pinus ponderosa 2017      0.121036885      1.0314888        0.069346239
# 22            redwood  211  Sequoia sempervirens 2015      0.016623356      0.2088320        0.061435520
# 23            redwood  211  Sequoia sempervirens 2016      0.014524437      0.1709126        0.068355218
# 24            redwood  211  Sequoia sempervirens 2017      0.021424332      0.2468932        0.067841494
# 25         sugar pine  117     Pinus lambertiana 2015      0.053011167      1.3104473        0.015637467
# 26         sugar pine  117     Pinus lambertiana 2016      0.056388306      1.3779503        0.014696721
# 27         sugar pine  117     Pinus lambertiana 2017      0.066584007      1.5662252        0.015391827
# 28          white fir   15        Abies concolor 2015      0.246932788      1.2755223        0.122947789
# 29          white fir   15        Abies concolor 2016      0.238599028      1.2427280        0.116755441
# 30          white fir   15        Abies concolor 2017      0.257970597      1.3384734        0.114953537
# > 

# pretty satisfied to see that these mortality estimates come out at about what I'd expect. Not terribly thrilled it is this easy to get them, but whatever. 
# some other functions that would be worth checking out: can we do growMort for years other than most recent? Does that require truncating the whole database, or perhaps doing a grpBy == INVYR?

gM_test <- growMort(ca, grpBy = MEASYEAR, polys = NULL,
                     returnSpatial = FALSE,
                     bySpecies = TRUE, 
                     bySizeClass = FALSE,
                     landType = 'forest',
                     treeType = 'all',
                     method = 'TI',
                     stateVar = 'BAA',
                     treeDomain = NULL,
                     areaDomain = NULL, 
                     totals = FALSE,
                     byPlot = FALSE, 
                     nCores = 4)
arrange(gM_test, MEASYEAR)
View(gM_test)

# this worked/ran. INVYR did not. The rows for abco are MEASYEAR 2011-2018 and YEAR 2015-2018. 

PLOT <- ca$PLOT
head(PLOT)
levels(as.factor(PLOT$INVYR))
#1994-2018.
dim(PLOT)


######################
## try the same steps with a different source dataset
# this db is the one I'm working from for data analysis, I think...(yes. and added on the 2016 data in an appendix)
ca <- readFIA('C:/Users/stellac/Dropbox/Work/Cal_Work/CA_mort_shared/Data/CA')
gM_test <- growMort(ca, grpBy = MEASYEAR, polys = NULL,
                    returnSpatial = FALSE,
                    bySpecies = TRUE, 
                    bySizeClass = FALSE,
                    landType = 'forest',
                    treeType = 'all',
                    method = 'TI',
                    stateVar = 'BAA',
                    treeDomain = NULL,
                    areaDomain = NULL, 
                    totals = FALSE,
                    byPlot = FALSE, 
                    nCores = 4)
arrange(gM_test, MEASYEAR)
View(gM_test)
#This only shows measyear 2011-2015, and year 2015. I sort of thought it might go farther back in time.... 