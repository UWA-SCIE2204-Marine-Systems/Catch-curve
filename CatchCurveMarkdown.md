Catch curve analysis
================
Matt Navarro
10/03/2020

## how to use R studio

By now you should have logged into an ecocloud R studio session and
created and saved an R script called something like ‘Catch curve.r’ in
your workspace folder.

If you haven’t done this you should go back nd check the instructions on
How to use ecocloud
<https://github.com/UWA-SCIE2204-Marine-Systems/How-to-use-ecoloud/blob/master/README.md>

You should now be looking at a window similar to
this:

</br>

![*blank*](https://github.com/UWA-SCIE2204-Marine-Systems/Catch-curve/blob/master/figure/blank.PNG)

</br>

Thoughout this document there are sections that look like this (notice
the blue-gray box around the text and different font):

``` r
library(RCurl)
```

    ## Warning: package 'RCurl' was built under R version 3.6.2

The text in these sections is R code. To run the code simply copy the
text inside these sections, open your ecocloud R studio session, and
paste the text into the top left corner box.

Do this now for the ‘library(RCurl)’ text above. Note: you can ignore
any sections with \# at the start such as the “\#\# Warning: package
‘RCurl’ was built under R version 3.6.2”.

Now highlight the text you just pasted and click on the run button shown
in the image
below:

![*run*](https://github.com/UWA-SCIE2204-Marine-Systems/Catch-curve/blob/master/figure/run.PNG)

You have just loaded some extra software into R studio (known as a
package) that we need for this lab.

Going through the exercises below repeat this process: (i) copy the
text, (ii) open R studio, paste the text in the top left corner, (iii)
highlight the text and click run. Do not try too hard to understand how
the code works - I will give you some notes on what the cod is doing
along the way. I am more interested in you learning how the actual catch
curve analysis works.

The order that you enter and run the text is important, so make sure you
don’t skip any sections.

At the end of the lab you should have pasted all of the code from this
document into the box in the top left corner of your R studio session.
If you click the save button you will be able to reopen this text
anytime and re-run the analysis by highlighting all of the code and
clicking run.

## Description of exercise

We are going to use a linear catch curve technique to determine whether
a stock is being overfished. The data are simulated, but are based
closely on the WA Dhufish
<http://www.fish.wa.gov.au/species/wa-dhufish/Pages/default.aspx>

This is a long lived endemic species in WA and is a key species in WA’s
recreaitonal and commercial fisheries.

We are going to present analysis based on two data sets. They each
contain age frequencies of 500 fish. The first is based on constant
recruitment - this means that the same number of juvenile fish enter the
fishery each year. The second is based on variable recruitment - this
means that the number of juvenile fish entering the fishery varies by
year

Variable recruitment is probably more realistic as recruitment for many
fish species is driven by oceanographic features (e.g. currents). We
will talk about this more in the third lecture.

For each data set we want to know:

\-Is the stock overfished? And,

\-Is our analysis reliable?

## Big thanks and a disclaimer

This script was prepared largely by Alex Hesp from WA Fisheries
(Department of Primary Industries and Regional Development) just for you
guys. Bit of a legend\!

### Disclaimer:

The author does not warrent that the information provided is free from
errors or omissions. The author do not accept any form of liability, for
the information provided, or for any consequences arising from its use
or any reliance placed upon it. The information, opinions and advice
provided may not relate, or be relevant, to a reader’s/user’s particular
circumstance. Readers must evaluate the relevance for their purposes of
the provided information, and actively seek review from the Department
prior to publishing or entering into any commitment based on the
information. Opinions expressed by the author are the individual
opinions expressed by that person and are not necessarily those of the
Department.

## Main analysis

### Load the constant recruitment data and have a look

``` r
data_const <- read.csv(text=getURL("https://raw.githubusercontent.com/UWA-SCIE2204-Marine-Systems/Catch-curve/master/AgeDatWithConstRec.csv"))
```

The code above downloads the data set and loads it into your R studio
session calling it ‘data\_const’. You should be able to see data\_const
in the top right box of Rstudio (under the “Environment” tab).

``` r
head(data_const)
```

    ##   Age Frequency
    ## 1   0         0
    ## 2   1         0
    ## 3   2         0
    ## 4   3         2
    ## 5   4         5
    ## 6   5        19

The head function shows you the top 6 rows of the data set. You can see
that there are two columns in the data: (i) age and (ii) frequency. you
should be able to see that there are no fish that are 2 years old, 5
that are 4 years old, and 19 that are 5 years
old.

``` r
plot(data_const$Age, data_const$Frequency, "o", main="Age sample data", cex.main=1.0, 
        xlab="Age class",ylab="Frequency", frame=F, ylim=c(0,80))
```

![](CatchCurveMarkdown_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
The plot function shows the data on a graph of age against frequency.

it shows that the sample consists of very few young fish.

Frequency peaks at 7 years at around 65 fish.it then trails off till
aroud 20 and is relatively flat above age 20.

### Setting up the data

For catch curve analysis we are only really interested in the ages that
the fishery is impacting on. A common assumption is to use the peak age
frequency + 1. looking at the plot we can see the peak frequency is 7
and so the cut off should be 8.

``` r
MinCutOff = data_const[which.max(data_const$Frequency),"Age"] + 1
MinCutOff
```

    ## [1] 8

``` r
plot(data_const$Age, data_const$Frequency, "o", main="Age sample data", cex.main=1.0,
        xlab="Age class",ylab="Frequency", frame=F, ylim=c(0,80))
abline(v = MinCutOff, col="red", lty=2)
```

![](CatchCurveMarkdown_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The code above found the peak age frequency (which we already knew was
7) and added 1 storing the result (8) as MinCutOff. We then simply added
a vertical line to our plot at age 8 to show where this minimum cut off
falls in our sample.

We are also not interested in really old fish that aren’t very common in
the sample. We will, somewhat arbitrarily, say that we are interested in
all ages up to the age where the frequency of fish is just 1. The code
below identifies the age where frequency first reaches 1 and then adds a
vertical line at this age to our plot.

``` r
MaxCutOff <- data_const[match( 1 , data_const$Frequency), "Age"]
plot(data_const$Age, data_const$Frequency, "o", main="Age sample data", cex.main=1.0,
        xlab="Age class",ylab="Frequency", frame=F, ylim=c(0,80))
abline(v = c(MinCutOff, MaxCutOff), col="red", lty=2)
```

![](CatchCurveMarkdown_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

For the catch curve analysis we are only really interested in the data
above the minimum age (8) and below the maximum age (25).

To help us focus on just the age groups we are interested in we can take
a data subset. The code below extracts the data for the ages we are
interested in and creates a new data set called data\_analysis. It also
plots this data subset providing us with a better view of the data we
are interested
in.

``` r
data_analysis<- data_const[ data_const$Age >= MinCutOff & data_const$Age <= MaxCutOff,  ]
plot(data_analysis$Age, data_analysis$Frequency, "o", main="Age sample data", cex.main=1.0,
        xlab="Age class",ylab="Frequency", frame=F)
```

![](CatchCurveMarkdown_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Catch curve analysis - estimating total mortality

Catch curve analysis involves fitting a straight line to our subset of
the age frequency data. Do you think that a straight line would fit this
data well? I don’t really think so - at age 20+ the data is more or less
flat.

One option to fix this is to perform a natural log transformation of the
frequency variable. A natural log transformation takes the frequency
variable and replaced it with the natural lof the frequency - e.g. a
frequency of 10 will become ln(10) = 2.3. Note that the log function in
R by default takes the natural logarithm.

The code below shows what our data looks like if we plot the nl of
frequency against
age.

``` r
plot(data_analysis$Age, log(data_analysis$Frequency), "o", main="Age sample data", cex.main=1.0,
        xlab="Age class",ylab="log of frequency", frame=F)
```

![](CatchCurveMarkdown_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

That looks better\! Notice that the data seems to (fairly closely)
resemble a straight line.

Now lets fit a model.

The code below fits a linear model to the log of frequency against age
for our data subset. Basically it fits a straight line through the above
plot.

``` r
CatchCurveModel <- lm(log(Frequency) ~ Age, data = data_analysis)
```

The code above fits a linear model to the log of frequency against age
for our data subset. Basically it fits a straight line through the above
plot.

We can have a look at how the line fits through our data (don’t worry
about figuring out how the code works)

``` r
EstlnFreq <- predict(lm(log(Frequency) ~ Age, data = data_analysis))
plot(data_analysis$Age, log(data_analysis$Frequency), "o", main="Age sample data", cex.main=1.0,
        xlab="Age class",ylab="Frequency", frame=F)
lines(data_analysis$Age,EstlnFreq,"l",col="blue")
```

![](CatchCurveMarkdown_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Seems like a reasonable fit.

Now, the whole reason we made this model was to estimate the total
mortality (Z) in the population. We can do this by looking at how
frequency drops off with age across our data subset - that is the slope
of the blue line in our plot.

We can have a look at the slope by looking at the model summary.

``` r
summary(CatchCurveModel)
```

    ## 
    ## Call:
    ## lm(formula = log(Frequency) ~ Age, data = data_analysis)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.46352 -0.21135 -0.04866  0.12990  0.91349 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.12123    0.28128   21.76 2.59e-13 ***
    ## Age         -0.22795    0.01626  -14.02 2.10e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.358 on 16 degrees of freedom
    ## Multiple R-squared:  0.9247, Adjusted R-squared:   0.92 
    ## F-statistic: 196.5 on 1 and 16 DF,  p-value: 2.101e-10

``` r
Z <- CatchCurveModel$coefficients[[2]]*-1
Z
```

    ## [1] 0.2279549

The slope, which is equal to the total mortality rate (Z), is next to
Age. It is 0.23. in the above code we also stored the total mortality in
R calling it Z - you should notice ‘Z’ appearing in the top right box
under ‘Environment’ along with the data sets we have loaded or created.

### Is overfishing occuring?

You will recall from the lecture that a rule of thumb is that
overfishing is not ocuring if the fishing mortality rate is less than
the natural mortality rate. To start to determine if this is true we
need to estimate the natural mortality rate.

Natural mortality can be estimated using the maximum observed age for
the fish species which is 40, and using the formula below based on the
paper at: <https://paperpile.com/shared/GSKSGX> We talked more about how
to do this in the lecture. We are going to store the natural mortality
rate in R studio as ‘M’

``` r
M <- exp(1.46 - 1.01*log(40))
M
```

    ## [1] 0.1037503

So we now know that total mortality (Z) is 0.23 and natural mortality
(M) is 0.10.

if we assume that all fish either die naturally or are caught, so we can
estimate fishing mortality by F = Z - M. As we have stored in R the
total mortality as Z, and natural mortality as M, we can estimate R
using the following code:

``` r
F <- Z - M
F
```

    ## [1] 0.1242046

Now that we know both the fishing mortality rate (F) and natural
mortality rate (M) we can assess whether overfishing is occuring.
Remember from the lecture that we said a rule of thumb in fisheries
science is that if the fishing mortality rate (F) is greater than the
natural mortality rate (M) than the fish population is being overfished.
We can assess whether F is greater than M by using the code:

``` r
F > M
```

    ## [1] TRUE

So F is 0.12 and M is 0.10, so it appears that overfishing is indeed
occuring, or atleast accoding to this analysis.

## variable recruitment data

Lets now look at the variable recrtuitment data. We will run fairly
quickly through this, so don’t try and understand the code. It is more
or less the same as the code you just used for the constant recruitment
data.

Firstly lets load the variable recruitment data and plot it side by side
with the constant recruitment
data:

``` r
data_var=read.csv("C:/GitHub/SCIE2204/Catch curve/AgeDatWithRecVar.csv", header=T, row.names = NULL)

par(mfrow=c(1,2))
plot(data_var$Age, data_var$Frequency, "o", main="Variable recruitment", cex.main=1.0, 
        xlab="Age class",ylab="Frequency", frame=F, ylim = c(0,130))
plot(data_const$Age, data_const$Frequency, "o", main="Constant recruitment", cex.main=1.0, 
        xlab="Age class",ylab="Frequency", frame=F, ylim = c(0,130))
```

![](CatchCurveMarkdown_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

The plots here shows the variable recruitment data on the left, and the
constant recruitment data (from the previous section) on the right.

Notice that the variable recruitment frequency data looks quite
different to the constant recruitment data. Most importantly there is a
huge spike in frequency at age 11 with around 120 individuals.

Recruitment spikes might result from a “good year” where currents were
favourable and/or spawning was highly successful.

You might be able to tell already that this is going to complicate our
analysis.

Lets first look at the minimum and maximum cutoffs:

``` r
MinCutOff = data_var[which.max(data_var$Frequency),"Age"] + 1
MaxCutOff <- data_var[match( 1 , data_var$Frequency), "Age"]
plot(data_var$Age, data_var$Frequency, "o", main="Age sample data", cex.main=1.0,
        xlab="Age class",ylab="Frequency", frame=F, ylim=c(0,130))
abline(v = c(MinCutOff, MaxCutOff), col="red", lty=2)
```

![](CatchCurveMarkdown_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Instantly because of this recruitment spike, the minimum cut off has
shifted to 12. It was 8 in our last analysis.

Lets see what happens when we fit the
model.

``` r
data_analysis<- data_var[ data_var$Age >= MinCutOff & data_var$Age <= MaxCutOff,  ]

CatchCurveModel_var <- lm(log(Frequency) ~ Age, data = data_analysis)
EstlnFreq <- predict(lm(log(Frequency) ~ Age, data = data_analysis))
plot(data_analysis$Age, log(data_analysis$Frequency), "o", main="Age sample data", cex.main=1.0,
        xlab="Age class",ylab="Frequency", frame=F)
lines(data_analysis$Age,EstlnFreq,"l",col="blue")
```

![](CatchCurveMarkdown_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

The model does not seem to fit as well as it did for the contant
recruitment data. In particular, there seems to be an outlier at age 16
- it suggests there are less 16 year old fish in the sample then we
might expect.

``` r
Z <- CatchCurveModel_var$coefficients[[2]]*-1
Z
```

    ## [1] 0.1839866

Our total mortality rate (Z) is now 0.18. This is much lower than the
previous estimate of 0.23.

The rate of natural mortality rate (M) for dhufish is still the same
(0.10) as it is based on the maximum observed age of the species (40).
Now that we have the total and natural mortality rate We can go ahead
and estimate the fishing mortality rate suggested by the variable
recruitment data.

``` r
F <- Z - M
F
```

    ## [1] 0.08023633

The fishing mortality rate is 0.08.

This is less than the narural mortality rate for WA Dhufish of 0.10,
suggesting that we don’t think this population is overfished.

## Some words of caution

Both data sets we used were simulated and in principle have the same
“true” fishing mortality rates. Despite this, we concluded that the
constant recruitment population was overfished whilst the variable
recruitment population wasn’t. This highlights that assumptions we make
in fitting models can have large effects on the outcomes.

Some of the assuptions of our catch curve analysis include:

1)  Fishing mortality has been constant. Remember we estimated just one
    number for all fish aged 8 to 25 (in the constant recruitment data).

2)  Annual recruitment is constant. This was inherent in our first
    dataset but is violated by our second data set.

3)  At our minimum cut-off of peak frequency + 1 the fish population is
    fully exposed to the fishery (we say fully recruited)

4)  Our sample reflects the underlying age structure of the population

5)  All fish are aged correctly - it can be quite tricky to count
    otolith rings

6)  The relationship between maximum age and natural mortlity rate is
    true for our species

7)  The rule of thumb that Fmsy = M is true. This is a very coarse
    approximation.

Often in assessing a fish stock a whole range of methods will be used
with different sets of assumptions. For those interested, you can see a
good example in the 2007 stock assessment for WA Dhufish (which used
catch curve analysis amongst other methods)
<https://paperpile.com/shared/pj8qFH>
