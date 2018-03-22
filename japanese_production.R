## Bodo Winter
## Sep 21, 2017
## Analysis of Japanese production data

##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Packages:

library(lme4)
library(afex)
library(stringr)
library(tidyverse)
library(here)

## Load in data:

setwd(here('data'))
jprod <- read_csv('JPNpolitenessProduction.csv')

## Create unique identifier for item:

jprod <- mutate(jprod,
	item = str_c(langtask, ':', scenario))

## The "FALSE" for Speaker 6 is casual:

jprod[jprod$condition2 == 'FALSE', ]$condition2 <- 'casual'



##------------------------------------------------------------------
## Get mora counts:
##------------------------------------------------------------------

## Load in mora counts:

mora_EPR <- read_csv('JPN_moras_EPrime.csv')
mora_PPT <- read_csv('JPN_moras_PPT.csv')

## Rename columns:

colnames(mora_EPR)[2:ncol(mora_EPR)] <- str_c(c('pol', 'cas'),
	'_', rep(1:10, each = 2))
colnames(mora_PPT)[2:ncol(mora_PPT)] <- str_c(c('cas', 'pol'),
	'_', rep(1:6, each = 2))

## Make into long format:

mora_PPT_long <- gather(mora_PPT,
	key = condition, value = moras, -Subject)
mora_EPR_long <- gather(mora_EPR,
	key = condition, value = moras, -Subject)

## Merge together and add task info:

moras <- bind_rows(mora_PPT_long, mora_EPR_long)
moras$task <- c(rep('ppt', nrow(mora_PPT_long)),
	rep('epr', nrow(mora_EPR_long)))

## Split condition information for matching:

moras <- moras %>%
	separate(condition, into = c('condition1', 'scenario'))

## Clean subject columns:

moras <- mutate(moras,
	Subject = str_extract(Subject, '([0-9])+')) %>%
	rename(speaker = Subject) %>%
	mutate(speaker = as.integer(speaker),
		scenario = as.integer(scenario))

## Merge together:

jprod <- left_join(jprod, moras)

## Calculate speech rate:

jprod <- mutate(jprod,
	rate = moras / dur)



##------------------------------------------------------------------
## Create effect coded variables:
##------------------------------------------------------------------

## Make all into factor:

jprod <- mutate(jprod,
	condition2_c = ifelse(condition2 == 'polite', 0.5, -0.5),
	gender_c = ifelse(gender == 'M', 0.5, -0.5),
	task_c = ifelse(task == 'epr', -0.5, 0.5))

# ## Contrast-code:

# contrasts(jprod$condition2_c) <- contr.sum(2) / 2
# contrasts(jprod$gender_c) <- contr.sum(2) / 2
# contrasts(jprod$task_c) <- contr.sum(2) / 2


##------------------------------------------------------------------
## For plotting, create z-scored dependent variables:
##------------------------------------------------------------------

## Log-transform duration:

jprod <- mutate(jprod,
	duroriginal = dur,
	dur = log10(dur))

## Z-scoring:

jprod_z <- mutate(jprod,
	dur = (dur - mean(dur, na.rm = T)) / sd(dur, na.rm = T),
	rate = (rate - mean(rate, na.rm = T)) / sd(rate, na.rm = T),
	inmn = (inmn - mean(inmn, na.rm = T)) / sd(inmn, na.rm = T),
	inmd = (inmd - mean(inmd, na.rm = T)) / sd(inmd, na.rm = T),
	f0mnhz = (f0mnhz - mean(f0mnhz, na.rm = T)) / sd(f0mnhz, na.rm = T),
	inrange = (inrange - mean(inrange, na.rm = T)) / sd(inrange, na.rm = T),
	f0sdhz = (f0sdhz - mean(f0sdhz, na.rm = T)) / sd(f0sdhz, na.rm = T),
	f0rghz = (f0rghz - mean(f0rghz, na.rm = T)) / sd(f0rghz, na.rm = T),
	jitloc = (jitloc - mean(jitloc, na.rm = T)) / sd(jitloc, na.rm = T),
	shimloc = (shimloc - mean(shimloc, na.rm = T)) / sd(shimloc, na.rm = T),
	h1mh2mn = (h1mh2mn - mean(h1mh2mn, na.rm = T)) / sd(h1mh2mn, na.rm = T),
	mnHNR = (mnHNR - mean(mnHNR, na.rm = T)) / sd(mnHNR, na.rm = T))



##------------------------------------------------------------------
## Mixed models with likelihood ratio tests:
##------------------------------------------------------------------

## Duration:

print(j.dur <- mixed(dur ~ condition2_c * (gender_c + task_c) +
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = jprod, method = 'LRT'))
aggregate(duroriginal ~ condition2, data = jprod, mean) %>%
	mutate(duroriginal = round(duroriginal, 1))
aggregate(duroriginal ~ condition2, data = jprod, sd) %>%
	mutate(duroriginal = round(duroriginal, 1))
aggregate(duroriginal ~ condition2 * task, data = jprod, mean) %>%
	mutate(duroriginal = round(duroriginal, 1))
mean(jprod$duroriginal)

## Rate:

print(j.rate <- mixed(rate ~ condition2_c * (gender_c + task_c) +
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = jprod, method = 'LRT'))
aggregate(rate ~ condition2, data = jprod, mean, na.rm = TRUE) %>%
	mutate(rate = round(rate, 1))
aggregate(rate ~ condition2, data = jprod, sd, na.rm = TRUE) %>%
	mutate(rate = round(rate, 1))
aggregate(rate ~ task, data = jprod, mean, na.rm = TRUE) %>%
	mutate(rate = round(rate, 1))
aggregate(rate ~ task, data = jprod, sd, na.rm = TRUE) %>%
	mutate(rate = round(rate, 1))
aggregate(rate ~ gender, data = jprod, mean, na.rm = TRUE) %>%
	mutate(rate = round(rate, 1))
aggregate(rate ~ gender, data = jprod, sd, na.rm = TRUE) %>%
	mutate(rate = round(rate, 1))
mean(jprod$rate, na.rm = TRUE)

## Intensity:

print(j.in <- mixed(inmd ~ condition2_c * (gender_c + task_c) +
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = jprod, method = 'LRT'))
aggregate(inmd ~ condition2, data = jprod, mean) %>%
	mutate(inmd = round(inmd, 1))
aggregate(inmd ~ condition2, data = jprod, sd) %>%
	mutate(inmd = round(inmd, 1))
aggregate(inmd ~ task, data = jprod, mean) %>%
	mutate(inmd = round(inmd, 1))
aggregate(inmd ~ condition2 * task, data = jprod, mean) %>%
	mutate(inmd = round(inmd, 1))

## Intensity range:

print(j.inrange <- mixed(inrange ~ condition2_c * (gender_c + task_c) +
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = jprod, method = 'LRT'))

## Pitch:

print(j.f0 <- mixed(f0mnhz ~ condition2_c * (gender_c + task_c) +
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = jprod, method = 'LRT'))
aggregate(f0mnhz ~ condition2, data = jprod, mean) %>%
	mutate(f0mnhz = round(f0mnhz, 1))
aggregate(f0mnhz ~ condition2, data = jprod, sd) %>%
	mutate(f0mnhz = round(f0mnhz, 1))
aggregate(f0mnhz ~ condition2 * task, data = jprod, mean) %>%
	mutate(f0mnhz = round(f0mnhz, 1))

## Pitch variability:

print(j.f0sd <- mixed(f0sdhz ~ condition2_c * (gender_c + task_c) +
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = jprod, method = 'LRT'))
aggregate(f0sdhz ~ condition2, data = jprod, mean) %>%
	mutate(f0sdhz = round(f0sdhz, 1))
aggregate(f0sdhz ~ gender, data = jprod, mean) %>%
	mutate(f0sdhz = round(f0sdhz, 1))
aggregate(f0sdhz ~ condition2 * task, data = jprod, mean) %>%
	mutate(f0sdhz = round(f0sdhz, 1))

## Pitch range:

print(j.f0range <- mixed(f0rghz ~ condition2_c * (gender_c + task_c) +
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = jprod, method = 'LRT'))

## Jitter:

print(j.jit <- mixed(jitloc ~ condition2_c * (gender_c + task_c) +
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = jprod, method = 'LRT'))
aggregate(jitloc ~ condition2 * task, data = jprod, mean)

## Shimmer:

print(j.shim <- mixed(shimloc ~ condition2_c * (gender_c + task_c) +
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = jprod, method = 'LRT'))

## H1-H2:

print(j.h1h2 <- mixed(h1mh2mn ~ condition2_c * (gender_c + task_c) +
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = jprod, method = 'LRT'))
aggregate(h1mh2mn ~ condition2, data = jprod, mean) %>%
	mutate(h1mh2mn = round(h1mh2mn, 1))
aggregate(h1mh2mn ~ condition2, data = jprod, sd) %>%
	mutate(h1mh2mn = round(h1mh2mn, 1))

## HNR:

print(j.HNR <- mixed(mnHNR ~ condition2_c * (gender_c + task_c) +
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = jprod, method = 'LRT'))
aggregate(mnHNR ~ condition2, data = jprod, mean) %>%
	mutate(mnHNR = round(mnHNR, 1))
aggregate(mnHNR ~ condition2 * task, data = jprod, mean) %>%
	mutate(mnHNR = round(mnHNR, 1))


##------------------------------------------------------------------
## Create marginal models for plotting with z-scored data, Japanese:
##------------------------------------------------------------------

## Duration:

print(j.ppt.dur <- mixed(dur ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'ppt'), method = 'LRT'))
print(j.epr.dur <- mixed(dur ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'epr'), method = 'LRT'))

## Rate:

print(j.ppt.rate <- mixed(rate ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'ppt'), method = 'LRT'))
print(j.epr.rate <- mixed(rate ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'epr'), method = 'LRT'))

## Intensity:

print(j.ppt.in <- mixed(inmd ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'ppt'), method = 'LRT'))
print(j.epr.in <- mixed(inmd ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'epr'), method = 'LRT'))

## Intensity range:

print(j.ppt.inrange <- mixed(inrange ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'ppt'), method = 'LRT'))
print(j.epr.inrange <- mixed(inrange ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'epr'), method = 'LRT'))

## Pitch:

print(j.ppt.f0 <- mixed(f0mnhz ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'ppt'), method = 'LRT'))
print(j.epr.f0 <- mixed(f0mnhz ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'epr'), method = 'LRT'))

## Pitch variability:

print(j.ppt.f0sd <- mixed(f0sdhz ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'ppt'), method = 'LRT'))
print(j.epr.f0sd <- mixed(f0sdhz ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'epr'), method = 'LRT'))

## Pitch range:

print(j.ppt.f0range <- mixed(f0rghz ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'ppt'), method = 'LRT'))
print(j.epr.f0range <- mixed(f0rghz ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'epr'), method = 'LRT'))

## Shimmer:

print(j.ppt.shim <- mixed(shimloc ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'ppt'), method = 'LRT'))
print(j.epr.shim <- mixed(shimloc ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'epr'), method = 'LRT'))

## Jitter:

print(j.ppt.jit <- mixed(jitloc ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'ppt'), method = 'LRT'))
print(j.epr.jit <- mixed(jitloc ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'epr'), method = 'LRT'))

## H1-H2:

print(j.ppt.h1h2 <- mixed(h1mh2mn ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'ppt'), method = 'LRT'))
print(j.epr.h1h2 <- mixed(h1mh2mn ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'epr'), method = 'LRT'))

## HNR:

print(j.ppt.HNR <- mixed(mnHNR ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'ppt'), method = 'LRT'))
print(j.epr.HNR <- mixed(mnHNR ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = filter(jprod_z, task == 'epr'), method = 'LRT'))



##------------------------------------------------------------------
## Get Korean data:
##------------------------------------------------------------------

## Load in Korean data for comparison:

setwd(here('data'))
wg2012 <- read_csv('KRNpolitenessWinterGrawunder2012.csv')
brown2014 <- read_csv('KRNpolitenessProduction_2014data.csv')

## Extract item column from Winter & Grawunder 2012 data:

wg2012 <- separate(wg2012,
	interval,
	into = c('task2', 'item', 'conditioncode', 'condition'))

## Get Korean jamos:

wg2012jamo <- read_csv('KRNpolitenessWinterGrawunder2012jamo.csv')
setwd(here('data/brown2014_jamos'))
myjamo <- tibble(scenario = 1:10, con = NA, pan = NA)
for (i in 1:10) {
	suppressWarnings(scen <- readLines(list.files()[i]))
	scen <- str_split(scen, '\\.|\\?|,|( )')
	scen <- lapply(scen, FUN = function(x) x[x != ''])
	con_count <- sum(str_count(scen[[1]]))
	pan_count <- sum(str_count(scen[[2]]))
	this_scen <- as.numeric(str_extract(list.files()[i], '[0-9]+'))
	myjamo[myjamo$scenario == this_scen, ]$con <- con_count
	myjamo[myjamo$scenario == this_scen, ]$pan <- pan_count
	}
myjamo <- gather(myjamo, key = 'condition1', value = 'jamo', -scenario)

## Merge those into the respective files:

brown2014 <- left_join(brown2014, myjamo)
wg2012jamo <- mutate(wg2012jamo,
	# task = ifelse(task == 'not', 'note', 'dct'),
	scenario = ifelse(scenario > 5, scenario - 5, scenario),
	attitude = ifelse(attitude == 'inf', 'impol', 'pol'),
	scenario = as.character(scenario)) %>%
	rename(condition = attitude,
		item = scenario)
wg2012 <- left_join(wg2012, wg2012jamo)

## Create a duration column:

wg2012 <- mutate(wg2012,
	dur = inted - intst)

## Calculate rates:

wg2012 <- mutate(wg2012,
	rate = jamo / dur)
brown2014 <- mutate(brown2014,
	rate = jamo / dur)

## Process the Korean data:

wg2012 <- wg2012 %>%
	select(filename, gender, task, item, subject, attitude,
		dur, rate, f0mn, f0sd, inmd, jitloc, shimloc, h1mh2mn, mnHNR) %>%
	rename(language = filename, condition = attitude,
		speaker = subject)
brown2014 <- brown2014 %>%
	select(language, gender, task, scenario, speaker, condition2,
		dur, rate, f0mnhz, f0sdhz, inmd, jitloc, shimloc, h1mh2mn, mnHNR) %>%
	rename(condition = condition2, f0mn = f0mnhz, f0sd = f0sdhz,
		item = scenario)

## Extract only powerpoint task of Winter & Grawunder (2012) (exclude note task):

wg2012 <- filter(wg2012, task != 'not')

## Log-transform duration:

wg2012 <- mutate(wg2012,
	dur = log10(dur))
brown2014 <- mutate(brown2014,
	dur = log10(dur))

## Z-score everything, Winter & Grawunder (2012):

wg2012_z <- mutate(wg2012,
	dur = (dur - mean(dur, na.rm = T)) / sd(dur, na.rm = T),
	rate = (rate - mean(rate, na.rm = T)) / sd(rate, na.rm = T),
	inmd = (inmd - mean(inmd, na.rm = T)) / sd(inmd, na.rm = T),
	f0mn = (f0mn - mean(f0mn, na.rm = T)) / sd(f0mn, na.rm = T),
	f0sd = (f0sd - mean(f0sd, na.rm = T)) / sd(f0sd, na.rm = T),
	jitloc = (jitloc - mean(jitloc, na.rm = T)) / sd(jitloc, na.rm = T),
	shimloc = (shimloc - mean(shimloc, na.rm = T)) / sd(shimloc, na.rm = T),
	h1mh2mn = (h1mh2mn - mean(h1mh2mn, na.rm = T)) / sd(h1mh2mn, na.rm = T),
	mnHNR = (mnHNR - mean(mnHNR, na.rm = T)) / sd(mnHNR, na.rm = T))

## Z-score everything, Brown et al.(2014):

brown2014_z <- mutate(brown2014,
	dur = (dur - mean(dur, na.rm = T)) / sd(dur, na.rm = T),
	rate = (rate - mean(rate, na.rm = T)) / sd(rate, na.rm = T),
	inmd = (inmd - mean(inmd, na.rm = T)) / sd(inmd, na.rm = T),
	f0mn = (f0mn - mean(f0mn, na.rm = T)) / sd(f0mn, na.rm = T),
	f0sd = (f0sd - mean(f0sd, na.rm = T)) / sd(f0sd, na.rm = T),
	jitloc = (jitloc - mean(jitloc, na.rm = T)) / sd(jitloc, na.rm = T),
	shimloc = (shimloc - mean(shimloc, na.rm = T)) / sd(shimloc, na.rm = T),
	h1mh2mn = (h1mh2mn - mean(h1mh2mn, na.rm = T)) / sd(h1mh2mn, na.rm = T),
	mnHNR = (mnHNR - mean(mnHNR, na.rm = T)) / sd(mnHNR, na.rm = T))

## Make all into factor:

brown2014_z <- mutate(brown2014_z,
	condition2_c = ifelse(condition == 'polite', 0.5, -0.5),
	gender_c = ifelse(gender == 'M', 0.5, -0.5))
wg2012_z <- mutate(wg2012_z,
	condition2_c = ifelse(condition == 'pol', 0.5, -0.5),
	gender_c = ifelse(gender == 'M', 0.5, -0.5))



##------------------------------------------------------------------
## Create marginal models for plotting with z-scored data, Japanese:
##------------------------------------------------------------------

## Duration:

print(k.ppt.dur <- mixed(dur ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = wg2012_z, method = 'LRT'))
print(k.epr.dur <- mixed(dur ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = brown2014_z, method = 'LRT'))

## Duration:

print(k.ppt.rate <- mixed(rate ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = wg2012_z, method = 'LRT'))
print(k.epr.rate <- mixed(rate ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = brown2014_z, method = 'LRT'))

## Intensity:

print(k.ppt.in <- mixed(inmd ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = wg2012_z, method = 'LRT'))
print(k.epr.in <- mixed(inmd ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = brown2014_z, method = 'LRT'))

## Intensity range:

print(k.ppt.inrange <- mixed(inrange ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = wg2012_z, method = 'LRT'))
print(k.epr.inrange <- mixed(inrange ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = brown2014_z, method = 'LRT'))

## Pitch:

print(k.ppt.f0 <- mixed(f0mn ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = wg2012_z, method = 'LRT'))
print(k.epr.f0 <- mixed(f0mn ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = brown2014_z, method = 'LRT'))

## Pitch variability:

print(k.ppt.f0sd <- mixed(f0sd ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = wg2012_z, method = 'LRT'))
print(k.epr.f0sd <- mixed(f0sd ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = brown2014_z, method = 'LRT'))

## Pitch range:

print(k.ppt.f0range <- mixed(f0rghz ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = wg2012_z, method = 'LRT'))
print(k.epr.f0range <- mixed(f0rghz ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = brown2014_z, method = 'LRT'))

## Shimmer:

print(k.ppt.shim <- mixed(shimloc ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = wg2012_z, method = 'LRT'))
print(k.epr.shim <- mixed(shimloc ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = brown2014_z, method = 'LRT'))

## Jitter:

print(k.ppt.jit <- mixed(jitloc ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = wg2012_z, method = 'LRT'))
print(k.epr.jit <- mixed(jitloc ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = brown2014_z, method = 'LRT'))

## H1-H2:

print(k.ppt.h1h2 <- mixed(h1mh2mn ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = wg2012_z, method = 'LRT'))
print(k.epr.h1h2 <- mixed(h1mh2mn ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = brown2014_z, method = 'LRT'))

## HNR:

print(k.ppt.HNR <- mixed(mnHNR ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = wg2012_z, method = 'LRT'))
print(k.epr.HNR <- mixed(mnHNR ~ condition2_c + gender_c + 
	(1 + condition2_c|speaker) + (1 + condition2_c|item),
	data = brown2014_z, method = 'LRT'))




##------------------------------------------------------------------
## Make a graph of intensity, duration, f0, f0sd, jitter, HNR
##------------------------------------------------------------------

## Create vector with all prediction data frame names:

mypreds <- c('dur', 'rate', 'in', 'f0', 'f0sd',
	'jit', 'shim', 'h1h2', 'HNR')

## Plotting parameters:

x_fac <- 0.2

## Make the plot:

quartz('', 11, 6)
par(mai = rep(0.1, 4), mfrow = c(2, 1), omi = c(0.75, 1.5, 0.25, 0.25))
# Plot 1:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n',
	xlab = '', ylab = '', xlim = c(0.5, length(mypreds) * 2 - 0.5),
	ylim = c(-1, 2))
abline(h = 0, lwd = 1.5, lty = 2)
axis(side = 2, at = seq(-1, 2, 0.5),
	las = 2, lwd.ticks = 2, font = 2, cex.axis = 1.05)
text(labels = 'Polite - informal difference',
	x = -2.25, y = -1.25,
	font = 2, cex = 1.5, xpd = NA, srt = 90)
text(labels = '(z-scores)', 
	x = -1.7, y = -1.25,
	font = 2, cex = 1.25, xpd = NA, srt = 90)
# Plot 1, data:
for (i in seq_along(mypreds)) {
	jap_ppt <- summary(get(str_c('j.ppt.', mypreds[i]))$full.model)
	jap_ppt <- jap_ppt$coefficients['condition2_c', ]

	kor_ppt <- summary(get(str_c('k.ppt.', mypreds[i]))$full.model)
	kor_ppt <- kor_ppt$coefficients['condition2_c', ]

	# Korean:

	segments(x0 = seq(1, 18, 2)[i] - x_fac,
		y0 = kor_ppt[1] - 1.96 * kor_ppt[2],
		y1 = kor_ppt[1] + 1.96 * kor_ppt[2],
		lwd = 2, col = 'grey55')

	points(x = seq(1, 18, 2)[i] - x_fac,
		y = kor_ppt[1],
		pch = 21,
		cex = 1.15,
		col = 'black', lwd = 2,
		bg = 'white')

	## Japanese:

	segments(x0 = seq(1, 18, 2)[i] + x_fac,
		y0 = jap_ppt[1] - 1.96 * jap_ppt[2],
		y1 = jap_ppt[1] + 1.96 * jap_ppt[2],
		lwd = 2)
	
	points(x = seq(1, 18, 2)[i] + x_fac,
		y = jap_ppt[1],
		pch = 15,
		cex = 1.15)

	}
legend('topright',
	legend = c('Japanese', 'Korean'),
	pch = c(15, 21),
	col = 'black',
	pt.bg = c('black', 'white'), lwd = 2, cex = 1.1, box.lwd = 2)
text(x = 0.38, y = 1.85, labels = '(a)', font = 2, cex = 1.25)
mtext('Discourse Completion Task', line = -2, xpd = NA, font = 2,
	cex = 1.6)
box(lwd = 2)
# Plot 2:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n',
	xlab = '', ylab = '', xlim = c(0.5, length(mypreds) * 2 - 0.5),
	ylim = c(-1, 2))
abline(h = 0, lwd = 1.5, lty = 2)
axis(side = 2, at = seq(-1, 2, 0.5),
	las = 2, lwd.ticks = 2, font = 2, cex.axis = 1.15)
# Plot 2, data:
for (i in seq_along(mypreds)) {
	jap_epr <- summary(get(str_c('j.epr.', mypreds[i]))$full.model)
	jap_epr <- jap_epr$coefficients['condition2_c', ]

	kor_epr <- summary(get(str_c('k.epr.', mypreds[i]))$full.model)
	kor_epr <- kor_epr$coefficients['condition2_c', ]

	# Korean:

	segments(x0 = seq(1, 19, 2)[i] - x_fac,
		y0 = kor_epr[1] - 1.96 * kor_epr[2],
		y1 = kor_epr[1] + 1.96 * kor_epr[2],
		lwd = 2, col = 'grey55')

	points(x = seq(1, 18, 2)[i] - x_fac,
		y = kor_epr[1],
		pch = 21,
		cex = 1.15,
		col = 'black', lwd = 2,
		bg = 'white')

	## Japanese:

	segments(x0 = seq(1, 19, 2)[i] + x_fac,
		y0 = jap_epr[1] - 1.96 * jap_epr[2],
		y1 = jap_epr[1] + 1.96 * jap_epr[2],
		lwd = 2)
	
	points(x = seq(1, 18, 2)[i] + x_fac,
		y = jap_epr[1],
		pch = 15,
		cex = 1.15)

	}
mtext('Reading Task', line = -2, xpd = NA, font = 2,
	cex = 1.6)
text(x = 0.38, y = 1.85, labels = '(b)', font = 2, cex = 1.25)
axis(side = 1, at = seq(1, 18, 2),
	font = 2, cex.axis = 1.25,
	labels = c('Duration', 'Rate', 'Intensity', 'Pitch', 'Pitch SD',
		'Jitter', 'Shimmer', 'H1-H2', 'HNR'),
	lwd = 2, lwd.ticks = 2)
legend('topright',
	legend = c('Japanese', 'Korean'),
	pch = c(15, 21),
	col = 'black',
	pt.bg = c('black', 'white'), lwd = 2, cex = 1.1, box.lwd = 2)
box(lwd = 2)




##------------------------------------------------------------------
## See what predicts politeness:
##------------------------------------------------------------------

# ## Central imputation of missing values:

# jprod_z_impute <- jprod_z
# jprod_z_impute[is.na(jprod_z_impute$rate), ]$rate <- mean(jprod_z_impute$rate,
	# na.rm = TRUE)
# jprod_z_impute <- mutate(jprod_z_impute,
	# condition2 = as.factor(condition2))

# library(ranger)
# jap.forest <- ranger(condition2 ~ dur + rate + inmd + f0mnhz + f0sdhz +
	# jitloc + shimloc + h1mh2mn + mnHNR, data = jprod_z_impute,
	# importance = 'permutation', num.trees = 3000, mtry = 3)

# mypreds

