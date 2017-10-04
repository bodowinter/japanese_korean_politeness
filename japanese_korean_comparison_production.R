## Bodo Winter
## Sep 24, 2017
## Comparison of Japanese and Korean production data

##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Packages:

library(lme4)
library(afex)
library(stringr)
library(tidyverse)

## Load in data:

setwd('/Users/winterb/Research/politeness/japanese_full/analysis/data/')
jprod <- read_csv('JPNpolitenessProduction.csv')
wg2012 <- read_csv('KRNpolitenessWinterGrawunder2012.csv')
brown2014 <- read_csv('KRNpolitenessProduction_2014data.csv')

## Create unique identifier for item:

jprod <- mutate(jprod,
	item = str_c(langtask, ':', scenario))

## The "FALSE" for Speaker 6 is casual:

jprod[jprod$condition2 == 'FALSE', ]$condition2 <- 'casual'

## Extract item column from Winter & Grawunder 2012 data:

wg2012 <- separate(wg2012,
	interval,
	into = c('task2', 'item', 'conditioncode', 'condition'))

## Create a duration column:

wg2012 <- mutate(wg2012,
	dur = inted - intst)



##------------------------------------------------------------------
## Merge files:
##------------------------------------------------------------------

## Use median for comparison because that's the only thing in W&G2012:

## Reduce dataframes to overlapping variables and rename:

jprod <- jprod %>%
	select(language, gender, task, item, speaker, condition2,
		dur, f0mnhz, f0sdhz, inmd, jitloc, shimloc, h1mh2mn, mnHNR) %>%
	rename(condition = condition2,
		f0mn = f0mnhz, f0sd = f0sdhz) %>%
	mutate(speaker = as.character(speaker))
wg2012 <- wg2012 %>%
	select(filename, gender, task, item, subject, attitude,
		dur, f0mn, f0sd, inmd, jitloc, shimloc, h1mh2mn, mnHNR) %>%
	rename(language = filename, condition = attitude,
		speaker = subject)
brown2014 <- brown2014 %>%
	select(language, gender, task, scenario, speaker, condition2,
		dur, f0mnhz, f0sdhz, inmd, jitloc, shimloc, h1mh2mn, mnHNR) %>%
	rename(condition = condition2, f0mn = f0mnhz, f0sd = f0sdhz,
		item = scenario)

## Give them unique identifiers to not confuse them:

brown2014 <- mutate(brown2014,
	item = str_c('brown2014_', item),
	speaker = str_c('brown2014_', speaker))

## Merge Powerpoint ('ppt') tasks for both languages:

jppt <- filter(jprod, task == 'ppt')
jepr <- filter(jprod, task == 'epr')
wg2012 <- filter(wg2012, task != 'not')

## Bind together:

dct <- bind_rows(jppt, wg2012)	# discourse completion task
epr <- bind_rows(jepr, brown2014)	# discourse completion task

## Rename content to match:

dct <- mutate(dct,
	condition = ifelse(condition == 'casual', 'inf', condition),
	condition = ifelse(condition == 'polite', 'pol', condition))
epr <- mutate(epr,
	condition = ifelse(condition == 'casual', 'inf', condition),
	condition = ifelse(condition == 'polite', 'pol', condition))

## Fix the language column:

dct <- mutate(dct,
	language = str_sub(language, 1, 3))
epr <- mutate(epr,
	language = str_sub(language, 1, 3))

## Log-transform durations:

dct <- mutate(dct,
	dur = log10(dur))
epr <- mutate(epr,
	dur = log10(dur))

## For exploratory plotting, change variables ('y' below):

# jprod %>%
	# ggplot(aes(x = task, y = h1mh2mn, fill = condition2)) +
	# geom_boxplot()


##------------------------------------------------------------------
## Perform mixed models, DCT:
##------------------------------------------------------------------

## Duration:

print(ppt.dur <- mixed(dur ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = dct, method = 'LRT'))

## Intensity:

print(ppt.in <- mixed(inmd ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = dct, method = 'LRT'))

## Pitch:

print(ppt.f0 <- mixed(f0mn ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = dct, method = 'LRT'))

## Pitch standard deviation:

print(ppt.f0sd <- mixed(f0sd ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = dct, method = 'LRT'))

## Jitter:

print(ppt.jit <- mixed(jitloc ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = dct, method = 'LRT'))

## Shimmer:

print(ppt.shim <- mixed(shimloc ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = dct, method = 'LRT'))

## H1-H2:

print(ppt.h1h2 <- mixed(h1mh2mn ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = dct, method = 'LRT'))

## HNR:

print(ppt.hnr <- mixed(mnHNR ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = dct, method = 'LRT'))


##------------------------------------------------------------------
## Perform mixed models, epr:
##------------------------------------------------------------------

## Pitch:

print(ppt.f0 <- mixed(f0mn ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = epr, method = 'LRT'))

## Intensity:

print(ppt.in <- mixed(inmd ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = epr, method = 'LRT'))

## Pitch standard deviation:

print(ppt.f0sd <- mixed(f0sd ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = epr, method = 'LRT'))

## Duration:

print(ppt.dur <- mixed(dur ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = epr, method = 'LRT'))

## Jitter:

print(ppt.jit <- mixed(jitloc ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = epr, method = 'LRT'))

## Shimmer:

print(ppt.shim <- mixed(shimloc ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = epr, method = 'LRT'))

## H1-H2:

print(ppt.h1h2 <- mixed(h1mh2mn ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = epr, method = 'LRT'))

## HNR:

print(ppt.hnr <- mixed(mnHNR ~ condition * language + (1 + condition|speaker) + (1 + condition|item),
	data = epr, method = 'LRT'))



##------------------------------------------------------------------
## Perform mixed models, both models:
##------------------------------------------------------------------

## Create one data frame with everything together in one model:

both <- bind_rows(epr, dct)

## Make task labels consistent:

both <- mutate(both,
	task = ifelse(task == 'ppt', 'dct', task))

## Duration:

print(both.dur <- mixed(dur ~ condition * language +
	(1 + condition|speaker) + (1 + condition|item),
	data = both, method = 'LRT'))

## Intensity:

print(both.in <- mixed(inmd ~ condition * language +
	(1 + condition|speaker) + (1 + condition|item),
	data = both, method = 'LRT'))

## Pitch:

print(both.f0 <- mixed(f0mn ~ condition * language +
	(1 + condition|speaker) + (1 + condition|item),
	data = both, method = 'LRT'))

## Pitch standard deviation:

print(both.f0sd <- mixed(f0sd ~ condition * language +
	(1 + condition|speaker) + (1 + condition|item),
	data = both, method = 'LRT'))
	
## Jitter:

print(both.jit <- mixed(jitloc ~ condition * language +
	(1 + condition|speaker) + (1 + condition|item),
	data = both, method = 'LRT'))

## Shimmer:

print(both.shim <- mixed(shimloc ~ condition * language +
	(1 + condition|speaker) + (1 + condition|item),
	data = both, method = 'LRT'))

## H1-H2:

print(both.h1h2 <- mixed(h1mh2mn ~ condition * language +
	(1 + condition|speaker) + (1 + condition|item),
	data = both, method = 'LRT'))

## HNR:

print(both.hnr <- mixed(mnHNR ~ condition * language +
	(1 + condition|speaker) + (1 + condition|item),
	data = both, method = 'LRT'))




