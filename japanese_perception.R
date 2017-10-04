## Bodo Winter
## Sep 21, 2017
## Analysis of Japanese perception data

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
jper <- read_csv('JPNpolitenessPerception.csv')

## Rename main dependent variable:

jper <- mutate(jper,
	Resp = `Decision.RESP[Trial]`)

## Create a global trial ID:
## (easier to deal with than with all the different trial IDs)

jper$GlobalTrial <- 1:160

## Get rid of those that have NAs for response:

jper_backup <- jper
jper <- filter(jper,
	!is.na(Resp))
1 - (nrow(jper) / nrow(jper_backup))	# 10.4% exclusion

## Loop through and get which sound file it is:

jper$ChosenFile <- NA
for (i in 1:nrow(jper)) {
	jper[i, ]$ChosenFile <- unlist(select(jper[i, ], SoundFile, SoundFile2))[jper[i, ]$Resp]
	}

## Get rid of those that have NAs for response:

jper_backup <- jper
jper <- filter(jper,
	!is.na(Resp))
1 - (nrow(jper) / nrow(jper_backup))	# 10.4% exclusion

## Rename chosen file:

jper$PoliteResponse <- str_split(jper$ChosenFile,
	pattern = '(_|\\.)',
	simplify = TRUE)[, 3]
jper <- mutate(jper,
	PoliteResponse = ifelse(PoliteResponse == 'cas', 'casual', 'polite'))

## Recode '`Running[Trial]`':

jper <- rename(jper,
	PairRepetition = `Running[Trial]`) %>%
	mutate(PairRepetition = ifelse(PairRepetition == 'SenarioList', 1, 2))

## Rename RT variable and log-transform:

jper <- rename(jper,
	RT = `Decision.RT[Trial]`) %>%
	mutate(LogRT = log10(RT + 1))

## Center continuous variables:

jper <- mutate(jper,
	LogRT_c = LogRT - mean(LogRT),
	GlobalTrial_c = GlobalTrial - mean(GlobalTrial),
	Trial_c = Trial - mean(Trial))

## Create accuracy measure:

jper$ACC <- NA
jper[jper$PoliteResponse == 'casual', ]$ACC <- 0
jper[jper$PoliteResponse == 'polite', ]$ACC <- 1

## Create accuracy factor measure for mixed model:

jper <- mutate(jper,
	ACC_fac = factor(ACC),
	Resp_fac = factor(Resp))

## Create items from file name:

jper <- mutate(jper,
	item = str_extract(SoundFile, '_[0-9]+_'),
	item = str_replace_all(item, '_', ''))

## Get speaker gender:

jper_speaker <- read_csv('JPNpoliteness_PerceptionStimuli_AcousticData.csv')
jper_speaker <- filter(jper_speaker, !duplicated(speaker)) %>%
	select(speaker, gender) %>%
	rename(SpeakerNumber = speaker, SpeakerSex = gender)
jper <- left_join(jper, jper_speaker)

## Get average accuracy:

mean(jper$ACC)



##------------------------------------------------------------------
## Logistic mixed regression analysis:
##------------------------------------------------------------------

## Effect code sex predictors:

jper <- mutate(jper,
	SpeakerSex = factor(SpeakerSex),
	Sex = factor(Sex))
contrasts(jper$SpeakerSex) <- contr.sum(2) / 2
contrasts(jper$Sex) <- contr.sum(2) / 2

## Center pair repetition:

jper <- mutate(jper,
	PairRepetition_c = PairRepetition - 1.5)

## Create main model:

print(ACC_mdl <- mixed(ACC_fac ~ SpeakerSex * Sex + Trial_c + PairRepetition_c + 
	(1|Subject) + (1|item) + (1|SpeakerNumber),
	data = jper, family = 'binomial', method = 'LRT'))

## Look at intercept of main model:

summary(ACC_mdl$full.model)

## Check accuracy by gender:

aggregate(ACC ~ SpeakerSex, jper, mean)
