#!/usr/bin/env Rscript

library(tidyverse)
library(ggbeeswarm)
library(egg)
library(viridis)
library(RColorBrewer)
library(ggrepel)

source('scripts/_include_options.R')
source('scripts/_include_functions.R')

# Comparative data are pulled from Weyher & Chiou 2013 poster

alldata = readRDS('checkpoints/alldata_checkpoints.rds')
age.nona = readRDS('checkpoints/age_nona.rds')

m.focals = subset(alldata,focal.as %in% 'AM')
f.focals = subset(alldata,focal.as %in% 'AF')

m.meta = unique(subset(m.focals,select=c('meta_id','dateC','focalAnimal','last_point')))
f.meta = unique(subset(f.focals,select=c('meta_id','dateC','focalAnimal','last_point')))

m.pts = sum(c(-10000,diff(m.focals$timestamp))>50,na.rm=T)
f.pts = sum(c(-10000,diff(f.focals$timestamp))>50,na.rm=T)

# sum(m.focals$behavior %in% c('R','F','T','G','B','P','I','H','J','K','O')))


# Male male aggression
m.focals$mm = with(m.focals,ifelse(!is.na(partner.as) & partner.as == 'AM',TRUE,FALSE))
m.focals$mf = with(m.focals,ifelse(!is.na(partner.as) & partner.as == 'AF',TRUE,FALSE))
f.focals$ff = with(f.focals,ifelse(!is.na(partner.as) & partner.as == 'AF',TRUE,FALSE))
f.focals$fm = with(f.focals,ifelse(!is.na(partner.as) & partner.as == 'AM',TRUE,FALSE))

# Displacement rates

d_rate = sum(with(m.focals,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl')) / (m.pts * 30 / 60)
sum(with(f.focals,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl')) / (f.pts * 30 / 60)


d_rate_m = sum(with(m.focals,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl' & mm)) / (m.pts * 30 / 60)
d_rate_f = sum(with(m.focals,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl' & mf)) / (m.pts * 30 / 60)
sum(with(f.focals,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl' & ff)) / (f.pts * 30 / 60)
sum(with(f.focals,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl' & fm)) / (f.pts * 30 / 60)

# Bootstrap

m.focals.split = split(m.focals,as.character(m.focals$meta_id))
f.focals.split = split(f.focals,as.character(f.focals$meta_id))

save(list=c('m.meta','m.focals','m.focals.split','f.meta','f.focals','f.focals.split'),file='kinda_aggression.RData')

if (!file.exists('checkpoints/m_displacement_resample.rds')) {
	m.displacement.resample = do.call(rbind,parallel::mclapply(1:500,function(i) {
		these.focals = as.character(sample(m.meta$meta_id,replace=T))
		m.this = do.call(rbind,m.focals.split[these.focals])
		pts.this = sum(c(-10000,diff(m.this$timestamp))>50,na.rm=T)
		data.frame(
			behavior = 'displacement',
			nrep=i,
			rate = sum(with(m.this,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl')) / (pts.this * 30 / 60),
			rate_m = sum(with(m.this,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl' & mm)) / (pts.this * 30 / 60),
			rate_f = sum(with(m.this,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl' & mf)) / (pts.this * 30 / 60)
		)
	},mc.cores=96))
	saveRDS(m.displacement.resample,file='checkpoints/m_displacement_resample.rds')
} else {
	m.displacement.resample = readRDS('checkpoints/m_displacement_resample.rds')
}
if (!file.exists('checkpoints/f_displacement_resample.rds')) {
	f.displacement.resample = do.call(rbind,parallel::mclapply(1:500,function(i) {
		these.focals = as.character(sample(f.meta$meta_id,replace=T))
		f.this = do.call(rbind,f.focals.split[these.focals])
		pts.this = sum(c(-10000,diff(f.this$timestamp))>50,na.rm=T)
		data.frame(
			behavior = 'displacement',
			nrep=i,
			rate = sum(with(f.this,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl')) / (pts.this * 30 / 60),
			rate_m = sum(with(f.this,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl' & fm)) / (pts.this * 30 / 60),
			rate_f = sum(with(f.this,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl' & ff)) / (pts.this * 30 / 60)
		)
	},mc.cores=96))
	saveRDS(f.displacement.resample,file='checkpoints/f_displacement_resample.rds')
} else {
	f.displacement.resample = readRDS('checkpoints/f_displacement_resample.rds')
}

dspl.rates = data.frame(
	behavior = 'displacement',
	sex = c(rep('male',3),rep('female',3)),
	partner = c('all','male','female','all','male','female'),
	rate = c(
		sum(with(m.focals,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl')) / (m.pts * 30 / 60),
		sum(with(m.focals,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl' & mm)) / (m.pts * 30 / 60),
		sum(with(m.focals,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl' & mf)) / (m.pts * 30 / 60),
		sum(with(f.focals,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl')) / (f.pts * 30 / 60),
		sum(with(f.focals,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl' & fm)) / (f.pts * 30 / 60),
		sum(with(f.focals,focalBehavior %in% 'AOSOC' & focalMod1 %in% 'Dspl' & ff)) / (f.pts * 30 / 60)
	),
	rate_lb = c(
		as.numeric(unlist(lapply(m.displacement.resample[c(2,3,4)+1],quantile,0.025))),
		as.numeric(unlist(lapply(f.displacement.resample[c(2,3,4)+1],quantile,0.025)))
	),
	rate_ub = c(
		as.numeric(unlist(lapply(m.displacement.resample[c(2,3,4)+1],quantile,0.975))),
		as.numeric(unlist(lapply(f.displacement.resample[c(2,3,4)+1],quantile,0.975)))
	)
)

dspl.rates = within(dspl.rates,{
	sex_label = factor(sex,levels=c('female','male'),labels=c('adult females','adult males'))
	partner_label = factor(partner,levels=c('all','female','male'),labels=c('all\npartners','adult\nfemale\npartners','adult\nmale\npartners'))
	partner_sex_label = factor(partner,levels=c('female','male'),labels=c('female','male'))
	partner_type_label = factor(partner,levels=c('all','female','male'),labels=c('all\npartners','adult\npartners','adult\npartners'))
})

# ('#fdbf6f','#ff7f00','#33a02c')
p = ggplot() +
	geom_blank(data=dspl.rates,aes(partner_type_label,rate),stat='identity',position=position_dodge(width = 0.9)) +
	geom_bar(data=subset(dspl.rates,partner=='all'),aes(partner_type_label,rate),fill='#bcbcbc',stat='identity',position=position_dodge(width = 0.9)) +
	geom_bar(data=subset(dspl.rates,partner!='all'),aes(partner_type_label,rate,fill=partner_sex_label),stat='identity',position=position_dodge(width = 0.9)) +
	geom_errorbar(data=dspl.rates,aes(partner_type_label,alpha=partner_sex_label,ymin=rate_lb,ymax=rate_ub),width=0.1,position=position_dodge(width = 0.9),show.legend=FALSE) +
	facet_wrap(~sex_label,nrow=1) +
	scale_fill_manual(name='Partner sex',values=c('#33a02c','#ff7f00'),na.translate=FALSE) +
	scale_alpha_manual(name='Partner sex',values=c(1,0.5),na.translate=FALSE) +
	theme_classic(base_size=16) +
	theme(
		strip.background=element_blank(),
		strip.text = element_text(hjust=0.5),
		axis.title.x=element_blank(),
		legend.position='bottom',
		legend.title=element_text(hjust=1,vjust=0.5)
	) +
	ylab(expression('Displacement rate ('*italic(h)^-1*')'))


# > quantile(m.aggression.resample$d_rate,c(0.025,0.975))
#       2.5%      97.5% 
# 0.01356415 0.02357481

# > quantile(f.aggression.resample$d_rate,c(0.025,0.975))
#      2.5%     97.5% 
# 0.0160323 0.0213936 

# Give aggression
agg.codes = c(
	'Bite',  # Good
	'BobHe', # Good
	'BrowR', # Good
	'Chas',  # Good
	'EyeFla',# Good
	'Grab',  # Good
	'Hold',  # Good
	'Lung',  # Good
	'OthAgo',# Good
	'Push',  # Good
	'Yawn',  # Good
	'VocThr' #Good
)

# Receive aggression (submission behaviors)
sub.codes = c(
	'Cowr', # Good
#	'Scrat',
	'TailR',# Good
#	'BodShk',
	'Screm' # Good
)

alldata$mod1[alldata$mod1 == 'TAIL'] = 'TailR'
alldata$focalMod1[alldata$focalMod1 == 'TAIL'] = 'TailR'


# Aggression rates
a_rate_m = sum(with(m.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes & mm)) / (m.pts * 30 / 60)
a_rate_f = sum(with(m.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes & mf)) / (m.pts * 30 / 60)
sum(with(f.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes & ff)) / (f.pts * 30 / 60)
sum(with(f.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes & fm)) / (f.pts * 30 / 60)

# Bootstrap

m.focals.split = split(m.focals,as.character(m.focals$meta_id))
f.focals.split = split(f.focals,as.character(f.focals$meta_id))

save(list=c('m.meta','m.focals','m.focals.split','f.meta','f.focals','f.focals.split'),file='kinda_aggression.RData')

if (!file.exists('checkpoints/m_aggression_resample.rds')) {
	m.aggression.resample = do.call(rbind,parallel::mclapply(1:500,function(i) {
		these.focals = as.character(sample(m.meta$meta_id,replace=T))
		m.this = do.call(rbind,m.focals.split[these.focals])
		pts.this = sum(c(-10000,diff(m.this$timestamp))>50,na.rm=T)
		data.frame(
			behavior = 'aggression',
			nrep=i,
			rate = sum(with(m.this,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes)) / (pts.this * 30 / 60),
			rate_m = sum(with(m.this,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes & mm)) / (pts.this * 30 / 60),
			rate_f = sum(with(m.this,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes & mf)) / (pts.this * 30 / 60)
		)
	},mc.cores=96))
	saveRDS(m.aggression.resample,file='checkpoints/m_aggression_resample.rds')
} else {
	m.aggression.resample = readRDS('checkpoints/m_aggression_resample.rds')
}
if (!file.exists('checkpoints/f_aggression_resample.rds')) {
	f.aggression.resample = do.call(rbind,parallel::mclapply(1:500,function(i) {
		these.focals = as.character(sample(f.meta$meta_id,replace=T))
		f.this = do.call(rbind,f.focals.split[these.focals])
		pts.this = sum(c(-10000,diff(f.this$timestamp))>50,na.rm=T)
		data.frame(
			behavior = 'aggression',
			nrep=i,
			rate = sum(with(f.this,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes)) / (pts.this * 30 / 60),
			rate_m = sum(with(f.this,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes & fm)) / (pts.this * 30 / 60),
			rate_f = sum(with(f.this,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes & ff)) / (pts.this * 30 / 60)
		)
	},mc.cores=96))
	saveRDS(f.aggression.resample,file='checkpoints/f_aggression_resample.rds')
} else {
	f.aggression.resample = readRDS('checkpoints/f_aggression_resample.rds')
}

agg.rates = data.frame(
	behavior = 'aggression',
	sex = c(rep('male',3),rep('female',3)),
	partner = c('all','male','female','all','male','female'),
	rate = c(
		sum(with(m.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes)) / (m.pts * 30 / 60),
		sum(with(m.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes & mm)) / (m.pts * 30 / 60),
		sum(with(m.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes & mf)) / (m.pts * 30 / 60),
		sum(with(f.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes)) / (f.pts * 30 / 60),
		sum(with(f.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes & fm)) / (f.pts * 30 / 60),
		sum(with(f.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% agg.codes & ff)) / (f.pts * 30 / 60)
	),
	rate_lb = c(
		as.numeric(unlist(lapply(m.aggression.resample[c(2,3,4)+1],quantile,0.025))),
		as.numeric(unlist(lapply(f.aggression.resample[c(2,3,4)+1],quantile,0.025)))
	),
	rate_ub = c(
		as.numeric(unlist(lapply(m.aggression.resample[c(2,3,4)+1],quantile,0.975))),
		as.numeric(unlist(lapply(f.aggression.resample[c(2,3,4)+1],quantile,0.975)))
	)
)

agg.rates = within(agg.rates,{
	sex_label = factor(sex,levels=c('female','male'),labels=c('adult females','adult males'))
	partner_label = factor(partner,levels=c('all','female','male'),labels=c('all\npartners','adult\nfemale\npartners','adult\nmale\npartners'))
	partner_sex_label = factor(partner,levels=c('female','male'),labels=c('female','male'))
	partner_type_label = factor(partner,levels=c('all','female','male'),labels=c('all\npartners','adult\npartners','adult\npartners'))
})





# Submission rates
s_rate_m = sum(with(m.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% sub.codes & mm)) / (m.pts * 30 / 60)
s_rate_f = sum(with(m.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% sub.codes & mf)) / (m.pts * 30 / 60)
sum(with(f.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% sub.codes & ff)) / (f.pts * 30 / 60)
sum(with(f.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% sub.codes & fm)) / (f.pts * 30 / 60)

# Bootstrap

m.focals.split = split(m.focals,as.character(m.focals$meta_id))
f.focals.split = split(f.focals,as.character(f.focals$meta_id))

save(list=c('m.meta','m.focals','m.focals.split','f.meta','f.focals','f.focals.split'),file='kinda_aggression.RData')

if (!file.exists('checkpoints/m_submission_resample.rds')) {
	m.submission.resample = do.call(rbind,parallel::mclapply(1:500,function(i) {
		these.focals = as.character(sample(m.meta$meta_id,replace=T))
		m.this = do.call(rbind,m.focals.split[these.focals])
		pts.this = sum(c(-10000,diff(m.this$timestamp))>50,na.rm=T)
		data.frame(
			behavior = 'submission',
			nrep=i,
			rate = sum(with(m.this,focalBehavior %in% 'AOSOC' & focalMod1 %in% sub.codes)) / (pts.this * 30 / 60),
			rate_m = sum(with(m.this,focalBehavior %in% 'AOSOC' & focalMod1 %in% sub.codes & mm)) / (pts.this * 30 / 60),
			rate_f = sum(with(m.this,focalBehavior %in% 'AOSOC' & focalMod1 %in% sub.codes & mf)) / (pts.this * 30 / 60)
		)
	},mc.cores=96))
	saveRDS(m.submission.resample,file='checkpoints/m_submission_resample.rds')
} else {
	m.submission.resample = readRDS('checkpoints/m_submission_resample.rds')
}
if (!file.exists('checkpoints/f_submission_resample.rds')) {
	f.submission.resample = do.call(rbind,parallel::mclapply(1:500,function(i) {
		these.focals = as.character(sample(f.meta$meta_id,replace=T))
		f.this = do.call(rbind,f.focals.split[these.focals])
		pts.this = sum(c(-10000,diff(f.this$timestamp))>50,na.rm=T)
		data.frame(
			behavior = 'submission',
			nrep=i,
			rate = sum(with(f.this,focalBehavior %in% 'AOSOC' & focalMod1 %in% sub.codes)) / (pts.this * 30 / 60),
			rate_m = sum(with(f.this,focalBehavior %in% 'AOSOC' & focalMod1 %in% sub.codes & fm)) / (pts.this * 30 / 60),
			rate_f = sum(with(f.this,focalBehavior %in% 'AOSOC' & focalMod1 %in% sub.codes & ff)) / (pts.this * 30 / 60)
		)
	},mc.cores=96))
	saveRDS(f.submission.resample,file='checkpoints/f_submission_resample.rds')
} else {
	f.submission.resample = readRDS('checkpoints/f_submission_resample.rds')
}

sub.rates = data.frame(
	behavior = 'submission',
	sex = c(rep('male',3),rep('female',3)),
	partner = c('all','male','female','all','male','female'),
	rate = c(
		sum(with(m.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% sub.codes)) / (m.pts * 30 / 60),
		sum(with(m.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% sub.codes & mm)) / (m.pts * 30 / 60),
		sum(with(m.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% sub.codes & mf)) / (m.pts * 30 / 60),
		sum(with(f.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% sub.codes)) / (f.pts * 30 / 60),
		sum(with(f.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% sub.codes & fm)) / (f.pts * 30 / 60),
		sum(with(f.focals,focalBehavior %in% 'AOAGO' & focalMod1 %in% sub.codes & ff)) / (f.pts * 30 / 60)
	),
	rate_lb = c(
		as.numeric(unlist(lapply(m.submission.resample[c(2,3,4)+1],quantile,0.025))),
		as.numeric(unlist(lapply(f.submission.resample[c(2,3,4)+1],quantile,0.025)))
	),
	rate_ub = c(
		as.numeric(unlist(lapply(m.submission.resample[c(2,3,4)+1],quantile,0.975))),
		as.numeric(unlist(lapply(f.submission.resample[c(2,3,4)+1],quantile,0.975)))
	)
)

sub.rates = within(sub.rates,{
	sex_label = factor(sex,levels=c('female','male'),labels=c('adult females','adult males'))
	partner_label = factor(partner,levels=c('all','female','male'),labels=c('all\npartners','adult\nfemale\npartners','adult\nmale\npartners'))
	partner_sex_label = factor(partner,levels=c('female','male'),labels=c('female','male'))
	partner_type_label = factor(partner,levels=c('all','female','male'),labels=c('all\npartners','adult\npartners','adult\npartners'))
})


ago.rates = rbind(
	dspl.rates,
	agg.rates,
	sub.rates
)

ago.rates$behavior = factor(ago.rates$behavior,levels=c('displacement','aggression','submission'))
	

p = ggplot() +
	geom_blank(data=ago.rates,aes(partner_type_label,rate),stat='identity',position=position_dodge(width = 0.9)) +
	geom_bar(data=subset(ago.rates,partner=='all'),aes(partner_type_label,rate),fill='#bcbcbc',stat='identity',position=position_dodge(width = 0.9)) +
	geom_bar(data=subset(ago.rates,partner!='all'),aes(partner_type_label,rate,fill=partner_sex_label),stat='identity',position=position_dodge(width = 0.9)) +
	geom_errorbar(data=ago.rates,aes(partner_type_label,alpha=partner_sex_label,ymin=rate_lb,ymax=rate_ub),width=0.1,position=position_dodge(width = 0.9),show.legend=FALSE) +
	facet_grid(behavior~sex_label,scales='free_y') +
	scale_fill_manual(name='Partner sex',values=c('#33a02c','#ff7f00'),na.translate=FALSE) +
	scale_alpha_manual(name='Partner sex',values=c(1,0.5),na.translate=FALSE) +
	theme_classic(base_size=16) +
	theme(
		strip.background=element_blank(),
		strip.text.x.top = element_text(hjust=0.5),
		strip.text.y.right = element_text(angle=0,hjust=0),
		axis.title.x=element_blank(),
		legend.position='bottom',
		legend.title=element_text(hjust=1,vjust=0.5)
	) +
	ylab(expression('Rate ('*italic(h)^-1*')'))







