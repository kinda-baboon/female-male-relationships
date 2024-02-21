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


rep.events = readRDS('checkpoints/reproductive_events.rds')
alldata = readRDS('checkpoints/alldata_checkpoints.rds')
proxdata = readRDS('checkpoints/proxdata_checkpoint.rds')
age.nona = readRDS('checkpoints/age_nona.rds')

rep.events.split = split(rep.events,rep.events$id)

rep.events = do.call(rbind,lapply(rep.events.split,function(x) {
	result = data.frame()
	for (i in 1:nrow(x)) {
		out = data.frame(x[i,])
		out$event = 'cycling'
		out$time_lag = 0
		if (i == 1) {
			out$date_start = as.Date('1999-12-31')
			out$date_end = x[i,]$date_start
		} else if (i == nrow(x)) {
			out$date_start = x[i,]$date_end
			out$date_end = as.Date('2999-12-31')
		} else if (x$time_lag[i] > 0) {
			out$date_start = x[i-1,]$date_end
			out$date_end = x[i,]$date_start
		} else {
			out = data.frame()
		}
		result = rbind(result,out)
	}
	result = rbind(x,result)
	result[order(result$date_start),]
}))



if (any(with(age.nona,agesex %in% c('AF') & !ID %in% rep.events$id))) {
	missing.females = unique(subset(age.nona,agesex %in% c('AF') & !ID %in% rep.events$id)$ID)
	rep.events = rbind(
		rep.events,
		data.frame(
			id = missing.females,
			event='cycling',
			date_start = as.Date('1999-12-31'),
			date_end = as.Date('2999-12-31'),
			time_lag = NA,
			note = ''
		)
	)
}

row.names(rep.events) = NULL

rep.events$event_code = toupper(substr(rep.events$event,1,1))

alldata$partner.as = agesex.key[with(alldata,paste0(partner,lubridate::year(dateC)))]

alldata$actor.rpc = proxdata$actor.rpc = NA
alldata$recipient.rpc = proxdata$recipient.rpc = NA
alldata$focal.rpc = proxdata$focal.rpc = NA
alldata$partner.rpc = proxdata$partner.rpc = NA

for (i in 1:nrow(rep.events)) {
	this = rep.events[i,]
	alldata$actor.rpc[alldata$actor == this$id & alldata$dateC >= this$date_start & alldata$dateC < this$date_end] = alldata$recipient.rpc[alldata$recipient == this$id & alldata$dateC >= this$date_start & alldata$dateC < this$date_end] = alldata$focal.rpc[alldata$focalAnimal == this$id & alldata$dateC >= this$date_start & alldata$dateC < this$date_end] = alldata$partner.rpc[alldata$partner == this$id & alldata$dateC >= this$date_start & alldata$dateC < this$date_end] = proxdata$actor.rpc[proxdata$actor == this$id & proxdata$dateC >= this$date_start & proxdata$dateC < this$date_end] = proxdata$recipient.rpc[proxdata$recipient == this$id & proxdata$dateC >= this$date_start & proxdata$dateC < this$date_end] = proxdata$focal.rpc[proxdata$focalAnimal == this$id & proxdata$dateC >= this$date_start & proxdata$dateC < this$date_end] = proxdata$partner.rpc[proxdata$partner == this$id & proxdata$dateC >= this$date_start & proxdata$dateC < this$date_end] = this$event_code
	proxdata$actor.rpc[proxdata$actor == this$id & proxdata$dateC >= this$date_start & proxdata$dateC < this$date_end] = proxdata$recipient.rpc[proxdata$recipient == this$id & proxdata$dateC >= this$date_start & proxdata$dateC < this$date_end] = proxdata$focal.rpc[proxdata$focalAnimal == this$id & proxdata$dateC >= this$date_start & proxdata$dateC < this$date_end] = proxdata$partner.rpc[proxdata$partner == this$id & proxdata$dateC >= this$date_start & proxdata$dateC < this$date_end] = proxdata$actor.rpc[proxdata$actor == this$id & proxdata$dateC >= this$date_start & proxdata$dateC < this$date_end] = proxdata$recipient.rpc[proxdata$recipient == this$id & proxdata$dateC >= this$date_start & proxdata$dateC < this$date_end] = proxdata$focal.rpc[proxdata$focalAnimal == this$id & proxdata$dateC >= this$date_start & proxdata$dateC < this$date_end] = proxdata$partner.rpc[proxdata$partner == this$id & proxdata$dateC >= this$date_start & proxdata$dateC < this$date_end] = this$event_code
}

max.date = lubridate::year(max(alldata$dateC,na.rm=TRUE))

# Recode rep.events so that all possible female year rpc combinations are accounted for
rep.events.years = do.call(rbind,lapply(split(rep.events,1:nrow(rep.events)),function(x) {
	year.range = do.call(seq,as.list(lubridate::year(c(x$date_start,x$date_end))))
	year.range = year.range[year.range >= 2010 & year.range <= max.date]
	if (length(year.range)) {
		data.frame(x,year=year.range)
	} else {
		data.frame()
	}
}))

rownames(rep.events.years) = NULL
rep.events.years$rpc_id = with(rep.events.years,paste(id,year,event_code,sep='-'))

repdata = alldata

repdata$ACTOR = with(repdata,ifelse(!is.na(actor.rpc),paste(actor,lubridate::year(dateC),actor.rpc,sep='-'),actor))
repdata$RECIPIENT = with(repdata,ifelse(!is.na(recipient.rpc),paste(recipient,lubridate::year(dateC),recipient.rpc,sep='-'),recipient))
repdata$FOCALANIMAL = with(repdata,ifelse(!is.na(focal.rpc),paste(focalAnimal,lubridate::year(dateC),focal.rpc,sep='-'),focalAnimal))
repdata$PARTNER = with(repdata,ifelse(!is.na(partner.rpc),paste(partner,lubridate::year(dateC),partner.rpc,sep='-'),partner))

all.adults = subset(age.nona,agesex %in% c('AF','AM'),select=c('ID','Sex','year','agesex'))

male.adults = subset(all.adults,agesex %in% 'AM')
female.adults = subset(all.adults,agesex %in% 'AF')

female.adults = do.call(rbind,lapply(split(female.adults,1:nrow(female.adults)),function(x) {
	out = unique(subset(rep.events.years,id == x$ID & year == x$year,select=c('id','event_code','year','rpc_id')))
	if (nrow(out)) {
		data.frame(x[c('ID','Sex','year')],agesex='AF',rpc=out$event_code,rpc_id=out$rpc_id)
	} else {
		data.frame()
	}
}))

male.adults = data.frame(male.adults,rpc=NA,rpc_id = male.adults$ID)

all.adults = rbind(female.adults,male.adults)

all.adults$id_backup = all.adults$ID
all.adults$ID = all.adults$rpc_id

all.adults$rpc_id = NULL

repdata$actor_backup = repdata$actor
repdata$recipient_backup = repdata$recipient
repdata$focalAnimal_backup = repdata$focalAnimal
repdata$partner_backup = repdata$partner

repdata$actor = repdata$ACTOR
repdata$recipient = repdata$RECIPIENT
repdata$focalAnimal = repdata$FOCALANIMAL
repdata$partner = repdata$PARTNER

# repdata = subset(repdata,!is.na(dateC) & lubridate::year(dateC) > 2010 & lubridate::year(dateC) < 2019)
repdata = subset(repdata,!is.na(dateC))

repdata = subset(repdata,focalAnimal %in% all.adults$ID & focal.as %in% c('AF','AM'))

# Calculate fraction of focal time spent grooming

repallyears = repdata
repallyears$focalAnimal = gsub('-[0-9]{4}-','-',repallyears$focalAnimal)

rpc.t.grooming.all.years = do.call(rbind,lapply(split(repallyears,repallyears$focalAnimal),function(x) {
	npts = sum(c(-10000,diff(x$timestamp))>50)
	if (!is.na(npts) & npts > 120) {
		data.frame(
			rpc_id = unique(x$focalAnimal),
			id = gsub('-[CLP]$','',unique(x$focalAnimal)),
			sex = substr(unique(x$focal.as),2,2),
			rpc = unique(x$focal.rpc),
			count = as.integer(c(
				table(factor(subset(x,focalBehavior == 'G' & partner %in% all.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
				table(factor(subset(x,focalBehavior == 'B' & partner %in% all.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
			)),
			norm = as.numeric(c(
				table(factor(subset(x,focalBehavior == 'G' & partner %in% all.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
				table(factor(subset(x,focalBehavior == 'B' & partner %in% all.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
			) / npts),
			partner.as = c(
				'AF','AM','AF','AM'
			),
			direction = c(
				1,1,0,0
			)
		)
	} else {
		data.frame()
	}
}))


rpc.t.grooming.all.years$rpc[is.na(rpc.t.grooming.all.years$rpc)] = 'M'
rpc.t.grooming.all.years$rpc = factor(rpc.t.grooming.all.years$rpc,levels=c('C','P','L','M'),labels=c('Cycling','Pregnant','Lactating','none (male)'))

rpc.t.grooming.all.years$direction_label = factor(rpc.t.grooming.all.years$direction,levels=c(1,0),labels=c('give groom','receive groom'))

rpc.t.grooming.all.years$partner_label = factor(rpc.t.grooming.all.years$partner.as,levels=c('AF','AM'),labels=c('female\npartners','male\npartners'))

dodge = position_dodge(width = 0.9)
p = ggplot(subset(rpc.t.grooming.all.years,sex=='F')) +
	geom_quasirandom(aes(partner_label,norm,color=direction_label),dodge.width=0.9) +
	geom_boxplot(aes(partner_label,norm,alpha=direction_label),position=dodge,width=0.1,outlier.shape=NA,show.legend=FALSE) +
	facet_wrap(~rpc,nrow=1) +
	# scale_color_manual(
	# 	values = c(
	# 		'Cycling'     = '#1b9e77',
	# 		'Pregnant'    = '#d95f02',
	# 		'Lactating'   = '#7570b3',
	# 		'none (male)' = '#000000'
	# 	)
	# ) +
	scale_color_manual(
		values = c(
			'give groom'   = '#E64B35',
			'receive groom'= '#4DBBD5'
		)
	) +
	scale_alpha_manual(
		values = c(
			'give groom'   = 0.75,
			'receive groom'= 0.75
		)
	) +
	theme_classic(base_size=16) +
	theme(
		legend.position='bottom',
		legend.title=element_blank(),
		axis.title.x=element_blank(),
		strip.background=element_blank()
	) +
	guides(
		color=guide_legend(override.aes=list(size=3))
	) +
	ylab('Fraction activity budget') +
	ggtitle('Groomings across adult female reproductive states')











rpc.t.grooming.all.years.nondirectional = do.call(rbind,lapply(split(repallyears,repallyears$focalAnimal),function(x) {
	npts = sum(c(-10000,diff(x$timestamp))>50)
	if (!is.na(npts) & npts > 120) {
		data.frame(
			rpc_id = unique(x$focalAnimal),
			id = gsub('-[CLP]$','',unique(x$focalAnimal)),
			sex = substr(unique(x$focal.as),2,2),
			rpc = unique(x$focal.rpc),
			count = as.integer(c(
				table(factor(subset(x,focalBehavior %in% c('G','B') & partner %in% all.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
			)),
			norm = as.numeric(c(
				table(factor(subset(x,focalBehavior %in% c('G','B') & partner %in% all.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
			) / npts),
			partner.as = c(
				'AF','AM'
			)
		)
	} else {
		data.frame()
	}
}))


rpc.t.grooming.all.years.nondirectional$rpc[is.na(rpc.t.grooming.all.years.nondirectional$rpc)] = 'M'
rpc.t.grooming.all.years.nondirectional$rpc = factor(rpc.t.grooming.all.years.nondirectional$rpc,levels=c('C','P','L','M'),labels=c('cycling','pregnant','lactating','none (male)'))

# rpc.t.grooming.all.years.nondirectional$direction_label = factor(rpc.t.grooming.all.years.nondirectional$direction,levels=c(1,0),labels=c('give groom','receive groom'))

rpc.t.grooming.all.years.nondirectional$partner_label = factor(rpc.t.grooming.all.years.nondirectional$partner.as,levels=c('AF','AM'),labels=c('female partners','male partners'))

dodge = position_dodge(width = 0.9)
p = ggplot(subset(rpc.t.grooming.all.years.nondirectional,sex=='F')) +
	geom_quasirandom(aes(rpc,norm,color=partner_label),dodge.width=0.9) +
	geom_boxplot(aes(rpc,norm,alpha=partner_label),position=dodge,width=0.1,outlier.shape=NA,show.legend=FALSE) +
	facet_wrap(~partner_label,nrow=1) +
	# scale_color_manual(
	# 	values = c(
	# 		'Cycling'     = '#1b9e77',
	# 		'Pregnant'    = '#d95f02',
	# 		'Lactating'   = '#7570b3',
	# 		'none (male)' = '#000000'
	# 	)
	# ) +
	scale_color_manual(
		values = c(
			'#31a354',
			'#756bb1'
		)
	) +
	scale_alpha_manual(
		values = c(
			0.75,
			0.75
		)
	) +
	theme_classic(base_size=16) +
	theme(
		legend.position='none',
		legend.title=element_blank(),
		axis.title.x=element_blank(),
		strip.background=element_blank()
	) +
	guides(
		color=guide_legend(override.aes=list(size=3))
	) +
	ylab('Fraction activity budget') #+
	#ggtitle('Groomings across adult female reproductive states')
p.rpc.groom.rates = p

rpc.stats = subset(rpc.t.grooming.all.years.nondirectional,sex=='F')

to.do = list(	
	c('cycling','pregnant'),
	c('cycling','lactating'),
	c('pregnant','lactating')
)
do.call(rbind,lapply(split(rpc.stats,rpc.stats$partner.as),function(x) {
	do.call(rbind,lapply(to.do,function(y) {
		test = wilcox.test(
			x$norm[x$rpc==y[1]],
			x$norm[x$rpc==y[2]]
		)
		data.frame(
			partner.as = unique(x$partner.as),
			x1 = y[1],
			x2 = y[2],
			m1 = mean(x$norm[x$rpc==y[1]]),
			m2 = mean(x$norm[x$rpc==y[2]]),
			n1 = sum(x$rpc==y[1]),
			n2 = sum(x$rpc==y[2]),
			p = test$p.value
		)
	}))
}))

do.call(rbind,lapply(split(rpc.stats,rpc.stats$partner.as),function(x) {
	do.call(rbind,lapply(c('cycling','pregnant','lactating'),function(y) {
		test = wilcox.test(
			x$norm[x$rpc==y],
			x$norm[x$rpc!=y]
		)
		data.frame(
			partner.as = unique(x$partner.as),
			x1 = y,
			m1 = mean(x$norm[x$rpc==y]),
			m2 = mean(x$norm[x$rpc!=y]),
			n1 = sum(x$rpc==y),
			n2 = sum(x$rpc!=y),
			p = test$p.value
		)
	}))
}))

# Direction
rpc.directions = do.call(rbind,lapply(split(droplevels(subset(rpc.t.grooming.all.years,sex=='F')),subset(rpc.t.grooming.all.years,sex=='F')$rpc[,drop=TRUE]),function(x) {
	dir.counts = tapply(x$count,x$direction_label,sum)
	dir.test = binom.test(dir.counts['receive groom'],sum(dir.counts))
	
	y = subset(droplevels(subset(rpc.t.grooming.all.years,sex=='F')),rpc != unique(x$rpc))
	dir.counts2 = tapply(y$count,y$direction_label,sum)
	
	dir.test2 = prop.test(c(dir.counts['receive groom'],dir.counts2['receive groom']),c(sum(dir.counts),sum(dir.counts2)))
	
	# wilcox.test(
	# 	c(rep(1,dir.counts['receive groom']),rep(0,dir.counts['give groom'])),
	# 	c(rep(1,dir.counts2['receive groom']),rep(0,dir.counts2['give groom']))
	# )
	data.frame(
		rpc = unique(x$rpc),
		n_mf = dir.counts['receive groom'],
		n = sum(dir.counts),
		prop_mf = dir.counts['receive groom'] / sum(dir.counts),
		lb = dir.test$conf.int[1],
		ub = dir.test$conf.int[2],
		p = dir.test$p.value,
		prop_other = dir.test2$estimate[2],
		p_other = dir.test2$p.value
	)
}))

rpc.directions$rpc_label = factor(rpc.directions$rpc,levels=rev(levels(rpc.directions$rpc)),labels=tolower(rev(levels(rpc.directions$rpc))))

dodge = position_dodge(width = 0.9)
p = ggplot(rpc.directions) +
	geom_bar(aes(rpc_label,prop_mf),width=0.75,stat='identity',fill='#e0e0e0') +
	geom_errorbar(aes(rpc_label,ymin=lb,ymax=ub),width=0.2) +
	geom_hline(yintercept=0.5,linetype=2,linewidth=0.5) +
	scale_y_continuous(breaks=seq(0,1,0.2)) +
	coord_flip(ylim=c(0,1)) +
	theme_classic(base_size=16) +
	theme(
		legend.position='none',
		# axis.text.x=element_text(angle=-45,hjust=0,vjust=0.5),
		axis.title.y = element_blank()
	) +
	ylab('Fraction by males')
p.rpc.groom.direction = p

# t.grooming.per.year = do.call(rbind,lapply(alldata.split,function(z) {
# 	this.year = unique(lubridate::year(z$dateC))
# 	all.adults = subset(age.nona,year == this.year & agesex %in% c('AF','AM') & !ID %in% natal.males)
# 	do.call(rbind,lapply(split(z,z$focalAnimal),function(x) {
# 		npts = sum(c(-10000,diff(x$timestamp))>50)
# 		if (!is.na(npts) & npts > 120) {
# 			data.frame(
# 				id = unique(x$focalAnimal),
# 				year = this.year,
# 				sex = substr(unique(x$focal.as),2,2),
# 				count = as.integer(c(
# 					table(factor(subset(x,focalBehavior == 'G' & partner %in% all.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
# 					table(factor(subset(x,focalBehavior == 'B' & partner %in% all.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
# 				)),
# 				norm = as.numeric(c(
# 					table(factor(subset(x,focalBehavior == 'G' & partner %in% all.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
# 					table(factor(subset(x,focalBehavior == 'B' & partner %in% all.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
# 				) / npts),
# 				norm_mf = as.numeric(c(
# 					table(factor(subset(x,focalBehavior == 'G' & partner %in% all.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
# 					table(factor(subset(x,focalBehavior == 'B' & partner %in% all.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
# 				) / npts) / rep(as.integer(table(all.adults$Sex)),2) * mean(table(all.adults$Sex)),
# 				partner.as = c(
# 					'AF','AM','AF','AM'
# 				),
# 				direction = c(
# 					1,1,0,0
# 				)
# 			)
# 		} else {
# 			data.frame()
# 		}
# 	}))
# }))
rpc.t.grooming.per.year = do.call(rbind,lapply(split(repallyears,repallyears$year), function(z) {
	this.year = unique(lubridate::year(z$dateC))
	these.adults = subset(all.adults,year==this.year & agesex %in% c('AF','AM') & !ID %in% natal.males)
	do.call(rbind,lapply(split(z,z$focalAnimal),function(x) {
		npts = sum(c(-10000,diff(x$timestamp))>50)
		if (!is.na(npts) & npts > 60) {
			data.frame(
				rpc_id = unique(x$focalAnimal),
				year = this.year,
				id = gsub('-[CLP]$','',unique(x$focalAnimal)),
				sex = substr(unique(x$focal.as),2,2),
				rpc = unique(x$focal.rpc),
				count = as.integer(c(
					table(factor(subset(x,focalBehavior == 'G' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
					table(factor(subset(x,focalBehavior == 'B' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
				)),
				norm = as.numeric(c(
					table(factor(subset(x,focalBehavior == 'G' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
					table(factor(subset(x,focalBehavior == 'B' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
				) / npts),
				partner.as = c(
					'AF','AM','AF','AM'
				),
				direction = c(
					1,1,0,0
				)
			)
		} else {
			data.frame()
		}
	}))
}))

rpc.t.grooming.per.year$rpc[is.na(rpc.t.grooming.per.year$rpc)] = 'M'
rpc.t.grooming.per.year$rpc = factor(rpc.t.grooming.per.year$rpc,levels=c('C','P','L','M'),labels=c('Cycling','Pregnant','Lactating','none (male)'))

rpc.t.grooming.per.year$direction_label = factor(rpc.t.grooming.per.year$direction,levels=c(1,0),labels=c('give groom','receive groom'))

rpc.t.grooming.per.year$partner_label = factor(rpc.t.grooming.per.year$partner.as,levels=c('AF','AM'),labels=c('female\npartners','male\npartners'))


dodge = position_dodge(width = 0.9)
p = ggplot(subset(rpc.t.grooming.per.year,sex=='F' & year > 2011 & year < 2019)) +
	geom_quasirandom(aes(partner_label,norm,color=direction_label),dodge.width=0.9) +
	geom_boxplot(aes(partner_label,norm,alpha=direction_label),position=dodge,width=0.1,outlier.shape=NA,show.legend=FALSE) +
	facet_grid(year~rpc) +
	# scale_color_manual(
	# 	values = c(
	# 		'Cycling'     = '#1b9e77',
	# 		'Pregnant'    = '#d95f02',
	# 		'Lactating'   = '#7570b3',
	# 		'none (male)' = '#000000'
	# 	)
	# ) +
	scale_color_manual(
		values = c(
			'give groom'   = '#E64B35',
			'receive groom'= '#4DBBD5'
		)
	) +
	scale_alpha_manual(
		values = c(
			'give groom'   = 0.75,
			'receive groom'= 0.75
		)
	) +
	theme_classic(base_size=16) +
	theme(
		legend.position='bottom',
		legend.title=element_blank(),
		axis.title.x=element_blank(),
		strip.text.y.right=element_text(angle=0),
		strip.background=element_blank()
	) +
	guides(
		color=guide_legend(override.aes=list(size=3))
	) +
	ylab('Fraction activity budget') +
	ggtitle('Groomings across adult female reproductive states')


# OK NOW redo but with proxfocs

prepdata = proxdata

prepdata$ACTOR = with(prepdata,ifelse(!is.na(actor.rpc),paste(actor,lubridate::year(dateC),actor.rpc,sep='-'),actor))
prepdata$RECIPIENT = with(prepdata,ifelse(!is.na(recipient.rpc),paste(recipient,lubridate::year(dateC),recipient.rpc,sep='-'),recipient))
prepdata$FOCALANIMAL = with(prepdata,ifelse(!is.na(focal.rpc),paste(focalAnimal,lubridate::year(dateC),focal.rpc,sep='-'),focalAnimal))
prepdata$PARTNER = with(prepdata,ifelse(!is.na(partner.rpc),paste(partner,lubridate::year(dateC),partner.rpc,sep='-'),partner))

prepdata$actor_backup = prepdata$actor
prepdata$recipient_backup = prepdata$recipient
prepdata$focalAnimal_backup = prepdata$focalAnimal
prepdata$partner_backup = prepdata$partner

prepdata$actor = prepdata$ACTOR
prepdata$recipient = prepdata$RECIPIENT
prepdata$focalAnimal = prepdata$FOCALANIMAL
prepdata$partner = prepdata$PARTNER

# prepdata = subset(prepdata,!is.na(dateC) & lubridate::year(dateC) > 2010 & lubridate::year(dateC) < 2019)
prepdata = subset(prepdata,!is.na(dateC))

prepdata = subset(prepdata,focalAnimal %in% all.adults$ID & focal.as %in% c('AF','AM'))

shared.columns = intersect(colnames(repdata),colnames(prepdata))

prepdata.all = rbind(repdata[,shared.columns],prepdata[,shared.columns])


aw = subset(prepdata.all,behavior %in% 'AOSOC' & mod1 %in% c('Appr','Wthd') & actor.as %in% c('AM','AF') & recipient.as %in% c('AM','AF') & actor.as != recipient.as)

aw = within(aw,{
	focalAnimal = gsub('-[0-9]{4}-','-',focalAnimal)
	partner = gsub('-[0-9]{4}-','-',partner)
	actor = gsub('-[0-9]{4}-','-',actor)
	recipient = gsub('-[0-9]{4}-','-',recipient)
})


aw$dyadkey = with(aw,ifelse(focal.as=='AM',paste0(focalAnimal,'_',partner),paste0(partner,'_',focalAnimal)))

appr.wthd = do.call(rbind,lapply(split(aw,aw$meta_id),function(x) {
	do.call(rbind,lapply(split(x,x$dyadkey),function(y) {
		out = data.frame()
		ticker = 0
		for (i in 1:nrow(y)) {
			z = y[i,]
			if ((ticker == 0) && (z$mod1 == 'Appr')) {
				this = z[c('meta_id','dyadkey','focalAnimal','partner','actor','actor.as','timestamp')]
				names(this)[names(this) %in% c('actor','actor.as','timestamp')] = paste0('appr_',names(this)[names(this) %in% c('actor','actor.as','timestamp')])
				ticker = 1
			} else if ((ticker == 1) && (z$mod1 == 'Wthd')) {
				that = z[c('actor','actor.as','timestamp')]
				names(that) = paste0('wthd_',names(that))
				out = rbind(out,data.frame(this,that,duration = that$wthd_timestamp - this$appr_timestamp))
				ticker = 0
			}
		}
		out
	}))
}))

rownames(appr.wthd) = NULL

appr.wthd$female_rpc = gsub('.*?-([CLP]).*','\\1',appr.wthd$dyadkey)
appr.wthd$bout_id = 1:nrow(appr.wthd)

appr.wthd$dyadkey2 = with(appr.wthd,gsub('-[CLP]$','',dyadkey))

hinde.overall = tidyr::pivot_longer(do.call(rbind,lapply(split(appr.wthd,appr.wthd$female_rpc),function(x) {
	data.frame(
		rpc = unique(x$female_rpc),
		hinde = as.numeric((table(x$appr_actor.as) / nrow(x) * 100)['AF'] - (table(x$wthd_actor.as) / nrow(x) * 100)['AF']),
		frac_appr_m = as.numeric(table(x$appr_actor.as)['AM']/nrow(x)),
		frac_wthd_m = as.numeric(table(x$wthd_actor.as)['AM']/nrow(x))
	)
})),cols=c('hinde','frac_appr_m','frac_wthd_m'))


if (!file.exists('checkpoints/hinde_rpc_resample.rds')) {
	hinde.resample = do.call(rbind,parallel::mclapply(1:10000,function(i) {
		y = appr.wthd[sample(1:nrow(appr.wthd),replace=TRUE),]
		do.call(rbind,lapply(split(y,y$female_rpc),function(x) {
			data.frame(
				nrep=i,
				rpc = unique(x$female_rpc),
				hinde = as.numeric((table(x$appr_actor.as) / nrow(x) * 100)['AF'] - (table(x$wthd_actor.as) / nrow(x) * 100)['AF']),
				frac_appr_m = as.numeric(table(x$appr_actor.as)['AM']/nrow(x)),
				frac_wthd_m = as.numeric(table(x$wthd_actor.as)['AM']/nrow(x))
			)
		}))
	},mc.cores=4))
	saveRDS(hinde.resample,file='checkpoints/hinde_rpc_resample.rds')
} else {
	hinde.resample = readRDS('checkpoints/hinde_rpc_resample.rds')
}

hinde.ci = do.call(rbind,lapply(split(hinde.resample,hinde.resample$rpc),function(x) data.frame(rpc=unique(x$rpc),name='hinde',lb=as.numeric(quantile(x$hinde,c(0.025))),ub=as.numeric(quantile(x$hinde,c(0.975))))))

hinde.overall$rpc_label = factor(hinde.overall$rpc,levels=rev(c('C','L','P')),labels=rev(c('cycling','lactating','pregnant')))
hinde.ci$rpc_label = factor(hinde.ci$rpc,levels=rev(c('C','L','P')),labels=rev(c('cycling','lactating','pregnant')))

# Pairwise tests


p = ggplot() +
	geom_bar(data=subset(hinde.overall,name=='hinde'),aes(rpc_label,value),stat='identity',color=NA,fill='#4DBBD5FF',width=0.75) +
	geom_point(data=subset(hinde.overall,name=='hinde'),aes(rpc_label,value),color='#000000',shape=19,size=2) +
	geom_errorbar(data=hinde.ci,aes(rpc_label,ymin=lb,ymax=ub),width=0.2) +
	geom_hline(yintercept=0,linetype=2,color='#000000') +
	scale_y_continuous(limits=c(-100,100),breaks=seq(-100,100,50)) +
	coord_flip() +
	theme_classic(base_size=16) +
	theme(
		axis.title.y = element_blank()
	) +
	ylab('Hinde\'s index')
p.rpc.hinde = p


p.fig2 = ggpubr::ggarrange(p.rpc.groom.rates,p.rpc.groom.direction,p.rpc.hinde,nrow=3,heights=c(0.5,0.25,0.25),labels='AUTO')

pdf(file='figures/fig2.pdf',useDingbats=FALSE)
p.fig2
dev.off()


# all.adults$sexrpc = with(all.adults,ifelse(is.na(rpc),Sex,paste(Sex,rpc,sep='-')))
# 
# adults.rpc = subset(all.adults,select=c('ID','sexrpc'))
# 
# # Take contingency as input and sum by category
# bf.as = function(a,asx,all.x=NULL,all.y=NULL) {
# 	beh = a$contingency[,,1]
# 	npts = a$points.table
# 	if (is.null(all.x)) {
# 		x.individuals = unique(asx[[1]])
# 	} else {
# 		x.individuals = unique(asx[[1]][!is.na(match(asx[[2]],all.x)),drop=TRUE])
# 	}
# 	if (is.null(all.y)) {
# 		y.individuals = unique(asx[[1]])
# 	} else {
# 		y.individuals = unique(asx[[1]][!is.na(match(asx[[2]],all.y)),drop=TRUE])
# 	}
# 	x.individuals = intersect(x.individuals,do.call(intersect,dimnames(beh)))
# 	y.individuals = intersect(y.individuals,do.call(intersect,dimnames(beh)))
# 	sum(beh[x.individuals,y.individuals]) / sum(npts[intersect(union(x.individuals,y.individuals),names(npts))])
# }
# 
# 
# # Take contingency as input and sum by category
# bf.as = function(a,asx,all.x=NULL,all.y=NULL) {
# 	beh = a$contingency[,,1]
# 	npts = a$points.table
# 	if (is.null(all.x)) {
# 		x.individuals = unique(asx[[1]])
# 	} else {
# 		x.individuals = unique(asx[[1]][!is.na(match(asx[[2]],all.x)),drop=TRUE])
# 	}
# 	if (is.null(all.y)) {
# 		y.individuals = unique(asx[[1]])
# 	} else {
# 		y.individuals = unique(asx[[1]][!is.na(match(asx[[2]],all.y)),drop=TRUE])
# 	}
# 	x.individuals = intersect(x.individuals,do.call(intersect,dimnames(beh)))
# 	y.individuals = intersect(y.individuals,do.call(intersect,dimnames(beh)))
# 	sum(beh[x.individuals,y.individuals]) / sum(npts[intersect(union(x.individuals,y.individuals),names(npts))])
# }
# 
# foo = lapply(split(repdata,lubridate::year(repdata$dateC)),function(x) {
# 	this.year = unique(year(x$dateC))
# 	# these.adults = subset(all.adults,year == this.year & agesex %in% c('AF','AM') & !ID %in% natal.males)
# 	these.adults = subset(all.adults,year == this.year & agesex %in% c('AF','AM') & !ID %in% natal.males,select=c('ID','sexrpc'))
# 	groom = ct(x,behaviors='G',individuals=these.adults$ID,directional=TRUE)
# 	
# 	groom_freqs_m_fc = bf.as(groom,these.adults,all.x='M',all.y='F-C')
# 	groom_freqs_m_fl = bf.as(groom,these.adults,all.x='M',all.y='F-L')
# 	groom_freqs_m_fp = bf.as(groom,these.adults,all.x='M',all.y='F-P')
# 	
# 	groom_freqs_fc_m = bf.as(groom,these.adults,all.x='F-C',all.y='M')
# 	groom_freqs_fl_m = bf.as(groom,these.adults,all.x='F-L',all.y='M')
# 	groom_freqs_fp_m = bf.as(groom,these.adults,all.x='F-P',all.y='M')
# 	
# 	groom_freqs_f_fc = bf.as(groom,these.adults,all.x=c('F-C','F-L','F-P'),all.y='F-C')
# 	groom_freqs_f_fl = bf.as(groom,these.adults,all.x=c('F-C','F-L','F-P'),all.y='F-L')
# 	groom_freqs_f_fp = bf.as(groom,these.adults,all.x=c('F-C','F-L','F-P'),all.y='F-P')
# 	
# 	groom_freqs_fc_f = bf.as(groom,these.adults,all.x='F-C',all.y=c('F-C','F-L','F-P'))
# 	groom_freqs_fl_f = bf.as(groom,these.adults,all.x='F-L',all.y=c('F-C','F-L','F-P'))
# 	groom_freqs_fp_f = bf.as(groom,these.adults,all.x='F-P',all.y=c('F-C','F-L','F-P'))
# 	
# 	# groom_freqs_f_f = bf.as(groom,these.adults,all.x=c('F-C','F-L','F-P'),all.y=c('F-C','F-L','F-P'))
# 	groom_freqs_m_m = bf.as(groom,these.adults,all.x='M',all.y='M')
# 	
# 	data.frame(
# 		year = this.year,
# 		sex1 = c(
# 			rep('M',3),
# 			rep('F',3),
# 			rep('F',6),
# 			'M'
# 		),
# 		sex2 = c(
# 			rep('F',3),
# 			rep('M',3),
# 			rep('F',6),
# 			'M'
# 		),
# 		rpc1 = c(
# 			rep(NA,3),
# 			'C','L','P',
# 			rep(NA,3),
# 			'C','L','P',
# 			NA
# 		),
# 		rpc2 = c(
# 			'C','L','P',
# 			rep(NA,3),
# 			'C','L','P',
# 			rep(NA,3),
# 			NA
# 		),
# 		rpc = c(
# 			'C','L','P',
# 			'C','L','P',
# 			'C','L','P',
# 			'C','L','P',
# 			NA			
# 		),
# 		direction = c(
# 			rep(0,3),
# 			rep(1,3),
# 			rep(0,3),
# 			rep(1,3),
# 			NA			
# 		),
# 		partner_sex = c(
# 			rep('M',3),
# 			rep('M',3),
# 			rep('F',3),
# 			rep('F',3),
# 			NA			
# 		),
# 		category = c(
# 			'm_fc',
# 			'm_fl',
# 			'm_fp',
# 			'fc_m',
# 			'fl_m',
# 			'fp_m',
# 			'f_fc',
# 			'f_fl',
# 			'f_fp',
# 			'fc_f',
# 			'fl_f',
# 			'fp_f',
# 			'm_m'
# 		),
# 		rate = as.numeric(c(
# 			m_fc=groom_freqs_m_fc,
# 			m_fl=groom_freqs_m_fl,
# 			m_fp=groom_freqs_m_fp,
# 			fc_m=groom_freqs_fc_m,
# 			fl_m=groom_freqs_fl_m,
# 			fp_m=groom_freqs_fp_m,
# 			f_fc=groom_freqs_f_fc,
# 			f_fl=groom_freqs_f_fl,
# 			f_fp=groom_freqs_f_fp,
# 			fc_f=groom_freqs_fc_f,
# 			fl_f=groom_freqs_fl_f,
# 			fp_f=groom_freqs_fp_f,
# 			m_m=groom_freqs_m_m
# 		))
# 	)
# })
# 
# p = ggplot(subset(do.call(rbind,foo),!is.na(rpc)),aes(rpc,rate,fill=partner_sex)) + geom_bar(stat='identity',position='dodge') + facet_grid(year~factor(direction)) + theme_classic() + theme(strip.background=element_blank())




