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

mf.comparative = read.delim('data/comparative_mf.txt')

load(behavior.file,verbose=TRUE)

# Scan for duplicates
id.duplicates = function(x) {
	do.call(rbind,lapply(split(x,x$meta_id),function(y) {
		with(y,data.frame(
			meta_id = unique(meta_id),
			odf = unique(filename),
			focalAnimal = unique(focalAnimal),
			dateC = unique(dateC),
			time_start = unique(time),
			datetime = paste(unique(dateC),unique(time),sep='-'),
			focal_datetime = paste(unique(focalAnimal),unique(dateC),unique(time),sep='-'),
			last_point = unique(last_point),
			unique_key = paste(unique(focalAnimal),unique(dateC),paste(paste0(timestamp,'-',focalBehavior),collapse=','),sep='_')
		))
	}))
}

alldata.focals = id.duplicates(alldata)
proxdata.focals = id.duplicates(proxdata)

alldata.dups = subset(alldata.focals,datetime %in% subset(alldata.focals,duplicated(datetime))$datetime)
proxdata.dups = subset(proxdata.focals,datetime %in% subset(proxdata.focals,duplicated(datetime))$datetime)

alldata.dups = alldata.dups[order(alldata.dups$datetime),]
proxdata.dups = proxdata.dups[order(proxdata.dups$datetime),]

write.table(rbind(alldata.dups,proxdata.dups),file='~/Downloads/duplicated_odfs.txt',sep='\t',row.names=FALSE,quote=FALSE)

# # Temporary
# alldata = subset(alldata,!meta_id %in% rbind(alldata.dups,proxdata.dups)$meta_id)
# proxdata = subset(proxdata,!meta_id %in% rbind(alldata.dups,proxdata.dups)$meta_id)



dup.decisions = read.delim('data/duplicates_decisions.txt')
dup.keep = subset(dup.decisions,decision %in% 1:2)
dup.remove = subset(dup.decisions,!decision %in% 1:2)

alldata = subset(alldata,!filename %in% dup.remove$filename)
proxdata = subset(proxdata,!filename %in% dup.remove$filename)


# Female-male grooming interactions are characterized by greater and sustained grooming by males

ages = read.delim('data/baboon_year_ages.txt')

sex.key = ages$Sex
names(sex.key) = ages$ID

# alldata.split = split(alldata.adultdate,year(alldata.adultdate$dateC))
# 
# foobar = do.call(rbind,lapply(alldata.split,function(x) {
# 	this.year = unique(year(x$dateC))
# 	these.adults = individuals[individuals[[paste('Age',this.year,sep='_')]] %in% 'ADU' & !(individuals$Sex == 'M' & individuals$natal),c('ID','Sex')]
# 	groom = ct(x,behaviors='G',individuals=these.adults$ID,directional=TRUE)
# 	groom_freqs = bf(groom,digits=2)
# 	
# 	out = groom_freqs$table
# 	
# 	out$year = this.year
# 	
# 	out$sex1 = sex.key[out$Var1]
# 	out$sex2 = sex.key[out$Var2]
# 	out
# }))


alldata$actor = gsub(' *$','',alldata$actor)
alldata$partner = gsub(' *$','',alldata$partner)
alldata$focalAnimal = gsub(' *$','',alldata$focalAnimal)

ids = ages$ID[ages$ID != '']

subset(alldata,!focalAnimal %in% ids)$focalAnimal %>% unique

age.df = subset(ages,select=c('ID','Sex',paste0('Age_',2010:2022)))

age.long = tidyr::pivot_longer(age.df,cols=paste0('Age_',2010:2022))

age.long$year = gsub('Age_','',age.long$name)

age.nona = subset(age.long,!(is.na(value) | value %in% c('','?')))

age.nona$value[grepl('\\?$',age.nona$value)] = gsub('\\?','',age.nona$value[grepl('\\?$',age.nona$value)])
age.nona$value[age.nona$value %in% c('A','AD','ADU ')] = 'ADU'
age.nona$value[age.nona$value %in% c('SUB/ADU')] = 'SUB'

age.nona$rowcode = paste0(age.nona$ID,age.nona$year)
age.nona$agesex = with(age.nona,paste0(substr(value,1,1),Sex))

age.nona = subset(age.nona,nchar(agesex) == 2)

agesex.key = age.nona$agesex
names(agesex.key) = age.nona$rowcode

alldata$year = lubridate::year(alldata$dateC)

alldata$focal.as = agesex.key[with(alldata,paste0(focalAnimal,year))]
alldata$actor.as = agesex.key[with(alldata,paste0(actor,year))]
alldata$recipient.as = agesex.key[with(alldata,paste0(recipient,year))]

alldata$actor_as = NULL
alldata$recipient_as = NULL

intersex.groomings = subset(alldata,actor.as %in% c('AF','AM') & recipient.as %in% c('AF','AM') & actor.as != recipient.as & behavior %in% 'G')

intersex.binom.model = with(intersex.groomings,binom.test(sum(actor.as=='AM'),sum(!is.na(actor.as)),p=0.05,alternative='two.sided'))

all.years = data.frame(
	year = 'all',
	estimate=intersex.binom.model$estimate,
	lb=intersex.binom.model$conf.int[1],
	ub=intersex.binom.model$conf.int[2],
	pval=intersex.binom.model$p.value
)

year.by.year = do.call(rbind,lapply(split(intersex.groomings,intersex.groomings$year),function(x) {
	model = with(x,binom.test(sum(actor.as=='AM'),sum(!is.na(actor.as)),p=0.05,alternative='two.sided'))
	data.frame(year=unique(x$year),estimate=model$estimate,lb=model$conf.int[1],ub=model$conf.int[2],pval = model$p.value)
}))

all.years = rbind(all.years,year.by.year)

all.years$year = factor(all.years$year,levels=all.years$year)

all.years$year_type = with(all.years,factor(is.na(as.integer(as.character(year))),levels=c('TRUE','FALSE'),labels=c('all','year')))

p = ggplot() +
	geom_bar(data=all.years,aes(year,estimate,fill=year=='all'),stat='identity',width=0.75) +
	geom_errorbar(data=all.years,aes(x=year,ymin=lb,ymax=ub),width=0.1) +
	geom_hline(yintercept=0.5,linetype=2,linewidth=0.5) +
	facet_grid(.~year_type,scales='free_x',space='free',drop=TRUE) +
	scale_fill_manual(values=c('TRUE'='#a6cee3','FALSE'='#e0e0e0')) +
	scale_y_continuous(breaks=seq(0,1,0.2)) +
	coord_cartesian(ylim=c(0,1)) +
	theme_classic(base_size=16) +
	ylab('Proportion') +
	theme(
		legend.position='none',
		axis.text.x=element_text(angle=-45,hjust=0,vjust=0.5),
		axis.title.x = element_blank(),
		strip.background=element_blank(),
		strip.text=element_blank(),
		plot.margin = margin(t=0,r=20,b=0,l=30)
	) #+
	#ggtitle('Grooming direction by year')
p.groom.direction = p











# Kinda baboon social relationships are unusually inclined towards strong relationships between female and males
alldata.adultfocals = subset(alldata,focal.as %in% c('AF','AM'))

# Summary statistics

# Total focal sample time
sum(c(-10000,diff(alldata.adultfocals $timestamp))>50,na.rm=TRUE)*2/60

# Total number of focals
length(unique(alldata.adultfocals$meta_id))

table(unique(alldata.adultfocals[c('meta_id','focal.as')])$focal.as)







library(lubridate)

alldata.adultdate = subset(alldata.adultfocals,!is.na(dateC) & year(dateC) > 2010)

# Use older life history table but only to call natal males
individuals = readRDS(file.path('../dominance_rank/checkpoints','individuals.rds'))

# adults_boons = individuals[apply(individuals[,grepl('^Age_[0-9]{4}$',colnames(individuals))],1,function(x) any(x %in% 'ADU')),]$ID

natal.males = sort(union(
	unlist(lapply(split(age.nona,age.nona$ID),function(x) if (all(x$Sex == 'M') && any(!x$agesex %in% c('AF','AM'))) unique(x$ID) else character(0))),
	c('LEO','CAT','JK','FRA','NEL','LIB','WOO','SHO')
))


# Sex ratio
mf.ratio.with.natal = reshape2::melt(lapply(split(age.nona,age.nona$year),function(x) {
	these.adults = subset(x,agesex %in% c('AM','AF'))
	table(these.adults$Sex)
}))
names(mf.ratio.with.natal) = c('sex','count','year')

mf.ratio.no.natal = reshape2::melt(lapply(split(age.nona,age.nona$year),function(x) {
	these.adults = subset(x,agesex %in% c('AM','AF') & !ID %in% natal.males)
	table(these.adults$Sex)
}))
names(mf.ratio.no.natal) = c('sex','count','year')

mf.ratio = reshape2::melt(lapply(split(age.nona,age.nona$year),function(x) {
	these.adults = subset(x,agesex %in% c('AM','AF'))
	these.adults$sex2 = these.adults$Sex
	these.adults$sex2[these.adults$ID %in% natal.males] = 'm'
	table(these.adults$sex2)
}))
names(mf.ratio) = c('agesex','count','year')

mf.ratio$sex = factor(mf.ratio$agesex,levels=c('m','M','F'),labels=c('M','M','F'))
mf.ratio$adult = factor(mf.ratio$agesex,levels=c('m','M','F'),labels=c('natal','adult','adult'))
mf.ratio$agesex.label = factor(mf.ratio$agesex,levels=c('m','M','F'),labels=c('male (natal)','male (non-natal)','female'))

mf.ratio2 = reshape2::melt(lapply(split(age.nona,age.nona$year),function(x) {
	these.all = subset(x,TRUE)
	these.all$sex2 = with(these.all,ifelse(!is.na(agesex) & agesex %in% c('AF','AM'),Sex,'na'))
	these.all$sex2[with(these.all,sex2 != 'na' & ID %in% natal.males)] = 'm'
	table(these.all$sex2)
}))
names(mf.ratio2) = c('agesex','count','year')

mf.ratio2$agesex.label = factor(mf.ratio2$agesex,levels=c('na','m','M','F'),labels=c('non-adult','male (natal)','male (non-natal)','female'))


# Sex ratio
p = ggplot(droplevels(subset(mf.ratio,year > 2010 & year < 2020)),aes(factor(year),count,fill=agesex.label)) +
	geom_bar(stat='identity',position=position_stack(),width=0.75) +
	scale_fill_manual(name='Sex',values=c('#fdbf6f','#ff7f00','#33a02c')) +
	theme_classic(base_size=16) +
	theme(
		axis.title.x=element_blank(),
		legend.title=element_blank(),
		axis.text.x = element_text(angle=-45,hjust=0,vjust=0.5),
		plot.margin = margin(t=20,r=10,b=0,l=30)
	) +
	ylab('Count')# +
	#ggtitle('Adult sex composition')
p.mf.ratio = p

p = ggplot(droplevels(subset(mf.ratio2,T)),aes(factor(year),count,fill=agesex.label)) +
	geom_bar(stat='identity',position=position_stack(),width=0.75) +
	scale_fill_manual(name='Sex',values=c('#dddddd','#fdbf6f','#ff7f00','#33a02c')) +
	theme_classic(base_size=16) +
	theme(
		axis.title.x=element_blank(),
		legend.title=element_blank(),
		axis.text.x = element_text(angle=-45,hjust=0,vjust=0.5)
	) +
	ylab('Count')# +
	#ggtitle('Adult sex composition')
p.mf.ratio2 = p

ggsave(p.mf.ratio2,file='figures/age_sex_ratio.pdf',useDingbats=FALSE,height=4)

mf.long = droplevels(subset(mf.ratio,year > 2010))

mf.long$agesex = factor(mf.long$agesex,levels=c('F','M','m'))

mf.wide = do.call(rbind,lapply(split(mf.long,mf.long$year),function(x) {
	if (nrow(x) == 2) {
		x = rbind(
			x,
			data.frame(
				agesex = 'm',
				count = 0,
				year = unique(x$year),
				sex = 'M',
				adult = 'natal',
				agesex.label='male (non-natal)'
			)
		)
	}
	with(x,data.frame(year = unique(year),f = count[agesex=='F'],m = count[agesex=='M'],m_natal = count[agesex=='m'],MF=count[agesex=='M']/sum(count),mf=sum(count[agesex%in%c('m','M')])/sum(count)))
}))


alldata.adultdate$partner.as = agesex.key[with(alldata.adultdate,paste0(partner,lubridate::year(dateC)))]

alldata.split = split(alldata.adultdate,year(alldata.adultdate$dateC))

t.grooming.all.years = do.call(rbind,lapply(split(alldata.adultdate,alldata.adultdate$focalAnimal),function(x) {
	npts = sum(c(-10000,diff(x$timestamp))>50)
	these.adults = unique(subset(age.nona,agesex %in% c('AF','AM') & !ID %in% natal.males,select=c('ID','Sex','agesex')))
	those.adults = unique(rbind(within(unique(subset(alldata.adultdate,select=c('focalAnimal','focal.as'))),{id=focalAnimal;as=focal.as})[c('id','as')],within(subset(unique(subset(alldata.adultdate,select=c('partner','partner.as'))),partner.as %in% c('AF','AM')),{id=partner;as=partner.as})[c('id','as')]))
	if (!is.na(npts) & npts > 120) {
		data.frame(
			id = unique(x$focalAnimal),
			year = -9999,
			sex = substr(unique(x$focal.as),2,2),
			count = as.integer(c(
				table(factor(subset(x,focalBehavior == 'G' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
				table(factor(subset(x,focalBehavior == 'B' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
			)),
			norm = as.numeric(c(
				table(factor(subset(x,focalBehavior == 'G' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
				table(factor(subset(x,focalBehavior == 'B' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
			) / npts),
			norm_mf = as.numeric(c(
				table(factor(subset(x,focalBehavior == 'G' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
				table(factor(subset(x,focalBehavior == 'B' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
			) / npts) / as.integer(table(these.adults$agesex)[c('AF','AM','AF','AM')]) * nrow(these.adults) / 2,
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

n.grooming.all.years = reshape2::melt(with(subset(alldata.adultdate,lubridate::year(dateC) >= 2011 & lubridate::year(dateC) <= 2019 & actor.as %in% c('AM','AF') & recipient.as %in% c('AM','AF') & behavior %in% 'G'),table(actor.as,recipient.as)))

n.grooming.all.years = within(n.grooming.all.years,{
	x_label = factor(paste(actor.as,recipient.as,sep='-'),levels=c('AF-AF','AM-AF','AF-AM','AM-AM'),labels=c('between\nfemales','between\nsexes','between\nsexes','between\nmales'))
	x_label2 = factor(paste(actor.as,recipient.as,sep='-'),levels=c('AF-AF','AM-AF','AF-AM','AM-AM'),labels=c('between females','between sexes','between sexes','between males'))
	sex_label = factor(actor.as,levels=c('AF','AM'),labels=c('female','male'))
	proportion = value / sum(value)
})

n.grooming.all.years.error = do.call(rbind,lapply(split(n.grooming.all.years,n.grooming.all.years$x_label),function(x) {
	this_count = sum(x$value)
	total_count = sum(n.grooming.all.years$value)
	result = binom.test(this_count,total_count,p=0.5)
	data.frame(
		year = -9999,
		x_label = unique(x$x_label),
		x_label2 = unique(x$x_label2),
		lb = result$conf.int[1],
		ub = result$conf.int[2],
		lb_count = result$conf.int[1] * total_count,
		ub_count = result$conf.int[2] * total_count
	)
}))

n.grooming.per.year = do.call(rbind,lapply(alldata.split,function(z) {
	out = reshape2::melt(with(subset(z,lubridate::year(dateC) >= 2011 & lubridate::year(dateC) <= 2019 & actor.as %in% c('AM','AF') & recipient.as %in% c('AM','AF') & behavior %in% 'G'),table(actor.as,recipient.as)))
	out = within(out,{
		year = unique(z$year)
		x_label = factor(paste(actor.as,recipient.as,sep='-'),levels=c('AF-AF','AM-AF','AF-AM','AM-AM'),labels=c('between\nfemales','between\nsexes','between\nsexes','between\nmales'))
		x_label2 = factor(paste(actor.as,recipient.as,sep='-'),levels=c('AF-AF','AM-AF','AF-AM','AM-AM'),labels=c('between females','between sexes','between sexes','between males'))
		sex_label = factor(actor.as,levels=c('AF','AM'),labels=c('female','male'))
		proportion = value / sum(value)
	})
	out
}))

p.grooming.all.years = do.call(rbind,lapply(split(alldata.adultdate,alldata.adultdate$focalAnimal),function(x) {
	npts = sum(c(-10000,diff(x$timestamp))>50)
	these.adults = subset(age.nona,agesex %in% c('AF','AM') & !ID %in% natal.males)
	if (!is.na(npts) & npts > 120) {
		data.frame(
			id = unique(x$focalAnimal),
			year = -9999,
			sex = substr(unique(x$focal.as),2,2),
			count = as.integer(c(
				table(factor(subset(x,focalBehavior == 'G' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
				table(factor(subset(x,focalBehavior == 'B' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
			)),
			norm = as.integer(c(
				table(factor(subset(x,focalBehavior == 'G' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
				table(factor(subset(x,focalBehavior == 'B' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
			)) / sum(as.integer(c(
				table(factor(subset(x,focalBehavior == 'G' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
				table(factor(subset(x,focalBehavior == 'B' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
			))),
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


t.grooming.all.years.undirected = do.call(rbind,lapply(split(alldata.adultdate,alldata.adultdate$focalAnimal),function(x) {
	npts = sum(c(-10000,diff(x$timestamp))>50)
	these.adults = unique(subset(age.nona,agesex %in% c('AF','AM') & !ID %in% natal.males,select=c('ID','Sex','agesex')))
	those.adults = unique(rbind(within(unique(subset(alldata.adultdate,select=c('focalAnimal','focal.as'))),{id=focalAnimal;as=focal.as})[c('id','as')],within(subset(unique(subset(alldata.adultdate,select=c('partner','partner.as'))),partner.as %in% c('AF','AM')),{id=partner;as=partner.as})[c('id','as')]))
	if (!is.na(npts) & npts > 120) {
		data.frame(
			id = unique(x$focalAnimal),
			year = -9999,
			sex = substr(unique(x$focal.as),2,2),
			count = as.integer(c(
				table(factor(subset(x,focalBehavior %in% c('G','B') & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
			)),
			norm = as.numeric(c(
				table(factor(subset(x,focalBehavior %in% c('G','B') & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
			) / npts),
			partner.as = c(
				'AF','AM'
			)
		)
	} else {
		data.frame()
	}
}))


t.grooming.per.year = do.call(rbind,lapply(alldata.split,function(z) {
	this.year = unique(lubridate::year(z$dateC))
	these.adults = subset(age.nona,year == this.year & agesex %in% c('AF','AM') & !ID %in% natal.males)
	# mf.ratio
	do.call(rbind,lapply(split(z,z$focalAnimal),function(x) {
		npts = sum(c(-10000,diff(x$timestamp))>50)
		if (!is.na(npts) & npts > 120) {
			data.frame(
				id = unique(x$focalAnimal),
				year = this.year,
				sex = substr(unique(x$focal.as),2,2),
				count = as.integer(c(
					table(factor(subset(x,focalBehavior == 'G' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
					table(factor(subset(x,focalBehavior == 'B' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
				)),
				norm = as.numeric(c(
					table(factor(subset(x,focalBehavior == 'G' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
					table(factor(subset(x,focalBehavior == 'B' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
				) / npts),
				norm_mf = as.numeric(c(
					table(factor(subset(x,focalBehavior == 'G' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM'))),
					table(factor(subset(x,focalBehavior == 'B' & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
				) / npts) / rep(as.integer(table(these.adults$Sex)),2) * mean(table(these.adults$Sex)),
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

t.grooming.per.year.undirected = do.call(rbind,lapply(alldata.split,function(z) {
	this.year = unique(lubridate::year(z$dateC))
	these.adults = subset(age.nona,year == this.year & agesex %in% c('AF','AM') & !ID %in% natal.males)
	# mf.ratio
	do.call(rbind,lapply(split(z,z$focalAnimal),function(x) {
		npts = sum(c(-10000,diff(x$timestamp))>50)
		if (!is.na(npts) & npts > 120) {
			data.frame(
				id = unique(x$focalAnimal),
				year = this.year,
				sex = substr(unique(x$focal.as),2,2),
				count = as.integer(c(
					table(factor(subset(x,focalBehavior %in% c('G','B') & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
				)),
				norm = as.numeric(c(
					table(factor(subset(x,focalBehavior %in% c('G','B') & partner %in% these.adults$ID & partner.as %in% c('AM','AF'))$partner.as,levels=c('AF','AM')))
				) / npts)
			)
		} else {
			data.frame()
		}
	}))
}))


t.grooming.per.year$direction_label = factor(t.grooming.per.year$direction,levels=c(1,0),labels=c('give groom','receive groom'))
t.grooming.all.years$direction_label = factor(t.grooming.all.years$direction,levels=c(1,0),labels=c('give groom','receive groom'))
p.grooming.all.years$direction_label = factor(p.grooming.all.years$direction,levels=c(1,0),labels=c('give groom','receive groom'))

t.grooming.per.year$partner_label = factor(t.grooming.per.year$partner.as,levels=c('AF','AM'),labels=c('female\npartners','male\npartners'))
t.grooming.all.years$partner_label = factor(t.grooming.all.years$partner.as,levels=c('AF','AM'),labels=c('female\npartners','male\npartners'))
t.grooming.all.years.undirected$partner_label = factor(t.grooming.all.years.undirected$partner.as,levels=c('AF','AM'),labels=c('female\npartners','male\npartners'))
p.grooming.all.years$partner_label = factor(p.grooming.all.years$partner.as,levels=c('AF','AM'),labels=c('female\npartners','male\npartners'))

t.grooming.per.year$sex_label = factor(t.grooming.per.year$sex,levels=c('F','M'),labels=c('females','males'))
t.grooming.all.years$sex_label = factor(t.grooming.all.years$sex,levels=c('F','M'),labels=c('females','males'))
t.grooming.all.years.undirected$sex_label = factor(t.grooming.all.years.undirected$sex,levels=c('F','M'),labels=c('females','males'))
p.grooming.all.years$sex_label = factor(p.grooming.all.years$sex,levels=c('F','M'),labels=c('females','males'))




p = ggplot() +
	geom_bar(data=subset(n.grooming.all.years),mapping=aes(x_label,value,fill=sex_label),stat='identity',position=position_stack(),width=0.75) +
	geom_errorbar(data=n.grooming.all.years.error,mapping=aes(x_label,ymin=lb_count,ymax=ub_count),width=0.2) +
	scale_fill_manual(name='Grooming sex',values=c('male'='#ff7f00','female'='#33a02c')) +
	theme_classic(base_size=16) +
	theme(
		axis.title.x=element_blank()
	) +
	ylab('Count')

n.grooming.per.year = do.call(rbind,lapply(split(n.grooming.per.year,n.grooming.per.year$year),function(x) {
	this.year = unique(x$year)
	mf = subset(mf.ratio,year == this.year & agesex != 'm')
	out = do.call(rbind,lapply(split(x,x$x_label2),function(y) {
		this.label = unique(y$x_label2)
		y$count_adj = y$value / (
			if (this.label == 'between females') {
				subset(mf,sex=='F')$count * (subset(mf,sex=='F')$count - 1)
			} else if (this.label == 'between sexes') {
				subset(mf,sex=='F')$count * subset(mf,sex=='M')$count
			} else if (this.label == 'between males') {
				subset(mf,sex=='M')$count * (subset(mf,sex=='M')$count - 1)
			}
		)
		y
	}))
	out$proportion_mf = out$count_adj / sum(out$count_adj)
	out$count_mf = out$proportion_mf * sum(out$value)
	out
}))

n.grooming.per.year.error = do.call(rbind,lapply(split(n.grooming.per.year,n.grooming.per.year$year),function(z) {
	out = do.call(rbind,lapply(split(z,z$x_label),function(x) {
		this_count = sum(x$value)
		total_count = sum(z$value)
		result = binom.test(this_count,total_count,p=0.5)
		
		this.year = unique(x$year)
		this.label = unique(x$x_label2)
		mf = subset(mf.ratio,year == this.year & agesex != 'm')
		
		lb_adj = result$conf.int[1] / (
			if (this.label == 'between females') {
				subset(mf,sex=='F')$count * (subset(mf,sex=='F')$count - 1)
			} else if (this.label == 'between sexes') {
				subset(mf,sex=='F')$count * subset(mf,sex=='M')$count
			} else if (this.label == 'between males') {
				subset(mf,sex=='M')$count * (subset(mf,sex=='M')$count - 1)
			}
		)
		ub_adj = result$conf.int[2] / (
			if (this.label == 'between females') {
				subset(mf,sex=='F')$count * (subset(mf,sex=='F')$count - 1)
			} else if (this.label == 'between sexes') {
				subset(mf,sex=='F')$count * subset(mf,sex=='M')$count
			} else if (this.label == 'between males') {
				subset(mf,sex=='M')$count * (subset(mf,sex=='M')$count - 1)
			}
		)
		
		scale.factor = ifelse(all(x$proportion_mf == 0),
			1,
			sum(x$proportion_mf) / (sum(x$proportion) / (
				if (this.label == 'between females') {
					subset(mf,sex=='F')$count * (subset(mf,sex=='F')$count - 1)
				} else if (this.label == 'between sexes') {
					subset(mf,sex=='F')$count * subset(mf,sex=='M')$count
				} else if (this.label == 'between males') {
					subset(mf,sex=='M')$count * (subset(mf,sex=='M')$count - 1)
				}
			))
		)
		
		out2 = data.frame(
			year = unique(z$year),
			x_label = unique(x$x_label),
			x_label2 = unique(x$x_label2),
			lb = result$conf.int[1],
			ub = result$conf.int[2],
			lb_adj = lb_adj * scale.factor,
			ub_adj = ub_adj * scale.factor,
			lb_count = result$conf.int[1] * total_count,
			ub_count = result$conf.int[2] * total_count
		)
		# out2$lb_adj = ifelse(out2$lb_adj < 0,0,out2$lb_adj)
		# out2$ub_adj = ifelse(out2$ub_adj > 1,1,out2$ub_adj)
		out2
	}))
	out
}))

p = ggplot() +
	geom_bar(data=subset(n.grooming.per.year,year < 2019),mapping=aes(x_label2,proportion),stat='identity',position=position_stack(),width=0.75) +
	geom_errorbar(data=subset(n.grooming.per.year.error,year<2019),mapping=aes(x_label2,ymin=lb,ymax=ub),width=0.2) +
	facet_wrap(~year,nrow=1,strip.position='top',scales='fixed') +
	scale_fill_manual(name='Grooming sex',values=c('male'='#ff7f00','female'='#33a02c')) +
	scale_y_continuous(limits=c(0,0.8),breaks=seq(0,0.8,0.2)) +
	theme_classic(base_size=16) +
	theme(
		axis.title.x=element_blank(),
		strip.background=element_blank(),
		axis.text.x=element_text(angle=-45,hjust=0,vjust=0.5)
	) +
	ylab('Proportion')


n.between.sex.grooming.per.year = do.call(
	rbind,
	lapply(
		with(subset(n.grooming.per.year,year <= 2019 & x_label2 == 'between sexes'),split(data.frame(value,proportion,proportion_mf,count_mf,x_label2,x_label,year),year)),
		function(x) {
			data.frame(
				value=sum(x$value),
				proportion=sum(x$proportion),
				proportion_mf=sum(x$proportion_mf),
				count_mf=sum(x$count_mf),
				unique(x[5:7])
			)
		}
	)
)


p = ggplot() +
	geom_ribbon(data=subset(n.grooming.per.year.error,year < 2019 & x_label2 == 'between sexes'),mapping=aes(year,ymin=lb,ymax=ub),fill='#deebf7',alpha=0.75) +
	geom_point(data=n.between.sex.grooming.per.year,aes(year,proportion)) +
	geom_line(data=n.between.sex.grooming.per.year,aes(year,proportion)) +
	# facet_wrap(~year,nrow=1,strip.position='top',scales='fixed') +
	# scale_fill_manual(name='Grooming sex',values=c('male'='#ff7f00','female'='#33a02c')) +
	# scale_y_continuous(limits=c(0,0.8),breaks=seq(0,0.8,0.2)) +
	scale_x_continuous(limits=c(2011,2018),breaks=seq(2011,2018,1)) +
	theme_classic(base_size=16) +
	theme(
		axis.title.x=element_blank(),
		strip.background=element_blank(),
		axis.text.x=element_text(angle=-45,hjust=0,vjust=0.5)
	) +
	ylab('Proportion')

# Normalize to sex ratio (wideform)

p = ggplot() +
	geom_ribbon(data=subset(n.grooming.per.year.error,x_label2 == 'between sexes'),mapping=aes(year,ymin=lb_adj,ymax=ub_adj),fill='#a1d99b',alpha=0.75) +
	geom_line(data=n.between.sex.grooming.per.year,aes(year,proportion_mf),color='#31a354') +
	geom_point(data=n.between.sex.grooming.per.year,aes(year,proportion_mf),size=2,color='#31a354') +
	geom_ribbon(data=subset(n.grooming.per.year.error,x_label2 == 'between sexes'),mapping=aes(year,ymin=lb,ymax=ub),fill='#bcbddc',alpha=0.5) +
	geom_line(data=n.between.sex.grooming.per.year,aes(year,proportion),linetype=3,color='#756bb1',linewidth=0.5) +
	geom_point(data=n.between.sex.grooming.per.year,aes(year,proportion),shape=21,size=2,color='#756bb1') +
	# facet_wrap(~year,nrow=1,strip.position='top',scales='fixed') +
	# scale_fill_manual(name='Grooming sex',values=c('male'='#ff7f00','female'='#33a02c')) +
	# scale_y_continuous(limits=c(0,0.8),breaks=seq(0,0.8,0.2)) +
	scale_x_continuous(limits=c(2011,2019),breaks=seq(2011,2019,1)) +
	scale_y_continuous(limits=c(0.2,max(subset(n.grooming.per.year.error,x_label2 == 'between sexes')$ub_adj)),breaks=seq(0,1,0.5)) +
	theme_classic(base_size=16) +
	theme(
		axis.title.x=element_blank(),
		strip.background=element_blank(),
		axis.text.x=element_text(angle=-45,hjust=0,vjust=0.5)
	) +
	ylab('Proportion')

x1 = subset(n.grooming.per.year.error,x_label2 == 'between sexes',select=c('year','lb','ub'))
x2 = subset(n.grooming.per.year.error,x_label2 == 'between sexes',select=c('year','lb_adj','ub_adj'))
x3 = subset(n.between.sex.grooming.per.year,select=c('year','proportion'))
x4 = subset(n.between.sex.grooming.per.year,select=c('year','proportion_mf'))

x1$category = factor('raw proportions',levels=c('raw proportions','adjusted for sex ratio'))
x2$category = factor('adjusted for sex ratio',levels=c('raw proportions','adjusted for sex ratio'))
x3$category = factor('raw proportions',levels=c('raw proportions','adjusted for sex ratio'))
x4$category = factor('adjusted for sex ratio',levels=c('raw proportions','adjusted for sex ratio'))

names(x1) = names(x2) = c('year','lb','ub','category')
names(x3) = names(x4) = c('year','proportion','category')

x.1 = rbind(x1,x2)
x.2 = rbind(x3,x4)

# Normalize to sex ratio (longform)
p = ggplot() +
	geom_ribbon(data=within(x.1,{ub=ifelse(ub>1,1,ub)}),mapping=aes(year,ymin=lb,ymax=ub,fill=category),alpha=1) +
	geom_line(data=x.2,aes(year,proportion,color=category,linetype=category)) +
	geom_point(data=x.2,aes(year,proportion,color=category,shape=category),size=2) +
	facet_wrap(~category,nrow=1,strip.position='top',scales='fixed') +
	# scale_fill_manual(name='Grooming sex',values=c('male'='#ff7f00','female'='#33a02c')) +
	# scale_y_continuous(limits=c(0,0.8),breaks=seq(0,0.8,0.2)) +
	scale_fill_manual(values=c('#fbb4ae','#b3cde3')) +
	scale_color_manual(values=c('#e41a1c','#377eb8')) +
	scale_linetype_manual(values=c(2,1)) +
	scale_shape_manual(values=c(21,19)) +
	scale_x_continuous(limits=c(2011,2019),breaks=seq(2011,2019,1)) +
	scale_y_continuous(limits=c(0,max(x.1$ub)),breaks=seq(0,1,0.5)) +
	coord_cartesian(ylim=c(0,1)) +
	theme_classic(base_size=16) +
	theme(
		axis.title.x=element_blank(),
		strip.background=element_blank(),
		axis.text.x=element_text(angle=-45,hjust=0,vjust=0.5),
		panel.grid.major.y=element_line(linetype=2,linewidth=0.5,color='#cccccc'),
		legend.position='none',
		plot.margin = margin(t=0,r=20,b=0,l=30)
	) +
	ylab('Proportion') #+
	#ggtitle('Between-sex grooming proportions')
p.intersex.mf = p



dodge = position_dodge(width = 0.9)
p = ggplot(subset(t.grooming.per.year,TRUE)) +
	geom_quasirandom(aes(partner_label,norm,color=direction_label),dodge.width=0.9) +
	geom_boxplot(aes(partner_label,norm,alpha=direction_label),position=dodge,width=0.1,outlier.shape=NA,show.legend=FALSE) +
	facet_grid(year~factor(sex)) +
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
		strip.background=element_blank(),
		strip.text.y.right=element_text(angle=0)
	) +
	guides(
		color=guide_legend(override.aes=list(size=3))
	) +
	ylab('Fraction activity budget') +
	ggtitle('Groomings across adults')

p1.5 = ggplot(subset(t.grooming.per.year,TRUE)) +
	geom_quasirandom(aes(partner_label,norm_mf,color=direction_label),dodge.width=0.9) +
	geom_boxplot(aes(partner_label,norm_mf,alpha=direction_label),position=dodge,width=0.1,outlier.shape=NA,show.legend=FALSE) +
	facet_grid(year~sex_label) +
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
		strip.background=element_blank(),
		strip.text.y.right=element_text(angle=0)
	) +
	guides(
		color=guide_legend(override.aes=list(size=3))
	) +
	ylab('Fraction activity budget') +
	ggtitle('Groomings across adults')


p1 = ggplot(subset(t.grooming.all.years,TRUE)) +
	geom_quasirandom(aes(partner_label,norm,color=direction_label),dodge.width=0.9) +
	geom_boxplot(aes(partner_label,norm,alpha=direction_label),position=dodge,width=0.1,outlier.shape=NA,show.legend=FALSE) +
	facet_grid(~sex_label) +
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
		# axis.text.x=element_text(angle=-45,hjust=0,vjust=0.5),
		strip.background=element_blank()
	) +
	guides(
		color=guide_legend(override.aes=list(size=3))
	) +
	ylab('Fraction activity budget') +
	ggtitle('Raw grooming rates')


t.grooming.all.years.undirected$partner_label2 = factor(t.grooming.all.years.undirected$partner.as,levels=c('AF','AM'),labels=c('female','male'))

p1.0 = ggplot(subset(t.grooming.all.years.undirected,TRUE)) +
	geom_quasirandom(aes(partner_label2,norm,color=partner.as),dodge.width=0.9) +
	geom_boxplot(aes(partner_label2,norm),position=dodge,width=0.1,outlier.shape=NA,show.legend=FALSE) +
	facet_grid(~sex_label) +
	scale_color_manual(
		values = c(
			'AF' = '#31a354',
			'AM' = '#756bb1'
		)
	) +
	# scale_color_manual(
	# 	values = c(
	# 		'Cycling'     = '#1b9e77',
	# 		'Pregnant'    = '#d95f02',
	# 		'Lactating'   = '#7570b3',
	# 		'none (male)' = '#000000'
	# 	)
	# ) +
	# scale_color_manual(
	# 	values = c(
	# 		'give groom'   = '#E64B35',
	# 		'receive groom'= '#4DBBD5'
	# 	)
	# ) +
	# scale_alpha_manual(
	# 	values = c(
	# 		'give groom'   = 0.75,
	# 		'receive groom'= 0.75
	# 	)
	# ) +
	theme_classic(base_size=16) +
	theme(
		legend.position='none',
		legend.title=element_blank(),
		# axis.title.x=element_blank(),
		# axis.text.x=element_text(angle=-45,hjust=0,vjust=0.5),
		strip.background=element_blank()
	) +
	guides(
		color=guide_legend(override.aes=list(size=3))
	) +
	xlab('Partner sex') + ylab('Rate') #+
	#ggtitle('Raw grooming rates')
p.grooming.rates = p1.0




egg::ggarrange(p.mf.ratio,p.intersex.mf,p.groom.direction,nrow=3)

p.fig1 = ggpubr::ggarrange(p.mf.ratio,p.intersex.mf,ggpubr::ggarrange(p.groom.direction,p.grooming.rates,nrow=1,widths=c(2,2),labels=c('C','D')),ncol=1,heights=c(2,2.25,2),labels=c('A','B',''))

pdf(file='figures/fig1.pdf',useDingbats=FALSE)
p.fig1
dev.off()




p1.1 = ggplot(subset(t.grooming.all.years,TRUE)) +
	geom_quasirandom(aes(partner_label,norm_mf,color=direction_label),dodge.width=0.9) +
	geom_boxplot(aes(partner_label,norm_mf,alpha=direction_label),position=dodge,width=0.1,outlier.shape=NA,show.legend=FALSE) +
	facet_grid(~sex_label) +
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
		# axis.text.x=element_text(angle=-45,hjust=0,vjust=0.5),
		strip.background=element_blank()
	) +
	guides(
		color=guide_legend(override.aes=list(size=3))
	) +
	ylab('Fraction activity budget (normalized)') +
	ggtitle('Normalized grooming rates')


p2 = ggplot(subset(p.grooming.all.years,TRUE)) +
	geom_quasirandom(aes(partner_label,norm,color=direction_label),dodge.width=0.9) +
	geom_boxplot(aes(partner_label,norm,alpha=direction_label),position=dodge,width=0.1,outlier.shape=NA,show.legend=FALSE) +
	facet_grid(~sex_label) +
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
		# axis.text.x=element_text(angle=-45,hjust=0,vjust=0.5),
		strip.background=element_blank()
	) +
	guides(
		color=guide_legend(override.aes=list(size=3))
	) +
	ylab('Fraction groomings') +
	ggtitle('Groomings across adults')




# # DISABLE THE CHUNK OF CODE BELOW
# if (FALSE) {
# foo = lapply(alldata.split,function(x) {
# 	this.year = unique(year(x$dateC))
# 	these.adults = subset(age.nona,year == this.year & agesex %in% c('AF','AM') & !ID %in% natal.males)
# 	groom = ct(x,behaviors='G',individuals=these.adults$ID,directional=TRUE)
# 	groom_freqs = bf(groom,digits=2)
# 	groom_freqs_mf = subset.as(groom_freqs,these.adults,all.x='M',all.y='F')
# 	groom_freqs_fm = subset.as(groom_freqs,these.adults,all.x='F',all.y='M')
# 	groom_freqs_ff = subset.as(groom_freqs,these.adults,all.x='F',all.y='F')
# 	groom_freqs_mm = subset.as(groom_freqs,these.adults,all.x='M',all.y='M')
# 	diag(groom_freqs_ff$array) = NA
# 	diag(groom_freqs_mm$array) = NA
# 
# 	ff_grm_freqs = sum(groom_freqs_ff$array,na.rm=TRUE)
# 	mf_grm_freqs = sum(groom_freqs_mf$array,na.rm=TRUE)
# 	fm_grm_freqs = sum(groom_freqs_fm$array,na.rm=TRUE)
# 	mm_grm_freqs = sum(groom_freqs_mm$array,na.rm=TRUE)
# 
# 	# RAW COUNTS
# 	groom_mf = subset.as2(groom$contingency,these.adults,all.x='M',all.y='F')
# 	groom_fm = subset.as2(groom$contingency,these.adults,all.x='F',all.y='M')
# 	groom_ff = subset.as2(groom$contingency,these.adults,all.x='F',all.y='F')
# 	groom_mm = subset.as2(groom$contingency,these.adults,all.x='M',all.y='M')
# 
# 	diag(groom_mm) = NA
# 	diag(groom_ff) = NA
# 
# 	ff_grm = sum(groom_ff,na.rm=TRUE)
# 	mf_grm = sum(groom_mf,na.rm=TRUE)
# 	fm_grm = sum(groom_fm,na.rm=TRUE)
# 	mm_grm = sum(groom_mm,na.rm=TRUE)
# 
# 	groom.by.sex = c(FF=ff_grm,MF=mf_grm,FM=fm_grm,MM=mm_grm)
# 
# 	groom.freqs.by.sex = c(FF=ff_grm_freqs,MF=mf_grm_freqs,FM=fm_grm_freqs,MM=mm_grm_freqs)
# 	groom.opportunity.by.sex = c(FF=sum(!is.na(groom_ff)),MF=sum(!is.na(groom_mf)),FM=sum(!is.na(groom_fm)),MM=sum(!is.na(groom_mm)))
# 	groom.freqs.normalized = groom.freqs.by.sex / groom.opportunity.by.sex
# 	list(
# 		freq=groom.freqs.by.sex,
# 		opp=groom.opportunity.by.sex,
# 		norm=groom.freqs.normalized
# 	)
# })
# 
# mf.ratios = do.call(rbind,lapply(foo,function(x) {
# 	out1 = reshape2::melt(x$freq / sum(x$freq))
# 	out1$comparison=rownames(out1)
# 	out1$type = 'frequencies'
# 	out2 = reshape2::melt(x$norm / sum(x$norm))
# 	out2$comparison=rownames(out2)
# 	out2$type = 'normalized'
# 	rbind(out1,out2)
# }))
# mf.ratios$year = as.integer(substr(rownames(mf.ratios),1,4))
# 
# mf.ratios = subset(mf.ratios,!is.nan(value))
# mf.ratios$comparison = factor(mf.ratios$comparison,levels=c('FF','FM','MF','MM'))
# 
# # Male-female groomings
# p = ggplot() +
# 	geom_bar(aes(year,value,fill=comparison),data=subset(mf.ratios,comparison%in% c('FF','MM')),stat='identity',position=position_dodge2(width=1,padding=0.5)) +
# 	geom_bar(aes(year,value,fill=comparison),data=subset(mf.ratios,comparison%in% c('MF','FM')),stat='identity',position=position_stack(),width=0.225) +
# 	facet_wrap(~type,nrow=2,scales='fixed') +
# 	scale_fill_brewer(palette='Dark2',name='Category',labels=c('F->F','F->M','M->F','M->M')) +
# 	scale_x_continuous(breaks=seq(range(mf.ratios$year)[1],range(mf.ratios$year)[2],1)) +
# 	scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.5)) +
# 	theme_classic(base_size=24) +
# 	theme(
# 		axis.title.x=element_blank(),
# 		strip.background=element_blank(),
# 		strip.text=element_text(hjust=0),
# 		axis.text.x=element_text(angle=-45,hjust=0,vjust=0.5)
# 	) +
# 	ylab('Proportion')
# }

# mf.2013 = read.delim('data/comparative_mf.txt')
# mf.2013$species = factor(mf.2013$species,levels=c('papio','hamadryas','anubis','ursinus','cynocephalus','kindae'),labels=c('Guinea','Hamadryas','Anubis','Chacma','Yellow','Kinda'))
# 
# p = ggplot(data=droplevels(subset(mf.2013,species %in% c('Chacma','Yellow','Kinda'))),aes(species,proportion,fill=female)) +
# 	geom_bar(stat='identity',position=position_dodge()) +
# 	coord_flip() +
# 	scale_fill_brewer(palette='Dark2') +
# 	theme_classic(base_size=24) + theme(legend.title=element_blank(),axis.title.y=element_blank()) +
# 	ylab('Proportion')
	

# Now do sociality scores


saveRDS(alldata.split,file='checkpoints/alldata_split.rds')

foo = do.call(rbind,lapply(alldata.split,function(x) {
	this.year = unique(year(x$dateC))
	these.adults = subset(age.nona,year == this.year & agesex %in% c('AF','AM') & !ID %in% natal.males)
	groom = ct(x,behaviors='G',individuals=these.adults$ID,directional=FALSE)
	groom_freqs = bf(groom,prettify=FALSE)$table
	rownames(groom_freqs) = with(groom_freqs,paste0(Var1,'_',Var2))
	groom.key = groom_freqs$scores
	names(groom.key) = rownames(groom_freqs)
	
	groomD = ct(x,behaviors='G',individuals=these.adults$ID,directional=TRUE)
	groomD_freqs = bf(groomD,prettify=FALSE)$table
	rownames(groomD_freqs) = with(groomD_freqs,paste0(Var1,'_',Var2))
	groomD.key = groomD_freqs$scores
	names(groomD.key) = rownames(groomD_freqs)

	prox = ct(x,behaviors=distances,individuals=these.adults$ID)
	prox_cscore = cs(prox,prettify=FALSE)$table
	rownames(prox_cscore) = with(prox_cscore,paste0(Var1,'_',Var2))
	prox.key = prox_cscore$scores
	names(prox.key) = rownames(prox_cscore)
	
	out = groom_freqs[,1:2]
	out$groom = groom.key[rownames(out)]
	out$groomDirectional = groomD.key[rownames(out)]
	out$prox = prox.key[rownames(out)]
	out$socialist = with(
		out,
		(groom/mean(groom) + prox/mean(prox)) / 2
	)
	out$sex1 = as.character(sex.key[out$Var1])
	out$sex2 = as.character(sex.key[out$Var2])
	out$year = unique(lubridate::year(x$dateC))
	out
}))

foo$type = factor(with(foo,paste0(sex1,sex2)),levels=c('FF','FM','MF','MM'))
foo$type2 = factor(foo$type,levels=c('FF','FM','MF','MM'),labels=c('FF','FM','FM','MM'))

foo$Var1 = factor(foo$Var1)
foo$Var2 = factor(foo$Var2)

foo$Var3 = with(foo,ifelse(as.integer(Var1) < as.integer(Var2),as.character(Var1),as.character(Var2)))
foo$Var4 = with(foo,ifelse(as.integer(Var1) < as.integer(Var2),as.character(Var2),as.character(Var1)))
foo$dyadkey = with(foo,paste0(ifelse(as.integer(Var1) < as.integer(Var2),paste0(Var1,'_',Var2),paste0(Var2,'_',Var1)),'_',year))

socialist.undirected = droplevels(unique(subset(foo,year < 2019,select=c('Var3','Var4','dyadkey','type2','year','socialist'))))

socialist.undirected$friendship = socialist.undirected$socialist > quantile(socialist.undirected$socialist,0.95)
socialist.undirected$tukey.friendship = socialist.undirected$socialist > quantile(socialist.undirected$socialist,0.75) + 1.5 * abs(diff(quantile(socialist.undirected$socialist,c(0.75,0.25))))
socialist.undirected$quartile.friendship = socialist.undirected$socialist > quantile(socialist.undirected$socialist,0.75)
socialist.undirected$median.friendship = socialist.undirected$socialist > quantile(socialist.undirected$socialist,0.5)

p = ggplot(within(subset(socialist.undirected,socialist>0),{type_label=factor(type2,levels=c('FF','FM','MM'),labels=c('female-female','female-male','male-male')); friendship_label = factor(friendship,levels=c('FALSE','TRUE'),labels=c('not friends','friends'))}),aes(socialist,fill=friendship_label)) +
	geom_histogram() +
	facet_grid(year~type_label,scales='free_y') +
	scale_fill_manual(values=c('friends'='#ff0000','not friends'='#000000')) +
	theme_classic(base_size=16) +
	theme(
		strip.background=element_blank(),
		strip.text.y.right = element_text(angle=0),
		legend.title=element_blank(),
		axis.text.y=element_blank(),
		axis.ticks.y=element_blank(),
		legend.position = 'bottom'
	) +
	xlab('Sociality index') +
	ylab('Count')

# Now classify friendships using the same nonredundant quantile
socialist.perboon = subset(foo,year < 2019)


socialist.perboon$friendship = socialist.perboon$socialist > quantile(socialist.undirected$socialist,0.95)
socialist.perboon$tukey.friendship = socialist.perboon$socialist > quantile(socialist.undirected$socialist,0.75) + 1.5 * abs(diff(quantile(socialist.undirected$socialist,c(0.75,0.25))))
socialist.perboon$quartile.friendship = socialist.perboon$socialist > quantile(socialist.undirected$socialist,0.75)
socialist.perboon$median.friendship = socialist.perboon$socialist > quantile(socialist.undirected$socialist,0.5)



library(ggbeeswarm)
library(ggrepel)

socialist.perboon$sex_label = factor(socialist.perboon$sex2,levels=c('F','M'),labels=c('females','males'))

# Example of a male
set.seed(42)
p.mrw = ggplot() +
	geom_quasirandom(
		data=subset(socialist.perboon,Var1 == 'MRW' & Var2 %in% unique(Var2[Var1 == 'MRW' & tukey.friendship])),
		aes(1,socialist,color=tukey.friendship)
	) +
	geom_quasirandom(
		data=subset(socialist.perboon,Var1 == 'MRW' & !Var2 %in% unique(Var2[Var1 == 'MRW' & tukey.friendship])),
		aes(1,socialist),
		color='#000000'
	) +
	geom_text_repel(
		data = subset(socialist.perboon,Var1 == 'MRW' & Var2 %in% unique(Var2[Var1 == 'MRW' & tukey.friendship])),
		aes(1,socialist,label=Var2,color=tukey.friendship),
        position=position_quasirandom(),
        min.segment.length = 0,
        seed=42
    ) +
    facet_grid(year~sex_label,scales='free_y') +
	coord_flip(ylim=c(0,20)) +
	scale_color_manual(values=c('TRUE'='#ff0000','FALSE'='#000000')) +
	theme_classic(base_size=12) +
	theme(
		strip.background=element_blank(),
		strip.text.y.right = element_text(angle=0),
		axis.text.x=element_blank(),
		axis.title.x=element_blank(),
		axis.text.y = element_blank(),
		axis.ticks.y = element_blank(),
		# axis.title.y = element_text(angle=0,vjust=0.5),
		axis.title.y = element_text(),
		legend.position='none'
	) +
	xlab('MRW') +
	ylab('Sociality index')

# Example of a female
set.seed(42)
p.dol = ggplot() +
	geom_quasirandom(
		data=subset(socialist.perboon,Var1 == 'DOL' & Var2 %in% unique(Var2[Var1 == 'DOL' & tukey.friendship])),
		aes(1,socialist,color=tukey.friendship)
	) +
	geom_quasirandom(
		data=subset(socialist.perboon,Var1 == 'DOL' & !Var2 %in% unique(Var2[Var1 == 'DOL' & tukey.friendship])),
		aes(1,socialist),
		color='#000000'
	) +
	geom_text_repel(
		data = subset(socialist.perboon,Var1 == 'DOL' & Var2 %in% unique(Var2[Var1 == 'DOL' & tukey.friendship])),
		aes(1,socialist,label=Var2,color=tukey.friendship),
        position=position_quasirandom(),
        min.segment.length = 0,
        seed=42
    ) +
    facet_grid(year~sex_label,scales='free_y') +
	coord_flip(ylim=c(0,20)) +
	scale_color_manual(values=c('TRUE'='#ff0000','FALSE'='#000000')) +
	theme_classic(base_size=12) +
	theme(
		strip.background=element_blank(),
		strip.text.x.top = element_blank(),
		strip.text.y.right = element_text(angle=0),
		axis.text.y = element_blank(),
		axis.ticks.y = element_blank(),
		# axis.title.y = element_text(angle=0,vjust=0.5),
		axis.title.y = element_text(),
		legend.position='none'
	) +
	ylab('Sociality index') +
	xlab('DOL')

pdf(file='figures/sociality_example.pdf',useDingbats=FALSE)
egg::ggarrange(p.mrw,p.dol,nrow=2,heights=c(1,1.5),newpage=FALSE)
dev.off()

socialist.perboon.split = split(socialist.perboon,list(socialist.perboon$Var1,socialist.perboon$year))

friend.counts = do.call(rbind,lapply(socialist.perboon.split,function(x) {
	if (nrow(x)) {
		with(x,data.frame(
			id = unique(Var1),
			sex = unique(sex1),
			year = unique(year),
			n_friends = c(sum(tukey.friendship & sex2 == 'F'),sum(tukey.friendship & sex2 == 'M')),
			friend_sex = c('F','M')
		))
	} else {
		data.frame()
	}
}))
friend.counts$category = factor(
	with(friend.counts,paste0(friend_sex,' friends (',sex,')')),
	levels = c('F friends (F)','M friends (F)','F friends (M)','M friends (M)')
)

friend.counts$n_friends_bins = factor(with(friend.counts,ifelse(
	n_friends > 1,
	'multiple friends',
	ifelse(
		n_friends == 1,
		'one friend',
		'no friends'
	)
)),levels=c('no friends','one friend','multiple friends'))


# Test
between.sex.n.friends = subset(friend.counts,category %in% c('F friends (M)','M friends (F)'))

with(subset(friend.counts,sex != friend_sex),wilcox.test(n_friends[sex=='F'],n_friends[sex=='M']))
with(subset(friend.counts,sex == 'F'),wilcox.test(n_friends[friend_sex=='M'],n_friends[friend_sex=='F']))
with(subset(friend.counts,friend_sex == 'F'),wilcox.test(n_friends[sex == 'F'],n_friends[sex=='M']))




p = ggplot(friend.counts,aes(n_friends,fill=sex)) +
	geom_histogram() +
	facet_grid(year~category) +
	scale_x_continuous(breaks=with(friend.counts,seq(0,max(n_friends)))) +
	scale_y_continuous(breaks=seq(0,10,10)) +
	scale_fill_manual(values=c('M'='#ff7f00','F'='#33a02c')) +
	theme_classic(base_size=16) +
	theme(
		legend.position='none',
		strip.background=element_blank(),
		strip.text.y.right = element_text(angle=0)
	) +
	xlab('Number of friends') +
	ylab('Count')

friend.counts.summary = do.call(rbind,lapply(split(friend.counts,list(friend.counts$year,friend.counts$category)),function(x) { with(x,data.frame(sex=unique(sex),year=unique(year),friend_sex=unique(friend_sex),category=unique(category),n_friends_bins=names(table(n_friends_bins)),count=as.integer(table(n_friends_bins)),frac=as.numeric(table(n_friends_bins)/sum(table(n_friends_bins)))))}))

friend.counts.summary = data.frame(
	friend.counts.summary,
	do.call(rbind,lapply(split(friend.counts.summary,1:nrow(friend.counts.summary)),function(x) {
		if (x$count) {
			res = with(x,binom.test(count,round(count/frac),p=0.5))
			data.frame(
				lb=res$conf.int[1],ub=res$conf.int[2],p=res$p.value
			)
		} else {
			data.frame(
				lb = 0,ub = 0,p=1
			)
		}
	}))
)

rownames(friend.counts.summary) = NULL

# p = ggplot(friend.counts.summary,aes(x=1,y=frac,fill=n_friends_bins)) +
# 	geom_bar(stat='identity',width=1,position='stack') +
# 	facet_grid(category~year) +
# 	# scale_x_continuous(breaks=with(friend.counts,seq(0,max(n_friends)))) +
# 	# scale_y_continuous(breaks=seq(0,10,10)) +
# 	scale_fill_manual(values=c('no friends' = '#fefefe','one friend' = '#deebf7','multiple friends'='#3182bd')) +
# 	coord_polar(theta='y',start=pi) +
# 	theme_void(base_size=16) +
# 	theme(
# 		legend.position='none',
# 		strip.background=element_blank(),
# 		strip.text.y.right = element_text(angle=0)
# 	) +
# 	xlab('Number of friends') +
# 	ylab('Count')

# p = ggplot(within(droplevels(subset(friend.counts.summary,n_friends_bins %in% c('multiple friends','one friend') & !category %in% 'M friends (M)')),{n_friends_bins=factor(n_friends_bins,levels=c('one friend','multiple friends')); category = factor(category,levels=c('F friends (F)','F friends (M)','M friends (F)'),labels=c('female friends of females','female friends of males','male friends of females'))})) +
# 	geom_point(aes(x=year,y=frac,color=n_friends_bins)) +
# 	geom_line(aes(x=year,y=frac,color=n_friends_bins)) +
# 	geom_ribbon(aes(x=year,ymin=lb,ymax=ub,fill=n_friends_bins),alpha=0.1,color=NA) +
# 	scale_x_continuous(breaks=with(friend.counts.summary,seq(min(year),max(year)))) +
# 	scale_y_continuous(breaks=seq(0,1,0.5)) +
# 	facet_wrap(~category,nrow=1) +
# 	# scale_x_continuous(breaks=with(friend.counts,seq(0,max(n_friends)))) +
# 	# scale_y_continuous(breaks=seq(0,10,10)) +
# 	# scale_color_manual(values=c('no friends' = '#deebf7','one friend' = '#9ecae1','multiple friends'='#3182bd')) +
# 	scale_color_manual(values=c('no friends' = '#deebf7','one friend' = '#666666','multiple friends'='#e41a1c')) +
# 	scale_fill_manual(values=c('no friends' = '#deebf7','one friend' = '#666666','multiple friends'='#e41a1c')) +
# 	theme_classic(base_size=16) +
# 	theme(
# 		legend.position='bottom',
#  		strip.background=element_blank(),
#  		strip.text = element_text(hjust=0),
#  		axis.text.x = element_text(hjust=0,angle=-45,vjust=0.5),
#  		axis.title.x = element_blank(),
#  		legend.title=element_blank(),
#  		plot.margin=unit(c(5.5, 20, 5.5, 5.5), "points")
# 	) +
# 	ylab('Fraction of individuals')

friend.counts.summary$sex_label = factor(friend.counts.summary$sex,levels=c('F','M'),labels=c('females','males'))

p = ggplot(within(droplevels(subset(friend.counts.summary,n_friends_bins %in% c('multiple friends','one friend') & !category %in% c('F friends (F)','M friends (M)'))),{n_friends_bins=factor(n_friends_bins,levels=c('multiple friends','one friend','no friends'),labels=c('multiple\nfriends','one\nfriend','no \nfriends')); category = factor(category,levels=c('F friends (F)','F friends (M)','M friends (F)'),labels=c('female friends of females','female friends of males','male friends of females'))})) +
	geom_bar(aes(x=year,y=frac,fill=n_friends_bins),stat='identity',alpha=0.5,width=0.5) +
	geom_point(aes(x=year,y=frac,color=n_friends_bins)) +
	# geom_line(aes(x=year,y=frac,color=n_friends_bins)) +
	# geom_ribbon(aes(x=year,ymin=lb,ymax=ub,fill=n_friends_bins),alpha=0.1,color=NA) +
	geom_linerange(aes(x=year,ymin=lb,ymax=ub),alpha=0.5,linewidth=0.25,color='#000000') +
	scale_x_continuous(breaks=with(friend.counts.summary,seq(min(year),max(year)))) +
	scale_y_continuous(breaks=seq(0,1,0.5)) +
	facet_grid(n_friends_bins~sex_label) +
	# scale_x_continuous(breaks=with(friend.counts,seq(0,max(n_friends)))) +
	# scale_y_continuous(breaks=seq(0,10,10)) +
	# scale_color_manual(values=c('no friends' = '#deebf7','one friend' = '#9ecae1','multiple friends'='#3182bd')) +
	scale_color_manual(values=c('no\nfriends' = '#00A087FF','one\nfriend' = '#4DBBD5FF','multiple\nfriends'='#E64B35FF')) +
	scale_fill_manual(values=c('no\nfriends' = '#00A087FF','one\nfriend' = '#4DBBD5FF','multiple\nfriends'='#E64B35FF')) +
	theme_classic(base_size=16) +
	theme(
		legend.position='none',
 		strip.background=element_blank(),
 		strip.text.y.right = element_text(angle=0,hjust=0),
 		panel.grid.major.y = element_line(color='#000000',linewidth=0.1,linetype=2),
 		axis.text.x = element_text(hjust=0,angle=-45,vjust=0.5),
 		axis.title.x = element_blank(),
 		legend.title=element_blank(),
 		plot.margin=unit(c(5.5, 20, 5.5, 5.5), "points")
	) +
	ylab('Fraction of individuals')
p.n.friends = p

# p = ggplot(droplevels(subset(within(droplevels(subset(friend.counts.summary,n_friends_bins %in% c('multiple friends','one friend') & !category %in% 'M friends (M)')),{n_friends_bins=factor(n_friends_bins,levels=c('one friend','multiple friends')); category = factor(category,levels=c('F friends (F)','F friends (M)','M friends (F)'),labels=c('female friends of females','female friends of males','male friends of females'))}),n_friends_bins == 'multiple friends'))) +
# 	geom_point(aes(x=year,y=frac,color=n_friends_bins)) +
# 	geom_line(aes(x=year,y=frac,color=n_friends_bins)) +
# 	geom_ribbon(aes(x=year,ymin=lb,ymax=ub,fill=n_friends_bins),alpha=0.1,color=NA) +
# 	scale_x_continuous(breaks=with(friend.counts.summary,seq(min(year),max(year)))) +
# 	scale_y_continuous(breaks=seq(0,1,0.5)) +
# 	facet_wrap(~category,nrow=1) +
# 	# scale_x_continuous(breaks=with(friend.counts,seq(0,max(n_friends)))) +
# 	# scale_y_continuous(breaks=seq(0,10,10)) +
# 	# scale_color_manual(values=c('no friends' = '#deebf7','one friend' = '#9ecae1','multiple friends'='#3182bd')) +
# 	scale_color_manual(values=c('no friends' = '#deebf7','one friend' = '#666666','multiple friends'='#e41a1c')) +
# 	scale_fill_manual(values=c('no friends' = '#deebf7','one friend' = '#666666','multiple friends'='#e41a1c')) +
# 	theme_classic(base_size=16) +
# 	theme(
# 		legend.position='bottom',
#  		strip.background=element_blank(),
#  		strip.text = element_text(hjust=0),
#  		axis.text.x = element_text(hjust=0,angle=-45,vjust=0.5),
#  		axis.title.x = element_blank(),
#  		legend.title=element_blank(),
#  		plot.margin=unit(c(5.5, 20, 5.5, 5.5), "points")
# 	) +
# 	ylab('Fraction of individuals')
# 
# p = ggplot(droplevels(subset(within(droplevels(subset(friend.counts.summary,n_friends_bins %in% c('multiple friends','one friend') & !category %in% 'M friends (M)')),{n_friends_bins=factor(n_friends_bins,levels=c('one friend','multiple friends')); category = factor(category,levels=c('F friends (F)','F friends (M)','M friends (F)'),labels=c('female friends of females','female friends of males','male friends of females'))}),n_friends_bins == 'one friend'))) +
# 	geom_point(aes(x=year,y=frac,color=n_friends_bins)) +
# 	geom_line(aes(x=year,y=frac,color=n_friends_bins)) +
# 	geom_ribbon(aes(x=year,ymin=lb,ymax=ub,fill=n_friends_bins),alpha=0.1,color=NA) +
# 	scale_x_continuous(breaks=with(friend.counts.summary,seq(min(year),max(year)))) +
# 	scale_y_continuous(breaks=seq(0,1,0.5)) +
# 	facet_wrap(~category,nrow=1) +
# 	# scale_x_continuous(breaks=with(friend.counts,seq(0,max(n_friends)))) +
# 	# scale_y_continuous(breaks=seq(0,10,10)) +
# 	# scale_color_manual(values=c('no friends' = '#deebf7','one friend' = '#9ecae1','multiple friends'='#3182bd')) +
# 	scale_color_manual(values=c('no friends' = '#deebf7','one friend' = '#666666','multiple friends'='#e41a1c')) +
# 	scale_fill_manual(values=c('no friends' = '#deebf7','one friend' = '#666666','multiple friends'='#e41a1c')) +
# 	theme_classic(base_size=16) +
# 	theme(
# 		legend.position='bottom',
#  		strip.background=element_blank(),
#  		strip.text = element_text(hjust=0),
#  		axis.text.x = element_text(hjust=0,angle=-45,vjust=0.5),
#  		axis.title.x = element_blank(),
#  		legend.title=element_blank(),
#  		plot.margin=unit(c(5.5, 20, 5.5, 5.5), "points")
# 	) +
# 	ylab('Fraction of individuals')





# Friendship stability across years
socialist.undirected$dyadyearkey = socialist.undirected$dyadkey
socialist.undirected$dyadkey = substr(socialist.undirected$dyadkey,1,7)

friend.dyads = unique(socialist.undirected$dyadkey[socialist.undirected$tukey.friendship & socialist.undirected$type2 == 'FM'])

fm.socialist.undirected = subset(socialist.undirected,type2 == 'FM')

fm.socialist.undirected = do.call(rbind,lapply(split(fm.socialist.undirected,fm.socialist.undirected$year),function(x) {
	x$quantile = ecdf(x$socialist)(x$socialist)
	x$quantile[x$socialist == 0] = 0
	x
}))

fm.socialist.undirected.friends = subset(fm.socialist.undirected,dyadkey %in% friend.dyads)

fm.socialist.undirected.friends = fm.socialist.undirected.friends[with(fm.socialist.undirected.friends,order(year,Var4,Var3)),]

fm.socialist.undirected.friends$dyadkey = factor(fm.socialist.undirected.friends$dyadkey,levels=unique(fm.socialist.undirected.friends$dyadkey))

fm.socialist.undirected.friends$sex3 = sex.key[fm.socialist.undirected.friends$Var3]
fm.socialist.undirected.friends$sex4 = sex.key[fm.socialist.undirected.friends$Var4]

fm.socialist.undirected.friends$dyadlabel = with(fm.socialist.undirected.friends,ifelse(sex3 == 'F',paste0(Var3,' & ',Var4),paste0(Var4,' & ',Var3)))

fm.socialist.undirected.friends$f = unlist(lapply(strsplit(fm.socialist.undirected.friends$dyadlabel,' & '),function(x) x[1]))
fm.socialist.undirected.friends$m = unlist(lapply(strsplit(fm.socialist.undirected.friends$dyadlabel,' & '),function(x) x[2]))

# fm.socialist.undirected.friends = fm.socialist.undirected.friends[with(fm.socialist.undirected.friends,order(year,m,f)),]
# fm.socialist.undirected.friends$dyadlabel = factor(fm.socialist.undirected.friends$dyadlabel,levels=unique(fm.socialist.undirected.friends$dyadlabel))


library(viridis)

fm.socialist.undirected.friends.split = split(fm.socialist.undirected.friends,fm.socialist.undirected.friends$dyadkey)

fm.socialist.undirected.friends.links = do.call(rbind,lapply(fm.socialist.undirected.friends.split,function(x) {
	y = data.frame(dyadkey=unique(x$dyadkey),year=min(x$year):max(x$year))
	y = merge(y,subset(x,select=c('dyadlabel','year','tukey.friendship','quartile.friendship','median.friendship','quantile')),all.x=TRUE)
	y$tukey.friendship[is.na(y$tukey.friendship)] = FALSE
	y$quartile.friendship[is.na(y$quartile.friendship)] = FALSE
	y$median.friendship[is.na(y$median.friendship)] = FALSE
	y$consecutive = y$consecutive2 = y$consecutive3 = NA
	if(nrow(y) > 1) {
		y$consecutive[2:nrow(y)] = y$tukey.friendship[2:nrow(y)] & diff(y$tukey.friendship) == 0
		y$consecutive2[2:nrow(y)] = y$quartile.friendship[2:nrow(y)] & diff(y$quartile.friendship) == 0
		y$consecutive3[2:nrow(y)] = y$median.friendship[2:nrow(y)] & diff(y$median.friendship) == 0
	} else {
		y$consecutive = NA
	}
	y$yearprior = y$year - 1
	y
}))

fm.socialist.undirected.friends.order = do.call(rbind,lapply(split(fm.socialist.undirected.friends,fm.socialist.undirected.friends$dyadlabel),function(x) {
	data.frame(unique(x[,c('dyadlabel','f','m')]),yearstart = min(x$year),yearend=max(x$year))
}))

fm.socialist.undirected.friends.order2 = do.call(rbind,lapply(split(fm.socialist.undirected.friends.order,fm.socialist.undirected.friends.order$m),function(x) {
	x$malestart = min(x$yearstart)
	x
}))

fm.socialist.undirected.friends.order2 = fm.socialist.undirected.friends.order2[with(fm.socialist.undirected.friends.order2,order(malestart,m,yearstart,yearend,f)),]

fm.socialist.undirected.friends.links$dyadlabel = factor(fm.socialist.undirected.friends.links$dyadlabel,levels=rev(fm.socialist.undirected.friends.order2$dyadlabel))
fm.socialist.undirected.friends$dyadlabel = factor(fm.socialist.undirected.friends$dyadlabel,levels=rev(fm.socialist.undirected.friends.order2$dyadlabel))

fm.socialist.undirected.friends$friendshiplabel = factor(fm.socialist.undirected.friends$tukey.friendship,levels=c('TRUE','FALSE'),labels=c('friends (> threshold)','not friends (< threshold)'))

p = ggplot() +
	geom_blank(data=fm.socialist.undirected.friends,aes(year,dyadlabel)) +
	# geom_segment(data=subset(fm.socialist.undirected.friends.links,!is.na(consecutive) & !consecutive & !consecutive3),aes(x=yearprior,y=dyadlabel,xend=year,yend=dyadlabel),linetype=2,linewidth=0.25,alpha=0.5) +
	# geom_segment(data=subset(fm.socialist.undirected.friends.links,!is.na(consecutive) & !consecutive & consecutive3),aes(x=yearprior,y=dyadlabel,xend=year,yend=dyadlabel),linetype=1,linewidth=0.25) +
	geom_segment(data=subset(fm.socialist.undirected.friends.links,!is.na(consecutive) & !consecutive & consecutive3),aes(x=yearprior,y=dyadlabel,xend=year,yend=dyadlabel),linetype=2,linewidth=0.25) +
	geom_segment(data=subset(fm.socialist.undirected.friends.links,!is.na(consecutive) & consecutive),aes(x=yearprior,y=dyadlabel,xend=year,yend=dyadlabel),linetype=1,linewidth=0.75) +
	geom_point(data=fm.socialist.undirected.friends,aes(year,dyadlabel,shape=friendshiplabel,size=friendshiplabel,color=quantile)) +
	scale_x_continuous(breaks=do.call(seq,as.list(range(fm.socialist.undirected.friends$year)))) +
	scale_shape_manual(values=c('friends (> threshold)'=19,'not friends (< threshold)'=21)) +
	scale_size_manual(values=c('friends (> threshold)'=2.5,'not friends (< threshold)'=1.5)) +
	scale_color_viridis(name='Quantile',breaks=seq(0,1,0.5)) +
	theme_classic(base_size=16) +
	theme(
		axis.title.y = element_blank(),
		axis.title.x = element_blank(),
		axis.text.y = element_text(family='Courier',size=8),
		legend.position='bottom',
		plot.margin = margin(t=0,r=10,b=0,l=30)
	) +
	guides(size=guide_legend(title=NULL,nrow=2),shape=guide_legend(title=NULL,nrow=2)) +
	xlab('Year')
p.multiyear.links = p

fm.socialist.undirected.split = split(fm.socialist.undirected,fm.socialist.undirected$dyadkey)

fm.links = do.call(rbind,lapply(fm.socialist.undirected.split,function(x) {
	y = data.frame(dyadkey=unique(x$dyadkey),year=min(x$year):max(x$year))
	y = merge(y,subset(x,select=c('dyadkey','year','tukey.friendship','quantile')),all.x=TRUE)
	y$tukey.friendship[is.na(y$tukey.friendship)] = FALSE
	y$friendship_prior = if (nrow(y) == 1) NA else c(NA,y$tukey.friendship[1:(nrow(y)-1)])
	y$consecutive = NA
	if(nrow(y) > 1) {
		y$consecutive[2:nrow(y)] = y$tukey.friendship[2:nrow(y)] & diff(y$tukey.friendship) == 0
	} else {
		y$consecutive = NA
	}
	y$yearprior = y$year - 1
	y
}))

fm.links.coresidency = subset(fm.links,!is.na(consecutive))

# year1.friendship = as.integer(table(fm.links.coresidency$friendship_prior))
# year2.friendship = as.integer(table(fm.links.coresidency$tukey.friendship))
# consecutive.rate = as.integer(table(fm.links.coresidency$consecutive))
# 
# consecutive.friendship.matrix = matrix(c(
# 	nrow(fm.links.coresidency) - (year1.friendship[2] - consecutive.rate[2]) - (year2.friendship[2] - consecutive.rate[2]) - consecutive.rate[2],
# 	year1.friendship[2] - consecutive.rate[2],
# 	year2.friendship[2] - consecutive.rate[2],
# 	consecutive.rate[2]
# ),nrow=2,
# dimnames=list(c('year1_not_friend','year1_friend'),c('year2_not_friend','year2_friend')))

consecutive.friendship.matrix = as.matrix(table(fm.links.coresidency$friendship_prior,fm.links.coresidency$tukey.friendship))
fisher.test(consecutive.friendship.matrix)

p.fig3 = ggpubr::ggarrange(p.n.friends,p.multiyear.links,nrow=2,heights=c(1.5,2.5),labels='AUTO')

pdf(file='figures/fig3.pdf',useDingbats=FALSE,height=10)
p.fig3
dev.off()



# STOP HERE
aw = subset(alldata,behavior %in% 'AOSOC' & mod1 %in% c('Appr','Wthd') & actor.as %in% c('AM','AF') & recipient.as %in% c('AM','AF') & actor.as != recipient.as)

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

appr.wthd$bout_id = 1:nrow(appr.wthd)

dir.create('checkpoints',showWarnings=FALSE)
saveRDS(appr.wthd,file='checkpoints/focal_approach_withdraws.rds')

# Clean 
# Hinde's index
(table(appr.wthd$appr_actor.as) / nrow(appr.wthd) * 100)['AF'] - (table(appr.wthd$wthd_actor.as) / nrow(appr.wthd) * 100)['AF']

fisher.test(with(appr.wthd,table(appr_actor.as,wthd_actor.as)))

# Now find groomings

timed.behaviors = c('OV','Sexual','Social','Solitary','{end}')

appr.wthd.grooms = do.call(rbind,lapply(split(appr.wthd,appr.wthd$bout_id),function(x) {
	z = subset(alldata,meta_id %in% x$meta_id & timestamp > x$appr_timestamp & timestamp < x$wthd_timestamp & behavior %in% timed.behaviors)
	z$duration = diff(c(z$timestamp,x$wthd_timestamp))
	y = subset(z,partner == x$partner)
	if (nrow(y) & any(y$behavior %in% 'Social' & y$mod1 %in% 'Grm')) {
		data.frame(
			bout_id = x$bout_id,
			n_grooms = with(y,sum(behavior %in% 'Social' & mod1 %in% 'Grm')),
			n_grooms_f = with(y,sum(behavior %in% 'Social' & mod1 %in% 'Grm' & actor.as == 'AF')),
			n_grooms_m = with(y,sum(behavior %in% 'Social' & mod1 %in% 'Grm' & actor.as == 'AM')),
			t_grooms = with(y,sum(duration[behavior %in% 'Social' & mod1 %in% 'Grm'])),
			t_grooms_f = with(y,sum(duration[behavior %in% 'Social' & mod1 %in% 'Grm' & actor.as == 'AF'])),
			t_grooms_m = with(y,sum(duration[behavior %in% 'Social' & mod1 %in% 'Grm' & actor.as == 'AM'])),
			first_groom_timestamp = subset(y,behavior %in% 'Social' & mod1 %in% 'Grm')$timestamp[1],
			first_groom_as = subset(y,behavior %in% 'Social' & mod1 %in% 'Grm')$actor.as[1]
		)
	} else {
		data.frame()
	}
}))

appr.wthd.merged = merge(appr.wthd,appr.wthd.grooms,by='bout_id',all.x=TRUE)

appr.wthd.merged$appr_groom_lag = with(appr.wthd.merged,first_groom_timestamp - appr_timestamp)

# Chances that approacher is also the groomer
fisher.test(with(appr.wthd.merged,table(appr_actor.as,first_groom_as)))

# Quantiles of lag time between approach and groom
quantile(appr.wthd.merged$appr_groom_lag,na.rm=T,seq(0,1,0.1))

# Now proxfocs
proxdata$abort = FALSE # grepl('ABORT',proxdata$filename,ignore.case=TRUE)

proxdata$focal.as = agesex.key[with(proxdata,paste0(focalAnimal,lubridate::year(dateC)))]
proxdata$partner.as = agesex.key[with(proxdata,paste0(partner,lubridate::year(dateC)))]
proxdata$actor.as = agesex.key[with(proxdata,paste0(actor,lubridate::year(dateC)))]
proxdata$recipient.as = agesex.key[with(proxdata,paste0(recipient,lubridate::year(dateC)))]

proxclean = subset(proxdata,!abort & partner.as %in% c('AF','AM') & focal.as %in% c('AF','AM'))

proxclean$dyadkey = with(proxclean,paste0(focalAnimal,'_',partner))

prox.appr.wthd = do.call(rbind,lapply(split(proxclean,proxclean$meta_id),function(x) {
	do.call(rbind,lapply(split(x,x$dyadkey),function(y) {
		out = data.frame()
		ticker = 0
		for (i in 1:nrow(y)) {
			z = y[i,]
			if ((ticker == 0) && (z$mod1 %in% 'Appr')) {
				this = z[c('meta_id','dyadkey','focalAnimal','partner','actor','actor.as','timestamp')]
				names(this)[names(this) %in% c('actor','actor.as','timestamp')] = paste0('appr_',names(this)[names(this) %in% c('actor','actor.as','timestamp')])
				ticker = 1
			} else if ((ticker == 1) && (z$mod1 %in% 'Wthd')) {
				that = z[c('actor','actor.as','timestamp')]
				names(that) = paste0('wthd_',names(that))
				out = rbind(out,data.frame(this,that,duration = that$wthd_timestamp - this$appr_timestamp))
				ticker = 0
			}
		}
		out
	}))
}))
rownames(prox.appr.wthd) = NULL

prox.appr.wthd.backup = prox.appr.wthd
prox.appr.wthd = subset(prox.appr.wthd,duration >= 20)

prox.appr.wthd$bout_id = 1:nrow(prox.appr.wthd)

saveRDS(prox.appr.wthd,file='checkpoints/proxfoc_approach_withdraws.rds')

# Clean 

#Hinde's index
(table(prox.appr.wthd$appr_actor.as) / nrow(prox.appr.wthd) * 100)['AF'] - (table(prox.appr.wthd$wthd_actor.as) / nrow(prox.appr.wthd) * 100)['AF']

fisher.test(with(prox.appr.wthd,table(appr_actor.as,wthd_actor.as)))

# Now find groomings
prox.appr.wthd.grooms = do.call(rbind,lapply(split(prox.appr.wthd,prox.appr.wthd$bout_id),function(x) {
	z = subset(proxdata,meta_id %in% x$meta_id & timestamp > x$appr_timestamp & timestamp < x$wthd_timestamp & behavior %in% timed.behaviors)
	z$duration = diff(c(z$timestamp,x$wthd_timestamp))
	y = subset(z,meta_id %in% x$meta_id & timestamp > x$appr_timestamp & timestamp < x$wthd_timestamp & partner == x$partner)
	if (nrow(y) & any(y$behavior %in% 'Social' & y$mod1 %in% 'Grm')) {
		data.frame(
			bout_id = x$bout_id,
			n_grooms = with(y,sum(behavior %in% 'Social' & mod1 %in% 'Grm')),
			n_grooms_f = with(y,sum(behavior %in% 'Social' & mod1 %in% 'Grm' & actor.as == 'AF')),
			n_grooms_m = with(y,sum(behavior %in% 'Social' & mod1 %in% 'Grm' & actor.as == 'AM')),
			t_grooms = with(y,sum(duration[behavior %in% 'Social' & mod1 %in% 'Grm'])),
			t_grooms_f = with(y,sum(duration[behavior %in% 'Social' & mod1 %in% 'Grm' & actor.as == 'AF'])),
			t_grooms_m = with(y,sum(duration[behavior %in% 'Social' & mod1 %in% 'Grm' & actor.as == 'AM'])),
			first_groom_timestamp = subset(y,behavior %in% 'Social' & mod1 %in% 'Grm')$timestamp[1],
			first_groom_as = subset(y,behavior %in% 'Social' & mod1 %in% 'Grm')$actor.as[1]
		)
	} else {
		data.frame()
	}
}))

# appr.wthd.grooms = do.call(rbind,lapply(split(appr.wthd,appr.wthd$bout_id),function(x) {
# 	z = subset(alldata,meta_id %in% x$meta_id & timestamp > x$appr_timestamp & timestamp < x$wthd_timestamp & behavior %in% timed.behaviors)
# 	z$duration = diff(c(z$timestamp,x$wthd_timestamp))
# 	y = subset(z,partner == x$partner)
# 	if (nrow(y) & any(y$behavior %in% 'Social' & y$mod1 %in% 'Grm')) {
# 		data.frame(
# 			bout_id = x$bout_id,
# 			n_grooms = with(y,sum(behavior %in% 'Social' & mod1 %in% 'Grm')),
# 			n_grooms_f = with(y,sum(behavior %in% 'Social' & mod1 %in% 'Grm' & actor.as == 'AF')),
# 			n_grooms_m = with(y,sum(behavior %in% 'Social' & mod1 %in% 'Grm' & actor.as == 'AM')),
# 			t_grooms = with(y,sum(duration[behavior %in% 'Social' & mod1 %in% 'Grm'])),
# 			t_grooms_f = with(y,sum(duration[behavior %in% 'Social' & mod1 %in% 'Grm' & actor.as == 'AF'])),
# 			t_grooms_m = with(y,sum(duration[behavior %in% 'Social' & mod1 %in% 'Grm' & actor.as == 'AM'])),
# 			first_groom_timestamp = subset(y,behavior %in% 'Social' & mod1 %in% 'Grm')$timestamp[1],
# 			first_groom_as = subset(y,behavior %in% 'Social' & mod1 %in% 'Grm')$actor.as[1]
# 		)
# 	} else {
# 		data.frame()
# 	}
# }))

prox.appr.wthd.merged = merge(prox.appr.wthd,prox.appr.wthd.grooms,by='bout_id',all.x=TRUE)

prox.appr.wthd.merged$appr_groom_lag = with(prox.appr.wthd.merged,first_groom_timestamp - appr_timestamp)

fisher.test(with(prox.appr.wthd.merged ,table(appr_actor.as,first_groom_as)))


prox.appr.wthd.merged = within(prox.appr.wthd.merged,{
	n_grooms[is.na(n_grooms)] = 0
	n_grooms_f[is.na(n_grooms_f)] = 0
	n_grooms_m[is.na(n_grooms_m)] = 0
	t_grooms[is.na(t_grooms)] = 0
	t_grooms_f[is.na(t_grooms_f)] = 0
	t_grooms_m[is.na(t_grooms_m)] = 0
})



with(subset(prox.appr.wthd.merged,appr_actor.as == 'AM'),wilcox.test(t_grooms_m,t_grooms_f,alternative='greater'))
with(subset(prox.appr.wthd.merged,appr_actor.as == 'AM'),t.test(t_grooms_m,t_grooms_f,alternative='greater'))

with(subset(prox.appr.wthd.merged,appr_actor.as == 'AF'),wilcox.test(t_grooms_m,t_grooms_f,alternative='less'))
with(subset(prox.appr.wthd.merged,appr_actor.as == 'AF'),t.test(t_grooms_m,t_grooms_f,alternative='less'))

with(subset(appr.wthd.merged,appr_actor.as == 'AM'),wilcox.test(t_grooms_m,t_grooms_f,alternative='greater'))
with(subset(appr.wthd.merged,appr_actor.as == 'AM'),t.test(t_grooms_m,t_grooms_f,alternative='greater'))

with(subset(appr.wthd.merged,appr_actor.as == 'AF'),wilcox.test(t_grooms_m,t_grooms_f,alternative='less'))
with(subset(appr.wthd.merged,appr_actor.as == 'AF'),t.test(t_grooms_m,t_grooms_f,alternative='less'))


# Combine with the friendships
appr.wthd.combined = rbind(prox.appr.wthd.merged,appr.wthd.merged)

meta.years = rbind(
	unique(subset(alldata,select=c('meta_id','dateC'))),
	unique(subset(proxdata,select=c('meta_id','dateC')))
)
appr.wthd.combined = merge(appr.wthd.combined,meta.years,by='meta_id',all.x=TRUE)


appr.wthd.combined$focal.as = with(appr.wthd.combined,agesex.key[paste0(focalAnimal,lubridate::year(dateC))])

appr.wthd.combined$mfdyadkey = with(appr.wthd.combined,ifelse(focal.as == 'AF',paste0(focalAnimal,'_',partner),paste0(partner,'_',focalAnimal)))


appr.wthd.combined$friend_dyad = with(appr.wthd.combined,mfdyadkey%in% gsub('_$|^_','',friend.dyads))



friend.appr.wthd.stats = do.call(rbind,lapply(split(appr.wthd.combined,appr.wthd.combined$friend_dyad),function(x) {
	do.call(rbind,lapply(split(x,x$mfdyadkey),function(y) {
		with(y,data.frame(
			dyadkey = unique(mfdyadkey),
			friend_dyad = unique(friend_dyad),
			n = nrow(y),
			min_duration = min(duration),
			med_duration = median(duration),
			max_duration = max(duration),
			appr_frac_m = mean(appr_actor.as == 'AM'),
			wthd_frac_m = mean(wthd_actor.as == 'AM'),
			hinde=mean(appr_actor.as=='AF')*100 - mean(wthd_actor.as=='AF')*100			
		))
	}))
}))


friend.appr.wthd.stats.wide = subset(friend.appr.wthd.stats,n>=10 & friend_dyad)

friend.appr.wthd.stats.long = tidyr::pivot_longer(friend.appr.wthd.stats.wide,cols=c('min_duration','med_duration','max_duration','appr_frac_m','wthd_frac_m','hinde'))

p1 = ggplot() +
	geom_histogram(data=subset(friend.appr.wthd.stats.long,name == 'hinde'),aes(value),bins=20) +
	geom_vline(xintercept = 0,linetype=1,linewidth=0.5,color='#ff0000') +
	scale_x_continuous(breaks=seq(-100,100,50),limits=c(-100,100)) +
	scale_y_continuous(breaks=seq(0,5,5),limits=c(0,5)) +
	theme_classic(base_size=16) +
	theme(
		strip.background=element_blank()
	) +
	xlab('Hinde\'s index') +
	ylab('Count')
p2 = ggplot() +
	geom_histogram(data=subset(friend.appr.wthd.stats.long,name == 'appr_frac_m'),aes(value),bins=20) +
	geom_vline(xintercept = 0.5,linetype=1,linewidth=0.5,color='#ff0000') +
	scale_x_continuous(breaks=seq(0,1,0.5),limits=c(0,1)) +
	scale_y_continuous(breaks=seq(0,5,5),limits=c(0,5)) +
	theme_classic(base_size=16) +
	theme(
		strip.background=element_blank()
	) +
	xlab('Fraction approaches by male') +
	ylab('Count')
p3 = ggplot() +
	geom_histogram(data=subset(friend.appr.wthd.stats.long,name == 'wthd_frac_m'),aes(value),bins=20) +
	geom_vline(xintercept = 0.5,linetype=1,linewidth=0.5,color='#ff0000') +
	scale_x_continuous(breaks=seq(0,1,0.5),limits=c(0,1)) +
	scale_y_continuous(breaks=seq(0,5,5),limits=c(0,5)) +
	theme_classic(base_size=16) +
	theme(
		strip.background=element_blank()
	) +
	xlab('Fraction withdraws by male') +
	ylab('Count')

egg::ggarrange(p1,p2,p3)




# Focals and proxfocs together









all.appr.wthd.merged = rbind(
	appr.wthd.merged,
	prox.appr.wthd.merged
)

hinde.resample = unlist(parallel::mclapply(1:500,function(i) {
	x = all.appr.wthd.merged[sample(1:nrow(all.appr.wthd.merged),replace=TRUE),]
	(table(x$appr_actor.as) / nrow(all.appr.wthd.merged) * 100)['AF'] - (table(all.appr.wthd.merged$wthd_actor.as) / nrow(all.appr.wthd.merged) * 100)['AF']
}.mc.cores=4))

saveRDS(hinde.resample,file='checkpoints/hinde_resample.rds')
# no point calculating duration









# Reproduction condition

# lactating
# pregancy
# cycling

inf.births = read.delim('data/inf_births.tsv')

inf.births$dateF = lubridate::ymd(with(inf.births,paste(Year_Determined,Month_Determined,Day_Determined,sep='-')))

af.years = data.frame(
	id = gsub('[0-9]{4}$','',names(which(agesex.key == 'AF'))),
	year = gsub('^[A-Z]*','',names(which(agesex.key == 'AF')))
)

inf.births = inf.births[order(inf.births$dateF),]


dods = readRDS('data/dods.fixed.RDS')

inf.deaths = subset(dods,ID %in% inf.births$Inf_ID & !is.na(date) & grepl('death|dead|died',Departure_Reason,ignore.case=TRUE))
inf.deaths$date = as.Date(inf.deaths$date)

gestation.length = 180 # 6 months
lactation.length = 270 # 9 months
inf.death.length = 60 # 6 months

# rep.events = do.call(rbind,lapply(split(inf.births,inf.births$Moth_ID),function(x) {
# 	do.call(rbind,lapply(split(x,1:nrow(x)),function(y) {
# 		this.death = subset(inf.deaths,ID == y$Inf_ID)
# 		data.frame(
# 			id = y$Moth_ID,
# 			event=c('pregnant','lactation','cycling'),
# 			dateE=c(
# 				y$dateF-gestation.length, # gestation
# 				y$dateF, # birth
# 				if (y$Inf_ID %in% 'ISA') {
# 					this.death$date + inf.death.length
# 				} else {
# 					y$dateF + lactation.length
# 				}
# 				
# 				# if (nrow(this.death)) { if ((y$dateF+270) > (this.death$date+60)) this.death$date+60 else y$dateF + 270 } else { y$dateF + 270 } # lactation estimate
# 			)
# 		)
# 	}))
# }))
# 
# foo = do.call(rbind,lapply(split(rep.events,rep.events$id),function(x) data.frame(x,timediff=c(NA,diff(x$dateE)))))
# rownames(foo) = NULL
# 
# moo = inf.births
# moo$mid = with(moo,paste0(Moth_ID,'_',dateF))
# rownames(moo) = moo$mid
# 
# records.to.check = moo[with(foo[c(as.integer(rownames(subset(foo,timediff < 0)))-2,as.integer(rownames(subset(foo,timediff < 0)))+1),],paste0(id,'_',dateE)),]
# 
# records.to.check = records.to.check[order(rownames(records.to.check)),]


rep.events = do.call(rbind,lapply(split(inf.births,inf.births$Moth_ID),function(x) {
	do.call(rbind,lapply(split(x,1:nrow(x)),function(y) {
		this.death = subset(inf.deaths,ID == y$Inf_ID)
		data.frame(
			id = y$Moth_ID,
			event=c('pregnant','lactating'),
			date_start=c(
				y$dateF-gestation.length, # gestation
				y$dateF
			),
			date_end=c(
				y$dateF,
				if (nrow(this.death)) { if ((y$dateF+lactation.length) > (this.death$date+inf.death.length)) this.death$date+inf.death.length else y$dateF + lactation.length } else { y$dateF + lactation.length } # lactation estimate
			)
		)
	}))
}))
rep.events = rep.events[with(rep.events,order(id,date_start,date_end)),]

rep.events = do.call(rbind,lapply(split(rep.events,rep.events$id),function(x) {
	x$time_lag = c(NA,as.integer(x$date_start[2:nrow(x)]-x$date_end[1:(nrow(x)-1)]))
	x
}))
rownames(rep.events) = NULL

rep.events.corrected = rep.events
rep.events.corrected$note = ''

for (i in 1:nrow(rep.events)) {
	if (!is.na(rep.events$time_lag[i]) & rep.events$time_lag[i] < 0) {
		cat(i,'\n')
		date1 = rep.events$date_end[i-1]
		date2 = rep.events$date_start[i]
		rep.events.corrected$date_end[i-1] = mean(c(date1,date2))
		rep.events.corrected$date_start[i] = mean(c(date1,date2))
		rep.events.corrected$note[i] = if (nchar(rep.events.corrected$note[i])) paste0(rep.events.corrected$note[i],'; lag ',rep.events$time_lag[i],' with previous row corrected to 0.') else paste0('lag ',rep.events$time_lag[i],' with previous row corrected to 0.')
		rep.events.corrected$note[i-1] = if (nchar(rep.events.corrected$note[i-1])) paste0(rep.events.corrected$note[i-1],'; lag ',rep.events$time_lag[i],' with following row corrected to 0.') else paste0('lag ',rep.events$time_lag[i],' with following row corrected to 0.')
		rep.events.corrected$time_lag[i] = 0
	}
}

saveRDS(rep.events.corrected,file='checkpoints/reproductive_events.rds')
saveRDS(alldata,file='checkpoints/alldata_checkpoints.rds')
saveRDS(proxclean,file='checkpoints/proxdata_checkpoint.rds')
saveRDS(age.nona,file='checkpoints/age_nona.rds')




# Timed data with alldata




groomdata = alldata.with.durations


groomdata$actor.as = agesex.key[with(groomdata,paste0(actor,lubridate::year(dateC)))]
groomdata$recipient.as = agesex.key[with(groomdata,paste0(recipient,lubridate::year(dateC)))]

groomdata = subset(groomdata,behavior %in% 'Social' & mod1 %in% 'Grm' & !is.na(actor.as) & !is.na(recipient.as) & !is.na(duration))

with(subset(groomdata,actor.as %in% c('AF','AM') & recipient.as %in% c('AF','AM')),table(actor.as,recipient.as))


with(subset(groomdata,actor.as %in% 'AM' & recipient.as %in% 'AF'),sum(duration,na.rm=T))






with(subset(groomdata,actor.as %in% 'AM' & recipient.as %in% 'AF'),sum(duration)/60)
with(subset(groomdata,actor.as %in% 'AF' & recipient.as %in% 'AM'),sum(duration)/60)
with(subset(groomdata,actor.as %in% 'AM' & recipient.as %in% 'AM'),sum(duration)/60)
with(subset(groomdata,actor.as %in% 'AF' & recipient.as %in% 'AF'),sum(duration)/60)
with(subset(groomdata,actor.as %in% c('AF','AM') & recipient.as %in% c('AF','AM')),sum(duration)/60)
with(subset(groomdata,actor.as %in% c('AM')),sum(duration)/60)
with(subset(groomdata,actor.as %in% c('AM') & !recipient.as %in% c('AF','AM')),sum(duration)/60)
with(subset(groomdata,actor.as %in% c('AF')),sum(duration)/60)
with(subset(groomdata,actor.as %in% c('AF') & !recipient.as %in% c('AF','AM')),sum(duration)/60)

cat(c(
with(subset(groomdata,actor.as %in% 'AM' & recipient.as %in% 'AF'),mean(duration)),
with(subset(groomdata,actor.as %in% 'AF' & recipient.as %in% 'AM'),mean(duration)),
with(subset(groomdata,actor.as %in% 'AM' & recipient.as %in% 'AM'),mean(duration)),
with(subset(groomdata,actor.as %in% 'AF' & recipient.as %in% 'AF'),mean(duration)),
with(subset(groomdata,actor.as %in% c('AF','AM') & recipient.as %in% c('AF','AM')),mean(duration)),
with(subset(groomdata,actor.as %in% c('AM')),mean(duration)),
with(subset(groomdata,actor.as %in% c('AM') & !recipient.as %in% c('AF','AM')),mean(duration)),
with(subset(groomdata,actor.as %in% c('AF')),mean(duration)),
with(subset(groomdata,actor.as %in% c('AF') & !recipient.as %in% c('AF','AM')),mean(duration))
),sep='\n')

cat(c(
with(subset(groomdata,actor.as %in% 'AM' & recipient.as %in% 'AF'),min(duration[duration>0])),
with(subset(groomdata,actor.as %in% 'AF' & recipient.as %in% 'AM'),min(duration[duration>0])),
with(subset(groomdata,actor.as %in% 'AM' & recipient.as %in% 'AM'),min(duration[duration>0])),
with(subset(groomdata,actor.as %in% 'AF' & recipient.as %in% 'AF'),min(duration[duration>0])),
with(subset(groomdata,actor.as %in% c('AF','AM') & recipient.as %in% c('AF','AM')),min(duration[duration>0])),
with(subset(groomdata,actor.as %in% c('AM')),min(duration[duration>0])),
with(subset(groomdata,actor.as %in% c('AM') & !recipient.as %in% c('AF','AM')),min(duration[duration>0])),
with(subset(groomdata,actor.as %in% c('AF')),min(duration[duration>0])),
with(subset(groomdata,actor.as %in% c('AF') & !recipient.as %in% c('AF','AM')),min(duration[duration>0]))
),sep='\n')

cat(c(
with(subset(groomdata,actor.as %in% 'AM' & recipient.as %in% 'AF'),max(duration)),
with(subset(groomdata,actor.as %in% 'AF' & recipient.as %in% 'AM'),max(duration)),
with(subset(groomdata,actor.as %in% 'AM' & recipient.as %in% 'AM'),max(duration)),
with(subset(groomdata,actor.as %in% 'AF' & recipient.as %in% 'AF'),max(duration)),
with(subset(groomdata,actor.as %in% c('AF','AM') & recipient.as %in% c('AF','AM')),max(duration)),
with(subset(groomdata,actor.as %in% c('AM')),max(duration)),
with(subset(groomdata,actor.as %in% c('AM') & !recipient.as %in% c('AF','AM')),max(duration)),
with(subset(groomdata,actor.as %in% c('AF')),max(duration)),
with(subset(groomdata,actor.as %in% c('AF') & !recipient.as %in% c('AF','AM')),max(duration))
),sep='\n')



# moo[with(foo[as.integer(rownames(subset(foo,timediff < 0)))-2,],paste0(id,'_',dateE)),]


# male.grooming = do.call(rbind,lapply(alldata.split,function(x) {
# 	this.year = unique(year(x$dateC))
# 	these.adults = individuals[individuals[[paste('Age',this.year,sep='_')]] %in% 'ADU' & !(individuals$Sex == 'M' & individuals$natal),c('ID','Sex')]
# 	groom = ct(x,behaviors='G',individuals=these.adults$ID,directional=TRUE)
# 	groom_freqs = bf(groom,digits=2)
# 	groom_freqs_mf = subset.as(groom_freqs,these.adults,all.x='M',all.y='F')
# 	
# 	out = data.frame(year=this.year,reshape2::melt(lapply(split(groom_freqs_mf$table,groom_freqs_mf$table$Var1),function(x) sum(x$scores))))
# 	names(out)[3] = 'id'
# 	out
# }))
# 
# 
# p = ggplot(male.grooming,aes(year,value,alpha=id,fill=id=='GAF')) +
# 	geom_bar(stat='identity',position=position_dodge()) +
# 	scale_alpha_manual(values=rep(1,14),guide=FALSE) +
# 	scale_fill_brewer(palette='Dark2',guide=FALSE) +
# 	theme_classic(base_size=24) + theme(axis.title.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
# 	scale_x_continuous(breaks=seq(2010,2019,1)) +
# 	ylab('Frequency')


# Simulate sex ratio correction

f = 40
m = 60

# mat = matrix(rnorm(10000,mean=10,sd=1),nrow=100,ncol=100)
mat = matrix(rnorm(10000,mean=10,sd=1),nrow=100,ncol=100)
diag(mat) = 0

d = data.frame(
	i = 1:100,
	sex = c(rep('M',m),rep('F',f))
)

ff = sum(mat[subset(d,sex == 'F')$i,subset(d,sex=='F')$i])
fm = sum(mat[subset(d,sex == 'F')$i,subset(d,sex=='M')$i])
mf = sum(mat[subset(d,sex == 'M')$i,subset(d,sex=='F')$i])
mm = sum(mat[subset(d,sex == 'M')$i,subset(d,sex=='M')$i])

c(ff,fm,mf,mm) / c(
	f * (f-1),
	f * m, f * m,
	m * (m-1)
)
