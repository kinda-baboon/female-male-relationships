#!/usr/bin/env Rscript

# build contingency table
ct <- function(a,cols=c('behavior','actor','recipient','timestamp','focalAnimal'),behaviors=FALSE,individuals=FALSE,distances=c("0m","0-2m","2-6m",">6m","none"),directional=FALSE,interval=120,cutoff=50,directionalFocals=FALSE) {
	b <- a
	b[[cols[4]]] <- as.numeric(b[[cols[4]]])
	b$tempnumber <- b[[cols[4]]] - c(10000,b[[cols[4]]][1:(length(b[[cols[4]]])-1)])
	b$tempnumber[b$tempnumber < 0] <- interval
	b <- b[b$tempnumber > cutoff,]

	npts_table <- table(b[[cols[5]]])
	npts_table <- npts_table[!is.na(match(rownames(npts_table),individuals))]

	npts_matrix <- matrix(0,nrow=length(individuals),ncol=length(individuals),dimnames=list(individuals,individuals))
	npts_matrix[names(npts_table),] <- npts_table

	if (!directionalFocals) {
		# Total number of points is not directional because behaviors can get recorded actively or passively
		npts_matrix <- npts_matrix + t(npts_matrix)
	}
	npts_matrix[seq(1,length(npts_matrix),ncol(npts_matrix)+1)] <- 0

	if (!identical(behaviors,FALSE)) a <- a[!is.na(match(a[[cols[1]]],behaviors)),]
	a <- a[!is.na(match(a[[cols[2]]],individuals)) & !is.na(match(a[[cols[3]]],individuals)),]
	nn <- array(0,dim=c(length(individuals),length(individuals),length(behaviors)),dimnames=list(individuals,individuals,behaviors))
	x <- as.array(table(a[[cols[2]]],a[[cols[3]]],a[[cols[1]]]))
	ix <- as.matrix(expand.grid(dimnames(x)))
	nn[ix] <- x[ix]
	if (!directional) {
		y <- aperm(x,c(2,1,3))
		iy <- as.matrix(expand.grid(dimnames(y)))
		nn[iy] <- nn[iy] + y[iy]
	}
	l <- list('data.frame'=a,'points.table'=npts_table,'points.matrix'=npts_matrix,'contingency'=nn)
	class(l) <- 'contingency'
	l
}

# calculate proximity statistics (esp. C-scores, Smuts, 1985, Palombit et al. 1997)
cs <- function(a,zero.to.two.only=FALSE,prettify=TRUE,digits=4) {
	nn <- a$contingency
	npts_matrix <- a$points.matrix
	npts_array <- array(rep(npts_matrix,length(dimnames(nn)[[3]])),dim=c(length(dimnames(nn)[[1]]),length(dimnames(nn)[[2]]),length(dimnames(nn)[[3]])),dimnames=list(dimnames(nn)[[1]],dimnames(nn)[[2]],dimnames(nn)[[3]]))
	tscores <- nn / npts_array
	na <- !is.na(apply(nn,1:2,sum)[rownames(nn),colnames(nn)] / npts_matrix[rownames(nn),colnames(nn)])
	na <- array(na,dim=c(nrow(na),ncol(na),length(dimnames(npts_array)[[3]])),dimnames=list(rownames(na),colnames(na),dimnames(npts_array)[[3]]))
	tscores[na][is.na(tscores[na])] <- 0
	if (!zero.to.two.only) {
		cscores <- tscores[,,1] + tscores[,,2] + tscores[,,3] / 4
	} else {
		cscores <- tscores[,,1] + tscores[,,2]
	}
	cscores[is.na(cscores) | is.infinite(cscores) | is.nan(cscores)] <- NA

	cscores <- (cscores / sum(cscores,na.rm=TRUE)) * sum(nn)
	
	ctable <- expand.grid(rownames(cscores),colnames(cscores),stringsAsFactors=FALSE)
	
	cscores <- round(cscores,digits)
	ctable$scores <- as.numeric(cscores)

# 	if (prettify) {
# 		ctable$scores <- 10^digits * round(as.numeric(cscores),digits)
# 		cscores <- 10^digits * round(cscores,digits)
# 		cscores <- round(cscores,digits)
# 		ctable$scores <- as.numeric(cscores)
#  	} else {
#  		ctable$scores <- as.numeric(cscores)
#  	}
	ctable <- ctable[order(ctable[[2]],-ctable$scores),]
	ctable <- ctable[ctable[[1]] != ctable[[2]],]
	l <- list('array'=cscores,'table'=ctable)
	class(l) <- 'c.scores'
	l
}



# calculate frequencies of behaviors of interest
bf <- function(a,prettify=TRUE,digits=4) {
	beh <- a$contingency
	npts_matrix <- a$points.matrix
	npts_array <- array(rep(npts_matrix,length(dimnames(beh)[[3]])),dim=c(length(dimnames(beh)[[1]]),length(dimnames(beh)[[2]]),length(dimnames(beh)[[3]])),dimnames=list(dimnames(beh)[[1]],dimnames(beh)[[2]],dimnames(beh)[[3]]))
	behfreqs <- beh / npts_array

	behfreqs[is.na(behfreqs) | is.infinite(behfreqs) | is.nan(behfreqs)] <- NA

	behfreqs <- (behfreqs / sum(behfreqs,na.rm=TRUE)) * sum(beh)

	behtable <- expand.grid(rownames(behfreqs),colnames(behfreqs),dimnames(beh)[[3]],stringsAsFactors=FALSE)

	behfreqs <- round(behfreqs,digits)
	behtable$scores <- as.numeric(behfreqs)

# 	if (prettify) {
# 		behtable$scores <- 10^digits * round(as.numeric(behfreqs),digits)
# 		behfreqs <- 10^digits * round(behfreqs,digits)
# 	} else {
# 		behtable$scores <- as.numeric(behfreqs)
# 	}

	behtable <- behtable[order(behtable[[2]],-behtable$scores),]
	behtable <- behtable[behtable[[1]] != behtable[[2]],]
	l <- list('array'=behfreqs,'table'=behtable)
	class(l) <- 'b.freqs'
	l
}

# subset matrix or array by age-sex class
subset.as <- function(a,asx,all.x=TRUE,all.y=TRUE) {
	m <- a[['array']]
	d <- a[['table']]
	if (identical(all.x,TRUE)) {
		x.individuals <- asx[[1]]
	} else {
		x.individuals <- asx[[1]][!is.na(match(asx[[2]],all.x)),drop=TRUE]
	}
	if (identical(all.y,TRUE)) {
		y.individuals <- asx[[1]]
	} else {
		y.individuals <- asx[[1]][!is.na(match(asx[[2]],all.y)),drop=TRUE]
	}
	if (class(m) == 'matrix') {
		l <- list('array'=m[x.individuals,y.individuals],'table'=d[!is.na(match(d[[1]],x.individuals)) & !is.na(match(d[[2]],y.individuals)),])
	} else if (class(m) == 'array') {
		l <- list('array'=m[x.individuals,y.individuals,],'table'=d[!is.na(match(d[[1]],x.individuals)) & !is.na(match(d[[2]],y.individuals)),])
	}
	class(l) <- class(a)
	l
}

# Subset any matrix
subset.as2 <- function(m,asx,all.x=TRUE,all.y=TRUE) {
	if (identical(all.x,TRUE)) {
		x.individuals <- asx[[1]]
	} else {
		x.individuals <- asx[[1]][!is.na(match(asx[[2]],all.x)),drop=TRUE]
	}
	if (identical(all.y,TRUE)) {
		y.individuals <- asx[[1]]
	} else {
		y.individuals <- asx[[1]][!is.na(match(asx[[2]],all.y)),drop=TRUE]
	}
	if (class(m) == 'matrix') {
		m <- m[x.individuals,y.individuals]
	} else if (class(m) == 'array') {
		m <- m[x.individuals,y.individuals,]
	}
	m
}
