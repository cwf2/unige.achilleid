# Mt. A. faculty research day 2018
#
# Modified from week-5.R

# libraries

# load base script
source(file.path("scripts", "loeb.R"))

#
# functions
#

make.samples <- function(verses, labels=NULL, n=20) {
  # cut a text from the corpus into n-line samples
  
  sampleid <- floor(seq(along.with = verses)/n)
  samples <- split(verses, sampleid)
  samples <- squash.ortho(unlist(lapply(samples, paste, collapse=" ")))
  
  if (! is.null(labels)) {
    labels <- sapply(split(labels, sampleid), function(lab) {
      paste(lab[1], lab[length(lab)], sep=":")
    })
  }
  
  names(samples) <- labels
  return(samples)
}

make.theme.index <- function(file) {
  # read in scene divisions and classifications from a file
  
  themes <- read.table(file, header=T, stringsAsFactors=F)
  themes$note <- factor(themes$note, levels=themes$note)
  lno <- sapply(strsplit(corpus[["stat_ach"]]$loc, ","), as.integer)
  lno <- lno[2, lno[1,]==1]
  index <- themes[findInterval(lno, themes$start), "note"]
  return(index)
}


#
# main
#

# load the corpus
index.file <- file.path("data", "index-large-corpus.txt")
index <- fread(index.file)
index$src <- squash.cit(index$cit)
setkey(index, src)
corpus <- load.ref.corpus(index.file)

# create samples of 50 lines, tagged by source, class
samples <- lapply(corpus, function(x){
  make.samples(x$verse, labels=x$loc, n=50)
})
samples <- data.table(
  src = rep(names(samples), times=sapply(samples, length)),
  id = names(unlist(samples)),
  text = unlist(samples)
)
samples$class <- as.factor(index[samples$src, class])
samples$src <- as.factor(samples$src)
setkey(samples, src, class)

# calculate all word frequencies for the corpus
wf.corpus <- wc(samples$text, freq=F, remove.hapax=T)

# build a feature set:
#  - drop the top 50 words, take the next 2000
features <- names(wf.corpus)[51:2050]

# calculate a feature vector for each sample using these 2000 word frequencies
vec.samples <- rbindlist(lapply(samples$text, feat.vec, features=features))
setnames(vec.samples, features)

# PCA
pca.model <- prcomp(vec.samples)
pca.samples <- data.table(pca.model$x[,1:2], samples[,.(src, class)])

# mask to filter out ov_met
mask <- samples$src != 'ov_met'

# simple plot of all samples by class
pdf(file='epic-elegy-50_line-no_met.pdf', height=9, width=14, pointsize = 14)
plot(pca.model$x[mask, 1:2], col=pca.samples[mask, class], lwd=1.5)
dev.off()

pdf(file='epic-elegy-50_line-met.pdf', height=9, width=14, pointsize = 14)
plot(pca.model$x[mask, 1:2], col=pca.samples[mask, class], lwd=1.5)
points(pca.model$x[!mask, 1:2], col=3, lwd=1.5)
dev.off()

pdf(file='epic-elegy-50_line-no_met-div.pdf', height=9, width=14, pointsize = 14)
plot(pca.model$x[mask, 1:2], col=pca.samples[mask, class], lwd=1.5)
abline(v=-2.1, lty=3, lwd=1.5)
dev.off()
