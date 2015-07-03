# read-loeb-exports
#
#  This script reads a whole directory of CSV files exported 
#  from the Loeb site over the course of the semester, culls
#  redundant entries and saves what's left.

ingest.dir <- function(dir, pattern=NA) {
  files <- dir(dir)
  if (! is.na(pattern)) {
    files <- grep(pattern, files, value=T, perl=T)
  }
  do.call(rbind,
    lapply(file.path(dir, files), function(file) {
      read.csv(file, skip=1, stringsAsFactors=F)
    })
  )
}

loeb.data <- ingest.dir(file.path("data", "loeb-raw-exports"))
loeb.data <- unique(loeb.data)
write.table(loeb.data, file.path("data", "all-loeb.txt"), quote=T, sep="\t", col.names=T, row.names=F)
