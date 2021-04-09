#!/usr/bin/env Rscript
library(sf)
library(ggplot2)

# Load and subset GIS maps
shps = st_read('GIS/GEM_ew_19.shp')

# Load participant locations
locs = read.delim("locations.txt")
clubLocs = read.delim("clubLocations.txt")

# Track missing locations
missing = as.character(locs$Location[which(!locs$Location %in% shps$GEN)])
write.table(missing, file="missing.txt")

# Merge GIS and incidence
shpsi = merge(shps, locs, by.x="GEN", by.y="Location")
shpsc = merge(shps, clubLocs, by.x="GEN", by.y="Location")
ambiguous = c(as.character(shpsi$GEN), as.character(shpsc$GEN))[which(duplicated(c(shpsi$GEN, shpsc$GEN)))]
write.table(ambiguous, file="ambiguous.txt")
# Handle ambiguous names using the NUTS: https://de.wikipedia.org/wiki/NUTS:DE
if(length(levels(shpsi$nuts))) {
    shpsi = shpsi[-which(shpsi$nuts !="" & as.character(shpsi$nuts) != shpsi$NUTS),]
}
if(length(levels(shpsc$nuts))) {
    shpsc = shpsc[-which(shpsc$nuts !="" & as.character(shpsc$nuts) != shpsc$NUTS),]
}

# Plot
annotation = "GIS: © GeoBasis-DE / BKG 2020"
bgColor="#003974"
starColor="#d7ab4e"
g = ggplot(shps) + geom_sf(fill=bgColor, color=bgColor) + theme_classic()
g = g + theme(axis.line=element_blank(), axis.text=element_blank(), axis.ticks = element_blank(), legend.title=element_text(size=24), legend.text=element_text(size=24), plot.title=element_text(size=24))
g = g + labs(x="", y="")
g = g + annotate(geom="text", label=annotation, x=Inf, y=-Inf, hjust=1, vjust=-0.1, size=5)
g = g + geom_sf_text(data=shpsi, label="★", size=9, color=starColor, family = "HiraKakuPro-W3")
g = g + geom_sf_text(data=shpsc, label="K", size=9, color=starColor)
#g + geom_sf_text(data=st_jitter(shpsi, factor=0.09), label="★", size=5, color="red", family = "HiraKakuPro-W3")
ggsave("participants.png", plot=g, width=6, height=6)
