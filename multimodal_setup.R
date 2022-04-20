
# multimodal seqFISH object from brain

setwd('~/Dropbox/MSSM/giottoBuild/brainDNA/')
colfunc <- colorRampPalette(c("red","white","blue"))
colfunc2 <- colorRampPalette(c("blue","white","red"))

# DNA data
data.dna <- read.csv("TableS8_brain_DNAseqFISH_25kb_voxel_coordinates_2762cells.csv")
# name loci by chromID_regionID
data.dna$featureID <- paste0(data.dna$chromID,'_',data.dna$regionID_chrom)
# convert to feature by cell count matrix
features.use <- lapply(1:20,function(x) paste0(x,'_',1:60))
features.use <- unlist(features.use)
cells.use <- sort(unique(data.dna$finalcellID))

source('Functions.R')
m.dna <- convertCountMtx(raw_data = data.dna,features_col = 'featureID',cells_col = 'finalcellID',
                         cells_ID = cells.use,features_names = features.use)

# reference genomic coordinates
coords.use <- read.csv("~/Dropbox/MSSM/seqFISHplus_brain/TableS0_25kbcoords.csv")
coords.use$Region <- sapply(as.character(coords.use$Name),function(x) 
  as.numeric(unlist(strsplit(x,'#'))[2]))

# RNA & intron copy count data
data.rna <- read.csv('TableS5_brain_RNA_profiles_2762cells.csv')
# cell by count matrix of mRNA copies
m.mrna <- data.rna[,7:ncol(data.rna)]
m.mrna <- m.mrna[,grep('intron',colnames(m.mrna),invert = T)]
rownames(m.mrna) <- data.rna$finalcellID
m.mrna <- t(m.mrna)
# cell by count matrix of intron copies
m.intron <- data.rna[,grep('intron',colnames(data.rna))]
rownames(m.intron) <- data.rna$finalcellID
m.intron <- t(m.intron)
# check if cells are the same between DNA and RNA
identical(colnames(m.dna),colnames(m.mrna))

# intron voxel coordinates
data.intron <- read.csv('TableS6_brain_intron_voxel_coordinates.csv')

# chromatin (global) data (normalized & scaled by Yodai)
data.IF.1 <- read.csv("~/Dropbox/MSSM/seqFISHplus_brain/RevisionAnalysis/LC1-brain-029-brain-repALL-IF-intensity-mean-norm-per-fov.csv")
data.IF.2 <- read.csv("~/Dropbox/MSSM/seqFISHplus_brain/RevisionAnalysis/LC1-brain-029-brain-repALL-IF-intensity-mean-norm-per-fov-Zscore-per-cell.csv")
m.chromatin.norm <- data.IF.1[,6:ncol(data.IF.1)]
m.chromatin.scaled <- data.IF.2[,6:ncol(data.IF.2)]
rownames(m.chromatin.norm) <- data.IF.1$finalcellID
m.chromatin.norm <- t(m.chromatin.norm)
rownames(m.chromatin.scaled) <- data.IF.2$finalcellID
m.chromatin.scaled <- t(m.chromatin.scaled)
identical(colnames(m.chromatin.norm),colnames(m.chromatin.scaled))
identical(colnames(m.chromatin.norm),colnames(m.dna))

# cell centroid
loc.use <- read.csv('science.abj1966_table_s4.csv')
rownames(loc.use) <- loc.use$finalcellID
loc.use <- loc.use[,grep('centroid',colnames(loc.use))]
colnames(loc.use) <- c('x','y','z')

# set up Giotto object with multimodal profiles
devtools::load_all("~/Giotto/")
meta.use <- data.rna[,1:6]
rna_data = list(raw = m.mrna)
dna_data = list(raw = m.dna)
intron_data <- list(raw = m.intron)
IF_data <- list(norm = m.chromatin.norm)


brain <- createGiottoObject(expression = list(rna = rna_data,  dna = dna_data, intron = intron_data, chromatin = IF_data),
                              spatial_locs = loc.use,
                              expression_feat = list('rna','dna','intron','chromatin'))
brain <- addCellMetadata(brain,new_metadata = meta.use)
print (brain@expression_feat)
# add scaled IF data
brain <- set_expression_values(gobject = brain,feat_type = 'chromatin',name = 'scaled',values = m.chromatin.scaled)


######

# add subcellular coordinates (3D)

# add DNA loci
loc.dna <- data.dna[,c('x','y','z','featureID')]
brain <- addGiottoPoints3D_v2(gobject = brain,coords = loc.dna,feat_type = 'dna')

# add intron loci
loc.intron <- data.intron[,c('x','y','z','gene')]
brain <- addGiottoPoints3D_v2(gobject = brain,coords = loc.intron,feat_type = 'intron')

saveRDS(brain,file = 'brain_multimodal.rds')

spatInSituPlotPoints(brain,feats = '1_1',feat_type = 'dna')

