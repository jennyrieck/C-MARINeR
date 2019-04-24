nki.dir<-'C:/Users/jenny/Downloads/NKI_Rockland/'

# ### All the noGSR functional connectivity matrices:
# 
# fmri.conn.fns<-list.files(nki.dir,'fcMRI_noGSR_connectivity_matrix_file')
# ids<-gsub('_fcMRI_noGSR_connectivity_matrix_file.txt','',fmri.conn.fns)
# 
# i<-1
# 
# this.conn<-read.table(paste0(nki.dir,fmri.conn.fns[3]))

## read in metadata to find ages
metadata<-read.csv('C:/Users/jenny/Downloads/NKI_Rockland_csv')
metadata.fmri<-metadata[intersect(which(metadata$upload_data.imaging_modality=='fMRI'),
                                  grep('noGSR',metadata$upload_data.network_name)),]
metadata.fmri.ids<-gsub('_fcMRI_noGSR','',metadata.fmri$upload_data.network_name)

### find 10 young, 10 old:
YA<-metadata.fmri$upload_data.network_name[metadata.fmri$upload_data.age_range_min>=20 & metadata.fmri$upload_data.age_range_min<36][1:10]
YA.ids<-gsub('_fcMRI_noGSR','',YA)
OA<-metadata.fmri$upload_data.network_name[metadata.fmri$upload_data.age_range_min>=65 & metadata.fmri$upload_data.age_range_min<85][1:10]
OA.ids<-gsub('_fcMRI_noGSR','',OA)


roi.labs.full<-read.csv(paste0(nki.dir,YA.ids[1],'_fcMRI_noGSR_region_names_full_file.txt'),header=F)
roi.labs<-gsub(' ','.',roi.labs.full$V1)
roi.labs<-gsub('Left','lh',roi.labs)
roi.labs<-gsub('Right','rh',roi.labs)
roi.labs<-gsub('Temporal','temp',roi.labs)
roi.labs<-gsub('Frontal','front',roi.labs)
roi.labs<-gsub('Occipital','occi',roi.labs)
roi.labs<-gsub('occipital','occi',roi.labs)
roi.labs<-gsub('Parietal','pari',roi.labs)
roi.labs<-gsub('cingulate','cing',roi.labs)
roi.labs<-gsub('Cingulate','cing',roi.labs)
roi.labs<-gsub('anterior','ant',roi.labs)
roi.labs<-gsub('posterior','post',roi.labs)
roi.labs<-gsub('superior','sup',roi.labs)
roi.labs<-gsub('Superior','sup',roi.labs)
roi.labs<-gsub('Inferior','inf',roi.labs)
roi.labs<-gsub('inferior','inf',roi.labs)
roi.labs<-gsub('Middle','mid',roi.labs)
roi.labs<-gsub('Lateral','lat',roi.labs)
roi.labs<-gsub('Medial','med',roi.labs)
roi.labs<-gsub('fusiform','fusi',roi.labs)


conn.cube.YA<-array(NA,dim=c(length(roi.labs),length(roi.labs),length(YA.ids)))
rownames(conn.cube.YA) <- colnames(conn.cube.YA) <- roi.labs
dimnames(conn.cube.YA)[[3]] <- YA

conn.cube.OA<-array(NA,dim=c(length(roi.labs),length(roi.labs),length(OA.ids)))
rownames(conn.cube.OA) <- colnames(conn.cube.OA) <- roi.labs
dimnames(conn.cube.OA)[[3]] <- OA


for(y in 1:length(YA.ids)){
  #conn.cube[,,y]<-read.table(paste0(nki.dir,YA.ids[y],'_fcMRI_noGSR_connectivity_matrix_file.txt'),header = F)
  this.conn<-read.table(paste0(nki.dir,YA.ids[y],'_fcMRI_noGSR_connectivity_matrix_file.txt'),header = F)
  diag(this.conn)<-1
  conn.cube.YA[,,y]<-as.matrix(this.conn)
#  print(dim(this.conn))
}

for(o in 1:length(OA.ids)){
  #conn.cube[,,y]<-read.table(paste0(nki.dir,YA.ids[y],'_fcMRI_noGSR_connectivity_matrix_file.txt'),header = F)
  this.conn<-read.table(paste0(nki.dir,OA.ids[o],'_fcMRI_noGSR_connectivity_matrix_file.txt'),header = F)
  diag(this.conn)<-1
  conn.cube.OA[,,o]<-as.matrix(this.conn)
  #  print(dim(this.conn))
}

save(conn.cube.YA, conn.cube.OA, conn.cube.tog,random.network.design, file='connectivity_cubes.rda')

rand.network<-sample(c(1:7),188,replace=T)
random.network.design<-paste0('Network',rand.network)

conn.cube.tog<-array(NA,dim=c(length(roi.labs),length(roi.labs),length(YA.ids)+length(OA.ids)))
rownames(conn.cube.tog) <- colnames(conn.cube.tog) <- roi.labs
conn.cube.tog[,,1:10]<-conn.cube.YA
conn.cube.tog[,,11:20]<-conn.cube.OA

meanCube<-apply(conn.cube.tog,c(1,2),mean)






