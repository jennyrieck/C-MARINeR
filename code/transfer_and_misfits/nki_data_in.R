nki.dir<-'C:/Users/jenny/Downloads/NKI_Rockland/'

### All the noGSR functional connectivity matrices:

fmri.conn.fns<-list.files(nki.dir,'fcMRI_noGSR_connectivity_matrix_file')
ids<-gsub('_fcMRI_noGSR_connectivity_matrix_file.txt','',fmri.conn.fns)

i<-1

this.conn<-read.table(paste0(nki.dir,fmri.conn.fns[3]))

## read in metadata to find ages


metadata<-read.csv('C:/Users/jenny/Downloads/NKI_Rockland_csv')
metadata.fmri<-metadata[intersect(which(metadata$upload_data.imaging_modality=='fMRI'),
                                  grep('noGSR',metadata$upload_data.network_name)),]
metadata.fmri.ids<-gsub('_fcMRI_noGSR','',metadata.fmri$upload_data.network_name)










