### orientation checks...
### radio vs. neuro
### across brains matches for dims
## pixdim? there might be a negative there.


## this is hierarchical. use ANTsR if possible, oro.nifti if needed
## can use other image reading tools, too.
## just not now.

fmri_to_matrix <- function(fmri_fileName, brainMask_fileName = NULL, roiMask_fileName = NULL, fmri_threshold = NULL, roi_labels = NULL){
  
  suppressWarnings(antsr_available <- require(ANTsR))
  suppressWarnings(oro.nifti_available <- require(oro.nifti))
  
  use_antsr <- use_oro.nifti <- FALSE
  if(antsr_available){
    use_antsr <- TRUE
  }else{
    if(oro.nifti_available){
      use_oro.nifti <- TRUE
    }else{
      stop("You must install 'ANTsR' or 'oro.nifti' packages.")
    } 
  }
  
  ## actually simplify for now.
  ## just perform all the reads (up to 3)
  if(use_antsr){
    fmri_image <- antsImageRead(fmri_fileName)
  }
  if(use_oro.nifti){
    fmri_image <- readNIfTI(fmri_fileName)
  }
  if(length(dim(fmri_image)) != 4 ){
    stop("'fmri_image' is not a 4D array")
  }
  
  if(use_antsr){
    if(!is.null(brainMask_fileName)){
      brainMask_image <- antsImageRead(brainMask_fileName)
      
      ## what else should I test for?
      if(!all(ANTsRCore::origin(fmri_image)[1:3] == ANTsRCore::origin(brainMask_image))){
        stop("origin not the same between fmri_fileName and brainMask_fileName")
      }
      
    }else{
      brainMask_image <- array(1, dim=c(dim(fmri_image)[1:3]))
    }
    
    if(!is.null(roiMask_fileName)){
      roiMask_image <- antsImageRead(roiMask_fileName)
      
      ## what else should I test for?
      if(!all(ANTsRCore::origin(fmri_image)[1:3] == ANTsRCore::origin(roiMask_image))){
        stop("origin not the same between fmri_fileName and roiMask_fileName")
      }
      
    }else{
      roiMask_image <- array(1, dim=c(dim(fmri_image)[1:3]))
    }
  }
  
  if(use_oro.nifti){
    if(!is.null(brainMask_fileName)){
      brainMask_image <- readNIfTI(brainMask_fileName)
      
      if( 
        (fmri_image@qoffset_x != brainMask_image@qoffset_x) |
        (fmri_image@qoffset_y != brainMask_image@qoffset_y) |
        (fmri_image@qoffset_z != brainMask_image@qoffset_z)
      ){
        stop("origin not the same between fmri_fileName and brainMask_fileName")
      }
      
    }else{
      brainMask_image <- array(1, dim=c(dim(fmri_image)[1:3]))
    }
    
    if(!is.null(roiMask_fileName)){
      roiMask_image <- readNIfTI(roiMask_fileName)
      
      ## what else should I test for?
      if( 
        (fmri_image@qoffset_x != roiMask_image@qoffset_x) |
        (fmri_image@qoffset_y != roiMask_image@qoffset_y) |
        (fmri_image@qoffset_z != roiMask_image@qoffset_z)
      ){
        stop("origin not the same between fmri_fileName and roiMask_fileName")
      }
      
    }else{
      roiMask_image <- array(1, dim=c(dim(fmri_image)[1:3]))
    }
  }
  
  
  if( length(dim(brainMask_image))!=3 ){
    stop("brainMask_image is not a 3D array")
  }
  if( length(dim(roiMask_image))!=3  ){
    stop("brainMask_image is not a 3D array")
  }
  if(!all(dim(brainMask_image)==dim(roiMask_image)) ){
    stop("brainMask_image and roiMask_image dimensions are not the same.")
  }
  if(!all(dim(brainMask_image)==dim(fmri_image)[1:3]) ){
    stop("brainMask_image and fmri_image do not share common dimensions.")
  }
  if(!all(dim(roiMask_image)==dim(fmri_image)[1:3]) ){
    stop("roiMask_image and fmri_image do not share common dimensions.")
  }
  
  ## now that we're here, combine the two masks.
  ## drop all places that either are 0
  
  finalMask_image <- roiMask_image
  finalMask_image[ as.array(brainMask_image)==0 ] <- 0
  
  if(!is.null(fmri_threshold)){
    fmri_image[fmri_image <= fmri_threshold] <- NA
  }
  
  
  return( timeseries_to_matrix(fmri_image, finalMask_image, roi_labels) )
  
}
