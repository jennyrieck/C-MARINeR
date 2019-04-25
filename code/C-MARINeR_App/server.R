library(shiny)
library(DistatisR)
library(prettyGraphs)
library(ExPosition)
library(gplots)
library(colorRamps)
shinyServer(function(input, output) {
  
  ### test data
  load('connectivity_cubes.rda')
  # this is the data frames and arrays from the global environment
  arrayList <- ls(envir = globalenv())[sapply(ls(envir = globalenv()), function(x) class(get(x))) == "array" ]
  designList<- grep('des',ls(envir = globalenv())[sapply(ls(envir = globalenv()), function(x) class(get(x))) == "character" ],value = T)
  
  thisCube<-eventReactive(input$go,{
    get(input$cubeIn)})
  
  my_range <- reactive({
    cbind(1,min(dim(thisCube()))-1)})
  
  thisColRowDesign<-eventReactive(input$replot,{
    get(input$rowColDesign)})
  
  
  
  
  statis.res<-reactive({distatis(thisCube(), Distance = F,
                                 nfact2keep=min(dim(thisCube()))-1)  })
  
  output$scree1<-renderPlot({
    plot(statis.res()$res4Cmat$eigValues,type = 'l', xlab='Component', ylab='Eigen Value',main='Rv Scree Plot')
  })
  
  output$scree2<-renderPlot({
    plot(statis.res()$res4Splus$eigValues,type = 'l', xlab='Component', ylab='Eigen Value',main='Compromise Scree Plot')
  })
  
  output$compromise_factor_map<-renderPlot({
    roi.design.colors<-createColorVectorsByDesign(makeNominalData(as.matrix(thisColRowDesign())))
    
    prettyPlot(statis.res()$res4Splus$F, x_axis = isolate(input$fp_xaxis), y_axis = isolate(input$fp_yaxis),
               col=roi.design.colors$oc,cex=1.5,dev.new=F,
               display_names = isolate(input$fp_labels),
               xlab=paste0('Component ', isolate(input$fp_xaxis), ': ', statis.res()$res4Splus$tau[isolate(input$fp_xaxis)], '% variance explained'),
               ylab=paste0('Component ', isolate(input$fp_yaxis), ': ', statis.res()$res4Splus$tau[isolate(input$fp_yaxis)], '% variance explained'))
    legend('topright',row.names(roi.design.colors$gc),fill=roi.design.colors$gc)
    
    ## Herve's plotting
    #compromise.graph.out <- createFactorMap(statis.res()$res4Splus$F,
    #                                        axis1 = isolate(input$fp_xaxis), 
    #                                        axis2 = isolate(input$fp_yaxis),
    #                                        title = 'Compromise Space',
    #                                        col.points = roi.design.colors$oc,
    #                                        display.labels=isolate(input$fp_labels),
    #                                        alpha.points=.6,cex=7) 
    # label4S <- createxyLabels.gen(
    #   x_axis   = isolate(input$fp_xaxis), y_axis = isolate(input$fp_yaxis),
    #   lambda   = statis.res()$res4Splus$eigValues , 
    #   tau      = statis.res()$res4Splus$tau,
    #   axisName = "Component ")
    # compromise.Smap <-  compromise.graph.out$zeMap + label4S 
    # compromise.Smap
       
  })
  output$compromise_heat_map<-renderPlot({
    lvs.to.rebuild<-c(isolate(input$heatmap_lvs[1]):isolate(input$heatmap_lvs[2]))
    rois.reorder<-sort.int(thisColRowDesign(), index.return = T)
    roi.design.colors<-createColorVectorsByDesign(makeNominalData(as.matrix(thisColRowDesign())))
    
    heatmap(tcrossprod(statis.res()$res4Splus$F[rois.reorder$ix,lvs.to.rebuild]),
            Rowv=NA, Colv=NA,
            ColSideColors = roi.design.colors$oc[rois.reorder$ix],RowSideColors = roi.design.colors$oc[rois.reorder$ix])
  })
  
  
  
})
