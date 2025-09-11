#' @title Create a Pretty Heatmap GIF
#' @description
#' The \code{heatmapGIF()} function calculates and visualizes distances between surface meshes in an aesthetically pleasing GIF.
#'
#' @param sub1 The reference mesh.
#' @param sub2 The target mesh.
#' @param path The path to the file to save.
#' @param steps The number of transitions between images in the GIF.
#' @param colExtremes A vector of colours specifying the minimum and maximum colour values in the legend, respectively.
#' @param userMatrix A 4 by 4 matrix describing user actions to display the scene.
#' @param legend A logical value indicating whether a legend should be plotted and included in the final GIF.
#' @param bg The background colour: choices are "black" or "white".
#' @param symm A logical value indicating whether the GIF should be symmetric: alternating between two morphs from a central
#' average. Alternatively, GIFs may be directional, with a clear reference and target mesh.
#' @seealso heatmapPretty
#' @export
#'

heatmapGIF=function(sub1,sub2,path,steps=20,colExtremes=c("#0288D1", "#D32F2F"),userMatrix,legend=TRUE,bg="black",symm=TRUE,limit=NULL,legend_name="Closest Point Distance (mm)"){
  while(rgl.cur()>0){close3d()}
  name=gsub(".gif","",basename(path));folder=paste(dirname(path),name,"",sep="/")
  background=match(bg, c("white", "black"))
  dir.create(folder)
  col_ramp = colorRampPalette(c(colExtremes[1], "#FAFAFA", colExtremes[2]))
  col = col_ramp(100)
  lm1=sub1$vb[-4,];lm2=sub2$vb[-4,]
  avg=(lm1+lm2)/2;avg_face=sub2;avg_face$vb[-4,]=avg
  if(steps%%2==1&symm==TRUE){interval=steps+1}else{interval=steps}
  change=(lm2-lm1)/interval
  morphs=interval+1
  mat=array(NA,dim=c(dim(lm1)[1],dim(lm1)[2],morphs))
  for(i in 0:interval){mat[,,i+1]=lm1+(change*i)}
  if(symm==TRUE){
    dists_1=meshDist(avg_face,sub1,plot=FALSE)$dists; dists_2=meshDist(avg_face,sub2,plot=FALSE)$dists
    dists=c(dists_2,dists_1)
  } else{dists=meshDist(sub1,sub2,plot=FALSE)$dists}
  if(!is.null(limit)){to=limit;from=-limit} else{to=max(abs(dists));from=-to}
  open3d(zoom = 0.7, userMatrix = userMatrix)
  par3d("windowRect"= c(0, 100, 600, 800))
  bg3d(color=c("white", "black")[background])
  if(legend==TRUE){
    jpeg(paste0(folder, name,"_scale.png"),height=1000,width=275)
    par(cex=2.75,cex.axis=0.9,bg=c("white", "black")[background],col.lab=c("black", "white")[background],fg=c("black", "white")[background],lwd=5)
  }
  if(symm==TRUE){
    meshDist(sub2,distvec=dists_2,rampcolors=col,from=from,to=to,steps=100,titleplot=legend_name,xaxt=5)
  } else{meshDist(sub2,distvec=dists,rampcolors=col,from=from,to=to,steps=100,titleplot=legend_name,xaxt=5)}
  if(legend==TRUE){
    axis(2,col.axis=c("black", "white")[background],col=c("black", "white")[background],lwd.ticks=3)
    dev.off()
  }
  rgl.snapshot(paste0(folder,name,sprintf("%03d",morphs),"_1.png"),top=TRUE)
  clear3d()
  shade3d(sub2,col="white",specular=1)
  rgl.snapshot(paste0(folder,name,sprintf("%03d",morphs),"_2.png"),top=TRUE)
  clear3d()
  for(i in 1:interval){
    sub=sub2;sub$vb[-4,]=mat[,,i]
    if(symm==TRUE){
      dist_vec=meshDist(avg_face,sub,plot=FALSE)$dists
    } else{dist_vec=meshDist(sub1,sub,plot=FALSE)$dists}
    meshDist(sub,distvec=dist_vec,rampcolors=col,from=from,to=to,steps=100)
    rgl.snapshot(paste0(folder,name,sprintf("%03d",i),"_1.png"),top=TRUE)
    clear3d()
    shade3d(sub,col="white",specular=1)
    rgl.snapshot(paste0(folder,name,sprintf("%03d",i),"_2.png"),top=TRUE)
    clear3d()
  }
  while(rgl.cur()>0){close3d()}
  hm = paste0(folder,name,sprintf("%03d",1:morphs),"_1.png")
  mo = paste0(folder,name,sprintf("%03d",1:morphs),"_2.png")
  order=c(1:morphs,morphs:1)
  hm=hm[order];mo=mo[order]
  hm_img=list();mo_img=list()
  for(i in 1:length(hm)){
    hm_img[[i]]=image_read(hm[i])
    mo_img[[i]]=image_read(mo[i])
  }
  if(legend==TRUE){
    leg=image_read(paste0(folder,name,"_scale.png"));leg=image_crop(leg,"200x1000+0+0")
    for(i in 1:length(hm)){
      hm_img[[i]]=image_append(c(image_resize(leg,"x700"),hm_img[[i]]))
    }
  }
  img_combined=image_append(c(hm_img[[1]],mo_img[[1]]))
  for (i in 2:length(hm)){
    img_combined=c(img_combined,image_append(c(hm_img[[i]],mo_img[[i]])))
  }
  # Animate
  my_animation=image_animate(img_combined,fps=10,dispose="previous",optimize=TRUE)
  # Save
  image_write(my_animation,paste0(folder,name,".gif"))
}

#' @title Create a Pretty Heatmap
#' @description
#' The \code{heatmapPretty()} function calculates and visualizes distances between surface meshes in an aesthetically pleasing heatmap.
#'
#' @param sub1 The reference mesh.
#' @param sub2 The target mesh.
#' @param path The path to the file to save.
#' @param colExtremes A vector of colours specifying the minimum and maximum colour values in the legend, respectively.
#' @param userMatrix A 4 by 4 matrix describing user actions to display the scene.
#' @param legend A logical value indicating whether a legend should be plotted and included in the final heatmap.
#' @param bg The background colour: choices are "black" or "white".
#' @param limit The upper and lower bounds of the heatmap scale.
#' @seealso heatmapDirect
#' @export
#'

heatmapPretty=function(sub1,sub2,path,colExtremes=c("#0288D1", "#D32F2F"),userMatrix,legend=TRUE,legend_name="Closest Point Distance (mm)",legend_orientation="vertical",bg="black",limit=NULL){
  while(rgl.cur()>0){close3d()}
  name=gsub(".png","",basename(path));folder=paste0(dirname(path),"/")
  background=match(bg, c("white", "black"))
  col_ramp = colorRampPalette(c(colExtremes[1],"#FAFAFA",colExtremes[2]))
  col = col_ramp(100)
  dists=meshDist(sub2,sub1,plot=FALSE)$dists
  if(!is.null(limit)){to=limit;from=-limit} else{to=max(abs(dists));from=-to}
  open3d(zoom = 0.7, userMatrix = userMatrix)
  par3d("windowRect"= c(0, 100, 600, 800))
  bg3d(color=c("white", "black")[background])
  if(legend==TRUE){
    custom_colors=c(colExtremes[1],"#FAFAFA",colExtremes[2])
    custom_breaks=c(from,0,to)
    leg=data.frame(x=1:3,y=1:3,value=(custom_breaks))
    plot=ggplot(leg,aes(x,y,fill=value))+
      geom_tile()
    ori=match(legend_orientation,c("vertical", "horizontal"))
    if(ori==2){
      plot=plot+
        scale_fill_gradientn(colors=custom_colors,limits=c(from,to),name=legend_name,
                             guide=guide_colorbar(barwidth=7.5,barheight=0.75,ticks=TRUE,ticks.colour="black",frame.colour="black",direction="horizontal",nbin=100))+
        theme(legend.title=element_text(face="bold",hjust=0.5, size=9),legend.title.position="top",
              legend.text=element_text(size=9))
      leg_grab=get_legend(plot)
      ggsave(paste0(folder,name,"_scale_vec.pdf"),leg_grab,height=2,width=4,units="cm",dpi="print")
    } else {
      plot=plot+
        scale_fill_gradientn(colors=custom_colors,limits=c(from,to),name=legend_name,
                             guide=guide_colorbar(barwidth=0.75,barheight=6.5,ticks=TRUE,ticks.colour="black",frame.colour="black",direction="vertical",nbin=100))+
        theme(legend.title=element_text(face="bold",hjust=0.5, size=9),legend.title.position="top",
              legend.text=element_text(size=9))
      leg_grab=get_legend(plot)
      ggsave(paste0(folder,name,"_scale_vec.pdf"),leg_grab,height=4.5,width=2.5,units="cm",dpi="print")
    }
    #jpeg(paste0(folder,name,"_scale.png"),height=1000,width=275)
    #par(cex=2.75,cex.axis=0.9,bg=c("white", "black")[background],col.lab=c("black", "white")[background],fg=c("black", "white")[background],lwd=5)
  }
  meshDist(sub1,distvec=dists,rampcolors=col,from=from,to=to,steps=100,titleplot=legend_name,xaxt=5)
  # if(legend==TRUE){
  #   axis(2,col.axis=c("black", "white")[background],col=c("black", "white")[background],lwd.ticks=3)
  #   dev.off()
  # }
  rgl.snapshot(path,top=TRUE)
  close3d()
  # if(legend==TRUE){
  #   hm=image_read(path);leg=image_read(paste0(folder,name,"_scale.png"))
  #   leg=image_crop(leg,"200x1000+0+0")
  #   combine=image_write(image_append(c(image_resize(leg,"x700"),hm)),paste0(folder,name,"_complete.png"),format="png")
  # }
}

#' @title Create a Pretty Heatmap - Directional
#' @description
#' The \code{heatmapDirect()} function calculates and visualizes a unidirectional distance vector onto a surface mesh
#' in an aesthetically pleasing heatmap: useful for plotting F-values, Z-scores, or p-values onto a shape.
#' @param sub1 The reference mesh.
#' @param distvec A vector containing distances for each vertex/coordinate of x.
#' @param path The path to the file to save.
#' @param col Specify the colour value for maximum value in the legend.
#' @param userMatrix A 4 by 4 matrix describing user actions to display the scene.
#' @param legend A logical value indicating whether a legend should be plotted and included in the final heatmap.
#' @param bg The background colour: choices are "black" or "white".
#' @seealso heatmapPretty
#' @export
#'

heatmapDirect=function(sub1,distvec,path,colExtreme="#D32F2F",userMatrix,legend=TRUE,bg="black",legend_orientation="vertical",legend_name="Closest Point Distance (mm)",limit=NULL){
  while(rgl.cur()>0){close3d()}
  name=gsub(".png","",basename(path));folder=paste0(dirname(path),"/")
  background=match(bg, c("white", "black"))
  col_ramp = colorRampPalette(c("#FAFAFA", colExtreme))
  col = col_ramp(100)
  open3d(zoom = 0.7, userMatrix = userMatrix)
  par3d("windowRect"= c(0, 100, 600, 800))
  bg3d(color=c("white", "black")[background])
  if(!is.null(limit)){to=limit;from=0} else{to=max(abs(distvec));from=0}
  if(legend==TRUE){
    custom_colors=c("#FAFAFA", colExtreme)
    custom_breaks=c(from,to)
    leg=data.frame(x=1:2,y=1:2,value=(custom_breaks))
    plot=ggplot(leg,aes(x,y,fill=value))+
      geom_tile()
    ori=match(legend_orientation,c("vertical", "horizontal"))
    if(ori==2){
      plot=plot+
        scale_fill_gradientn(colors=custom_colors,limits=c(from,to),name=legend_name,
                             guide=guide_colorbar(barwidth=7.5,barheight=0.75,ticks=TRUE,ticks.colour="black",frame.colour="black",direction="horizontal",nbin=100))+
        theme(legend.title=element_text(face="bold",hjust=0.5, size=9),legend.title.position="top",
              legend.text=element_text(size=9))
      leg_grab=get_legend(plot)
      ggsave(paste0(folder,name,"_scale_vec.pdf"),leg_grab,height=2,width=4,units="cm",dpi="print")
    } else {
      plot=plot+
        scale_fill_gradientn(colors=custom_colors,limits=c(from,to),name=legend_name,
                             guide=guide_colorbar(barwidth=0.75,barheight=6.5,ticks=TRUE,ticks.colour="black",frame.colour="black",direction="vertical",nbin=100))+
        theme(legend.title=element_text(face="bold",hjust=0.5, size=9),legend.title.position="top",
              legend.text=element_text(size=9))
      leg_grab=get_legend(plot)
      ggsave(paste0(folder,name,"_scale_vec.pdf"),leg_grab,height=4.5,width=2.5,units="cm",dpi="print")
    }
    # jpeg(paste0(folder,name,"_scale.png"),height=1000,width=275)
    # par(cex=2.75,cex.axis=0.9,bg=c("white", "black")[background],col.lab=c("black", "white")[background],fg=c("black", "white")[background],lwd=5)
  }
  meshDist(sub1,distvec=abs(distvec),rampcolors=col,steps=100,titleplot=legend_name,xaxt=5,from=from,to=to)
  # if(legend==TRUE){
  #   axis(2,col.axis=c("black", "white")[background],col=c("black", "white")[background],lwd.ticks=3)
  #   dev.off()
  # }
  rgl.snapshot(path,top=TRUE)
  close3d()
  # if(legend==TRUE){
  #   hm=image_read(path);leg=image_read(paste0(folder,name,"_scale.png"))
  #   leg=image_crop(leg,"200x1000+0+0")
  #   combine=image_write(image_append(c(image_resize(leg,"x700"),hm)),paste0(folder,name,"_complete.png"),format="png")
  # }
}




