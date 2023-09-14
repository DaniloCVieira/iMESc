#newcolhabs<-vals$newcolhabs
ggbox<-function(res,pal,violin=F,horiz=F,base_size=12,cex.axes=1,cex.lab=1,
                cex.main=1,xlab=colnames(res)[1],ylab=colnames(res)[2],main="",
                box_linecol="firebrick",box_alpha=0.7,newcolhabs,cex.label_panel=10,varwidth=F, linewidth=.8, theme='theme_bw', grid=T, background="white",xlab_rotate=0,ylab_rotate=0){
  wrap=F
  if(ncol(res)>2){
    res2<-res
    colnames(res2)[1]<-c("x")
    res2<-reshape2::melt(res2,"x")
    colnames(res2)[3]<-"y"
    res<-res2
    wrap=T

  } else{
    colnames(res)<-c("x","y")
  }

  coline<-box_linecol
  cols<-newcolhabs[[pal]](nlevels(res$x))
  cols<-lighten(cols,box_alpha)
  p<-ggplot(res, aes(x=x, y=y, fill=x))
  if(isTRUE(violin)){
    p<-p+geom_violin(color=coline)
  } else{
    p<-p+stat_boxplot(geom='errorbar', linetype=1, width=0.3,color=coline)+
      geom_boxplot(fill="white")+  geom_boxplot(varwidth =varwidth,size=linewidth,color=coline)
  }




  p<-switch(theme,
            'theme_grey'={p+theme_grey(base_size)},
            'theme_bw'={p+theme_bw(base_size)},
            'theme_linedraw'={p+theme_linedraw(base_size)},
            'theme_light'={p+theme_light(base_size)},
            'theme_dark'={p+theme_dark(base_size)},
            'theme_minimal'={p+theme_minimal(base_size)},
            'theme_classic'={p+theme_classic(base_size)},
            'theme_void'={p+theme_void(base_size)})


  if(isFALSE(grid)){
    p<-p+theme(panel.grid=element_blank())
  }
  #theme(panel.background=element_rect(fill=NA, color=background))

  p<-p+
    scale_fill_manual(values=cols) +
    ggtitle(main) +
    xlab(xlab)+ylab(ylab)+ theme(
      legend.position="none",

      #panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
      strip.text.x = element_text(size = cex.label_panel),
      axis.line=element_line(),
      axis.text=element_text(size=cex.axes),
      axis.title=element_text(size=cex.lab),
      plot.title=element_text(size=cex.main,face="bold"),
      axis.text.x = element_text(angle = xlab_rotate,vjust = .5, hjust = .5),
      axis.text.y = element_text(angle = ylab_rotate,vjust = .5, hjust = .5)

    )

  if(isTRUE(horiz)){
    p<-p+coord_flip()

  }

  if(isTRUE(wrap)){

    p<-p+facet_wrap(~variable, scales = "free_y")
  }
  p
}
