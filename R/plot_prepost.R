#' Plots the distribution of all variables in the completed versus the original dataset.
#'
#' @param data dataframe of the completed data, can also be a multiple imputed dataset (mids), imputed by MICE
#' @param orig_data dataframe of the incompleted dataset
#' @param numcols number of columns in the final plot
#' @return Returns a plot with multiple panels, for each variable of the dataset, comparing the distribution with the original data.
plot_prepost <- function(data=NULL, orig_data=NULL, numcols=1){
  if(class(data)=="mids"){
    nvar <- ncol(data$data)
  }else{
    nvar <- ncol(data)
  }
  plots <- vector("list", nvar)

  if(class(data)=="mids"){
    datalong <- complete(data, "long", include = TRUE)
    for(i in 3:(nvar+2)){
      if(class(datalong[,i])=="factor"){
        plot.df <- data.frame(prop.table(table(datalong$.imp,datalong[,i])))
        plot.df$Var2 <- factor(plot.df$Var2, levels=levels(datalong[,i]))
        plots[[i-2]] <- ggplot(plot.df, aes(x=Var2, y=Freq, fill=Var1))+
          geom_bar(stat="identity", position = position_dodge(width=1))+
          xlab(colnames(datalong)[i])+scale_fill_discrete("Imputation set")+
          theme_bw()+theme(text=element_text(size=16))
      }else{
        plot.df <- data.frame(aggregate(datalong[,i]~datalong[,".imp"], FUN = quantile, na.rm=TRUE))
        plot.df <- data.frame(cbind(imp= plot.df$datalong.....imp..,
                                    med= plot.df$datalong...i.[,3],
                                    lo = plot.df$datalong...i.[,2],
                                    hi = plot.df$datalong...i.[,4]))
        plots[[i-2]] <- ggplot(plot.df,
                               aes(x=colnames(datalong)[i],y=med, ymin=lo, ymax=hi, col=factor(imp)))+
          geom_pointrange(position = position_dodge(width=1))+
          ylab("Median (IQR)")+scale_x_discrete("", labels=colnames(datalong)[i])+
          scale_color_discrete("Imputation set")+
          theme_bw()+theme(text=element_text(size=16))
      }
    }
  }else{
    for(i in 1:ncol(data)){
      if(class(data[,i])=="factor"){
        plot.df <- data.frame(prop.table(table(data[,i])))
        plot.df <- rbind(plot.df, data.frame(prop.table(table(orig_data[,colnames(data)[i]]))))
        plot.df$dataset <- rep(c("Imputed", "Original"), each=length(levels(data[,i])))
        plot.df$Var1 <- factor(plot.df$Var1, levels=levels(data[,i]))
        plots[[i]] <- ggplot(plot.df, aes(x=Var1, y=Freq, fill=dataset))+
          geom_bar(stat="identity", position = position_dodge(width=1))+
          xlab(colnames(data)[i])+scale_fill_discrete("Dataset")+
          theme_bw()+theme(text=element_text(size=16))
      }else{
        plot.df <- (data.frame(t(cbind(quantile(data[,i]),
                                       quantile(orig_data[,colnames(data)[i]], na.rm=TRUE)))))
        plot.df$dataset <- c("Imputed", "Original")
        plots[[i]] <- ggplot(plot.df,
                             aes(x=colnames(data)[i],y=X50., ymin=X25., ymax=X75., col=factor(dataset)))+
          geom_pointrange(position = position_dodge(width=1))+
          ylab("Median (IQR)")+scale_color_discrete("Dataset")+
          scale_x_discrete("", labels=colnames(data)[i])+
          theme_bw()+theme(text=element_text(size=16))
      }
    }
  }

  multiplot(plotlist = plots,cols = numcols)
}
