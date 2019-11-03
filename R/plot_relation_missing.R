#' Plot the relationship between the variables of the dataset and the
#' missingness of the variable of interest
#'
#' @param varbl variable of interest (character)
#' @param data dataframe of the data (including the variable of interest)
#' @return Returns a plot showing the OR of the other variables for the missingness of the variable of interest
plot_relation_missing <- function(varbl=NULL, data=NULL){
  y <- as.numeric(is.na(data[,varbl]))
  x <- data[,-which(colnames(data)==varbl)]
  x$y <- y
  fit <- glm(y~.,family="binomial", data=x)

  plot.df     <- data.frame(coef(summary(fit)))
  plot.df$var <- rownames(plot.df)
  ggplot(plot.df[-1,],
         aes(x=var,
             y=exp(Estimate),
             ymin=exp(Estimate-1.96*Std..Error),
             ymax=exp(Estimate+1.96*Std..Error)))+
    geom_pointrange()+
    theme_bw()+
    xlab("")+ylab(paste("OR for missing",varbl,"value"))+
    geom_hline(yintercept = 1)+
    scale_y_log10()+
    coord_flip()+
    theme(text=element_text(size=16),
          axis.text.y = element_text(angle = 45))
}
