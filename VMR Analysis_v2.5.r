##  Visual Motor Response analysis ver2.5 written in R by Shi Song Rong @ Massachusetts Eye and Ear  ##

require(xlsx)
require(ggplot2)

setwd("D:/VMR_temp_dir")
ExpID <- "ABTL_injection1_5dpf"
binAssignment.file <- "bin_assignment.csv"
vmr.files.combination <- c("ABTL_injection1_5dpf.csv")
grouping_files <- c("ABTL_injection1_5dpf_grouping.csv")
Groups <- c("Uninjected", "Injected")
remove.outliers <- F

#### FUNCTION1 ####
VMR.summary.plot <- function(x,ActivityVar,ExpID,pdfname,IfCombined=F) {
  if (!IfCombined) {
    y <- x[,c("time","TimeSecond","TimeGroup","RepeatGroup",ActivityVar,"Groups")]
    colnames(y) <- c("time","TimeSecond","TimeGroup","RepeatGroup","ActivityVar","Groups")
    
    pdf(file = paste("./",ExpID,"/",pdfname,'_SummaryData_Mean_Lined-Up.pdf',sep=""),
        bg = "white", family="Times", width = 9, height=6)
    print(
      ggplot(y, aes(x=TimeSecond,y=ActivityVar,color=Groups))+
        geom_line(size = 0.2)+
        scale_x_continuous(breaks = c(-30, -15, 0, 15, 30))+
        geom_vline(xintercept = 0, color="firebrick",linetype="dotted", size = 0.2)+
        ggtitle("Locomotion recorded with VMR instrument")+  # Title
        labs(x="Time centered about light change (second)",y="Activity")+
        facet_wrap(~RepeatGroup, ncol=3)+
        scale_colour_discrete("Group")+
        theme_bw()
    )
    dev.off()
    
    pdf(file = paste("./",ExpID,"/",pdfname,'_SummaryData_Mean_Smoothed.pdf',sep=""),
        bg = "white", family="Times", width = 9, height=6)
    print(
      ggplot(y, aes(x=TimeSecond,y=ActivityVar,color=Groups))+
        geom_point(size = 0.3)+
        scale_x_continuous(breaks = c(-30, -15, 0, 15, 30))+
        stat_smooth(method = "gam", formula=y~s(x,k=30), size=0.2)+
        geom_vline(xintercept = 0, color="firebrick", linetype="dotted", size = 0.2)+
        ggtitle("Locomotion recorded with VMR instrument")+  # Title
        labs(x="Time centered about light change (second)",y="Activity")+
        facet_wrap(~RepeatGroup, ncol=3)+
        scale_colour_discrete("Group")+
        theme_bw()
    )
    dev.off() 
    
    # plots by repeat: the differences between repeats
    for (i in Groups) {
      y <- x[x$Groups==i,c("time","TimeSecond","TimeGroup","StimulusGroup","RepeatGroup",ActivityVar,"Groups")]
      colnames(y) <- c("time","TimeSecond","TimeGroup","StimulusGroup","RepeatGroup","ActivityVar","Groups")
      
      pdf(file = paste("./",ExpID,"/",pdfname,'_SummaryData_Mean_CompareRepeats_',i,'Lined-Up.pdf',sep=""),
          bg = "white", family="Times", width = 9, height=6)
      print(
        
        ggplot(y, aes(x=TimeSecond,y=ActivityVar,color=TimeGroup))+
          geom_point(size = 0.3)+
          geom_line()+
          scale_x_continuous(breaks = c(-30, -15, 0, 15, 30))+
          geom_vline(xintercept = 0, color="firebrick", linetype="dotted", size = 0.2)+
          ggtitle("Locomotion recorded with VMR instrument")+  # Title
          labs(x="Time centered about light change (second)",y="Activity")+
          facet_wrap(~StimulusGroup, nrow = 2)+
          scale_colour_discrete("Repeat group")+
          theme_bw()
      )
      
      dev.off() 
    }
  }
  
  if (IfCombined) {
    y <- x[,c("time","Groups","StimulusGroup",ActivityVar)]
    colnames(y) <- c("time","Groups","StimulusGroup","ActivityVar")
    
    pdf(file = paste("./",ExpID,"/",pdfname,'_SummaryDataCombined_Mean_Lined-Up.pdf',sep=""),
        bg = "white", family="Times", width = 8, height=6)
    print(
      ggplot(y, aes(x=time, y=ActivityVar, color=Groups))+
        geom_line(size = 0.2)+
        scale_x_continuous(breaks = c(-30, -15, 0, 15, 30))+
        geom_vline(xintercept = 0, color="firebrick",linetype="dotted", size = 0.2)+
        # geom_vline(xintercept = -1, color="red", linetype="dotted",size = 0.5)+
        # geom_vline(xintercept = 1, color="red", linetype="dotted", size = 0.5)+
        ggtitle("Locomotion recorded with VMR instrument")+  # Title
        labs(x="Time centered at light change (second)",y="Activity")+
        facet_wrap(~StimulusGroup, ncol=1)+
        scale_colour_discrete("Groups")+
        theme_bw()
    )
    dev.off()
  }

}

#### FUNCTION2 ####
VMR.plot <- function(x,ActivityVar,ExpID,pdfname) {
  y <- x[,c("time","TimeSecond","TimeGroup","RepeatGroup",ActivityVar,"Groups","exp_animal")]
  colnames(y) <- c("time","TimeSecond","TimeGroup","RepeatGroup","ActivityVar","Groups","exp_animal")
  
  pdf(file = paste("./",ExpID,"/",pdfname,'_RawData_ScatterPlot.pdf',sep=""),
      bg = "white", family="Times", width = 9, height=6)
  print(
    ggplot(y, aes(x=time,y=ActivityVar,color=Groups))+
      geom_point(aes(group = exp_animal), size = 0.1)+
      stat_summary(aes(y = ActivityVar), fun.y=mean, geom="line", size=0.3)+
      geom_vline(xintercept = 300, color="firebrick", linetype="dotted", size = 0.3)+
      geom_vline(xintercept = 2100, color="firebrick", linetype="dotted", size = 0.3)+
      geom_vline(xintercept = 3900, color="firebrick", linetype="dotted", size = 0.3)+
      geom_vline(xintercept = 5700, color="firebrick", linetype="dotted", size = 0.3)+
      geom_vline(xintercept = 7500, color="firebrick", linetype="dotted", size = 0.3)+
      geom_vline(xintercept = 9300, color="firebrick", linetype="dotted", size = 0.3)+
      ggtitle("Locomotion recorded with VMR instrument")+  # Title
      labs(x="Time (second)",y="Activity")+
      facet_wrap(~Groups, ncol=1, scales="free")+
      scale_colour_discrete("Group")+
      theme_bw()
  )
  dev.off()
  
  pdf(file = paste("./",ExpID,"/",pdfname,'_RawData_AveragedLines.pdf',sep=""),
      bg = "white", family="Times", width = 9, height=6)
  print(
    ggplot(y, aes(x=TimeSecond,y=ActivityVar,color=Groups))+
      geom_point(aes(group = exp_animal), size = 0.1)+
      stat_summary(aes(y = ActivityVar), fun.y=mean, geom="line", size=0.3)+
      scale_x_continuous(breaks = c(-30, -15, 0, 15, 30))+
      
      geom_vline(xintercept = 0, color="firebrick", linetype="dotted", size = 0.3)+
      ggtitle("Locomotion recorded with VMR instrument")+  # Title
      labs(x="Time (second)",y="Activity")+
      facet_wrap(~RepeatGroup, ncol=3)+
      theme_bw()
  )
  dev.off()
}

#### FUNCTION3 ####
VMR.test <- function(x,ActivityVar,ExpID){

  var_LightGroup <- c('Light-On 1.Off','Light-On 1.On',
                      'Light-Off 1.On','Light-Off 1.Off',
                      'Light-On 2.Off','Light-On 2.On',
                      'Light-Off 2.On','Light-Off 2.Off',
                      'Light-On 3.Off','Light-On 3.On',
                      'Light-Off 3.On','Light-Off 3.Off',
                      'Light-On 4.Off','Light-On 4.On',
                      'Light-Off 4.On','Light-Off 4.Off')
  aov_results <- data.frame(LightGroup=as.character(),P_ANOVA=as.numeric(), P_KWrank=as.numeric())
  TukeyHSD_results <- data.frame(LightGroup=as.character(),group=as.character(), diff=as.numeric(), lwr=as.numeric(), upr=as.numeric(), p.adj=as.numeric())

  for (i in 1:length(var_LightGroup)) {
    temp <- summary_activity[summary_activity$LightGroup==var_LightGroup[i],]
    cat(paste("ANOVA and Kruskal-Wallis tests: ", ActivityVar," ~ Groups at ", var_LightGroup[i], sep=""),"...")
    f <- as.formula(paste(ActivityVar," ~ Groups", sep=""))
    fit <- aov(f, data=temp)
    fit_KWrank <- kruskal.test(f,data=temp)

    if (nrow(aov_results)>0 & !is.null(summary(fit)[[1]][["Pr(>F)"]][[1]])) { aov_results <- rbind(aov_results, data.frame(LightGroup=var_LightGroup[i],P_ANOVA=summary(fit)[[1]][["Pr(>F)"]][[1]],P_KWrank=fit_KWrank$p.value))}
    if (nrow(aov_results)>0 & is.null(summary(fit)[[1]][["Pr(>F)"]][[1]])) { aov_results <- rbind(aov_results, data.frame(LightGroup=var_LightGroup[i],P_ANOVA=NA,P_KWrank=NA)) }

    if (nrow(aov_results)==0 & !is.null(summary(fit)[[1]][["Pr(>F)"]][[1]])) { aov_results <- data.frame(LightGroup=var_LightGroup[i],P_ANOVA=summary(fit)[[1]][["Pr(>F)"]][[1]],P_KWrank=fit_KWrank$p.value) }
    if (nrow(aov_results)==0 & is.null(summary(fit)[[1]][["Pr(>F)"]][[1]])) { aov_results <- data.frame(LightGroup=var_LightGroup[i],P_ANOVA=NA,P_KWrank=NA) }

    cat(paste("TukeyHSD tests: ", ActivityVar," ~ Groups at ", var_LightGroup[i],sep=""),"...")

    b <- TukeyHSD(fit)
    b <- data.frame(b["Groups"])
    colnames(b) <- c("diff", "lwr", "upr","p.adj")
    b$LightGroup <- var_LightGroup[i]
    b$group <- row.names(b)
    b <- b[,c( "LightGroup", "group", "diff", "lwr", "upr", "p.adj")]

    if (nrow(TukeyHSD_results)>0) { TukeyHSD_results <- rbind(TukeyHSD_results, b) }
    if (nrow(TukeyHSD_results)==0) { TukeyHSD_results <- b }

    cat("done.","\n")
  }
  write.xlsx(aov_results, file = paste("./",ExpID,"/",ExpID,'_ANOVA-KWrank.xlsx',sep=""),row.names = F)
  write.xlsx(TukeyHSD_results, file = paste("./",ExpID,"/",ExpID,'_TukeyHSD.xlsx',sep=""),row.names = F)
}

# Load VMR datasets
for (k in vmr.files.combination) {
  cat(paste("Recoding " , k, "\n", sep=""))
  vmr.rawdata <- read.csv(file=paste("./", ExpID, "/", k, sep=""), stringsAsFactors = F)
  
  vmr.easyraw <- vmr.rawdata[vmr.rawdata$activ != 1,c("time","animal","activ")]
  time_end <- as.integer(max(vmr.rawdata$time))
  
  vmr.fraction <- data.frame(time=as.numeric(),animal=as.numeric(),activity=as.numeric())
  tmp_fraction <- data.frame(time=as.numeric(),animal=as.numeric(),activity=as.numeric())
  for (j in 0:95) {
    cat( "Animal: ", j, "\n", sep="")
    tmp_well <- vmr.easyraw[vmr.easyraw$animal == j, ]
    for (i in 1:time_end) {
      a <- i-1
      tmp <- tmp_well[(tmp_well$time <=i) & (tmp_well$time > a),]
      b <- nrow(tmp[,])
      if (b==0) {
        tmp_fraction <- rbind(tmp_fraction, data.frame(time=i, animal=j, activity=0))
      } else {
        tmp_fraction <- rbind(tmp_fraction, data.frame(time=i, animal=j, activity=b/30))
      }
    }
    if (nrow(vmr.fraction)>0) {vmr.fraction <- rbind(vmr.fraction, tmp_fraction)}
    if (nrow(vmr.fraction)==0) {vmr.fraction <- tmp_fraction}
    tmp_fraction <- data.frame(time=as.numeric(),animal=as.numeric(),activity=as.numeric())
  }
  write.csv(vmr.fraction, file = paste("./", ExpID, "/", gsub(".CSV", "_vmr.fraction.csv",k,ignore.case = T), sep=""), row.names = F)
}

for (i in 1:length(vmr.files.combination)) {
  cat("Loading in ", paste("./", ExpID, "/", gsub(".CSV", "_vmr.fraction.csv",vmr.files.combination[i],ignore.case = T), sep=""), "\n")
  if ( i == 1 ) { 
    tmp <- read.csv(file=paste("./", ExpID, "/", gsub(".CSV", "_vmr.fraction.csv",vmr.files.combination[i],ignore.case = T), sep=""), stringsAsFactors = F) 
    tmp$exp_animal <- unlist(lapply(tmp[,c("animal")], paste,vmr.files.combination[i], sep="_"))
  } else { }
  if ( i > 1 ) { 
    a <- read.csv(file=paste("./", ExpID, "/", gsub(".CSV", "_vmr.fraction.csv",vmr.files.combination[i],ignore.case = T), sep=""), stringsAsFactors = F)
    a$exp_animal <- unlist(lapply(a[,c("animal")], paste,vmr.files.combination[i], sep="_"))
    tmp <- rbind(tmp, a)
  } else { }
}
vmr.fraction <- tmp
tmp <- NULL; a <- NULL

for (i in 1:length(grouping_files)) {
  cat("Loading in ", grouping_files[i], "\n")
  if ( i == 1 ) { 
    tmp <- read.csv(file=paste("./", ExpID, "/", grouping_files[i], sep=""), stringsAsFactors = F) 
    tmp$exp_animal <- unlist(lapply(tmp[,c("animal")], paste, vmr.files.combination[i], sep="_"))
  } else { }
  if ( i > 1 ) { 
    a <- read.csv(file=paste("./", ExpID, "/", grouping_files[i], sep=""), stringsAsFactors = F)
    a$exp_animal <- unlist(lapply(a[,c("animal")], paste, vmr.files.combination[i], sep="_"))
    tmp <- rbind(tmp, a)
  } else { }
}
grouping <- tmp
tmp <- NULL; a <- NULL

binAssignment <- read.csv(file=paste("./", ExpID, "/", binAssignment.file, sep=""), stringsAsFactors = F)

vmr.fraction <- merge(vmr.fraction, grouping, by = "exp_animal", sort = F)
vmr.fraction <- vmr.fraction[vmr.fraction$Groups!="",]
vmr.fraction <- vmr.fraction[vmr.fraction$Groups %in% Groups,]
vmr.fraction <- merge(vmr.fraction, binAssignment, by = "time", sort = F)
vmr.fraction <- vmr.fraction[vmr.fraction$StimulusGroup != "",]

# Summary_activity
ActivityVar <- c("activity")
summary_activity <- NULL
a <- NULL
for (i in ActivityVar){
  for (j in sort(unique(vmr.fraction$time))) {
    for (k in Groups) {
      cat(i,"-",j,"-",k,"\n")
      if (length(a)>0) {
        a <- rbind(a, data.frame(time=as.numeric(j),Groups=as.character(k), 
                                 Activity=as.numeric(mean(vmr.fraction[vmr.fraction$time == j & vmr.fraction$Groups == k, i])),
                                 sd=sd(vmr.fraction[vmr.fraction$time == j & vmr.fraction$Groups == k, i])))
      }
      if (length(a)==0) {
        a <- data.frame(time=as.numeric(j), Groups=as.character(k), 
                        Activity=as.numeric(mean(vmr.fraction[vmr.fraction$time == j & vmr.fraction$Groups == k, i])),
                        sd=sd(vmr.fraction[vmr.fraction$time == j & vmr.fraction$Groups == k, i]))
      }
    }
  }
  colnames(a) <- c("time","Groups", i, paste(i,"_SD",sep=""))
  if (length(summary_activity)>0) {
    summary_activity <- cbind(summary_activity,a[,c(i,paste(i,"_SD",sep=""))])
  }
  
  if (length(summary_activity)==0) {
    summary_activity <- a
  }
  a <- NULL
}
colnames(summary_activity) <- c("time","Groups","Activity", "Activity_SD")
summary_activity <- merge(summary_activity, binAssignment, by = "time", sort = F)

ActivityVar <- c("activity")
summary_activity_combined <- NULL
a <- NULL
for (i in ActivityVar){
  for (j in sort(unique(vmr.fraction$TimeSecond))) {
    for (k in Groups) {
      cat(i,"-",j,"-",k,"\n")
      if (length(a)>0) {
        a <- rbind(a, data.frame(time=as.numeric(j),Groups=as.character(k), StimulusGroup="Light-On",
                                 Activity=as.numeric(mean(vmr.fraction[vmr.fraction$TimeSecond == j & vmr.fraction$Groups == k & vmr.fraction$StimulusGroup == "Light-On", i])),
                                 sd=sd(vmr.fraction[vmr.fraction$TimeSecond == j & vmr.fraction$Groups == k, i])))
        a <- rbind(a, data.frame(time=as.numeric(j),Groups=as.character(k), StimulusGroup="Light-Off",
                                 Activity=as.numeric(mean(vmr.fraction[vmr.fraction$TimeSecond == j & vmr.fraction$Groups == k & vmr.fraction$StimulusGroup == "Light-Off", i])),
                                 sd=sd(vmr.fraction[vmr.fraction$TimeSecond == j & vmr.fraction$Groups == k, i])))
      }
      if (length(a)==0) {
        a <- data.frame(time=as.numeric(j), Groups=as.character(k), StimulusGroup="Light-On",
                        Activity=as.numeric(mean(vmr.fraction[vmr.fraction$TimeSecond == j & vmr.fraction$Groups == k & vmr.fraction$StimulusGroup == "Light-On", i])),
                        sd=sd(vmr.fraction[vmr.fraction$TimeSecond == j & vmr.fraction$Groups == k, i]))
        a <- rbind(a, data.frame(time=as.numeric(j),Groups=as.character(k), StimulusGroup="Light-Off",
                                 Activity=as.numeric(mean(vmr.fraction[vmr.fraction$TimeSecond == j & vmr.fraction$Groups == k & vmr.fraction$StimulusGroup == "Light-Off", i])),
                                 sd=sd(vmr.fraction[vmr.fraction$TimeSecond == j & vmr.fraction$Groups == k, i])))
      }
    }
  }
  colnames(a) <- c("time","Groups", "StimulusGroup",i, paste(i, "_SD", sep=""))
  if (length(summary_activity_combined)>0) {
    summary_activity_combined <- cbind(summary_activity_combined,a[,c(i,paste(i, "_SD", sep=""))])
  }
  
  if (length(summary_activity_combined)==0) {
    summary_activity_combined <- a
  }
  a <- NULL
}
colnames(summary_activity_combined) <- c("time","Groups","StimulusGroup","Activity", "Activity_SD")

numeric2factor <- function(x, DefinedLevels = c(0,1,2)) {factor(x, levels = DefinedLevels)}
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
as.charactor.factor <- function(x) {as.character(levels(x))[x]}

vmr.fraction$TimeGroup <- numeric2factor(vmr.fraction$TimeGroup,DefinedLevels=c('Repeat-1','Repeat-2','Repeat-3','Repeat-4'))
vmr.fraction$Groups <- numeric2factor(vmr.fraction$Groups,DefinedLevels=Groups)

summary_activity$TimeGroup <- numeric2factor(summary_activity$TimeGroup,DefinedLevels=c('Repeat-1','Repeat-2','Repeat-3','Repeat-4'))
summary_activity$Groups <- numeric2factor(summary_activity$Groups,DefinedLevels=Groups)

summary_activity_combined$Groups <- numeric2factor(summary_activity_combined$Groups,DefinedLevels=Groups)
summary_activity_combined$StimulusGroup <- as.charactor.factor(summary_activity_combined$StimulusGroup)

#### Analysis ####
VMR.summary.plot(x=summary_activity,ActivityVar="Activity",ExpID=ExpID,pdfname=ExpID)
VMR.summary.plot(x=summary_activity_combined,ActivityVar="Activity",ExpID=ExpID,pdfname=ExpID, IfCombined=T)
VMR.plot(x=vmr.fraction,ActivityVar="activity",ExpID=ExpID,pdfname=ExpID)
VMR.test(x=summary_activity,ActivityVar="Activity",ExpID=ExpID)
