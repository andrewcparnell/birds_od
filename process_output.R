# Create some plots based on the output of the JOINT model runs

# GOAL: try and answer the key question - do dairy farms have more flocking (i.e. more dispersion) in winter?

# Clear workspace
rm(list=ls())

# Set the working directory
setwd("~/transfer/McMahon_20140829/new_models_20151001")

# Load in relevant packages
library(reshape2)
library(ggplot2)
library(car)

# Load in the data
load(file='data_all.rda')

# Load in the output
all_pars = vector('list',3)
for(i in 1:3) {
  all_pars[[i]] = read.csv(paste(colnames(data_all)[i],'_joint_pars.csv',sep=''))
  names(all_pars)[i] = paste(colnames(data_all)[i])    
}

##########################

# Remember the mean of this distribution is mu and the variance is mu+mu^2/phi
# So there will be lots of parameters which contribute to increase variability

# So plot beta_day, beta_winter and beta_day_winter on same plot
mean_pars = vector('list',9)
names(mean_pars) = outer(c('Abundance','Farmland indicator','Invertebrates'),c(' - dairy',' - winter',' - dairy*winter'),FUN=paste0)
count = 1
for(i in 1:3) {
  mean_pars[[count]] = all_pars[[i]]$beta_dairy
  mean_pars[[count+1]] = all_pars[[i]]$beta_winter
  mean_pars[[count+2]] = all_pars[[i]]$beta_dairy_winter
  count = count+3
}  
mean_pars= lapply(mean_pars,'[',1001:2000) # Weird burn-in on one of the

# Convert to appropriate df
df = melt(mean_pars)
df$response = unlist(lapply(strsplit(df$L1,' - '),'[',1))
df$response2 = unlist(lapply(strsplit(df$L1,' - '),'[',2))
names(df) = c('value','fullname','response','response2')
df$response2 = factor(df$response2,levels=c('dairy*winter','dairy','winter'))

# Plot
ggplot(df,aes(x=response2,y=value,fill=response))+geom_boxplot()+theme_bw()+xlab('')+ylab('Effect size') + theme(legend.position='None')+ggtitle('Mean effect of dairy and winter on response variables')+ facet_grid(response ~ ., scales = "free_x")+theme(axis.title.y = element_text(angle = 0, hjust = 0,vjust=1))+scale_y_continuous(breaks=seq(-1,2,by=0.5))+coord_flip()+geom_hline(aes(yintercept=0))
ggsave('new_Fig1.pdf',width=8,height=6)

##########################

# Now create the same plot again but for OD
od_pars = vector('list',9)
names(od_pars) = outer(c('Abundance','Farmland indicator','Invertebrates'),c(' - dairy',' - winter',' - dairy*winter'),FUN=paste0)
count = 1
for(i in 1:3) {
  od_pars[[count]] = all_pars[[i]]$phi_dairy
  od_pars[[count+1]] = all_pars[[i]]$phi_winter
  od_pars[[count+2]] = all_pars[[i]]$phi_dairy_winter
  count = count+3
}  
od_pars= lapply(od_pars,'[',1001:2000) # Weird burn-in on one of the

# Convert to appropriate df
df = melt(od_pars)
df$response = unlist(lapply(strsplit(df$L1,' - '),'[',1))
df$response2 = unlist(lapply(strsplit(df$L1,' - '),'[',2))
names(df) = c('value','fullname','response','response2')
df$response2 = factor(df$response2,levels=c('dairy*winter','dairy','winter'))

# Plot
ggplot(df,aes(x=response2,y=value,fill=response))+geom_boxplot()+theme_bw()+xlab('')+ylab('Effect size') + theme(legend.position='None')+ggtitle('Over-dispersion effect of dairy and winter on response variables\n(higher values mean less dispersion)')+ facet_grid(response ~ ., scales = "free_x")+theme(axis.title.y = element_text(angle = 0, hjust = 0,vjust=1))+scale_y_continuous(breaks=seq(-5,5,by=1))+coord_flip()+geom_hline(aes(yintercept=0))
ggsave('new_Fig2.pdf',width=8,height=6)

########
# Old code

# # First let's get a plot of just the phi_dairy_winter parameter - the interaction between season and dairy
# od_pars = vector('list',3)
# names(od_pars) = names(all_pars)
# for(i in 1:3) od_pars[[i]] = all_pars[[i]]$phi_dairy_winter
# od_pars= lapply(od_pars,'[',1001:2000) # Weird burn-in on one of the
# 
# # Sort into a decent data set of ggplot
# names(od_pars) = c("Abundance","Farmland indicator","Invertebrates")
# df = melt(od_pars)
# names(df) = c('value','response')
# 
# # Create a plot
# ggplot(df,aes(x=response,y=value,fill=response))+geom_boxplot()+theme_bw()+coord_flip()+xlab('')+ylab('Over-dispersion effect of winter on dairy farms') + theme(legend.position='None')+ggtitle('Over-dispersion effect of winter on dairy farms (higher values mean less dispersion)')
# 
# + scale_y_continuous(breaks=seq(-4,2,by=1),limits=c(-4,2))
# ggsave('new_Fig1.pdf',width=8,height=6)

##########################

# To answer this question what you really want to observe is the joint effect of dairy/winter on dispersion in the model - running the whole lot together




