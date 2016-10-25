# Create some plots based on the output of the joint model runs

# GOAL: try and answer the key question - do dairy farms have more flocking (i.e. more dispersion) in winter?

# Clear workspace
rm(list=ls())

# Set the working directory
#setwd("~/github/birds_od")

# Load in relevant packages
library(reshape2)
library(ggplot2)
library(car)
library(plyr)
library(viridis)
library(GGally)

# Load in the data
load(file='birds.rda')

# Plot requirements
plot_dims = c(8,4) # This is the width and heigth at which to save all plots

##########################

# First just create a plot of the two response variables to show the over-dispersion
b_m = melt(birds[,c('Abundance','Farmland_i','System','Season')])
b_m$variable = revalue(b_m$variable, c("Farmland_i"='Farmland indicator\nspecies abundance','Abundance'="Total bird\nabundance"))

# First plot is over system
f1 = ggplot(b_m,aes(x=System,y=value,fill=System)) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0)) +
  theme(legend.position='None') +
  ylab('Count') +
  ylim(0,250) +
  facet_grid(variable ~ ., scales = "free") +
  coord_flip()
print(f1)
ggsave(f1,file=paste0('count_system_boxplot.pdf'),width=plot_dims[1],height=plot_dims[2])

# Next plot is over Season
f2 = ggplot(b_m,aes(x=Season,y=value,fill=Season)) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0)) +
  theme(legend.position='None') +
  ylab('Count') +
  ylim(0,250) +
  facet_grid(variable ~ ., scales = "free") +
  coord_flip()
print(f2)
ggsave(f2,file=paste0('count_season_boxplot.pdf'),width=plot_dims[1],height=plot_dims[2])

# Finally a scatter plot of response count vs duration
df = with(birds,data.frame(Duration=rep(Duration,2),value=c(Abundance,Farmland_i),Variable=b_m$variable,System=b_m$System,Day=c(Day,Day)))
f3 = ggplot(df,aes(x=Duration,y=value,colour=System)) +
  geom_point(position=position_jitter(width=0.5), alpha=0.5) +
  stat_smooth(se=FALSE,method='lm') +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0)) +
  theme(legend.position='top') +
  ylab('Count') +
  facet_grid(Variable ~ ., scales = "free")
print(f3)
ggsave(f3,file=paste0('count_duration_scatter.pdf'),width=plot_dims[1],height=plot_dims[2])

# Have a quick look at day - almost totally unimportant
f4 = ggplot(df,aes(x=Day,y=value,colour=System)) +
  geom_point(position=position_jitter(width=0.5), alpha=0.5) +
  stat_smooth(se=FALSE) +#,method='lm') +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0)) +
  theme(legend.position='top') +
  ylab('Count') +
  facet_grid(Variable ~ ., scales = "free")
print(f4)

##########################

# Load in the parameters output
all_pars = vector('list',2)
for(i in 1:2) {
  all_pars[[i]] = read.csv(paste(colnames(birds)[i],'_pars.csv',sep=''))
  names(all_pars)[i] = paste(colnames(birds)[i])
}

# # Optionsally create a pairs plot of the first 11 parameters
# pm = ggpairs(all_pars[[1]][,c(3:5,7:9)])
# print(pm)
# pm = ggpairs(all_pars[[2]][,c(3:5,7:9)])
# print(pm)


##########################

# Perform some posterior predictive checking by simulating from the posterior distribution of the likelihood
y_pred = vector('list',2)
for(i in 1:2) {
  mu_cols = grep('mu\\.',colnames(all_pars[[i]]))
  mu_pars = all_pars[[i]][,mu_cols]
  phi_cols = grep('phi\\.',colnames(all_pars[[i]]))
  phi_pars = all_pars[[i]][,phi_cols]

  # Now simulate from the negative binomial distribution
  y_pred[[i]] = matrix(NA,ncol=ncol(mu_pars),nrow=nrow(mu_pars))
  for(j in 1:nrow(y_pred[[i]])) {
    for(k in 1:ncol(y_pred[[i]])) y_pred[[i]][j,k] = rnbinom(1,mu=exp(mu_pars[j,k]),size=phi_pars[j,k])
  }

  # Now plot column-wise means vs y values
  y_med = apply(y_pred[[i]],2,'median')
  df = data.frame(birds[,i],y_med)
  colnames(df) = c('Response','Median')

  p = ggplot(df,aes(x=Response,y=Median,colour=Response)) +
    geom_point() +
    theme_bw() +
    theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0)) +
    theme(legend.position='None') +
    ylab('Median\npredicted\ncount') +
    xlab('True count') +
    geom_abline(intercept = 0, slope = 1, colour='Red') +
    ggtitle(paste0('Posterior predictive comparison for ',colnames(birds)[i],'\n'))

  print(p)
  ggsave(p,file=paste0(colnames(birds)[i],'_posterior_predictive.pdf'),width=plot_dims[1],height=plot_dims[2])

  # Correlation between true and predictive
  print(with(df,cor(Response,Median)))

}

##########################

# Remember the mean of this distribution is mu and the variance is mu+mu^2/phi
# So there will be lots of parameters which contribute to increase variability

# So plot beta_day, beta_winter and beta_day_winter on same plot
mean_pars = vector('list',6)
names(mean_pars) = outer(c('Abundance','Farmland indicator'),c(' - dairy',' - winter',' - dairy*winter'),FUN=paste0)

count = 1
for(i in 1:2) {
  mean_pars[[count]] = exp(all_pars[[i]]$beta_dairy)
  mean_pars[[count+1]] = exp(all_pars[[i]]$beta_winter)
  mean_pars[[count+2]] = exp(all_pars[[i]]$beta_dairy_winter)
  count = count+3
}

# Create some CIs
q_round = function(x) round(quantile(x,probs=c(0.025,0.975)),2)
print(lapply(mean_pars,'q_round'))

# Convert to appropriate df
df = melt(mean_pars)
df$response = unlist(lapply(strsplit(df$L1,' - '),'[',1))
df$response2 = unlist(lapply(strsplit(df$L1,' - '),'[',2))
names(df) = c('value','fullname','response','response2')
df$response2 = factor(df$response2,levels=c('dairy*winter','dairy','winter'))
df$response = revalue(df$response, c("Farmland indicator"='Farmland indicator\nspecies abundance','Abundance'="Total bird\nabundance"))


# Plot
df$response = factor(df$response, 
                    levels = c("Total bird\nabundance", 
                               "Farmland indicator\nspecies abundance"))
p2 = ggplot(df,aes(x=response2,y=value,fill=response)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette="Dark2") +
  theme_bw() +
  xlab('') +
  ylab('Effect size') +
  theme(legend.position='None') +
  ggtitle('Mean effect of dairy and winter on response variables') +
  facet_grid(response ~ ., scales = "free_x") +
  theme(axis.title.y = element_text(angle = 0, hjust = 0,vjust=1)) +
  ylim(0,4) +
  coord_flip() +
  geom_hline(aes(yintercept=1))
print(p2)
ggsave(p2,file='mean_effect.pdf',width=plot_dims[1],height=plot_dims[2])

##########################

# Now create the same plot again but for OD
od_pars = vector('list',6)
names(od_pars) = outer(c('Abundance','Farmland indicator'),c(' - dairy',' - winter',' - dairy*winter'),FUN=paste0)
count = 1
for(i in 1:2) {
  od_pars[[count]] = exp(all_pars[[i]]$theta_dairy)
  od_pars[[count+1]] = exp(all_pars[[i]]$theta_winter)
  od_pars[[count+2]] = exp(all_pars[[i]]$theta_dairy_winter)
  count = count+3
}

# Create some CIs
q_round = function(x) round(quantile(x,probs=c(0.025,0.975)),2)
print(lapply(od_pars,'q_round'))


# Convert to appropriate df
df = melt(od_pars)
df$response = unlist(lapply(strsplit(df$L1,' - '),'[',1))
df$response2 = unlist(lapply(strsplit(df$L1,' - '),'[',2))
names(df) = c('value','fullname','response','response2')
df$response2 = factor(df$response2,levels=c('dairy*winter','dairy','winter'))
df$response = revalue(df$response, c("Farmland indicator"='Farmland indicator\nspecies abundance','Abundance'="Total bird\nabundance"))

# Plot
df$response = factor(df$response, 
                     levels = c("Total bird\nabundance", 
                                "Farmland indicator\nspecies abundance"))
p3 = ggplot(df,aes(x=response2,y=value,fill=response)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette="Dark2") +
  theme_bw()+ 
  xlab('') +
  ylab('Effect size') +
  theme(legend.position='None') +
  ggtitle('Effect of farm system and survey season on over-dispersion in bird counts\n(effect values indicate the extent of over-dispersion in counts data)') +
  facet_grid(response ~ ., scales = "free_x") +
  theme(axis.title.y = element_text(angle = 0, hjust = 0,vjust=1)) +
  #scale_y_continuous(breaks=seq(0,75,by=10)) +
  #ylim(0,100) +
  coord_flip() +
  scale_y_sqrt(limits=c(0,80),breaks=c(1,2,5,seq(10,80,by=10)))
  #geom_hline(aes(yintercept=0))
print(p3)
ggsave(p3,file='od_effect.pdf',width=plot_dims[1],height=plot_dims[2])

