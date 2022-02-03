# Fitting thermal performance curves

setwd("C:/Users/Invunche/Dropbox/GitHub/PhD_thesis_general_introduction")
#-----------------------------------------------------------------------------
# Libraries
library(nls.multstart)
# install package from GitHub
#remotes::install_github("padpadpadpad/rTPC")
library(rTPC)
#-----------------------------------------------------------------------------
#read data from:
#Mesas, Andres; Jaramillo, Angélica;Castañeda, Luis (2021), Experimental
#evolution on heat tolerance and thermal performance curves under contrasting
#thermal selection in Drosophila subobscura, Dryad, Dataset,
#https://doi.org/10.5061/dryad.f4qrfj6vm

dat<-read.csv("mesas_etal_data.csv")
d <- subset(dat, dat$selection == "control")

names(d)
#change names
d<-dplyr::rename(d,"rate"="climbing_speed")
d<-dplyr::rename(d,"temp"="temperature")
d <- aggregate(rate~temp,d,mean)

# get start values and fit model
start_vals <- get_start_vals(d$temp, d$rate, model_name = 'rezende_2019')
start_vals
# fit model
mod <- nls.multstart::nls_multstart(rate~rezende_2019(temp = temp,q10,a,b,c),
data = d,
iter = c(4,4,4,4),
start_lower = start_vals - 10,
start_upper = start_vals + 10,
lower = get_lower_lims(d$temp, d$rate, model_name = 'rezende_2019'),
upper = get_upper_lims(d$temp, d$rate, model_name = 'rezende_2019'),
supp_errors = 'Y',
convergence_count = FALSE)

# look at model fit
summary(mod)

# get predictions
preds <- data.frame(temp = seq(-20, max(d$temp), length.out = 100))
preds <- broom::augment(mod, newdata = preds)

# plot
{
pdf("Figure 1.1A Thermal performance curve fitted with data of Mesas et al 2021.pdf",
    width = 4.5,height = 4.5,useDingbats = FALSE)
plot(preds$temp,preds$.fitted,type = "l", lty = 1,lwd=3,xaxt="n",
     yaxt="n",
     ylab="Performance", xlab="Body temperature")
abline(v=28.3,lty=2)
axis(1,at = seq(-20,100,10), labels = F, tick = TRUE)
axis(2,at = seq(0,1,0.2), labels = F, tick = TRUE)
legend("topleft","A",bty = "n",cex=1.4)
dev.off()
}
