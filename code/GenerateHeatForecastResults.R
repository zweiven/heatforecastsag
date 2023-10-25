library(data.table)
library(maps)
library(here)
library(fixest)
library(ggplot2)
library(cowplot)
library(kableExtra)

# Load the estimation data
est.data = fread(here("data", "heatforecasts_us_corn.csv"))

# What lead times will we use
lead.times = 1:7

#####################################
#
# Main text figures, tables, stats
#
#####################################


##################
# Figure 1: forecast quality
##################

# Panel a: map
fips.dt = data.table(maps::county.fips)
fips.dt[,c("state","county"):=tstrsplit(polyname, ",")]

fips.geo=merge(fips.dt, 
               map_data("county"), 
               by.x=c("state", "county"), 
               by.y=c("region","subregion"))
avg.fcast.quality.dt = unique(est.data[,.(fips, fcast.stat=avg.frac.edd.sin.pred.3d)])

plot.geo = merge(fips.geo, 
                 avg.fcast.quality.dt,
                 by="fips", 
                 all.x=T)
map.p = ggplot(plot.geo,
               aes(long, lat, group = group)) +
  geom_polygon(aes(fill=fcast.stat*100)) +
  coord_map(projection = "albers", 
            parameters = c(30,39),
            xlim=c(-120,-72)) + 
  scale_fill_viridis_c(option = "A",
                       name="% EDD correctly \n predicted (3-day lead)",
                       na.value = "#EFEFEF",
                       limits=c(0,100)) +
  theme_minimal() +
  theme(panel.grid=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.2,0.16),
        legend.direction = "horizontal",
        plot.margin=unit(c(0, 0, 0, 0), units="cm"))+#"right") + 
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0.5, barwidth = 8)
         )

# Panel b: time trends in forecast quality, both county-specific and aggregate
agg.pred = est.data[,.(frac.edd.sin.pred.3d=mean(frac.edd.sin.pred.3d, na.rm=T)), by=yr]

# example counties: those with records every year and highest yield
avg.yield=est.data[,.(avg.crop.yield=mean(crop.yield, na.rm=T), n.recs=.N), by=fips]
setorder(avg.yield, -avg.crop.yield)
top.counties=avg.yield[n.recs==14][1:2,fips] # Phelps County, NE; Calhoun County, GA
top.county.pred=est.data[fips %in% top.counties,.(fips, yr,frac.edd.sin.pred.3d)]

ts.p = ggplot(est.data, aes(x=yr, y=frac.edd.sin.pred.3d*100)) + 
  geom_line(aes(group=fips, color="All counties")) + #color="#efefef"
  geom_line(data=top.county.pred, aes(group=fips, color="Example counties")) + #, color="#444444") + 
  geom_line(data=agg.pred, aes(color="National")) + #color="red"  
  scale_x_continuous(breaks=2008:2021, expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  scale_color_manual(values=c("All counties"="#EFEFEF", 
                              "Example counties"="#444444",
                              "National"="#FF0000"
                              )) + 
  xlab("Year") + 
  ylab("% EDD correctly \npredicted (3-day lead)") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.line = element_line(size=1),
        axis.text.x = element_text(angle=45, vjust = 0.6),
        legend.position = c(0.7, 0.24),
        legend.background = element_rect(fill="#FFFFFF"),
        legend.title=element_blank(),
        legend.margin = margin(0, 2, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        plot.margin = unit(c(0.1,1,0.1,1), units="cm")) + 
  guides(color = guide_legend(override.aes = list(size = 2)))
  

# panel c: within-year variation in both incidence and forecast of extreme heat
mo.pred = est.data[,.(edd.sin.mo.4=mean(edd.sin.mo.4, na.rm=T),
                      edd.sin.mo.5=mean(edd.sin.mo.5, na.rm=T),
                      edd.sin.mo.6=mean(edd.sin.mo.6, na.rm=T),
                      edd.sin.mo.7=mean(edd.sin.mo.7, na.rm=T),
                      edd.sin.mo.8=mean(edd.sin.mo.8, na.rm=T),
                      edd.sin.mo.9=mean(edd.sin.mo.9, na.rm=T),
                      edd.sin.pred.3d.mo.4=mean(edd.sin.pred.3d.mo.4, na.rm=T),
                      edd.sin.pred.3d.mo.5=mean(edd.sin.pred.3d.mo.5, na.rm=T),
                      edd.sin.pred.3d.mo.6=mean(edd.sin.pred.3d.mo.6, na.rm=T),
                      edd.sin.pred.3d.mo.7=mean(edd.sin.pred.3d.mo.7, na.rm=T),
                      edd.sin.pred.3d.mo.8=mean(edd.sin.pred.3d.mo.8, na.rm=T),
                      edd.sin.pred.3d.mo.9=mean(edd.sin.pred.3d.mo.9, na.rm=T)), by=fips]


mo.pred.long = melt(est.data, id.vars = "fips", 
                    measure.vars=list("edd"=paste0("edd.sin.mo.",4:9),
                                      "edd.pred"=paste0("edd.sin.pred.3d.mo.",4:9)),
                    variable.name = "month")
mo.pred.long[,month:=factor(x=month.name[(as.integer(month)+3)],
                            levels=month.name)]
mo.pred.long[,month.abbr:=factor(substr(month, 1, 3), 
                                 levels=substr(month.name, 1, 3))]
mo.pred.long[,edd.pred.frac:=edd.pred/edd]

# national edd incidence
edd.national = mo.pred.long[,.(edd=sum(edd, na.rm=T)), by = month]
max.edd=max(edd.national$edd)
mean.county.edd = mean(mo.pred.long$edd, na.rm=T)
vert.scale.sec.axis = (3*mean.county.edd)/100
edd.color = "darkred"
month.p = ggplot(mo.pred.long, aes(x=month.abbr,
                         y=edd.pred.frac*100, 
                         group=month)) + 
  geom_boxplot(position = position_nudge(x=-0.1),
               width = 0.1) + 
  geom_boxplot(aes(y=edd/vert.scale.sec.axis),
               color=edd.color,
               position = position_nudge(x=0.1),
               width = 0.1
               ) + 
  xlab("Month") + 
  ylab("% EDD correctly \npredicted  (3-day-lead)") + 
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,100),
                     sec.axis = sec_axis(trans = ~.*vert.scale.sec.axis,
                                         name = "EDD")) +
  theme_minimal() + 
  theme(panel.grid=element_blank(),
        axis.ticks = element_line(),
        axis.line = element_line(),
        axis.line.y.right = element_line(color = edd.color), 
        axis.ticks.y.right = element_line(color = edd.color),
        axis.text.y.right = element_text(color = edd.color),
        axis.title.y.right = element_text(color = edd.color),
        plot.margin = unit(c(0.1,0.1,0.1,1), units="cm"))

noleg = theme(legend.position="none",
              plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
map.leg = get_legend(map.p)
ts.leg = get_legend(ts.p)

plot_grid(map.p,
          plot_grid(month.p,
                    ts.p, 
                    nrow=2,
                    rel_heights = c(0.5,0.5),
                    labels=c("b","c")),
          ncol=2,
          labels = c("a",""),
          rel_widths = c(0.6, 0.4))
ggsave(here("results","figures","Fig1.eps"),
       width=9,
       height=5,
       units="in")

###############
# Summary stats in text
###############
nrow(est.data) # number of obs
length(unique(est.data$fips)) # number of counties
length(unique(est.data$yr)) # number of years
median(est.data$edd.sin) # median EDD exposure
mean(est.data$crop.yield) # average yield




###############
# Figure 2: Baseline results
###############

# Baseline results, accounting for interactions with long-run average T and
# deviation of the current annual average temp from that long-run temperature

baseline.res = lapply(lead.times, FUN=function(lead) {
  lead.frm = as.formula(paste0("log(crop.yield) ~ edd.sin.silking + edd.sin.silking:avg.tmax.longrun.m26 + edd.sin.notsilking + edd.sin.notsilking:avg.tmax.longrun.m26  + edd.sin.silking:avg.tmax.deviation + edd.sin.notsilking:avg.tmax.deviation + edd.sin.pred.",lead,"d.silking + edd.sin.pred.",lead,"d.notsilking +  avg.tmax.silking + avg.tmax.silking.sq + avg.tmax.notsilking + avg.tmax.notsilking.sq + totprecip.silking + totprecip.silking.sq + totprecip.notsilking + totprecip.notsilking.sq | fips[study.yr] + state.yr"))
  tmp.mod = feols(lead.frm,
                  data=est.data,
                  cluster = c("fips", "state.yr"))
  tmp.mod
})




baseline.res.dt = rbindlist(lapply(lead.times, FUN=function(lead) {
  data.table(est=coeftable(baseline.res[[lead]])[c(1:4),1],
             se=coeftable(baseline.res[[lead]])[c(1:4),2],
             p=coeftable(baseline.res[[lead]])[c(1:4),4],
             lead=lead,
             coef=c("edd.silking", "edd.notsilking", "edd.pred.silking", "edd.pred.nosilking"),
             coef.type=c("edd","edd", "edd.pred", "edd.pred"),
             timing=c("Silking", "Not silking", "Silking", "Not silking"))
}))

baseline.plot = ggplot(baseline.res.dt, 
       aes(x=lead, 
           y=est,
           group=paste0(coef, lead),
           color=coef.type,
           shape=timing
       )) + 
  geom_point(position=position_dodge2(width=0.3),
             size=3) + 
  geom_errorbar(aes(ymin=est-1.96*se,
                    ymax=est+1.96*se), 
                width=0.3,
                size=1.2,
                position=position_dodge2(width=0.3)) +
  xlab("Forecast Lead (days)") + 
  ylab("Effect on yield (log points)") + 
  scale_x_continuous(breaks=1:7) + 
  scale_shape_discrete(name="Crop Stage") + 
  scale_color_manual(name="Effect",
                     values = c("#777777", "#01796F"),
                     labels=c("Extreme Degree Day (EDD)",
                              "Prediction Benefits (per EDD)")) + 
  geom_hline(yintercept = 0) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.line.y = element_line(size=1))


ggsave(here("results","figures","Fig2.eps"),
       width=8,
       height=4,
       units="in")



#####################
# Fig 3: East-West (cutoff: 100W) breakdown
#####################
ew.res = lapply(lead.times, FUN=function(lead) {
  lead.frm = as.formula(paste0("log(crop.yield) ~ edd.sin.silking + edd.sin.silking:avg.tmax.longrun.m26 + edd.sin.notsilking + edd.sin.notsilking:avg.tmax.longrun.m26  + edd.sin.silking:avg.tmax.deviation + edd.sin.notsilking:avg.tmax.deviation + edd.sin.pred.",lead,"d.silking + edd.sin.pred.",lead,"d.notsilking +  avg.tmax.silking + avg.tmax.silking.sq + avg.tmax.notsilking + avg.tmax.notsilking.sq + totprecip.silking + totprecip.silking.sq + totprecip.notsilking + totprecip.notsilking.sq | fips[study.yr] + state.yr"))
  tmp.mod = feols(lead.frm,
                  data=est.data,
                  cluster = c("fips", "state.yr"), 
                  split = ~e100)
  tmp.mod
})
ew.res.dt = rbindlist(lapply(lead.times, FUN=function(lead) {
  rbind(data.table(est=coeftable(ew.res[[lead]]$`FALSE`)[c(1:4),1],
                   se=coeftable(ew.res[[lead]]$`FALSE`)[c(1:4),2],
                   p=coeftable(ew.res[[lead]]$`FALSE`)[c(1:4),4],
                   lead=lead,
                   coef=c("edd.silking", "edd.notsilking", "edd.pred.silking", "edd.pred.nosilking"),
                   coef.type=c("edd","edd", "edd.pred", "edd.pred"),
                   timing=c("Silking", "Not silking", "Silking", "Not silking"),
                   region="West"),
        data.table(est=coeftable(ew.res[[lead]]$`TRUE`)[c(1:4),1],
                   se=coeftable(ew.res[[lead]]$`TRUE`)[c(1:4),2],
                   p=coeftable(ew.res[[lead]]$`TRUE`)[c(1:4),4],
                   lead=lead,
                   coef=c("edd.silking", "edd.notsilking", "edd.pred.silking", "edd.pred.nosilking"),
                   coef.type=c("edd","edd", "edd.pred", "edd.pred"),
                   timing=c("Silking", "Not silking", "Silking", "Not silking"),
                   region="East"))
}))

ew.res.dt[, region:=factor(region, levels=c("West", "East"))]

ew.plot = ggplot(ew.res.dt, 
                 aes(x=lead, 
                     y=est,
                     group=paste0(coef, lead),
                     color=coef.type,
                     #shape=timing
                 )) + 
  geom_point(position=position_dodge2(width=0.3),
             size=3) + 
  geom_errorbar(aes(ymin=est-1.96*se,
                    ymax=est+1.96*se), 
                width=0.3,
                size=1.2,
                position=position_dodge2(width=0.3)) +
  xlab("Forecast Lead (days)") + 
  ylab("Effect on yield (log points)") + 
  scale_x_continuous(breaks=1:7) + 
  scale_shape_discrete(name="Crop Stage") + 
  scale_color_manual(name="Effect",
                     values = c("#777777", "#01796F"),
                     labels=c("Extreme Degree Day (EDD)",
                              "Prediction Benefits (per EDD)")) + 
  geom_hline(yintercept = 0) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.line.y = element_line(size=1),
        panel.spacing = unit(2, "lines"),
        strip.text=element_text(size=16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16)) + 
  facet_grid(timing ~ region)
ew.plot

ggsave(here("results","figures","Fig3.eps"),
       width=9,
       height=6,
       units="in")

# Is this big?
# Using % approximation to log-linear model, scale from yield % effects to production effects,
# summed over the 2008-2021 sample
potential.fcast.production.benefit = sum(est.data[e100==F,edd.sin.pred.7d.notsilking*crop.yield*acres.planted]*ew.res.dt[lead==7 & coef=="edd.pred.nosilking",est], na.rm=T)
# compared to total yield.
total.yield.west = sum(est.data[e100==F,crop.yield*acres.planted], na.rm=T)
total.yield.national = sum(est.data[,crop.yield*acres.planted], na.rm=T)
potential.fcast.production.benefit/total.yield.west
potential.fcast.production.benefit/total.yield.national



########################
# Figure 4: role of irrigation (suggestive analysis)
########################

# Might expect forecast response in areas with  moderate irrigation, as
# areas too dependent on irrigation might just be irrigating regularly and forecasts
# wouldn't change that decision. In other words, irrigation benefit in the absence
# of extreme heat should be marginal.

# Filter to NE, KS, which seem to have transition between irrigated & not.
# might expect effects in moderate irrigation levels
state.filt.data = est.data[state.id %in% c(20, 31),]
irr.interquartile = quantile(unique(state.filt.data[,.(fips, irr.ops.frac)])$irr.ops.frac, c(0.25, 0.75), na.rm=T)
state.filt.data[,is.irr.interquartile:=irr.ops.frac>irr.interquartile[1] & irr.ops.frac < irr.interquartile[2]]
irr.ops.frac.dt = unique(state.filt.data[,.(fips,  state.id, e100, irr.stat=irr.ops.frac, is.irr.interquartile)])

plot.geo = merge(fips.geo, 
                 irr.ops.frac.dt,
                 by="fips", 
                 all.x=T)


fips.geo=merge(fips.dt, 
               map_data("county"), 
               by.x=c("state", "county"), 
               by.y=c("region","subregion"))

# and filter to KS/NE, where we do the irrigation analysis below

map.irr = ggplot(plot.geo[state %in% c("kansas", "nebraska")],
                 aes(long, lat, group = group)) +
  geom_polygon(aes(fill=irr.stat*100)) +
  geom_polygon(data=plot.geo[state %in% c("kansas","nebraska") & is.irr.interquartile,],
               aes(color="25-75 percentile irrigation"),
               fill=NA) +
  #geom_vline(xintercept = -100, linetype = "dashed") +
  coord_map(projection = "albers", 
            parameters = c(32,38),
            xlim=c(-105,-92)) + 
  # scale_fill_gradient(name="", limits=c(0,100),
  #                     low = "#FFFFFF",
  #                     high="#358856",
  #                     na.value = "#EFEFEF") +
  # scale_fill_viridis_c(option = "A",
  #                      name="% operations using irrigation",
  #                      na.value = "#EFEFEF",
  #                      limits=c(0,100)) +
  scale_fill_gradient(name="% operations using irrigation",
                      low="#FFFFFF",
                      high="#354c7c",
                      na.value = "#000000",
                      limits=c(0,100)) + 
  scale_color_manual(name="",
                     values="#F62681")+
  theme_minimal() +
  theme(panel.grid=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",#c(0.2,0.16),
        legend.direction = "horizontal",
        legend.box = "vertical", 
        legend.margin = margin(0,0,0,0,"cm"),
        legend.spacing.y = unit(0,"cm"),
        plot.margin=unit(c(0, 0, 0, 0), units="cm"))+#"right") + 
  guides(fill = guide_colourbar(title.position="top",
                                title.vjust = 1,
                                title.hjust = 0.5, barwidth = 10),
         color = guide_legend(title.position="top")
  )
map.irr


state.res = lapply(lead.times, FUN=function(lead) {
  lead.frm = as.formula(paste0("log(crop.yield) ~ edd.sin.silking + edd.sin.silking:avg.tmax.longrun.m26 + edd.sin.notsilking + edd.sin.notsilking:avg.tmax.longrun.m26  + edd.sin.silking:avg.tmax.deviation + edd.sin.notsilking:avg.tmax.deviation + edd.sin.pred.",lead,"d.silking + edd.sin.pred.",lead,"d.notsilking +  avg.tmax.silking + avg.tmax.silking.sq + avg.tmax.notsilking + avg.tmax.notsilking.sq + totprecip.silking + totprecip.silking.sq + totprecip.notsilking + totprecip.notsilking.sq | fips[study.yr] + state.yr"))
  tmp.mod = feols(lead.frm,
                  data=state.filt.data,
                  cluster = c("fips", "state.yr"), 
                  split = ~ is.irr.interquartile
  )
  tmp.mod
})
state.res.dt = rbindlist(lapply(lead.times, FUN=function(lead) {
  rbind(data.table(est=coeftable(state.res[[lead]]$`FALSE`)[c(1:4),1],
                   se=coeftable(state.res[[lead]]$`FALSE`)[c(1:4),2],
                   p=coeftable(state.res[[lead]]$`FALSE`)[c(1:4),4],
                   lead=lead,
                   coef=c("edd.silking", "edd.notsilking", "edd.pred.silking", "edd.pred.nosilking"),
                   coef.type=c("edd","edd", "edd.pred", "edd.pred"),
                   timing=c("Silking", "Not silking", "Silking", "Not silking"),
                   irr.level="<25 or >75 \npercentile irrigation"),
        data.table(est=coeftable(state.res[[lead]]$`TRUE`)[c(1:4),1],
                   se=coeftable(state.res[[lead]]$`TRUE`)[c(1:4),2],
                   p=coeftable(state.res[[lead]]$`TRUE`)[c(1:4),4],
                   lead=lead,
                   coef=c("edd.silking", "edd.notsilking", "edd.pred.silking", "edd.pred.nosilking"),
                   coef.type=c("edd","edd", "edd.pred", "edd.pred"),
                   timing=c("Silking", "Not silking", "Silking", "Not silking"),
                   irr.level="25-75 \npercentile irrigation"))
}))
state.irr.plot = ggplot(state.res.dt, 
                        aes(x=lead, 
                            y=est,
                            group=paste0(coef, lead),
                            color=coef.type,
                        )) + 
  geom_point(position=position_dodge2(width=0.3),
             size=3) + 
  geom_errorbar(aes(ymin=est-1.96*se,
                    ymax=est+1.96*se), 
                width=0.3,
                size=1.2,
                position=position_dodge2(width=0.3)) +
  xlab("Forecast Lead (days)") + 
  ylab("Effect on yield (log points)") + 
  scale_x_continuous(breaks=1:7) + 
  scale_shape_discrete(name="Crop Stage") + 
  scale_color_manual(name="Effect",
                     values = c("#777777", "#01796F"),
                     labels=c("Extreme Degree Day (EDD)",
                              "Prediction Benefits (per EDD)")) + 
  geom_hline(yintercept = 0) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.line.y = element_line(size=1),
        panel.spacing = unit(2, "lines"),
        strip.text=element_text(size=16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16)) + 
  facet_grid(timing ~ irr.level)
state.irr.plot

# combine KS/NE irrigation map with estimates
plot_grid(map.irr, 
          state.irr.plot,
          ncol = 2, rel_widths = c(0.3, 0.7),
          labels = c("a","b"))

ggsave(here("results","figures","Fig4.eps"),
       width=9,
       height=6,
       units="in")

########################################
#
# Supplementary Information (SI) results
#
########################################

###################
# Table S1: Robustness of null result
###################

# Robustness to inclusion of additional EDD measures
edd32.res = lapply(lead.times, FUN=function(lead) {
  lead.frm = as.formula(paste0("log(crop.yield) ~ edd.sin.silking + edd.sin.silking:avg.tmax.longrun.m26 + edd.sin.notsilking + edd.sin.notsilking:avg.tmax.longrun.m26  + edd.sin.silking:avg.tmax.deviation + edd.sin.notsilking:avg.tmax.deviation + edd.sin.pred.",lead,"d.silking + edd.sin.pred.",lead,"d.notsilking +  avg.tmax.silking + avg.tmax.silking.sq + avg.tmax.notsilking + avg.tmax.notsilking.sq + totprecip.silking + totprecip.silking.sq + totprecip.notsilking + totprecip.notsilking.sq + edd.sin.32 | fips[study.yr] + state.yr"))
  tmp.mod = feols(lead.frm,
                  data=est.data,
                  cluster = c("fips", "state.yr"))
  tmp.mod
})

edd32.res.dt = rbindlist(lapply(lead.times, FUN=function(lead) {
  data.table(est=coeftable(edd32.res[[lead]])[c(1:4),1],
             se=coeftable(edd32.res[[lead]])[c(1:4),2],
             p=coeftable(edd32.res[[lead]])[c(1:4),4],
             lead=lead,
             coef=c("edd.silking", "edd.notsilking", "edd.pred.silking", "edd.pred.nosilking"),
             coef.type=c("edd","edd", "edd.pred", "edd.pred"),
             timing=c("Silking", "Not silking", "Silking", "Not silking"))
}))

edd3234.res = lapply(lead.times, FUN=function(lead) {
  lead.frm = as.formula(paste0("log(crop.yield) ~ edd.sin.silking + edd.sin.silking:avg.tmax.longrun.m26 + edd.sin.notsilking + edd.sin.notsilking:avg.tmax.longrun.m26  + edd.sin.silking:avg.tmax.deviation + edd.sin.notsilking:avg.tmax.deviation + edd.sin.pred.",lead,"d.silking + edd.sin.pred.",lead,"d.notsilking +  avg.tmax.silking + avg.tmax.silking.sq + avg.tmax.notsilking + avg.tmax.notsilking.sq + totprecip.silking + totprecip.silking.sq + totprecip.notsilking + totprecip.notsilking.sq + edd.sin.32 + edd.sin.34 | fips[study.yr] + state.yr"))
  tmp.mod = feols(lead.frm,
                  data=est.data,
                  cluster = c("fips", "state.yr"))
  tmp.mod
})

edd3234.res.dt = rbindlist(lapply(lead.times, FUN=function(lead) {
  data.table(est=coeftable(edd3234.res[[lead]])[c(1:4),1],
             se=coeftable(edd3234.res[[lead]])[c(1:4),2],
             p=coeftable(edd3234.res[[lead]])[c(1:4),4],
             lead=lead,
             coef=c("edd.silking", "edd.notsilking", "edd.pred.silking", "edd.pred.nosilking"),
             coef.type=c("edd","edd", "edd.pred", "edd.pred"),
             timing=c("Silking", "Not silking", "Silking", "Not silking"))
}))

# Robustness to getting rid of per-county linear trends
baseline.noctrend.res = lapply(lead.times, FUN=function(lead) {
  lead.frm = as.formula(paste0("log(crop.yield) ~ edd.sin.silking + edd.sin.silking:avg.tmax.longrun.m26 + edd.sin.notsilking + edd.sin.notsilking:avg.tmax.longrun.m26  + edd.sin.silking:avg.tmax.deviation + edd.sin.notsilking:avg.tmax.deviation + edd.sin.pred.",lead,"d.silking + edd.sin.pred.",lead,"d.notsilking +  avg.tmax.silking + avg.tmax.silking.sq + avg.tmax.notsilking + avg.tmax.notsilking.sq + totprecip.silking + totprecip.silking.sq + totprecip.notsilking + totprecip.notsilking.sq | fips + state.yr"))
  tmp.mod = feols(lead.frm,
                  data=est.data,
                  cluster = c("fips", "state.yr"))
  tmp.mod
})

baseline.noctrend.res.dt = rbindlist(lapply(lead.times, FUN=function(lead) {
  data.table(est=coeftable(baseline.noctrend.res[[lead]])[c(1:4),1],
             se=coeftable(baseline.noctrend.res[[lead]])[c(1:4),2],
             p=coeftable(baseline.noctrend.res[[lead]])[c(1:4),4],
             lead=lead,
             coef=c("edd.silking", "edd.notsilking", "edd.pred.silking", "edd.pred.nosilking"),
             coef.type=c("edd","edd", "edd.pred", "edd.pred"),
             timing=c("Silking", "Not silking", "Silking", "Not silking"))
}))

# Drop main inclusion of tmax and tmaxsq
baseline.notmax.res = lapply(lead.times, FUN=function(lead) {
  lead.frm = as.formula(paste0("log(crop.yield) ~ edd.sin.silking + edd.sin.silking:avg.tmax.longrun.m26 + edd.sin.notsilking + edd.sin.notsilking:avg.tmax.longrun.m26  + edd.sin.silking:avg.tmax.deviation + edd.sin.notsilking:avg.tmax.deviation + edd.sin.pred.",lead,"d.silking + edd.sin.pred.",lead,"d.notsilking + totprecip.silking + totprecip.silking.sq + totprecip.notsilking + totprecip.notsilking.sq | fips[study.yr] + state.yr"))
  tmp.mod = feols(lead.frm,
                  data=est.data,
                  cluster = c("fips", "state.yr"))
  tmp.mod
})
baseline.notmax.res.dt = rbindlist(lapply(lead.times, FUN=function(lead) {
  data.table(est=coeftable(baseline.notmax.res[[lead]])[c(1:4),1],
             se=coeftable(baseline.notmax.res[[lead]])[c(1:4),2],
             p=coeftable(baseline.notmax.res[[lead]])[c(1:4),4],
             lead=lead,
             coef=c("edd.silking", "edd.notsilking", "edd.pred.silking", "edd.pred.nosilking"),
             coef.type=c("edd","edd", "edd.pred", "edd.pred"),
             timing=c("Silking", "Not silking", "Silking", "Not silking"))
}))


# Weighting by acres planted
baseline.acreweight.res = lapply(lead.times, FUN=function(lead) {
  lead.frm = as.formula(paste0("log(crop.yield) ~ edd.sin.silking + edd.sin.silking:avg.tmax.longrun.m26 + edd.sin.notsilking + edd.sin.notsilking:avg.tmax.longrun.m26  + edd.sin.silking:avg.tmax.deviation + edd.sin.notsilking:avg.tmax.deviation + edd.sin.pred.",lead,"d.silking + edd.sin.pred.",lead,"d.notsilking +  avg.tmax.silking + avg.tmax.silking.sq + avg.tmax.notsilking + avg.tmax.notsilking.sq + totprecip.silking + totprecip.silking.sq + totprecip.notsilking + totprecip.notsilking.sq | fips + state.yr"))
  tmp.mod = feols(lead.frm,
                  data=est.data,
                  cluster = c("fips", "state.yr"),
                  weights = ~acres.planted)
  tmp.mod
})

baseline.acreweight.res.dt = rbindlist(lapply(lead.times, FUN=function(lead) {
  data.table(est=coeftable(baseline.acreweight.res[[lead]])[c(1:4),1],
             se=coeftable(baseline.acreweight.res[[lead]])[c(1:4),2],
             p=coeftable(baseline.acreweight.res[[lead]])[c(1:4),4],
             lead=lead,
             coef=c("edd.silking", "edd.notsilking", "edd.pred.silking", "edd.pred.nosilking"),
             coef.type=c("edd","edd", "edd.pred", "edd.pred"),
             timing=c("Silking", "Not silking", "Silking", "Not silking"))
}))

# Robustness to ignoring intraseasonal variation in effects of weather -- just 
# estimate single effect of all weather variables and prediction 
# (but retain interactions between) EDD and average heat
baseline.nointraseasonal.res = lapply(lead.times, FUN=function(lead) {
  lead.frm = as.formula(paste0("log(crop.yield) ~ edd.sin + edd.sin:avg.tmax.longrun.m26 + edd.sin:avg.tmax.deviation + edd.sin.pred.",lead,"d + avg.tmax + avg.tmax.sq + totprecip + totprecip.sq | fips[study.yr] + state.yr"))
  tmp.mod = feols(lead.frm,
                  data=est.data,
                  cluster = c("fips", "state.yr"))
  tmp.mod
})

baseline.nointraseasonal.res.dt = rbindlist(lapply(lead.times, FUN=function(lead) {
  data.table(est=coeftable(baseline.nointraseasonal.res[[lead]])[c(1:2),1],
             se=coeftable(baseline.nointraseasonal.res[[lead]])[c(1:2),2],
             p=coeftable(baseline.nointraseasonal.res[[lead]])[c(1:2),4],
             lead=lead,
             coef=c("edd", "edd.pred"),
             coef.type=c("edd","edd.pred"),
             timing=c("Full year", "Full year"))
}))

# Make a table of all of these. We'll have columns as lead.times, and row groups
# as coefficients from different models
getstars = function(p) {
  stars = rep("  ",length(p))
  stars[p<0.1] = ".  "
  stars[p<0.05] = "*  "
  stars[p<0.01] = "** "
  stars[p<0.001] = "***"
  stars
}
rob.tab = lapply(1:7, FUN=function(l) {
  rbind(
    edd32.res.dt[coef=="edd.pred.silking" & lead==l, 
                 sprintf("%.4f%3s",
                         est,
                         getstars(p)
                 )],
    edd32.res.dt[coef=="edd.pred.silking" & lead==l, 
                 sprintf("(%.4f)",
                         se
                 )],
    edd32.res.dt[coef=="edd.pred.nosilking" & lead==l, 
                 sprintf("%.4f%3s",
                         est,
                         getstars(p)
                 )],
    edd32.res.dt[coef=="edd.pred.nosilking" & lead==l, 
                 sprintf("(%.4f)",
                         se
                 )],
    
    
    edd3234.res.dt[coef=="edd.pred.silking" & lead==l, 
                   sprintf("%.4f%3s",
                           est,
                           getstars(p)
                   )],
    edd3234.res.dt[coef=="edd.pred.silking" & lead==l, 
                   sprintf("(%.4f)",
                           se
                   )],
    edd3234.res.dt[coef=="edd.pred.nosilking" & lead==l, 
                   sprintf("%.4f%3s",
                           est,
                           getstars(p)
                   )],
    edd3234.res.dt[coef=="edd.pred.nosilking" & lead==l, 
                   sprintf("(%.4f)",
                           se
                   )],
    
    baseline.noctrend.res.dt[coef=="edd.pred.silking" & lead==l, 
                             sprintf("%.4f%3s",
                                     est,
                                     getstars(p)
                             )],
    baseline.noctrend.res.dt[coef=="edd.pred.silking" & lead==l, 
                             sprintf("(%.4f)",
                                     se
                             )],
    baseline.noctrend.res.dt[coef=="edd.pred.nosilking" & lead==l, 
                             sprintf("%.4f%3s",
                                     est,
                                     getstars(p)
                             )],
    baseline.noctrend.res.dt[coef=="edd.pred.nosilking" & lead==l, 
                             sprintf("(%.4f)",
                                     se
                             )],
    
    baseline.notmax.res.dt[coef=="edd.pred.silking" & lead==l, 
                           sprintf("%.4f%3s",
                                   est,
                                   getstars(p)
                           )],
    baseline.notmax.res.dt[coef=="edd.pred.silking" & lead==l, 
                           sprintf("(%.4f)",
                                   se
                           )],
    baseline.notmax.res.dt[coef=="edd.pred.nosilking" & lead==l, 
                           sprintf("%.4f%3s",
                                   est,
                                   getstars(p)
                           )],
    baseline.notmax.res.dt[coef=="edd.pred.nosilking" & lead==l, 
                           sprintf("(%.4f)",
                                   se
                           )],
    
    
    
    
    
    baseline.acreweight.res.dt[coef=="edd.pred.silking" & lead==l, 
                               sprintf("%.4f%3s",
                                       est,
                                       getstars(p)
                               )],
    baseline.acreweight.res.dt[coef=="edd.pred.silking" & lead==l, 
                               sprintf("(%.4f)",
                                       se
                               )],
    baseline.acreweight.res.dt[coef=="edd.pred.nosilking" & lead==l, 
                               sprintf("%.4f%3s",
                                       est,
                                       getstars(p)
                               )],
    baseline.acreweight.res.dt[coef=="edd.pred.nosilking" & lead==l, 
                               sprintf("(%.4f)",
                                       se
                               )],
    
    baseline.nointraseasonal.res.dt[coef.type=="edd.pred" & lead==l, 
                                    sprintf("%.4f%3s",
                                            est,
                                            getstars(p)
                                    )],
    baseline.nointraseasonal.res.dt[coef.type=="edd.pred" & lead==l, 
                                    sprintf("(%.4f)",
                                            se
                                    )]
  )
})
rob.tab.mat = cbind(rob.tab[[1]],rob.tab[[2]],rob.tab[[3]],rob.tab[[4]],rob.tab[[5]],rob.tab[[6]],rob.tab[[7]])
rownames(rob.tab.mat) = c("Silking","",
                          "Non-silking","",
                          "Silking","",
                          "Non-silking","",
                          "Silking","",
                          "Non-silking","",
                          "Silking","",
                          "Non-silking","",
                          "Silking","",
                          "Non-silking","",
                          "Full year","")
rob.tab.tex = kable(x=rob.tab.mat, 
                    format = "latex",
                    row.names = T,
                    col.names = c("1 day", paste0(2:7," days")),
                    linesep = c("",
                                "\\addlinespace"),
                    vline="",
                    escape=F, 
                    caption = "\\label{tab:rob}Robustness of results to alternative specifications (row groups)
      for each forecast lead (columns). Estimates are benefits of an additional EDD 
      being correctly forecast (log points). First row group includes
      another measure of EDD using 32C as a threshold. Second row group includes two
      additional measures of EDD, using 32C and 34C as thresholds. Third row group
      drops per-county linear trends, fourth drops main linear and quadratic effects of average maximum temperature,
      fifth weights observations by acres planted,
      and the final row group only uses one measure of 
      each weather variable (temperature, precipitation, their squares, and EDD) per county and year
      rather than separating exposure by silking vs. non-silking months. 
      Standard errors clustered two ways by county and year. 
      Significance indicators: . $<$0.1, * $<$0.05, ** $<$0.01, ***$<$ 0.001.")
rob.tab.tex = add_header_above(rob.tab.tex, 
                               header=c(" "=1,
                                        "Forecast lead"=7),
                               line=F)
rob.tab.tex = group_rows(rob.tab.tex, group_label="Include $>$32C EDD", 
                         start_row = 1, end_row = 4, escape = F)
rob.tab.tex = group_rows(rob.tab.tex, group_label="Include $>$32C EDD and $>$34C EDD", 
                         start_row = 5, end_row = 8, escape = F)
rob.tab.tex = group_rows(rob.tab.tex, group_label="No per-county linear trends", 
                         start_row = 9, end_row = 12, escape = F)
rob.tab.tex = group_rows(rob.tab.tex, group_label="No main effects of average daily max temperature", 
                         start_row = 13, end_row = 16, escape = F)
rob.tab.tex = group_rows(rob.tab.tex, group_label="Weighted by corn acres planted", 
                         start_row = 17, end_row = 20, escape = F)
rob.tab.tex = group_rows(rob.tab.tex, group_label="No intraseasonal heterogeneity", 
                         start_row = 21, end_row = 22, escape = F)
save_kable(rob.tab.tex, here("results",
                             "tables",
                             "TableS1.tex"))



############
# Fig S2: Calculate EDD, PEDD, CPEDD prior to within-county spatial averaging
############
# use 'hires' version of heat and forecast measures that are based on extreme
# heat calculations at pixel level, with EDD then averaged over space
# (vs spatially averaging first and then calculating extremes)

baseline.hires.res = lapply(lead.times, FUN=function(lead) {
  lead.frm = as.formula(paste0("log(crop.yield) ~ edd.hires.silking + edd.hires.silking:avg.tmax.longrun.m26  + edd.hires.silking:avg.tmax.deviation + edd.hires.notsilking + edd.hires.notsilking:avg.tmax.longrun.m26  + edd.hires.notsilking:avg.tmax.deviation + edd.hires.pred.",lead,"d.silking + edd.hires.pred.",lead,"d.notsilking +  avg.tmax.silking + avg.tmax.silking.sq + avg.tmax.notsilking + avg.tmax.notsilking.sq + totprecip.silking + totprecip.silking.sq + totprecip.notsilking + totprecip.notsilking.sq | fips[study.yr] + state.yr"))
  tmp.mod = feols(lead.frm,
                  data=est.data,
                  cluster = c("fips", "state.yr"))
  tmp.mod
})

baseline.hires.res.dt = rbindlist(lapply(lead.times, FUN=function(lead) {
  data.table(est=coeftable(baseline.hires.res[[lead]])[c(1:4),1],
             se=coeftable(baseline.hires.res[[lead]])[c(1:4),2],
             p=coeftable(baseline.hires.res[[lead]])[c(1:4),4],
             lead=lead,
             coef=c("edd.silking", "edd.notsilking", "edd.pred.silking", "edd.pred.nosilking"),
             coef.type=c("edd","edd", "edd.pred", "edd.pred"),
             timing=c("Silking", "Not silking", "Silking", "Not silking"))
}))


baseline.hires.plot = ggplot(baseline.hires.res.dt, 
                       aes(x=lead, 
                           y=est,
                           group=paste0(coef, lead),
                           color=coef.type,
                           shape=timing
                       )) + 
  geom_point(position=position_dodge2(width=0.3),
             size=3) + 
  geom_errorbar(aes(ymin=est-1.96*se,
                    ymax=est+1.96*se), 
                width=0.3,
                size=1.2,
                position=position_dodge2(width=0.3)) +
  xlab("Forecast Lead (days)") + 
  ylab("Effect on yield (log points)") + 
  scale_x_continuous(breaks=1:7) + 
  scale_shape_discrete(name="Crop Stage") + 
  scale_color_manual(name="Effect",
                     values = c("#777777", "#01796F"),
                     labels=c("Extreme Degree Day (EDD)",
                              "Prediction Benefits (per EDD)")) + 
  geom_hline(yintercept = 0) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.line.y = element_line(size=1))



ggsave(here("results","figures","FigS2.eps"),
       width=8,
       height=4,
       units="in")

#######################
# Figure S3: naive estimates with homogenous EDD effect
#######################
# Naive estimates
naive.res = lapply(lead.times, FUN=function(lead) {
  lead.frm = as.formula(paste0("log(crop.yield) ~ edd.sin + edd.sin.pred.",lead,"d + avg.tmax + avg.tmax.sq + totprecip + totprecip.sq | fips[study.yr] + state.yr"))
  tmp.mod = feols(lead.frm,
                  data=est.data,
                  cluster = c("fips", "state.yr"))
  tmp.mod
})



naive.res.dt = rbindlist(lapply(lead.times, FUN=function(lead) {
  data.table(est=coeftable(naive.res[[lead]])[c(1:2),1],
             se=coeftable(naive.res[[lead]])[c(1:2),2],
             p=coeftable(naive.res[[lead]])[c(1:2),4],
             lead=lead,
             coef=c("edd", "edd.pred"))
}))

naive.plot = ggplot(naive.res.dt, 
                    aes(x=lead, 
                        y=est,
                        group=paste0(coef, lead),
                        color=coef
                    )) + 
  geom_point(position=position_dodge2(width=0.3),
             size=3) + 
  geom_errorbar(aes(ymin=est-1.96*se,
                    ymax=est+1.96*se), 
                width=0.3,
                size=1.2,
                position=position_dodge2(width=0.3)) +
  xlab("Forecast Lead (days)") + 
  ylab("Effect on yield (log points)") + 
  scale_x_continuous(breaks=1:7) + 
  scale_color_manual(name="Effect",
                     values = c("#777777", "#01796F"),
                     labels=c("Extreme Degree Day (EDD)",
                              "Prediction Benefits (per EDD)")) + 
  geom_hline(yintercept = 0) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.line.y = element_line(size=1))

ggsave(here("results","figures","FigS3.eps"),
       width=8,
       height=4,
       units="in")



# By what % would prediction reduce yield loss if the EDD
# and predicted EDD coefficients had causal interpretations?
naive.res.dt[, .(perc.reduced.yield.loss=est[coef=="edd.pred"]/est[coef=="edd"]),by=lead]



###################
# Table S2: power concerns: is loss of significance when moving from
# the naive model to our baseline spec because of precision loss from
# additional regressors or removal of omitted variable bias? 
# To illustrate this, we compare the naive model to one which 
# adds only the interaction of EDD with baseline temperature, and does
# not separate EDD effects by season. This highlights that for the
# focal coefficients on correctly predicted EDD, the SEs don't 
# change much, but point estimates do (spatial heterogeneity in EDD
# effects matter). 

power.comp.dt = rbind(baseline.nointraseasonal.res.dt[coef=="edd.pred",.(est,se,p,lead)][,mod:='nosilkingsplit'],
                      naive.res.dt[coef=="edd.pred",.(est,se,p,lead)][,mod:="naive"])
setorder(power.comp.dt, lead, mod)

power.tab = lapply(1:7, FUN=function(l) {
  rbind(
    baseline.nointraseasonal.res.dt[coef=="edd.pred" & lead==l, 
                                    sprintf("%.4f%3s",
                                            est,
                                            getstars(p)
                                    )],
    baseline.nointraseasonal.res.dt[coef=="edd.pred" & lead==l, 
                                    sprintf("(%.4f)",
                                            se
                                    )],
    naive.res.dt[coef=="edd.pred" & lead==l, 
                 sprintf("%.4f%3s",
                         est,
                         getstars(p)
                 )],
    naive.res.dt[coef=="edd.pred" & lead==l, 
                 sprintf("(%.4f)",
                         se
                 )]
  )
})
power.tab.mat = cbind(power.tab[[1]],power.tab[[2]],power.tab[[3]],power.tab[[4]],power.tab[[5]],power.tab[[6]],power.tab[[7]])
rownames(power.tab.mat) = c("EDD effects depend on average temperature","",
                            "Naive model","")
power.tab.tex = kable(x=power.tab.mat, 
                      format = "latex",
                      row.names = T,
                      col.names = c("1 day", paste0(2:7," days")),
                      linesep = c("",
                                  "\\addlinespace"),
                      vline="",
                      escape=F, 
                      caption = "\\label{tab:rob}Investigation of loss of significance when moving from the naive specification
                    to one in which the effects of EDD are allowed to depend on long-run average temperature and 
                    current year average temperature deviations from long run average. 
      for each forecast lead (columns). Estimates are benefits of an additional EDD 
      being correctly forecast (log points). 
      Standard errors clustered two ways by county and year. 
      Significance indicators: . $<$0.1, * $<$0.05, ** $<$0.01, ***$<$ 0.001.")
power.tab.tex = add_header_above(power.tab.tex, 
                                 header=c(" "=1,
                                          "Forecast lead"=7),
                                 line=F)
save_kable(power.tab.tex, here("results",
                               "tables",
                               "TableS2.tex"))
