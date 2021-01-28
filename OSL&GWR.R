install.packages(c("geojsonio","plotly","rgdal","spdep","car"))
library(tidyverse) 
library(tmap) 
library(geojsonio) 
library(plotly) 
library(rgdal) 
library(broom) 
library(mapview) 
library(crosstalk) 
library(sf) 
library(sp) 
library(spdep) 
library(car)
londonshp<-readOGR("/Users/apple/Desktop/bkcasa0005/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
londonshpsf<-st_as_sf(londonshp)
londonshpsf
BNG = "+init=epsg:27700"
londonshpsfBNG<-st_transform(londonshpsf,BNG)
qtm(londonshpsfBNG)
crimedata<- read_csv("/Users/apple/Desktop/gisdata.csv") 
str(crimedata)
crimedata <- left_join(londonshpsfBNG, crimedata,
                             by = c("GSS_CODE" = "Area Code"))
tmap_mode("view")
qtm(crimedata,fill="BURGLARY+ROBBERY+THEFT",borders=NULL,fill.palette="Blues",fill.n=6)
model1<-lm(`BURGLARY+ROBBERY+THEFT`~`Unemployment rate`+`% children living in out-of-work households`+`16-18 year olds not in education employment or training` +`Modelled Household median income estimates 2012/13`+`afforabel housing`,data=crimedata)
summary(model1)

spreadLevelPlot(model1)


crimedata$model1_resids<-model1$residuals
qplot(model1$residuals)+geom_histogram()
qplot(model1$residuals)+geom_histogram(binwidth = 0.7)
vif(model1)

install.packages("corrplot")
library(corrplot)
tempdf <- st_set_geometry(crimedata,NULL)
tempdf <- tempdf[,c("Unemployment rate","% children living in out-of-work households","Proportion of population aged 65 and over","16-18 year olds not in education employment or training","Modelled Household median income estimates 2012/13","afforabel housing")]
names(tempdf) <- c("UNEM", "OUT","16-18","INCOME","AFF")
cormat <- cor(tempdf[,1:5], use="complete.obs", method="pearson")
corrplot(cormat)
plot(model1)# Plot result
durbinWatsonTest(model1) #Autocorrelation
tmap_mode("view")
tm_shape(crimedata)+
tm_polygons("model1_resids",
palette = "RdYlBu")

crimedatasp <- as(crimedata,"Spatial")
names(crimedatasp)
# calculate the centroids of all borough in London
coordsw<-coordinates(crimedatasp)
plot(coordsw)
lb_nb<-poly2nb(crimedatasp,queen=T)
knn_lb<-knearneigh(coordsw,k=4)
lb_knn<-knn2nb(knn_lb)


plot(lb_nb,coordinates(coordsw),col="red")
plot(lb_knn,coordinates(coordsw),col="blue")
plot(crimedatasp)
#create a spatial weights matrix object from  weight
lb.queens_weight<-nb2listw(lb_nb,style="C")
lb.knn_4_weight<-nb2listw(lb_knn,style="C")
#moran's I test on the residuals
moran.test(crimedatasp@data$model1_resids,lb.queens_weight)
moran.test(crimedatasp@data$model1_resids,lb.knn_4_weight)

install.packages("spgwr")
library(spgwr)
#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(`BURGLARY+ROBBERY+THEFT`~`Unemployment rate`+`% children living in out-of-work households`+`16-18 year olds not in education employment or training` +`Modelled Household median income estimates 2012/13`+`afforabel housing`,data=crimedata,coords=coordsw,adapt=T)
#run the gwr model
gwr.model = gwr(`BURGLARY+ROBBERY+THEFT`~`Unemployment rate`+`% children living in out-of-work households`+`16-18 year olds not in education employment or training` +`Modelled Household median income estimates 2012/13`+`afforabel housing`,data=crimedata,coords=coordsw, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
gwr.model

results<-as.data.frame(gwr.model$SDF)
names(results)
crimedatasp@data$coefunemp<-results$X.Unemployment.rate.
crimedatasp@data$coefout<-results$X...children.living.in.out.of.work.households.
crimedatasp@data$coef16edu<-results$X.16.18.year.olds.not.in.education.employment.or.training.
crimedatasp@data$coefincome<-results$X.Modelled.Household.median.income.estimates.2012.13.
crimedatasp@data$coefaff<-results$X.afforabel.housing.
tm_shape(crimedatasp) +
tm_polygons(col = "coefunemp", palette = "RdBu", alpha = 0.5)
tm_shape(crimedatasp) +
  tm_polygons(col = "coefout", palette = "RdBu", alpha = 0.5)
tm_shape(crimedatasp) +
  tm_polygons(col = "coef16edu", palette = "RdBu", alpha = 0.5)
tm_shape(crimedatasp) +
  tm_polygons(col = "coefincome", palette = "RdBu", alpha = 0.5)
tm_shape(crimedatasp) +
  tm_polygons(col = "coefaff", palette = "RdBu", alpha = 0.5)

#statistically significant test
sigTestunem= abs(gwr.model$SDF$"`Unemployment rate`") -2 * gwr.model$SDF$"`Unemployment rate`_se"
sigTestout= abs(gwr.model$SDF$"`% children living in out-of-work households`") -2 * gwr.model$SDF$"`% children living in out-of-work households`_se"
sigTest16edu= abs(gwr.model$SDF$"`16-18 year olds not in education employment or training`") -2 * gwr.model$SDF$"`16-18 year olds not in education employment or training`_se"
sigTestincome= abs(gwr.model$SDF$"`Modelled Household median income estimates 2012/13`") -2 * gwr.model$SDF$"`Modelled Household median income estimates 2012/13`_se"
sigTestaff= abs(gwr.model$SDF$"`afforabel housing`") -2 * gwr.model$SDF$"`afforabel housing`_se"

crimedatasp$GWRunemsig<-sigTestunem
tm_shape(crimedatasp) +
tm_polygons(col = "GWRunemsig", palette = "RdYlBu")

crimedatasp$GWRoutsig<-sigTestout
tm_shape(crimedatasp) +
  tm_polygons(col = "GWRoutsig", palette = "RdYlBu")

crimedatasp$GWR16edusig<-sigTest16edu
tm_shape(crimedatasp) +
  tm_polygons(col = "GWR16edusig", palette = "RdYlBu")

crimedatasp$GWRincomesig<-sigTestincome
tm_shape(crimedatasp) +
  tm_polygons(col = "GWRincomesig", palette = "RdYlBu")

crimedatasp$GWRaffsig<-sigTestaff
tm_shape(crimedatasp) +
  tm_polygons(col = "GWRaffsig", palette = "RdYlBu")









