# Supervised: temporal features, leakage check of the temporal validation and recursive
# forecast horizons (inst/www/funs_SL.R, funs_spatiotemporal_validation.R, module00, module08)
set.seed(1)
dates<-seq(as.Date("2015-01-01"),by="month",length.out=96)
series<-function(s){
  temp<-as.numeric(stats::arima.sim(list(ar=.6),96))+s
  y<-numeric(96); y[1]<-5
  for(t in 2:96) y[t]<-5+.75*(y[t-1]-5)+2*sin(2*pi*as.numeric(format(dates[t],"%m"))/12)+.8*temp[t]+rnorm(1,0,.5)
  data.frame(y=y,temp=temp,row.names=paste0("s",s,"_",format(dates,"%Y%m")))
}
df<-do.call(rbind,lapply(1:4,series))
attr(df,"time")<-data.frame(date=rep(dates,4),row.names=rownames(df))
attr(df,"coords")<-data.frame(x=rep(1:4,each=96),y=0,row.names=rownames(df))

# features created by the Temporal Features builder itself
vals<-shiny::reactiveValues(saved_data=list(ts=df),cur_data="ts")
out<-NULL
shiny::testServer(tool2_tab10$server,args=list(vals=vals),{
  session$setInputs(data_x="ts",time_col="date",group_by_coords=TRUE,past_only=TRUE,prefix="",vars="y")
  st<-function(type,vars,settings) list(type=type,vars=vars,prefix="",settings=settings)
  steps<-list(st("memory","y",list(lags="1,2")),st("trend","y",list(windows="3",summaries=c("mean","sd"))),
              st("change","y",list(lags="1",types="diff")),st("led","y",list(alpha=.3,initial=NA,center=FALSE)),
              st("cumulative","y",list(types="mean")),st("anomaly","y",list(windows="4",types="zscore")),
              st("memory","temp",list(lags="1")),st("lead","y",list(leads="3")),
              st("seasonality","y",list(time_col="date",terms="month_cyclic")))
  out<<-apply_feature_recipe(data_x(),steps)
})
meta<-attr(out,"temporal_feature_meta")
check("metadata keeps the recipe (summary, LED alpha, time column)",
      meta$detail[meta$feature=="y_roll3_mean"]=="mean"&&meta$alpha[meta$feature=="y_led"]==0.3&&all(meta$time_col=="date"))
check("calendar variables recorded as calendar",all(meta$type[meta$feature%in%c("date_month_sin","date_month_cos")]=="calendar"))

# the user removes the rows with missing values (start and end of each series)
keep<-stats::complete.cases(out)
feat<-out[keep,]
for(a in c("time","coords")) attr(feat,a)<-attr(out,a)[keep,,drop=FALSE]
attr(feat,"temporal_feature_meta")<-meta
preds<-c("y_lag1","y_lag2","y_roll3_mean","y_roll3_sd","y_diff1","y_led","y_cum_mean","y_anom_4_zscore","temp","temp_lag1","date_month_sin","date_month_cos")

ctx<-sl_recursive_setup(feat,preds,"y",feat$y)
check("derived predictors reproduced from the response (rows removed)",isTRUE(ctx$ok)&&identical(ctx$time_col,"date"))

# leakage checks
check("valid horizon with lag 1 and monthly blocks = 1",sl_valid_horizon(meta,preds,"y",0,1)==1)
check("observed predictors, horizon 6: leakage flagged",length(stcv_leakage_check(meta,preds,"y",TRUE,horizon=6,gap=0,steps_per_block=1)$critical)==1)
check("recursive evaluation: no leakage",length(stcv_leakage_check(meta,preds,"y",TRUE,horizon=6,gap=0,steps_per_block=1,recursive=TRUE)$critical)==0)
p2<-c("y_lag1","y_roll3_mean","temp","date_month_sin","date_month_cos")
check("direct forecast (lead 3) without gap: leakage flagged",length(stcv_leakage_check(meta,p2,"y_lead3",TRUE,horizon=1,gap=0,steps_per_block=1,response_meta=meta)$critical)==1)
check("direct forecast (lead 3) with gap 2: no leakage",length(stcv_leakage_check(meta,p2,"y_lead3",TRUE,horizon=1,gap=2,steps_per_block=1,response_meta=meta)$critical)==0)

# prequential folds as the module builds them, recursive evaluation
x<-feat[,preds]; y<-feat$y
tdf<-data.frame(Lon=attr(feat,"coords")$x,Lat=0,Tempo=stcv_fixed_blocks(attr(feat,"time")$date,"month",1),.response=y)
cv<-make_st_validation(tdf,spattime_names=c("Lon","Lat","Tempo"),response=".response",validation_type="time_block_prequential",
                       k_time=length(unique(tdf$Tempo)),initial_train_blocks=48,horizon_blocks=6,gap_blocks=0,step_blocks=6,temporal_window="expanding",verbose=FALSE)
cf<-stcv_to_caret(cv)
attr(cf,"params")<-list(validation_type="time_block_prequential",horizons=c(1,3,6),horizon_map=make_horizon_map(cf),gap_blocks=0L,
                        horizon_eval="recursive",exo_mode="known",known_vars=NULL,leak=list(h_valid=1,steps_per_block=1))
of<-sl_onestep_folds(cf)
check("one-step tuning folds test only the first block",all(lengths(of$indexOut)==4))
args_train<-list(as.matrix(x),y,"lm",trControl=caret::trainControl(method="cv",index=of$index,indexOut=of$indexOut,savePredictions="final"),tuneLength=1)
m<-do.call(caret::train,args_train)
attr(m,"cvt")<-attr(cf,"params")
rec<-sl_run_recursive(m,args_train,cf,x,y,"y",feat,feat$y,seed=1)
check("recursive forecasts for every test row",rec$failed==0&&!anyNA(rec$pred$pred))
# first step after the origin: recursive = observed (same fold model)
fit1<-{ a<-args_train; f<-names(cf$index)[1]; a[[1]]<-as.matrix(x)[cf$index[[f]],]; a[[2]]<-y[cf$index[[f]]]
        a$trControl<-caret::trainControl(method="none"); a$tuneGrid<-m$bestTune; a$tuneLength<-NULL; do.call(caret::train,a) }
r1<-rec$pred[rec$pred$Resample==names(cf$index)[1]&rec$pred$steps_ahead==1,]
check("first step: recursive = observed prediction",max(abs(r1$pred-predict(fit1,newdata=as.matrix(x)[r1$rowIndex,,drop=FALSE])))<1e-8)
attr(m,"recursive")<-rec
hz<-horizon_curve_data(m,"exact")
check("recursive error grows from t+1 to t+3",hz$summary$RMSE_mean[2]>hz$summary$RMSE_mean[1])
m_obs<-do.call(caret::train,utils::modifyList(args_train,list(trControl=caret::trainControl(method="cv",index=cf$index,indexOut=cf$indexOut,savePredictions="final"))))
attr(m_obs,"cvt")<-utils::modifyList(attr(cf,"params"),list(horizon_eval="observed"))
ho<-horizon_curve_data(m_obs,"exact")
check("observed predictors: horizons beyond t+1 flagged",identical(ho$summary$Leakage,c("no","yes","yes")))
check("horizon curve plots",inherits(gg_horizon_curve(hz,"RMSE"),"ggplot")&&inherits(gg_horizon_curve(ho,"RMSE"),"ggplot"))
