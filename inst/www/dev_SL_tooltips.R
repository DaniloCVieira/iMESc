
apply_recursive <- function(lst) {
  # Função interna para aplicar recursivamente
  apply_recursive_internal <- function(x) {
    xx<-x[sapply(x,length)>0]


    if (!all(sapply(xx,length)==1)) {
      # Se for uma lista, aplique recursivamente a cada elemento
      lapply(x, apply_recursive_internal)
    } else {

      tas<-sapply(x,function(xx) attr(xx, "Rd_tag"))
      text<-as.character(x)
      dots<-which(grepl("dots",tas))
      if(length(dots)>0){
        text[dots]<-"..."
      }
      value=gsub("  "," ",paste0(sapply(text, function(x) gsub("\n","",x)),collapse=""))

      value
    }
  }

  # Chama a função interna
  apply_recursive_internal(lst)
}


gH<-function(fun,section){
  tempfile(fileext = ".txt")
  gbRd::Rd_fun(fun, keep_section=section)

}
get_params_from_rd<-function(arguments){

  texts<-apply_recursive(arguments)
  value=paste(unlist(texts[-1],recursive = T),collapse="")
  arg=texts[[1]]
  data.frame(arg=arg,value=value)

}
get_args_description<-function(path){
  rd<-gH(path,"\\arguments")
  section="arguments"
  sec<-which(grepl(section,sapply(rd, function(x) attr(x,'Rd_tag'))))

  item<-which(grepl("item",sapply(rd[[sec]],function(x) attr(x,'Rd_tag'))))
  arguments<-rd[[sec]][item]
  dff<-do.call(rbind,lapply(arguments,get_params_from_rd))
  dff

}

formals_string<-function(string){
  fun_name <- strsplit(string, "::")[[1]][2]
  package_name <- strsplit(string, "::")[[1]][1]
  function_name2<-paste0(fun_name,".default")
  fun <- get(function_name2, envir = asNamespace(package_name))
  formals(fun)
}

gaussip_fun<-function(scaled = TRUE, kernel=c("rbfdot","polydot","vanilladot","tanhodot","laplacedot","besseldot","anovadot","splinedot"),var=1,  tol=0.0005){}

ksvm_fun<-function(nu = 0.2, epsilon = 0.1,
                   class.weights = NULL, cross = 0,
                   tol = 0.001, shrinking = TRUE){}

sl_ctl_formals<-list(
  nb={
    args=c(formals_string('stats::density')[c("bw","adjust","kernel","window")])
    attr(args,'fun_name')<-"stats::density"
    attr(args,'argname')<-"density"
    args
  },
  xyf={
    args<-c(formals(kohonen::somgrid))
    attr(args,'argname')<-"grid"
    attr(args,'fun_name')<-"kohonen::somgrid"
    args
  },
  glm={
    args<-c(formals(stats::glm.control))
    attr(args,'argname')<-"control"
    attr(args,'fun_name')<-"stats::glm.control"
    args
  },
  cforest={
    args<-c(formals(party::cforest_control))
    attr(args,'argname')<-"control"
    attr(args,'fun_name')<-"party::cforest_control"
    args
  }

)

sl_formals<-list(
  rf=formals_string('randomForest::randomForest'),
  nb=formals_string('klaR::NaiveBayes'),
  knn=formals(caret::knn3Train),
  gbm=formals(gbm::gbm.fit),
  xyf=formals(kohonen::supersom),
  glm=formals(stats::glm),
  dnn=formals(deepnet::sae.dnn.train),
  earth=formals_string('earth::earth'),
  gamSpline=formals(gam::gam),
  gaussprRadial=formals(gaussip_fun),
  svmLinear=formals(ksvm_fun),
  svmRadial=formals(ksvm_fun),
  svmRadialCost=formals(ksvm_fun),
  avNNet=formals_string('caret::avNNet'),
  nnet=formals_string('nnet::nnet'),
  pcaNNet=formals_string('caret::pcaNNet'),
  rpart=formals(rpart::rpart.control),
  blackboost=formals(mboost::blackboost),
  randomGLM=formals(randomGLM::randomGLM),
  gbm_h2o=formals(h2o::h2o.gbm),
  monmlp=formals(monmlp::monmlp.fit),
  mlpML=formals_string('RSNNS::mlp'),
  rbf=formals_string("RSNNS::rbf"),
  evtree=formals(evtree::evtree.control)

)
sl_formals<-sl_formals[newmodels]
sl_formals<-sapply(names(sl_formals),function(i){
  attr(sl_formals[[i]],"fun_name")<-train_functions[[i]]
  sl_formals[[i]]
})

train_functions3<-train_functions2[newmodels]
train_functions3$rpart$topic<-"rpart.control"


names(train_functions3)

control_funs<-sapply(sl_ctl_formals,function(x){attr(x,"fun_name")})
control_funs<-lapply(control_funs,function(x) {
  res<-strsplit(x,"::")[[1]]
  names(res)<-c("package","topic")
  as.list(res)
})



sl_formals<-lapply(sl_formals,function(x){
 x[!sapply(x,class)%in%c("name")]
})

remove<-c('x',"y","name",'weights','trace','stratify','Scale.y','na.action',
          "xtest","ytest",'do.trace',"importance",'classwt','localImp','keep.forest','corr.bias','keep.inbag',"prob",'class.stratify.cv',"verbose","n.cores","keep.data","keep.data","cv.folds","train.fraction","offset","misc","prior","subset","wp","nfold","ncross","keepxy","glm","whatmap","model","start","contrasts","kernel","allowParallel","categoricalColumns","maxCategoricalLevels","multiClass.global","multiClass.pairwise","sampleBaggingWeights","mandatoryCovariates",'nThreads',"includeSelfinteractions","multiClass.minObs","multiClass.ignoreLevels","replaceBadBagFeatures","interactionsMandatory","randomSeed","keep_cross_validation_models","keep_cross_validation_predictions","keep_cross_validation_fold_assignment","score_each_iteration","cases.specified","iter.stopped","monotone","scale.y","init.weights","silent","inputsTest","targetsTest","pruneFunc","pruneFuncParams","linOut","seed","cores")

sl_formals$randomGLM$type<-NULL

sl_formals$gbm_h2o<-sl_formals$gbm_h2o[c("stopping_metric","stopping_tolerance","distribution","min_split_improvement")]

sl_formals<-lapply(sl_formals,function(x){
  x[c(remove)]<-NULL
  x
})
sl_ctl_formals<-lapply(sl_ctl_formals,function(x){
  x[c(remove)]<-NULL
  x
})
sl_ctl_formals$nb$kernel<-
sl_ctl_formals$nb$window<-c('gaussian','rectangular','triangular','epanechnikov','biweight','cosine','optcosine')

sl_ctl_formals$cforest$teststat<-c("max","quad")
sl_ctl_formals$cforest$testtype <-c("Teststatistic","Bonferroni","MonteCarlo")
sl_ctl_formals$cforest$replace<-F

model_parameters$knn_class<-model_parameters$knn
model_parameters$knn_reg<-model_parameters$knn_reg
sl_formals<-sapply(names(sl_formals), function(i){
  sl_formals[[i]][!names(sl_formals[[i]])%in%model_parameters[[i]]$parameter]
})
sl_ctl_formals<-sapply(names(sl_ctl_formals), function(i){
  sl_ctl_formals[[i]][!names(sl_ctl_formals[[i]])%in%model_parameters[[i]]$parameter]
})


sl_formals$gbm<-sl_formals$gbm[c("bag.fraction")]

sl_formals$xyf$dist.fcts<-c("euclidean",'bray',"manhattan",'sumofsquares',"tanimoto")


x<-iris[1:4]
y<-iris$Species
model<-"xyf"





rdtag<-function(x){attr(x,"Rd_tag")}
# Função de substituição recursiva para tratar múltiplas ocorrências de padrões
gsub_recursive <- function(patterns, replacements, x) {
  if (length(patterns) == 0) {
    return(x)
  } else {
    result <- gsub(patterns[1], replacements[1], x)
    if (patterns[1] == "\\\\link\\{([^{}]*)\\}") {
      # Lidar com o caso aninhado de link dentro de code
      result <- gsub_recursive(patterns[-1], replacements[-1], result)
    } else {
      result <- gsub_recursive(patterns[-1], replacements[-1], result)
    }
    return(result)
  }
}
i="nb"
funlist<-control_funs
get_sl_tips<-function(funlist){
  sl_tips<-lapply(seq_along(funlist),function(i){
    path<-do.call(utils::help,funlist[[i]])
    rd<-gbRd::Rd_fun(path, keep_section="\\arguments")
    args_rd<-rd[sapply(rd, rdtag)=="\\arguments"]

    string<-paste0(capture.output(rd),collapse = "")
    re<-stringr::str_replace_all(string, "\\s+", " ")
    out_usage<-strsplit(re,"\\item")[[1]][-1]
    names<-gsub("\\{(.+?)\\}.*", "\\1", out_usage)


    # Substituições e padrões
    patterns <- c("\\\\code\\{([^{}]*)\\}", "\\\\link\\{([^{}]*)\\}", "\\\\dots")
    replacements <- c("<code>\\1</code>", "<link>\\1</link>", "...")

    # Aplicar substituições recursivas
    texto_corrigido <- gsub_recursive(patterns, replacements, out_usage)

    texto_corrigido<-as.character(sapply(texto_corrigido,function(x){
      gsub("\\}","",as.character(strsplit(x,"\\}\\{")[[1]])[2])
    }))


    values<-as.character(sapply(texto_corrigido,function(x) HTML(paste0("<p>",gsub("\\\\","",x),"</p>"))))

    # values<-gsub("\\{","<span> ",values)
    #values<-gsub("\\}","</span>",values)

    res<-data.frame(values)
    temp<-strsplit(out_usage,"\\}\\{")
    names<-sapply(temp,function(x) gsub("\\{","",x[[1]]))
    res$name<-sapply(strsplit(names,", "),function(x) x[[1]])
    rownames(res)<-make.unique(res$name)
    res

  })
  names(sl_tips)<-names(funlist)
  sl_tips
}

sl_formals$dnn$hidden<-NULL
sl_tips<-get_sl_tips(train_functions3)
sl_tips2<-get_sl_tips(control_funs)
for(i in names(sl_tips2) ){
  sl_tips[[i]]<-rbind(sl_tips[[i]],
                   sl_tips2[[i]])
}


sl_ctl_formals$xyf<-NULL
sl_formals$rf$sampsize<-NULL
sl_formals$rf<-sl_formals$rf
sl_formals$earth<-sl_formals$earth[c("pmethod")]
sl_formals$avNNet<-NULL
sl_formals$nnet<-sl_formals$nnet
sl_formals$nnet$softmax<-NULL
sl_formals$pcaNNet$thresh<-NULL
sl_formals$randomGLM<-NULL
sl_formals$gbm_h2o<-NULL
sl_tips$nb["window",1]<-sl_tips$nb["kernel",1]


saveRDS(sl_formals,"inst/www/sl_formals.rds")
saveRDS(sl_ctl_formals,"inst/www/sl_ctl_formals.rds")
saveRDS(sl_tips,"inst/www/sl_tips.rds")
