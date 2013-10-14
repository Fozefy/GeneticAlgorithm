###################Adds ability to merge two environments
extend.env <- function(env, var.list){
  for(name in names(var.list)){
    env[[name]] <- var.list[[name]]
  }
  env
}

