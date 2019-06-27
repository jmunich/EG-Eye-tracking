files<-list.files()
functions<-files[grep("*.R$",x=files)]
functions<-functions[-c(11,13)]
for(f in functions){
  source(f)
}
