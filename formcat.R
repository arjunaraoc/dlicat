#forms a catalogue with key entries that need checking
library(stringr)
formcat<-function(filen) {

  con = file(filen, "r")

  i=1      
  while(TRUE) {
    cat<-readLines(con,n=1)
    if (length(cat)>0) {
        catl<-strsplit(cat,"dc.")
        title<-getitemdetails(catl,"title: (.+)$")

        author<-getitemdetails(catl,"contributor.author: (.+)$")
        id<-getitemdetails(catl,"Book Source: Digital Library of India Item ([0-9]+\\.[0-9]+) ")
        sc<-getitemdetails(catl,"description.scanningcentre: (.+)$")
        slib<-getitemdetails(catl,"source.library: (.+)$")
        slang<-getitemdetails(catl,"language.iso: (.+)$")
        sclass<-getitemdetails(catl,"subject.classification: (.+)$")
        pub<-getitemdetails(catl,"^publisher\\: (.+)$")
        pdate<-getitemdetails(catl,"date.citation: (.+)$")
        #first time check
        if (i==1) {
          dlicat<-data.frame(title=title,author=author,id=id,sclass=sclass,slang=slang,sc=sc,slib=slib,pub=pub,pdate=pdate,stringsAsFactors=FALSE)
          i=2
          
        }else
          dlicat<-rbind(dlicat,c(title,author,id,sclass,slang,sc,slib,pub,pdate),deparse.level=0,stringsAsFactors=FALSE)
        
    }else
       break
  }
  close(con)
  names(dlicat)<-c("title","author","id","sclass","slang","sc","slib","pub","pdate")
  return(dlicat)
}

#returns matched strings. if more than 1,returns a string with substrings separated by comma
getitemdetails<- function(splitline,pattern) {
  detail<-lapply(splitline, function(x) { str_match(x,pattern) })
  detail<-detail[[1]][,2]
  detail<-c(detail[which(!is.na(detail))])
  if (identical(detail,character(0))) 
    detail<-""
  else { 
    detail<-lapply(detail,str_squish)
    if (length(detail)>1) {

      detail<-paste(paste0('"',detail, '"'), collapse = ", ")
    }
  }
  return (detail)
}
  
