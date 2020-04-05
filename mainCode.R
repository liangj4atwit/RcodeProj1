library("png")
#library("colorspace")

#The goal is to identify color of images, data set are in ARSS/ColorID/Data, Classification refence is List.csv
#IdCC and isColor are the two functions to work on.

#Read Image
#path<-"Put Path Here"
x <- readPNG(path, native=FALSE)
# These are the RGB values 
Bk = c(0,0,0)
Br = c(153, 102, 51)
Re = c(255, 0, 0)
Or = c(255, 153, 0)
Ye = c(255, 255, 0)
Gr = c(0, 255, 00)
Bl = c(0, 0, 255)
Vi = c(255, 0, 255)
Gy = c(204, 204, 204)
Go = c(205, 205, 166)
Sy = c(228,228,228)
#Make Data Frame
COLORCODE=rbind(Bk,Br,Re,Or,Ye,Gr,Bl,Vi,Gy,Go,Sy)
#Check Image Direction
checkDirImg<-function(imageIn){
  nr=nrow(imageIn)
  nc=ncol(imageIn)
  if(nr<nc){
    return(imageIn)
  }
  else{
    CrImg=array(dim =c( nc,nr,3))
    for(i in 1:nr){
      for(j in 1:nc){
        CrImg[j,i,]=imageIn[i,j,]
      }
    }
    return(CrImg)
  }
}
#Convert From Image To Matrix
covMat<-function(inImg){
  nr=nrow(inImg)
  nc=ncol(inImg)
  r=inImg[,,1]
  g=inImg[,,2]
  b=inImg[,,3]
  a=inImg[,,4]
  Mrgb=rgb(r,g,b,a)
  Orgb=array(dim =c( nr,nc,3))
  for(i in 1:nr){
    for(j in 1:nc){
      temp=rgb(r[i,j],g[i,j],b[i,j],a[i,j])
      tempC=col2rgb(temp)
      Orgb[i,j,1]=tempC[1]
      Orgb[i,j,2]=tempC[2]
      Orgb[i,j,3]=tempC[3]
    }
  }
  return(Orgb)
}

#Check If Color Matches
isColor<-function(Tv,Rv,ERR){
  #If fit in range, then its true
  if((Rv[1]-ERR[1])>Tv[1]){
    return(FALSE)
  }
  if((Rv[2]-ERR[2])>Tv[2]){
    return(FALSE)
  }
  if((Rv[3]-ERR[3])>Tv[3]){
    return(FALSE)
  }
  if((Rv[1]+ERR[1])<Tv[1]){
    return(FALSE)
  }
  if((Rv[2]+ERR[2])<Tv[2]){
    return(FALSE)
  }
  if((Rv[3]+ERR[3])<Tv[3]){
    return(FALSE)
  }
  return(TRUE)
}

#Identify Color Code
IdCC<-function(MatIn){
  #Standard Error include to provide a range of fitting
  SDerr=c(sd(MatIn[,,1]),sd(MatIn[,,2]),sd(MatIn[,,3]))/25.5
  SDerr=round(SDerr)
  nr=nrow(MatIn)
  nc=ncol(MatIn)
  #Make NA Matrix
  CCMat=matrix(NA,nr,nc)
  for(i in 1:nr){
    for(j in 1:nc){
      for (C in 1:11) {
        if(isColor(MatIn[i,j,],COLORCODE[C,],SDerr)){
          CCMat[i,j]=C
        }
      }
    }
  }
  #It should return an NA matrix with all identidied pixels to its tag
  return(CCMat)
}



#Compress Image
compress<-function(Intake){
  nr=nrow(Intake)
  nc=ncol(Intake)
  rmr<-data.frame()
  rmc<-data.frame()
  RT=FALSE
  CT=FALSE
  for(indi in 1:nr){
    Rtemp=FALSE
    for(indj in 1:nc){
      if(is.na(Intake[indi,indj])){
        Rtemp=TRUE
        RT=TRUE
      }
    }
  }
  if(Rtemp){
    rmr=c(rmr,indi)
  }
  
  for(j in 1:nc){
    Ctemp=FALSE
    for(i in 1:nr){
      if(is.na(Intake[i,j])){
        Ctemp=TRUE
        CT=TRUE
      }
    }
  }
  if(Ctemp){
    rmc=c(rmc,j)
  }
  rmr=as.numeric(rmr)
  rmc=as.numeric(rmc)
  if(RT){
    out=Intake[-rmr,]
  }
  if(!RT){
    out=Intake
  }
  if(CT){
    Eout=out[,-rmc]
  }
  if(!CT){
    Eout=out
  }
  return(Eout)
}

linfit<-function(MatIn){
  MatIn=y
  mapI<-data.frame()
  for(i in 1:9){
   i=1
   mapI<-which(MatIn == i,arr.ind = TRUE)
  }
  print(mapI)
}








#Main
main<-function(PNGin){
  rgbI=covMat(PNGin)
  rgbI=checkDirImg(rgbI)
  CLRMat=IdCC(rgbI)
  #Comp=compress(CLRMat)
  #linfit(Comp)
  return(rgbI)
}

