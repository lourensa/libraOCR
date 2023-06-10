
#    libraOCR is a library to convert a movie of a display of a balance into a table, using OCR.
#
#    Copyright (C) 2023  Aris Lourens
#
#    This file is part of libraOCR.
#
#    libraOCR is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    libraOCR is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

# utility functions
#


#' Get the list of jpg-files of a given experiment
#'
#' Get the list of available jpg-file names of given experiment `exp`.
get_list_of_jpg_files <- function(INI,exp) {

   # init
   pattern = "image_.*[.]jpg"

   # get the experiments section
   expsec = get_exp_section(INI,exp)
   video  = expsec$video
   outdir = expsec$outdir

   # check directory
   if (dir.exists(outdir)) {
      # get files
      files = dir(outdir,pattern=pattern,full.names=TRUE)
   } else {
      files = NULL
   }

   # return
   return(files)
}

#' Read the precvious stored clip area
read_clip_area <- function(INI,exp) {

   # init
   clipname = "image_clip"

   # get the experiments section
   expsec = get_exp_section(INI,exp)
   outdir = expsec$outdir

   clipfile = cfn_check_file(clipname,path=outdir,check=TRUE)
   if (! is.null(clipfile)) {
      clip = cfnLoad(clipname,path=outdir)
   } else {
      clip = NULL
   }

   # return
   return(clip)
}

#' Write the clip area for future use
write_clip_area <- function(clip,INI,exp) {
   # init
   clipname = "image_clip"

   # get the experiments section
   expsec = get_exp_section(INI,exp)
   outdir = expsec$outdir

   # save
   cfnSaveAs(clip,clipname,path=outdir)

   # return
   return(invisible(NULL))
}

#' Read the precvious stored tilt line
read_tilt_line <- function(INI,exp) {

   # init
   tiltname = "image_tilt"

   # get the experiments section
   expsec = get_exp_section(INI,exp)
   outdir = expsec$outdir

   tiltfile = cfn_check_file(tiltname,path=outdir,check=TRUE)
   if (! is.null(tiltfile)) {
      tilt = cfnLoad(tiltname,path=outdir)
   } else {
      tilt = NULL
   }

   # return
   return(tilt)
}

#' Write the tilt line for future use
write_tilt_line <- function(tilt,INI,exp) {
   # init
   tiltname = "image_tilt"

   # get the experiments section
   expsec = get_exp_section(INI,exp)
   outdir = expsec$outdir

   # save
   cfnSaveAs(tilt,tiltname,path=outdir)

   # return
   return(invisible(NULL))
}


#' Split a video into one picture per frame
#'
#' @export
split_video_to_jpg <- function(video,outdir) {
   av::av_video_images(video=video,destdir=outdir,format="jpg")
   files = dir(outdir,pattern=".*[.]jpg",full.names=TRUE)
   return(invisible(files))
}


#' Load one frame of a split movie
#' 
load_frame <- function(nr,outdir) {
   # init
   fmt = "image_%6.6d.jpg"
   pattern = sprintf(fmt,nr)
   jpgfile = dir(outdir,pattern=pattern,full.name=TRUE)
   img=jpeg::readJPEG(jpgfile)
   return(img)
}

#' extract frame numbers from filenames
#'
#' extract frame numbers from filenames
extract_frame_numbers <- function(files) {

   # init
   pref  = "^image_"
   post  = "[.]jpg$"

   # replace
   nrs = basename(files)
   nrs = gsub(pref,"",nrs)
   nrs = gsub(post,"",nrs)
   nrs = as.numeric(nrs)

   # return
   return(nrs)
}


#' Order columns in OCR result
order_ocr_results <- function(res) {

   # init
   nmesbeg = c("nr","time","word","confidence","weight","cycle")
   nmesend = c("file")

   # order columns
   nmes    = names(res)
   nmesbeg = nmesbeg[nmesbeg %in% nmes]
   nmes    = nmes[! nmes %in% nmesbeg]
   nmesend = nmesend[nmesend %in% nmes]
   nmes    = nmes[! nmes %in% nmesend]
   nmes    = c(nmesbeg,nmes,nmesend)
   res = res[nmes]

   # return
   return(invisible(res))
}

#' Save OCR results to CSV-file
save_ocr_results <- function(lst,outfile) {

   # order columns
   lst = order_ocr_results(lst)

   # csv-file
   if (! is.null(outfile)) {
      cat("Write csv-file",outfile,"\n")
      tout = lst
      tout$file = gsub("/","\\",tout$file,fixed=TRUE)
      write.csv(tout,file=outfile,row.names=FALSE,na="")
   }

   # return
   return(invisible(lst))
}

#' Load previuos saved OCR results from CSV-file
load_ocr_results <- function(csvfile) {

   # try to read the csv-file
   if (file.exists(csvfile)) {
      # former result found
      cat("Load data from csv-file", csvfile,"\n")
      lst = read.csv(csvfile,na.strings = "", stringsAsFactors = FALSE)
      lst = as.list(lst)
   } else {
      lst = NULL
   }

   # return
   return(invisible(lst))
}


img_get_clip_area <- function(img) {

   # plot image
   col = "red"
   img_plot(img)

   cat("Select two opposite points to identify the clip area...\n")
   flush.console()

   lb = options()$locatorBell
   on.exit(options(locatorBell=lb))
   options(locatorBell=FALSE)

   clip1 = locator(1,type="p",col=col)
   with(clip1,abline(h=y,v=x,col=col))
   clip2 = locator(1,type="p",col=col)
   with(clip2,abline(h=y,v=x,col=col))
   clip = clip1
   clip$x[2] = clip2$x[1]
   clip$y[2] = clip2$y[1]

   # return
   return(clip)
}

img_clip <- function(img,clip=NULL,tilt=NULL) {
   
   if (is.null(clip)) {return(img)}

   dm     = dim(img)[2:1]
   img    = img_as_3d(img)
   clip$x = range(pmax(1,pmin(round(clip$x),dm[1])))
   clip$y = range(pmax(1,pmin(round(clip$y),dm[2])))
   ii = clip$x[1]:clip$x[2]
   jj = dm[2] + 1 - clip$y[2]:clip$y[1]
   if (is.null(tilt)) {
      out = img[jj,ii,]
   } else {
      # use tilt
      # define grid points, rotate them, and read them from img
      ix = rep(ii-(ii[1]-1),each=length(jj))
      iy = rep(jj-(jj[1]-1),times=length(ii))
      rot = solve(tilt$trf) %*% rbind(ix,iy)  # x=rot[,1], y=rot[,2]
      # translate
      rot[1,] = rot[1,] + clip$x[1]
      rot[2,] = rot[2,] + clip$y[1]
      # copy
      out = array(0,dim=c(length(jj),length(ii),3))
      for (i in 1:3) {
         out[cbind(iy,ix,i)] = img[cbind(rot[2,],rot[1,],i)]
      }
   }

   return(out)
}


img_get_tilt_line <- function(img) {

   # plot image
   col = "blue"
   img_plot(img)

   cat("Select two points to identify the tilt line along the bottom of the digits...\n")
   flush.console()

   lb = options()$locatorBell
   on.exit(options(locatorBell=lb))
   options(locatorBell=FALSE)

   # get line
   tline = locator(2,type="o",col=col)

   # create tilt data
   ang  = atan2(diff(tline$y),diff(tline$x))
   trf  = matrix(c(cos(ang),sin(ang),-sin(ang),cos(ang)),2,2)
   tilt = list(line=tline,ang=ang,trf=trf)

   # return
   return(tilt)
}


img_reduce <- function(img,step=c(1,1),smooth=c(0,0)) {
   #    img     RGB matrix

   img = img_as_3d(img)

   # controleer step
   if (length(step) == 1) {
      step = rep(step,2)
   } else if (length(step) < 1) {
      step = c(1,1)
   }

   # smooth
   out = img_smooth(img,smooth)

   # selectie, step[1:2] = x,y = dim2,dim1
   dm  = dim(img)
   n1  = dm[1]
   n2  = dm[2]
   start = pmax(round(step/2),1)    # offset voor beginpunt 
   ii1 = seq(start[2],n1,step[2])
   ii2 = seq(start[1],n2,step[1])
   out = out[ii1,ii2,1:3]

   # return
   return(out)
}

img_smooth <- function(img,smooth) {
   return(img_smooth2(img,smooth))
}

img_smooth1 <- function(img,smooth) {

   # controleer smooth
   if (length(smooth) == 1) {
      smooth = rep(smooth,2)
   } else if (length(smooth) < 1) {
      smooth = c(0,0)
   }
   smooth = smooth[1:2]

   # no smooth?
   if (all(smooth == 0)) {
      return(img)
   }

   # smooth[1:2] = x,y = dim2,dim1
   dm  = dim(img)
   n1  = dm[1]
   n2  = dm[2]
   out = array(0,dim=dm)
   n   = array(0,dim=c(n1,n2))
   for (i2 in (-smooth[1]):smooth[1]) {
      ii2 = seq(max(1,1+i2),min(n2+i2,n2),1)
      o2  = ii2-i2
#print(cbind(ii2,o2))

      for (i1 in (-smooth[2]):smooth[2]) {
         ii1 = seq(max(1,1+i1),min(n1+i1,n1),1)
         o1  = ii1-i1

         out[o1,o2,] = out[o1,o2,] + img[ii1,ii2,]
         n[o1,o2]    = n[o1,o2] + 1
      }
   }
   for (i in 1:3) {
      out[,,i] = out[,,i]/n
   }


   # return
   return(out)
}

img_smooth2 <- function(img,smooth) {

   # controleer smooth
   if (length(smooth) == 1) {
      smooth = rep(smooth,2)
   } else if (length(smooth) < 1) {
      smooth = c(0,0)
   }
   smooth = smooth[1:2]

   # no smooth?
   if (all(smooth == 0)) {
      return(img)
   }

   smooth = abs(smooth)

   # smooth[1:2] = x,y = dim2,dim1
   dm = dim(img)
   n1 = dm[1]
   n2 = dm[2]
   n = img
   for (i in 1:max(smooth)) {
      out = img
      n[] = 1
      if (i <= smooth[1]) { # is x = dim[2]
         out[, -1,] = out[, -1,] + img[,-n2,]
         out[,-n2,] = out[,-n2,] + img[, -1,]
         n[, -1,] = n[, -1,] + 1
         n[,-n2,] = n[,-n2,] + 1
      }
      if (i <= smooth[2]) { # is y = dim[1]
         out[ -1,,] = out[ -1,,] + img[-n1,,]
         out[-n1,,] = out[-n1,,] + img[ -1,,]
         n[ -1,,] = n[ -1,,] + 1
         n[-n1,,] = n[-n1,,] + 1
      }
      # result
      img = out/n
   }


   # return
   return(img)
}


img_as_raster <- function(img) {
   if (! is.raster(img)) {
      if (is.numeric(img) & length(dim(img)) == 3) {
         img = as.raster(img)
      }
   }
   return(img)
}

img_as_3d <- function(img) {
   if (is.raster(img)) {
      dm=dim(img)
      timg = col2rgb(as.matrix(img))/255.
      img = array(NA,dim=c(dm,3))
      img[,,1]=timg[1,]
      img[,,2]=timg[2,]
      img[,,3]=timg[3,]
   }
   return(img)
}

img_as_raw_png <- function(img) {
   # convert image to raw-PNG
   if (is.numeric(img) & length(dim(img)) == 3) {
      img  = png::writePNG(img,target=raw())
   } else if (is.raster(img)) {
      img = img_as_3d(img)
      img  = png::writePNG(img,target=raw())
   } else if (! is.raw(img)) {
      img = NULL
   }
   return(img)
}



#' plot image of different types
img_plot <- function(img)  {
   plot(img_as_raster(img))
}


#' The workhorse function for the image OCR list
img_ocr_list <- function(files,outfile=NULL,clip=NULL,
                         step=1,smooth=0,minconfidence=-1,
                         framerate=NULL,cycle=NULL,draw=FALSE,col="red") {

   # init
   fact = 100000
   out = list(nr=c(),time=c(),word=c(),weight=c(),file=c())

   # minconfidence
   minconfidence = cfn_none(minconfidence,-1)

   # framerate
   if (is.null(framerate)) {
      cat("No framerate given, set to 1\n")
      ffact = 1
   } else {
      ffact = 1/framerate
   }

   # init out
   if (is.character(files)) {
      # create out
      out = list(file = files)
   } else if (is.list(files)) {
      # former results
      out = files
   }
   # add missing data
   if (! "nr" %in% names(out)) {
      out$nr   = extract_frame_numbers(out$file)
   }
   for (nme in c("time","word","confidence","weight","cycle")) {
      if (! nme %in% names(out)) {
         out[[nme]] = rep(NA,length(out$file))
      }
   }

   # order columns
   out = order_ocr_results(out)

   # determine cycle
   if (is.null(cycle)) {
      if (all(is.na(out$cycle))) {
         cycle = 1
      } else {
         cycle = max(out$cycle,na.rm=TRUE) + 1
         cat("Current cycle number",cycle,"\n")
      }
   }

   # find missing data
   #    too low confidence
   sel  = (out$confidence < minconfidence)
   sel[is.na(sel)] = FALSE
   #    no result yet
   sel  = sel | is.na(out$word)
   #    subset to be processed
   idxs = seq_along(out$file)
   idxs = idxs[sel]

   # OCR
   nid  = 0  # number of identified images
   ostr = paste0("/",length(idxs),"...\n") # text for counter
   for (i in seq_along(idxs)) {
      if ((i %% 100) == 0) {
         cat(paste0(i,ostr))
         flush.console()
      }
      idx  = idxs[i]
      file = out$file[idx]
      res = img_ocr(file,clip=clip,step=step,smooth=smooth,draw=draw,col=col)
      words = res$word
      if (length(words) == 1) {
         # OK
         # check confidence
         if (res$confidence > cfn_none(out$confidence[idx],-999)) {
            # confidence improved, store new result
            nid = nid + 1
            out$word[idx]       = words
            out$time[idx]       = (out$nr[idx]-1)*ffact
            out$cycle[idx]      = cycle
            out$confidence[idx] = res$confidence
         } else {
            cat(i,"file",file,
                "New confidence value",paste0("(",res$confidence,")"),
                "not greater than former one",paste0("(",out$confidence[idx],")"),"\n")
         }
      } else {
         # beetje onbepaald
         cat(i,"file",file,"onbepaald\n")
      }
   }
   cat(paste0(i,ostr))
   cat("Number of identified images",nid,"out of",length(idxs),"\n")
   flush.console()

   # gewicht bepalen
   words = out$word
   words = gsub("[.]","",words)  # punt verwijderen
   out$weight = as.numeric(words)/fact

   # csv-file
   save_ocr_results(lst=out,outfile)

   # return
   return(invisible(out))
}

#' The workhorse function for the image OCR
img_ocr <- function(img,clip=NULL,step=1,smooth=0,draw=FALSE,col="red") {

   # inint
   charset = "-.0123456789"

   # ocr sttings
   loc_engine <- tesseract::tesseract(options = list(tessedit_char_whitelist = charset))

   # read file if img is of class character
   if (is.character(img)) {
      img = jpeg::readJPEG(img)         # 3D RGB
   }

   # clip and reduce
   img = img_clip(img,clip)
   img = img_reduce(img,step=step,smooth=smooth)

   # convert image to raw-PNG
   rv = img_as_raw_png(img)

   # OCR
   res = tesseract::ocr_data(rv, engine = loc_engine)

   # draw result
   if (draw) {
      plot(as.raster(img))
      for(i in seq_along(res$word)){
         w=res$word[i]
         bb=scan(text=res$bbox[i],what=1L,sep=",")
         cat(w,"\n")
         x=bb[c(1,3)]
         y=dim(img)[1]-bb[c(2,4)]
         lines(x[c(1,2,2,1,1)],y[c(1,1,2,2,1)],col=col)
         text(mean(x),mean(y),label=w,col=col,cex=2)
      }
   }

   # return
   return(res)
}




