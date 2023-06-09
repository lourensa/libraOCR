
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

# steps of OCR


#' @describeIn step3_split_videos Read the ini-file
#' @export
step1_initialise <- function(inifile="ocr.ini") {

   INI = ReadIniFileRecursive(inifile,ssep="/",asep=",",inherit=TRUE)

   # return
   return(INI)
}


#' @describeIn step3_split_videos Get a list of available experiments
#' @export
step2_list_of_experiments <- function(INI,exps=NULL) {

   exps = get_list_of_experiments(INI,exps)

   # return
   return(exps)
}


#' Split video's
#'
#' Split video's
#' @param INI  ini-structure
#' @param exp  name of one experiment. 
#'             If NULL then the main section with all expereiments is returned. 
#'             If not NULL, the subsection of that experiment is returned.
#' @export
step3_split_videos <- function(INI,exps) {

   # init
   frfrac = 0.5  # frame fraction available

   # get exp names which realy exist
   exps = get_list_of_experiments(INI,exps)

   # loop over all experiments
   for (exp in exps) {
      expsec =  get_exp_section(INI,exp)
      video  = expsec$video
      outdir = expsec$outdir

      # check jpg files in outdir
      files   = get_list_of_jpg_files(INI,exp)
      nframes = av::av_media_info(video)$video$frames
      frac = length(files)/nframes
      if (frac > frfrac) {
         cat("Video of experiment",exp,
             "already split-up, frame-fraction:",frac,".\n")
      } else {
         cat("Split video of experiment",exp,"\n")
         if (! dir.exists(outdir)) {
            cat("Create output directory",outdir,"...\n")
            dir.create(outdir,recursive=TRUE)
         }
         files = split_video_to_jpg(video,outdir)
      }
   }

   # return
   return(invisible(files))
}

#' @describeIn step3_split_videos Define the clip area of a movie
#' @export
step4a_define_clip_area <- function(INI,exps,replace=FALSE) {

   # get exp names which realy exist
   exps = get_list_of_experiments(INI,exps)

   # loop over all experiments
   for (exp in exps) {
      expsec =  get_exp_section(INI,exp)
      video  = expsec$video
      outdir = expsec$outdir

      clip = read_clip_area(INI,exp)
      if (is.null(clip) | replace) {
         files = get_list_of_jpg_files(INI,exp)
         if (length(files) > 0) {
            # define clip area
            img  = jpeg::readJPEG(files[1])
            clip = img_get_clip_area(img)
            # save clip
            write_clip_area(clip,INI,exp)
         } else {
            msg = paste0("Experiment: ",exp,"\n   No jpeg-files found. Split video first.")
            cat(msg,"\n")
            warning(msg)
         }
      } else {
         cat("Clip area of experiment",exp,"already defined.\n")
      }

   }

   # return
   return(invisible(NULL))
}

#' @describeIn step3_split_videos Define the tilt line of a movie
#' @export
step4b_define_tilt_line <- function(INI,exps,replace=FALSE) {

   # get exp names which realy exist
   exps = get_list_of_experiments(INI,exps)

   # loop over all experiments
   for (exp in exps) {
      expsec =  get_exp_section(INI,exp)
      video  = expsec$video
      outdir = expsec$outdir

      tilt = read_tilt_line(INI,exp)
      if (is.null(tilt) | replace) {
         files = get_list_of_jpg_files(INI,exp)
         if (length(files) > 0) {
            # define tilt line
            img  = jpeg::readJPEG(files[1])
            tilt = img_get_tilt_line(img)
            # save tilt
            write_tilt_line(tilt,INI,exp)
         } else {
            msg = paste0("Experiment: ",exp,"\n   No jpeg-files found. Split video first.")
            cat(msg,"\n")
            warning(msg)
         }
      } else {
         cat("Tilt line of experiment",exp,"already defined.\n")
      }

   }

   # return
   return(invisible(NULL))
}


#' @param set  character or list; 
#'             If character, the name of a set in `ocr_settings` of the INI file.
#'             If a list, settings containing a value for `step`, `smooth`,
#'             and, optionally, `cycle`.
#' @describeIn step3_split_videos OCR the images and save the result as a CSV-file
#' @export
step5_ocr_images <- function(INI,exps,set=list(step=3,smooth=0,cycle=1),draw=FALSE) {

   # get exp names which realy exist
   exps = get_list_of_experiments(INI,exps)

   # get set
   set           = get_ocr_settings(INI,set)
   step          = set$step
   smooth        = set$smooth
   cycle         = set$cycle
   minconfidence = set$minconfidence

   # loop over all experiments
   for (exp in exps) {
      expsec  =  get_exp_section(INI,exp)
      video   = expsec$video
      csvfile = expsec$csvfile

      # clip (required)
      clip = read_clip_area(INI,exp)
      if (is.null(clip)) {
         msg = paste0("No clip area defined for experiment ",exp,". No OCR performed.")
         cat(msg,"\n")
         warning(msg)
         next
      }

      # tilt (optional)
      tilt = read_tilt_line(INI,exp)
      if (! is.null(tilt)) {
         cat("Using tilt settings for experiment",exp,"\n")
      }

      # try to read the csv-file
      res = load_ocr_results(csvfile)
      if (is.null(res)) {
         # no csv-file, load jpg filenames
         res = get_list_of_jpg_files(INI,exp)
      }

      # OCR
      framerate = av::av_media_info(video)$video$framerate
      res = img_ocr_list(res,csvfile,clip=clip,step=step,draw=draw,
                         smooth=smooth,minconfidence=minconfidence,
                         cycle=cycle,framerate=framerate)

   }

   # return
   return(invisible(NULL))
}

# plot

#' @describeIn step3_split_videos Create a plot of the OCR results
#' @export
step6_plot_result <- function(INI,exps=NULL,plts=NULL) {

   # get exp names which realy exist
   exps = get_list_of_experiments(INI,exps)

   # get plot settings to use
   plts = get_list_of_plot_settings(INI,plts=plts)


   # local functions
   loc_plt_set <- function(width=15,height=10,col="red",
                       type="pdf",xlab="time [s]",ylab="weight [g]",
                       minconfidence=90,...) {
      # init
      nmes = c("width","height","col","type","xlab","ylab","minconfidence")
      # defaults
      out = list(...)
      for (nme in nmes) {
         out[[nme]] = get(nme)
      }
      return(out)
   }
   loc_plot <- function(INI,exp,plt) {
      # init
      nopltcols = c("width","height","type","minconfidence") # data from pltsec not for plot command
      outnme = paste0(exp,"_",plt)

      # get settings
      expsec  =  get_exp_section(INI,exp)
      pltsec  =  get_plt_section(INI,plt)
      pltsec  =  do.call("loc_plt_set",args=pltsec)

      minconfidence = pltsec$minconfidence
      csvfile = expsec$csvfile

      # title
      main = expsec$title
      if (is.null(main)) {
         main = expsec$id
      }

      # read file
      dat = load_ocr_results(csvfile)
      if (! is.null(dat)) {
         dat = cfnSelList(dat,dat$confidence >= minconfidence)
      }

      # open plot file
      pst = with(pltsec,cfnDevOpen(type,out=outnme,width=width,height=height))
      on.exit(cfnDevClose(pst))
      
      # plot
      args = pltsec
      for (nme in nopltcols) {
         args[[nme]] = NULL
      }
      args$x = dat$time
      args$y = dat$weight
      args$main = main
      do.call("plot",args=args)

      return(invisible(NULL))
   }

   # plot all figures
   for (exp in exps) {
      for (plt in plts) {
         loc_plot(INI,exp,plt)
      }
   }

   return(invisible(NULL))
}

