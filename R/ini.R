
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


# functions with direct interactions with the INI structure

#' Get the experiments section or one experiment subsection
#'
#' Get the experiments section or one experiment subsection
#' @param INI  ini-structure
#' @param exp  name of one experiment. 
#'             If NULL then the main section with all expereiments is returned. 
#'             If not NULL, the subsection of that experiment is returned.
get_exp_section <- function(INI,exp=NULL) {
   # init
   expsec_name = "experiments"

   # get main section
   expsec = INI[[expsec_name]]

   if (! is.null(exp)) {
      expsec = expsec[[exp]]
   }

   return(expsec)
}

#' Get the plot setting sections or one specific subsection
#'
#' Get the plot setting sections or one specific subsection
#' @param INI  ini-structure
#' @param plt  name of one plot setting 
#'             If NULL then the main section with all plot settings is returned. 
#'             If not NULL, the subsection of that plot setting is returned.
get_plt_section <- function(INI,plt=NULL) {
   # init
   pltsec_name = "plots"

   # get main section
   pltsec = INI[[pltsec_name]]

   if (! is.null(plt)) {
      pltsec = pltsec[[plt]]
   }

   return(pltsec)
}

#' Get the specified set of OCR settings
#'
#' Get the specified set of OCR settings
get_ocr_settings <- function(INI,set) {
   # init
   ocrsec_name = "ocr_settings"

   loc_full_names <- function(step=1,smooth=0,cycle=NULL,minconfidence=-1,...) {
      out = list(step=step,smooth=smooth,cycle=cycle,minconfidence=minconfidence)
      lst = list(...)
      for (nme in names(lst)) {
         cat("Unknown set option:",nme,"\n")
         out[[nme]]=lst[[nme]]
      }
      return(out)
   }

   # if set is a list, return set
   if (is.list(set)) {
      out = set
   } else {
      # get main section
      out = INI[[ocrsec_name]]

      # test set
      if (set %in% names(out)) {
         out = out[[set]]
      } else {
         stop("Set ",set," not found in ocr_settings.")
      }
   }

   # defaults and full names
   out = do.call("loc_full_names",args=out)

   return(out)
}


#' Get experiment names
#'
#' Get experiment names. Check or all given `exps` exist, only keep those defined in `INI`.
#' The intersection of `exps` and all experiments defined in `INI` is returned.
get_list_of_experiments <- function(INI,exps=NULL) {

   # get the experiments section
   expsec = get_exp_section(INI,NULL)

   all_exps = names(expsec)
   if (is.null(exps)) {
      out = all_exps
   } else {
      # check or all given exps exist, only keep descibed in ocr.ini
      # The intersection of exps and all_exps is returned
      out = exps[exps %in% all_exps]
      if (! all(exps %in% out)) {
         msg = paste0("Not all wanted sections in INI-file:\n",
                      "    wanted   : ",paste(exps,collapse=" "),"\n",
                      "    available: ",paste(out ,collapse=" "))
         cat(msg,"\n")
         warning(msg)
      }
   }
   cat("Process experiments:",out,"\n")

   # return
   return(out)
}

#' Get plot setting names
get_list_of_plot_settings <- function(INI,plts=NULL) {

   # get the experiments section
   pltsec = get_plt_section(INI,NULL)

   all_plts = names(pltsec)
   if (is.null(plts)) {
      out = all_plts
   } else {
      # check or all given plts exist, only keep descibed in ocr.ini
      # The intersection of plts and all_plts is returned
      out = plts[plts %in% all_plts]
      if (! all(plts %in% out)) {
         msg = paste0("Not all wanted plot sections in INI-file:\n",
                      "    wanted   : ",paste(plts,collapse=" "),"\n",
                      "    available: ",paste(out ,collapse=" "))
         cat(msg,"\n")
         warning(msg)
      }
   }

   # return
   return(out)
}


