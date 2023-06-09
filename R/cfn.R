

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

# ----- cfn_none -----

#' Replace a non finite argument by a given value
#'
#' Return `replace` if the argument `x` contains no finite value. 
#' @param x   argument of any type
#' @returns Returns `replace` if no finite value is given, `x` otherwise.
#' @noRd
cfn_none <- function(x,replace=NA) {
   if (is.none(x)){
      if (missing(replace)){replace=NA}
      x = replace
   }
   return(x)
}

#' @describeIn cfn_none Test argument for being nothing or missing
#' @noRd
is.none <- function(x){
   ret=FALSE
   if (missing(x)) {
      ret = TRUE
   } else if (is.null(x)) {
      ret = TRUE
   } else if (all(is.na(x))) {
      ret = TRUE
   } else if (is.numeric(x)) {
      if (! any(is.finite(x))) {
         ret = TRUE
      }
   }
   return(ret)
}

# ----- ReadIniFile -----


# inlezen van een ini-file

#' Read an ini-file
#'
#' This function reads an ini-file and stores the data in a list structure. The ini-file is structured
#' in sections with multiple options in each section. Comments are preceded by '#'.
#' @param infile      name of the ini-file
#' @param addto       a list to which the new data has to be added (optional)
#' @param ssep        subsection separation character(s), if NULL no subsectiosns are recognized. If this character apears in a section name then the section is split up in subsections. This may be done up to an arbitrary number of sublevels.
#' @param asep        array separation character, when given an atempt is made to split an option value into an array (not implemented yet)
#' @details Example of an ini-file:\cr
#' \verb{
#' # comment
#'
#' [section1]
#' option1 = value1
#'
#' [section2]
#' option2 = value2
#' option3 = value3
#'
#' # subsection definition in an option name
#' subsection2/option4 = value4
#' }
#' @return List with read data. The list contains for each section a sublist with all options.
#' @note Comments in the ini-file at the end of an option line are not recognized in the current version.
#' @keywords ini-file python
#' @seealso \code{\link{ReadIniFileRecursive}}, \code{\link{iniFileRead}}
#' @noRd
ReadIniFile <- function(infile,addto=NULL,ssep=NULL,asep=NULL) {

   # init
   if(missing(addto)){out = list()}
   else              {out = addto}

   # read new file
   ini2 = iniFileRead(infile)

   # combine
   out = iniFileMerge(out,ini2)

   # split hierachicaly
   if(! is.null(ssep)) {
      out  = iniFileSplit(out ,ssep=ssep)
   }

   # split arrays
   if (! is.null(asep)) {
      out = cfnListArray(out,sep=asep)
   }

   # return
   return(out)
}

#' Read an ini-file recursive
#'
#' This function reads an ini-file recursively. See also \code{\link{ReadIniFile}}.
#' @param infile      name of the ini-file
#' @param addto       existing list to add new read data to (optional). A recursive section \code{recsection} in \code{addto} is processed before processing \code{infile}.
#' @param recsection  name of the section containing recursive file names
#' @param ssep        subsection separation character(s), if NULL no subsectiosns are recognized. If this character apears in a section name then the section is split up in subsections. This may be done up to an arbitrary number of sublevels.
#' @param inherit     logical; inherit variables. Values of variables may inherit values of other variables. This is denoted as "$\{\code{varname}\}", where \code{varname} is the name of another variable within the INI structure. See also \code{\link{cfnInheritVars}} and \code{\link{iniInheritBuildIn}}.
#' @return List with read data. The list contains for each section a sublist with all options.
#' @note Comments in the ini-file at the end of an option line are not recognized in the current version.
#' @seealso \code{\link{ReadIniFile}}, \code{\link{iniFileRecursive}}, \code{\link{cfnInheritVars}}, \code{\link{iniInheritBuildIn}}, \code{\link{iniBuildInVars}}.
#' @noRd
ReadIniFileRecursive <- function(infile,addto=list(),recsection="recursive",ssep=NULL,asep=NULL,inherit=FALSE) {

   # init
   if(missing(addto)){addto = list()}

   # check for existence of the recursive section in addto
   if(! is.null(addto[[recsection]])) {
      for(rfile in addto[[recsection]]) {
         if(! is.list(rfile)){
            # recall this routine
            out = iniFileRecursive(infile=rfile,recsection=recsection)
            # combine out with addto
            addto = iniFileMerge(addto,out)
         }
      }
   }

   # read infile
   if(! missing(infile)){
      out = iniFileRecursive(infile=infile,recsection=recsection)
      # combine
      out = iniFileMerge(addto,out)
   } else {
      out = addto
   }

   # split hierachicaly
   if(! is.null(ssep)){
      out = iniFileSplit(out,ssep=ssep)
   }

   # inherit variables
   if(inherit) {
      out = cfnInheritVars(out)
   }

   # split arrays
   if (! is.null(asep)) {
      out = cfnListArray(out,sep=asep)
   }

   # return
   return(out)
}

#' Read an ini-file recursively
#'
#' Read an ini-file recursvely. This function can read multiple nested ini-files which contain relative paths.
#' @param  infile      name of the ini-file
#' @param  recsection  name of the section containing recursive file names
#' @return List with read data. The list contains for each section a sublist with all options.
#' @keywords ini-file python
#' @details  This function is called recursively to read multiple nested ini-files. After reading each file the build in variabels are inherited, regardless of the value of the \code{inherit} parameter in \code{\link{ReadIniFileRecursive}}.
#'  See \code{\link{iniBuildInVars}} which build in variables are available.
#' @seealso \code{\link{ReadIniFile}}, \code{\link{ReadIniFileRecursive}}, \code{\link{iniInheritBuildIn}}, \code{\link{iniBuildInVars}}
#' @noRd
iniFileRecursive <- function(infile,recsection="recursive") {

   # init

   # check existence of infile
   if(! file.exists(infile)) {
      warning("ini-file ",infile," not found")
      return(list())
   }

   # split filename in directory and filename
   filedir  = dirname(infile)
   filename = basename(infile)

   # go to directory where to read the file
   currdir = getwd()
   on.exit(setwd(currdir))
   setwd(filedir)

   # read infile
   out = iniFileRead(infile=filename)

   # inherit build in variables
   out = iniInheritBuildIn(out)

   # check for existence of the recursive section
   if(! is.null(out[[recsection]])) {
      for(rfile in out[[recsection]]) {
         if(! is.list(rfile)){
            # recall this routine
            out2 = Recall(infile=rfile,recsection=recsection)
            # combine out2 with out
            out = iniFileMerge(out,out2)
         }
      }
   }

   # return
   return(out)
}


#' Basic read of ini-file
#'
#' This function reads an ini-file and stores the data in a list structure. The ini-file is structured
#' in sections with multiple options in each section. Comments are preceded by '#'.
#' Example of an ini-file:\cr
#' \cr
#' # comment\cr
#' [section1]\cr
#' option1 = value1\cr
#' \cr
#' [section2]\cr
#' option2 = value2\cr
#' option3 = value3\cr
#' @param infile      name of the ini-file
#' @return List with read data. The list contains for each section a sublist with all options.
#' @note Comments in the ini-file at the end of an option line are not recognized in the current version.
#' @keywords ini-file python
#' @seealso \code{\link{ReadIniFile}}, \code{\link{ReadIniFileRecursive}}
#' @noRd
iniFileRead <- function(infile) {

   # init
   out = list()
   section = NULL

   # read line function
   rlf <- function(lun){
      # inlezen van een regel en interpreteren

      # init
      ret = list()

      # read line
      line = readLines(lun,n=1,ok=TRUE)
      line = cfnTrim(line)

      if(is.null(line) | length(line) == 0){
         # end of file
         ret$type = "EOF"
      } else {
         if(nchar(line) == 0){
            # empty record
            ret$type = "empty"
         } else {
            c1 = substr(line,1,1)
            if(c1 == "#") {
               # comment
               ret$type = "comment"
            } else if(c1 == "["){
               # section
               ret$type = "section"
               ipos = unlist(gregexpr(pattern ="]",line))
               ret$section = substr(line,2,ipos-1)
            } else {
               # option
               ipos = unlist(gregexpr(pattern ="=",line))
               if(length(ipos) > 0){
                  ret$type = "option"
                  ipos   = ipos[1]
                  ret$option = cfnTrim(substr(line,1,ipos-1))
                  value  = cfnTrim(substr(line,ipos+1,nchar(line)))
                  if(! is.na(suppressWarnings(as.numeric(value)))){value=as.numeric(value)}
                  ret$value = value
               } else {
                  ret$type = "unknown"
               }
            }
         }
      }

      # return
      return(ret)
   }

   # open file
   lun = file(infile,open="rt")
   on.exit(close(lun))

   # inlezen
   ret = rlf(lun)
   i = 1   # safety first, counter to prevent dead lock
   while(ret$type != "EOF" & i < 10000) {
      i = i + 1
      if(ret$type == "section") {
         section = ret$section
         # check existance of section
         created = FALSE
         for(j in 1:length(section)) {
            if(is.null(out[[section[1:j]]])){
               out[[section[1:j]]] = list()
               created = TRUE
            }
         }
         if(! created) {
            cat("ReadIniFile: section",section,"exists. New options will be added\n")
         }
      } else if(ret$type == "option") {
         if(! is.null(out[[c(section,ret$option)]])){
            cat("ReadIniFile: section ",section,"option",ret$option,"exists. Value will be replaced.\n")
         }
         out[[c(section,ret$option)]] = ret$value
      }
      # volgende regel
      ret = rlf(lun)
   }

   # return
   return(out)
}



#' Split an ini-structure hierachicaly
#'
#' This function splits ini-data structure hierachicaly. 
#' @param ini         ini-file structure
#' @param ssep        subsection separation character(s), if \code{NULL} of \code{NA} no subsectiosns are recognized. If this character apears in a section name then the section is split up in subsections. This may be done up to an arbitrary number of sublevels. The default value is "/".
#' @details If any section name contains the character \code{sep} it will be split into subsections.
#'
#' **NEW** Subsections can be defined in the option name too, like `subsection/subsubsection/option = value`. 
#' The subsections defined in this way are relative to the section this option is defined in.
#' @return    List with ini-data split up into a hierachical structure.
#' @keywords ini-file python
#' @seealso \code{\link{ReadIniFile}}, \code{\link{ReadIniFileRecursive}}
#' @noRd
iniFileSplit <- function(ini,ssep="/") {

   # init
   if(is.null(ssep)){ssep=NA}
   if(is.na(ssep)  ){return(ini)}
   out = list()

   # local functions
   split_name <- function(nme,ssep) {
      # return: list(snme,ssep)
      section = nme
      for(sep in ssep){
         sects = unlist(strsplit(section,split=sep,fixed=TRUE))
         if(length(sects) > 1){
            # first matching separation character found
            section = sects
            # set sep as first character
            ssep = c(sep,ssep)
            ssep = ssep[! duplicated(ssep)]
            # remove empty strings
            for(j in length(section):1) {
               if(nchar(section[j]) == 0) {
                  cat("Warning iniFileSplit: sublevel name has an empty string at level",
                      paste0(j,","),"\nlevel removed in",paste(section,collapse=ssep[1]),"\n")
                  section = section[-j]
               }
            }
            # no further checking
            break()
         }
      }

      # out
      out = list(snme=section,ssep=ssep)

      # return
      return(out)
   }

   # recursive function
   inispl <- function(ini,ssep) {
      out = list()
      for(nme in names(ini)) {
         if(is.list(ini[[nme]])) {
            # section: split name
            tmp = split_name(nme,ssep)
            section = tmp$snme
            ssep = tmp$ssep
            # create sublists
            out2 = list()
            for(i in 1:length(section)) {
               out2[[ section[1:i] ]] = list()
            }
            # recall further sublists
            out2[[section]] = Recall(ini=ini[[nme]],ssep=ssep)
            # merge
            out = iniFileMerge(ini1=out,ini2=out2,silent=TRUE)
         } else {
            # option
            tmp = split_name(nme,ssep)
            snme = tmp$snme
            tmp = c()
            for (x in head(snme,-1)) {
               tmp = c(tmp,x)
               if (is.null(out[[tmp]])){out[[tmp]]=list()}
            }
            if(! is.null(out[[snme]])){
               cat("iniFileSplit: option",nme,"exists. Value will be replaced.\n")
            }
            out[[snme]] = ini[[nme]]
         }
      }
      return(out)
   }

   # splitsen
   out = inispl(ini=ini,ssep=ssep)

   # return
   return(out)
}

#' Combine two ini-data structures
#'
#' Combine two ini-data structures to one structure. The content of ini-data structure \code{ini2} is added to \code{ini1}. Content of \code{ini1} with the same name in \code{ini2} (section and option) is replaced by the values of \code{ini2}. If one ini-structure is split into subsections (see \code{\link{iniFileSplit}} and the other is not then common subsection names are not found and both versions are retained. In that case, if the split action is performed afterwards then the replacement of common names take place. 
#' @param ini1        ini-structure 1
#' @param ini2        ini-structure 2, the content of \code{ini2} is added to \code{ini1}
#' @param silent      TRUE: don't show warning when overwriting a structure, FALSE: warnings
#' @return    List with the combined ini-data where the content of ini2 takes precedence over ini1.
#' @keywords ini-file python
#' @seealso \code{\link{ReadIniFile}}, \code{\link{ReadIniFileRecursive}}, \code{\link{iniFileSplit}}
#' @noRd
iniFileMerge <- function(ini1,ini2,silent=TRUE) {

   # init

   # recursive function
   iniadd <- function(ini1,ini2) {
      nmes1 = names(ini1)
      for(nme in names(ini2)) {
         if(nme %in% nmes1) {
            # if list, then revursive
            if(is.list(ini2[[nme]])) {
               ini1[[nme]] = Recall(ini1[[nme]],ini2[[nme]])
            } else {
               # ini12[[nme]] is an option, replace
               if(! silent){warning("value of ",nme," replaced")}
               ini1[[nme]] = ini2[[nme]]
            }
         } else {
            # name not in ini1, add
            ini1[[nme]] = ini2[[nme]]
         }
      }
      return(ini1)
   }

   # combine
   ini1 = iniadd(ini1,ini2)

   # return
   return(ini1)
}

#' Combine two ini-data structures (obsolete)
#'
#' Obsolete, see \code{\link{iniFileMerge}} Combine two ini-data structures to one structure.  
#' @param ini1        ini-structure 1
#' @param ini2        ini-structure 2, the content of \code{ini2} is added to \code{ini1}
#' @param silent      TRUE: don't show warning when overwriting a structure, FALSE: warnings
#' @noRd
iniFileAdd <- function(ini1,ini2,silent=TRUE) {
   .Deprecated(new="iniFileMerge")
   ini1 = iniFileMerge(ini1,ini2,silent)
   # return
   return(ini1)
}


#' Convert a list to ini-file structure
#'
#' Convert a list to an ini-file structure. Every list level in the list-structure will be a section level. 
#' All variables containing values will be options.
#' @param  lst     list; structure with multiple levels allowed
#' @param  ssep    section level separator
#' @param  outfile character; filename of the ini-file to be written, if omitted or \code{NA} or \code{NULL} no file is written.
#' @return Character string with one value for each record for the ini file.
#' @seealso \code{\link{ReadIniFile}}, \code{\link{ReadIniFileRecursive}}
#' @noRd
iniList2INI <- function(lst,ssep="/",outfile=NULL) {
   # write a list as an ini file

   # init
   pre       = "["   # section prefix
   post      = "]"   # section postfix
   vsep      = ","   # value separator
   blankline = TRUE # TRUE: add a blank line before each new section
   out       = c()
   if(any(is.null(outfile),is.na(outfile))){outfile=NULL}

   # fun
   locProcLvl <- function(lst,sec=c(),out) {
      # lst   sublist

      lsec = ""
      lrec = length(out)
      #
      for(nme in names(lst)) {
         if(is.list(lst[[nme]])) {
            out  = Recall(lst[[nme]],sec=c(sec,nme),out=out)
            lrec = length(out)
         } else {
            # add section
            csec = paste0(pre,paste(sec,collapse=ssep),post)
            if(lsec != csec) {
               # blank line
               if(blankline) {
                  lrec = lrec + 1
                  out[lrec] = ""
               }
               # section
               lrec = lrec + 1
               out[lrec] = csec
               lsec = csec
            }
            # add value
            val = lst[[nme]]
            if(length(val) > 1){val = paste(val,collapse=vsep)}
            lrec = lrec + 1
            out[lrec] = paste(nme,"=",val,sep=" ")
         }
      }
      # return
      return(out)
   }

   # run
   out = locProcLvl(lst=lst,sec=c(),out=out)

   # write
   if(! is.null(outfile)) {
      cat(unlist(out),"",file=outfile,sep="\n")
   }

   # return
   return(invisible(out))
}

#' Get level of ini-file structure
#'
#' Get a specific subsection level of an ini-file structure. The keys of lower levels are inherited.
#' @param  ini  ini-file structure
#' @param  secs subsection to be returned. Written as section names separated by \code{ssep} or multiple values.
#' @inheritParams ReadIniFileRecursive
#' @seealso \code{\link{iniGetVar}}
#' @noRd
iniGetSec <- function(ini,secs,ssep="/",inherit=TRUE) {

   # init
   out = list()

   # split secs
   secs = unlist(strsplit(secs,split=ssep))
#   nsec = length(secs)

   # inherit variables
   if(inherit) {
      ini = cfnInheritVars(ini)
   }

   # collapse
   out = cfnListCollapse(ini,secs,out)

   # return
   return(out)
}

#' Get level of ini-file structure
#'
#' Get a specific subsection level of an ini-file structure. The keys of lower levels are inherited.
#' @inheritParams iniGetSec
#' @seealso \code{\link{iniGetSec}}
#' @noRd
iniFileGet <- function(ini,secs,ssep="/",inherit=TRUE) {
   return(iniGetSec(ini=ini,secs=secs,ssep=ssep,inherit=inherit))
}

#' Get a certain subsection version of an ini-structure
#'
#' Get a certain subsection version of an ini-structure.
#' @param  ini   list; list with sublists and the character variables as defined in `vars`
#' @param  vars  character; variable names within `ini`.
#' @details
#' The variable `vars` may contain multiple variable names. 
#' If a variable in `vars` exists in `ini` its content is assumed to be a subsection in `ini`. 
#' If this subsection exists it is returned. If not, a warning is issued and the next variable name of `vars` is checked.
#'
#' If no selection of a subsection has been made, a warning is issued and `ini` is returned unaltered.
#'
#' It is no error if any variable in `vars` does not exist in `ini`. If none of them exist a warning is issued.
#' @seealso [iniGetVersion()], [iniGetUseVersion()], [iniGetCurVersion()]
#' @noRd
iniSelectVersion <- function(ini,vars) {

   found = FALSE
   nmes  = names(ini)
   for (var in vars) {
      if (var %in% nmes) {
         nme = ini[[var]]
         if (nme %in% nmes) {
            # found
            ini = ini[[nme]]
            found = TRUE
            break
         } else {
            warning("Section ",nme," not found in ini-structure.")
         }
      }
   }
   if (! found) {
      warning("No subsection found in ini-structure.")
   }
   # return
   return(ini)
}

#' @describeIn iniSelectVersion Select a subsection of `ini` as defined in `ini$useversion` or `ini$currentversion`.
#' @noRd
iniGetVersion <- function(ini) {
   return(iniSelectVersion(ini,vars=c("useversion","currentversion")))
}

#' @describeIn iniSelectVersion Select a subsection of `ini` as defined in `ini$useversion`.
#' @noRd
iniGetUseVersion <- function(ini) {
   return(iniSelectVersion(ini,vars=c("useversion")))
}

#' @describeIn iniSelectVersion Select a subsection of `ini` as defined in `ini$currentversion`.
#' @noRd
iniGetCurVersion <- function(ini) {
   return(iniSelectVersion(ini,vars=c("currentversion")))
}



#' Get a Variable of an ini-structure
#'
#' Get a variable of an ini-structure from a given section level. If the variable is not present at a certain level one level higher is checked. This is repeated until the variable is found. If the variable is not found then the \code{default} is searched.  \code{NULL} is returned.
#' @param  var       character; name of the variable which value is to be returned
#' @param  default   character; name of the section containing default values
#' @param  fallback  value to be returned if \code{var} is not found in the given path nor in the default section.
#' @inheritParams iniGetSec
#' @seealso \code{\link{iniGetSec}}
#' @noRd
iniGetVar <- function(ini,var,secs,ssep="/",fallback=NULL,default="default",inherit=FALSE) {

   # init
   out        = NULL

   # split secs
   secs  = unlist(strsplit(secs,split=ssep))
   isecs = seq_along(secs)

   # check sections
   ok = TRUE
   for (i in isecs) {
      scs = secs[1:i]
      if (is.null(ini[[scs]])) {
         ok = FALSE
         break
      }
   }

   # inherit variables
   if(inherit) {
      ini = cfnInheritVars(ini)
   }

   # find variable
   if (ok) {
      for (i in rev(isecs)) {
         scs = secs[1:i]
         if (var %in% names(ini[[scs]])) {
            out = ini[[scs]][[var]]
            break
         }
      }
   }

   # default
   if (is.null(out)) {
      if (! is.null(default)) {
         if (default %in% names(ini)) {
            out = ini[[default]][[var]]
         }
      }
   }

   # fallback
   if (is.null(out)) {
      out = fallback
   }

   # return
   return(out)
}


#' Get a list of build in variables
#'
#' Get a list of build in variables. The values of these variables are assigned during the function call.
#' @param ppfix    character; a string to be used as a prefiz and postfix for the variable names.
#' @return A list with variables which names are pre and postfixed by a \code{ppfix}. The variables are \code{curdir} which contains the current working directory, \code{now} contains the date and time in format "\%Y-\%m-\%d \%H:\%M:\%S". The variables \code{sysname}, \code{nodename}, \code{login} and \code{user} contain the values as returned by \code{Sys.info()}. The environment variables \code{HOME} end \code{PATH} are added, of which the values are returned by \code{Sys.getenv()}.
#' @noRd
iniBuildInVars <- function(ppfix="_") {

   # init
   prefix   = ppfix
   postfix  = ppfix
   dtformat = "%Y-%m-%d %H:%M:%S"
   envvars  = c("HOME","PATH")
   out = list()

   # local functions
   locAdd <- function(nme,val) {
      nme = paste0(prefix,nme,postfix)
      out[[nme]] = val
      return(out)
   }

   # current working directory
   out = locAdd("curdir",getwd())

   # current time
   out = locAdd("now",format(Sys.time(),dtformat))

   # system info
   sysinf = as.list(Sys.info())
   out = locAdd("sysname" ,sysinf$sysname)
   out = locAdd("nodename",sysinf$nodename)
   out = locAdd("login"   ,sysinf$login)
   out = locAdd("user"    ,sysinf$user)

   # environment variables
   for (envvar in envvars) {
      out = locAdd(envvar,Sys.getenv(envvar))
   }

   # return
   return(out)
}


#' Inherit build in variables
#'
#' Inherit build in variables
#' @param  ini  ini-file structure
#' @seealso \code{\link{iniBuildInVars}}
#' @noRd
iniInheritBuildIn <- function(ini) {

   # 
   vars = iniBuildInVars()
   ini  = cfnInheritVars(ini,vars=vars,onlyvars=TRUE)
   # return
   return(ini)
}

# ----- cfnInheritVars -----


#' Substitute variables in list structure
#'
#' Substitute variables in a list structure, the variables are hierarchicaly inherited.
#' Only entries of \code{data} or \code{vars} which contain data (i.e. not a list structure) are
#' treated as variables.
#' Each variable is prepended by "$\{" and postpended by "\}".
#' @param data  list object
#' @param vars  list with extra variables to be used (optional). Variables with the same name in \code{data} take precedence over the variables in \code{vars}.
#' @param onlyvars  logical; If \code{TRUE}, only variables of \code{vars} are inherited, if \code{FALSE} variables of \code{data} are used too (default).
#' @details This function is somehow equivalent to the \code{BasicInterpolation} of the \code{Python configparser}.
#' @seealso \code{\link{cfnSubstVars}}
#' @examples
#' leesplankje=list(aap="noot",mies=list(wim="zus ${aap} ${jet} ${vuur}",jet="teun",gijs=list(does="hok ${wim}",vuur="scha-pen")),vuur=list())
#' str(leesplankje)
#' str(cfnInheritVars(leesplankje))
#' @noRd
cfnInheritVars <- function(data,vars,ssep=NA,onlyvars=FALSE) {

   # init
   vtype = 1  # type of variable definition, maybe for future use
   types = list()
   types[[1]] = list(prefix="${",postfix="}")
   types[[2]] = list(prefix="%(",postfix=")")
   if(missing(vars)){vars=list()}

   prefix  = types[[vtype]]$prefix
   postfix = types[[vtype]]$postfix


   # remove lists from vars
   for(nme in names(vars)){if(is.list(vars[[nme]])){vars[[nme]]=NULL}}

   # run
#   data = locLevel(data,vars)
   data = cfnInheritVarsExecute(data=data,vars=vars,prefix=prefix,postfix=postfix,onlyvars=onlyvars)

   # return
   return(data)
}



#' Work horse function for variable inheritance
#' @noRd
cfnInheritVarsExecute <- function(data,vars,prefix,postfix,onlyvars=FALSE) {

   # get all variable names and list names
   nmesvar = c()
   nmeslst = c()
   for(nme in names(data)) {
      if(is.list(data[[nme]])){nmeslst = c(nmeslst,nme)}else{nmesvar = c(nmesvar,nme)}
   }

   # copy all variables of data of the lowest level to vars
   if (! onlyvars) {
      for(nme in nmesvar) {vars[[nme]]=data[[nme]]}
   }

   # substitute variables
   vnmes   = names(vars)
   vnmespp = paste0(prefix,vnmes,postfix)
   cont    = (length(vnmespp) > 0)
   k = 0
   while(cont) {
      cont = FALSE
      for(nme in nmesvar) {
         if(is.character(data[[nme]])) {
            if(any(grepl(prefix,data[[nme]],fixed=TRUE))){
               isel = c()
               for(i in 1:length(vnmespp)) {
                  if(grepl(vnmespp[i],data[[nme]],fixed=TRUE)) {
                     isel = c(isel,i)
                  }
               }
               if(length(isel) > 0) {
                  cont =TRUE
                  for(i in isel){
                     vnme   = vnmes[i]
                     vnmepp = vnmespp[i]
                     val    = vars[[vnme]]
                     data[[nme]] = gsub(vnmepp,val,data[[nme]],fixed=TRUE)
                  }
               }
            }
         }
      }
      # max 10 cycles
      k = k + 1
      if(k > 10){cont=FALSE}
   }

   # copy all variables of data of the lowest level to vars
   if (! onlyvars) {
      for(nme in nmesvar) {vars[[nme]]=data[[nme]]}
   }

   # next levels
   for(nme in nmeslst){data[[nme]] = Recall(data[[nme]],vars=vars,prefix=prefix,postfix=postfix,onlyvars=onlyvars)}

   # return
   return(data)
}

# ----- cfnDB -----


# some important database function, to be split up into multiple files

#' Make a selection in a list
#'
#' Make a selection of a list of data instead of converting it to a data.frame first.
#' All data in the list must have the same length.
#' This method is much faster then make the selection in a data frame and much less memory demanding, 
#' especially for large data sets. Thereby, an extended definition of the selection is implemented.
#' @param    structure  data structure, list or data.frame
#' @param    sel        selection definition. see details section.
#' @details The `sel` parameter may be a list or a character. They cannot be used both.
#'
#' **character**
#'
#' If `sel` is a character string then it is assumed to be a selection statement using 
#' relational (==, <=, etc.) and logical (&, |) operations. This expression is evaluated 
#' within the environment of the data object `structure`.
#'
#' **list**
#'
#' The \code{sel} definition should contain one of:
#'    \itemize{
#'       \item logical; for each row in the list a value TRUE (select) or FALSE (not selected). If the logical array is shorter than the data length then it is recycled.
#'       \item numeric; row numbers to be selected, positive numbers are the rows to be selected, negative numbers the rows to be removed. Positive and negative numbers may not be used together.
#'       \item list; A (nested) list structure. The name of each entry is
#'           \enumerate{
#'                    \item    \strong{name}     columnname of structure, this may be an\cr
#'                                    array: contains values which are to be retained for name\cr
#'                                    list : continuing structure\cr
#'                                           if this list contains multiple entries, these entries are\cr
#'                                           combined by an \code{.and.} operator.\cr
#'                    \item    \strong{relational operator}, this operator is applied to the last found columnname\cr
#'                           in the sel list\cr
#'                           list : each name in this list is one of the relational operators:\cr
#'                               .lt.    values <    name$.lt.\cr
#'                               .le.    values <=   name$.le.\cr
#'                               .eq.    values %in% name$.eq.\cr
#'                               .ne.  ! values %in% name$.eq.\cr
#'                               .gt.    values >    name$.gt.\cr
#'                               .ge.    values >=   name$.ge.\cr
#'                               .like.  values LIKE name$.like.   (wildcards permitted)\cr
#'                               .bt.    name$bt[1] <= values <= name$bt[2]   (=between)\cr
#'                               .cnt.   contains    , select grep(name$.cnt.,values)\cr
#'                                    .cnt. may contain more values, these are combined with OR\cr
#'                               .ncnt. contains not, ! select grep(name$.ncnt.,values)\cr
#'                               All variables may contain only one value, except for .eq., which may \cr
#'                               contain multiple values, and .bt., which may conatin two values.\cr
#'                     \item    \strong{logical operator}, the value of an operator is a list\cr
#'                            .and.\cr
#'                            .or.\cr
#'                            .xor.\cr
#'                            .not.    if the .not. structure contains multiple arrays, these arays are\cr
#'                                     first coerged to one aray by an .and. operation. Then the result\cr
#'                                     is inversed by .not.\cr
#'                     \item   \strong{logical function}, the value is a character array with one or more column names to be tested. If multiple column names are given then the result is coerged by .and.\cr
#'                            .is.na.      checks for NA values in data array, returns TRUE if NA, FALSE otherwise\cr 
#'                            .not.is.na.  checks for NA values in data array, returns FALSE if NA, TRUE otherwise\cr 
#'                            .is.finite.      checks for finite values in data array, returns TRUE if finite, FALSE otherwise\cr 
#'                            .not.is.finite.  checks for finite values in data array, returns FALSE if finite, TRUE otherwise\cr 
#'                    \item    \strong{named substructure}\cr
#'                            When an entry is none of the above items it is assumed to be a substructure.\cr
#'                            The name of the substrucure has no special meaning, it is evaluated as if it \cr
#'                            was an .and. operator.\cr
#'                            To be warned for spelling errors in the names, a named substructure will issue\cr
#'                            a warning. If the first character of the name is a colon ":" then the warning\cr
#'                            is suppressed.\cr
#'           }
#'     }
#' @seealso The workhorse function is \code{\link{cfnSelListSel}}.
#' @examples
#' x   = list(ll=letters,LL=LETTERS,nrs=1:26)
#' sel = list(ll=list(.bt.=c("c","x")),nrs=list(.lt.=21))
#' cfnSelList(x,sel)
#' cfnSelList(x,c(1,3,26,18))
#' @noRd
cfnSelList <- function(structure,sel) {
   # make a selection of a list of data instead of converting it to a data.frame first
   # All data in the list must have the same length.
   # This method is much faster then make the selection in a dataframe and much less memory demanding
   #

   # evaluate selection statement
   sel = cfnSelListSel(structure,sel,na.rm=TRUE)

   # perform selection
   if (is.data.frame(structure)) {
      structure = structure[sel,]
   } else if(is.list(structure)){
      for(i in 1:length(structure)) {
         structure[[i]] = structure[[i]][sel]
      }
   } else {
      # ERROR
      warning("structure of unimplemented type: ",class(structure))
   }

   # return
   return(structure)
}


#' Evaluate List Selection Procedure
#'
#' List selection procedure as used in \code{\link{cfnSelList}}. This function returns a boolean array with which a selection on the input table can be made.
#' @param  na.rm     logical; if \code{FALSE} then \code{NA} values are retained, if \code{TRUE} then \code{NA} values are removed (if \code{sel} is numeric) or set to \code{FALSE} (if the result is a logical array).
#' @noRd
cfnSelListSel <- function(structure,sel,na.rm=TRUE) {

   # local functions
   cfnSelList.AND <- function(sel) {
      # perform logical operator AND
      ret = TRUE
      for(i in 1:length(sel)){ret = ret & sel[[i]]}
      # return
      return(ret)
   }
   cfnSelList.OR <- function(sel) {
      # perform logical operator OR
      ret = FALSE
      for(i in 1:length(sel)){ret = ret | sel[[i]]}
      # return
      return(ret)
   }
   cfnSelList.XOR <- function(sel) {
      # perform logical operator XOR
      ret = 0
      for(i in 1:length(sel)){ret = ret + sel[[i]]}
      ret = (ret == 1)
      # return
      return(ret)
   }
   cfnSelList.NOT <- function(sel) {
      # perform logical operator NOT
      # to be sure sel contains only one value or arry first perform the default AND
      ret = cfnSelList.AND(sel)
      ret = (! ret)
      # return
      return(ret)
   }
   cfnSelList.LogicalFunction <- function(lnme,sel) {
      # logical functions
      # -----------------
      # sel contains names of data columns
      ret  = list()
      cols = sel
      if (! all(cols %in% colnames)) {
         warning("Logical function ",lnme,", not all column names in structure: ",cols[! cols %in% colnames])
         cols = cols[cols %in% colnames]
      }
      if (lnme == ".is.na.") {
         for (col in cols) {
            ret[[col]] = is.na(structure[[col]])
         }
      } else if (lnme == ".not.is.na.") {
         for (col in cols) {
            ret[[col]] = ! is.na(structure[[col]])
         }
      } else if (lnme == ".is.finite.") {
         for (col in cols) {
            ret[[col]] = is.finite(structure[[col]])
         }
      } else if (lnme == ".not.is.finite.") {
         for (col in cols) {
            ret[[col]] = ! is.finite(structure[[col]])
         }
      }
      ret = cfnSelList.AND(ret)    # AND
      # return
      return(ret)
   }
   cfnSelList.RelationalOperator <- function(lnme,datarray,arg) {
      # relationale operators
      # ---------------------
      # check arg type
      if (is.numeric(datarray) & (! is.numeric(arg))) {
         arg = as.numeric(arg)
      }
      # find rop
      if     (lnme == ".lt."  ){ret = (datarray <    arg)}
      else if(lnme == ".le."  ){ret = (datarray <=   arg)}
      else if(lnme == ".eq."  ){ret = (datarray %in% arg)}
      else if(lnme == ".ne."  ){ret = (! datarray %in% arg)}
      else if(lnme == ".gt."  ){ret = (datarray >    arg)}
      else if(lnme == ".ge."  ){ret = (datarray >=   arg)}
      else if(lnme == ".like."){ret = grepl(arg,datarray)}
      else if(lnme == ".bt."  ){
         # between
         v = c(arg,NA,NA)[1:2]
         if(! any(is.na(v))){v=sort(v)}
         ret = TRUE
         if(! is.na(v[1])){ret = ret & (datarray >=   v[1])}
         if(! is.na(v[2])){ret = ret & (datarray <=   v[2])}
      } else if(lnme %in% c(".cnt.",".ncnt.")) {
         # contains (cnt) or not (ncnt)
         icnt = c()
         for (cnt in arg) {
            icnt = c(icnt,grep(cnt,datarray,invert=FALSE))
         }
         icnt = sort(unique(icnt))
         ret = rep(FALSE,length(datarray))
         ret[icnt] = TRUE
         if(lnme == ".ncnt."){ret = (! ret)} # invert selection
      }
      # convert NA to FALSE
      ret[is.na(ret)] = FALSE
      # return
      return(ret)
   }
   cfnSelList.sel <- function(datarray,sel) {
      # recursive function
      # Arguments
      #    datarray   data array to use with relational operators
      #               This array is filled with the data as soon as a column name is encountered
      #    sel        current sel level
      #               This list may contain: 1. column names         (list or array)
      #                                      2. logical operators    (list)
      #                                      3. relational operators (array)
      #                                      4. empty   this assumed to .and.
      #
      # Data structures used from outside this function
      #    structure  list with data
      #    colnames   column names of structure
      #    

      # init
      lfuns = c(".is.na.",".not.is.na.",".is.finite.",".not.is.finite.")
      lops  = c(".and.",".or.",".xor.",".not.")
      rops0 = c("lt","le","eq","ne","ge","gt","bt","cnt","ncnt","like")
      rops  = paste0(".",rops0,".")
      preropfun = ".fun"  # prefix to rops to make lnme a rop-functions

      
      for(i in 1:length(sel)) {

         nme  = names(sel)[i]

         # check or current entry of sel is named
         # if not then the entry is assumed to be .and.
         if(is.null(nme))  {nme = ".and."}
         else if(nme == ""){nme = ".and."}
         lnme = tolower(nme)


         if(nme %in% colnames) {
            # column names
            # ------------
            if(is.list(sel[[i]])){
               # list
               sel[[i]] = Recall(structure[[nme]],sel[[i]])
               sel[[i]] = cfnSelList.AND(sel[[i]])
            } else {
               # data
               sel[[i]] = (structure[[nme]] %in% sel[[i]])
            }
         } else if(lnme %in% lops) {
            # logical operators
            # -----------------
            sel[[i]] = Recall(datarray,sel[[i]])
            if     (lnme == ".and.") {sel[[i]]=cfnSelList.AND(sel[[i]])}  # AND
            else if(lnme == ".or.")  {sel[[i]]=cfnSelList.OR(sel[[i]]) }  # OR
            else if(lnme == ".xor.") {sel[[i]]=cfnSelList.XOR(sel[[i]])}  # XOR
            else if(lnme == ".not.") {sel[[i]]=cfnSelList.NOT(sel[[i]])}  # NOT
            else {warning("Logical operator ",nme," unknown!")}  # WARNING
         } else if(lnme %in% c(rops,rops0)) {
            # relational operators
            # --------------------
            # check pre and post dot in lnme
            if(lnme %in% rops0){lnme=paste0(".",lnme,".")}
            # perform selection
            sel[[i]] = cfnSelList.RelationalOperator(lnme,datarray,sel[[i]])
         } else if(lnme %in% paste0(preropfun,rops)) {
            # relational operator functions
            # -----------------------------
            lnme = sub(paste0("^",preropfun),"",lnme)
            cnme = head(sel[[i]], 1)
            arg  = tail(sel[[i]],-1)
            sel[[i]] = cfnSelList.RelationalOperator(lnme,structure[[cnme]],arg)
         } else if(lnme %in% lfuns) {
            # logical functions
            # -----------------
            sel[[i]] = cfnSelList.LogicalFunction(lnme,sel[[i]])
         } else {
            # named substructure
            if(! substr(nme,1,1) == ":"){
               # WARNING
               warning("Column ",nme," not in input structure!")
            }
            # a substructure is assumed to be evaluated as an .and. operator
            sel[[i]] = Recall(datarray,sel[[i]])
            sel[[i]] = cfnSelList.AND(sel[[i]])    # AND
         }
      }
      # return
      return(sel)
   }

   # check structure
   if (! is.list(structure)) {
      stop("Input structure is not of class list: ",class(structure))
   }

   # if sel is a list
   if(is.list(sel)) {
      colnames = names(structure)
      sel = cfnSelList.sel(datarray=NULL,sel)
      sel = cfnSelList.AND(sel)
   } else if (is.character(sel)) {
      # character
      sel = with(structure,eval(parse(text=sel)))
      # result should be a logical array of length structure[[1]]
      if (is.logical(sel)) {
         if (length(sel) != length(structure[[1]])) {
            stop("Result of selection statement has wrong length.")
         }
      } else {
         stop("Result of selection statement is not of class logical.")
      }
   }


   # remove NA values from selection
   if (na.rm) {
      if(any(is.na(sel))) {
         if(is.numeric(sel)){sel = sel[! is.na(sel)]}
         if(is.logical(sel)){sel[is.na(sel)] = FALSE}
      }
   }


   # return
   return(sel)
}


#' Order a table by columns
#'
#' Order a table by columns
#' @param   obj          list containing a data table
#' @param   cols         column name(s) which contains the group id, multiple names are allowed.
#' @seealso \code{\link{cfnSelList}}
#' @noRd
cfnOrderList <- function(obj,cols) {

   # init
   orderid     = ".order.tmp."

   # order obj
   #   add a temporaray data column to keep the data in the same order as far as possible
   obj[[orderid]] = 1:length(obj[[ cols[1] ]])

   # create an order index
   ix = do.call(order,args=obj[c(cols,orderid)])

   # order the table
   obj = cfnSelList(obj,ix)

   # remove temporary column
   obj[[orderid]] = NULL

   # return
   return(obj)
}


#' Summarize grouped data
#'
#' summarize column data which are within the same group.
#' Type of aggreagation can be defined by: sumcols, mincols, maxcols, concatenate.
#' @param   obj          list containing a data table
#' @param   groupids     column name(s) which contains the group id, multiple names are allowed.
#' @param   sumcols      column names which have to be summarized
#' @param   mincols      column names from which the minimum value has to retained
#' @param   maxcols      column names from which the maximum value has to retained
#' @param   meancols     column names from which the mean value has to retained
#' @param   catcols      column names from which the content has to be concatenated (character)
#'                 (see also sep)
#'           from all other columns a random value is kept within each group
#' @param   sep          field seperator for concatenate columns (see \code{catcols})
#' @noRd
cfnGroupSum <- function(obj,groupids,
                        sumcols=NULL,mincols=NULL,maxcols=NULL,meancols=NULL,catcols=NULL,sep=" ") {
   # summarize column data which are within the same group.
   # Type of aggreagation can be defined by: sumcols, mincols, maxcols, concatenate.
   # Arguments
   #    obj          list containing a data table
   #    groupids     column name(s) which contains the group id
   #    sumcols      column names which have to be summarized
   #    mincols      column names from which the minimum value has to retained
   #    maxcols      column names from which the maximum value has to retained
   #    catcols      column names from which the content has to be concatenated (character)
   #                 (see also sep)
   #           from all other columns a random value is kept within each group
   #    sep          seperator for concatenate columns \code{catcols}
   # Out
   #    same list as input but now reduced and orderd to groupid

   # init
   groupid = ".groupid.tmp."
   countid = ".count.tmp."       # needed for: meancols
#   orderid = ".order.tmp."
   allcountids = c()

   lsum  = (length(sumcols) > 0)
   lmin  = (length(mincols) > 0)
   lmax  = (length(maxcols) > 0)
   lmean = (length(meancols) > 0)
   lcon  = (length(catcols) > 0)


   # order by groupids (and add groupid)
   obj = cfnGroupOrder(obj,groupids,grpid=groupid)

   # init allcountids
   if (lmean) {
      allcountids = c(allcountids,paste0(countid,meancols))
   }

   # create count columns
   n = length(obj[[groupid]])
   for (col in allcountids) {
      obj[[col]] = rep(1,n)
   }

   # summarize
   cont = TRUE
   while(cont) {
      # init selection
      n   = length(obj[[groupid]])
      ix  = 1:(n-1)
      sel = obj[[groupid]][ix] == obj[[groupid]][ix+1]
      ix  = ix[sel]

      sel = ((ix %% 2) == 1)
      ixo = ix[sel]
      ixe = ix[! sel]
      if(length(ixo) > length(ixe)){ix = ixo}else{ix = ixe}

      if(length(ix) > 0) {
         # summarize
         if(lsum) {
            for(col in sumcols)
              {obj[[col]][ix] = obj[[col]][ix] + obj[[col]][ix+1] }
         }

         # min
         if(lmin) {
            for(col in mincols)
              {obj[[col]][ix] = pmin(obj[[col]][ix],obj[[col]][ix+1]) }
         }

         # max
         if(lmax) {
            for(col in maxcols)
              {obj[[col]][ix] = pmax(obj[[col]][ix],obj[[col]][ix+1]) }
         }

         # mean
         if(lmean) {
            for (col in meancols) {
               obj[[col]][ix] = obj[[col]][ix] + obj[[col]][ix+1]
            }
         }

         # concatenate
         if(lcon) {
            for(col in catcols)
              {obj[[col]][ix] = paste(obj[[col]][ix],obj[[col]][ix+1],sep=sep) }
         }

         # count
         for (col in allcountids) {
            obj[[col]][ix] = obj[[col]][ix] + obj[[col]][ix+1]
         }

         # remove used records
         obj = cfnSelList(obj,-(ix+1))

         # check length
         if(length(obj[[1]]) < 2){cont = FALSE}
      } else {
         cont = FALSE
      }
   }

   # calc means
   if (lmean) {
      for (col in meancols) {
         cntcol = paste0(countid,col)
         obj[[col]] = obj[[col]]/obj[[cntcol]]
      }
   }

   # remove temporary columns
   obj[[groupid]] = NULL
   for (col in allcountids) {
      obj[[col]] = NULL
   }

   # return
   return(obj)
}

#' Cumulative summarize grouped data
#'
#' Cumulative summarize column data which are within the same group.
#' @param   obj          list containing a data table
#' @param   groupids     column name(s) which contains the group id, multiple names are allowed.
#' @param   sumcols      column names which have to be summarized
#' @param   skip.na      skip NA values in cumsum. If TRUE then NA-values are treated as 0 during cumsum but in the results are still NA.
#'                       If FALSE then alle values after an NA value are NA too.
#' @seealso \code{\link{cfnGroupSum}}
#' @noRd
cfnGroupCumsum <- function(obj,groupids,sumcols=NULL,skip.na=FALSE) {
   # summarize column data which are within the same group.
   # Out
   #    same list as input but now reduced and orderd to groupid

   # init
   grpid   = ".groupid.tmp."
   firstid = ".first.group.record.tmp."
#   countid = ".count.tmp."       # needed for: meancols
#   allcountids = c()

   lsum  = (length(sumcols) > 0)

   # order by groupids (and add groupid)
   obj = cfnGroupOrder(obj,groupids,grpid=grpid)

   # add first_group_rec to obj
   obj = cfnGroupFirst(obj,grpid,firstid,grpid=grpid)

   # summarize for each sumcol
   selfirst = obj[[firstid]]
   n = length(selfirst)
   if (n > 1) {
      I = 2:n
      for (sumcol in sumcols) {
         val    = obj[[sumcol]]
         naval  = is.na(val)
         if (skip.na){val[naval] = 0}
         valcum = rep(NA,n)
         valcum[selfirst] = val[selfirst]
         #
         sel = is.na(valcum[-1])
         nlast = n+1
         ncurr = sum(sel)
         iter = 0  # noodstop
         while (nlast > ncurr & iter < n) {
           # opvragen NA velden
            ii = I[sel]
            valcum[ii] = valcum[ii-1] + val[ii]
            # next
            sel   = is.na(valcum[-1])
            nlast = ncurr
            ncurr = sum(sel)
            iter = iter + 1
         }
         # set NA values back to result
         valcum[naval] = NA
         # overschrijven
         obj[[sumcol]] = valcum
      }
   }

   # remove temporary columns
   obj[[grpid]]   = NULL
   obj[[firstid]] = NULL

   # return
   return(obj)
}

#' Combine multiple columns to one groupid
#'
#' Combine multiple columns to one groupid
#' @param   obj          list containing a data table
#' @param   groupids     column name(s) which contains the group id, multiple names are allowed.
#' @param   grouid       column nmae of combined data
#' @seealso \code{\link{cfnGroupSum}}, \code{\link{cfnGroupFirst}}, \code{\link{cfnGroupLast}}, \code{\link{cfnGroupOrder}}
#' @noRd
cfnAddGroupId <- function(obj,groupids,grpid=".groupid.tmp.") {

   # init
   sep = "_"

   # create grpid (temporary name)
   gid = ""
   for(nme in groupids) {
      gid = paste(gid,obj[[nme]],sep=sep)
   }
   obj[[grpid]] = gid

   # return
   return(obj)
}

#' Order a table by groupids
#'
#' Order a table by groupid. The record order within a group is preserved.
#' @param   obj          list containing a data table
#' @param   groupids     column name(s) which contains the group id, multiple names are allowed.
#' @param   grpid        column name of combined groupids. If not NULL then this column will be added to the output.
#' @seealso \code{\link{cfnGroupSum}}, \code{\link{cfnGroupFirst}}, \code{\link{cfnGroupLast}}
#' @noRd
cfnGroupOrder <- function(obj,groupids,grpid=NULL) {

   # init
   grpid_def = ".groupid.tmp."
   orderid   = ".order.tmp."
   sep       = "_"

   # determine grpid_use name
   grpid_use = c(grpid,grpid_def)[1]

   # create grpid (if not exists)
   if (! grpid_use %in% names(obj)) {
      obj = cfnAddGroupId(obj,groupids,grpid=grpid_use)
   }

   # order obj
   #   add a temporaray data column to keep the data in the same order as far as possible
   obj[[orderid]] = 1:length(obj[[grpid_use]])
   ix = order(obj[[grpid_use]],obj[[orderid]])
   obj = cfnSelList(obj,ix)

   # remove temporary columns
   if (is.null(grpid)) {
      obj[[grpid_use]] = NULL
   }
   obj[[orderid]] = NULL

   # return
   return(obj)
}


#' Find the first record of a group
#'
#' Find the first record of a group. The output list is ordered on groupid. The record order within a group is preserved.
#' @param   obj          list containing a data table
#' @param   groupids     column name(s) which contains the group id, multiple names are allowed.
#' @param   firstrec     name of the column to be added containing a boolean indicating the first record in a group
#' @param   grpid        column name of combined groupids. If not NULL then this column will be added to the output.
#' @seealso \code{\link{cfnGroupSum}}, \code{\link{cfnGroupLast}}, \code{\link{cfnGroupOrder}}
#' @noRd
cfnGroupFirst <- function(obj,groupids,firstrec,grpid=NULL) {

   # init
   grpid_def = ".groupid.tmp."

   # determine grpid_def name
   grpid_use = c(grpid,grpid_def)[1]

   # order the list by groupids
   obj = cfnGroupOrder(obj,groupids,grpid_use)

   # find the first values
   n = length(obj[[grpid_use]])
   if (n > 1) {
      ii = 2:n
      sel = c(TRUE,obj[[grpid_use]][ii] != obj[[grpid_use]][ii-1])
   } else {
      sel = rep(TRUE,n)
   }
   obj[[firstrec]] = sel

   # remove grpid_use
   if (is.null(grpid)) {
      obj[[grpid_use]] = NULL
   }

   # return
   return(obj)
}

#' Find the last record of a group
#'
#' Find the last record of a group. The output list is ordered on groupid. The record order within a group is preserved.
#' @param   obj          list containing a data table
#' @param   groupids     column name(s) which contains the group id, multiple names are allowed.
#' @param   lastrec      name of the column to be added containing a boolean indicating the last record in a group
#' @param   grpid        column name of combined groupids. If not NULL then this column will be added to the output.
#' @seealso \code{\link{cfnGroupSum}}, \code{\link{cfnGroupFirst}}, \code{\link{cfnGroupOrder}}
#' @noRd
cfnGroupLast <- function(obj,groupids,lastrec,grpid=NULL) {

   # init
   grpid_def = ".groupid.tmp."

   # determine grpid_use name
   grpid_use = c(grpid,grpid_def)[1]

   # order the list by groupids
   obj = cfnGroupOrder(obj,groupids,grpid_use)

   # find the last values
   n = length(obj[[grpid_use]])
   if (n > 1) {
      ii = 2:n
      sel = c(obj[[grpid_use]][ii] != obj[[grpid_use]][ii-1],TRUE)
   } else {
      sel = rep(TRUE,n)
   }
   obj[[lastrec]] = sel

   # remove grpid_use
   if (is.null(grpid)) {
      obj[[grpid_use]] = NULL
   }

   # return
   return(obj)
}


#' Make a list unique
#'
#' Make a list or data.frame unique based on given column names
#' @param  obj         list or data.frame data structure
#' @param  cols        columns to be used to detect duplicates.
#'                     If \code{NA}  all columns are used
#' @noRd
cfnUnique <- function(obj,cols=NA) {
   # Make a list or data.frame unique based on given column names

   # init
   class = class(obj)
   if(all(is.na(cols))){cols=names(obj)}


   # combine columns
   if(length(cols) == 1) {
      vals = obj[[cols[1]]]
   } else {
      vals=""
      for(col in cols){vals = paste(vals,obj[[col]])}
   }

   # find unique records
   sel = ! duplicated(vals)


   # Make selection
   if(class == "list")           {obj = cfnSelList(obj,sel)}
   else if(class == "data.frame"){obj = obj[sel,]}

   # return
   return(obj)
}


#' Merge two list over a common column
#'
#' Merge two list over a common column
#' @param    list1     input list 1
#' @param    col1      column name in \code{list1} to be compared to \code{col2}
#' @param    list2     input list 2
#' @param    col2      column name in \code{list2} to be compared to \code{col1}. If \code{NULL} \code{col1} is used.
#' @param    addcols   columns of \code{list2} which data has to be added to \code{list1}. If \code{NA} then all columns of \code{list2} are added.
#' @param    all.x     TRUE  records with no match, NA will be output data
#'                    output number of records equals to number of records in list1
#'              FALSE records with no match will be removed from list1
#'                    output number of records equals to the intersection of list1 and list2
#' @param    prefix    add the prefix to the column names of addcols
#'              This is usefull when the column names of \code{list2} are available in \code{list1} and
#'              may not be overwritten
#' @seealso \code{\link{cfnCat}}
#' @noRd
cfnMerge <- function(list1,col1,list2,col2=NULL,addcols=NA,all.x=FALSE,prefix=NA) {
   # Merge two lists over col1 vs col2
   # ---------------------------------
   # Add columns 'addcols' from 'list2' to 'list1' by joining
   # the tables over 'col1' of 'list1' and 'col2' of 'list2'
   # The first match of 'col1' in 'col2' will be used!
   # Arguments
   #    list1     
   #    col1
   #    list2
   #    col2
   #    addcols   columns of list2 which data has to be added to list1
   #    all.x     TRUE  records with no match, NA will be output data
   #                    output number of records equals to number of records in list1
   #              FALSE records with no match will be removed from list1
   #                    output number of records equals to the intersection of list1 and list2
   #    prefix    add the prefix to the column names of addcols.
   #              This is usefull when the column names are available in list1 and
   #              may not be overwritten
   # See also
   #    cfnCat()

   # init
   vsep = "_:_"                                     # value separator if multiple columns (col1 or col2) have to be combined into 1, just some weird string
   if (any(is.na(col2))) {col2 = col2[! is.na(col2)]}
   if (is.null(col2)) {col2 = col1}                 # default value for col2
   if(all(is.na(addcols))){addcols=names(list2)}    # default value for addcols
   if(is.na(prefix)){prefix=""}

   # remove col2 columns from addcols
   sel     = (addcols %in% col2)
   addcols = addcols[! sel]

   # combine columns if necesary
   if (length(col1) > 1) {
      rmucol1 = TRUE
      ucol1   = paste("_",col1,"_",collapse=vsep)
      list1[[ucol1]] = list1[[col1[1]]]
      for (col in col1[-1]) {list1[[ucol1]] = paste(list1[[ucol1]],list1[[col]],sep=vsep)}
   } else {
      rmucol1 = FALSE
      ucol1   = col1
   }
   #
   if (length(col2) > 1) {
      rmucol2 = TRUE
      ucol2   = paste("_",col2,"_",collapse=vsep)
      list2[[ucol2]] = list2[[col2[1]]]
      for (col in col2[-1]) {list2[[ucol2]] = paste(list2[[ucol2]],list2[[col]],sep=vsep)}
   } else {
      rmucol2 = FALSE
      ucol2   = col2
   }

   # 
   nin     = length(list1[[ucol1]])

   # find match indices
   ix      = match(list1[[ucol1]],list2[[ucol2]])

   # what to do with records without a match
   if(! all.x) {
      # remove records from list1 without a match in list2
      sel   = (! is.na(ix))
      list1 = cfnSelList(list1,sel)
      ix    = ix[sel]
   }

   # add columns to list1
   for(col in addcols){list1[[paste0(prefix,col)]] = list2[[col]][ix]}

   # count records
   nout = length(list1[[ucol1]])
   if(nin > nout){cat("Records lost! Number of records on input:",nin,"  on output:",nout,"  lost:",nin-nout,"\n")}

   # remove ucol1 and ucol2
   if (rmucol1) {list1[[ucol1]]=NULL}
   if (rmucol2) {list2[[ucol2]]=NULL}  # niet echt noodzakelijk

   # return
   return(list1)
}


#' Lookup table
#'
#' This function searches values in a table and returns the corresponding values of a specified column.
#' @param   sv     search value
#' @param   tab    lookup table
#' @param   svcol  name of column in \code{tab} to match the search value \code{sv}
#' @param   retcol name of the column of \code{tab} which values have to be returned
#' @param   na     if ! is.na(na) then replace \code{NA} by na
#' @noRd
cfnLookup <- function(sv,tab,svcol,retcol,na=NA) {
   # Lookup table
   # Arguments
   #    sv     search value
   #    tab    lookup table
   #    svcol  name of column in tab to match the search value
   #    retcol name of the column of tab which values have to be returned
   #    na     if ! is.na(na) then replace NA by na

   ix     = match(sv,tab[[svcol]])
   retval = tab[[retcol]][ix]

   # replace missing values
   if(! is.na(na)) {
      sel         = is.na(retval)
      retval[sel] = na
   }

   # return
   return(retval)
}


#' Replace factors by arrays
#'
#' Replace all columns of \code{obj} of class 'factor' by simple values arrays.
#' @param    obj      structure (list) from which cactor columns are replaced
#' @param    columns  names of the columns to be transformed. If \code{NA} then all columns of class 'factor' will be processed.
#' @noRd
cfnUnFactor <- function(obj,columns=NA) {
   # transform factor columns to plain arrays

   # when no column names given, process all columns
   if(all(is.na(columns))){columns=names(obj)}

   # process
   for(col in columns) {
      if(class(obj[[col]]) == "factor")
         {obj[[col]] = levels(obj[[col]])[obj[[col]]] }
   }

   # return
   return(obj)
}

# ----- cfnLoad -----

#' Load a data structure
#'
#' Load a data structure from file <path>/<name>.RData
#' @param   name     name of the structure, also bodyname of the file (without directory and extension)
#'                   Only load the structure if it not already exists
#' @param   path     directory to load the data from, multiple directories may be given. 
#'                   path defaults to LUE$cfn$dataPath or c("data",".") (see \code{\link{cfnFileName}})
#' @seealso \code{\link{cfnFileName}} \code{\link{cfnSave}} \code{\link{cfnAutoSave}} \code{\link{cfnAutoLoad}}
#' @noRd
cfnLoad <- function(name,path=NA) {
   # load a data structure from file <path>/<name>.RData
   # Only load the structure if it not already exists
   #
   # Arguments
   #    name     
   #    path     directory to read data from
   #             path defaults to LUE$cfn$dataPath or c("data",".") (see cfnFileName)


   # load
   rname = name
   if(! exists(name)) {
      filename = cfnFileName(name,path,check=TRUE)
      if(is.na(filename)) {
         message("cfnLoad: Input file for dataset ",name," not found!")
         return(NULL)
      }
      rname = load(filename)
      if(length(rname) > 1) {
         cat(paste("   Multiple structures found in file",filename,"\n"))
         cat(paste("  ",rnmes,"\n",collapse=" "))
         cat("   only the first structure is retained\n")
         rname = rname[1]
      }
      cat(paste0("Load structure ",rname," from file ",filename,"...\n",sep=""))
   }

   return(get(rname))
}


#' Check availability of data for loading
#'
#' Check availability of data for loading with \code{\link{cfnLoad}} or \code{\link{cfnAutoLoad}}
#' @param   name     name of the structure, also bodyname of the file (without directory and extension)
#'                   Only load the structure if it not already exists
#' @param   path     directory to load the data from, multiple directories may be given. 
#'                   path defaults to LUE$cfn$dataPath or c("data",".") (see \code{\link{cfnFileName}})
#' @seealso \code{\link{cfnFileName}} \code{\link{cfnSave}} \code{\link{cfnAutoSave}} \code{\link{cfnLoad}} \code{\link{cfnAutoLoad}}
#' @return The function returns an integer value: 
#'         0: data set not found and data not currently loaded,
#'         1: data file exists,
#'         2: data currently loaded,
#'         3: data file exists and data currently loaded.
#' The returned value can be used as a boolean, where 0 is FALSE means no data available, and 1,2 or 3 is TRUE otherwise.
#' @noRd
cfnLoadCheck <- function(name,path=NA) {

   # init
   ret = 0L

   # check
   if(exists(name)) {
      # data currently loaded
      ret = ret + 2L
   }

   filename = cfnFileName(name,path,check=TRUE)
   if(! is.na(filename)) {
      # data file exists
      ret = ret + 1L
   }

   # return
   return(ret)
}

# ----- cfnSave -----


#' Save a data structure
#'
#' Save a data structure to file <path>/<name>.RData
#' @param   name     name of the structure
#' @param   path     directory to save data to
#'                   path defaults to LUE$cfn$dataPath or c("data",".") (see \code{\link{cfnFileName}})
#' @seealso \code{\link{cfnFileName}}, \code{\link{cfnLoad}}, \code{\link{cfnSaveAs}}, \code{\link{cfnAutoSave}}, \code{\link{cfnAutoLoad}}
#' @noRd
cfnSave <- function(name,path=NA) {
   # Save a data structure to file <path>/<name>.RData
   #
   # Aruments
   #    name     name of the structure
   #    path     directory to save data to


   # parent frame
   pf = parent.frame()

   if(exists(name,where=pf)) {
      filename = cfnFileName(name,path)
      cat(paste0("Save structure ",name," to file ",filename,"...\n"))
      save(list=name,file=filename,envir=pf)
   } else if(exists(name,inherits=TRUE)) {
      filename = cfnFileName(name,path)
      cat(paste0("Save structure ",name," to file ",filename,"...\n"))
      save(list=name,file=filename)
   } else {
      message("Structure ",name," not found.")
   }

   return()
}

#' Save an object under a different name
#'
#' Save an object under a different name to file <path>/<name>.RData
#' @param   obj      object to be stored
#' @param   name     name of the structure
#' @param   path     directory to save data to
#'                   path defaults to LUE$cfn$dataPath or c("data",".") (see \code{\link{cfnFileName}})
#' @seealso \code{\link{cfnFileName}}, \code{\link{cfnLoad}}, \code{\link{cfnSave}}, \code{\link{cfnAutoSave}}, \code{\link{cfnAutoLoad}}
#' @noRd
cfnSaveAs <- function(obj,name,path=NA) {

   assign(name,obj)
   cfnSave(name,path=path)
   rm(list=name)

   return(invisible())
}

