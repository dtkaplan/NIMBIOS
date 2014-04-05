#' Support for Simultaneous Collaborative Editing
#' 
#' @title Simultaneous Collaborate Editing
#' @description Support for collaborative editing in Rmd and R formats
#' @param doc A character string naming the document in the collaborative editor.
#' It's best if this is simple and memorable, so you can communicate it to your
#' collaborators.  Don't use spaces.
#' @param localdoc A character string naming the file on your R-system which will
#' be synchronized to the collaborative buffer when you give the \code{"capture"} 
#' argument.  By default, this is set to be the same as \code{doc}.  But you can use any
#' document that you're willing to overwrite.  If it's to be a new document, give
#' the name (including relative path) of that document as \code{localdoc}.  If it's 
#' an existing document, you can give the name or use \code{file.choose()}
#' to select it.  REMEMBER the file extension.
#' @param group A character string naming the Firepad group in which the document 
#' will be housed.  By default, this is \code{"mosaic-web"}, which was
#' set up for experimental testing.  IT SHOULD BE TURNED INTO AN OPTION setting.
#' @param project A character string naming the project.  By default, for COMP-121
#' it's set to \code{"CS121"}.  THIS SHOULD BE TURNED INTO AN OPTION.
#' @param buffermode A string, either \code{"Markdown"} or \code{"r"} or some other allowable mode 
#' for the CodeMirror editor.
#' @return A function that can be called with arguments \code{"edit"} to bring up
#' the collaborative browser editor, \code{"knit"} to run an Rmd document through knitr/markdown,
#' \code{"source"} to source the file into the global environment, \code{"capture"} to save 
#' the collaborative buffer to the local file (given by the argument \code{localdoc}),
#' or \code{"info"} to give various information about the buffer.
#' @export
collaborate <- function(group="mosaic-web",project="CS121",doc,
                        localdoc=paste(doc,"Rmd",sep="."),
                        buffermode=c("Markdown","r")) {
  # Test whether localdoc already exists and ask if they want to override it.
  
  # To be implemented.
  
  buffermode <- match.arg(buffermode)
  
  if( !require(RCurl) ) stop("Must install 'RCurl' package.")
  if( !require(markdown) ) stop("Must install 'markdown' package.")
  if (missing(doc))
    stop("Must specify name of collaborative buffer as the 'doc' argument.")
  docURL <- paste('https://',group,
                  '.firebaseio.com/',project,'/',
                  doc,'/first/.json?pretty=TRUE',sep='')
  editBufferURL <- paste("http://www.mosaic-web.org/go/firepad/examples/teamedit.html?project=",project,"&doc=",doc,"&mode=",buffermode,sep="")
  synchronizeBufferURL <- paste("http://www.mosaic-web.org/go/firepad/examples/updateFirepad.html?project=",project,"&doc=",doc,sep="")
  f = function(what=c("source","capture","knit","edit","info"),local=FALSE,...) {
    # browseURL(synchronizeBufferURL)
    # cat("synchronizing ...")
    # Sys.sleep(2)
    # Read in the contents of the collaborative editor
    content <- getURL(docURL)
    # Get rid of the opening and closing quotes
    content <- substr(content,2,nchar(content)-1)
    content <- gsub("\\\\n", "\n", content) # Should be restricted to being outside of quotes
    content <- gsub('\\\\','',content) # kill the escapes on the escaped quotes
    what <- match.arg(what) # what to do
    switch(what,
           source = {if(local) eval(parse(text=content), ...)
                     else eval(parse(text=purl(text=content)),
                               envir=globalenv(),...) },
           knit = { tmpNames <- 
                      paste("team-edit-",project,"-",doc,
                            c(".html",".md",".Rmd"),sep="")
                    htmlName <- tmpNames[1]
                    mdName <- tmpNames[2]
                    rmdName <- tmpNames[3]
                    writeLines(content, rmdName)
                    knit2html( rmdName, output=mdName )
                    markdownToHTML( mdName, output=htmlName )
                    browseURL(htmlName)
                    #rstudio::viewer(htmlName,height=500)
           },
           edit = { browseURL(editBufferURL) },
           capture = {  cat(content,file=localdoc)
                        file.edit(localdoc)
           },
           info = {return(list(project=project,doc=doc,localdoc=localdoc)) }
    )
  }
  return(f)
}


