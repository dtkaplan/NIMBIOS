\name{collaborate}
\alias{collaborate}
\title{Simultaneous Collaborate Editing}
\usage{
collaborate(group = "mosaic-web", project = "CS121", doc,
  localdoc = paste(doc, "Rmd", sep = "."), buffermode = c("Markdown", "r"))
}
\arguments{
  \item{doc}{A character string naming the document in the
  collaborative editor. It's best if this is simple and
  memorable, so you can communicate it to your
  collaborators.  Don't use spaces.}

  \item{localdoc}{A character string naming the file on
  your R-system which will be synchronized to the
  collaborative buffer when you give the \code{"capture"}
  argument.  By default, this is set to be the same as
  \code{doc}.  But you can use any document that you're
  willing to overwrite.  If it's to be a new document, give
  the name (including relative path) of that document as
  \code{localdoc}.  If it's an existing document, you can
  give the name or use \code{file.choose()} to select it.
  REMEMBER the file extension.}

  \item{group}{A character string naming the Firepad group
  in which the document will be housed.  By default, this
  is \code{"mosaic-web"}, which was set up for experimental
  testing.  IT SHOULD BE TURNED INTO AN OPTION setting.}

  \item{project}{A character string naming the project.  By
  default, for COMP-121 it's set to \code{"CS121"}.  THIS
  SHOULD BE TURNED INTO AN OPTION.}

  \item{buffermode}{A string, either \code{"Markdown"} or
  \code{"r"} or some other allowable mode for the
  CodeMirror editor.}
}
\value{
A function that can be called with arguments \code{"edit"}
to bring up the collaborative browser editor, \code{"knit"}
to run an Rmd document through knitr/markdown,
\code{"source"} to source the file into the global
environment, \code{"capture"} to save the collaborative
buffer to the local file (given by the argument
\code{localdoc}), or \code{"info"} to give various
information about the buffer.
}
\description{
Support for collaborative editing in Rmd and R formats
}
\details{
Support for Simultaneous Collaborative Editing
}

