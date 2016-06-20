OBJS=Version.o Diff.o XmlDiff.o XmlDiff1.o XmlDiff2.o Main.o
SYSLIBS=-syslib net
INCLUDES=-i../..

diff:Main.o XmlDiff.o ../XML/Types.o ../XML/Generator.o ../XML/Parser.o XmlDiff1.o XmlDiff2.o Diff.o ../Monad/MonadIO.o ../Monad/MonadT.o ../HTML/HtmlFragment.o ../HTML/HtmlFragmentT.o ../HTML/DOM.o ../HTML/Diff.o ../HTML/Filter.o ../HTML/MonadHtml.o ../Parser/Parser.o ../HTTP/Types.o ../Arrow/Runnable.o ../XML/Dom.o ../Monad/ParserT.o ../Monad/MonadParser.o ../XML/MonadDom.o ../XML/DomT.o ../HTML/Types.o Version.o ../Monad/MonadState.o ../Monad/MonadControl.o ../Server/Types.o ../HTTP/Templates.o
	ghc -o d2 Main.o XmlDiff.o ../XML/Types.o ../XML/Generator.o ../XML/Parser.o XmlDiff1.o XmlDiff2.o Diff.o ../Monad/MonadIO.o ../Monad/MonadT.o ../HTML/HtmlFragment.o ../HTML/HtmlFragmentT.o ../HTML/DOM.o ../HTML/Diff.o ../HTML/Filter.o ../HTML/MonadHtml.o ../Parser/Parser.o ../HTTP/Types.o ../Arrow/Runnable.o ../XML/Dom.o ../Monad/ParserT.o ../Monad/MonadParser.o ../XML/MonadDom.o ../XML/DomT.o ../HTML/Types.o Version.o ../Monad/MonadState.o ../Monad/MonadControl.o ../Server/Types.o ../HTTP/Templates.o -syslib net -syslib template-haskell

include ../../config/make.conf

