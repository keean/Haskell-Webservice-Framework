-- main.hs (C)2001 Keean Schupke
--
--      main program, uses DOM module 

module Main(main) where
--import System.IO.Unsafe (unsafeInterleaveIO)
--import Data.Char
import System.IO
import Lib.MetaSearch.Parser
import Lib.MetaSearch.DOM
import Lib.MetaSearch.Filter
import Lib.MetaSearch.Forest
import Lib.MetaSearch.MetaSearch

------------------------------------------------------------------------------
-- do the search

main :: IO ()
main = do
    (_fh,dc) <- getDocument (MkElem (Document,"file:./propertyFinder.html",[]))
    rs <- return $ fTidyDOM dc
    fi <- case filterDOM (do
            (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            select [2]
            cut 1
            (graft . elemIs) (MkElem (Tag,"TR",[]))
            select [1]
            cut 1
            ) rs of
        DOMOut _ d -> return d
        DOMVoid -> error "initial filter failed"
    (a,fj) <- case filterDOM (do
            (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            select [3]
            cut 1
            (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            select [0]
            cut 1
            (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            select [0]
            cut 1
            (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            discardBranches 3
            -- select [2]
            -- cut 1
            -- (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            -- discardBranches 1
            -- rep
            (from . elemIs) (MkElem (Tag,"TABLE",[MkAttribute ("class","blue")]))
            (from . elemIs) (MkElem (Tag,"TD",[]))
            (from . elemIs) (MkElem (Tag,"TD",[]))
            (from . elemIs) (MkElem (Tag,"TD",[]))
            pr <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
            bd <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
            (from . elemIs) (MkElem (Tag,"TABLE",[]))
            (from . elemIs) (MkElem (Tag,"TD",[]))
            dt <- (moveExc . elemIs) (MkElem (Tag,"IMG",[]))
            im <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
            dc' <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
            (from . elemIs) (MkElem (Tag,"TD",[]))
            (from . elemIs) (MkElem (Tag,"TD",[]))
            (from . elemIs) (MkElem (Tag,"TD",[]))
            ad <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
            ag <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
            (from . elemIs) (MkElem (Tag,"TD",[]))

            return ((getLink "href" dt,getImage "src" im,extractLiteral dc'),(extractLiteral ad,extractLiteral ag,extractPrice pr, extractString bd))
            ) rs of
        DOMOut a d -> return (a,d)
        DOMVoid -> error "property parse failed"
    (_b,_fk) <- case filterDOM (do
            (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            select [2]
            cut 1
            cut 1
            (graft . elemIs) (MkElem (Tag,"A",[]))
            l <- filterLinks
            l `seq` return l) rs of
        DOMOut a' d -> return (a',d)
        DOMVoid -> error "link parse failed"
    hPutStrLn stderr (showString (generateXML fj) "\n")
    --hPutStrLn stderr (showString (generateXML fk) "\n")
    print a
    hPutStrLn stderr $ (case parseResult (matchParser (stringNcs "More than") char) (extractString fi) of
        Ok _ _ -> "false"
        _ -> "true")
    print $ gotAllPFProperties fi
