-- Haskell HTML/SQL module
-- Copyright (C) 2002 Keean Schupke

module Modules.Issues.App (initialise) where

import Monad
import IO
import FiniteMap
--import Exception
import Concurrent
import Maybe
import Int

import Lib.Monad.MonadT
import Lib.Server.Types
import Lib.Server.Error
import Lib.XML.Types
import Lib.XML.DOM
import Lib.HTML.Types
import Lib.HTML.MonadHtml
import Lib.HTML.HtmlFragment
-- import Lib.HTML.HtmlFragmentT
import Lib.HTML.MenuBar
import Lib.HTML.Colours
import Lib.HTTP.Headers
import Lib.DBC.HIODBC
import Lib.DBC.HSQL
import Lib.DBC.Query

type DbcState = MVar (Maybe SqlDbcBuf)

initialise :: IO Application
initialise = do
	hPutStr stderr "initialise issues...\n"
	dbc <- newMVar Nothing
	waitSync <- newEmptyMVar
	doneSync <- newEmptyMVar
	return ("/issues.html",AppMethods {
		start = appStart dbc waitSync doneSync,
		stop = appStop waitSync doneSync,
		request = appRequest dbc
	})
	where

	appStart :: DbcState -> MVar () -> MVar () -> IO ()
	appStart dbc wait done = do
		forkIO do
			connectToDb connection (\dbcbuf -> do
				swapMVar dbc (Just dbcbuf)
				readMVar wait
				swapMVar dbc Nothing
				putMVar done ())
		return ()
		where
	
		connection :: (SqlDsn,SqlUid,SqlAuth)
		connection = ("Issues","","")
	
	appStop :: MVar () -> MVar () -> IO ()
	appStop wait done = do
		putMVar wait ()
		takeMVar done
		return ()

	toAttributes :: [(String,String)] -> [Attribute]
	toAttributes nv = map (\a -> MkAttribute a) nv

	appRequest :: DbcState -> RequestHandler
	appRequest dbcmv req response = (do
		maybeDbcBuf <- readMVar dbcmv
		case maybeDbcBuf of
			Just (dbc) -> issuesPage dbc
			Nothing -> write response $ errorReport "Database not available.") where

		submitPadded :: String -> [(String,String)] -> HtmlFragment ()
		submitPadded s args = htmlNoULink (url req) (setAttributes (toAttributes args) (toAttributes $ fmToList (form req))) (htmlNobrPad s)

		submit :: [(String,String)] -> [Attribute] -> HtmlFragment () -> HtmlFragment ()
		submit args at frag = (attrNoULink (url req) at (setAttributes (toAttributes args) (toAttributes $ fmToList (form req))) (frag))

		issuesPage :: SqlDbcBuf -> IO ()
		issuesPage dbc = (do
			sqlStmt dbc (\stb -> do
				write response (do
					--htmlText "HTTP/1.0 200 OK\n"
					--htmlHeaders [MkAttribute ("Set-Cookie","TEST=cookie-value")]
					htmlHead (do
						htmlStyle (do 
							-- removed font-size-adjust:0.5;
							htmlText ("\nTD {font-family:arial,helvetica,sans-serif;" ++ defaultFontSize (headers req) ++"}\n")
							htmlText ("TD.menu-selected {color:#ffffff;font-family:arial,helvetica,sans-serif;" ++
								defaultFontSize (headers req) ++"}\n")
							htmlText ("TD.sort-selected {color:#000000;font-family:arial,helvetica,sans-serif;" ++ 
								defaultFontSize (headers req) ++"}\n")
							htmlText ("BODY {font-family:arial,helvetica,sans-serif;" ++ defaultFontSize (headers req) ++"}\n")
							htmlText ("A.light {color:#cccccc}\n"))
						htmlTitle "Project Tracker")
					attrBody [MkAttribute ("bgcolor","#ffffff"),MkAttribute ("link","#3366cc"),MkAttribute ("vlink","#3366cc")] (
						attrTable [MkAttribute ("width","100%"),MkAttribute ("height","100%"),MkAttribute ("cols","1")] (do
							htmlTR (htmlTD (do
								selPage <- menuBar (MenuColours {
										menuUnselectedBackground="#cccccc",
										menuUnselectedLowlight="#999999",
										menuUnselectedHighlight="#ffffff",
										menuSelectedBackground="#3366cc",
										menuSelectedForeground="#ffffff",
										menuSelectedLowlight="#333399",
										menuSelectedHighlight="#ccccff",
										menuDataBackground="#ffffff"
									}) "issues-menu" [
										(htmlB (htmlNobrPad "Projects"),return statusPage stb),
										(htmlB (htmlNobrPad "Issues"),return htmlNull :: HtmlFragmentT IO ()),
										(htmlB (htmlNobrPad "Comments"),return htmlNull :: HtmlFragmentT IO ()),
										(htmlB (htmlNobrPad "Timeline"),return htmlNull :: HtmlFragmentT IO ()),
										(htmlB (htmlNobrPad "Headers"),return $ htmlSmall $ showHeaders $ fmToList (headers req) :: HtmlFragmentT IO ())
									] (return htmlNull :: HtmlFragmentT IO ()) (url req) (form req)
								selPage) :: HtmlFragmentT IO ())
							htmlTR (attrTD [MkAttribute ("valign","bottom")] (
								attrTable [MkAttribute ("width","100%"),MkAttribute ("cols","1")] (do
									htmlTR (attrTD [MkAttribute ("bgcolor","#3366CC"),MkAttribute ("width","100%"),
										MkAttribute ("height","2")] htmlNullImg)
									htmlTR (attrTD [MkAttribute ("align","right")] $ htmlSmall $ attrFont [MkAttribute ("color","#cccccc")] $ htmlNobrText "&copy; 2002 Fry-IT Ltd."))))))))) where

		statusPage :: SqlStBuf -> HtmlFragmentT IO ()
		statusPage stb = (case getOrder of
			ord -> (do
				proj <- htmlProjects stb ord
				return (do
					htmlBR
					htmlCenter (do
						attrTable [MkAttribute ("width","95%"),MkAttribute ("cellspacing","1")] (do
							htmlTR (do
								attrTD [MkAttribute ("valign","middle"),MkAttribute ("colspan","6"),MkAttribute ("width","100%")] 
									(attrTable [MkAttribute ("width","100%")] $ htmlTR (do
										attrTD [MkAttribute ("valign","middle"),MkAttribute ("width","100%")]
											(htmlBig $ htmlB $ htmlNobrText "Project Status Summaries")
										priorityKey ord)))
							hRule
							htmlTR (attrTD [MkAttribute ("colspan","6"),MkAttribute ("width","100%"),
								MkAttribute ("height","2")] htmlNullImg)
							htmlTR (attrTD [MkAttribute ("colspan","6"),MkAttribute ("width","100%")] (showSort ord))
							hRule
							proj))))) where

			hRule :: HtmlTableFragment ()
			hRule = do
				htmlTR (attrTD [MkAttribute ("colspan","6"),MkAttribute ("width","100%"),
					MkAttribute ("height","2")] htmlNullImg)
				htmlTR (attrTD [MkAttribute ("colspan","6"),MkAttribute ("colspan","2"),MkAttribute ("width","100%"),
					MkAttribute ("bgcolor","#3366cc"),MkAttribute ("height","2")] htmlNullImg)

		getOrder :: [(String,Bool)]
		getOrder = split (lookupWithDefaultFM (form req) "project0" "sort-by") "" where

			split :: String -> String -> [(String,Bool)]
			split "" _ = []
			split (s0:ss) cs 
				| s0=='0' = (reverse cs,False):split ss ""
				| s0=='1' = (reverse cs,True):split ss ""
				| otherwise = split ss (s0:cs)

		showHeaders :: [(String,String)] -> HtmlFragment ()
		showHeaders [] = htmlNull
		showHeaders ((n,v):hs) = do
			htmlB $ htmlText n
			htmlText ": "
			htmlText v
			htmlBR
			showHeaders hs

		joinSort :: [(String,Bool)] -> String
		joinSort [] = ""
		joinSort ((s,b):ns) = s ++ (if b then "1" else "0") ++ joinSort ns

		showSort :: [(String,Bool)] -> HtmlFragment ()
		showSort ord = htmlNobr (htmlSmall (do
			if ord /= [] 
				then do
					htmlB (do
						submit [("sort-by","")] [] (htmlNbspText "Sort")
						htmlNbspText ": ")
					sort ord
				else do
					htmlB (htmlNbspText "Sort: ")
					htmlNbspText "none")) where

			sort :: [(String,Bool)] -> HtmlFragment ()
			sort [] = htmlNbspText "None"
			sort ((n,d):s) = do
				submit [("sort-by",joinSort $ truncateSort n ord)] [] (htmlNbspText n)
				htmlNbspText "-"
				submit [("sort-by",joinSort $ replace (n,not d) ord)] [] (htmlNbspText (if d then "ascending" else "descending"))
				if s == []
					then return ()
					else do
						htmlNbspText "&gt;"
						sort s

		priorityKey :: [(String,Bool)] -> HtmlRowFragment ()
		priorityKey ord = do
			attrTD [MkAttribute ("width","100%")] htmlNullImg
			attrTD [MkAttribute ("class","sort-selected"),MkAttribute ("valign","middle")] (upDownLabel ord "Priority" "priority")
			htmlTD (htmlNobrText " ") 
			attrTD [MkAttribute ("bgcolor",show $ MkHSV (0.455,0.0,0.8)),
				MkAttribute ("valign","middle")] (htmlSmall $ htmlNobrText " Low")
			gradientBar (0.455,0.0,0.8) (0.455,1.0,0.8) 5 (0,22) (1,21)
			attrTD [MkAttribute ("bgcolor",show $ MkHSV (0.455,1.0,0.8)),
				MkAttribute ("valign","middle")] (htmlSmall $ htmlNobrText "High ")

		gradientBar :: (Double,Double,Double) -> (Double,Double,Double) -> Int -> (Int,Int) -> (Int,Int) -> HtmlRowFragment ()
		gradientBar (h0,s0,v0) (h1,s1,v1) width (r0,r1) (first,end) = bar first where

			bar :: Int -> HtmlRowFragment ()
			bar i
				| i==end = htmlNull
				| otherwise = (do
					attrTD [MkAttribute ("bgcolor",show $ MkHSV (grd h0 h1,grd s0 s1,grd v0 v1))] 
						(attrImg [MkAttribute ("width",show width),MkAttribute ("height","1"),MkAttribute ("alt","")])
					bar (i+1)) where

				grd :: Double -> Double -> Double
				grd a b = (b-a)*(fromIntegral (i-r0)/fromIntegral r1)+a

		upDownLabel :: [(String,Bool)] -> String -> String -> HtmlFragment ()
		upDownLabel ord label name = case find ord name 1 of
			Just (n,dir)
				| dir==False -> htmlB (htmlNobr (do
					submit [("sort-by",joinSort $ replace (name,True) ord)] [MkAttribute ("class","light")] (do
						htmlNbspText " &lt;" 
						htmlSuper (htmlNbspText "  "))
					submit [("sort-by",joinSort $ remove name ord)] [] (do
						htmlNbspText label
						htmlSub (htmlNbspText $ show n))
					submit [("sort-by",joinSort $ remove name ord)] [] (do
						htmlNbspText "&gt; ")))
				| otherwise -> htmlB (htmlNobr (do
					submit [("sort-by",joinSort $ replace (name,False) ord)] [] (do
						htmlNbspText " &lt;"
						htmlSuper(htmlNbspText "  ")
						htmlNbspText label
						htmlSub (htmlNbspText $ show n))
					submit [("sort-by",joinSort $ replace (name,False) ord)] [MkAttribute ("class","light")] (do
						htmlNbspText "&gt; ")))
			_ -> htmlB (htmlNobr (do
					submit [("sort-by",joinSort $ replace (name,True) ord)] [MkAttribute ("class","light")] (do
						htmlNbspText " &lt;"
						htmlSuper (htmlNbspText "  "))
					submit [("sort-by",joinSort $ replace (name,True) ord)] [] (do
						htmlNbspText label)
					submit [("sort-by",joinSort $ replace (name,False) ord)] [MkAttribute ("class","light")] (do
						htmlSub (htmlNbspText "  ")
						htmlNbspText "&gt; ")))

		find :: [(String,Bool)] -> String -> Int -> Maybe (Int,Bool)
		find [] _ _ = Nothing
		find ((l,d):ls) n i 
			| l==n = Just (i,d)
			| otherwise = find ls n (i+1)

		replace :: (String,Bool) -> [(String,Bool)] -> [(String,Bool)]
		replace sb [] = [sb]
		replace w@(wn,_) (s0@(sn,_):ss)
			| wn==sn = w:ss
			| otherwise = s0:replace w ss

		remove :: String -> [(String,Bool)] -> [(String,Bool)]
		remove _ [] = []
		remove n (s0@(m,_):ss)
			| n==m = ss
			| otherwise = s0:remove n ss

		truncateSort :: String -> [(String,Bool)] -> [(String,Bool)]
		truncateSort _ [] = []
		truncateSort n (s0@(m,_):ss)
			| n==m = [s0]
			| otherwise = s0:truncateSort n ss
		
		htmlProjects :: SqlStBuf -> [(String,Bool)] -> HtmlTableFragmentT IO ()
		htmlProjects stb ord = (do
					(clist,ctype) <- doQuery stb (projectsView ord)
					rows <- formatRows clist ctype
					return (do
						htmlTR (do
							attrTD [MkAttribute ("class","sort-selected"),MkAttribute ("align","center"),
								MkAttribute ("valign","middle"),MkAttribute ("width","12%")] (upDownLabel ord "Project" "project")
							attrTD [MkAttribute ("class","sort-selected"),MkAttribute ("align","center"),
								MkAttribute ("valign","middle"),MkAttribute ("width","20%")] (upDownLabel ord "Status" "status")
							attrTD [MkAttribute ("class","sort-selected"),MkAttribute ("align","center"),
								MkAttribute ("valign","middle"),MkAttribute ("width","12%")] (upDownLabel ord "Lead" "lead")
							attrTD [MkAttribute ("class","sort-selected"),MkAttribute ("align","center"),
								MkAttribute ("valign","middle"),MkAttribute ("width","13%")] (upDownLabel ord "Start Date" "start")
							attrTD [MkAttribute ("class","sort-selected"),MkAttribute ("align","center"),
								MkAttribute ("valign","middle"),MkAttribute ("width","13%")] (upDownLabel ord "Due Date" "due")
							attrTD [MkAttribute ("class","sort-selected"),MkAttribute ("align","center"),
								MkAttribute ("valign","middle"),MkAttribute ("width","30%")]
								(upDownLabel ord "Description" "description"))
						rows)) where
	
			formatRows :: ColNameList -> ProjectsRow -> HtmlTableFragmentT IO ()
			formatRows clist ctype = do
				status <- (up . fetch) stb
				case status of
					SqlSuccess -> do
						row <- getRowAsFM stb clist
						rows <- formatRows clist ctype
						htmlFormat ctype row
						rows
					_ -> return $ htmlNull

			htmlFormat :: ProjectsRow -> SqlColMap -> HtmlTableFragment ()
			htmlFormat (pid,pn,pc,ps,pu,pp,pa,pt,pd) cmap = (do
				htmlTR (do
					colourByPid (htmlB $ submitPadded (cmap<?>pn)
						[("project-id",cmap<?>pid)])
					colourByPid (htmlNobrPad $ cmap<?>pt)
					colourByPid (htmlNobrPad $ cmap<?>pa)
					colourByPid (htmlNobrPad $ cmap<?>ps)
					colourByPid (htmlNobrPad $ cmap<?>pu)
					colourByPid (htmlNobrPad $ limit 24 (cmap<?>pd)))) where

				limit :: Int -> String -> String
				limit _ [] = ""
				limit l s = lim 0 s where

					lim :: Int -> String -> String
					lim _ [] = ""
					lim i (t0:tt)
						| i<l = (t0:lim (i+1) tt)
						| otherwise = "..."

				colourByPid :: HtmlFragment () -> HtmlRowFragment ()
				colourByPid =attrTD [MkAttribute ("valign","middle"),
					MkAttribute ("bgcolor",show $ MkHSV (0.455,(read $ cmap<?>pp),0.8))]

----------------------------------------------------------------------------
-- Projects Database Schema
----------------------------------------------------------------------------

projectsTable :: TypedTable ProjectsCols
projectsTable = TypedTable ((SqlTable "projects"),projectsCols)

data ProjectsCols = ProjectsCols {
	projectID :: TypedColumn Int,
	projectName :: TypedColumn String,
	projectCreator :: TypedColumn Int,
	projectStart :: TypedColumn Float,
	projectDue :: TypedColumn Float,
	projectPriority :: TypedColumn Float,
	projectAssigned :: TypedColumn Int,
	projectStatus :: TypedColumn Int,
	projectDescription :: TypedColumn String
}

projectsCols :: ProjectsCols
projectsCols = ProjectsCols {
	projectID = TypedColumn (SqlColumn "id"),
	projectName = TypedColumn (SqlColumn "name"),
	projectCreator = TypedColumn (SqlColumn "created_by"),
	projectStart = TypedColumn (SqlColumn "start_date"),
	projectDue = TypedColumn (SqlColumn "due_date"),
	projectPriority = TypedColumn (SqlColumn "priority"),
	projectAssigned = TypedColumn (SqlColumn "assigned_to"),
	projectStatus = TypedColumn (SqlColumn "status"),
	projectDescription = TypedColumn (SqlColumn "description")
}

statusTable :: TypedTable StatusCols
statusTable = TypedTable ((SqlTable "status_types"),statusCols)

data StatusCols = StatusCols {
	statusID :: TypedColumn Int,
	statusName :: TypedColumn String
}

statusCols :: StatusCols
statusCols = StatusCols {
	statusID = TypedColumn (SqlColumn "id"),
	statusName = TypedColumn (SqlColumn "name")
}

staffTable :: TypedTable StaffCols
staffTable = TypedTable ((SqlTable "staff"),staffCols)

data StaffCols = StaffCols {
	staffID :: TypedColumn Int,
	staffName :: TypedColumn String
}

staffCols :: StaffCols
staffCols = StaffCols {
	staffID = TypedColumn (SqlColumn "id"),
	staffName = TypedColumn (SqlColumn "name")
}

----------------------------------------------------------------------------

type ProjectsRow = (TypedExpr Int,TypedExpr String,TypedExpr String,TypedExpr Float,
	TypedExpr Float,TypedExpr Float,TypedExpr String,TypedExpr String,TypedExpr String)

projectsView :: [(String,Bool)] -> Query ProjectsRow
projectsView ord = (do
	p <- source projectsTable
	s <- source statusTable
	t0 <- source staffTable
	t1 <- source staffTable
	pid <- project (p!projectID)
	pName <- project (p!projectName)
	pCreator <- project (t1!staffName)
	pStart <- project (p!projectStart)
	pDue <- project (p!projectDue)
	pPri <- project (p!projectPriority)
	pAssign <- project (t0!staffName)
	pStatus <- project (s!statusName)
	pDesc <- project (p!projectDescription)
	restrict (p!projectStatus <=> s!statusID)
	restrict (p!projectAssigned <=> t0!staffID)
	restrict (p!projectCreator <=> t1!staffID)
	orderList (reverse ord) s t0 p
	return (pid,pName,pCreator,pStart,pDue,pPri,pAssign,pStatus,pDesc)) where

	orderList :: [(String,Bool)] -> UsedTable StatusCols -> UsedTable StaffCols -> UsedTable ProjectsCols -> Query ()
	orderList [] _ _ _ = return ()
	orderList ((n,d):ns) s t0 p = do
		case n of
			"status" -> order d (s!statusName)
			"lead" -> order d (t0!staffName)
			"start" -> order d (p!projectStart)
			"due" -> order d (p!projectDue)
			"priority" -> order d (p!projectPriority)
			"description" -> order d (p!projectDescription)
			_ -> order d (p!projectName)
		orderList ns s t0 p

