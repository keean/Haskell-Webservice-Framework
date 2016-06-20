-- Haskell MenuBar module
-- Copyright (C) 2002 Keean Schupke

module Lib.HTML.MenuBar (menuBar,MenuColours(..)) where

import Control.Monad
import Data.FiniteMap

import Lib.XML.Types
import Lib.XML.MonadDom
import Lib.HTML.Types
import Lib.HTTP.Types
import Lib.HTML.Types
import Lib.HTML.MonadHtml
import Lib.HTML.HtmlFragment

type Colour = String
data MenuColours = MenuColours {
	menuUnselectedBackground :: Colour,
	menuUnselectedLowlight :: Colour,
	menuUnselectedHighlight :: Colour,
	menuSelectedBackground :: Colour,
	menuSelectedForeground :: Colour,
	menuSelectedLowlight :: Colour,
	menuSelectedHighlight :: Colour,
	menuDataBackground :: Colour
}

fvars :: HttpRequest -> FiniteMap String String
fvars req = (\(UriParameters a) -> a) (uriParameters $ requestURI req)

menuBar :: MenuColours -> String -> [(HtmlFragment (),a)] -> a -> URI -> HttpRequest -> HtmlFragment a
menuBar colours label opts def uri req =
	case lookupWithDefaultFM (fvars req) "0" label of
		sel -> case (read sel) of
			j -> do
				attrTable [MkAttribute ("width","100%"),MkAttribute ("cols",show $ 3 * (length opts) + 1)] (do
					htmlTR (blankRow (length opts) j 0)
					f <- htmlTR (do
						form <- genCells 0 j opts def
						return form)
					htmlTR (blankRow (length opts) j 0)
					genHorizDiv (length opts) j
					htmlTR (attrTD [MkAttribute ("colspan",show $ 3 * (length opts) + 1),
						MkAttribute ("bgcolor",menuSelectedBackground colours),MkAttribute ("height","2")] (htmlNullImg))
					htmlTR (attrTD [MkAttribute ("colspan",show $ 3 * (length opts) + 1),
						MkAttribute ("bgcolor",menuSelectedBackground colours)] (htmlNobrText " "))
					return f)
	where

		blankRow :: Int -> Int -> Int -> HtmlRow ()
		blankRow n j i
			| i<n = do
				if i==j
					then do
						if i/=0 then spacingCell menuSelectedHighlight else htmlNull
						spacingCell menuSelectedBackground
						spacingCell menuSelectedLowlight
					else do
						if i/=0 then spacingCell menuUnselectedHighlight else htmlNull
						spacingCell menuUnselectedBackground
						spacingCell menuUnselectedLowlight
				blankRow n j (i+1)
			| otherwise = do
				if i/=0 then spacingCell menuUnselectedHighlight else htmlNull
				wideSpCell menuUnselectedBackground

		genHorizDiv :: Int -> Int -> HtmlTable ()
		genHorizDiv nopts sel = (do
			htmlTR (lowlight nopts sel 0)
			htmlTR (highlight nopts sel 0)) where

			lowlight :: Int -> Int -> Int -> HtmlRow ()
			lowlight n j i
				| i<n = do
					if i==j
						then do
							if i/=0 then spacingCell menuSelectedHighlight  else htmlNull
							spacingCell menuSelectedBackground
							spacingCell menuSelectedLowlight
						else do
							if i/=0 then spacingCell menuUnselectedHighlight else htmlNull
							attrTD [MkAttribute ("colspan","2"),MkAttribute ("bgcolor",menuUnselectedLowlight colours)] htmlNullImg
					lowlight n j (i+1)
				| otherwise = do
					if i/=0 then spacingCell menuUnselectedHighlight else htmlNull
					wideSpCell menuUnselectedLowlight

			highlight :: Int -> Int -> Int -> HtmlRow ()
			highlight n j i
				| i<n = do
					if i==j
						then do
							if i/=0 then spacingCell menuSelectedHighlight else htmlNull
							spacingCell menuSelectedBackground
							spacingCell menuSelectedHighlight
						else attrTD [MkAttribute ("colspan",if i==0 then "2" else "3"),
							MkAttribute ("bgcolor",menuSelectedHighlight colours)] htmlNullImg
					highlight n j (i+1)
				| otherwise = do
					if i/=0 then spacingCell menuUnselectedHighlight else htmlNull
					wideSpCell menuSelectedHighlight

		genCells :: Int -> Int -> [(HtmlFragment (),a)] -> a -> HtmlRow a
		genCells i _ [] d = do
			menuPadding i
			return d
		genCells i j ((opt,fn):rest) d = ( do
			if i==j
				then do
					if i /= 0 
						then spacingCell menuSelectedHighlight
						else htmlNull
					menuCellSelected opt
					spacingCell menuSelectedLowlight
					genCells (i+1) j rest d
					return fn
				else do
					if i /= 0
						then spacingCell menuUnselectedHighlight
						else htmlNull
					menuCell i opt
					spacingCell menuUnselectedLowlight
					f <- genCells (i+1) j rest d
					return f) where

			menuCell :: Int -> HtmlFragment () -> HtmlRow ()
			menuCell n contents = attrTD [MkAttribute ("bgcolor",menuUnselectedBackground colours),
				MkAttribute ("align","center")] $ htmlNoULink (show uri)
					(setAttributes [MkAttribute (label,show n)] (fmToAttributes $ fvars req)) (contents)

			menuCellSelected :: HtmlFragment () -> HtmlRow ()
			menuCellSelected contents = attrTD [MkAttribute ("bgcolor",menuSelectedBackground colours),
				MkAttribute ("align","center"),MkAttribute ("class","menu-selected"),
				MkAttribute ("style","{color: "++menuSelectedForeground colours++";}")] (contents)

		spacingCell :: (MenuColours -> Colour) -> HtmlRow ()
		spacingCell cs = do
			attrTD [MkAttribute ("bgcolor",cs colours)] $ htmlNullImg

		wideSpCell :: (MenuColours -> Colour) -> HtmlRow ()
		wideSpCell cs = do
			attrTD [MkAttribute ("width","100%"),MkAttribute ("bgcolor",cs colours)] $ htmlNullImg

		menuPadding :: Int -> HtmlRow ()
		menuPadding i = do
			if i/=0 then spacingCell menuUnselectedHighlight else htmlNull
			wideSpCell menuUnselectedBackground

