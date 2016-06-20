{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Lib.HTML.Diff(htmlDiffHtml) where

import IO

import Lib.XML.Types
import Lib.XML.Generator
import Lib.Data.Diff
import Lib.Data.XmlDiff
import Lib.HTML.MonadHtml
import Lib.HTML.HtmlFragmentT

data EditList a = CopyList [a]
	| InsertList [a]
	| DeleteList [a]
	| InsertTreeList [a]
	| DeleteTreeList [a]
	| InsertSwapList [a]
	| DeleteSwapList [a]
	| ReplaceList [a] [a] deriving Show

htmlDiffHtml :: (DOM -> DOM -> (ZSDist,Edit XmlElement)) -> DOM -> DOM -> HtmlFragmentT IO ()
htmlDiffHtml f a b = hdiff (f a b) where

	hdiff :: (ZSDist,Edit XmlElement) -> HtmlFragmentT IO ()
	hdiff (d,e) = htmlEditTable d (editToEditList e)

showIndent :: Int -> XmlElement -> [XmlElement]
showIndent i e 
 	| i>0 = EntityRef "nbsp":EntityRef "nbsp":EntityRef "nbsp":showIndent (i-1) e
	| otherwise = [CharData (showElement e "")]

htmlElements :: Int -> [XmlElement] -> HtmlFragmentT IO Int
htmlElements i [] = return i
htmlElements i (e0:es) = do
	case e0 of
		(STag _ _) -> do
			htmlText (showIndent i e0)
			htmlBR
			htmlElements (i+1) es
		(ETag _) -> do
			htmlText (showIndent (i-1) e0)
			htmlBR
			htmlElements (i-1) es
		(Text t) -> do
			htmlText t
			(\f -> case es of
				(Text _:_) -> f
				_ -> do
					htmlBR
					f) (htmlElements i es)
		_ -> do
			htmlText (showIndent i e0)
			htmlBR
			htmlElements i es
   where

htmlEditTable :: ZSDist -> [EditList XmlElement] -> HtmlFragmentT IO ()
htmlEditTable dif el = htmlCenter $ htmlDoc $ htmlBody $ do
	attrTable [MkAttribute ("width","90%"),MkAttribute ("cellpadding","1")] $ do
		htmlTR $ attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("align","center")] $ do
			attrSpan [MkAttribute ("style","color:#ffffff")] $ do
				htmlNobrText ((showString "distance = " . shows dif) "")
		htmlTR $ attrTD [MkAttribute ("bgcolor","#666666")] $ do
			attrTable [MkAttribute ("width","100%")] $ htmlEdit 0 0 el

htmlEdit :: Int -> Int -> [EditList XmlElement] -> HtmlTableT IO ()
htmlEdit _ _ [] = return ()
htmlEdit i j (DeleteSwapList x:InsertSwapList y:ms) = do
	(i2,j2) <- htmlTR $ do
		i1 <- attrTD [MkAttribute ("bgcolor","#cc99ff"),MkAttribute ("width","50%")] $ do
			htmlSmall $ htmlElements i x
		attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("width","1")] htmlNullImg
		j1 <- attrTD [MkAttribute ("bgcolor","#cc99ff"),MkAttribute ("width","50%")] $ do
			htmlSmall $ htmlElements j y
		return (i1,j1)
	htmlTR $ attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("height","1")] htmlNullImg
	htmlEdit i2 j2 ms
htmlEdit i j (InsertSwapList y:DeleteSwapList x:ms) = do
	(i2,j2) <- htmlTR $ do
		i1 <- attrTD [MkAttribute ("bgcolor","#cc99ff"),MkAttribute ("width","50%")] $ do
			htmlSmall $ htmlElements i x
		attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("width","1")] htmlNullImg
		j1 <- attrTD [MkAttribute ("bgcolor","#cc99ff"),MkAttribute ("width","50%")] $ do
			htmlSmall $ htmlElements j y
		return (i1,j1)
	htmlTR $ attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("height","1")] htmlNullImg
	htmlEdit i2 j2 ms
htmlEdit i j (CopyList x:ms) = do
	(i2,j2) <- htmlTR $ do
		i1 <- attrTD [MkAttribute ("bgcolor","#ffffff"),MkAttribute ("width","50%")] $ do
			htmlSmall $ htmlElements i x
		attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("width","1")] htmlNullImg
		j1 <- attrTD [MkAttribute ("bgcolor","#ffffff"),MkAttribute ("width","50%")] $ do
			htmlSmall $ htmlElements j x
		return (i1,j1)
	htmlTR $ attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("height","1")] htmlNullImg
	htmlEdit i2 j2 ms
htmlEdit i j (InsertList x:ms) = do
	j2 <- htmlTR $ do
		attrTD [MkAttribute ("bgcolor","#99ff99")] $ htmlSmall $ htmlText [EntityRef "nbsp"]
		attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("width","1")] htmlNullImg
		j1 <- attrTD [MkAttribute ("bgcolor","#99ff99")] $ htmlSmall $ htmlElements j x
		return j1
	htmlTR $ attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("height","1")] htmlNullImg
	htmlEdit i j2 ms
htmlEdit i j (DeleteList x:ms) = do
	i2 <- htmlTR $ do
		i1 <- attrTD [MkAttribute ("bgcolor","#ff9999")] $ htmlSmall $ htmlElements i x
		attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("width","1")] htmlNullImg
		attrTD [MkAttribute ("bgcolor","#ff9999")] $ htmlSmall $ htmlText [EntityRef "nbsp"]
		return i1
	htmlTR $ attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("height","1")] htmlNullImg
	htmlEdit i2 j ms
htmlEdit i j (InsertTreeList x:ms) = do
	j2 <- htmlTR $ do
		attrTD [MkAttribute ("bgcolor","#99ffff")] $ htmlSmall $ htmlText [EntityRef "nbsp"]
		attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("width","1")] htmlNullImg
		j1 <- attrTD [MkAttribute ("bgcolor","#99ffff")] $ htmlSmall $ htmlElements j x
		return j1
	htmlTR $ attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("height","1")] htmlNullImg
	htmlEdit i j2 ms
htmlEdit i j (DeleteTreeList x:ms) = do
	i2 <- htmlTR $ do
		i1 <- attrTD [MkAttribute ("bgcolor","#ffff99")] $ htmlSmall $ htmlElements i x
		attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("width","1")] htmlNullImg
		attrTD [MkAttribute ("bgcolor","#ffff99")] $ htmlSmall $ htmlText [EntityRef "nbsp"]
		return i1
	htmlTR $ attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("height","1")] htmlNullImg
	htmlEdit i2 j ms
htmlEdit i j (ReplaceList x y:ms) = do
	(i2,j2) <- htmlTR $ do
		i1 <- attrTD [MkAttribute ("bgcolor","#9999ff")] $ htmlSmall $ htmlElements i x
		attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("width","1")] htmlNullImg
		j1 <- attrTD [MkAttribute ("bgcolor","#9999ff")] $ htmlSmall $ htmlElements j y
		return (i1,j1)
	htmlTR $ attrTD [MkAttribute ("bgcolor","#666666"),MkAttribute ("height","1")] htmlNullImg
	htmlEdit i2 j2 ms
htmlEdit i j (DeleteSwapList _:ms) = htmlEdit i j ms
htmlEdit i j (InsertSwapList _:ms) = htmlEdit i j ms

editToEditList :: Edit a -> [EditList a]
editToEditList (Insert x:ms) = reverse $ insert ms [x]
editToEditList (Delete x:ms) = reverse $ delete ms [x]
editToEditList (InsertTree x:ms) = reverse $ insertTree ms [x]
editToEditList (DeleteTree x:ms) = reverse $ deleteTree ms [x]
editToEditList (InsertSwap x:ms) = reverse $ insertSwap ms [x]
editToEditList (DeleteSwap x:ms) = reverse $ deleteSwap ms [x]
editToEditList (Copy x:ms) = reverse $ copy ms [x]
editToEditList (Replace x y:ms) = reverse $ replace ms [x] [y] 
editToEditList _ = []

insert :: Edit a -> [a] -> [EditList a]
insert (Insert x:ms) xs = insert ms (x:xs)
insert (Delete x:ms) xs = InsertList xs:delete ms [x]
insert (InsertTree x:ms) xs = InsertList xs:insertTree ms [x]
insert (DeleteTree x:ms) xs = InsertList xs:deleteTree ms [x]
insert (InsertSwap x:ms) xs = InsertList xs:insertSwap ms [x]
insert (DeleteSwap x:ms) xs = InsertList xs:deleteSwap ms [x]
insert (Copy x:ms) xs = InsertList xs:copy ms [x]
insert (Replace x y:ms) xs = InsertList xs:replace ms [x] [y]
insert _ xs = [InsertList xs]

delete :: Edit a -> [a] -> [EditList a]
delete (Insert x:ms) xs = DeleteList xs:insert ms [x]
delete (Delete x:ms) xs = delete ms (x:xs)
delete (InsertTree x:ms) xs = DeleteList xs:insertTree ms [x]
delete (DeleteTree x:ms) xs = DeleteList xs:deleteTree ms [x]
delete (InsertSwap x:ms) xs = DeleteList xs:insertSwap ms [x]
delete (DeleteSwap x:ms) xs = DeleteList xs:deleteSwap ms [x]
delete (Copy x:ms) xs = DeleteList xs:copy ms [x]
delete (Replace x y:ms) xs = DeleteList xs:replace ms [x] [y]
delete _ xs = [DeleteList xs]

insertTree :: Edit a -> [a] -> [EditList a]
insertTree (Insert x:ms) xs = InsertTreeList xs:insert ms [x]
insertTree (Delete x:ms) xs = InsertTreeList xs:delete ms [x]
insertTree (InsertTree x:ms) xs = insertTree ms (x:xs)
insertTree (DeleteTree x:ms) xs = InsertTreeList xs:deleteTree ms [x]
insertTree (InsertSwap x:ms) xs = InsertTreeList xs:insertSwap ms [x]
insertTree (DeleteSwap x:ms) xs = InsertTreeList xs:deleteSwap ms [x]
insertTree (Copy x:ms) xs = InsertTreeList xs:copy ms [x]
insertTree (Replace x y:ms) xs = InsertTreeList xs:replace ms [x] [y]
insertTree _ xs = [InsertTreeList xs]

deleteTree :: Edit a -> [a] -> [EditList a]
deleteTree (Insert x:ms) xs = DeleteTreeList xs:insert ms [x]
deleteTree (Delete x:ms) xs = DeleteTreeList xs:delete ms [x]
deleteTree (InsertTree x:ms) xs = DeleteTreeList xs:insertTree ms [x]
deleteTree (DeleteTree x:ms) xs = deleteTree ms (x:xs)
deleteTree (InsertSwap x:ms) xs = DeleteTreeList xs:insertTree ms [x]
deleteTree (DeleteSwap x:ms) xs = DeleteTreeList xs:deleteTree ms [x]
deleteTree (Copy x:ms) xs = DeleteTreeList xs:copy ms [x]
deleteTree (Replace x y:ms) xs = DeleteTreeList xs:replace ms [x] [y]
deleteTree _ xs = [DeleteTreeList xs]

insertSwap :: Edit a -> [a] -> [EditList a]
insertSwap (Insert x:ms) xs = InsertSwapList xs:insert ms [x]
insertSwap (Delete x:ms) xs = InsertSwapList xs:delete ms [x]
insertSwap (InsertTree x:ms) xs = InsertSwapList xs:insertTree ms [x]
insertSwap (DeleteTree x:ms) xs = InsertSwapList xs:deleteTree ms [x]
insertSwap (InsertSwap x:ms) xs = insertSwap ms (x:xs)
insertSwap (DeleteSwap x:ms) xs = InsertSwapList xs:deleteSwap ms [x]
insertSwap (Copy x:ms) xs = InsertSwapList xs:copy ms [x]
insertSwap (Replace x y:ms) xs = InsertSwapList xs:replace ms [x] [y]
insertSwap _ xs = [InsertTreeList xs]

deleteSwap :: Edit a -> [a] -> [EditList a]
deleteSwap (Insert x:ms) xs = DeleteSwapList xs:insert ms [x]
deleteSwap (Delete x:ms) xs = DeleteSwapList xs:delete ms [x]
deleteSwap (InsertTree x:ms) xs = DeleteSwapList xs:insertTree ms [x]
deleteSwap (DeleteTree x:ms) xs = DeleteSwapList xs:deleteTree ms [x]
deleteSwap (InsertSwap x:ms) xs = DeleteSwapList xs:insertSwap ms [x]
deleteSwap (DeleteSwap x:ms) xs = deleteSwap ms (x:xs)
deleteSwap (Copy x:ms) xs = DeleteSwapList xs:copy ms [x]
deleteSwap (Replace x y:ms) xs = DeleteSwapList xs:replace ms [x] [y]
deleteSwap _ xs = [DeleteSwapList xs]

copy :: Edit a -> [a] -> [EditList a]
copy (Insert x:ms) xs = CopyList xs:insert ms [x]
copy (Delete x:ms) xs = CopyList xs:delete ms [x]
copy (InsertTree x:ms) xs = CopyList xs:insertTree ms [x]
copy (DeleteTree x:ms) xs = CopyList xs:deleteTree ms [x]
copy (InsertSwap x:ms) xs = CopyList xs:insertSwap ms [x]
copy (DeleteSwap x:ms) xs = CopyList xs:deleteSwap ms [x]
copy (Copy x:ms) xs = copy ms (x:xs)
copy (Replace x y:ms) xs = CopyList xs:replace ms [x] [y]
copy _ xs = [CopyList xs]

replace :: Edit a -> [a] -> [a] -> [EditList a]
replace (Insert x:ms) xs ys = ReplaceList xs ys:insert ms [x]
replace (Delete x:ms) xs ys = ReplaceList xs ys:delete ms [x]
replace (InsertTree x:ms) xs ys = ReplaceList xs ys:insertTree ms [x]
replace (DeleteTree x:ms) xs ys = ReplaceList xs ys:deleteTree ms [x]
replace (InsertSwap x:ms) xs ys = ReplaceList xs ys:insertSwap ms [x]
replace (DeleteSwap x:ms) xs ys = ReplaceList xs ys:deleteSwap ms [x]
replace (Copy x:ms) xs ys = ReplaceList xs ys:copy ms [x]
replace (Replace x y:ms) xs ys = replace ms (x:xs) (y:ys)
replace _ xs ys = [ReplaceList xs ys]

