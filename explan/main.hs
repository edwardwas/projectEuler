{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import System.Directory
import System.FilePath.Posix
import Control.Applicative
import qualified Data.Char as C

toTitle :: String -> String
toTitle (x:xs) = (C.toUpper x):xs

isTex :: String -> Bool
isTex s = (a /= "") && (b == ".tex")
	where (a,b) = splitExtension s

addConcept :: Monad m => String -> LaTeXT_ m
addConcept s = (section $ fromString $ toTitle $ fst $ splitExtensions s) <> 
		(label $ fromString $ "sec:" ++ (fst $ splitExtensions s)) <>
		(input $ fromString "concepts/"++s)

theConcepts :: Monad m => [String] -> LaTeXT_ m
theConcepts f = mconcat $ map (addConcept) $ filter (isTex) f

packages = ["amsmath","listings","color","hyperref"] :: [PackageName]
subDirs = ["020","024","096","108","127"] :: [String]

stripLeadingZeros :: String -> String
stripLeadingZeros s@(x:xs) = if x == '0' then stripLeadingZeros xs else s

getSubDir :: Monad m => String -> LaTeXT_ m
getSubDir s = do
	section $ fromString $ (++) "Problem " $ stripLeadingZeros s
	input $ fromString $ "../" ++ s ++ "/explanation.tex"

thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
	documentclass [a4paper] report
	title "Explanations of Project Euler Solutions"
	author "Edward Wastell"
	mconcat $ map (usepackage []) packages

theBody :: Monad m => LaTeXT_ m
theBody = do
	maketitle
	tableofcontents
	input "config.tex"

comb f = do
	thePreamble
	document $ mconcat [
		theBody, 
		chapter "Concepts",
		theConcepts f,
		chapter "Solutions",
		mconcat $ map (getSubDir) subDirs
		]

main = do
	c <- (comb) <$> getDirectoryContents "concepts/"
	execLaTeXT c >>= renderFile "main.tex"
