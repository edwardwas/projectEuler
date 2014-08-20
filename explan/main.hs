{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX

packages = ["amsmath","listings","color"] :: [PackageName]
subDirs = ["020","024","096"] :: [String]

stripLeadingZeros :: String -> String
stripLeadingZeros s@(x:xs) = if x == '0' then stripLeadingZeros xs else s

getSubDir :: Monad m => String -> LaTeXT_ m
getSubDir s = do
	section $ fromString $ (++) "Problem " $ stripLeadingZeros s
	input $ fromString $ "../" ++ s ++ "/explanation.tex"

thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
	documentclass [a4paper] article
	title "Explanations of Project Euler Solutions"
	author "Edward Wastell"
	mconcat $ map (usepackage []) packages

theBody :: Monad m => LaTeXT_ m
theBody = do
	maketitle
	tableofcontents
	input "config.tex"
	mconcat $ map (getSubDir) subDirs

comb :: Monad m => LaTeXT_ m
comb = do
	thePreamble
	document theBody

main = execLaTeXT comb >>= renderFile "main.tex"
