import Data.Char

main :: IO ()
main = interact removeNewline

removeNewline :: String -> String
removeNewline "" = ""
removeNewline ('\n' : '\n' : cs) = "\n\n" ++ removeNewline cs
removeNewline ('\n' : cs@(c : _))
	| isAlpha c = removeNewline cs
	| '「' <- c = removeNewline cs
removeNewline ('`' : '`' : '`' : cs) = "```" ++ processCode cs
removeNewline (c : cs) = c : removeNewline cs

processCode :: String -> String
processCode ('`' : '`' : '`' : cs) = "```" ++ removeNewline cs
processCode (c : cs) = c : processCode cs
processCode _ = error "code not end"
