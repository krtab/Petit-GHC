module Main where
import Parser
import Lexer
import System.Environment
import System.Exit
import Control.Exception
import Control.Monad
import Typing2
import Gene

main =
  catch


  (do
   args <- getArgs
   let (wrapper,argFile) =
         case args of
           ["--parse-only",f] -> ((`seq` (return .return $ ())), f)
           ["--type-only",f] -> (\file -> (w file) `seq` (return (return ())),f)
           [f] -> (\file -> (w file) `seq`
                            (return $
                             writeFile
                             ( (++ ".s") . reverse . drop 3 . reverse $ f)
                             (compileFile file)
                            ),
                   f
                  )
           _ -> (error  "usage : petitghc [--parse-only] file")
           
   if (take 3 . reverse $  argFile) /= "sh." 
     then error "Error : filename must end in .hs" 
     else return ()
   file <- readFile argFile
   case alexScanTokens (('\n':file) ++ "\n") >>= parser >>= wrapper of
     Right x -> x >> exitSuccess
     Left (t,l,c) -> (putStrLn $
                      "File \"" ++
                      argFile ++ "\", line " ++ (show (l-1)) ++
                      ", characters " ++ (show (c-1)) ++ "-" ++ (show c) ++ ":\n" ++
                      t ++ " error" )
                     >> (exitWith . ExitFailure $ 1)
  )
  
  
  (\e -> case fromException e :: Maybe ExitCode of
      Just _ -> throwIO e
      Nothing -> print e >> (exitWith $ ExitFailure 2)
  )





