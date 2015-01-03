module Typing2 where

import Typing.Definitions
import Typing.Subst
import Typing.FreeVars
import Typing.Env

w file = runEnv (primitives >> (wScope file)) 
