
{-|

Module      : Language.MicroPascal.Sem
Description : 
Copyright   : (c) Juan García Garland, 2019
License     : GPL-3
Maintainer  : jpgarcia@fing.edu.uy
Stability   : experimental
Portability : POSIX

AAG Stuff

-}

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.MicroPascal.Sem where

import Language.MicroPascal.AST
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive



$(deriveAG '' Program)


class SemLit a where
  sem_Lit :: a -> Attribution p -> Attribution '[ '((a, a), a)]
  lit     :: Label (a,a) 

instance SemLit a where
  sem_Lit a _ = (Label =. a) *. emptyAtt
  lit         = Label
