module Sem.Prompt where

import Polysemy
import Polysemy.Cont


prompt :: Members '[Cont ref] r => Sem r ()
prompt = pure ()
