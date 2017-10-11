module Eff where

import Freer
import OpenUnion

type Eff = Freer Union

send :: t a -> Eff a
send = (`Bind` Pure) . inj
