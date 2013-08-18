import Astro.Time
import IAU2000.Nutation
import Numeric.Units.Dimensional.Prelude

epochs = fmap (addTime j2000) ([-1000,-990..1000]*~~day)
nutas  = fmap nutationAngles2000A epochs

main = writeFile "iau2000A_test.txt" $ unlines $ fmap show $ zip epochs nutas
  
