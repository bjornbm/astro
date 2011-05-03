import qualified Astro.Orbit.Types.Tests
import qualified Astro.Orbit.Anomaly.Tests
import qualified Astro.Orbit.Conversion.Tests
import Test.Hspec

main = do
  Astro.Orbit.Types.Tests.main
  Astro.Orbit.Anomaly.Tests.main
  Astro.Orbit.Conversion.Tests.main
