import qualified Astro.Orbit.Types.Tests
import qualified Astro.Orbit.Anomaly.Tests
import qualified Astro.Orbit.Conversion.Tests
import qualified Astro.Orbit.Interpolate.Tests
import Test.Hspec.Monadic

main = hspec $ do
  Astro.Orbit.Types.Tests.specs
  Astro.Orbit.Anomaly.Tests.specs
  Astro.Orbit.Conversion.Tests.specs
  Astro.Orbit.Interpolate.Tests.specs
