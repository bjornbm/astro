import qualified Astro.Util.Tests
import qualified Astro.Orbit.Anomaly.Tests
import qualified Astro.Orbit.MEOE.Tests
import qualified Astro.Orbit.Conversion.Tests
import qualified Astro.Orbit.Interpolate.Tests
import qualified Astro.Trajectory.Cropped.Tests
import Test.Hspec

main = hspec $ do
  Astro.Util.Tests.specs
  Astro.Orbit.Anomaly.Tests.specs
  Astro.Orbit.MEOE.Tests.specs
  Astro.Orbit.Conversion.Tests.specs
  Astro.Orbit.Interpolate.Tests.specs
  Astro.Trajectory.Cropped.Tests.specs
