import qualified Astro.Orbit.Types.Tests
import qualified Astro.Orbit.Anomaly.Tests
import qualified Astro.Orbit.Conversion.Tests
import qualified Astro.Orbit.Interpolate.Tests

main = do
  Astro.Orbit.Types.Tests.main
  Astro.Orbit.Anomaly.Tests.main
  Astro.Orbit.Conversion.Tests.main
  Astro.Orbit.Interpolate.Tests.main
