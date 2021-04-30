import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

-- main = glob "src/**/*.*hs" >>= doctest
main = do
    src_hs <- glob "src/**/*.hs"
    src_lhs <- glob "src/**/*.lhs"
    doctest (src_hs ++ src_lhs ++ ["test/TestInstances.hs", "test/TestUtil.hs"])