import Test.Tasty
import Test.Tasty.QuickCheck

import Seq (seqTests)
import MSeq (mseqTests)
import PQueue (pQueueTests)

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 2000) $ testGroup "All"
  [ seqTests
  , mseqTests
  , pQueueTests
  ]
