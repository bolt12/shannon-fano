import Codec.Compression.ShannonFano
import Control.Monad (join)
import Text.Printf (printf)
import Test.QuickCheck (quickCheck, (==>), Property)

prop_freq_id :: String -> Property
prop_freq_id s = length s > 1 ==> let dt = genDecodeTable . code . frequency $ s
                                      in join (decode dt <$> compress frequency s) == Just s

prop_prob_id :: String -> Property
prop_prob_id s = length s > 1 ==> let dt = genDecodeTable . code . probability $ s
                                      in join (decode dt <$> compress frequency s) == Just s

prop_rev_table :: String -> Bool
prop_rev_table s = let ct = genCodeTable . code . frequency $ s
                       dt = genDecodeTable . code . frequency $ s
                       in dt == map swap ct
            where
                swap = (,) <$> snd <*> fst

tests = [("prop_freq_id", quickCheck prop_freq_id), ("prop_prob_id", quickCheck prop_prob_id), ("prop_rev_table", quickCheck prop_rev_table)]

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
