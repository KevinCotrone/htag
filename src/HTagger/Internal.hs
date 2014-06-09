module HTagger.Internal
    (
      concatSeq
    ) where


import qualified Data.Sequence as S
import qualified Data.Foldable as F
concatSeq :: S.Seq (S.Seq a) -> S.Seq a
concatSeq seqSeq = do
  F.foldl (\s ss -> s S.>< ss) S.empty seqSeq