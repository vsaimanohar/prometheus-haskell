{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Prometheus.Label (
    Label (..)
,   LabelPairs
,   Label0
,   Label1
,   Label2
,   Label3
,   Label4
,   Label5
,   Label6
,   Label7
,   Label8
,   Label9
,   Label10
,   Label11
,   Label12
,   Label13
,   Label14
,   Label15
) where

import Data.Text

-- | A list of tuples where the first value is the label and the second is the
-- value of that label.
type LabelPairs = [(Text, Text)]

-- | Label describes a class of types that can be used to as the label of
-- a vector.
class Ord l => Label l where
    labelPairs :: l -> l -> LabelPairs

type Label0 = ()

instance Label () where
    labelPairs () () = []

type Label1 = Text

instance Label Text where
    labelPairs key value = [(key, value)]

type Label2 = (Text, Text)

instance (a ~ Text, b ~ a) => Label (a, b)  where
    labelPairs (k1, k2) (v1, v2) = [(k1, v1), (k2, v2)]

type Label3 = (Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a) => Label (a, b, c)  where
    labelPairs (k1, k2, k3) (v1, v2, v3) = [(k1, v1), (k2, v2), (k3, v3)]

type Label4 = (Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a) => Label (a, b, c, d)  where
    labelPairs (k1, k2, k3, k4) (v1, v2, v3, v4) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4)]

type Label5 = (Text, Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a) => Label (a, b, c, d, e)  where
    labelPairs (k1, k2, k3, k4, k5) (v1, v2, v3, v4, v5) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5)]

type Label6 = (Text, Text, Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a) => Label (a, b, c, d, e, f)  where
    labelPairs (k1, k2, k3, k4, k5, k6) (v1, v2, v3, v4, v5, v6) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6)]

type Label7 = (Text, Text, Text, Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a, g ~ a) => Label (a, b, c, d, e, f, g)  where
    labelPairs (k1, k2, k3, k4, k5, k6, k7) (v1, v2, v3, v4, v5, v6, v7) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7)]

type Label8 = (Text, Text, Text, Text, Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a, g ~ a, h ~ a) => Label (a, b, c, d, e, f, g, h) where
    labelPairs (k1, k2, k3, k4, k5, k6, k7, k8)
               (v1, v2, v3, v4, v5, v6, v7, v8) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7), (k8, v8)]

type Label9 = (Text, Text, Text, Text, Text, Text, Text, Text,
               Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a, g ~ a, h ~ a, i ~ a) => Label (a, b, c, d, e, f, g, h, i) where
    labelPairs (k1, k2, k3, k4, k5, k6, k7, k8, k9)
               (v1, v2, v3, v4, v5, v6, v7, v8, v9) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7), (k8, v8), (k9, v9)]

type Label10 = (Text, Text, Text, Text, Text, Text, Text, Text,
               Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a, g ~ a, h ~ a, i ~ a, j ~ a) => Label (a, b, c, d, e, f, g, h, i, j) where
    labelPairs (k1, k2, k3, k4, k5, k6, k7, k8, k9, k10)
               (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7), (k8, v8), (k9, v9), (k10, v10)]

type Label11 = (Text, Text, Text, Text, Text, Text, Text, Text,
               Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a, g ~ a, h ~ a, i ~ a, j ~ a, k ~ a) => Label (a, b, c, d, e, f, g, h, i, j, k) where
    labelPairs (k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11)
               (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7), (k8, v8), (k9, v9), (k10, v10), (k11, v11)]

type Label12 = (Text, Text, Text, Text, Text, Text, Text, Text,
               Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a, g ~ a, h ~ a, i ~ a, j ~ a, k ~ a, l ~ a) => Label (a, b, c, d, e, f, g, h, i, j, k, l) where
    labelPairs (k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12)
               (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7), (k8, v8), (k9, v9), (k10, v10), (k11, v11), (k12, v12)]

type Label13 = (Text, Text, Text, Text, Text, Text, Text, Text,
               Text, Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a, g ~ a, h ~ a, i ~ a, j ~ a, k ~ a, l ~ a, m ~ a) => Label (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    labelPairs (k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13)
               (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7), (k8, v8), (k9, v9), (k10, v10), (k11, v11), (k12, v12), (k13, v13)]

type Label14 = (Text, Text, Text, Text, Text, Text, Text, Text,
               Text, Text, Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a, g ~ a, h ~ a, i ~ a, j ~ a, k ~ a, l ~ a, m ~ a, n ~ a) => Label (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    labelPairs (k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14)
               (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7), (k8, v8), (k9, v9), (k10, v10), (k11, v11), (k12, v12), (k13, v13), (k14, v14)]

type Label15 = (Text, Text, Text, Text, Text, Text, Text, Text,
               Text, Text, Text, Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a, g ~ a, h ~ a, i ~ a, j ~ a, k ~ a, l ~ a, m ~ a, n ~ a, o ~ a) => Label (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    labelPairs (k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15)
               (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7), (k8, v8), (k9, v9), (k10, v10), (k11, v11), (k12, v12), (k13, v13), (k14, v14), (k15, v15)]
