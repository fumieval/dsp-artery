module DSP.Artery.Filter.Types (FilterParam(..)) where

data FilterParam a = FilterParam
    { _filterCutOff :: a
    , _filterQ :: a
    }