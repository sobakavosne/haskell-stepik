module N3_Lists where

nTimes:: a -> Int -> [a]
nTimes a n = times a n
    where times _ 0 = []
          times x y = x : times a (y - 1)

-- nTimes 'l' 10

second (_ : x2 : _) = x2

sndHead = snd . head

-- $> sndHead [(1,2),(3,3),(5,10)]