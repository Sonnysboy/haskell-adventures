data Block = Block Integer Integer Integer -- define me!

-- The test will use `block` to create
-- the blocks.
block :: (Integer, Integer, Integer) -> Block
block (l, w, h) = Block l w h

getWidth :: Block -> Integer
getWidth b@(Block w _ _) = w

getLength :: Block -> Integer
getLength b@(Block _ l _) = l 

getHeight :: Block -> Integer
getHeight b@(Block _ _ h) = h

getVolume :: Block -> Integer
getVolume b@(Block l w h) = l * w * h

getSurfaceArea :: Block -> Integer
getSurfaceArea b@(Block l w h) = 2 * ((l * w) + (w * h) + (l * h)) 