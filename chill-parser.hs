-- one = '👍'
-- zero = '🤙'
one = '1'
zero = '0'

parse :: [Char] -> [Char]
parse text = filter (\x -> elem x [one, zero]) text

