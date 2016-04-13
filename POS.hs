import Data.Time.Clock
import Data.Time.Calendar
taxRate::Float
taxRate = 0.065


digitize::Integer -> [Integer]
digitize 0 = []
digitize x = digitize(div x 10) ++ [x `mod` 10]

cardType::Integer -> String
cardType a | head(digitize a) == 4 = "Visa"
	|head(digitize a) == 5 = "MasterCard"
	|head(digitize a) == 6 = "Discover"
	|otherwise = "Not Accepted"

pastDate::Integer->Int->Int-> IO Bool
pastDate y m d = do 
	d1 <- fmap utctDay getCurrentTime
	return ((fromGregorian y m d) < d1)

calcTax::Float->Float
calcTax a = a * taxRate

verifyCard::Integer->Bool
verifyCard a| length (cardType a) == 12 = False
	|pastDate a == False = False
	|otherwise = True