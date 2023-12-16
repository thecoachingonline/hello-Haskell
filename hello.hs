import Data.Bits
import Data.Vector

-- กำหนดค่าพารามิเตอร์การเข้ารหัส
let n = 4096
let q = 2^n

-- กำหนดฟังก์ชันสำหรับแปลงข้อความเป็นจุดในตาข่าย
def toPoint (s: String) :: Vector (Int, Int)
toPoint s = map ((\c -> (ord c, 1)) . fromChar) s

-- กำหนดฟังก์ชันสำหรับค้นหาจุดที่ใกล้กับจุดที่กำหนดมากที่สุด
def closestPoint (p: Vector (Int, Int)) :: Vector (Int, Int)
closestPoint p = foldr (minBy (\a b -> compare (dist p a) (dist p b))) p []

-- กำหนดฟังก์ชันสำหรับเข้ารหัสข้อความ
def encrypt (s: String) (sk: Vector (Int, Int)) :: Vector (Int, Int)
encrypt s sk = closestPoint (toPoint s + sk)

-- กำหนดฟังก์ชันสำหรับถอดรหัสข้อความ
def decrypt (c: Vector (Int, Int)) (sk: Vector (Int, Int)) :: String
decrypt c sk = map ((\(x, y) -> fromChar x) . div) (toPoint (closestPoint c - sk))

-- ตัวอย่างการเข้ารหัสข้อความ
main = do
  sk <- generateKey n
  c <- encrypt "สวัสดีชาวโลก" sk
  putStrLn $ decrypt c sk
