import scala.util.matching.Regex
import scala.collection.immutable.HashMap
import scala.collection.immutable.ListMap
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import scala.collection.immutable.ListSet

case class PlateNumber(plateNumber: String)

trait UnluckyNumber {
  def getNumbers():Set[String]
}

case object EasyGetUpsetNumber extends UnluckyNumber { 
  override def getNumbers(): Set[String] = Set("03",  "30",  "10",  "01",  "12",  "21",  "33",  "07",  "13",  "31",  "37",  "73",  "38",  "83")
}
case object EasyGetAccidentNumber extends UnluckyNumber {
  override def getNumbers(): Set[String] = Set("13", "31", "73", "37", "38", "83", "33")
}
case object OftenGetPaidRepairmentNumber extends UnluckyNumber {
  override def getNumbers(): Set[String] = Set("10","01","02","20","03","30","13","31","33","06","60","67","76")
}

case object GetElectricProblemNumber extends UnluckyNumber {
  override def getNumbers(): Set[String] = Set("11", "91", "19")
}

case object GetScratchWhenParkingNumber extends UnluckyNumber {
  override def getNumbers(): Set[String] = Set("35","53","36","63","39","93","17","71","77","76","67")
}

case object UnluckyAndGetAccidentNumber extends UnluckyNumber {
  override def getNumbers(): Set[String] = Set("13","31","37","73","30","03","1313","3737","3100","3031")
}

trait LuckyNumber {
  def getNumbers(): Set[String]
  def getDescripiton(): String
}

case object SafetyNumber extends LuckyNumber {
  override def getNumbers(): Set[String] = Set("15", "51", "55", "49", "94", "95", "59", "99")
  override def getDescripiton(): String = "สิ่งศักสิทธิ์คุ้มครอง แคล้วคลาดปลอดภัย"
}

case object AttractionNumber extends LuckyNumber {
  override def getNumbers(): Set[String] = Set("22", "23", "32", "24", "42", "26", "62", "29", "92", "36", "63")
  override def getDescripiton(): String = "เมตตามหานิยม"
}

case object PopularityNumber extends LuckyNumber {
  override def getNumbers(): Set[String] = Set("15", "51", "35", "53", "45", "54", "89", "98", "99")
  override def getDescripiton(): String = "เพิ่มบารมี"
}

case object WealthyNumber extends LuckyNumber {
  override def getNumbers(): Set[String] = Set("24", "42", "36", "63", "66", "28", "82")
  override def getDescripiton(): String = "การค้าขายร่ำรวย"
}

trait PlateNumberSumResult {
  def getSumResult(): String
  def getDescripiton(): String
}

case object PlateNumberSumResultEqualsOne extends PlateNumberSumResult {
  override def getSumResult() = "1"
  override def getDescripiton() = "ผลรวมเท่ากับ 1 เหมาะกับคนที่มียศมีตำแหน่ง ข้าราชการ ส่งผลให้เป็นผู้มีอำนาจวาสนา"
}

case object PlateNumberSumResultEqualsTwo extends PlateNumberSumResult {
  override def getSumResult() = "2"
  override def getDescripiton(): String = "ผลรวมเท่ากับ 2 เหมาะกับผู้หญิง คนทำงานด้านบริการ การเงินการธนาคาร ค้าขาย ช่วยส่งเสริมด้านการเงิน"
}

case object PlateNumberSumResultEqualsThree extends PlateNumberSumResult {
  override def getSumResult() = "3"
  override def getDescripiton(): String = "ผลรวมเท่ากับ 3 เหมาะกับผู้ชาย คนทำงานในเครื่องแบบ เช่น ทหาร ตำรวจ ช่างฝีมือ หมอ พยาบาล ส่งผลให้ขับรถใจร้อน"
}

case object PlateNumberSumResultEqualsFour extends PlateNumberSumResult {
  override def getSumResult() = "4"
  override def getDescripiton(): String = "ผลรวมเท่ากับ 4 เหมาะกับคนที่ทำงานด้านการเจรจาติดต่อสื่อสาร เช่น ผู้สื่อข่าว พิธีกร ส่งผลให้มีชื่อเสียง ประสบความสำเร็จ"
}

case object PlateNumberSumResultEqualsFive extends PlateNumberSumResult {
  override def getSumResult() = "5"
  override def getDescripiton(): String = "ผลรวมเท่ากับ 5 เหมาะกับทุกคน และคนที่ทำงานด้านการศึกษา ศาลยุติธรรม หมอ พยาบาล ส่งผลให้มีความน่าเชื่อถือ"
}

case object PlateNumberSumResultEqualsSix extends PlateNumberSumResult {
  override def getSumResult() = "6"
  override def getDescripiton(): String = "ผลรวมเท่ากับ 6 เหมาะกับคนทำงานด้านบันเทิง เสริมสวย ขายของสวยงาม ส่งผลดีเรื่องแคล้วคลาด มีคนคอยช่วยเหลืออุปถัมภ์"
}

case object PlateNumberSumResultEqualsSeven extends PlateNumberSumResult {
  override def getSumResult() = "7"
  override def getDescripiton(): String = "ผลรวมเท่ากับ 7 เหมาะกับคนทำอาชีพการเกษตร แต่ส่งผลเสียด้านการเงินและการเดินทางมีโอกาสเกิดอุบัติเหตุได้ง่ายๆ"
}

case object PlateNumberSumResultEqualsEight extends PlateNumberSumResult {
  override def getSumResult() = "8"
  override def getDescripiton(): String = "ผลรวมเท่ากับ 8 เหมาะกับคนทำงานกลางคืน อาชีพเสี่ยงอันตราย อาชีพสีเทา นักโหราศาสตร์ ส่งผลให้ร่ำรวย มั่งมี มีคนยกย่อง"
}

case object PlateNumberSumResultEqualsNine extends PlateNumberSumResult {
  override def getSumResult() = "9"
  override def getDescripiton(): String = "ผลรวมเท่ากับ 9 เหมาะกับทุกคน ส่งผลให้มีสิ่งศักดิ์สิทธิ์คุ้มครอง แคล้วคลาดปลอดภัย ปราศจากอุบัติเหตุต่างๆ"
}

trait BirthdayWithUnluckyNumber {
  def day: String
  def getUnluckyNumber: String
}

case object SundayWithUnluckyNumber extends BirthdayWithUnluckyNumber {
  override def day: String = "วันอาทิตย์"
  override def getUnluckyNumber: String = "6"
}

case object MondayWithUnluckNumber extends BirthdayWithUnluckyNumber {
  override def day: String = "วันจันทร์"
  override def getUnluckyNumber: String = "1"
}

case object TuesWithUnluckNumber extends BirthdayWithUnluckyNumber {
  override def day: String = "วันอังคาร"
  override def getUnluckyNumber: String = "2"
}

case object WendesdayDayWithUnluckNumber extends BirthdayWithUnluckyNumber {
  override def day: String = "วันพุธ (กลางวัน)"
  override def getUnluckyNumber: String = "3"
}

case object WendesdayNightWithUnluckNumber extends BirthdayWithUnluckyNumber {
  override def day: String = "วันพุธ (กลางคืน)"
  override def getUnluckyNumber: String = "5"
}
case object ThursdayWithUnluckNumber extends BirthdayWithUnluckyNumber {
  override def day: String = "วันพฤหัสบดี"
  override def getUnluckyNumber: String = "7"
}

case object FridayWithUnluckNumber extends BirthdayWithUnluckyNumber {
  override def day: String = "วันศุกร์"
  override def getUnluckyNumber: String = "8"
}

case object SaturdayWithUnluckNumber extends BirthdayWithUnluckyNumber {
  override def day: String = "วันเสาร์"
  override def getUnluckyNumber: String = "4"
}

object LuckyPlate {
  def validateSelectedEnhanceLuckyOption(input: String): Set[String] = {
    val selectedLuckyNumberOptionIds: Set[String] = input.split(",").toSet
    if (selectedLuckyNumberOptionIds.isEmpty)//selectedLuckyNumberOptionIds.forall(s=>s.toIntOption.)
      validateSelectedEnhanceLuckyOption(readLine())
    else
      selectedLuckyNumberOptionIds
  }

  def main(args: Array[String]) = {
    val allPlateNumbers = generateAllPlateNumber()
    val dayOptions = getBirthdayWithUnluckyNumberOptions()
    val allEnhanceLuckyOptions = getEnhanceLuckyOptions()
    println("=========== Lucky Plate Number ===========")
    print("กรุณาเลือกวันเกิด: (เลือกตัวเลข)")
    println(dayOptions.map(o=> s"\n${o._1}) ${o._2.day}").mkString)
    val selectedBirthdayOptionId: Int = readInt().intValue()
    println("กรุณาเลือกจุดประสงค์ที่ต้องการ (เลือกตัวเลขได้มากกว่า 1 ข้อ ใช้ , ในการคั่นตัวเลือก)")
    println(allEnhanceLuckyOptions.map(o=> s"\n${o._1}) ${o._2.getDescripiton()}").mkString)
    val selectedLuckyNumberOptionIds: Set[String] = validateSelectedEnhanceLuckyOption(readLine())
    val selectedDay = dayOptions(selectedBirthdayOptionId)
    val selectedLuckyNumberOptions: Set[LuckyNumber] = selectedLuckyNumberOptionIds.map(id=> allEnhanceLuckyOptions(id.toInt))
    val searchPlateNumberResult = searchGoodPlateNumber(allPlateNumbers, selectedDay.getUnluckyNumber , selectedLuckyNumberOptions)
    // val grouping: Map[String, SortedSet[String]] = groupPlateNumberBySumResult(searchPlateNumberResult)
    val grouping = groupPlateNumberBySumResult(searchPlateNumberResult)
    
    println("\n==========================================")
    println(s"\n\nผลลัพธ์ทั้งหมด ${grouping.map(g=>g._2.size).sum} รายการ")
    println(grouping.map(g=> s"\n\n${g._1} (${g._2.size}):\n${g._2.mkString("\n",", ","")}").mkString)
  }

  def getAllPlateNumberSumResultDescription(): ListMap[String, String] = {
    ListMap[String, String](
      PlateNumberSumResultEqualsOne.getSumResult() -> PlateNumberSumResultEqualsOne.getDescripiton(),
      PlateNumberSumResultEqualsTwo.getSumResult() -> PlateNumberSumResultEqualsTwo.getDescripiton(),
      PlateNumberSumResultEqualsThree.getSumResult() -> PlateNumberSumResultEqualsThree.getDescripiton(),
      PlateNumberSumResultEqualsFour.getSumResult() -> PlateNumberSumResultEqualsFour.getDescripiton(),
      PlateNumberSumResultEqualsFive.getSumResult() -> PlateNumberSumResultEqualsFive.getDescripiton(),
      PlateNumberSumResultEqualsSix.getSumResult() -> PlateNumberSumResultEqualsSix.getDescripiton(),
      PlateNumberSumResultEqualsSeven.getSumResult() -> PlateNumberSumResultEqualsSeven.getDescripiton(),
      PlateNumberSumResultEqualsEight.getSumResult() -> PlateNumberSumResultEqualsEight.getDescripiton(),
      PlateNumberSumResultEqualsNine.getSumResult() -> PlateNumberSumResultEqualsNine.getDescripiton(),
      )
  }

  def groupPlateNumberBySumResult(plateNumbers: Set[PlateNumber]) = {
    val sumResultMap: ListMap[String, String] = getAllPlateNumberSumResultDescription()
    var plateNumbersGroupBySumResult = plateNumbers.map(p => p.plateNumber.toInt).groupBy(g => sumNumbersUntilRemainOneDigit(g.toString))
    sumResultMap.map(resultNumber => (sumResultMap(resultNumber._1), plateNumbersGroupBySumResult(resultNumber._1).to[SortedSet]))
  }
  
  def getBirthdayWithUnluckyNumberOptions(): ListMap[Int, BirthdayWithUnluckyNumber] = {
    ListMap[Int, BirthdayWithUnluckyNumber](
      1 -> SundayWithUnluckyNumber,
      2 -> MondayWithUnluckNumber,
      3 -> TuesWithUnluckNumber,
      4 -> WendesdayDayWithUnluckNumber,
      5 -> WendesdayNightWithUnluckNumber,
      6 -> ThursdayWithUnluckNumber,
      7 -> FridayWithUnluckNumber,
      8 -> SaturdayWithUnluckNumber
    )
  }

  def getEnhanceLuckyOptions(): HashMap[Int, LuckyNumber] = {
    HashMap[Int,LuckyNumber](
      1 -> SafetyNumber,
      2 -> AttractionNumber,
      3 -> PopularityNumber,
      4 -> WealthyNumber
    )
  }

  def searchGoodPlateNumber(plateNumbers: Set[PlateNumber], unluckyNumberByDay: String, luckEnhanceNumbers: Set[LuckyNumber]): Set[PlateNumber] = {
    val luckyPlateNumbers: Set[PlateNumber] = filterOutAllUnluckyNumbers(plateNumbers, unluckyNumberByDay)
    val luckyNumbers = luckEnhanceNumbers.flatMap(rule => rule.getNumbers())
    getLuckyNumbers(luckyPlateNumbers, luckyNumbers)
  }

  def getLuckyNumbers(luckyPlateNumbers: Set[PlateNumber], luckyNumbers: Set[String]) : Set[PlateNumber] = {
    luckyPlateNumbers.filter(luckyPlate => luckyNumbers.exists(pickNumber=> luckyPlate.plateNumber.contains(pickNumber)))
  }

  def generateAllPlateNumber(): Set[PlateNumber] = {
    (1 to 9999).map(num => PlateNumber(num.toString())).toSet
  }

  def sumNumbersUntilRemainOneDigit(numberText: String): String = {
    if (numberText.length == 1)
      numberText
    else {
      val sum = numberText.map(_.toString.toInt).sum
      sumNumbersUntilRemainOneDigit(sum.toString)
    }
  }

  def getAllUnluckyNumbers(): Set[String] = {
    EasyGetUpsetNumber.getNumbers() ++
    EasyGetAccidentNumber.getNumbers() ++ 
    OftenGetPaidRepairmentNumber.getNumbers() ++ 
    GetElectricProblemNumber.getNumbers() ++
    GetScratchWhenParkingNumber.getNumbers() ++
    UnluckyAndGetAccidentNumber.getNumbers()
  }

  def filterOutAllUnluckyNumbers(plateNumbers: Set[PlateNumber], unluckyNumberByDay: String): Set[PlateNumber] = {
    val plateNumberWithoutUnluckyNumberByDay = filterOutUnluckyPlateNumberByDay(plateNumbers, unluckyNumberByDay)
    val combinedFilterOut = 
      filterOutUnluckyNumbers _ andThen 
      filterOutPlateNumberStartAndEndWithOne _ andThen 
      filterOutPlateNumberSumEqualsThirteen _
      
    combinedFilterOut(plateNumberWithoutUnluckyNumberByDay)
  }

  def filterOutUnluckyNumbers(plateNumbers: Set[PlateNumber]): Set[PlateNumber] = {
    val badNumbers = getAllUnluckyNumbers()
    plateNumbers.filter(plate =>
      badNumbers.forall(badNumber => !plate.plateNumber.contains(badNumber))
    )
  }

  def filterOutPlateNumberStartAndEndWithOne(plateNumbers: Set[PlateNumber]): Set[PlateNumber] = {
    plateNumbers.filterNot(plate => plate.plateNumber.length == 4 && plate.plateNumber.head == '1' && plate.plateNumber.last == '1')
  }

  def filterOutPlateNumberSumEqualsThirteen(plateNumbers: Set[PlateNumber]): Set[PlateNumber] = {
    plateNumbers.filterNot(plate =>
      filterOutNumberSumEqualsThirteen(plate.plateNumber)
    )
  }

  def filterOutNumberSumEqualsThirteen(plateNumber: String): Boolean = {
    def isSumEqualsThirteen(nums: Int*) = { nums.sum == 13 }
    def asInt(num: Char):Int = num.toString.toInt

    plateNumber.length match {
      case length if length == 2 => 
        isSumEqualsThirteen(asInt(plateNumber.head), asInt(plateNumber.last))
      case length if length == 3 => 
        val firstNum = asInt(plateNumber.head)
        val secondNum = asInt(plateNumber(1))
        val thirdNum = asInt(plateNumber.last)
        isSumEqualsThirteen(firstNum, secondNum, thirdNum) ||
        isSumEqualsThirteen(firstNum, thirdNum)
      case length if length == 4 => 
        val firstNum = asInt(plateNumber.head)
        val secondNum = asInt(plateNumber(1))
        val thirdNum = asInt(plateNumber(2))
        val fourthNum = asInt(plateNumber.last)
        isSumEqualsThirteen(firstNum, secondNum, thirdNum, fourthNum) ||
        isSumEqualsThirteen(firstNum, fourthNum) ||
        isSumEqualsThirteen(firstNum, secondNum) ||
        isSumEqualsThirteen(thirdNum, fourthNum)
      case _ => false
    }
  }

  def filterOutUnluckyPlateNumberByDay(plateNumbers: Set[PlateNumber], unluckyNumberByDay: String): Set[PlateNumber] = {
    plateNumbers.filterNot(plate => plate.plateNumber.contains(unluckyNumberByDay))
  }
}

