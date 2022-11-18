import scala.collection.immutable.TreeSet
import scala.collection.immutable.ListMap
import scala.collection.immutable.SortedSet
import scala.util.Sorting
import scala.io.StdIn.readLine

case class LicensePlate(number: String) extends Ordered[LicensePlate] {
  override def compare(that: LicensePlate): Int = {
    this.number.toInt - that.number.toInt
  }
}

sealed trait FilterLicensePlate {
  def description: String
  def numbers: TreeSet[String] = TreeSet.empty
  def number: String = numbers.mkString(",")
  def predicate(plate: LicensePlate): Boolean = {
    numbers.exists(num => plate.number.contains(num))
  }

  def filter(plates: TreeSet[LicensePlate]): TreeSet[LicensePlate] = {
    plates.filter(p=> predicate(p))
  }
}

sealed trait UnluckyNumberFilter extends FilterLicensePlate {
  override def predicate(plate: LicensePlate): Boolean = {
    numbers.forall(num => !plate.number.contains(num))
  }
}

sealed trait BlessingWithLuckyNumberFilter extends FilterLicensePlate

sealed abstract class CarOwnerBirthDayOfWeekWithUnluckyNumberFilter extends UnluckyNumberFilter {
  def dayOfWeek: String
  override def description: String = dayOfWeek
  override def number: String = ???
  override def numbers: TreeSet[String] = TreeSet(number)
}

sealed trait SumPlateNumberFilter extends FilterLicensePlate {
  override def number: String = ???
  override def numbers: TreeSet[String] = TreeSet(number)
}

case class SumPlateNumberWithLicensePlate(sumPlateNumber: SumPlateNumberFilter, licensePlates: TreeSet[LicensePlate]) extends Ordered[SumPlateNumberFilter] {
  override def compare(that: SumPlateNumberFilter): Int = {
    this.sumPlateNumber.number.toInt - that.number.toInt
  }
}

case object SumPlateNumberEqualsOne extends SumPlateNumberFilter {
  override def description: String = "เหมาะกับคนที่มียศมีตำแหน่ง ข้าราชการ ส่งผลให้เป็นผู้มีอำนาจวาสนา"
  override def number = "1"
}

case object SumPlateNumberEqualsTwo extends SumPlateNumberFilter {
  override def description: String = "เหมาะกับผู้หญิง คนทำงานด้านบริการ การเงินการธนาคาร ค้าขาย ช่วยส่งเสริมด้านการเงิน"
  override def number = "2"
}

case object SumPlateNumberEqualsThree extends SumPlateNumberFilter {
  override def description: String = "เหมาะกับผู้ชาย คนทำงานในเครื่องแบบ เช่น ทหาร ตำรวจ ช่างฝีมือ หมอ พยาบาล ส่งผลให้ขับรถใจร้อน"
  override def number = "3"
}

case object SumPlateNumberEqualsFour extends SumPlateNumberFilter {
  override def description: String = "เหมาะกับคนที่ทำงานด้านการเจรจาติดต่อสื่อสาร เช่น ผู้สื่อข่าว พิธีกร ส่งผลให้มีชื่อเสียง ประสบความสำเร็จ"
  override def number = "4"
}

case object SumPlateNumberEqualsFive extends SumPlateNumberFilter {
  override def number = "5"
  override def description: String = "เหมาะกับทุกคน และคนที่ทำงานด้านการศึกษา ศาลยุติธรรม หมอ พยาบาล ส่งผลให้มีความน่าเชื่อถือ"
}

case object SumPlateNumberEqualsSix extends SumPlateNumberFilter {
  override def number = "6"
  override def description: String = "เหมาะกับคนทำงานด้านบันเทิง เสริมสวย ขายของสวยงาม ส่งผลดีเรื่องแคล้วคลาด มีคนคอยช่วยเหลืออุปถัมภ์"
}

case object SumPlateNumberEqualsSeven extends SumPlateNumberFilter {
  override def number = "7"
  override def description: String = "เหมาะกับคนทำอาชีพการเกษตร แต่ส่งผลเสียด้านการเงินและการเดินทางมีโอกาสเกิดอุบัติเหตุได้ง่ายๆ"
}

case object SumPlateNumberEqualsEight extends SumPlateNumberFilter {
  override def number = "8"
  override def description: String = "เหมาะกับคนทำงานกลางคืน อาชีพเสี่ยงอันตราย อาชีพสีเทา นักโหราศาสตร์ ส่งผลให้ร่ำรวย มั่งมี มีคนยกย่อง"
}

case object SumPlateNumberEqualsNine extends SumPlateNumberFilter {
  override def number = "9"
  override def description: String = "เหมาะกับทุกคน ส่งผลให้มีสิ่งศักดิ์สิทธิ์คุ้มครอง แคล้วคลาดปลอดภัย ปราศจากอุบัติเหตุต่างๆ"
}

case object EasyGetUpsetNumber extends UnluckyNumberFilter { 
  override def description: String = "ใจร้อนขี้หงุดหงิด"
  override def numbers: TreeSet[String] =  TreeSet[String]("03", "30", "10", "01", "12", "21", "33", "07", "13", "31", "37", "73", "38", "83")
}

case object EasyGetAccidentNumber extends UnluckyNumberFilter {
  override def description: String = "เกิดอุบัติเหตุได้ง่าย"
  override def numbers: TreeSet[String] = TreeSet[String]("13", "31", "73", "37", "38", "83", "33")
}

case object OftenGetPaidRepairmentNumber extends UnluckyNumberFilter {
  override def description: String = "เสียเงินซ่อมรถเป็นประจำ"
  override def numbers: TreeSet[String] =  TreeSet[String]("10", "01", "02", "20", "03", "30", "13", "31", "33", "06", "60", "67", "76")
}

case object GetElectricProblemNumber extends UnluckyNumberFilter {
  override def description: String = "มีปัญหาเกี่ยวกับระบบไฟฟ้า"
  override def numbers: TreeSet[String] = TreeSet[String]("11", "91", "19")
}

case object GetScratchWhenParkingNumber extends UnluckyNumberFilter {
  override def description: String = "จอดอยู่เฉยๆ ก็มีรอยขีดข่วน"
  override def numbers: TreeSet[String] = TreeSet[String]("35", "53", "36", "63", "39", "93", "17", "71", "77", "76", "67")
}

case object UnluckyAndGetAccidentNumber extends UnluckyNumberFilter {
  override def description: String = "มักจะทำให้เกิดอุบัติเหตุ โชคไม่ดีในการขับขี่"
  override def numbers: TreeSet[String] = TreeSet[String]("13", "31", "37", "73", "30", "03", "1313", "3737", "3100", "3031")
}

case object UnluckyNumberSumEqualsThirteen extends UnluckyNumberFilter {
  override def description: String = "เลขทุกตัวบวกกันแล้วไม่ได้เลข 13, เลขคู่หน้าและคู่หลังบวกกันไม่ได้เลข 13, เลขแรกและเลขสุดท้ายบวกกันแล้วไม่ได้ 13"
  override def predicate(plate: LicensePlate): Boolean = {
    def isSumEualsThirteen(nums: Int*) = { nums.sum == 13 }
    def asInt(num: Char):Int = num.toString.toInt
    val isSumEqualsThirteen = plate.number.length match {
      case length if length == 2 => 
        isSumEualsThirteen(asInt(plate.number.head), asInt(plate.number.last))
      case length if length == 3 => 
        val firstNum = asInt(plate.number.head)
        val secondNum = asInt(plate.number(1))
        val thirdNum = asInt(plate.number.last)
        isSumEualsThirteen(firstNum, secondNum, thirdNum) ||
        isSumEualsThirteen(firstNum, thirdNum)
      case length if length == 4 => 
        val firstNum = asInt(plate.number.head)
        val secondNum = asInt(plate.number(1))
        val thirdNum = asInt(plate.number(2))
        val fourthNum = asInt(plate.number.last)
        isSumEualsThirteen(firstNum, secondNum, thirdNum, fourthNum) ||
        isSumEualsThirteen(firstNum, fourthNum) ||
        isSumEualsThirteen(firstNum, secondNum) ||
        isSumEualsThirteen(thirdNum, fourthNum)
      case _ => false
    }
    !isSumEqualsThirteen
  }
}

case object NumberLengthEqualsFourAndStartAndEndWithOne extends UnluckyNumberFilter {
  override def numbers: TreeSet[String] = TreeSet.empty
  override def description: String = ""
  override def predicate(plate: LicensePlate): Boolean = {
    !(plate.number.length == 4 && plate.number.head == '1' && plate.number.last == '1')
  }
}

case object SafetyNumber extends BlessingWithLuckyNumberFilter {
  override def numbers: TreeSet[String] = TreeSet("15", "51", "55", "49", "94", "95", "59", "99")
  override def description: String = "สิ่งศักสิทธิ์คุ้มครอง แคล้วคลาดปลอดภัย"
}

case object AttractionNumber extends BlessingWithLuckyNumberFilter {
  override def numbers: TreeSet[String] = TreeSet("22", "23", "32", "24", "42", "26", "62", "29", "92", "36", "63")
  override def description: String = "เมตตามหานิยม"
}

case object PopularityNumber extends BlessingWithLuckyNumberFilter {
  override def numbers: TreeSet[String] = TreeSet("15", "51", "35", "53", "45", "54", "89", "98", "99")
  override def description: String = "เพิ่มบารมี"
}

case object WealthyNumber extends BlessingWithLuckyNumberFilter {
  override def numbers: TreeSet[String] = TreeSet("24", "42", "36", "63", "66", "28", "82")
  override def description: String = "การค้าขายร่ำรวย"
}

case object SumOfFirstPairAndLastPairGreaterThanOrEqualsFive extends BlessingWithLuckyNumberFilter {
  override def description: String = "ดี (เลขที่มีผลรวมตั้งแต่ 5 ขึ้นไป)"
  override def predicate(plate: LicensePlate): Boolean = {
    plate.number.length == 4 && plate.number.map(num=> num.toInt).sum >=5
  }
}

case object SumOfFirstPairAndLastPairEqualsNine extends BlessingWithLuckyNumberFilter {
  override def description: String = "มีความเจริญก้าวหน้า (ผลรวมเลขคู่หน้า และผลรวมเลขคู่หลัง เป็น 9)"
  override def predicate(plate: LicensePlate): Boolean = {
    plate.number.length == 4 && plate.number.map(num=> num.toInt).sum == 9
  }
}

case object ContainsLuckyNumberSeven extends  BlessingWithLuckyNumberFilter {
  override def description: String = "เลขแห่งโชคลาภ (มีเลข 7 ในป้ายทะเบียน)"
  override def predicate(plate: LicensePlate): Boolean = {
    plate.number.contains("7")
  }
}

case object ContainsLuckyNumbeEight extends BlessingWithLuckyNumberFilter {
  override def description: String = "เหมาะกับการทำมาค้าขาย (มีเลข 8 ในป้ายทะเบียน)"
  override def predicate(plate: LicensePlate): Boolean = {
      plate.number.contains("8")
  }
}

case object ContainsLuckyNumbeNine extends BlessingWithLuckyNumberFilter {
  override def description: String = "ความเจริญก้าวหน้า (มีเลข 9 ในป้ายทะเบียน)"
  override def predicate(plate: LicensePlate): Boolean = {
      plate.number.contains("9")
  }
}

case object SundayUnluckyNumber extends CarOwnerBirthDayOfWeekWithUnluckyNumberFilter {
  override def dayOfWeek: String = "วันอาทิตย์"
  override def number: String = "6"
}

case object MondayUnluckyNumber extends CarOwnerBirthDayOfWeekWithUnluckyNumberFilter {
  override def dayOfWeek: String = "วันจันทร์"
  override def number: String = "1"
}

case object TuesdayUnluckyNumber extends CarOwnerBirthDayOfWeekWithUnluckyNumberFilter {
  override def dayOfWeek: String = "วันอังคาร"
  override def number: String = "2"
}

case object WendesdayDayUnluckyNumber extends CarOwnerBirthDayOfWeekWithUnluckyNumberFilter {
  override def dayOfWeek: String = "วันพุธ (กลางวัน)"
  override def number: String = "3"
}

case object WendesdayNightUnluckyNumber extends CarOwnerBirthDayOfWeekWithUnluckyNumberFilter {
  override def dayOfWeek: String = "วันพุธ (กลางคืน)"
  override def number: String = "5"
}

case object ThursdayUnluckyNumber extends CarOwnerBirthDayOfWeekWithUnluckyNumberFilter {
  override def dayOfWeek: String = "วันพฤหัสบดี"
  override def number: String = "7"
}

case object FridayUnluckyNumber extends CarOwnerBirthDayOfWeekWithUnluckyNumberFilter {
  override def dayOfWeek: String = "วันศุกร์"
  override def number: String = "8"
}

case object SaturdayUnluckyNumber extends CarOwnerBirthDayOfWeekWithUnluckyNumberFilter {
  override def dayOfWeek: String = "วันเสาร์"
  override def number: String = "4"
}

object MenuOptions {
  def getDayWithUnluckyNumbers(): ListMap[String, CarOwnerBirthDayOfWeekWithUnluckyNumberFilter] = {
    ListMap[String, CarOwnerBirthDayOfWeekWithUnluckyNumberFilter](
      "1" -> SundayUnluckyNumber,
      "2" -> MondayUnluckyNumber,
      "3" -> TuesdayUnluckyNumber,
      "4" -> WendesdayDayUnluckyNumber,
      "5" -> WendesdayNightUnluckyNumber,
      "6" -> ThursdayUnluckyNumber,
      "7" -> FridayUnluckyNumber,
      "8" -> SaturdayUnluckyNumber
    )
  }

  def getBlessingOptions(): ListMap[String, BlessingWithLuckyNumberFilter] = {
    ListMap[String, BlessingWithLuckyNumberFilter](
      "1" -> SafetyNumber,
      "2" -> AttractionNumber,
      "3" -> PopularityNumber,
      "4" -> WealthyNumber,
      "5" -> SumOfFirstPairAndLastPairGreaterThanOrEqualsFive,
      "6" -> SumOfFirstPairAndLastPairEqualsNine,
      "7" -> ContainsLuckyNumberSeven,
      "8" -> ContainsLuckyNumbeEight,
      "9" -> ContainsLuckyNumbeNine,
    )
  }

  def getPlateNumberSumOptions(): ListMap[String, SumPlateNumberFilter] = {
    ListMap[String, SumPlateNumberFilter](
      "1" -> SumPlateNumberEqualsOne,
      "2" -> SumPlateNumberEqualsTwo,
      "3" -> SumPlateNumberEqualsThree,
      "4" -> SumPlateNumberEqualsFour,
      "5" -> SumPlateNumberEqualsFive,
      "6" -> SumPlateNumberEqualsSeven,
      "7" -> SumPlateNumberEqualsSeven,
      "8" -> SumPlateNumberEqualsEight,
      "9" -> SumPlateNumberEqualsNine
    )
  }
}

object PlateFilter {
  def filter(
      plates: TreeSet[LicensePlate],
      filters: Set[FilterLicensePlate],
  ): TreeSet[LicensePlate] = {
    // plates.filter(plate => filters.forall(f => f.predicate(plate))) 
    val allFilters: Set[TreeSet[LicensePlate] => TreeSet[LicensePlate]] = (filters ++ getUnluckyNumberFilters()).map(f=> f.filter _)
    val combinedFilters: TreeSet[LicensePlate] => TreeSet[LicensePlate] = composedFilters(allFilters.head, allFilters.tail)
    combinedFilters(plates) 
  }

  def composedFilters(composedFilter: TreeSet[LicensePlate] => TreeSet[LicensePlate], filters: Set[TreeSet[LicensePlate] => TreeSet[LicensePlate]]): TreeSet[LicensePlate] => TreeSet[LicensePlate] = {
    if (filters.size == 0) {
      composedFilter
    } else if(filters.tail.isEmpty) {
      composedFilter andThen filters.head
    } else {
      composedFilters(composedFilter andThen filters.head, filters.tail)
    }
  }

  def getUnluckyNumberFilters(): Set[FilterLicensePlate] = {
    Set(EasyGetUpsetNumber,
        EasyGetAccidentNumber, 
        OftenGetPaidRepairmentNumber, 
        GetElectricProblemNumber, 
        GetScratchWhenParkingNumber, 
        UnluckyAndGetAccidentNumber,
        NumberLengthEqualsFourAndStartAndEndWithOne,
        UnluckyNumberSumEqualsThirteen)
  }
}

object PlateNumberCalculator {
  def sumUntilOneDigit(numberText: String): String = {
    if (numberText.length == 1)
      numberText
    else {
      val sum = numberText.map(_.toString.toInt).sum
      sumUntilOneDigit(sum.toString)
    }
  }
}

object LicensePlateGenerator {
  def generateLicensePlates(): TreeSet[LicensePlate] = {
    (1 to 9999).map(num => LicensePlate(num.toString())).to[TreeSet]
  }
}

object LuckyPlateLicense {
  def main(args: Array[String]): Unit = {
    val selectedBirthDayOfWeekFilter: Set[CarOwnerBirthDayOfWeekWithUnluckyNumberFilter] = Set(askForBirthDayOfWeek())
    val selectedOccupationFilters: Set[SumPlateNumberFilter] = askForOccupations()
    val selectedBlessingFilters: Set[FilterLicensePlate] = askForBlessing()
    val allSelectedFilter: Set[FilterLicensePlate] = (selectedBirthDayOfWeekFilter ++ selectedOccupationFilters ++ selectedBlessingFilters)
    val allLicensePlates: TreeSet[LicensePlate] = LicensePlateGenerator.generateLicensePlates()
    val luckyLicensePlates: TreeSet[LicensePlate] = PlateFilter.filter(allLicensePlates, allSelectedFilter)
    println(s"${luckyLicensePlates.map(l => l.number).mkString(", ")}")
    // val groupedPlates: Iterable[SumPlateNumberWithLicensePlate] = groupPlateNumberBySum(luckyLicensePlates)
    // println(s"ผลลัพธ์ทั้งหมด: ${groupedPlates.map(plate => plate.licensePlates.size).sum} รายการ")
    // println(groupedPlates.map(plate => s"\n${plate.sumPlateNumber.description} (${plate.licensePlates.size}) :\n\n${plate.licensePlates.map(p => p.number.toInt).mkString(", ")}\n\n").mkString("\n"))
  }

  def groupPlateNumberBySum(plates: TreeSet[LicensePlate])  = {
    val groupedPlates: Map[String, TreeSet[LicensePlate]] = plates.groupBy(p => PlateNumberCalculator.sumUntilOneDigit(p.number))
    val sumPlateNumbers: ListMap[String, SumPlateNumberFilter] = MenuOptions.getPlateNumberSumOptions()
    sumPlateNumbers.map(sumResult => SumPlateNumberWithLicensePlate(sumPlateNumbers(sumResult._1), groupedPlates(sumResult._1)))
  }

  def askForBirthDayOfWeek(): CarOwnerBirthDayOfWeekWithUnluckyNumberFilter = {
    val birthDayOfWeekOptions = MenuOptions.getDayWithUnluckyNumbers()
    println("กรุณาเลือกวันเกิดโดยการใส่ตัวเลข")
    println(birthDayOfWeekOptions.map(m => s"\n${m._1}) ${m._2.dayOfWeek}").mkString)
    val optionNumbers: Array[String] = birthDayOfWeekOptions.map(m => m._1).toArray
    val selectedBirthDayOfWeek: String = getSingleValueFromInput(optionNumbers, "กรุณาเลือกวันเกิดโดยการใส่ตัวเลข")
    birthDayOfWeekOptions(selectedBirthDayOfWeek)    
  }

  def askForBlessing(): Set[FilterLicensePlate] = {
    val blessingOptions = MenuOptions.getBlessingOptions()
    println("อยากเสริมความมงคลด้านไหน (เลือกตัวเลขได้มากกว่า 1 ข้อ ใช้ , ในการคั่นตัวเลือก)")
    println(blessingOptions.map(o => s"\n${o._1}) ${o._2.description}").mkString)
    val optionNumbers: Array[String] = blessingOptions.map(m => m._1).toArray
    val selectedBlessings: Array[String] = getMultipleValuesFromInput(optionNumbers, "กรุณาใส่ตัวเลข")
    selectedBlessings.map(b => blessingOptions(b)).toSet
  }

  def askForOccupations(): Set[SumPlateNumberFilter] = {
    val occupationOptions = MenuOptions.getPlateNumberSumOptions()
    println("อยากได้เลขที่เหมาะกับอาชีพอะไร ส่งเสริมด้านไหน (เลือกตัวเลขได้มากกว่า 1 ข้อ ใช้ , ในการคั่นตัวเลือก)")
    println(occupationOptions.map(o => s"\n${o._1}) ${o._2.description}").mkString)
    val optionNumbers: Array[String] = occupationOptions.map(m => m._1).toArray
    val selectedOccupations: Array[String] = getMultipleValuesFromInput(optionNumbers, "กรุณาใส่ตัวเลข")
    selectedOccupations.map(o => occupationOptions(o)).toSet
  }

  def getSingleValueFromInput(allowValues: Array[String], errorMessage: String): String = {
    val newInput = readLine()
    if (allowValues.exists(o => o == newInput)) {
      newInput
    }
    else
    {
      println(errorMessage)
      getSingleValueFromInput(allowValues, errorMessage)
    }
  }

  def getMultipleValuesFromInput(allowValues: Array[String], errorMessage: String): Array[String] = {
    val newInputs: Array[String] = readLine().split(",")
    if (newInputs.forall(newInput => allowValues.contains(newInput))) {
      newInputs
    } else {
      println(errorMessage)
      getMultipleValuesFromInput(allowValues, errorMessage)
    } 
  }
}
