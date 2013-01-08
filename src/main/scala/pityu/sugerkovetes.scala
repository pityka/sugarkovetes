/**
 * Rekurziv sugarkovetes hazi. Bartha Istvan bartha.pityu@gmail.com
 *
 */
package pityu.sugar

import scala.math.{ pow, cos, acos, sqrt, tan, Pi }

/**
 * Sugarkoveteshez szukseges segedfuggvenyek.
 *
 */
object Seged {

  val rendezes = scala.math.Ordering.Double.on((x: MetszesiEredmeny) => x.parameter)

  val Fekete = Szin(0, 0, 0)

  val Epsz = 0.00001

  def megszorit(d: Double) = if (d < Epsz) 0.0 else d

  def diffuzBRDF(pont: Vec3, normalis: Vec3, kd: Double, anyagszin: Szin, feny: FenyForras) = {
    val fenyirany = feny.hely - pont

    anyagszin * feny.szin * megszorit(fenyirany.normalize dot normalis) * kd
  }

  def spekularisBRDF(pont: Vec3, nezopont: Vec3, normalis: Vec3, feny: FenyForras, ks: Double, n: Int) = {
    val nezetiirany = nezopont - pont
    val beesesi = (pont - feny.hely)
    val r = idealisTukorIrany(beesesi, normalis)

    val cosdelta = (r.normalize) dot (nezetiirany.normalize)

    feny.szin * ks * scala.math.pow(megszorit(cosdelta), n)

  }

  def idealisTukorIrany(beeso: Vec3, normalis: Vec3): Vec3 =
    beeso.normalize - normalis * (normalis dot beeso.normalize) * 2

  def idealisToresIrany(beeso: Vec3, normalis: Vec3, eta: Double): Vec3 = {
    val cosalfa = (beeso.normalize * (-1.0)) dot normalis
    import scala.math._
    val tmp = cosalfa / eta - sqrt(1.0 - ((1.0 - pow(cosalfa, 2)) / pow(eta, 2)))

    beeso / eta + normalis * tmp

  }

  def haromszognormalis(a: Vec3, b: Vec3, c: Vec3) = ((c - a) cross (b - a)).normalize

  def delta(a: Vec3, b: Vec3, c: Vec3): Double = ((b - a) cross (c - a)) dot haromszognormalis(a, b, c)

  val XYnorm = Vec3(0, 0, 1)
  val XZnorm = Vec3(0, 1, 0)
  val YZnorm = Vec3(1, 0, 0)
  val koordinataNormalisok = List(XYnorm, XZnorm, YZnorm)

}
import Seged._
/**
 * Haromelemu vektor. Mind pont, mind vektor abrazolasara.
 */
case class Vec3(x: Double, y: Double, z: Double) {

  def +(that: Vec3) = Vec3(this.x + that.x, this.y + that.y, this.z + that.z)

  def -(that: Vec3) = Vec3(this.x - that.x, this.y - that.y, this.z - that.z)

  def /(d: Double) = Vec3(x / d, y / d, z / d)

  def *(d: Double) = Vec3(x * d, y * d, z * d)

  /** Elemenkent. */
  def *(that: Vec3) = Vec3(x * that.x, y * that.y, z * that.z)

  def normalize = this / this.hossz

  /** Vektorialis. */
  def cross(that: Vec3) = Vec3(1, 0, 0) * this.y * that.z + Vec3(0, 1, 0) * this.z * that.x + Vec3(0, 0, 1) * this.x * that.y - Vec3(1, 0, 0) * this.z * that.y - Vec3(0, 1, 0) * this.x * that.z - Vec3(0, 0, 1) * this.y * that.x

  def hossz = scala.math.sqrt(x * x + y * y + z * z)

  /** Skalaris. */
  def dot(that: Vec3) = this.x * that.x + this.y * that.y + this.z * that.z
}

/**
 *
 */
class Sugar private (val start: Vec3, val irany: Vec3) {
  /** Kiertekeli a sugarat. */
  def pontItt(p: Double) = start + irany * p

  /** Halad valamennyit a sugar iranyaba. */
  def halad(d: Double) = Sugar(start + irany * d, irany)

  override def toString = "Sugar(" + start + "," + irany + ")"

}

object Sugar {
  def apply(start: Vec3, i: Vec3) = new Sugar(start, i.normalize)
}

class Szin private (
    var r: Double,
    var g: Double,
    var b: Double) {
  def *(that: Double) = Szin(that * r, that * g, that * b)
  def *(that: Szin) = Szin(r * that.r, g * that.g, b * that.b)
  def +(that: Szin) = Szin(r + that.r, g + that.g, b + that.b)
  def hozzaad(that: Szin) {
    r += that.r
    g += that.g
    b += that.b
  }
  def /(that: Double) = this * (1 / that)

  /** [0,1] koze szoritja az erteket. */
  def clamp = Szin(
    if (r > 1.0) 1.0 else { if (r < 0.0) 0.0 else r },
    if (g > 1.0) 1.0 else { if (g < 0.0) 0.0 else g },
    if (b > 1.0) 1.0 else { if (b < 0.0) 0.0 else b }
  )

}
object Szin {
  def apply(r: Double, g: Double, b: Double) = new Szin(r, g, b)
}

case class FenyForras(hely: Vec3, szin: Szin)

case class AnyagTulajdonsagok(szin: Szin,
  ambiens: Double,
  reflektiv: Double,
  tores: Double, //Kr
  toresmutato: Double,
  emisszio: Szin,
  atlatszosag: Double,
  diffuz: Double,
  spekularis: Double,
  spekularisN: Int)

case class MetszesiEredmeny(parameter: Double, pont: Vec3, normalis: Vec3, anyag: AnyagTulajdonsagok)

/** Minden metszheto objektum ose. */
trait Metszheto {

  def metszesSugarral(s: Sugar): Option[MetszesiEredmeny]

}

/** Rekurziv sugarkovetes algoritmus. */
trait Sugarkovetes {

  /** Ezek reprezentaljak a szinterek. */
  val fenyek: List[FenyForras]

  /** Ezek reprezentaljak a szinterek. */
  val objektumok: List[Metszheto]

  /** Ezek reprezentaljak a szinterek. */
  val kornyezetiAmbiens: Szin

  // Segedfuggvenyek

  // v(x,xI)
  private def ketPontKozott(pont: Vec3, fenypont: Vec3): Double = {
    val sugarAPontbol = Sugar(pont, (fenypont - pont))
    val fenytav = (pont - fenypont).hossz

    sugarIndit(sugarAPontbol).filter { eredmeny =>
      (pont - eredmeny.pont).hossz <= fenytav
    }.map(_.anyag.atlatszosag).foldLeft(1.0)(_ * _)

  }

  // Li
  private def inkoherensFeny(pont: Vec3, feny: FenyForras): Szin = {
    feny.szin / scala.math.pow(((pont - feny.hely).hossz), 2) * ketPontKozott(pont, feny.hely)
  }

  /** Belepes az algoritmusba. */
  def sugarkovetesIndit(s: Sugar, szint: Int): Szin = {
    legkozelebbi(sugarIndit(s)) match {
      case None => Fekete
      case Some(metsz) => {
        EIE(metsz.pont, metsz.normalis, metsz.anyag, s.start, szint)
      }
    }
  }

  private def sugarIndit(sugar: Sugar): List[MetszesiEredmeny] = {
    val s2 = sugar.halad(Epsz)
    objektumok.map(_.metszesSugarral(s2)).filter(_.isDefined).map(_.get)
  }

  private def legkozelebbi(l: List[MetszesiEredmeny]) = l.sorted(Seged.rendezes).headOption

  /** Egszerusitett illuminacios egyenlet. */
  private def EIE(pont: Vec3, normalis: Vec3, anyag: AnyagTulajdonsagok, nezopont: Vec3, szint: Int): Szin = {
    if (szint <= 0) Fekete
    else {

      val Le = anyag.emisszio

      val ambiens = kornyezetiAmbiens * anyag.ambiens

      val inkoherens: Szin = fenyek.map { feny =>
        (diffuzBRDF(pont, normalis, anyag.diffuz, anyag.szin, feny) +
          spekularisBRDF(pont, nezopont, normalis, feny, anyag.spekularis, anyag.spekularisN)) *
          inkoherensFeny(pont, feny) *
          ((feny.hely - pont).normalize dot normalis)
      }.reduceLeft { (x, y) => y.hozzaad(x); y }

      val tukorirany = idealisTukorIrany(pont - nezopont, normalis)
      val tukor = legkozelebbi(sugarIndit(Sugar(pont, tukorirany))).map { metsz =>
        EIE(metsz.pont, metsz.normalis, metsz.anyag, pont, szint - 1)
      }.getOrElse(Fekete) * anyag.reflektiv

      val toresirany = idealisToresIrany(pont - nezopont, normalis, anyag.toresmutato)
      val tores = legkozelebbi(sugarIndit(Sugar(pont, toresirany))).map { metsz =>
        EIE(metsz.pont, metsz.normalis, metsz.anyag, pont, szint - 1)
      }.getOrElse(Fekete) * anyag.tores

      Le + ambiens + inkoherens + tukor + tores

    }
  }

}

// Innentol metszheto objektumok kovetkeznek. Implicit (normalvektoros) sik, gomb, es haromszog.

class Sik(pont: Vec3, normalis: Vec3, anyag: AnyagTulajdonsagok)
    extends Metszheto {

  def metszesSugarral(s: Sugar): Option[MetszesiEredmeny] = {
    val t = if ((normalis dot s.irany) != 0.0)
      (normalis dot (pont - s.start)) / (normalis dot s.irany) else -1
    if (t > 0) {
      val metszespont = s.pontItt(t)

      Some(MetszesiEredmeny(t, metszespont, normalis, anyag))
    } else None
  }

}

class Gomb(c: Vec3, r: Double, anyag: AnyagTulajdonsagok)
    extends Metszheto {
  def metszesSugarral(s: Sugar): Option[MetszesiEredmeny] = {
    val p0mC = s.start - c
    val determinans = {

      pow(2 * (s.irany dot p0mC), 2) -
        4 * (s.irany dot s.irany) * (
          (p0mC dot p0mC) - pow(r, 2)
        )
    }

    if (determinans >= Epsz) {
      val b = 2 * (s.irany dot p0mC)
      val a = s.irany dot s.irany
      val cc = p0mC dot p0mC

      val t1 = (-1 * b - sqrt(determinans)) / (2 * a)
      val t2 = (-1 * b + sqrt(determinans)) / (2 * a)

      val param = if (t1 >= Epsz && t2 >= Epsz) {
        if (t1 >= t2) Some(t2) else Some(t1)
      } else {
        if (t1 >= Epsz) Some(t1)
        else if (t2 >= Epsz) Some(t2)
        else None
      }

      param.map { parameter1 =>
        val pont = s.pontItt(parameter1)
        val normalis = pont - c
        MetszesiEredmeny(parameter1, pont, normalis.normalize, anyag)
      }

    } else None
  }
}

case class Haromszog(a: Vec3, b: Vec3, c: Vec3, anyag: AnyagTulajdonsagok)
    extends Metszheto {
  lazy val normalis = haromszognormalis(a, b, c)

  lazy val sik = new Sik(a, normalis, anyag)

  lazy val el0 = b - a
  lazy val el1 = c - b
  lazy val el2 = a - c

  def metszesSugarral(sugar: Sugar): Option[MetszesiEredmeny] = {
    sik.metszesSugarral(sugar).filter { metsz =>
      val p = metsz.pont

      val aa = p - a
      val bb = p - b
      val cc = p - c

      // Ezt a megoldast konnyebben megertem. Ha a befele mutato vektorok (aa,bb,cc) ugyanabba az iranyba mutatnak az adott ellel, akkor a vektorialis szorzatuk ugyanarra az iranyba mutat. http://www.scratchapixel.com/lessons/3d-basic-lessons/lesson-xx-ray-triangle-intersection/ray-triangle-intersection-geometric-solution/
      (
        (normalis dot (el0 cross aa)) < Epsz &&
        (normalis dot (el1 cross bb)) < Epsz &&
        (normalis dot (el2 cross cc)) < Epsz)

      /*

      Ez itt az oran megadott modszer, nekem nem ment.

      val projekcioNormalisa = koordinataNormalisok.map(x => (x, x dot this.normalis)).maxBy(x => scala.math.abs(x._2))._1
      // Ez ott nulla amelyik koordinatat elhagyjuk
      val seged = Vec3(1, 1, 1) - projekcioNormalisa

      // println(projekcioNormalisa)
      // println(seged)

      def szetszed(v: Vec3) = v match {
        case Vec3(a, b, 0) => (a, b)
        case Vec3(a, 0, b) => (a, b)
        case Vec3(0, a, b) => (a, b)
      }
      // Kiejtjuk az erintett koordinatat
      val aa = szetszed(seged * a)
      val bb = szetszed(seged * b)
      val cc = szetszed(seged * c)
      val pp = szetszed(seged * p)
      // println(p)
      // println((aa, bb, cc, pp))

      val l1 = (
        (bb._2 - cc._2) * (pp._1 - cc._1) - (bb._1 - cc._1) * (pp._2 - cc._2)
      ) / (
          (aa._1 - cc._1) * (bb._2 - cc._2) - (bb._1 - cc._1) * (aa._2 - cc._2)
        )

      val l2 = (
        -1 * (aa._2 - cc._2) * (pp._1 - cc._1) - (aa._1 - cc._1) * (pp._2 - cc._2)
      ) / (
          (aa._1 - cc._1) * (bb._2 - cc._2) - (bb._1 - cc._1) * (aa._2 - cc._2)
        )

      val l3 = 1.0 - l1 - l2

      (l1 >= 0 && l1 <= 1 &&
        l2 >= 0 && l2 <= 1 &&
        l3 >= 0 && l3 <= 1)
      */
    }
  }

}

/** Kamera (kepernyo) tulajdonsagai, es a sugarak inditasa. */
object Kepernyo {

  val fovx = Pi / 2.0
  val fovy = Pi / 2.0
  val eye = Vec3(0, 0, -1)
  val center = Vec3(0, 0, 0)
  val up = Vec3(0, 1, 0)

  def sugarkovetes(s: Sugarkovetes, width: Int, height: Int, szint: Int): IndexedSeq[IndexedSeq[Szin]] = {

    val w = (eye - center) / (eye - center).hossz
    val u = (up cross w) / (up cross w).hossz
    val v = w cross u

    def p(i: Int, j: Int): Vec3 = {
      val alfa: Double = tan(fovx / 2.0) * ((i - width / 2.0) / (width / 2.0))
      val beta: Double = tan(fovy / 2.0) * ((height / 2.0 - j) / (height / 2.0))

      eye + (u * alfa + v * beta - w)
    }

    (for (i <- (1 to width).par) yield {
      for (j <- 1 to height) yield {
        val p0 = p(i, j)
        val v = (p0 - eye).normalize
        s.sugarkovetesIndit(Sugar(p0, v), szint)
      }
    }).seq.toIndexedSeq
  }

}
