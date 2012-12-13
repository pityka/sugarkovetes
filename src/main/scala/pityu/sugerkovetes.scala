package pityu.sugar

import scala.math.{ pow, cos, acos, sqrt, tan, Pi }

object Seged {
  val Fekete = Szin(0, 0, 0)

  val Epsz = 0.0001

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
    val alfa = scala.math.acos(beeso.normalize dot normalis)
    import scala.math._
    val tmp = cos(alfa) / eta - sqrt(1.0 - ((1.0 - pow(cos(alfa), 2)) / pow(eta, 2)))

    beeso / eta + normalis * tmp
  }
}
import Seged._
case class Vec3(x: Double, y: Double, z: Double) {

  def +(that: Vec3) = Vec3(this.x + that.x, this.y + that.y, this.z + that.z)

  def -(that: Vec3) = Vec3(this.x - that.x, this.y - that.y, this.z - that.z)

  def /(d: Double) = Vec3(x / d, y / d, z / d)

  def *(d: Double) = Vec3(x * d, y * d, z * d)

  def normalize = this / this.hossz

  def cross(that: Vec3) = Vec3(1, 0, 0) * this.y * that.z + Vec3(0, 1, 0) * this.z * that.x + Vec3(0, 0, 1) * this.x * that.y - Vec3(1, 0, 0) * this.z * that.y - Vec3(0, 1, 0) * this.x * that.z - Vec3(0, 0, 1) * this.y * that.x

  def hossz = scala.math.sqrt(x * x + y * y + z * z)

  def nullaz = Vec3(if (this.x < Epsz) 0.0 else x,
    if (y < Epsz) 0.0 else y,
    if (z < Epsz) 0.0 else z)

  def dot(that: Vec3) = this.x * that.x + this.y * that.y + this.z * that.z
}

class Sugar private (val start: Vec3, val irany: Vec3) {
  def pontItt(p: Double) = start + irany * p

  def halad(d: Double) = Sugar(start + irany * d, irany)

  override def toString = "Sugar(" + start + "," + irany + ")"

}

object Sugar {
  def apply(start: Vec3, i: Vec3) = new Sugar(start, i.normalize)
}

case class Szin(r: Double, g: Double, b: Double) {
  def *(that: Double) = Szin(that * r, that * g, that * g)
  def *(that: Szin) = Szin(r * that.r, g * that.g, b * that.b)
  def +(that: Szin) = Szin(r + that.r, g + that.g, b + that.b)
  def /(that: Double) = this * (1 / that)

  def clamp = Szin(if (r > 1.0) 1.0 else { if (r < 0.0) 0.0 else r },
    if (g > 1.0) 1.0 else { if (g < 0.0) 0.0 else g },
    if (b > 1.0) 1.0 else { if (b < 0.0) 0.0 else b }
  )
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

trait Metszheto {

  def metszesSugarral(s: Sugar): Option[MetszesiEredmeny]

}

trait Sugarkovetes {

  val fenyek: List[FenyForras]

  val objektumok: List[Metszheto]

  val kornyezetiAmbiens: Szin

  // v(x,xI)
  def ketPontKozott(pont: Vec3, fenypont: Vec3): Double = {
    val sugarAPontbol = Sugar(pont, (fenypont - pont))
    val fenytav = (pont - fenypont).hossz

    sugarIndit(sugarAPontbol).filter { eredmeny =>
      (pont - eredmeny.pont).hossz <= fenytav
    }.map(_.anyag.atlatszosag).foldLeft(1.0)(_ * _)

  }

  // Li
  def inkoherensFeny(pont: Vec3, feny: FenyForras): Szin = {
    feny.szin / scala.math.pow(((pont - feny.hely).hossz), 2) * ketPontKozott(pont, feny.hely)
  }

  def sugarkovetesIndit(s: Sugar, szint: Int): Szin = {
    legkozelebbi(sugarIndit(s)) match {
      case None => Fekete
      case Some(metsz) => {
        // println(metsz)
        EIE(metsz.pont, metsz.normalis, metsz.anyag, s.start, szint)
      }
    }
  }

  def sugarIndit(sugar: Sugar): List[MetszesiEredmeny] =
    objektumok.map(_.metszesSugarral(sugar.halad(Epsz))).filter(_.isDefined).map(_.get)

  def legkozelebbi(l: List[MetszesiEredmeny]) = l.sortBy(_.parameter).headOption

  def EIE(pont: Vec3, normalis: Vec3, anyag: AnyagTulajdonsagok, nezopont: Vec3, szint: Int): Szin = {
    if (szint <= 0) Fekete
    else {
      // println("dfsd")
      val Le = anyag.emisszio

      val ambiens = kornyezetiAmbiens * anyag.ambiens

      val inkoherens: Szin = fenyek.map { feny =>
        (diffuzBRDF(pont, normalis, anyag.diffuz, anyag.szin, feny) +
          spekularisBRDF(pont, nezopont, normalis, feny, anyag.spekularis, anyag.spekularisN)) *
          inkoherensFeny(pont, feny) *
          ((feny.hely - pont).normalize dot normalis)
      }.reduce(_ + _)

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

class EgyszeruGomb(c: Vec3, r: Double, anyag: AnyagTulajdonsagok)
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
        // println((s, param, pont, t1, t2))
        MetszesiEredmeny(parameter1, pont, normalis.normalize, anyag)
      }

    } else None
  }
}

object Kepernyo {

  def sugarkovetes(s: Sugarkovetes, width: Int, height: Int, szint: Int): IndexedSeq[IndexedSeq[Szin]] = {
    val fovx = Pi / 2.0
    val fovy = Pi / 2.0
    val eye = Vec3(0, 0, -1)
    val center = Vec3(0, 0, 0)
    val up = Vec3(0, 1, 0)
    val w = (eye - center) / (eye - center).hossz
    val u = (up cross w) / (up cross w).hossz
    val v = w cross u

    def p(i: Int, j: Int): Vec3 = {
      val alfa: Double = tan(fovx / 2.0) * ((i - width / 2.0) / (width / 2.0))
      val beta: Double = tan(fovy / 2.0) * ((height / 2.0 - j) / (height / 2.0))

      eye + (u * alfa + v * beta - w)
    }

    for (i <- 1 to width) yield {
      for (j <- 1 to height) yield {
        val p0 = p(i, j)
        val v = (p0 - eye).normalize
        // println(Sugar(p0, v))
        s.sugarkovetesIndit(Sugar(p0, v), szint)
      }
    }
  }

}
