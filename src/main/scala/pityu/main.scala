package pityu.sugar

class Scene extends Sugarkovetes {

  val fenyek: List[FenyForras] = List(
    FenyForras(Vec3(0, 0, 0), Szin(1, 1, 1)),
    FenyForras(Vec3(0, 1, 0), Szin(1, 1, 1))
  // FenyForras(Vec3(-2, -3, 1), Szin(0, 3, 0))
  )

  val anyag1 = AnyagTulajdonsagok(szin = Szin(1, 1, 1),
    ambiens = 0.0,
    reflektiv = 1.0,
    tores = 0.0, //Kr
    toresmutato = 1,
    emisszio = Szin(0, 0.0, 0.0),
    atlatszosag = 0.0,
    diffuz = 0.2,
    spekularis = 0.0,
    spekularisN = 16)

  val piros = AnyagTulajdonsagok(szin = Szin(1, 0, 0),
    ambiens = 0.2,
    reflektiv = 0.0,
    tores = 0.0, //Kr
    toresmutato = 0,
    emisszio = Szin(0, 0.1, 0.0),
    atlatszosag = 0.0,
    diffuz = 0.9,
    spekularis = 0.1,
    spekularisN = 16)

  val uveg = AnyagTulajdonsagok(szin = Szin(1, 1, 1),
    ambiens = 0.0,
    reflektiv = 0.0,
    tores = 1.0, //Kr
    toresmutato = 1.1,
    emisszio = Szin(0, 0, 0.0),
    atlatszosag = 1.0,
    diffuz = 0.0,
    spekularis = 0.0,
    spekularisN = 16)

  val objektumok: List[Metszheto] = (
    new Sik(Vec3(0, -1, 0), Vec3(0, 1, 0), anyag1) ::
    new Sik(Vec3(0, 0, 1), Vec3(0, 0, -1), piros) ::
    new EgyszeruGomb(Vec3(0, 0.5, 0.5), 0.3, anyag1) ::
    new EgyszeruGomb(Vec3(-0.3, 0.15, 0.15), 0.15, uveg) ::
    // new EgyszeruGomb(Vec3(0.0, 0, 0.3), 0.1, anyag2)
    Nil
  )

  val kornyezetiAmbiens = Szin(0.1, 0.1, 0.1)
}

object Proba1 extends App {
  val s = new Scene
  val w = 300
  val h = 300
  val x = Kepernyo.sugarkovetes(s, w, h, 5)
  val tomb = x.map(_.map(_.clamp).map(x => convertRGBToInt((x.r * 255.0).toInt, (x.g * 255.0).toInt, (x.b * 255.0).toInt, 255)).toArray).toArray

  // println(x)
  // println(x.map(_.map(_.b * 255)))
  // println(tomb.deep)

  val wi = new Window(w, h)
  wi.setImage(tomb)

  // w.close
}

