package pityu.sugar

object Scene extends Sugarkovetes {

  val hatterZold = AnyagTulajdonsagok(szin = Szin(1, 1, 1),
    ambiens = 0.1,
    reflektiv = 0.0,
    tores = 0.0, //Kr
    toresmutato = 1,
    emisszio = Szin(0, 0.0, 0.0),
    atlatszosag = 0.0,
    diffuz = 0.3,
    spekularis = 0.0,
    spekularisN = 16)

  val hatterFeher = AnyagTulajdonsagok(szin = Szin(0.5, 0.8, 0.5),
    ambiens = 0.1,
    reflektiv = 0.0,
    tores = 0.0, //Kr
    toresmutato = 1,
    emisszio = Szin(0, 0.0, 0.0),
    atlatszosag = 0.0,
    diffuz = 0.3,
    spekularis = 0.0,
    spekularisN = 16)

  val mattKek = AnyagTulajdonsagok(szin = Szin(0, 0, 1),
    ambiens = 0.00,
    reflektiv = 0.0,
    tores = 0.0, //Kr
    toresmutato = 1,
    emisszio = Szin(0, 0.0, 0.0),
    atlatszosag = 0.0,
    diffuz = 0.1,
    spekularis = 0.1,
    spekularisN = 3)

  val piros = AnyagTulajdonsagok(szin = Szin(1, 0, 0),
    ambiens = 0.05,
    reflektiv = 0.3,
    tores = 0.3, //Kr
    toresmutato = 1.0,
    emisszio = Szin(0, 0.0, 0.0),
    atlatszosag = 0.7,
    diffuz = 0.2,
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

  val tukor = AnyagTulajdonsagok(szin = Szin(1, 1, 1),
    ambiens = 0.0,
    reflektiv = 0.7,
    tores = 0.2, //Kr
    toresmutato = 1.05,
    emisszio = Szin(0, 0, 0.0),
    atlatszosag = 0.8,
    diffuz = 0.0,
    spekularis = 0.1,
    spekularisN = 16)

  // csucsok
  val lentbalkozel = Vec3(0.4, -0.8, 0.1)
  val lentjobbkozel = Vec3(-0.4, -0.8, 0.1)
  val lentbaltavol = Vec3(0.4, -0.8, 0.7)
  val lentjobbtavol = Vec3(-0.4, -0.8, 0.7)
  val fentbalkozel = Vec3(0.4, 0.8, 0.1)
  val fentjobbkozel = Vec3(-0.4, 0.8, 0.1)
  val fentbaltavol = Vec3(0.4, 0.8, 0.7)
  val fentjobbtavol = Vec3(-0.4, 0.8, 0.7)

  val lentbalbalkozel = Vec3(0.8, -0.8, 0.1)
  val fentbalbalkozel = Vec3(0.8, 0.8, 0.1)

  val fenyek: List[FenyForras] = List(
    FenyForras(Vec3(1, 0, 0), Szin(1, 1, 1)),
    FenyForras(Vec3(-1, 1, 0), Szin(1, 1, 1)),
    FenyForras(Vec3(0, -0.6, 0.2), Szin(0, 0, 1)),
    FenyForras(Vec3(0, 0.6, 0.2), Szin(0, 0.5, 0))
  )

  val objektumok: List[Metszheto] = (
    new Sik(Vec3(0, -1, 0), Vec3(0, 1, 0), hatterFeher) ::
    new Sik(Vec3(0, 0, 1), Vec3(0, 0, -1), hatterZold) ::
    new Gomb(Vec3(0, 0.4, 0.5), 0.2, piros) ::
    new Gomb(Vec3(0.8, 0.4, 0.6), 0.2, piros) ::

    Haromszog(lentjobbkozel, lentbalkozel, lentbaltavol, mattKek) ::
    Haromszog(lentjobbtavol, lentjobbkozel, lentbaltavol, mattKek) ::
    Haromszog(fentbalkozel, fentjobbkozel, fentbaltavol, tukor) ::
    Haromszog(fentjobbkozel, fentjobbtavol, fentbaltavol, tukor) ::

    // Haromszog(lentbalkozel, fentbalkozel, lentjobbkozel, uveg) ::
    // Haromszog(lentjobbkozel, fentbalkozel, fentjobbkozel, uveg) ::

    Haromszog(lentbaltavol, fentbaltavol, lentjobbtavol, mattKek) ::
    Haromszog(lentjobbtavol, fentbaltavol, fentjobbtavol, mattKek) ::

    Haromszog(lentbaltavol, lentbalbalkozel, fentbalbalkozel, uveg) ::
    Haromszog(lentbaltavol, fentbalbalkozel, fentbaltavol, uveg) ::

    Haromszog(lentjobbkozel, lentjobbtavol, fentjobbkozel, uveg) ::
    Haromszog(fentjobbkozel, lentjobbtavol, fentjobbtavol, uveg) ::
    Nil
  )

  val kornyezetiAmbiens = Szin(1, 1, 1)
}

object Proba1 extends App {

  val w = 2000
  val h = 2000
  val rekurzioszint = 6

  val szinektombje = Kepernyo.sugarkovetes(Scene, w, h, rekurzioszint)

  // atalakitjuk bufferedimage kompatibilisse
  val tomb = szinektombje.map(_.map(_.clamp).map(x => convertRGBToInt((x.r * 255.0).toInt, (x.g * 255.0).toInt, (x.b * 255.0).toInt, 255)).toArray).toArray

  val bi = arrayToImage(tomb)

  val wi = new Window(w, h)
  val outputfile = new java.io.File("saved.png");
  javax.imageio.ImageIO.write(bi, "png", outputfile);
  wi.setImage(bi)

  // w.close
}

