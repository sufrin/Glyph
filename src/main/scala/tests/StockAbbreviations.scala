package org.sufrin.glyph
package tests

object StockAbbreviations {


    val lineDrawing = List(
      ("||" -> "\u2502"), //# |
      ("[-" -> "\u250c"), //#  ┌
      ("-]" -> "\u2514"), //# └
      ("{{" -> "\u23a8"), // # ⎨
      ("}}" -> "\u23ac"), // # ⎬
    )

    val greekUpper = List(
      ("Alpha" -> "\u0391"), //
      ("Beta" -> "\u0392"), //
      ("Gamma" -> "\u0393"), //
      ("Delta" -> "\u0394"), //
      ("Epsilon" -> "\u0395"), //
      ("Zeta" -> "\u0396"), //
      ("Eta" -> "\u0397"), //
      ("Theta" -> "\u0398"), //
      ("Iota" -> "\u0399"), //
      ("Kappa" -> "\u039A"), //
      ("Lambda" -> "\u039B"), //
      ("Mu" -> "\u039C"), //
      ("Nu" -> "\u039D"), //
      ("Xi" -> "\u039E"), //
      ("Omicron" -> "\u039F"), //
      ("Pi" -> "\u03A0"), //
      ("Rho" -> "\u03A1"), //
      ("Sigma" -> "\u03A2"), //
      ("Sigma" -> "\u03A3"), //
      ("Tau" -> "\u03A4"), //
      ("Upsilon" -> "\u03A5"), //
      ("Phi" -> "\u03A6"), //
      ("Chi" -> "\u03A7"), //
      ("Psi" -> "\u03A8"), //
      ("Omega" -> "\u03A9"), //
    )

    val arrows = List(
      "->" -> "→",
      //"<->" -> "↔",
      "│-" -> "\u251C",
      "<=" -> "\u21D0",       // left double ⇐
      "^=>" -> "\u21D1",      // up double
      "=>" -> "\u21D2",       // right double
      "_=>" -> "\u21D3",      // down double
      //"<=>" -> "\u21D4",    // left right double
      ("⇐>" -> "\u21D4"),     //
      ("<-" -> "\u2190"),     //
      ("->" -> "\u2192"),     //  # →
      ("<-" -> "\u2190"),     //
      ("^->" -> "\u2191"),    //   # ↑
      ("->" -> "\u2192"),     //   # →
      ("_->" -> "\u2193"),    //   # ↓
      ("←>" -> "\u2194"),     //  # type <-> when live
      (".→→" -> "⇉"),          //  # double right arrow
      (".\u2190\u2190" -> "⇇"),          //  # type <-<-

      ("+/>" -> "\u219b"),    //          # ↛
      ("+>" -> "\u21f8"),     //          # ⇸
      ("++>" -> "\u21fb"),    //          # ⇻
      ("->>" -> "\u21a0"),    //          # ↠
      (">->" -> "\u21a3"),    //          # ↣
      ("\\to" -> "\u21a6"),   //          # ↦
      //("|->" -> "\u21a6"),  //          # ↦

      ("\\uaa" -> "\u21D1"),  //          # ⇑
      ("\\daa" -> "\u21D3"),  //          # ⇓
      //("<->" -> "\u2194"),  //          # ↔

      ("\u21D0==" -> "\u21DA"), //      #  <===
      ("===>" -> "\u21DB"), //          # ⇛
      ("\u21DA=" -> "\u27f8"), //       # <====
      "===>" -> "\u21DB" , //          # ⇛
      ("<====" -> "\u27f8"), //          #
      ("====>" -> "\u27f9"), //          # ⇛
      ("<~~" -> "\u21DC"), //          # ⇜
      ("~~>" -> "\u21DD"), //          # ⇝

      ("lwa" -> "\u21E6"), //          # ⇦
      ("uwa" -> "\u21E7"), //          # ⇧
      ("rwa" -> "\u21E8"), //          # ⇨
      ("dwa" -> "\u21E9"), //          # ⇩

      ("\\lefthand", "\u261a"),
      ("\\righthand", "\u261b"),
      ("\\indexup", "\u261d"),
      ("\\indexdown", "\u261f"),
      )

    val abbreviations = List(


      "[=" -> "\u2291", // ⊑
      "]=" -> "\u2292", // ⊒
      "[[" -> "\u228f", // ⊏
      "]]" -> "\u2290", // ⊐
      "\\nand" -> "\u22bc", // ⊼

      ("[|" -> "\u27e6"), // # ⟦
      ("|]" -> "\u27e7"), // # ⟧
      ("(|" -> "\u2e28"), // # ⸨
      ("|)" -> "\u2e29"), // # ⸩
      ("{|" -> "\u2774"), // # ❴
      ("|}" -> "\u2775"), // # ❵

      ("\\ex" -> "\u2203"), //  # ∃
      ("\\notex" -> "\u2204"), //  # ∄
      //("∃/" -> "\u2204"), //  # ∄
      ("\\in" -> "\u2208"), //  # ∈
      ("\\notin" -> "\u2209"), //  # ∉
      //("∈/" -> "\u2209"), //  # ∉


      ("\\subeq" -> "\u227c"), // # ≼
      ("\\domeq" -> "\u227d"), // # ≽
      ("\\sub" -> "\u227a"), // # ≺
      ("\\dom" -> "\u227b"), // # ≻
      ("\\leq" -> "\u2264"), // # ≤
      ("\\geq" -> "\u2265"), // # ≥
      ("\\neq" -> "\u2260"), // # ≠

      ("[=" -> "\u2291"), // # ⊑
      //("=]" -> "\u2292"), // # ⊒
      ("[[" -> "\u228f"), // # ⊏
      ("]]" -> "\u2290"), // # ⊐
      //("nand" -> "\u22bc"), // # ⊼

      // Quotation marks
      ("``" -> "\u201c"), // # “
      ("''" -> "\u201d"), // # ”
      ("`" -> "\u2018"), // # ‘
      ("'" -> "\u2019"), // # ’



      ("\\zzz" -> "\u21af"), //          # ↯ // downwards zigzag



      ("\\subset" -> "\u2282"), //          # ⊂
      ("\\supset" -> "\u2283"), //          # ⊃
      ("\\=subset" -> "\u2286"), //          # ⊆
      ("\\=supset" -> "\u2287"), //          # ⊇



      ("[]" -> "\u220E"), //          # ∎
      ("compose" -> "\u2218"), //          # ∘
      ("dot" -> "\u00b7"), //          # ·
      ("bull" -> "\u2219"), //          # ∙
      ("tick" -> "\u221a"), //          # √
      ("||" -> "\u2225"), //          # ∥

      ("euro" -> "\u20ac"), //          # €
      ("pound" -> "\u20a4"), //          # ₤

      ("\\not" -> "\u00AC"), //          # ¬
      ("\\land" -> "\u2227"), //          # ∧
      //("/\\" -> "\u2227"), //          # ∧
      ("\\lor" -> "\u2228"), //          # ∨
      //("\\/" -> "\u2228"),
      ("\\cap" ->     "\u2229"),  //       # ∩
      ("\\cup" -> "\u222a"), //         # ∪

      ("\\so" -> "\u2234"), //          # ∴
      ("\\coz" -> "\u2235"), //          # ∵

      (":=" -> "\u2254"), //          # ≔
      ("\\Defs" -> "\u225c"), //          # ≜
      ("\\defs" -> "\u2258"), //          # ≘
      ("\\eqv" -> "\u2261"), //          # ≡
      //("xo" -> "\u2297"), //          # ⊗
      ("ox" -> "\u2297"), //          # ⊗
      ("o+" -> "\u2295"), //          # ⊕
      //("+o" -> "\u2295"), //          # ⊕
      //("|-" -> "\u22a2"), //          # ⊢
      //("true" -> "\u22a4"), //          # ⊤
      //("false" -> "\u22a5"), //          # ⊥
      ("\\top" -> "\u22a4"), //          # ⊤
      ("\\bot" -> "\u22a5"), //          # ⊥
      ("|=" -> "\u22a7"), //          # ⊧



      ("(c)" -> "\u00a9"), //  # ©
      ("><" -> "\u00d7"), //  # ×
      ("O/" -> "\u00D8"), //  # Ø
      ("<<" -> "\u00AB"), //  # «
      (">>" -> "\u00BB"), //  # »

      ("\\So" -> "\u2042"), //  # ⁂
      ("--" -> "\u2014"), //  # — (wide minus)
      ("ae" -> "æ"), // ᴂ
      ("Ae" -> "Æ"), // ᴂ
      )

      val composites = List(
        ("(c)", "©"),
        ("(r)", "\u00AE"),
        ("<3", "❤"),
        (":-|", "\uD83D\uDE10"),
        (":)", "\uD83D\uDE00"),
        (":O",  "\uD83D\uDE2E"),
        ("ukflag" -> "\uD83C\uDDEC\uD83C\uDDE7"),
        ("rainbow" -> "\uD83C\uDF08"),
        ("xgreen" -> "❎"),
        ("xred" -> "❌"),
        ("√green" -> "✅"),
        ("ored" -> "⭕"),
        ("star"->"⭐"),
        ("family" -> "\uD83E\uDDD1"),
        ("footprints" -> "\uD83D\uDC63"),
        ("faceinclouds" -> "\uD83D\uDE36\u200D\uD83C\uDF2B\uFE0F"),

      )

      val LRI =0x2066
      val PDI =0x2069
      val LRM = 0x200E
      val RLM = 0x200F

      val bidi = List(
        ("LRI" -> "\u2066"),
        ("RLI" -> "\u2067"),
        ("PDI" -> "\u2069"),
        ("LRM" -> "\u200E"),
        ("RLM" -> "\u200F")
      )

      def sloshName(s: String): String = s"$s/"
      def sloshLowerCaseName(s: String): String = s"${s.toLowerCase}/"

      def toNonRLM(string: String): String = {
        if (string.length>2 && string(0)==RLM && string.last==LRM)
          string.substring(1, string.length-1)
        else
          string
      }

      /** hebrew letters as unicode bidi isolates */
      val hebrew = {
        val names="alef/bet/gimel/dalet/he/vav/zayin/het/tet/yod/.kaf/kaf/lamed/.mem/mem/.nun/nun/samekh/ayin/.pe/pe/.tsadi/tsadi/qof/resh/shin/tav".split('/').toSeq
        val start = 0X05D0
        val codes = start until (start+names.length)
        names.map(sloshName).zip(codes.map(code=>f"$code%c"))
      }

      /** greek letters (upper and lower) */
      val greek = {
        val names = "Alpha/Beta/Gamma/Delta/Epsilon/Zeta/Eta/Theta/Iota/Kappa/Lambda/Mu/Nu/Xi/Omicron/Pi/Rho/.Sigma/Sigma/Tau/Upsilon/Phi/Chi/Psi/Omega".split('/').toSeq
        def codes(start: Int): Seq[Int] = start until (start+names.length)
        val exception: ((String,String))=>Boolean = {
          case (".Sigma\\", _) => true
          case other => false
        }
        val uppercase = names.map(sloshName).zip(codes(0X0391).map(code=>f"$code%c")).filterNot(exception) // there's no final capital Sigma
        val lowercase = names.map(sloshLowerCaseName).zip(codes(0X03B1).map(code=>f"$code%c"))
        uppercase++lowercase
      }

      /** Best in an explicit symbol font, else the scaling for the Skia fallback fails  */
      val playingCards = {
        val suits = "spades/hearts/diamonds/clubs".split('/').toSeq
        val cards = "1/2/3/4/5/6/7/8/9/10/jack/queen/king".split('/').toSeq
        val blocks = suits.zip(List(0x1F0A0, 0x1F0B0, 0x1F0C0, 0x1F0D0))
        for { (suit, start) <- blocks; (n, card) <-(1 to cards.length).zip(cards) } yield (s"$card$suit\\"->f"${start+n}%c")
      }

      val cardSuits = {
        val suits = "spades/hearts/diamonds/clubs".split('/').toSeq
        suits.map(sloshName).zip(List("\u2660", "\u2665", "\u2666", "\u2663"))
      }

      def toCharString(u: Int): String = new String(Array(u.toChar),0,1)

      val superscriptDigits = {
        // UNICODE COMPATIBILITY LUNACY
        val names = "0/1/2/3/4/5/6/7/8/9".split('/').toSeq
        val codes: Seq[Int] = List(0x2070, 0x00B9, 0x00B2, 0x00B3)++(0x2074 to 0x2079)
        (names.map{s=>s"^$s"}).zip(codes.map(toCharString))
      }

     val subscripts = List(
       ("_a" -> "\u2090"), //  #
       ("_e" -> "\u2091"), //  #
       ("_o" -> "\u2092"), //  #
       ("_x" -> "\u2093"), //  #
       ("_h" -> "\u2095"), //  #
       ("_k" -> "\u2096"), //  #
       ("_l" -> "\u2097"), //  #
       ("_m" -> "\u2098"), //  #
       ("_n" -> "\u2099"), //  #
       ("_p" -> "\u209A"), //  #
       ("_s" -> "\u209B"), //  #
       ("_t" -> "\u209C"), //  #
       ("_0" -> "\u2080"), //  # ₀ FOO₀
       ("_1" -> "\u2081"), //  # ₁
       ("_2" -> "\u2082"), //  # ₂
       ("_3" -> "\u2083"), //  # ₃
       ("_4" -> "\u2084"), //  # ₄
       ("_5" -> "\u2085"), //  # ₅
       ("_6" -> "\u2086"), //  # ₆
       ("_7" -> "\u2087"), //  # ₇
       ("_8" -> "\u2088"), //  # ₈
       ("_9" -> "\u2089"), //  # ₉
       ("_+_" -> "\u208A"), //  # ₊ foo₊
       ("_-_" -> "\u208B"), //  # ₋
       ("_=_" -> "\u208C"), //  # ₌
       ("_(" -> "\u208D"), //  # ₍
       ("_)" -> "\u208E"), //  # ₎
     )

     /** Unsoundly implemented in many fonts */
     val superscriptsigns = List(
       "^+^" -> "\u207A",
       "^-^" -> "\u207B",
       "^=^" -> "\u207C",
       "^(" -> "\u207D",
       "^)" -> "\u207E",
     )

      lazy val all = arrows++hebrew++composites++abbreviations++greek++cardSuits++superscriptDigits++superscriptsigns++subscripts

      if (false) locally { // check hebrew and greek
        for { (a, l)<-hebrew } println(a, l)
        for { (a, l)<-greek } println(a, l)
        for { (a, l)<-playingCards } println(a, l)
      }


}

