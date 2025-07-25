package org.sufrin.glyph
package tests

import tests.StockAbbreviations.greekUpper

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

    val abbreviations = List(
      "->" -> "→",
      "<->" -> "↔",
      "│-" -> "\u251C",
      "<=" -> "\u21D0", //
      "=>" -> "\u21D2", //

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
      ("∃/" -> "\u2204"), //  # ∄
      ("\\in" -> "\u2208"), //  # ∈
      ("\\notin" -> "\u2209"), //  # ∉
      ("∈/" -> "\u2209"), //  # ∉
      ("<-" -> "\u2190"), //
      ("^^" -> "\u2191"), //  # ↑
      ("->" -> "\u2192"), //  # →
      ("!!" -> "\u2193"), //  # ↓
      ("<->" -> "\u2194"), //  # ←

      ("subeq" -> "\u227c"), // # ≼
      ("domeq" -> "\u227d"), // # ≽
      ("sub" -> "\u227a"), // # ≺
      ("dom" -> "\u227b"), // # ≻
      ("leq" -> "\u2264"), // # ≤
      ("geq" -> "\u2265"), // # ≥
      ("neq" -> "\u2260"), // # ≠

      ("[=" -> "\u2291"), // # ⊑
      ("=]" -> "\u2292"), // # ⊒
      ("[[" -> "\u228f"), // # ⊏
      ("]]" -> "\u2290"), // # ⊐
      ("nand" -> "\u22bc"), // # ⊼

      // Quotation marks
      ("``" -> "\u201c"), // # “
      ("''" -> "\u201d"), // # ”
      ("`" -> "\u2018"), // # ‘
      ("'" -> "\u2019"), // # ’

      ("+/>" -> "\u219b"), //          # ↛
      ("+>" -> "\u21f8"), //          # ⇸
      ("++>" -> "\u21fb"), //          # ⇻
      ("->>" -> "\u21a0"), //          # ↠
      (">->" -> "\u21a3"), //          # ↣
      ("\\to" -> "\u21a6"), //          # ↦
      ("|->" -> "\u21a6"), //          # ↦

      ("\\zap" -> "\u21af"), //          # ↯

      ("\\uaa" -> "\u21D1"), //          # ⇑
      ("\\daa" -> "\u21D3"), //          # ⇓
      ("<->" -> "\u2194"), //          # ↔
      ("<=>" -> "\u21D4"), //          # ⇔

      ("\\sub" -> "\u2282"), //          # ⊂
      ("\\sup" -> "\u2283"), //          # ⊃
      ("\\=sub" -> "\u2286"), //          # ⊆
      ("\\=sup" -> "\u2287"), //          # ⊇

      ("<==" -> "\u21DA"), //          # ⇚
      ("==>" -> "\u21DB"), //          # ⇛
      ("<~~" -> "\u21DC"), //          # ⇜
      ("~~>" -> "\u21DD"), //          # ⇝

      ("lwa" -> "\u21E6"), //          # ⇦
      ("uwa" -> "\u21E7"), //          # ⇧
      ("rwa" -> "\u21E8"), //          # ⇨
      ("dwa" -> "\u21E9"), //          # ⇩

      ("[]" -> "\u220E"), //          # ∎
      ("compose" -> "\u2218"), //          # ∘
      ("dot" -> "\u00b7"), //          # ·
      ("bull" -> "\u2219"), //          # ∙
      ("tick" -> "\u221a"), //          # √
      ("||" -> "\u2225"), //          # ∥

      ("euro" -> "\u20ac"), //          # €
      ("pound" -> "\u20a4"), //          # ₤

      ("lnot" -> "\u00AC"), //          # ¬
      ("land" -> "\u2227"), //          # ∧
      ("/\\" -> "\u2227"), //          # ∧
      ("lor" -> "\u2228"), //          # ∨
      ("\\/" -> "\u2228"),
      ("cap" ->     "\u2229"),  //       # ∩
      ("cup" -> "\u222a"), //         # ∪

      ("\\so" -> "\u2234"), //          # ∴
      ("\\cos" -> "\u2235"), //          # ∵

      (":=" -> "\u2254"), //          # ≔
      ("Defs" -> "\u225c"), //          # ≜

      ("defs" -> "\u2258"), //          # ≘
      ("equiv" -> "\u2261"), //          # ≡
      ("xo" -> "\u2297"), //          # ⊗
      ("ox" -> "\u2297"), //          # ⊗
      ("o+" -> "\u2295"), //          # ⊕
      ("+o" -> "\u2295"), //          # ⊕
      ("|-" -> "\u22a2"), //          # ⊢
      ("true" -> "\u22a4"), //          # ⊤
      ("false" -> "\u22a5"), //          # ⊥
      ("top" -> "\u22a4"), //          # ⊤
      ("bot" -> "\u22a5"), //          # ⊥
      ("|=" -> "\u22a7"), //          # ⊧


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
      ("_+" -> "\u208A"), //  # ₊ foo₊
      ("_-" -> "\u208B"), //  # ₋
      ("_=" -> "\u208C"), //  # ₌
      ("_(" -> "\u208D"), //  # ₍
      ("_)" -> "\u208E"), //  # ₎

      ("(c)" -> "\u00a9"), //  # ©
      ("><" -> "\u00d7"), //  # ×
      ("O/" -> "\u00D8"), //  # Ø
      ("<<" -> "\u00AB"), //  # «
      (">>" -> "\u00BB"), //  # »

      ("So" -> "\u2042"), //  # ⁂
      ("--" -> "\u2014"), //  # — (wide minus)
      )

      val complex = List(
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
        ("ae" -> "æ"), // ᴂ
        ("Ae" -> "Æ"), // ᴂ
        ("genocide" -> "רֶצַח עַם")
      )

      val LRI=0x2066
      val PDI=0x2069

      val bidi = List(
        ("LRI" -> "\u2066"),
        ("RLI" -> "\u2067"),
        ("PDI" -> "\u2069")
      )

      def sloshName(s: String): String = s"\\$s"
      def sloshLowerCaseName(s: String): String = s"\\${s.toLowerCase}"

      /** hebrew letters as unicode bidi isolates */
      val hebrew = {
        val names="alef/beth/gimel/dalet/he/vav/zayin/het/tet/yod/.kaf/kaf/lamed/.mem/mem/.nun/nun/samekh/ayin/.pe/pe/.tsadi/tsadi/qof/resh/shin/tav".split('/').toSeq
        val start = 0X05D0
        val codes = start until (start+names.length)
        names.map(sloshName).zip(codes.map(code=>f"$LRI%c$code%c$PDI%c"))
      }

      /** greek letters (upper and lower) */
      val greek = {
        val names = "Alpha/Beta/Gamma/Delta/Epsilon/Zeta/Eta/Theta/Iota/Kappa/Lambda/Mu/Nu/Xi/Omicron/Pi/Rho/.Sigma/Sigma/Tau/Upsilon/Phi/Chi/Psi/Omega".split('/').toSeq
        def codes(start: Int): Seq[Int] = start until (start+names.length)
        val exception: ((String,String))=>Boolean = {
          case ("\\.Sigma", _) => true
          case other => false
        }
        val uppercase = names.map(sloshName).zip(codes(0X0391).map(code=>f"$code%c")).filterNot(exception) // there's no final capital Sigma
        val lowercase = names.map(sloshLowerCaseName).zip(codes(0X03B1).map(code=>f"$code%c"))
        uppercase++lowercase
      }

      lazy val all = bidi++hebrew++complex++abbreviations++greek

      if (false) locally { // check hebrew and greek
        for { (a, l)<-hebrew } println(a, l)
        for { (a, l)<-greek } println(a, l)
      }


}

