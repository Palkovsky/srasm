class ParserTests extends BaseSpec{
  describe("Parser (basic instructions)") {
    it("Should parse instructions with decimal arguments"){
      parse("LDX 2137") should equal(List(
        Instruction("LDX", Number(2137))
      ))
    }

    it("Should parse instructions with immediate decimal arguments"){
      parse("LDA #2137") should equal(List(
        Instruction("LDA",  Immediate(Number((2137))))
      ))
    }

    it("Should parse instructions with hexadecimal arguments"){
      parse("LDX $AB") should equal(List(
        Instruction("LDX", Number(10*16+11))
      ))
    }

    it("Should parse instructions with immediate hexadecimal arguments"){
      parse("LDA #$FF") should equal(List(
        Instruction("LDA", Immediate(Number(255)))
      ))
    }

    it("Should parse instructions with immediate decimal arguments and register"){
      parse("LDA #121, Y") should equal(List(
        Instruction("LDA", Immediate(Number(121)), Register("Y"))
      ))
    }

    it("Should parse instructions with immediate hexadecimal arguments and register"){
      parse("LDA #$FFFF, X") should equal(List(
        Instruction("LDA", Immediate(Number(256*256-1)), Register("X"))
      ))
    }

   it("Should parse indirect X instructions with immediate hexadecimal arguments"){
      parse("STA (#$FFFF, X)") should equal(List(
        IndirectX(Instruction("STA", Immediate(Number(256*256-1)), Register("X")))
      ))
   }

    it("Should parse indirect Y instructions with adecimal arguments"){
      parse("STA (2137), Y") should equal(List(
        IndirectY(Instruction("STA", Number(2137), Register("Y")))
      ))
    }

    it("Should parse instructions with label argument"){
      parse("JSR LABEL_1") should equal(List(
        Instruction("JSR", Label("LABEL_1"))
      ))
    }

    it("Should parse instructions with immediate label argument"){
      parse("LDY #LABEL_1") should equal(List(
        Instruction("LDY", Immediate(Label("LABEL_1")))
      ))
    }

    it("Should parse instructions with label argument and register"){
      parse("LDY LABEL_1, Y") should equal(List(
        Instruction("LDY", Label("LABEL_1"), Register("Y"))
      ))
    }

    it("Should parse instructions without arguments"){
      parse("NOP") should equal(List(
        Instruction("NOP")
      ))
    }

    it("Should allow defining labels"){
      parse("MY_LABEL:") should equal(List(
        LabelDefinition("MY_LABEL")
      ))
    }

    it("Should not allow using instruction names as labels"){
       parse("TXS:") should equal(List(
        Instruction("TXS"), Unknown(":") // Unknown by default means error
      ))
    }
  }
}
