import parser._

class ParserTests extends BaseSpec{
  describe("Parser - basic instructions") {
    it("Should parse instructions with decimal arguments"){
      parse("LDX 2137") should equal(List(
        InstructionNode("LDX", Number(2137))
      ))
    }

    it("Should parse instructions with immediate decimal arguments"){
      parse("LDA #2137") should equal(List(
        InstructionNode("LDA",  Immediate(Number((2137))))
      ))
    }

    it("Should parse instructions with hexadecimal arguments"){
      parse("EOR $AB") should equal(List(
        InstructionNode("EOR", Number(10*16+11))
      ))
    }

    it("Should parse instructions with immediate hexadecimal arguments"){
      parse("LDA #$FF") should equal(List(
        InstructionNode("LDA", Immediate(Number(255)))
      ))
    }

    it("Should parse instructions with immediate decimal arguments and register"){
      parse("LDA #121, Y") should equal(List(
        InstructionNode("LDA", Immediate(Number(121)), Register("Y"))
      ))
    }

    it("Should parse instructions with immediate hexadecimal arguments and register"){
      parse("LDA #$FFFF, X") should equal(List(
        InstructionNode("LDA", Immediate(Number(256*256-1)), Register("X"))
      ))
    }

   it("Should parse indirect X instructions with immediate hexadecimal arguments"){
      parse("STA (#$FFFF, X)") should equal(List(
        IndirectX(InstructionNode("STA", Immediate(Number(256*256-1)), Register("X")))
      ))
   }

    it("Should parse indirect Y instructions with adecimal arguments"){
      parse("STA (2137), Y") should equal(List(
        IndirectY(InstructionNode("STA", Number(2137), Register("Y")))
      ))
    }

    it("Should parse indirect instructions with hexadecimal arguments"){
      parse("JMP ($FFFF)") should equal(List(
        Indirect(InstructionNode("JMP", Number(65535)))
      ))
    }

    it("Should parse relative instructions with hexadecimal arguments"){
      parse("BEQ *$FFFF") should equal(List(
        Relative(InstructionNode("BEQ", Number(65535)))
      ))
    }

    it("Should parse relative instructions with label arguments"){
      parse("BEQ *LABEL") should equal(List(
        Relative(InstructionNode("BEQ", Label("LABEL")))
      ))
    }    

    it("Should parse instructions with label argument"){
      parse("JSR LABEL_1") should equal(List(
        InstructionNode("JSR", Label("LABEL_1"))
      ))
    }

    it("Should parse instructions with immediate label argument"){
      parse("LDY #LABEL_1") should equal(List(
        InstructionNode("LDY", Immediate(Label("LABEL_1")))
      ))
    }

    it("Should parse instructions with label argument and register"){
      parse("LDY LABEL_1, Y") should equal(List(
        InstructionNode("LDY", Label("LABEL_1"), Register("Y"))
      ))
    }

    it("Should parse instructions without arguments"){
      parse("NOP") should equal(List(
        InstructionNode("NOP")
      ))
    }

    it("Should allow defining labels"){
      parse("MY_LABEL:") should equal(List(
        LabelDefinition("MY_LABEL")
      ))
    }

  }

  describe("Parser - segment definitions"){
    it("Should allow defining CODE and DATA segments"){
      val code = """
         ORG $1000
         DATA {
            ARR1: 
         }
         code {
            BRK
         }
      """
      parse(code) should equal(List(
        InstructionNode("ORG", Number(4096)),
        Segment("DATA", List(LabelDefinition("ARR1"))),
        Segment("CODE", List(InstructionNode("BRK")))
      ))
    }
  }

    describe("Parser - comments"){
      it("Should ignore one-line comments"){
        val code = """
          LDA #$44 ;Put 68 in memory
          ; STA $0100
          STA $0101 ;Print it to the terminal device 
      """
        parse(code) should equal(List(
          InstructionNode("LDA", Immediate(Number(68))),
          InstructionNode("STA", Number(257))
        ))
      }

     it("Should ignore multi-line comments"){
        val code = """
          /* COMMENT */
          /*
          LDA #$44 ;Put 68 in memory
          STA $0100
          */
          STA $0101 ;Print it to the terminal device 
      """
        parse(code) should equal(List(
          InstructionNode("STA", Number(257))
        ))
     }

      it("Shouldn't mind unclosed multi-line comment"){
        val code = """
          /* COMMENT
          /*
          LDA #$44 ;Put 68 in memory
          STA $0100
          */
          STA $0101 ;Print it to the terminal device 
      """
        parse(code) should equal(List())
      }
    }

    describe("Parser - string literals"){
      it("Should detect string literals with sigle parenthesis"){
        val code = """
          LDA #$44
          'xDDD'
          STA $0101
      """
        parse(code) should equal(List(
          InstructionNode("LDA", Immediate(Number(68))),
          StringLiteral("xDDD"),
          InstructionNode("STA", Number(257))
        ))
      }

     it("Should detect string literals with double parenthesis"){
        val code = """
          LDA #$44
          "xDDD"
      """
        parse(code) should equal(List(
          InstructionNode("LDA", Immediate(Number(68))),
          StringLiteral("xDDD")
        ))
     }

     it("Should detect string literals inside segments"){
        val code = """
          DATA {
          buff:
            'xDD'
          }
      """
        parse(code) should equal(List(Segment("DATA", List(
          LabelDefinition("buff"),
          StringLiteral("xDD")
        ))))
      }

     it("Should detect string literals with nested single-parenthesis"){
        val code = """
          LDA #$44
          "xD'D'D"
      """
        parse(code) should equal(List(
          InstructionNode("LDA", Immediate(Number(68))),
          StringLiteral("xD'D'D")
        ))
     }

     it("Should detect string literals with nested double-parenthesis"){
        val code = """
          LDA #$44
          'xD"D"D'
      """
        parse(code) should equal(List(
          InstructionNode("LDA", Immediate(Number(68))),
          StringLiteral("xD\"D\"D")
        ))
      }
    }
}
