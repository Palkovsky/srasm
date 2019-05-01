ORG 1000h

DATA <= {
  ports <= 0x00, 0x00, 0x00
  arr <= 0x00, 0x01, 0x02, 0x03
}

CODE <= {
  LDX [0xFF], X
  TXS 2
  BRK

  JSR SUBRT_2

  SUBRT_1 <= {
    JSR SUBRT_2
  }

  SUBRT_2 <= {
    JSR SUBRT_1
  }
}
