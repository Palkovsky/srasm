ORG 1000h

DATA <= {
  ports <= 00h, 00h, 00h
  arr <= 00h, 01h, 02h, 03h
}

CODE <= {
  LDX [FFh], X
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
